package org.apache.hop.ui.trans.steps.expression;

import java.util.ArrayList;
import java.util.List;

import org.apache.hop.core.Const;
import org.apache.hop.core.annotations.PluginDialog;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.row.RowMetaInterface;
import org.apache.hop.core.row.value.ValueMetaFactory;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.trans.TransMeta;
import org.apache.hop.trans.step.BaseStepMeta;
import org.apache.hop.trans.step.StepMeta;
import org.apache.hop.trans.steps.expression.ExpressionField;
import org.apache.hop.trans.steps.expression.ExpressionMeta;
import org.apache.hop.ui.core.FormDataBuilder;
import org.apache.hop.ui.core.widget.ColumnInfo;
import org.apache.hop.ui.core.widget.ColumnsResizer;
import org.apache.hop.ui.core.widget.TableView;
import org.apache.hop.ui.expression.ExpressionEditorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TableItem;

@PluginDialog(id = "ExpressionValue", image = "ExpressionValue.svg", pluginType = PluginDialog.PluginType.STEP)
public class ExpressionDialog extends AbstractStepDialog<ExpressionMeta> {
	private static Class<?> PKG = ExpressionMeta.class; // for i18n purposes, needed by Translator2!!

	private TableView tblFields;
	private RowMetaInterface rowMeta;

	public ExpressionDialog(Shell parent, Object input, TransMeta transMeta, String stepName) {
		super(parent, (BaseStepMeta) input, transMeta, stepName);

		this.setText(BaseMessages.getString(PKG, "ExpressionDialog.Shell.Title"));
	}

	@Override
	public Point getMinimumSize() {
		return new Point(600, 400);
	}

	@Override
	protected void loadMeta(final ExpressionMeta meta) {
		int i = 0;
		for (ExpressionField value : meta.getExpressionValues()) {

			TableItem item = tblFields.getTable().getItem(i++);
			item.setText(1, Const.NVL(value.getName(), ""));
			item.setText(2, Const.NVL(value.getExpression(), ""));
			item.setText(3, Const.NVL(ValueMetaFactory.getValueMetaName(value.getType()), ""));
			if (value.getLength() >= 0) {
				item.setText(4, String.valueOf(value.getLength()));
			}
			if (value.getPrecision() >= 0) {
				item.setText(5, String.valueOf(value.getPrecision()));
			}
		}

		tblFields.setRowNums();
		tblFields.optWidth(true);

		wStepname.selectAll();
		wStepname.setFocus();
	}

	@Override
	protected void saveMeta(final ExpressionMeta meta) {

		// save step name
		stepname = wStepname.getText();

		int count = tblFields.nrNonEmpty();
		List<ExpressionField> values = new ArrayList<>(count);
		for (int i = 0; i < count; i++) {
			TableItem item = tblFields.getNonEmpty(i);

			ExpressionField value = new ExpressionField();
			value.setName(item.getText(1));
			value.setExpression(item.getText(2));
			value.setType(ValueMetaFactory.getIdForValueMeta(item.getText(3)));
			value.setLength(Const.toInt(item.getText(4), -1));
			value.setPrecision(Const.toInt(item.getText(5), -1));

			values.add(value);
		}

		meta.setExpressionValues(values);

//	    if ( !originalMeta.equals( currentMeta ) ) {
//	      currentMeta.setChanged();
//	      changed = currentMeta.hasChanged();
//	    }
	}

	@Override
	protected Control createDialogArea(final Composite parent) {

		ColumnInfo[] columns = new ColumnInfo[] {
				new ColumnInfo(BaseMessages.getString(PKG, "ExpressionDialog.ColumnInfo.Name.Label"),
						ColumnInfo.COLUMN_TYPE_TEXT, false),
				new ColumnInfo(BaseMessages.getString(PKG, "ExpressionDialog.ColumnInfo.Expression.Label"),
						ColumnInfo.COLUMN_TYPE_TEXT_BUTTON, false),
				new ColumnInfo(BaseMessages.getString(PKG, "ExpressionDialog.ColumnInfo.ValueType.Label"),
						ColumnInfo.COLUMN_TYPE_CCOMBO, ValueMetaFactory.getValueMetaNames()),
				new ColumnInfo(BaseMessages.getString(PKG, "ExpressionDialog.ColumnInfo.Length.Label"),
						ColumnInfo.COLUMN_TYPE_TEXT, false),
				new ColumnInfo(BaseMessages.getString(PKG, "ExpressionDialog.ColumnInfo.Precision.Label"),
						ColumnInfo.COLUMN_TYPE_TEXT, false) };

		columns[1].setUsingVariables(true);
		columns[1].setTextVarButtonSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {

				String expression = tblFields.getActiveTableItem().getText(tblFields.getActiveTableColumn());

				if (!shell.isDisposed()) {
					ExpressionEditorDialog dialog = new ExpressionEditorDialog(shell,
							SWT.APPLICATION_MODAL | SWT.SHEET);
					dialog.setExpression(expression);
					dialog.setVariables(stepMeta.getParentTransMeta());
					dialog.setRowMeta(rowMeta);
					expression = dialog.open();
					if (expression != null) {
						tblFields.getActiveTableItem().setText(tblFields.getActiveTableColumn(), expression);
					}
				}
			}
		});

		tblFields = new TableView(transMeta, parent, SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI, columns,
				this.getStepMeta().getExpressionValues().size(), lsMod, props);
		tblFields.setLayoutData(new FormDataBuilder().top().bottom().left().right().result());
		tblFields.getTable().addListener(SWT.Resize, new ColumnsResizer(4, 20, 46, 10, 10, 10));

		// Search the fields in the background
		new Thread(() -> {
			StepMeta stepMeta = transMeta.findStep(stepname);
			if (stepMeta != null)
				try {

					rowMeta = transMeta.getPrevStepFields(stepMeta);
				} catch (HopException e) {
					logError(BaseMessages.getString(PKG, "ExpressionDialog.Log.UnableToFindInput"));
				}
		}).start();

		return tblFields;
	}

}