package org.apache.hop.pipeline.transforms.expression;

import java.util.ArrayList;
import java.util.List;

import org.apache.hop.core.Const;
import org.apache.hop.core.annotations.PluginDialog;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.value.ValueMetaFactory;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.ITransformDialog;
import org.apache.hop.pipeline.transform.TransformMeta;
import org.apache.hop.ui.core.FormDataBuilder;
import org.apache.hop.ui.core.widget.ColumnInfo;
import org.apache.hop.ui.core.widget.ColumnsResizer;
import org.apache.hop.ui.core.widget.TableView;
import org.apache.hop.ui.expression.AbstractTransformDialog;
import org.apache.hop.ui.expression.ExpressionEditorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TableItem;

@PluginDialog(id = "Expression", image = "expression.svg", pluginType = PluginDialog.PluginType.TRANSFORM)
public class ExpressionDialog extends AbstractTransformDialog<ExpressionMeta> implements ITransformDialog {
	private static final Class<?> PKG = ExpressionMeta.class; // for i18n purposes, needed by Translator2!!

	private TableView wTableFields;
	private IRowMeta rowMeta;

	public ExpressionDialog(Shell parent, Object input, PipelineMeta pipelineMeta, String name) {
		super(parent, input, pipelineMeta, name);

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

			TableItem item = wTableFields.getTable().getItem(i++);
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

		wTableFields.setRowNums();
		wTableFields.optWidth(true);

		this.wTransformName.selectAll();
		this.wTransformName.setFocus();
	}

	@Override
	protected void saveMeta(final ExpressionMeta meta) {

		// save step name
		this.transformName = this.wTransformName.getText();

		int count = wTableFields.nrNonEmpty();
		List<ExpressionField> values = new ArrayList<>(count);
		for (int i = 0; i < count; i++) {
			TableItem item = wTableFields.getNonEmpty(i);

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

				String expression = wTableFields.getActiveTableItem().getText(wTableFields.getActiveTableColumn());

				if (!shell.isDisposed()) {
					ExpressionEditorDialog dialog = new ExpressionEditorDialog(shell,
							SWT.APPLICATION_MODAL | SWT.SHEET);
					dialog.setExpression(expression);
					dialog.setVariables(transformMeta.getParentPipelineMeta());
					dialog.setRowMeta(rowMeta);
					expression = dialog.open();
					if (expression != null) {
						wTableFields.getActiveTableItem().setText(wTableFields.getActiveTableColumn(), expression);
					}
				}
			}
		});

		wTableFields = new TableView(this.pipelineMeta, parent, SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI, columns,
				this.getTransformMeta().getExpressionValues().size(), lsMod, props);
		wTableFields.setLayoutData(new FormDataBuilder().top().bottom().left().right().result());
		wTableFields.getTable().addListener(SWT.Resize, new ColumnsResizer(4, 20, 46, 10, 10, 10));

		// Search the fields in the background
		new Thread(() -> {
			TransformMeta transformMeta = pipelineMeta.findTransform(transformName);
			if (transformMeta != null)
				try {

					rowMeta = pipelineMeta.getPrevTransformFields(transformMeta);
				} catch (HopException e) {
					logError(BaseMessages.getString(PKG, "ExpressionDialog.Log.UnableToFindInput"));
				}
		}).start();

		return wTableFields;
	}

}