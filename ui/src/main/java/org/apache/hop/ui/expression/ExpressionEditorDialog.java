package org.apache.hop.ui.expression;

import org.apache.hop.core.Const;
import org.apache.hop.core.row.RowMetaInterface;
import org.apache.hop.core.variables.VariableSpace;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.ui.core.ConstUI;
import org.apache.hop.ui.core.FormDataBuilder;
import org.apache.hop.ui.core.PropsUI;
import org.apache.hop.ui.core.gui.GUIResource;
import org.apache.hop.ui.core.gui.WindowProperty;
import org.apache.hop.ui.trans.step.BaseStepDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * This dialogs allows you to edit expression.
 *
 * @author Nicolas ADMENT
 * @since 25-02-2019
 */
public class ExpressionEditorDialog extends Dialog {
	private static final Class<?> PKG = ExpressionEditorDialog.class;

	public static final int LARGE_MARGIN = 15;

	private PropsUI props;
	private Shell shell;

	private VariableSpace variables;
	private RowMetaInterface rowMeta;
	private ExpressionEditor wEditor;
	private String expression;

	public ExpressionEditorDialog(Shell parent, int style) {
		super(parent, style);

		this.props = PropsUI.getInstance();
	}

	public String open() {

		shell = new Shell(getParent(), SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MAX | SWT.MIN);
		props.setLook(shell);
		//shell.setImage(GUIResource.getInstance().getImageHop());
		shell.setText(BaseMessages.getString(PKG, "ExpressionEditorDialog.Shell.Title"));
		shell.setLayout(new FormLayout());

		// The expression editor
		wEditor = new ExpressionEditor(shell, SWT.NONE);
		wEditor.setText(Const.NVL(expression, ""));
		wEditor.setRowMeta(rowMeta);
		wEditor.setVariables(variables);
		wEditor.setLayoutData(
				new FormDataBuilder().top().bottom(100, -50).left(0, LARGE_MARGIN).right(100, -LARGE_MARGIN).result());

		// The button bar
		Composite buttonBar = new Composite(shell, SWT.NONE);
		FormLayout buttonBarLayout = new FormLayout();
		buttonBarLayout.marginHeight = LARGE_MARGIN;
		buttonBarLayout.marginWidth = LARGE_MARGIN;
		buttonBar.setLayout(buttonBarLayout);
		buttonBar.setLayoutData(new FormDataBuilder().top(wEditor, 0).bottom().right().result());
		props.setLook(buttonBar);

		Button btnCancel = new Button(buttonBar, SWT.PUSH);
		btnCancel.setText(BaseMessages.getString(PKG, "System.Button.Cancel"));
		btnCancel.setLayoutData(new FormDataBuilder().bottom().right().result());
		btnCancel.addListener(SWT.Selection, event -> onCancelPressed());

		Button btnOK = new Button(buttonBar, SWT.PUSH);
		btnOK.setText(BaseMessages.getString(PKG, "System.Button.OK"));
		btnOK.setLayoutData(new FormDataBuilder().bottom().right(btnCancel, -ConstUI.SMALL_MARGIN).result());
		btnOK.addListener(SWT.Selection, Event -> onOkPressed());

		BaseStepDialog.setSize(shell);

		shell.open();
		Display display = shell.getDisplay();
		while (!shell.isDisposed()) {
			if (!display.readAndDispatch()) {
				display.sleep();
			}
		}

		return expression;
	}

	public void dispose() {
		WindowProperty winprop = new WindowProperty(shell);

		props.setScreen(winprop);
		shell.dispose();
	}

	protected void onOkPressed() {

		this.expression = wEditor.getText();

		dispose();
	}

	/**
	 * Called when the user cancels the dialog. Subclasses may override if desired.
	 */
	protected void onCancelPressed() {

		this.expression = null;

		// Close the SWT dialog window
		dispose();
	}

	public String getExpression() {
		return expression;
	}

	public void setExpression(String expression) {
		this.expression = expression;
	}

	public RowMetaInterface getRowMeta() {
		return rowMeta;
	}

	public void setRowMeta(RowMetaInterface rowMeta) {
		this.rowMeta = rowMeta;
	}

	public VariableSpace getVariables() {
		return variables;
	}

	public void setVariables(VariableSpace variables) {
		this.variables = variables;
	}

}
