package org.apache.hop.pipeline.transforms.where;

import org.apache.hop.core.Const;
import org.apache.hop.core.plugins.IPlugin;
import org.apache.hop.core.plugins.PluginRegistry;
import org.apache.hop.core.plugins.TransformPluginType;
import org.apache.hop.core.util.Utils;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.BaseTransformMeta;
import org.apache.hop.pipeline.transform.ITransformDialog;
import org.apache.hop.pipeline.transform.ITransformMeta;
import org.apache.hop.ui.core.ConstUI;
import org.apache.hop.ui.core.FormDataBuilder;
import org.apache.hop.ui.core.gui.GUIResource;
import org.apache.hop.ui.pipeline.transform.BaseTransformDialog;
import org.apache.hop.ui.util.SwtSvgImageUtil;
import org.eclipse.jface.fieldassist.ControlDecoration;
import org.eclipse.jface.fieldassist.FieldDecorationRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * A common implementation of the {@link BaseTransformDialog} that creates many of
 * the common UI components.
 * 
 * TODO: Move to org.apache.hop.pipeline.dialog but for now there is a problem with class loader
 * 
 * 
 */
public abstract class AbstractTransformDialog<META extends ITransformMeta<?,?>> extends BaseTransformDialog implements ITransformDialog {	
	public static final int LARGE_MARGIN = 15;

	protected static final int BUTTON_WIDTH = 80;

	protected ModifyListener lsMod;
	private final META meta;

	public AbstractTransformDialog(Shell parent, Object transformMeta, PipelineMeta pipelineMeta, String name) {
		super(parent,  (META) transformMeta, pipelineMeta, name);
		
		this.meta = (META) transformMeta;
	}

	public META getTransformMeta() {
		return meta;
	}

	protected final Control createContents(final Composite parent) {

		Control titleArea = this.createTitleArea(parent);

		// The title separator line
		Label titleSeparator = new Label(parent, SWT.HORIZONTAL | SWT.SEPARATOR);
		titleSeparator.setLayoutData(new FormDataBuilder().top(titleArea, LARGE_MARGIN).fullWidth().result());
		props.setLook(titleSeparator);

		// The button bar
		Control buttonBar = this.createButtonBar(parent);
		buttonBar.setLayoutData(new FormDataBuilder().fullWidth().bottom().result());

		// The bottom separator line
		Label bottomSeparator = new Label(parent, SWT.HORIZONTAL | SWT.SEPARATOR);
		bottomSeparator.setLayoutData(new FormDataBuilder().bottom(buttonBar, -LARGE_MARGIN).fullWidth().result());
		props.setLook(bottomSeparator);

		Composite area = new Composite(parent, SWT.NONE);
		area.setLayout(new FormLayout());
		area.setLayoutData(new FormDataBuilder().top(titleSeparator, LARGE_MARGIN)
				.bottom(bottomSeparator, -LARGE_MARGIN).fullWidth().result());
		props.setLook(area);

		this.createDialogArea(area);

		return area;
	}

	protected final Control createTitleArea(final Composite parent) {

		Composite composite = new Composite(parent, SWT.NONE);
		composite.setLayout(new FormLayout());
		composite.setLayoutData(new FormDataBuilder().top().fullWidth().result());
		props.setLook(composite);

		Label icon = new Label(composite, SWT.CENTER);
		icon.setImage(getImage());
		icon.setLayoutData(new FormDataBuilder().top().right(100, 0).width(ConstUI.ICON_SIZE).result());
		props.setLook(icon);

		Label label = new Label(composite, SWT.NONE);
		label.setText(BaseMessages.getString("System.Label.TransformName"));
		label.setLayoutData(new FormDataBuilder().top().left().right(icon, 100).result());
		props.setLook(label);

		// Widget Transform name
		wTransformName = new Text(composite, SWT.SINGLE | SWT.LEFT | SWT.BORDER);
		wTransformName.setLayoutData(new FormDataBuilder().top(label).left().right(icon, -ConstUI.ICON_SIZE).result());
		wTransformName.addModifyListener(lsMod);
		wTransformName.addSelectionListener(lsDef);
		props.setLook(wTransformName);

		final ControlDecoration deco = new ControlDecoration(wTransformName, SWT.TOP | SWT.LEFT);
		deco.setDescriptionText(BaseMessages.getString("System.TransformNameMissing.Msg"));
		deco.setImage(
				FieldDecorationRegistry.getDefault().getFieldDecoration(FieldDecorationRegistry.DEC_ERROR).getImage());
		deco.setShowOnlyOnFocus(true);
		deco.hide();

		wTransformName.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				if (wTransformName.getText().length() > 0) {
					deco.hide();
				} else {
					deco.show();
				}

				baseTransformMeta.setChanged();

				wOk.setEnabled(isValid());
			}
		});

		return composite;
	}

	/**
	 * Creates and returns the contents of the upper part of this dialog (above the
	 * button bar).
	 * <p>
	 * The <code>Dialog</code> implementation of this framework method creates and
	 * returns a new <code>Composite</code> with no margins and spacing. Subclasses
	 * should override.
	 * </p>
	 * 
	 * @param parent The parent composite to contain the dialog area
	 * @return the dialog area control
	 */
	protected Control createDialogArea(final Composite parent) {

		// Create the top level composite for the dialog area
		Composite composite = new Composite(parent, SWT.NONE);
		FormLayout formLayout = new FormLayout();
		formLayout.marginWidth = Const.FORM_MARGIN;
		formLayout.marginHeight = Const.FORM_MARGIN;
		composite.setLayout(formLayout);

		return composite;
	}

	protected Control createButtonBar(final Composite parent) {

		Composite composite = new Composite(parent, SWT.NONE);
		composite.setLayout(new FormLayout());
		composite.setLayoutData(new FormDataBuilder().fullWidth().bottom().result());
		composite.setFont(parent.getFont());
		props.setLook(composite);

		// Add the buttons to the button bar.
		this.createButtonsForButtonBar(composite);

		return composite;
	}

	protected void createButtonsForButtonBar(final Composite parent) {
		wCancel = new Button(parent, SWT.PUSH);
		wCancel.setText(BaseMessages.getString("System.Button.Cancel"));
//    wCancel.setLayoutData(new FormDataBuilder().bottom().right().width(BUTTON_WIDTH).result());
		wCancel.setLayoutData(new FormDataBuilder().bottom().right().result());
		wCancel.addListener(SWT.Selection, new Listener() {
			@Override
			public void handleEvent(Event e) {
				onCancelPressed();
			}
		});

		wOk = new Button(parent, SWT.PUSH);
		wOk.setText(BaseMessages.getString("System.Button.OK"));
//    wOK.setLayoutData(new FormDataBuilder().bottom().right(wCancel, -ConstUI.SMALL_MARGIN).width(BUTTON_WIDTH).result());
		wOk.setLayoutData(new FormDataBuilder().bottom().right(wCancel, -ConstUI.SMALL_MARGIN).result());
		wOk.addListener(SWT.Selection, new Listener() {
			@Override
			public void handleEvent(Event e) {
				onOkPressed();
			}
		});
	}

	/**
	 * This method is called by Spoon when the user opens the settings dialog of the
	 * step. It opens the dialog and returns only once the dialog has been closed by
	 * the user.
	 *
	 * If the user confirms the dialog, the meta object (passed in the constructor)
	 * is updated to reflect the new step settings. The changed flag of the meta
	 * object reflect whether the step configuration was changed by the dialog.
	 *
	 * If the user cancels the dialog, the meta object is not updated
	 *
	 * The open() method returns the name of the step after the user has confirmed
	 * the dialog, or null if the user cancelled the dialog.
	 */
	@Override
	public String open() {

		Shell parent = getParent();
		Display display = parent.getDisplay();

		// Create shell
		shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MAX | SWT.MIN);
		shell.setText(getText());
		// desactiver pour les tests uniquement sinon NPE
		setShellImage(shell, this.baseTransformMeta);

		// shell.setImage(GUIResource.getInstance().getImageVariable());

		FormLayout formLayout = new FormLayout();
		formLayout.marginWidth = LARGE_MARGIN;
		formLayout.marginHeight = LARGE_MARGIN;
		shell.setLayout(formLayout);
		shell.setMinimumSize(getMinimumSize());
		props.setLook(shell);

		// Default listener (for hitting "enter")
		lsDef = new SelectionAdapter() {
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				onOkPressed();
			}
		};

		// The ModifyListener used on all controls. It will update the meta object to
		// indicate that changes are being made.
		lsMod = new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				baseTransformMeta.setChanged();

				wOk.setEnabled(isValid());
			}
		};

		this.createContents(shell);

		// Save the value of the changed flag on the meta object. If the user cancels
		// the dialog, it will be restored to this saved value.
		// The "changed" variable is inherited from BaseStepDialog
		changed = baseTransformMeta.hasChanged();

		// Populate the dialog with the values from the meta object
		loadMeta(this.meta);

		// Restore the changed flag to original value, as the modify listeners fire
		// during dialog population
		this.transformMeta.setChanged(changed);

		// Detect X or ALT-F4 or something that kills this window...
		shell.addShellListener(new ShellAdapter() {
			@Override
			public void shellClosed(ShellEvent e) {
				onCancelPressed();
			}
		});

		// Set/Restore the dialog size based on last position on screen
		setSize(shell);

		// Set focus on transform name
		wTransformName.setText(this.transformName);
		wTransformName.selectAll();
		wTransformName.setFocus();

		// Open dialog and enter event loop
		shell.open();
		while (!shell.isDisposed()) {
			if (!display.readAndDispatch()) {
				display.sleep();
			}
		}

		// The "transformName" variable is inherited from BaseTransformDialog
		return transformName;
	}

	public Image getImage() {

		IPlugin plugin = PluginRegistry.getInstance().getPlugin(TransformPluginType.class,
				this.transformMeta.getTransformMetaInterface());

		if (plugin.getImageFile() != null) {
			return SwtSvgImageUtil.getImage(shell.getDisplay(), getClass().getClassLoader(), plugin.getImageFile(),
					ConstUI.ICON_SIZE, ConstUI.ICON_SIZE);
		}

		return GUIResource.getInstance().getImageTransformError();
	}

	/**
	 * Returns a point describing the minimum receiver's size. The x coordinate of
	 * the result is the minimum width of the receiver. The y coordinate of the
	 * result is the minimum height of the receiver.
	 * 
	 * @return the receiver's size
	 */
	public Point getMinimumSize() {
		return new Point(100, 50);
	}

	/**
	 * Called when the user confirms the dialog. Subclasses may override if desired.
	 */
	protected void onOkPressed() {

		if (Utils.isEmpty(wTransformName.getText())) {
			return;
		}

		transformName = wTransformName.getText();

		saveMeta(this.meta);

		// Close the SWT dialog window
		dispose();
	}

	/**
	 * Called when the user cancels the dialog. Subclasses may override if desired.
	 */
	protected void onCancelPressed() {
		this.transformName = null;

		// Restore initial state
		transformMeta.setChanged(changed);

		// Close the SWT dialog window
		dispose();
	}

	/**
	 * Copy information from the {@link META} meta to the dialog fields.
	 */
	protected abstract void loadMeta(final META transformMeta);

	/**
	 * This helper method takes the information configured in the dialog controls
	 * and stores it into the step configuration meta object
	 */
	/**
	 * Creates a new instance of {@link META} and populates it with provided
	 * data from the dialog.
	 *
	 * @param meta a new instance of {@link META}
	 */
	protected abstract void saveMeta(final META transformMeta);

	protected boolean isValid() {
		return !Utils.isEmpty(this.wTransformName.getText());
	}
}
