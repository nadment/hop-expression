/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.apache.hop.pipeline.transforms.where;

import org.apache.hop.core.Const;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.plugins.IPlugin;
import org.apache.hop.core.plugins.PluginRegistry;
import org.apache.hop.core.plugins.TransformPluginType;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.BaseTransformMeta;
import org.apache.hop.pipeline.transform.ITransformDialog;
import org.apache.hop.pipeline.transform.TransformMeta;
import org.apache.hop.ui.core.ConstUi;
import org.apache.hop.ui.core.FormDataBuilder;
import org.apache.hop.ui.core.gui.GuiResource;
import org.apache.hop.ui.expression.ExpressionEditor;
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
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

public class WhereDialog extends BaseTransformDialog implements ITransformDialog {

  private static final Class<?> PKG = WhereMeta.class; // for i18n purposes

  private final WhereMeta input;
  private ExpressionEditor wEditor;
  private ModifyListener lsMod;

  public WhereDialog(Shell parent, IVariables variables, Object in, PipelineMeta pipelineMeta,
      String name) {
    super(parent, variables, (BaseTransformMeta) in, pipelineMeta, name);
    input = (WhereMeta) in;
  }

  @Override
  public String open() {

    Shell parent = getParent();
    Display display = parent.getDisplay();

    // Create shell
    shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MAX | SWT.MIN);
    shell.setText(BaseMessages.getString(PKG, "WhereDialog.Shell.Title"));
    setShellImage(shell, input);

    FormLayout formLayout = new FormLayout();
    formLayout.marginWidth = Const.FORM_MARGIN;
    formLayout.marginHeight = Const.FORM_MARGIN;
    shell.setLayout(formLayout);
    shell.setMinimumSize(500, 300);

    props.setLook(shell);

    // Default listener (for hitting "enter")
    lsDef = new SelectionAdapter() {
      @Override
      public void widgetDefaultSelected(SelectionEvent e) {
        ok();
      }
    };

    // The ModifyListener used on all controls. It will update the meta object to
    // indicate that changes are being made.
    lsMod = e -> {
      baseTransformMeta.setChanged();
      wOk.setEnabled(isValid());
    };

    this.createContents(shell);

    // Save the value of the changed flag on the meta object. If the user cancels
    // the dialog, it will be restored to this saved value.
    // The "changed" variable is inherited from BaseStepDialog
    changed = baseTransformMeta.hasChanged();

    // Populate the dialog with the values from the meta object
    setWidgetsContent(input);

    // Restore the changed flag to original value, as the modify listeners fire
    // during dialog population
    this.transformMeta.setChanged(changed);

    // Detect X or ALT-F4 or something that kills this window...
    shell.addShellListener(new ShellAdapter() {
      @Override
      public void shellClosed(ShellEvent e) {
        cancel();
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
        this.transformMeta.getPluginId());

    if (plugin.getImageFile() != null) {
      return SwtSvgImageUtil.getImage(shell.getDisplay(), getClass().getClassLoader(),
          plugin.getImageFile(), ConstUi.ICON_SIZE, ConstUi.ICON_SIZE);
    }

    return GuiResource.getInstance().getImageError();
  }

  protected void setWidgetsContent(final WhereMeta meta) {
    if (meta.getExpression() != null) {
      this.wEditor.setText(meta.getExpression());
    }
  }

  protected void getWidgetsContent(final WhereMeta meta) {

    // save step name
    this.transformName = this.wTransformName.getText();

    meta.setExpression(this.wEditor.getText());
  }

  protected final Control createContents(final Composite parent) {

    Control titleArea = this.createTitleArea(parent);

    // The title separator line
    Label titleSeparator = new Label(parent, SWT.HORIZONTAL | SWT.SEPARATOR);
    titleSeparator.setLayoutData(
        new FormDataBuilder().top(titleArea, Const.FORM_MARGIN).fullWidth().result());
    props.setLook(titleSeparator);

    // Buttons
    wOk = new Button(shell, SWT.PUSH);
    wOk.setText(BaseMessages.getString(PKG, "System.Button.OK"));
    wOk.addListener(SWT.Selection, e -> ok());
    wCancel = new Button(shell, SWT.PUSH);
    wCancel.setText(BaseMessages.getString(PKG, "System.Button.Cancel"));
    wCancel.addListener(SWT.Selection, e -> cancel());

    setButtonPositions(new Button[] {wOk, wCancel}, props.getMargin(), null);

    Composite area = new Composite(parent, SWT.NONE);
    area.setLayout(new FormLayout());
    area.setLayoutData(new FormDataBuilder().top(titleSeparator, Const.FORM_MARGIN)
        .bottom(wOk, -Const.FORM_MARGIN).fullWidth().result());
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
    icon.setLayoutData(new FormDataBuilder().top().right(100, 0).width(ConstUi.ICON_SIZE).result());
    props.setLook(icon);

    Label label = new Label(composite, SWT.NONE);
    label.setText(BaseMessages.getString("System.Label.TransformName"));
    label.setLayoutData(new FormDataBuilder().top().left().right(icon, 100).result());
    props.setLook(label);

    // Widget Transform name
    wTransformName = new Text(composite, SWT.SINGLE | SWT.LEFT | SWT.BORDER);
    wTransformName.setLayoutData(
        new FormDataBuilder().top(label).left().right(icon, -ConstUi.ICON_SIZE).result());
    wTransformName.addModifyListener(lsMod);
    wTransformName.addSelectionListener(lsDef);
    props.setLook(wTransformName);

    final ControlDecoration deco = new ControlDecoration(wTransformName, SWT.TOP | SWT.LEFT);
    deco.setDescriptionText(BaseMessages.getString("System.TransformNameMissing.Msg"));
    deco.setImage(FieldDecorationRegistry.getDefault()
        .getFieldDecoration(FieldDecorationRegistry.DEC_ERROR).getImage());
    deco.setShowOnlyOnFocus(true);
    deco.hide();

    wTransformName.addListener(SWT.Modify, event -> {
      if (wTransformName.getText().length() > 0) {
        deco.hide();
      } else {
        deco.show();
      }
      baseTransformMeta.setChanged();
      wOk.setEnabled(isValid());
    });

    return composite;
  }


  protected Control createDialogArea(final Composite parent) {

    wEditor = new ExpressionEditor(parent, SWT.NONE, true);
    wEditor.setVariables(this.getVariables());
    wEditor.setLayoutData(new FormDataBuilder().top().fullWidth().bottom().result());

    // Search the fields in the background
    new Thread(() -> {
      try {
        TransformMeta transformMeta = pipelineMeta.findTransform(transformName);
        if (transformMeta != null) {
          IRowMeta rowMeta =
              pipelineMeta.getPrevTransformFields(this.getVariables(), transformMeta);
          wEditor.setRowMeta(rowMeta);
        }
      } catch (HopException e) {
        logError(BaseMessages.getString(PKG, "System.Dialog.GetFieldsFailed.Message"), e);
      }
    }).start();

    return parent;
  }

  /** Called when the user confirms the dialog. Subclasses may override if desired. */
  protected void ok() {

    if (Utils.isEmpty(wTransformName.getText())) {
      return;
    }

    transformName = wTransformName.getText();

    getWidgetsContent(input);

    // Close the SWT dialog window
    dispose();
  }

  /** Called when the user cancels the dialog. Subclasses may override if desired. */
  protected void cancel() {
    this.transformName = null;

    // Restore initial state
    transformMeta.setChanged(changed);

    // Close the SWT dialog window
    dispose();
  }

  protected boolean isValid() {
    return !Utils.isEmpty(this.wTransformName.getText());
  }
}
