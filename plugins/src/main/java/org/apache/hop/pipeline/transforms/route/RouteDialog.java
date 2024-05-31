/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.hop.pipeline.transforms.route;

import java.util.concurrent.CompletableFuture;
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
import org.apache.hop.pipeline.transform.ITransformDialog;
import org.apache.hop.pipeline.transform.TransformMeta;
import org.apache.hop.ui.core.ConstUi;
import org.apache.hop.ui.core.FormDataBuilder;
import org.apache.hop.ui.core.PropsUi;
import org.apache.hop.ui.core.dialog.BaseDialog;
import org.apache.hop.ui.core.gui.GuiResource;
import org.apache.hop.ui.core.widget.ColumnInfo;
import org.apache.hop.ui.core.widget.TableView;
import org.apache.hop.ui.expression.ExpressionEditorDialog;
import org.apache.hop.ui.expression.ExpressionMode;
import org.apache.hop.ui.pipeline.transform.BaseTransformDialog;
import org.apache.hop.ui.util.SwtSvgImageUtil;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

public class RouteDialog extends BaseTransformDialog implements ITransformDialog {
  private static final Class<?> PKG = RouteMeta.class; // For Translator

  private TableView wRoutes;

  private Combo wDefaultTarget;

  private final RouteMeta input;

  public RouteDialog(
      Shell parent,
      IVariables variables,
      Object input,
      PipelineMeta pipelineMeta,
      String transformName) {
    super(parent, variables, (RouteMeta) input, pipelineMeta, transformName);
    this.input = (RouteMeta) input;
  }

  @Override
  public String open() {
    Shell parent = getParent();

    // Create shell
    shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MAX | SWT.MIN);
    shell.setText(BaseMessages.getString(PKG, "RouteDialog.Shell.Title"));
    setShellImage(shell, input);

    FormLayout formLayout = new FormLayout();
    formLayout.marginWidth = PropsUi.getFormMargin();
    formLayout.marginHeight = PropsUi.getFormMargin();
    shell.setLayout(formLayout);
    shell.setMinimumSize(500, 300);

    PropsUi.setLook(shell);

    this.createContents(shell);

    // Save the value of the changed flag on the meta object. If the user cancels
    // the dialog, it will be restored to this saved value.
    // The "changed" variable is inherited from BaseStepDialog
    this.backupChanged = baseTransformMeta.hasChanged();

    // Populate the dialog with the values from the meta object
    setWidgetsContent(input);

    // Restore the changed flag to original value, as the modify listeners fire
    // during dialog population
    this.transformMeta.setChanged(backupChanged);

    // Set focus on transform name
    wTransformName.setText(this.transformName);
    wTransformName.selectAll();
    wTransformName.setFocus();

    BaseDialog.defaultShellHandling(shell, c -> ok(), c -> cancel());

    // The "transformName" variable is inherited from BaseTransformDialog
    return transformName;
  }

  protected final Control createContents(final Composite parent) {

    Control titleArea = this.createTitleArea(parent);

    // The title separator line
    Label titleSeparator = new Label(parent, SWT.HORIZONTAL | SWT.SEPARATOR);
    titleSeparator.setLayoutData(
        new FormDataBuilder().top(titleArea, PropsUi.getFormMargin()).fullWidth().result());
    PropsUi.setLook(titleSeparator);

    // Buttons
    wOk = new Button(shell, SWT.PUSH);
    wOk.setText(BaseMessages.getString(PKG, "System.Button.OK"));
    wOk.addListener(SWT.Selection, e -> ok());
    wCancel = new Button(shell, SWT.PUSH);
    wCancel.setText(BaseMessages.getString(PKG, "System.Button.Cancel"));
    wCancel.addListener(SWT.Selection, e -> cancel());

    setButtonPositions(new Button[] {wOk, wCancel}, PropsUi.getMargin(), null);

    Composite area = new Composite(parent, SWT.NONE);
    area.setLayout(new FormLayout());
    area.setLayoutData(
        new FormDataBuilder()
            .top(titleSeparator, PropsUi.getFormMargin())
            .bottom(wOk, -PropsUi.getFormMargin())
            .fullWidth()
            .result());
    PropsUi.setLook(area);

    this.createDialogArea(area);

    return area;
  }

  protected final Control createTitleArea(final Composite parent) {

    Composite composite = new Composite(parent, SWT.NONE);
    composite.setLayout(new FormLayout());
    composite.setLayoutData(new FormDataBuilder().top().fullWidth().result());
    PropsUi.setLook(composite);

    Label icon = new Label(composite, SWT.CENTER);
    icon.setImage(getImage());
    icon.setLayoutData(
        new FormDataBuilder().top().right(100, 0).width(ConstUi.LARGE_ICON_SIZE).result());
    PropsUi.setLook(icon);

    Label label = new Label(composite, SWT.NONE);
    label.setText(BaseMessages.getString(PKG, "System.TransformName.Label"));
    label.setToolTipText(BaseMessages.getString(PKG, "System.TransformName.Tooltip"));
    label.setLayoutData(new FormDataBuilder().top().left().right(icon, 100).result());
    PropsUi.setLook(label);

    // Widget Transform name
    wTransformName = new Text(composite, SWT.SINGLE | SWT.LEFT | SWT.BORDER);
    wTransformName.setLayoutData(
        new FormDataBuilder().top(label).left().right(icon, -PropsUi.getMargin()).result());
    wTransformName.addListener(SWT.Modify, event -> onChanged());
    PropsUi.setLook(wTransformName);

    return composite;
  }

  protected Control createDialogArea(final Composite parent) {
    int margin = PropsUi.getMargin();

    CompletableFuture<IRowMeta> rowMetaProvider =
        getAsyncRowMeta(this.getVariables(), pipelineMeta, transformName);

    String[] nextTransformNames = pipelineMeta.getNextTransformNames(transformMeta);

    // The routes
    //
    Label wlRoutes = new Label(parent, SWT.LEFT);
    wlRoutes.setText(BaseMessages.getString(PKG, "RouteDialog.Routes.Label"));
    FormData fdlRoutes = new FormData();
    fdlRoutes.left = new FormAttachment(0, 0);
    fdlRoutes.top = new FormAttachment(0, 0);
    fdlRoutes.right = new FormAttachment(100, 0);
    wlRoutes.setLayoutData(fdlRoutes);
    PropsUi.setLook(wlRoutes);

    ColumnInfo[] columns =
        new ColumnInfo[] {
          new ColumnInfo(
              BaseMessages.getString(PKG, "RouteDialog.ColumnInfo.Condition"),
              ColumnInfo.COLUMN_TYPE_TEXT_BUTTON,
              false),
          new ColumnInfo(
              BaseMessages.getString(PKG, "RouteDialog.ColumnInfo.TargetTransform"),
              ColumnInfo.COLUMN_TYPE_CCOMBO,
              nextTransformNames,
              false),
        };

    wRoutes =
        new TableView(
            variables,
            parent,
            SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI,
            columns,
            input.getRoutes().size(),
            null,
            props);
    PropsUi.setLook(wRoutes);
    columns[0].setUsingVariables(true);
    columns[0].setTextVarButtonSelectionListener(
        new SelectionAdapter() {
          @Override
          public void widgetSelected(SelectionEvent e) {

            String expression =
                wRoutes.getActiveTableItem().getText(wRoutes.getActiveTableColumn());

            if (!shell.isDisposed()) {
              ExpressionEditorDialog dialog = new ExpressionEditorDialog(shell);
              expression =
                  dialog.open(expression, getVariables(), ExpressionMode.ROW, rowMetaProvider);
              if (expression != null) {
                wRoutes.getActiveTableItem().setText(wRoutes.getActiveTableColumn(), expression);
              }
            }
          }
        });
    wRoutes.addListener(SWT.Modify, e -> onChanged());

    // The default target transform
    //
    wDefaultTarget = new Combo(parent, SWT.SINGLE | SWT.LEFT | SWT.BORDER);
    FormData fdDefaultTarget = new FormData();
    fdDefaultTarget.left = new FormAttachment(0, 0);
    fdDefaultTarget.right = new FormAttachment(100, 0);
    fdDefaultTarget.bottom = new FormAttachment(100, 0);
    wDefaultTarget.setLayoutData(fdDefaultTarget);
    wDefaultTarget.setItems(nextTransformNames);
    wDefaultTarget.addListener(SWT.Modify, e -> onChanged());
    PropsUi.setLook(wDefaultTarget);

    Label wlDefaultTarget = new Label(parent, SWT.LEFT);
    wlDefaultTarget.setText(BaseMessages.getString(PKG, "RouteDialog.DefaultTarget.Label"));
    FormData fdlDefaultTarget = new FormData();
    fdlDefaultTarget.left = new FormAttachment(0, 0);
    fdlDefaultTarget.right = new FormAttachment(100, 0);
    fdlDefaultTarget.bottom = new FormAttachment(wDefaultTarget, -margin);
    wlDefaultTarget.setLayoutData(fdlDefaultTarget);
    PropsUi.setLook(wlDefaultTarget);

    FormData fdRoutes = new FormData();
    fdRoutes.left = new FormAttachment(0, 0);
    fdRoutes.top = new FormAttachment(wlRoutes, margin);
    fdRoutes.right = new FormAttachment(100, 0);
    fdRoutes.bottom = new FormAttachment(wlDefaultTarget, -margin * 2);
    wRoutes.setLayoutData(fdRoutes);

    return parent;
  }

  // Search the fields in the background
  protected CompletableFuture<IRowMeta> getAsyncRowMeta(
      IVariables variables, PipelineMeta pipelineMeta, String transformName) {
    return CompletableFuture.supplyAsync(
        () -> {
          try {
            TransformMeta transformMeta = pipelineMeta.findTransform(transformName);
            if (transformMeta != null) {
              return pipelineMeta.getPrevTransformFields(variables, transformMeta);
            }
          } catch (HopException e) {
            // Ignore
          }
          return null;
        });
  }

  public Image getImage() {

    IPlugin plugin =
        PluginRegistry.getInstance()
            .getPlugin(TransformPluginType.class, this.transformMeta.getPluginId());

    if (plugin.getImageFile() != null) {
      return SwtSvgImageUtil.getImage(
          shell.getDisplay(),
          getClass().getClassLoader(),
          plugin.getImageFile(),
          ConstUi.LARGE_ICON_SIZE,
          ConstUi.LARGE_ICON_SIZE);
    }

    return GuiResource.getInstance().getImageError();
  }

  /** Update the meta object to indicate that changes are being made. */
  protected void onChanged() {
    baseTransformMeta.setChanged();
    wOk.setEnabled(isValid());
  }

  protected boolean isValid() {
    return !Utils.isEmpty(this.wTransformName.getText());
  }

  protected void getWidgetsContent(final RouteMeta meta) {
    int nrValues = wRoutes.nrNonEmpty();
    meta.getRoutes().clear();

    for (int i = 0; i < nrValues; i++) {
      TableItem item = wRoutes.getNonEmpty(i);

      Route target = new Route();
      target.setCondition(item.getText(1));
      target.setTransformName(item.getText(2));
      meta.getRoutes().add(target);
    }

    meta.setDefaultTargetTransformName(wDefaultTarget.getText());
  }

  /** Copy information from the meta-data input to the dialog fields. */
  protected void setWidgetsContent(final RouteMeta meta) {
    for (int i = 0; i < meta.getRoutes().size(); i++) {
      TableItem item = wRoutes.table.getItem(i);
      Route target = meta.getRoutes().get(i);
      if (target != null) {
        item.setText(1, Const.nullToEmpty(target.getCondition())); // The value
        item.setText(2, Const.nullToEmpty(target.getTransformName()));
      }
    }
    wRoutes.removeEmptyRows();
    wRoutes.setRowNums();
    wRoutes.optWidth(true);

    wDefaultTarget.setText(Const.nullToEmpty(meta.getDefaultTargetTransformName()));

    wTransformName.selectAll();
    wTransformName.setFocus();
  }

  protected void cancel() {
    transformName = null;
    input.setChanged(backupChanged);
    dispose();
  }

  protected void ok() {
    if (Utils.isEmpty(wTransformName.getText())) {
      return;
    }

    transformName = wTransformName.getText();

    getWidgetsContent(input);

    // Close the SWT dialog window
    dispose();
  }
}
