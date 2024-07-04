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
package org.apache.hop.pipeline.transforms.expression;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import org.apache.commons.lang.StringUtils;
import org.apache.hop.core.Const;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.plugins.IPlugin;
import org.apache.hop.core.plugins.PluginRegistry;
import org.apache.hop.core.plugins.TransformPluginType;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaFactory;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.expression.type.TypeId;
import org.apache.hop.expression.type.Types;
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
import org.apache.hop.ui.core.widget.ColumnsResizer;
import org.apache.hop.ui.core.widget.TableView;
import org.apache.hop.ui.expression.ExpressionEditorDialog;
import org.apache.hop.ui.expression.ExpressionMode;
import org.apache.hop.ui.pipeline.transform.BaseTransformDialog;
import org.apache.hop.ui.util.SwtSvgImageUtil;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

public class ExpressionDialog extends BaseTransformDialog implements ITransformDialog {
  private static final Class<?> PKG = ExpressionMeta.class;

  private static final String DIALOG_KEY = "expressiondialog";

  private final ExpressionMeta input;
  private TableView wTableFields;
  private ModifyListener lsMod;

  public ExpressionDialog(
      Shell parent, IVariables variables, ExpressionMeta transformMeta, PipelineMeta pipelineMeta) {
    super(parent, variables, transformMeta, pipelineMeta);
    this.input = transformMeta;
  }

  @Override
  public String open() {

    Shell parent = getParent();

    // Create shell
    shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MAX | SWT.MIN);
    shell.setText(BaseMessages.getString(PKG, "ExpressionDialog.Shell.Title"));
    setShellImage(shell, input);

    FormLayout formLayout = new FormLayout();
    formLayout.marginWidth = PropsUi.getFormMargin();
    formLayout.marginHeight = PropsUi.getFormMargin();
    shell.setLayout(formLayout);
    shell.setMinimumSize(700, 400);

    PropsUi.setLook(shell);

    // The ModifyListener used on all controls. It will update the meta object to
    // indicate that changes are being made.
    lsMod =
        e -> {
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

    // Set focus on transform name
    wTransformName.setText(this.transformName);
    wTransformName.selectAll();
    wTransformName.setFocus();

    BaseDialog.defaultShellHandling(shell, c -> ok(), c -> cancel());

    // The "transformName" variable is inherited from BaseTransformDialog
    return transformName;
  }

  protected void setWidgetsContent(final ExpressionMeta meta) {
    int i = 0;
    for (ExpressionField value : meta.getFields()) {

      TableItem item = wTableFields.getTable().getItem(i++);
      item.setText(1, Const.nullToEmpty(value.getName()));
      item.setText(2, Const.nullToEmpty(value.getExpression()));
      item.setText(3, Const.nullToEmpty(value.getType()));
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

  protected void getWidgetsContent(final ExpressionMeta meta) {

    // Save transform name
    this.transformName = this.wTransformName.getText();

    int count = wTableFields.nrNonEmpty();
    List<ExpressionField> fields = new ArrayList<>(count);
    for (int i = 0; i < count; i++) {
      TableItem item = wTableFields.getNonEmpty(i);

      ExpressionField value = new ExpressionField();
      value.setName(StringUtils.stripToNull(item.getText(1)));
      value.setExpression(item.getText(2));
      value.setType(item.getText(3));
      value.setLength(Const.toInt(item.getText(4), -1));
      value.setPrecision(Const.toInt(item.getText(5), -1));
      fields.add(value);
    }

    meta.setFields(fields);
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
    wTransformName.addModifyListener(lsMod);
    PropsUi.setLook(wTransformName);

    return composite;
  }

  protected Control createDialogArea(final Composite parent) {

    ColumnInfo[] columns =
        new ColumnInfo[] {
          new ColumnInfo(
              BaseMessages.getString(PKG, "ExpressionDialog.ColumnInfo.Name.Label"),
              ColumnInfo.COLUMN_TYPE_TEXT,
              false),
          new ColumnInfo(
              BaseMessages.getString(PKG, "ExpressionDialog.ColumnInfo.Expression.Label"),
              ColumnInfo.COLUMN_TYPE_TEXT_BUTTON,
              false),
          new ColumnInfo(
              BaseMessages.getString(PKG, "ExpressionDialog.ColumnInfo.Type.Label"),
              ColumnInfo.COLUMN_TYPE_CCOMBO,
              ValueMetaFactory.getValueMetaNames()),
          new ColumnInfo(
              BaseMessages.getString(PKG, "ExpressionDialog.ColumnInfo.Length.Label"),
              ColumnInfo.COLUMN_TYPE_TEXT,
              false),
          new ColumnInfo(
              BaseMessages.getString(PKG, "ExpressionDialog.ColumnInfo.Precision.Label"),
              ColumnInfo.COLUMN_TYPE_TEXT,
              false)
        };

    columns[1].setUsingVariables(true);
    columns[1].setTextVarButtonSelectionListener(
        new SelectionAdapter() {
          @Override
          public void widgetSelected(SelectionEvent e) {
            TableItem tableItem = wTableFields.getActiveTableItem();
            ExpressionEditorDialog dialog = (ExpressionEditorDialog) tableItem.getData(DIALOG_KEY);

            // Expression editor already open, bring dialog to front
            if (dialog != null) {
              dialog.setActive();
              return;
            }

            final IRowMeta rowMeta = new RowMeta();
            for (TableItem item : wTableFields.getNonEmptyItems()) {
              // All expressions previously defined
              if (item.equals(wTableFields.getActiveTableItem())) break;
              TypeId typeId = TypeId.of(item.getText(3));
              if (typeId != null) {
                String name = item.getText(1);
                rowMeta.addValueMeta(Types.createValueMeta(name, typeId));
              }
            }

            CompletableFuture<IRowMeta> rowMetaFutur =
                getAsyncRowMeta(getVariables(), pipelineMeta, transformName, rowMeta);

            dialog = new ExpressionEditorDialog(shell);
            tableItem.setData(DIALOG_KEY, dialog);

            String expression = tableItem.getText(wTableFields.getActiveTableColumn());
            expression = dialog.open(expression, getVariables(), ExpressionMode.ROW, rowMetaFutur);

            if (!shell.isDisposed()) {
              tableItem.setData(DIALOG_KEY, null);
              if (expression != null) {
                tableItem.setText(wTableFields.getActiveTableColumn(), expression);
              }
            }
          }
        });

    wTableFields =
        new TableView(
            this.getVariables(),
            parent,
            SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI,
            columns,
            this.input.getFields().size(),
            lsMod,
            props);
    wTableFields.setLayoutData(new FormDataBuilder().top().bottom().left().right().result());
    wTableFields.getTable().addListener(SWT.Resize, new ColumnsResizer(4, 20, 46, 10, 10, 10));

    return wTableFields;
  }

  // Search the fields in the background
  protected CompletableFuture<IRowMeta> getAsyncRowMeta(
      IVariables variables, PipelineMeta pipelineMeta, String transformName, IRowMeta rowMeta) {
    return CompletableFuture.supplyAsync(
        () -> {
          try {
            TransformMeta transformMeta = pipelineMeta.findTransform(transformName);
            if (transformMeta != null) {
              rowMeta.addRowMeta(pipelineMeta.getPrevTransformFields(variables, transformMeta));
            }
          } catch (HopException e) {
            // Ignore
          }
          return rowMeta;
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

  /** Called when the user confirms the dialog. */
  protected void ok() {

    if (Utils.isEmpty(wTransformName.getText())) {
      return;
    }

    transformName = wTransformName.getText();

    getWidgetsContent(input);

    // Close the SWT dialog window
    dispose();
  }

  /** Called when the user cancels the dialog. */
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
