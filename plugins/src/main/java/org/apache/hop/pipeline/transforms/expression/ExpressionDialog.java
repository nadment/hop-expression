/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.pipeline.transforms.expression;

import java.util.ArrayList;
import java.util.List;
import org.apache.hop.core.Const;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.plugins.IPlugin;
import org.apache.hop.core.plugins.PluginRegistry;
import org.apache.hop.core.plugins.TransformPluginType;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.value.ValueMetaFactory;
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
import org.apache.hop.ui.core.widget.ColumnInfo;
import org.apache.hop.ui.core.widget.ColumnsResizer;
import org.apache.hop.ui.core.widget.TableView;
import org.apache.hop.ui.expression.ExpressionEditorDialog;
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
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

public class ExpressionDialog extends BaseTransformDialog implements ITransformDialog {
  private static final Class<?> PKG =
      ExpressionMeta.class; // for i18n purposes, needed by Translator2!!

  private final ExpressionMeta input;
  private TableView wTableFields;
  private IRowMeta rowMeta;
  private ModifyListener lsMod;

  public ExpressionDialog(
      Shell parent, IVariables variables, Object in, PipelineMeta pipelineMeta, String name) {
    super(parent, variables, (BaseTransformMeta) in, pipelineMeta, name);
    input = (ExpressionMeta) in;
  }

  @Override
  public String open() {

    Shell parent = getParent();
    Display display = parent.getDisplay();

    // Create shell
    shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MAX | SWT.MIN);
    shell.setText(BaseMessages.getString(PKG, "ExpressionDialog.Shell.Title"));
    setShellImage(shell, input);

    FormLayout formLayout = new FormLayout();
    formLayout.marginWidth = Const.FORM_MARGIN;
    formLayout.marginHeight = Const.FORM_MARGIN;
    shell.setLayout(formLayout);
    shell.setMinimumSize(700, 400);

    props.setLook(shell);

    // Default listener (for hitting "enter")
    lsDef =
        new SelectionAdapter() {
          @Override
          public void widgetDefaultSelected(SelectionEvent e) {
            ok();
          }
        };

    // The ModifyListener used on all controls. It will update the meta object to
    // indicate that changes are being made.
    lsMod =
        new ModifyListener() {
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
    setWidgetsContent(input);

    // Restore the changed flag to original value, as the modify listeners fire
    // during dialog population
    this.transformMeta.setChanged(changed);

    // Detect X or ALT-F4 or something that kills this window...
    shell.addShellListener(
        new ShellAdapter() {
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

  protected void setWidgetsContent(final ExpressionMeta meta) {
    int i = 0;
    for (ExpressionField value : meta.getExpressionFields()) {

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

  protected void getWidgetsContent(final ExpressionMeta meta) {

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
    area.setLayoutData(
        new FormDataBuilder()
            .top(titleSeparator, Const.FORM_MARGIN)
            .bottom(wOk, -Const.FORM_MARGIN)
            .fullWidth()
            .result());
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
    deco.setImage(
        FieldDecorationRegistry.getDefault()
            .getFieldDecoration(FieldDecorationRegistry.DEC_ERROR)
            .getImage());
    deco.setShowOnlyOnFocus(true);
    deco.hide();

    wTransformName.addModifyListener(
        new ModifyListener() {
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
              BaseMessages.getString(PKG, "ExpressionDialog.ColumnInfo.ValueType.Label"),
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

            String expression =
                wTableFields.getActiveTableItem().getText(wTableFields.getActiveTableColumn());

            if (!shell.isDisposed()) {
              ExpressionEditorDialog dialog =
                  new ExpressionEditorDialog(shell, SWT.APPLICATION_MODAL | SWT.SHEET, true);
              dialog.setExpression(expression);
              dialog.setVariables(getVariables());
              dialog.setRowMeta(rowMeta);
              expression = dialog.open();
              if (expression != null) {
                wTableFields
                    .getActiveTableItem()
                    .setText(wTableFields.getActiveTableColumn(), expression);
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
            this.input.getExpressionFields().size(),
            lsMod,
            props);
    wTableFields.setLayoutData(new FormDataBuilder().top().bottom().left().right().result());
    wTableFields.getTable().addListener(SWT.Resize, new ColumnsResizer(4, 20, 46, 10, 10, 10));

    // Search the fields in the background
    new Thread(
            () -> {
              TransformMeta transformMeta = pipelineMeta.findTransform(transformName);
              if (transformMeta != null)
                try {

                  rowMeta = pipelineMeta.getPrevTransformFields(getVariables(), transformMeta);
                } catch (HopException e) {
                  logError(BaseMessages.getString(PKG, "ExpressionDialog.Log.UnableToFindInput"));
                }
            })
        .start();

    return wTableFields;
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
          ConstUi.ICON_SIZE,
          ConstUi.ICON_SIZE);
    }

    return GuiResource.getInstance().getImageError();
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
