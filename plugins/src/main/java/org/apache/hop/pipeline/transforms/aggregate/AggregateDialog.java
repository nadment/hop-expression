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

package org.apache.hop.pipeline.transforms.aggregate;

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
import org.apache.hop.ui.core.dialog.ErrorDialog;
import org.apache.hop.ui.core.gui.GuiResource;
import org.apache.hop.ui.core.widget.ColumnInfo;
import org.apache.hop.ui.core.widget.ColumnsResizer;
import org.apache.hop.ui.core.widget.TableView;
import org.apache.hop.ui.expression.ExpressionEditorDialog;
import org.apache.hop.ui.expression.ExpressionMode;
import org.apache.hop.ui.pipeline.transform.BaseTransformDialog;
import org.apache.hop.ui.util.SwtSvgImageUtil;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public class AggregateDialog extends BaseTransformDialog implements ITransformDialog {
  private static final Class<?> PKG = AggregateMeta.class; // For Translator

  private TableView wGroup;
  private TableView wAggregate;
  private Button wAlwaysAddResult;
  private ColumnInfo[] columnInfoGroup;

  private final AggregateMeta input;

  public AggregateDialog(Shell parent, IVariables variables, Object in, PipelineMeta pipelineMeta,
      String transformName) {
    super(parent, variables, (AggregateMeta) in, pipelineMeta, transformName);
    this.input = (AggregateMeta) in;
  }

  @Override
  public String open() {
    Shell parent = getParent();

    shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MAX | SWT.MIN);
    PropsUi.setLook(shell);
    setShellImage(shell, input);

    backupChanged = input.hasChanged();

    FormLayout formLayout = new FormLayout();
    formLayout.marginWidth = PropsUi.getFormMargin();
    formLayout.marginHeight = PropsUi.getFormMargin();

    shell.setLayout(formLayout);
    shell.setText(BaseMessages.getString(PKG, "AggregateDialog.Shell.Title"));

    Control titleArea = this.createTitleArea(shell);

    // The title separator line
    Label titleSeparator = new Label(shell, SWT.HORIZONTAL | SWT.SEPARATOR);
    titleSeparator.setLayoutData(
        new FormDataBuilder().top(titleArea, PropsUi.getFormMargin()).fullWidth().result());
    PropsUi.setLook(titleSeparator);

    SashForm wSashForm = new SashForm(shell, SWT.VERTICAL);
    PropsUi.setLook(wSashForm);

    Composite wGroupComposite = new Composite(wSashForm, SWT.NONE);
    wGroupComposite.setLayout(new FormLayout());
    PropsUi.setLook(wGroupComposite);
    this.createGroupFields(wGroupComposite);

    Composite wAggregateComposite = new Composite(wSashForm, SWT.NONE);
    wAggregateComposite.setLayout(new FormLayout());
    PropsUi.setLook(wAggregateComposite);
    this.createAggregateFields(wAggregateComposite);

    // THE BUTTONS
    wOk = new Button(shell, SWT.PUSH);
    wOk.setText(BaseMessages.getString(PKG, "System.Button.OK"));
    wOk.addListener(SWT.Selection, e -> ok());
    wCancel = new Button(shell, SWT.PUSH);
    wCancel.setText(BaseMessages.getString(PKG, "System.Button.Cancel"));
    wCancel.addListener(SWT.Selection, e -> cancel());

    setButtonPositions(new Button[] {wOk, wCancel}, PropsUi.getMargin(), null);

    // Always pass a result rows as output
    //
    wAlwaysAddResult = new Button(shell, SWT.CHECK);
    wAlwaysAddResult.setText(BaseMessages.getString(PKG, "AggregateDialog.AlwaysAddResult.Label"));
    wAlwaysAddResult
        .setToolTipText(BaseMessages.getString(PKG, "AggregateDialog.AlwaysAddResult.ToolTip"));
    PropsUi.setLook(wAlwaysAddResult);
    FormData fdAlwaysAddResult = new FormData();
    fdAlwaysAddResult.left = new FormAttachment(0, 0);
    fdAlwaysAddResult.bottom = new FormAttachment(wOk, -PropsUi.getMargin());
    fdAlwaysAddResult.right = new FormAttachment(100, 0);
    wAlwaysAddResult.setLayoutData(fdAlwaysAddResult);
    wAlwaysAddResult.addListener(SWT.Selection, e -> input.setChanged());


    FormData fdlGroupComposite = new FormData();
    fdlGroupComposite.left = new FormAttachment(0, 0);
    fdlGroupComposite.top = new FormAttachment(titleSeparator, PropsUi.getMargin());
    fdlGroupComposite.right = new FormAttachment(100, 0);
    fdlGroupComposite.bottom = new FormAttachment(wAlwaysAddResult, -PropsUi.getMargin());
    wSashForm.setLayoutData(fdlGroupComposite);
    wSashForm.setWeights(30, 70);

    getData();
    input.setChanged(backupChanged);

    BaseDialog.defaultShellHandling(shell, c -> ok(), c -> cancel());

    return transformName;
  }

  // Search the fields in the background
  protected CompletableFuture<IRowMeta> getAsyncRowMeta(IVariables variables,
      PipelineMeta pipelineMeta, String transformName) {
    return CompletableFuture.supplyAsync(() -> {
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
    label.setText(BaseMessages.getString("System.Label.TransformName"));
    //label.setText(BaseMessages.getString(PKG, "System.TransformName.Label"));
    //label.setToolTipText(BaseMessages.getString(PKG, "System.TransformName.Tooltip"));
    label.setLayoutData(new FormDataBuilder().top().left().right(icon, 100).result());
    PropsUi.setLook(label);

    // Widget Transform name
    wTransformName = new Text(composite, SWT.SINGLE | SWT.LEFT | SWT.BORDER);
    wTransformName.setLayoutData(
        new FormDataBuilder().top(label).left().right(icon, -PropsUi.getMargin()).result());
    wTransformName.addListener(SWT.Modify, event -> input.setChanged());
    PropsUi.setLook(wTransformName);
    
    return composite;
  }

  protected void createGroupFields(final Composite parent) {

    Label wlGroup = new Label(parent, SWT.NONE);
    wlGroup.setText(BaseMessages.getString(PKG, "AggregateDialog.Group.Label"));
    PropsUi.setLook(wlGroup);
    FormData fdlGroup = new FormData();
    fdlGroup.left = new FormAttachment(0, 0);
    fdlGroup.top = new FormAttachment(0, 0);
    wlGroup.setLayoutData(fdlGroup);

    int nrKeyCols = 1;
    int nrKeyRows = (input.getGroupFields() != null ? input.getGroupFields().size() : 1);

    columnInfoGroup = new ColumnInfo[nrKeyCols];
    columnInfoGroup[0] =
        new ColumnInfo(BaseMessages.getString(PKG, "AggregateDialog.ColumnInfo.GroupField"),
            ColumnInfo.COLUMN_TYPE_CCOMBO, new String[] {""}, false);

    wGroup = new TableView(variables, parent,
        SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL, columnInfoGroup,
        nrKeyRows, e -> input.setChanged(), props);

    Button wGet = new Button(parent, SWT.PUSH);
    wGet.setText(BaseMessages.getString(PKG, "AggregateDialog.GetFields.Button"));
    wGet.addListener(SWT.Selection, e -> getGroupField());
    FormData fdGet = new FormData();
    fdGet.top = new FormAttachment(wlGroup, PropsUi.getMargin());
    fdGet.right = new FormAttachment(100, 0);
    wGet.setLayoutData(fdGet);

    FormData fdGroup = new FormData();
    fdGroup.left = new FormAttachment(0, 0);
    fdGroup.top = new FormAttachment(wlGroup, PropsUi.getMargin());
    fdGroup.right = new FormAttachment(wGet, -PropsUi.getMargin());
    fdGroup.bottom = new FormAttachment(100, 0);
    wGroup.setLayoutData(fdGroup);
  }

  protected void createAggregateFields(final Composite parent) {
    // THE Aggregate fields
    Label wlAgg = new Label(parent, SWT.NONE);
    wlAgg.setText(BaseMessages.getString(PKG, "AggregateDialog.Aggregates.Label"));
    PropsUi.setLook(wlAgg);
    FormData fdlAgg = new FormData();
    fdlAgg.left = new FormAttachment(0, 0);
    fdlAgg.top = new FormAttachment(wGroup, PropsUi.getMargin());
    wlAgg.setLayoutData(fdlAgg);

    CompletableFuture<IRowMeta> rowMeta =
        getAsyncRowMeta(getVariables(), pipelineMeta, transformName);
    rowMeta.thenAccept(this::setComboBoxes);

    int upInsRows = (input.getAggregateFields() != null ? input.getAggregateFields().size() : 1);

    ColumnInfo[] columnInfoAggregate = new ColumnInfo[2];
    columnInfoAggregate[0] =
        new ColumnInfo(BaseMessages.getString(PKG, "AggregateDialog.ColumnInfo.Name"),
            ColumnInfo.COLUMN_TYPE_TEXT, false);
    columnInfoAggregate[1] =
        new ColumnInfo(BaseMessages.getString(PKG, "AggregateDialog.ColumnInfo.Expression"),
            ColumnInfo.COLUMN_TYPE_TEXT_BUTTON, false);
    columnInfoAggregate[1].setUsingVariables(true);
    columnInfoAggregate[1].setTextVarButtonSelectionListener(new SelectionAdapter() {
      @Override
      public void widgetSelected(SelectionEvent e) {

        String expression =
            wAggregate.getActiveTableItem().getText(wAggregate.getActiveTableColumn());

        if (!shell.isDisposed()) {
          ExpressionEditorDialog dialog = new ExpressionEditorDialog(shell);
          expression = dialog.open(expression, getVariables(), ExpressionMode.COLUMN, rowMeta);
          if (expression != null) {
            wAggregate.getActiveTableItem().setText(wAggregate.getActiveTableColumn(), expression);
          }
        }
      }
    });

    wAggregate = new TableView(variables, parent,
        SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL,
        columnInfoAggregate, upInsRows, e -> input.setChanged(), props);


    FormData fdAgg = new FormData();
    fdAgg.left = new FormAttachment(0, 0);
    fdAgg.top = new FormAttachment(wlAgg, PropsUi.getMargin());
    fdAgg.right = new FormAttachment(100, 0);
    fdAgg.bottom = new FormAttachment(100, 0);
    wAggregate.setLayoutData(fdAgg);
    wAggregate.getTable().addListener(SWT.Resize, new ColumnsResizer(4, 20, 76));
  }


  protected void setComboBoxes(final IRowMeta rowMeta) {

    List<String> fields = new ArrayList<>();
    for (int i = 0; i < rowMeta.size(); i++) {
      fields.add(rowMeta.getValueMeta(i).getName());
    }

    String[] fieldNames = fields.toArray(new String[fields.size()]);

    Const.sortStrings(fieldNames);
    columnInfoGroup[0].setComboValues(fieldNames);
  }

  public Image getImage() {

    IPlugin plugin = PluginRegistry.getInstance().getPlugin(TransformPluginType.class,
        this.transformMeta.getPluginId());

    if (plugin.getImageFile() != null) {
      return SwtSvgImageUtil.getImage(shell.getDisplay(), getClass().getClassLoader(),
          plugin.getImageFile(), ConstUi.LARGE_ICON_SIZE, ConstUi.LARGE_ICON_SIZE);
    }

    return GuiResource.getInstance().getImageError();
  }


  /** Copy information from the meta-data input to the dialog fields. */
  public void getData() {
    logDebug(BaseMessages.getString(PKG, "AggregateDialog.Log.GettingKeyInfo"));

    wAlwaysAddResult.setSelection(input.isAlwaysGivingBackOneRow());

    List<GroupField> groups = input.getGroupFields();
    if (groups != null) {
      for (int i = 0; i < groups.size(); i++) {
        GroupField group = groups.get(i);
        TableItem item = wGroup.table.getItem(i);
        if (group != null) {
          item.setText(1, group.getName());
        }
      }
    }
    wGroup.setRowNums();
    wGroup.optWidth(true);

    List<AggregateField> aggregates = input.getAggregateFields();
    if (aggregates != null) {
      for (int i = 0; i < aggregates.size(); i++) {
        AggregateField aggregate = aggregates.get(i);
        TableItem item = wAggregate.table.getItem(i);
        if (aggregate.getName() != null) {
          item.setText(1, aggregate.getName());
        }
        if (aggregate.getExpression() != null) {
          item.setText(2, aggregate.getExpression());
        }
      }
    }
    wAggregate.setRowNums();
    wAggregate.optWidth(true);

    wTransformName.setText(transformName);
    wTransformName.selectAll();
    wTransformName.setFocus();
  }

  private void cancel() {
    transformName = null;
    input.setChanged(backupChanged);
    dispose();
  }

  private void ok() {
    if (Utils.isEmpty(wTransformName.getText())) {
      return;
    }

    input.setAlwaysGivingBackOneRow(wAlwaysAddResult.getSelection());

    List<GroupField> groups = new ArrayList<>();
    int sizegroup = wGroup.nrNonEmpty();
    for (int i = 0; i < sizegroup; i++) {
      TableItem item = wGroup.getNonEmpty(i);
      GroupField group = new GroupField();
      group.setName(item.getText(1));
      groups.add(group);
    }
    input.setGroupFields(groups);

    List<AggregateField> aggregates = new ArrayList<>();
    int nrFields = wAggregate.nrNonEmpty();
    for (int i = 0; i < nrFields; i++) {
      TableItem item = wAggregate.getNonEmpty(i);
      AggregateField aggregate = new AggregateField();
      aggregate.setName(item.getText(1));
      aggregate.setExpression(item.getText(2));
      aggregates.add(aggregate);
    }
    input.setAggregateFields(aggregates);

    transformName = wTransformName.getText();

    dispose();
  }

  private void getGroupField() {
    try {
      IRowMeta r = pipelineMeta.getPrevTransformFields(variables, transformName);
      if (r != null && !r.isEmpty()) {
        BaseTransformDialog.getFieldsFromPrevious(r, wGroup, 1, new int[] {1}, new int[] {}, -1, -1,
            null);
      }
    } catch (HopException ke) {
      new ErrorDialog(shell,
          BaseMessages.getString(PKG, "AggregateDialog.FailedToGetFields.DialogTitle"),
          BaseMessages.getString(PKG, "AggregateDialog.FailedToGetFields.DialogMessage"), ke);
    }
  }
}
