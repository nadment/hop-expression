/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
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
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.ITransformDialog;
import org.apache.hop.pipeline.transform.TransformMeta;
import org.apache.hop.ui.core.dialog.BaseDialog;
import org.apache.hop.ui.core.dialog.ErrorDialog;
import org.apache.hop.ui.core.widget.ColumnInfo;
import org.apache.hop.ui.core.widget.ColumnsResizer;
import org.apache.hop.ui.core.widget.TableView;
import org.apache.hop.ui.expression.ExpressionEditorDialog;
import org.apache.hop.ui.expression.ExpressionMode;
import org.apache.hop.ui.pipeline.transform.BaseTransformDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
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

  private final AggregateMeta input;

  private ColumnInfo[] ciGroup;


  public AggregateDialog(
      Shell parent, IVariables variables, Object in, PipelineMeta pipelineMeta, String sname) {
    super(parent, variables, (AggregateMeta) in, pipelineMeta, sname);
    input = (AggregateMeta) in;
  }

  @Override
  public String open() {
    Shell parent = getParent();

    shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MAX | SWT.MIN);
    props.setLook(shell);
    setShellImage(shell, input);

    ModifyListener lsMod = e -> input.setChanged();
    SelectionListener lsSel =
        new SelectionAdapter() {
          @Override
          public void widgetSelected(SelectionEvent arg0) {
            input.setChanged();
          }
        };
    backupChanged = input.hasChanged();

    FormLayout formLayout = new FormLayout();
    formLayout.marginWidth = Const.FORM_MARGIN;
    formLayout.marginHeight = Const.FORM_MARGIN;

    shell.setLayout(formLayout);
    shell.setText(BaseMessages.getString(PKG, "AggregateDialog.Shell.Title"));

    int middle = props.getMiddlePct();
    int margin = props.getMargin();

    // TransformName line
    wlTransformName = new Label(shell, SWT.RIGHT);
    wlTransformName.setText(BaseMessages.getString(PKG, "AggregateDialog.TransformName.Label"));
    props.setLook(wlTransformName);
    fdlTransformName = new FormData();
    fdlTransformName.left = new FormAttachment(0, 0);
    fdlTransformName.right = new FormAttachment(middle, -margin);
    fdlTransformName.top = new FormAttachment(0, margin);
    wlTransformName.setLayoutData(fdlTransformName);
    wTransformName = new Text(shell, SWT.SINGLE | SWT.LEFT | SWT.BORDER);
    wTransformName.setText(transformName);
    props.setLook(wTransformName);
    wTransformName.addModifyListener(lsMod);
    fdTransformName = new FormData();
    fdTransformName.left = new FormAttachment(middle, 0);
    fdTransformName.top = new FormAttachment(0, margin);
    fdTransformName.right = new FormAttachment(100, 0);
    wTransformName.setLayoutData(fdTransformName);

    // Always pass a result rows as output
    //
    Label wlAlwaysAddResult = new Label(shell, SWT.RIGHT);
    wlAlwaysAddResult.setText(
        BaseMessages.getString(PKG, "AggregateDialog.AlwaysAddResult.Label"));
    wlAlwaysAddResult.setToolTipText(
        BaseMessages.getString(PKG, "AggregateDialog.AlwaysAddResult.ToolTip"));
    props.setLook(wlAlwaysAddResult);
    FormData fdlAlwaysAddResult = new FormData();
    fdlAlwaysAddResult.left = new FormAttachment(0, 0);
    fdlAlwaysAddResult.top = new FormAttachment(wTransformName, margin);
    fdlAlwaysAddResult.right = new FormAttachment(middle, -margin);
    wlAlwaysAddResult.setLayoutData(fdlAlwaysAddResult);
    wAlwaysAddResult = new Button(shell, SWT.CHECK);
    wAlwaysAddResult.setToolTipText(
        BaseMessages.getString(PKG, "AggregateDialog.AlwaysAddResult.ToolTip"));
    props.setLook(wAlwaysAddResult);
    FormData fdAlwaysAddResult = new FormData();
    fdAlwaysAddResult.left = new FormAttachment(middle, 0);
    fdAlwaysAddResult.top = new FormAttachment(wlAlwaysAddResult, 0, SWT.CENTER);
    fdAlwaysAddResult.right = new FormAttachment(100, 0);
    wAlwaysAddResult.setLayoutData(fdAlwaysAddResult);
    wAlwaysAddResult.addSelectionListener(lsSel);

    Label wlGroup = new Label(shell, SWT.NONE);
    wlGroup.setText(BaseMessages.getString(PKG, "AggregateDialog.Group.Label"));
    props.setLook(wlGroup);
    FormData fdlGroup = new FormData();
    fdlGroup.left = new FormAttachment(0, 0);
    fdlGroup.top = new FormAttachment(wlAlwaysAddResult, 2 * margin);
    wlGroup.setLayoutData(fdlGroup);

    int nrKeyCols = 1;
    int nrKeyRows = (input.getGroupFields() != null ? input.getGroupFields().size() : 1);

    ciGroup = new ColumnInfo[nrKeyCols];
    ciGroup[0] =
        new ColumnInfo(
            BaseMessages.getString(PKG, "AggregateDialog.ColumnInfo.GroupField"),
            ColumnInfo.COLUMN_TYPE_CCOMBO,
            new String[] {""},
            false);

    wGroup =
        new TableView(
            variables,
            shell,
            SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL,
            ciGroup,
            nrKeyRows,
            lsMod,
            props);

    Button wGet = new Button(shell, SWT.PUSH);
    wGet.setText(BaseMessages.getString(PKG, "AggregateDialog.GetFields.Button"));
    wGet.addListener(SWT.Selection, e -> getGroupField());
    FormData fdGet = new FormData();
    fdGet.top = new FormAttachment(wlGroup, margin);
    fdGet.right = new FormAttachment(100, 0);
    wGet.setLayoutData(fdGet);

    FormData fdGroup = new FormData();
    fdGroup.left = new FormAttachment(0, 0);
    fdGroup.top = new FormAttachment(wlGroup, margin);
    fdGroup.right = new FormAttachment(wGet, -margin);
    fdGroup.bottom = new FormAttachment(45, 0);
    wGroup.setLayoutData(fdGroup);

    // THE Aggregate fields
    Label wlAgg = new Label(shell, SWT.NONE);
    wlAgg.setText(BaseMessages.getString(PKG, "AggregateDialog.Aggregates.Label"));
    props.setLook(wlAgg);
    FormData fdlAgg = new FormData();
    fdlAgg.left = new FormAttachment(0, 0);
    fdlAgg.top = new FormAttachment(wGroup, margin);
    wlAgg.setLayoutData(fdlAgg);

    CompletableFuture<IRowMeta> rowMeta = getAsyncRowMeta(getVariables(), pipelineMeta, transformName);
    rowMeta.thenAccept(this::setComboBoxes);
    
    int upInsRows = (input.getAggregateFields() != null ? input.getAggregateFields().size() : 1);

    ColumnInfo[] ciReturn = new ColumnInfo[2];
    ciReturn[0] =
        new ColumnInfo(
            BaseMessages.getString(PKG, "AggregateDialog.ColumnInfo.Name"),
            ColumnInfo.COLUMN_TYPE_TEXT,
            false);
    ciReturn[1] =
        new ColumnInfo(
            BaseMessages.getString(PKG, "AggregateDialog.ColumnInfo.Expression"),
            ColumnInfo.COLUMN_TYPE_TEXT_BUTTON,
            false);
    ciReturn[1].setUsingVariables(true);
    ciReturn[1].setTextVarButtonSelectionListener(new SelectionAdapter() {
      @Override
      public void widgetSelected(SelectionEvent e) {

        String expression =
            wAggregate.getActiveTableItem().getText(wAggregate.getActiveTableColumn());

        if (!shell.isDisposed()) {
          ExpressionEditorDialog dialog = new ExpressionEditorDialog(shell);
          expression = dialog.open(expression, getVariables(), ExpressionMode.COLUMN, rowMeta);
          if (expression != null) {
            wAggregate.getActiveTableItem().setText(wAggregate.getActiveTableColumn(),
                expression);
          }
        }
      }
    });
    
    wAggregate =
        new TableView(
            variables,
            shell,
            SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL,
            ciReturn,
            upInsRows,
            lsMod,
            props);   
    
    // THE BUTTONS
    wOk = new Button(shell, SWT.PUSH);
    wOk.setText(BaseMessages.getString(PKG, "System.Button.OK"));
    wOk.addListener(SWT.Selection, e -> ok());
    wCancel = new Button(shell, SWT.PUSH);
    wCancel.setText(BaseMessages.getString(PKG, "System.Button.Cancel"));
    wCancel.addListener(SWT.Selection, e -> cancel());

    setButtonPositions(new Button[] {wOk, wCancel}, margin, null);

    FormData fdAgg = new FormData();
    fdAgg.left = new FormAttachment(0, 0);
    fdAgg.top = new FormAttachment(wlAgg, margin);
    fdAgg.right = new FormAttachment(100, 0);
    fdAgg.bottom = new FormAttachment(wOk, -margin);
    wAggregate.setLayoutData(fdAgg);
    wAggregate.getTable().addListener(SWT.Resize, new ColumnsResizer(4, 20, 76));
    
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
  
  protected void setComboBoxes(final IRowMeta rowMeta) {
    
    List<String> fields = new ArrayList<>();
    for (int i = 0; i < rowMeta.size(); i++) {
      fields.add(rowMeta.getValueMeta(i).getName());
    }
    
    String[] fieldNames = fields.toArray(new String[fields.size()]);

    Const.sortStrings(fieldNames);
    ciGroup[0].setComboValues(fieldNames);
  }

  /** Copy information from the meta-data input to the dialog fields. */
  public void getData() {
    logDebug(BaseMessages.getString(PKG, "AggregateDialog.Log.GettingKeyInfo"));

    wAlwaysAddResult.setSelection(input.isAlwaysGivingBackOneRow());
    
    List<GroupField> groups =  input.getGroupFields();
    if (groups != null) {
      for (int i = 0; i < groups.size(); i++) {
        GroupField group = groups.get(i);
        TableItem item = wGroup.table.getItem(i);
        if (group != null) {
           item.setText(1, group.getName());
        }
      }
    }

    List<AggregateField> aggregates =  input.getAggregateFields();
    if (aggregates != null) {
      for (int i = 0; i < aggregates.size(); i++) {
        AggregateField aggregate = aggregates.get(i);
        TableItem item = wAggregate.table.getItem(i);
        if ( aggregate.getName() != null) {
          item.setText(1, aggregate.getName());
        }
        if ( aggregate.getExpression() != null) {
          item.setText(2, aggregate.getExpression());
        }
      }
    }

    wGroup.setRowNums();
    wGroup.optWidth(true);
    wAggregate.setRowNums();
    wAggregate.optWidth(true);

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
        BaseTransformDialog.getFieldsFromPrevious(
            r, wGroup, 1, new int[] {1}, new int[] {}, -1, -1, null);
      }
    } catch (HopException ke) {
      new ErrorDialog(
          shell,
          BaseMessages.getString(PKG, "AggregateDialog.FailedToGetFields.DialogTitle"),
          BaseMessages.getString(PKG, "AggregateDialog.FailedToGetFields.DialogMessage"),
          ke);
    }
  }
}
