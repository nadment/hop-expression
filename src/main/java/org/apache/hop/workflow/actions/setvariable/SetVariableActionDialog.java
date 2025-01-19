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

package org.apache.hop.workflow.actions.setvariable;

import java.util.ArrayList;
import java.util.List;
import org.apache.hop.core.Const;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.ui.core.PropsUi;
import org.apache.hop.ui.core.dialog.BaseDialog;
import org.apache.hop.ui.core.dialog.MessageBox;
import org.apache.hop.ui.core.widget.ColumnInfo;
import org.apache.hop.ui.core.widget.ColumnsResizer;
import org.apache.hop.ui.core.widget.TableView;
import org.apache.hop.ui.expression.ExpressionEditorDialog;
import org.apache.hop.ui.expression.ExpressionMode;
import org.apache.hop.ui.pipeline.transform.BaseTransformDialog;
import org.apache.hop.ui.workflow.action.ActionDialog;
import org.apache.hop.ui.workflow.dialog.WorkflowDialog;
import org.apache.hop.workflow.WorkflowMeta;
import org.apache.hop.workflow.action.IAction;
import org.apache.hop.workflow.action.IActionDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

/** This dialog allows you to edit the Set variables action settings. */
public class SetVariableActionDialog extends ActionDialog implements IActionDialog {
  private static final Class<?> PKG = SetVariableAction.class; // For Translator

  private Text wName;

  private SetVariableAction action;

  private TableView wFields;

  private boolean changed;

  public SetVariableActionDialog(
      Shell parent, SetVariableAction action, WorkflowMeta workflowMeta, IVariables variables) {
    super(parent, workflowMeta, variables);
    this.action = action;

    if (this.action.getName() == null) {
      this.action.setName(BaseMessages.getString(PKG, "SetVariableAction.Name.Default"));
    }
  }

  @Override
  public IAction open() {
    Shell parent = getParent();

    shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.MIN | SWT.MAX | SWT.RESIZE);
    PropsUi.setLook(shell);
    WorkflowDialog.setShellImage(shell, action);

    ModifyListener lsMod = e -> action.setChanged();
    changed = action.hasChanged();

    FormLayout formLayout = new FormLayout();
    formLayout.marginWidth = PropsUi.getFormMargin();
    formLayout.marginHeight = PropsUi.getFormMargin();

    shell.setLayout(formLayout);
    shell.setText(BaseMessages.getString(PKG, "SetVariableAction.Title"));

    int middle = props.getMiddlePct();
    int margin = PropsUi.getMargin();

    // Buttons go at the very bottom
    //
    Button wOk = new Button(shell, SWT.PUSH);
    wOk.setText(BaseMessages.getString(PKG, "System.Button.OK"));
    wOk.addListener(SWT.Selection, e -> ok());
    Button wCancel = new Button(shell, SWT.PUSH);
    wCancel.setText(BaseMessages.getString(PKG, "System.Button.Cancel"));
    wCancel.addListener(SWT.Selection, e -> cancel());
    BaseTransformDialog.positionBottomButtons(shell, new Button[] {wOk, wCancel}, margin, null);

    // Name line
    Label wlName = new Label(shell, SWT.RIGHT);
    wlName.setText(BaseMessages.getString(PKG, "System.ActionName.Label"));
    wlName.setToolTipText(BaseMessages.getString(PKG, "System.ActionName.Tooltip"));
    PropsUi.setLook(wlName);
    FormData fdlName = new FormData();
    fdlName.left = new FormAttachment(0, 0);
    fdlName.right = new FormAttachment(middle, -margin);
    fdlName.top = new FormAttachment(0, margin);
    wlName.setLayoutData(fdlName);
    wName = new Text(shell, SWT.SINGLE | SWT.LEFT | SWT.BORDER);
    PropsUi.setLook(wName);
    wName.addModifyListener(lsMod);
    FormData fdName = new FormData();
    fdName.left = new FormAttachment(middle, 0);
    fdName.top = new FormAttachment(0, margin);
    fdName.right = new FormAttachment(100, 0);
    wName.setLayoutData(fdName);

    Label wlFields = new Label(shell, SWT.NONE);
    wlFields.setText(BaseMessages.getString(PKG, "SetVariableAction.Variables.Label"));
    PropsUi.setLook(wlFields);
    FormData fdlFields = new FormData();
    fdlFields.left = new FormAttachment(0, 0);
    fdlFields.top = new FormAttachment(wName, margin);
    wlFields.setLayoutData(fdlFields);

    int rows = action.getVariableDefinitions().size();

    ColumnInfo[] colinf = {
      new ColumnInfo(
          BaseMessages.getString(PKG, "SetVariableAction.Fields.Column.Name"),
          ColumnInfo.COLUMN_TYPE_TEXT,
          false),
      new ColumnInfo(
          BaseMessages.getString(PKG, "SetVariableAction.Fields.Column.Expression"),
          ColumnInfo.COLUMN_TYPE_TEXT_BUTTON,
          false),
      new ColumnInfo(
          BaseMessages.getString(PKG, "SetVariableAction.Fields.Column.Scope"),
          ColumnInfo.COLUMN_TYPE_CCOMBO,
          SetVariableScope.getDescriptions(),
          false),
    };
    colinf[0].setUsingVariables(true);
    colinf[1].setUsingVariables(true);
    colinf[1].setTextVarButtonSelectionListener(
        new SelectionAdapter() {
          @Override
          public void widgetSelected(SelectionEvent e) {

            String expression =
                wFields.getActiveTableItem().getText(wFields.getActiveTableColumn());

            if (!shell.isDisposed()) {
              ExpressionEditorDialog dialog = new ExpressionEditorDialog(shell);
              expression = dialog.open(expression, action, ExpressionMode.NONE, null);
              if (expression != null) {
                wFields.getActiveTableItem().setText(wFields.getActiveTableColumn(), expression);
              }
            }
          }
        });

    wFields =
        new TableView(
            variables,
            shell,
            SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI,
            colinf,
            rows,
            lsMod,
            props);

    FormData fdFields = new FormData();
    fdFields.left = new FormAttachment(0, 0);
    fdFields.top = new FormAttachment(wlFields, margin);
    fdFields.right = new FormAttachment(100, 0);
    fdFields.bottom = new FormAttachment(wOk, -2 * margin);
    wFields.setLayoutData(fdFields);
    wFields.getTable().addListener(SWT.Resize, new ColumnsResizer(4, 20, 60, 16));

    setWidgetsContent(action);

    BaseDialog.defaultShellHandling(shell, c -> ok(), c -> cancel());

    return action;
  }

  /** Copy information from the meta-data input to the dialog fields. */
  protected void setWidgetsContent(final SetVariableAction meta) {
    wName.setText(Const.nullToEmpty(meta.getName()));

    if (action.getVariableDefinitions() != null) {
      int i = 0;
      for (SetVariableDefinition definition : meta.getVariableDefinitions()) {
        TableItem item = wFields.table.getItem(i++);
        item.setText(1, Const.nullToEmpty(definition.getName()));
        item.setText(2, Const.nullToEmpty(definition.getExpression()));
        if (definition.getScope() != null) {
          item.setText(3, definition.getScope().getDescription());
        }
      }
      wFields.setRowNums();
      wFields.optWidth(true);
    }

    wName.selectAll();
    wName.setFocus();
  }

  protected void getWidgetsContent(final SetVariableAction meta) {
    meta.setName(wName.getText());

    int nrItems = wFields.nrNonEmpty();
    List<SetVariableDefinition> list = new ArrayList<>();
    for (int i = 0; i < nrItems; i++) {
      TableItem item = wFields.getTable().getItem(i);

      String name = item.getText(1);
      String expression = item.getText(2);
      SetVariableScope scope = SetVariableScope.lookupDescription(item.getText(3));
      list.add(new SetVariableDefinition(name, expression, scope));
    }
    meta.setVariableDefinitions(list);
  }

  private void cancel() {
    action.setChanged(changed);
    action = null;
    dispose();
  }

  private void ok() {
    if (Utils.isEmpty(wName.getText())) {
      MessageBox mb = new MessageBox(shell, SWT.OK | SWT.ICON_ERROR);
      mb.setText(BaseMessages.getString(PKG, "System.TransformActionNameMissing.Title"));
      mb.setMessage(BaseMessages.getString(PKG, "System.ActionNameMissing.Msg"));
      mb.open();
      return;
    }

    getWidgetsContent(action);

    dispose();
  }
}
