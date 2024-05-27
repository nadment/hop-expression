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

package org.apache.hop.workflow.actions.loop;

import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang.StringUtils;
import org.apache.hop.core.Const;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.ui.core.PropsUi;
import org.apache.hop.ui.core.dialog.BaseDialog;
import org.apache.hop.ui.core.dialog.MessageBox;
import org.apache.hop.ui.core.metadata.MetadataManager;
import org.apache.hop.ui.core.widget.ColumnInfo;
import org.apache.hop.ui.core.widget.MetaSelectionLine;
import org.apache.hop.ui.core.widget.TableView;
import org.apache.hop.ui.core.widget.TextVar;
import org.apache.hop.ui.expression.ExpressionText;
import org.apache.hop.ui.hopgui.file.workflow.HopWorkflowFileType;
import org.apache.hop.ui.pipeline.transform.BaseTransformDialog;
import org.apache.hop.ui.workflow.action.ActionDialog;
import org.apache.hop.ui.workflow.dialog.WorkflowDialog;
import org.apache.hop.workflow.WorkflowMeta;
import org.apache.hop.workflow.action.IAction;
import org.apache.hop.workflow.action.IActionDialog;
import org.apache.hop.workflow.config.WorkflowRunConfiguration;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

public class LoopActionDialog extends ActionDialog implements IActionDialog {

  private static final Class<?> PKG = LoopActionDialog.class; // For Translator

  private static final String COLON_SEPARATOR = " : ";

  private LoopAction action;

  private Text wName;
  private TextVar wFilename;
  private ExpressionText wCondition;
  private TableView wParameters;
  private MetaSelectionLine<WorkflowRunConfiguration> wRunConfiguration;

  public LoopActionDialog(
      Shell parent, IAction action, WorkflowMeta workflowMeta, IVariables variables) {
    super(parent, workflowMeta, variables);
    this.action = (LoopAction) action;

    if (this.action.getName() == null) {
      this.action.setName(BaseMessages.getString(PKG, "LoopAction.Name"));
    }
  }

  @Override
  public IAction open() {

    Shell parent = getParent();

    shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.MIN | SWT.MAX | SWT.RESIZE);
    PropsUi.setLook(shell);
    WorkflowDialog.setShellImage(shell, action);

    FormLayout formLayout = new FormLayout();
    formLayout.marginWidth = PropsUi.getFormMargin();
    formLayout.marginHeight = PropsUi.getFormMargin();

    shell.setLayout(formLayout);
    shell.setText(BaseMessages.getString(PKG, "LoopActionDialog.Title"));

    int middle = props.getMiddlePct();
    int margin = PropsUi.getMargin();

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
    FormData fdName = new FormData();
    fdName.left = new FormAttachment(middle, 0);
    fdName.top = new FormAttachment(0, margin);
    fdName.right = new FormAttachment(100, 0);
    wName.setLayoutData(fdName);
    Control lastControl = wName;

    Label wlFilename = new Label(shell, SWT.RIGHT);
    wlFilename.setText(BaseMessages.getString(PKG, "LoopActionDialog.Workflow.Label"));
    wlFilename.setToolTipText(BaseMessages.getString(PKG, "LoopActionDialog.Workflow.Tooltip"));
    PropsUi.setLook(wlFilename);
    FormData fdlFilename = new FormData();
    fdlFilename.left = new FormAttachment(0, 0);
    fdlFilename.right = new FormAttachment(middle, -margin);
    fdlFilename.top = new FormAttachment(lastControl, margin);
    wlFilename.setLayoutData(fdlFilename);

    // The filename browse button
    //
    // Browse for a file
    Button wbbFilename = new Button(shell, SWT.PUSH | SWT.CENTER);
    PropsUi.setLook(wbbFilename);
    wbbFilename.setText(BaseMessages.getString(PKG, "System.Button.Browse"));
    wbbFilename.setToolTipText(
        BaseMessages.getString(PKG, "System.Tooltip.BrowseForFileOrDirAndAdd"));
    FormData fdbFilename = new FormData();
    fdbFilename.top = new FormAttachment(wlFilename, 0, SWT.CENTER);
    fdbFilename.right = new FormAttachment(100, 0);
    wbbFilename.setLayoutData(fdbFilename);
    wbbFilename.addListener(SWT.Selection, e -> browseForFile());

    wFilename = new TextVar(variables, shell, SWT.SINGLE | SWT.LEFT | SWT.BORDER);
    PropsUi.setLook(wFilename);
    FormData fdFilename = new FormData();
    fdFilename.left = new FormAttachment(middle, 0);
    fdFilename.right = new FormAttachment(wbbFilename, -margin);
    fdFilename.top = new FormAttachment(wlFilename, 0, SWT.CENTER);
    wFilename.setLayoutData(fdFilename);
    lastControl = wFilename;

    Label wlRunConfiguration = new Label(shell, SWT.RIGHT);
    wlRunConfiguration.setText(
        BaseMessages.getString(PKG, "LoopActionDialog.RunConfiguration.Label"));
    wlRunConfiguration.setToolTipText(
        BaseMessages.getString(PKG, "LoopActionDialog.RunConfiguration.Tooltip"));
    PropsUi.setLook(wlRunConfiguration);
    FormData fdlRunConfiguration = new FormData();
    fdlRunConfiguration.left = new FormAttachment(0, 0);
    fdlRunConfiguration.top = new FormAttachment(lastControl, Const.isOSX() ? 0 : 5);
    fdlRunConfiguration.right = new FormAttachment(middle, -margin);
    wlRunConfiguration.setLayoutData(fdlRunConfiguration);

    wRunConfiguration =
        new MetaSelectionLine<>(
            variables,
            metadataProvider,
            WorkflowRunConfiguration.class,
            shell,
            SWT.BORDER,
            null,
            null,
            true);
    PropsUi.setLook(wRunConfiguration);
    FormData fdRunConfiguration = new FormData();
    fdRunConfiguration.left = new FormAttachment(middle, 0);
    fdRunConfiguration.top = new FormAttachment(wlRunConfiguration, 0, SWT.CENTER);
    fdRunConfiguration.right = new FormAttachment(100, 0);
    wRunConfiguration.setLayoutData(fdRunConfiguration);
    lastControl = wRunConfiguration;

    Label wlCondition = new Label(shell, SWT.RIGHT);
    wlCondition.setText(BaseMessages.getString(PKG, "LoopActionDialog.Condition.Label"));
    wlCondition.setToolTipText(BaseMessages.getString(PKG, "LoopActionDialog.Condition.Tooltip"));
    PropsUi.setLook(wlCondition);
    FormData fdlCondition = new FormData();
    fdlCondition.left = new FormAttachment(0, 0);
    fdlCondition.right = new FormAttachment(middle, -margin);
    fdlCondition.top = new FormAttachment(lastControl, margin);
    wlCondition.setLayoutData(fdlCondition);
    wCondition =
        new ExpressionText(
            action.getVariablesAndParameters(), shell, SWT.SINGLE | SWT.LEFT | SWT.BORDER);
    PropsUi.setLook(wCondition);
    FormData fdCondition = new FormData();
    fdCondition.left = new FormAttachment(middle, 0);
    fdCondition.right = new FormAttachment(100, 0);
    fdCondition.top = new FormAttachment(wlCondition, 0, SWT.CENTER);
    wCondition.setLayoutData(fdCondition);
    lastControl = wCondition;

    // Parameters
    //
    Label wlParameters = new Label(shell, SWT.LEFT);
    wlParameters.setText(BaseMessages.getString(PKG, "LoopActionDialog.Parammeters.Label"));
    PropsUi.setLook(wlParameters);
    FormData fdlParameters = new FormData();
    fdlParameters.left = new FormAttachment(0, 0);
    fdlParameters.top = new FormAttachment(lastControl, 2 * margin);
    fdlParameters.right = new FormAttachment(100, 0);
    wlParameters.setLayoutData(fdlParameters);
    lastControl = wlParameters;

    // Add buttons first, then the script field can use dynamic sizing
    //
    Button wOK = new Button(shell, SWT.PUSH);
    wOK.setText(BaseMessages.getString(PKG, "System.Button.OK"));
    wOK.addListener(SWT.Selection, e -> ok());
    Button wCancel = new Button(shell, SWT.PUSH);
    wCancel.setText(BaseMessages.getString(PKG, "System.Button.Cancel"));
    wCancel.addListener(SWT.Selection, e -> cancel());

    // Put these buttons at the bottom
    //
    BaseTransformDialog.positionBottomButtons(
        shell,
        new Button[] {
          wOK, wCancel,
        },
        margin,
        null);

    ColumnInfo[] columnInfos =
        new ColumnInfo[] {
          new ColumnInfo(
              BaseMessages.getString(PKG, "LoopActionDialog.Parammeters.Name.Column.Header"),
              ColumnInfo.COLUMN_TYPE_TEXT,
              false,
              false),
          new ColumnInfo(
              BaseMessages.getString(PKG, "LoopActionDialog.Parammeters.Value.Column.Header"),
              ColumnInfo.COLUMN_TYPE_TEXT,
              false,
              false),
        };
    columnInfos[1].setUsingVariables(true);

    wParameters =
        new TableView(
            variables, shell, SWT.BORDER, columnInfos, action.getParameters().size(), null, props);
    PropsUi.setLook(wParameters);
    FormData fdParameters = new FormData();
    fdParameters.left = new FormAttachment(0, 0);
    fdParameters.right = new FormAttachment(100, 0);
    fdParameters.top = new FormAttachment(lastControl, margin);
    fdParameters.bottom = new FormAttachment(wOK, -margin * 2);
    wParameters.setLayoutData(fdParameters);
    lastControl = wParameters;

    getData();

    BaseDialog.defaultShellHandling(shell, c -> ok(), c -> cancel());

    return action;
  }

  private void browseForFile() {
    HopWorkflowFileType<WorkflowMeta> workflowFileType = new HopWorkflowFileType<>();
    BaseDialog.presentFileDialog(
        shell,
        wFilename,
        variables,
        workflowFileType.getFilterExtensions(),
        workflowFileType.getFilterNames(),
        true);
  }

  private void cancel() {
    action = null;
    dispose();
  }

  private void getData() {
    wName.setText(Const.NVL(action.getName(), ""));
    wFilename.setText(Const.NVL(action.getFilename(), ""));
    wCondition.setText(Const.NVL(action.getCondition(), ""));

    int rowNr = 0;
    for (Parameter parameter : action.getParameters()) {
      TableItem item = wParameters.table.getItem(rowNr++);
      item.setText(1, Const.NVL(parameter.getName(), ""));
      item.setText(2, Const.NVL(parameter.getValue(), ""));
    }
    wParameters.setRowNums();
    wParameters.optWidth(true);

    // Get the workflow run configurations
    //
    MetadataManager<WorkflowRunConfiguration> workflowMetaManager =
        new MetadataManager<>(
            variables, getMetadataProvider(), WorkflowRunConfiguration.class, shell);
    List<String> entries = new ArrayList<>();
    try {
      workflowMetaManager.getNames().forEach(name -> entries.add(name));
    } catch (Exception e) {
      // Ignore
    }
    wRunConfiguration.setItems(entries.toArray(new String[0]));
    if (StringUtils.isNotEmpty(action.getRunConfigurationName())) {
      wRunConfiguration.setText(action.getRunConfigurationName());
    }

    wName.selectAll();
    wName.setFocus();
  }

  private void ok() {
    if (Utils.isEmpty(wName.getText())) {
      MessageBox mb = new MessageBox(shell, SWT.OK | SWT.ICON_ERROR);
      mb.setText(BaseMessages.getString(PKG, "LoopActionDialog.Dialog.ActionMissing.Header"));
      mb.setMessage(BaseMessages.getString(PKG, "LoopActionDialog.Dialog.ActionMissing.Message"));
      mb.open();
      return;
    }
    action.setName(wName.getText());
    action.setFilename(wFilename.getText());
    action.setCondition(wCondition.getText());
    action.getParameters().clear();
    for (int i = 0; i < wParameters.nrNonEmpty(); i++) {
      TableItem item = wParameters.getNonEmpty(i);
      action.getParameters().add(new Parameter(item.getText(1), item.getText(2)));
    }

    // Get the name of the run configuration:
    //
    String runConfigRaw = wRunConfiguration.getText();
    if (StringUtils.isEmpty(runConfigRaw)) {
      action.setRunConfigurationName(null);
    } else {
      int colonIndex = runConfigRaw.indexOf(COLON_SEPARATOR);
      if (colonIndex > 0 && colonIndex + COLON_SEPARATOR.length() < runConfigRaw.length()) {
        action.setRunConfigurationName(
            runConfigRaw.substring(colonIndex + COLON_SEPARATOR.length()));
      } else {
        action.setRunConfigurationName(runConfigRaw);
      }
    }

    action.setChanged();

    dispose();
  }
}
