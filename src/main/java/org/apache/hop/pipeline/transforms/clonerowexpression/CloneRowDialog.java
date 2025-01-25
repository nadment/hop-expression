/*
 * ! ******************************************************************************
 *
 * Hop : The Hop Orchestration Platform
 *
 * http://www.project-hop.org
 *
 *******************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 ******************************************************************************/

package org.apache.hop.pipeline.transforms.clonerowexpression;

import java.util.concurrent.CompletableFuture;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.ITransformDialog;
import org.apache.hop.pipeline.transform.TransformMeta;
import org.apache.hop.ui.core.PropsUi;
import org.apache.hop.ui.core.dialog.BaseDialog;
import org.apache.hop.ui.expression.ExpressionMode;
import org.apache.hop.ui.expression.ExpressionText;
import org.apache.hop.ui.pipeline.transform.BaseTransformDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

public class CloneRowDialog extends BaseTransformDialog implements ITransformDialog {
  private static final Class<?> PKG = CloneRowDialog.class; // for i18n purposes, needed by
  // Translator!!

  private final CloneRowMeta input;

  // nr clones
  private ExpressionText wnrClone;

  private Label wladdCloneFlag;
  private Button waddCloneFlag;
  private Label wlcloneFlagField;
  private ExpressionText wcloneFlagField;

  private Label wladdCloneNum;
  private Button waddCloneNum;
  private Label wlCloneNumField;
  private ExpressionText wCloneNumField;

  public CloneRowDialog(
      Shell parent, IVariables variables, CloneRowMeta transformMeta, PipelineMeta pipelineMeta) {
    super(parent, variables, transformMeta, pipelineMeta);
    input = transformMeta;
  }

  @Override
  public String open() {
    Shell parent = getParent();

    shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MIN | SWT.MAX);
    PropsUi.setLook(shell);
    setShellImage(shell, input);

    ModifyListener lsMod =
        new ModifyListener() {
          public void modifyText(ModifyEvent e) {
            input.setChanged();
          }
        };
    changed = input.hasChanged();

    FormLayout formLayout = new FormLayout();
    formLayout.marginWidth = PropsUi.getFormMargin();
    formLayout.marginHeight = PropsUi.getFormMargin();

    shell.setLayout(formLayout);
    shell.setText(BaseMessages.getString(PKG, "CloneRowDialog.Shell.Title"));

    int middle = props.getMiddlePct();
    int margin = PropsUi.getMargin();

    // TransformName line
    wlTransformName = new Label(shell, SWT.RIGHT);
    wlTransformName.setText(BaseMessages.getString(PKG, "System.TransformName.Label"));
    wlTransformName.setToolTipText(BaseMessages.getString(PKG, "System.TransformName.Tooltip"));
    PropsUi.setLook(wlTransformName);

    fdlTransformName = new FormData();
    fdlTransformName.left = new FormAttachment(0, 0);
    fdlTransformName.right = new FormAttachment(middle, -margin);
    fdlTransformName.top = new FormAttachment(0, margin);
    wlTransformName.setLayoutData(fdlTransformName);
    wTransformName = new Text(shell, SWT.SINGLE | SWT.LEFT | SWT.BORDER);
    wTransformName.setText(transformName);
    PropsUi.setLook(wTransformName);

    wTransformName.addModifyListener(lsMod);
    fdTransformName = new FormData();
    fdTransformName.left = new FormAttachment(middle, 0);
    fdTransformName.top = new FormAttachment(0, margin);
    fdTransformName.right = new FormAttachment(100, 0);
    wTransformName.setLayoutData(fdTransformName);

    CompletableFuture<IRowMeta> rowMeta =
        getAsyncRowMeta(getVariables(), pipelineMeta, transformName);

    // Number of clones line
    Label wlnrClone = new Label(shell, SWT.RIGHT);
    wlnrClone.setText(BaseMessages.getString(PKG, "CloneRowDialog.nrClone.Label"));
    PropsUi.setLook(wlnrClone);
    FormData fdlnrClone = new FormData();
    fdlnrClone.left = new FormAttachment(0, 0);
    fdlnrClone.right = new FormAttachment(middle, -margin);
    fdlnrClone.top = new FormAttachment(wTransformName, margin * 2);
    wlnrClone.setLayoutData(fdlnrClone);

    wnrClone =
        new ExpressionText(
            this.getVariables(),
            shell,
            SWT.SINGLE | SWT.LEFT | SWT.BORDER,
            ExpressionMode.ROW,
            rowMeta);
    PropsUi.setLook(wnrClone);
    wnrClone.setToolTipText(BaseMessages.getString(PKG, "CloneRowDialog.nrClone.Tooltip"));
    wnrClone.addModifyListener(lsMod);
    FormData fdNrClone = new FormData();
    fdNrClone.left = new FormAttachment(middle, 0);
    fdNrClone.top = new FormAttachment(wTransformName, margin * 2);
    fdNrClone.right = new FormAttachment(100, 0);
    wnrClone.setLayoutData(fdNrClone);

    // ///////////////////////////////
    // START OF Origin files GROUP //
    // ///////////////////////////////

    Group wOutputFields = new Group(shell, SWT.SHADOW_NONE);
    PropsUi.setLook(wOutputFields);
    wOutputFields.setText(BaseMessages.getString(PKG, "CloneRowDialog.wOutpuFields.Label"));

    FormLayout fdOutpuFieldsgroupLayout = new FormLayout();
    fdOutpuFieldsgroupLayout.marginWidth = 10;
    fdOutpuFieldsgroupLayout.marginHeight = 10;
    wOutputFields.setLayout(fdOutpuFieldsgroupLayout);

    // add clone flag?
    wladdCloneFlag = new Label(wOutputFields, SWT.RIGHT);
    wladdCloneFlag.setText(BaseMessages.getString(PKG, "CloneRowDialog.addCloneFlag.Label"));
    PropsUi.setLook(wladdCloneFlag);
    FormData fdlAddCloneFlag = new FormData();
    fdlAddCloneFlag.left = new FormAttachment(0, 0);
    fdlAddCloneFlag.top = new FormAttachment(0, 0);
    fdlAddCloneFlag.right = new FormAttachment(middle, -margin);
    wladdCloneFlag.setLayoutData(fdlAddCloneFlag);
    waddCloneFlag = new Button(wOutputFields, SWT.CHECK);
    waddCloneFlag.setToolTipText(
        BaseMessages.getString(PKG, "CloneRowDialog.addCloneFlag.Tooltip"));
    PropsUi.setLook(waddCloneFlag);
    FormData fdaddCloneFlag = new FormData();
    fdaddCloneFlag.left = new FormAttachment(middle, 0);
    fdaddCloneFlag.top = new FormAttachment(0, 0);
    fdaddCloneFlag.right = new FormAttachment(100, 0);
    waddCloneFlag.setLayoutData(fdaddCloneFlag);
    SelectionAdapter lsSelR =
        new SelectionAdapter() {
          public void widgetSelected(SelectionEvent arg0) {
            input.setChanged();
            activeaddCloneFlag();
          }
        };
    waddCloneFlag.addSelectionListener(lsSelR);

    // clone falg field line
    wlcloneFlagField = new Label(wOutputFields, SWT.RIGHT);
    wlcloneFlagField.setText(BaseMessages.getString(PKG, "CloneRowDialog.cloneFlagField.Label"));
    PropsUi.setLook(wlcloneFlagField);
    FormData fdlCloneFlagField = new FormData();
    fdlCloneFlagField.left = new FormAttachment(0, 0);
    fdlCloneFlagField.right = new FormAttachment(middle, -margin);
    fdlCloneFlagField.top = new FormAttachment(waddCloneFlag, margin * 2);
    wlcloneFlagField.setLayoutData(fdlCloneFlagField);

    wcloneFlagField =
        new ExpressionText(getVariables(), wOutputFields, SWT.SINGLE | SWT.LEFT | SWT.BORDER);
    PropsUi.setLook(wcloneFlagField);
    wcloneFlagField.setToolTipText(
        BaseMessages.getString(PKG, "CloneRowDialog.cloneFlagField.Tooltip"));
    wcloneFlagField.addModifyListener(lsMod);
    FormData fdcloneFlagField = new FormData();
    fdcloneFlagField.left = new FormAttachment(middle, 0);
    fdcloneFlagField.top = new FormAttachment(waddCloneFlag, margin * 2);
    fdcloneFlagField.right = new FormAttachment(100, 0);
    wcloneFlagField.setLayoutData(fdcloneFlagField);

    // add clone num?
    wladdCloneNum = new Label(wOutputFields, SWT.RIGHT);
    wladdCloneNum.setText(BaseMessages.getString(PKG, "CloneRowDialog.addCloneNum.Label"));
    PropsUi.setLook(wladdCloneNum);
    FormData fdlAddCloneNum = new FormData();
    fdlAddCloneNum.left = new FormAttachment(0, 0);
    fdlAddCloneNum.top = new FormAttachment(wcloneFlagField, margin);
    fdlAddCloneNum.right = new FormAttachment(middle, -margin);
    wladdCloneNum.setLayoutData(fdlAddCloneNum);
    waddCloneNum = new Button(wOutputFields, SWT.CHECK);
    waddCloneNum.setToolTipText(BaseMessages.getString(PKG, "CloneRowDialog.addCloneNum.Tooltip"));
    PropsUi.setLook(waddCloneNum);
    FormData fdAddCloneNum = new FormData();
    fdAddCloneNum.left = new FormAttachment(middle, 0);
    fdAddCloneNum.top = new FormAttachment(wcloneFlagField, margin);
    fdAddCloneNum.right = new FormAttachment(100, 0);
    waddCloneNum.setLayoutData(fdAddCloneNum);
    waddCloneNum.addSelectionListener(
        new SelectionAdapter() {
          public void widgetSelected(SelectionEvent arg0) {
            input.setChanged();
            activeaddCloneNum();
          }
        });

    // clone num field line
    wlCloneNumField = new Label(wOutputFields, SWT.RIGHT);
    wlCloneNumField.setText(BaseMessages.getString(PKG, "CloneRowDialog.cloneNumField.Label"));
    PropsUi.setLook(wlCloneNumField);
    fdlCloneFlagField = new FormData();
    fdlCloneFlagField.left = new FormAttachment(0, 0);
    fdlCloneFlagField.right = new FormAttachment(middle, -margin);
    fdlCloneFlagField.top = new FormAttachment(waddCloneNum, margin);
    wlCloneNumField.setLayoutData(fdlCloneFlagField);

    wCloneNumField =
        new ExpressionText(getVariables(), wOutputFields, SWT.SINGLE | SWT.LEFT | SWT.BORDER);
    PropsUi.setLook(wCloneNumField);
    wCloneNumField.setToolTipText(
        BaseMessages.getString(PKG, "CloneRowDialog.cloneNumField.Tooltip"));
    wCloneNumField.addModifyListener(lsMod);
    FormData fdCloneNumField = new FormData();
    fdCloneNumField.left = new FormAttachment(middle, 0);
    fdCloneNumField.top = new FormAttachment(waddCloneNum, margin);
    fdCloneNumField.right = new FormAttachment(100, 0);
    wCloneNumField.setLayoutData(fdCloneNumField);

    FormData fdOutpuFields = new FormData();
    fdOutpuFields.left = new FormAttachment(0, margin);
    fdOutpuFields.top = new FormAttachment(wlnrClone, 2 * margin);
    fdOutpuFields.right = new FormAttachment(100, -margin);
    wOutputFields.setLayoutData(fdOutpuFields);

    // ///////////////////////////////////////////////////////////
    // / END OF Origin files GROUP
    // ///////////////////////////////////////////////////////////

    // Some buttons
    wOk = new Button(shell, SWT.PUSH);
    wOk.setText(BaseMessages.getString(PKG, "System.Button.OK"));
    wOk.addListener(SWT.Selection, e -> ok());
    wCancel = new Button(shell, SWT.PUSH);
    wCancel.setText(BaseMessages.getString(PKG, "System.Button.Cancel"));
    wCancel.addListener(SWT.Selection, e -> cancel());
    setButtonPositions(new Button[] {wOk, wCancel}, margin, wOutputFields);

    getData();
    activeaddCloneFlag();
    // ActiveisNrCloneInField();
    activeaddCloneNum();

    input.setChanged(changed);

    BaseDialog.defaultShellHandling(shell, c -> ok(), c -> cancel());

    return transformName;
  }

  private void activeaddCloneFlag() {
    wlcloneFlagField.setEnabled(waddCloneFlag.getSelection());
    wcloneFlagField.setEnabled(waddCloneFlag.getSelection());
  }

  private void activeaddCloneNum() {
    wlCloneNumField.setEnabled(waddCloneNum.getSelection());
    wCloneNumField.setEnabled(waddCloneNum.getSelection());
  }

  /** Copy information from the meta-data input to the dialog fields. */
  public void getData() {
    if (input.getNrClones() != null) {
      wnrClone.setText(input.getNrClones());
    }
    waddCloneFlag.setSelection(input.isAddCloneFlag());
    if (input.getCloneFlagField() != null) {
      wcloneFlagField.setText(input.getCloneFlagField());
    }

    waddCloneNum.setSelection(input.isAddCloneNum());
    if (input.getCloneNumField() != null) {
      wCloneNumField.setText(input.getCloneNumField());
    }

    wTransformName.selectAll();
    wTransformName.setFocus();
  }

  private void cancel() {
    transformName = null;
    input.setChanged(changed);
    dispose();
  }

  private void ok() {
    if (Utils.isEmpty(wTransformName.getText())) {
      return;
    }

    transformName = wTransformName.getText(); // return value
    input.setNrClones(wnrClone.getText());
    input.setAddCloneFlag(waddCloneFlag.getSelection());
    input.setCloneFlagField(wcloneFlagField.getText());
    input.setAddCloneNum(waddCloneNum.getSelection());
    input.setCloneNumField(wCloneNumField.getText());
    dispose();
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
}
