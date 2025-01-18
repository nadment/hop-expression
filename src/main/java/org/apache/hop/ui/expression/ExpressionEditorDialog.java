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
package org.apache.hop.ui.expression;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.ui.core.ConstUi;
import org.apache.hop.ui.core.FormDataBuilder;
import org.apache.hop.ui.core.PropsUi;
import org.apache.hop.ui.core.gui.WindowProperty;
import org.apache.hop.ui.pipeline.transform.BaseTransformDialog;
import org.apache.hop.ui.util.HelpUtils;
import org.apache.hop.ui.util.SwtSvgImageUtil;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/** This dialog allows you to edit expression. */
public class ExpressionEditorDialog extends Dialog {
  private static final Class<?> PKG = ExpressionEditorDialog.class;

  private String expression;
  private Shell shell;
  private ExpressionEditor wEditor;

  public ExpressionEditorDialog(Shell parent) {
    super(parent, SWT.APPLICATION_MODAL | SWT.SHEET);
  }

  public String open(String expression, IVariables variables) {
    return open(expression, variables, ExpressionMode.NONE, null);
  }

  public String open(
      String expression,
      IVariables variables,
      ExpressionMode mode,
      CompletableFuture<IRowMeta> rowMetaFutur) {
    shell = new Shell(getParent(), SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MAX | SWT.MIN);
    PropsUi.setLook(shell);
    shell.setText(BaseMessages.getString(PKG, "ExpressionEditorDialog.Shell.Title"));
    shell.setImage(
        SwtSvgImageUtil.getImage(
            Display.getCurrent(),
            getClass().getClassLoader(),
            "expression.svg",
            ConstUi.SMALL_ICON_SIZE,
            ConstUi.SMALL_ICON_SIZE));

    FormLayout layout = new FormLayout();
    layout.marginTop = PropsUi.getFormMargin();
    layout.marginBottom = PropsUi.getFormMargin();
    layout.marginLeft = PropsUi.getFormMargin();
    layout.marginRight = PropsUi.getFormMargin();
    shell.setLayout(layout);

    Button btnCancel = new Button(shell, SWT.PUSH);
    btnCancel.setText(BaseMessages.getString(PKG, "System.Button.Cancel"));
    btnCancel.addListener(SWT.Selection, event -> onCancelPressed());

    Button btnOK = new Button(shell, SWT.PUSH);
    btnOK.setText(BaseMessages.getString(PKG, "System.Button.OK"));
    btnOK.addListener(SWT.Selection, event -> onOkPressed());

    HelpUtils.createHelpButton(
        shell,
        "https://github.com/nadment/hop-expression/blob/master/plugins/src/main/doc/expression.adoc");

    List<Button> buttons = new ArrayList<>();
    buttons.add(btnOK);
    buttons.add(btnCancel);
    BaseTransformDialog.positionBottomButtons(
        shell, buttons.toArray(new Button[0]), PropsUi.getMargin(), null);

    // ExpressionMode mode = (rowMetaFutur==null) ? ExpressionMode.NONE:ExpressionMode.ROW;

    // The expression editor
    wEditor = new ExpressionEditor(shell, SWT.BORDER, variables, mode, rowMetaFutur);
    wEditor.setText(expression);
    wEditor.setLayoutData(
        new FormDataBuilder().top().bottom(btnOK, -PropsUi.getMargin()).left().right().result());

    BaseTransformDialog.setSize(shell);

    shell.open();
    Display display = shell.getDisplay();
    while (!shell.isDisposed()) {
      if (!display.readAndDispatch()) {
        display.sleep();
      }
    }

    return this.expression;
  }

  /** Brings the dialog to the front if the dialog is already open. */
  public void setActive() {
    if (shell != null && !shell.isDisposed()) {
      shell.setActive();
    }
  }

  public void dispose() {
    WindowProperty winprop = new WindowProperty(shell);
    PropsUi.getInstance().setScreen(winprop);
    shell.dispose();
  }

  protected void onOkPressed() {

    this.expression = wEditor.getText();

    dispose();
  }

  /** Called when the user cancels the dialog. Subclasses may override if desired. */
  protected void onCancelPressed() {

    this.expression = null;

    // Close the SWT dialog window
    dispose();
  }
}
