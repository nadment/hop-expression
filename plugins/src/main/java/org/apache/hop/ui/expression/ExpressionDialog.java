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
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;

/**
 * This dialogs allows you to edit expression.
 */
public class ExpressionDialog extends Dialog {
  private static final Class<?> PKG = ExpressionDialog.class;

  private Shell shell;
  private ExpressionEditor wEditor;
  private String expression;

  public ExpressionDialog(Shell parent) {
    super(parent, SWT.APPLICATION_MODAL | SWT.SHEET);
  }

  public String open(String expression, IVariables variables, CompletableFuture<IRowMeta> rowMeta) {
    PropsUi props = PropsUi.getInstance();

    shell = new Shell(getParent(), SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MAX | SWT.MIN);
    props.setLook(shell);
    shell.setText(BaseMessages.getString(PKG, "ExpressionEditorDialog.Shell.Title"));
    shell.setImage(SwtSvgImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(),
        "expression.svg", ConstUi.SMALL_ICON_SIZE, ConstUi.SMALL_ICON_SIZE));

    FormLayout layout = new FormLayout();
    layout.marginTop = props.getMargin();
    layout.marginBottom = props.getMargin();
    layout.marginLeft = props.getMargin();
    layout.marginRight = props.getMargin();
    shell.setLayout(layout);

    Button btnCancel = new Button(shell, SWT.PUSH);
    btnCancel.setText(BaseMessages.getString(PKG, "System.Button.Cancel"));
    btnCancel.addListener(SWT.Selection, event -> onCancelPressed());

    Button btnOK = new Button(shell, SWT.PUSH);
    btnOK.setText(BaseMessages.getString(PKG, "System.Button.OK"));
    btnOK.addListener(SWT.Selection, event -> onOkPressed());

    HelpUtils.createHelpButton(shell,
        "https://github.com/nadment/hop-expression/blob/master/plugins/src/main/doc/expression.adoc");

    List<Button> buttons = new ArrayList<>();
    buttons.add(btnOK);
    buttons.add(btnCancel);
    BaseTransformDialog.positionBottomButtons(shell, buttons.toArray(new Button[0]),
        props.getMargin(), null);

    // The expression editor
    wEditor = new ExpressionEditor(shell, SWT.BORDER, variables, rowMeta);
    wEditor.setText(expression);
    wEditor.setLayoutData(
        new FormDataBuilder().top().bottom(btnOK, -props.getMargin()).left().right().result());

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
