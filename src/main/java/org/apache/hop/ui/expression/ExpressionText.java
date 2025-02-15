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
package org.apache.hop.ui.expression;

import java.util.concurrent.CompletableFuture;
import org.apache.hop.core.Const;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.ui.core.ConstUi;
import org.apache.hop.ui.core.PropsUi;
import org.apache.hop.ui.core.widget.ControlSpaceKeyAdapter;
import org.apache.hop.ui.util.SwtSvgImageUtil;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;

/**
 * A Widget that combines a Text widget with a Variable button that will insert an Environment
 * variable. The tool tip of the text widget shows the content of the Text widget with expanded
 * variables.
 */
public class ExpressionText extends Composite {
  protected static final Class<?> PKG = ExpressionText.class; // for i18n purposes

  protected ControlSpaceKeyAdapter controlSpaceKeyAdapter;

  protected IVariables variables;

  protected Text wText;

  protected ToolBar wToolBar;

  protected ExpressionMode mode;
  protected CompletableFuture<IRowMeta> rowMetaFutur;

  public ExpressionText(IVariables variables, Composite composite, int flags) {
    this(variables, composite, flags, ExpressionMode.NONE, null);
  }

  public ExpressionText(
      IVariables variables,
      Composite composite,
      int flags,
      ExpressionMode mode,
      CompletableFuture<IRowMeta> rowMetaFutur) {

    super(composite, SWT.NONE);

    this.variables = variables;
    this.mode = mode;
    this.rowMetaFutur = rowMetaFutur;

    PropsUi.setLook(this);

    final GridLayout layout = new GridLayout(2, false);
    layout.marginHeight = 0;
    layout.marginWidth = 0;
    layout.verticalSpacing = 0;
    layout.horizontalSpacing = 2;
    this.setLayout(layout);

    // add a text field on it...
    wText = new Text(this, flags);
    wText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

    // add button
    Image image =
        SwtSvgImageUtil.getImage(
            Display.getCurrent(),
            getClass().getClassLoader(),
            "expression.svg",
            ConstUi.MEDIUM_ICON_SIZE,
            ConstUi.MEDIUM_ICON_SIZE);

    wToolBar = new ToolBar(this, SWT.FLAT);
    final ToolItem toolItem = new ToolItem(wToolBar, SWT.NONE);
    toolItem.setImage(image);
    toolItem.setToolTipText("Browse");
    toolItem.addListener(SWT.Selection, e -> openExpressionDialog());

    wToolBar.setLayoutData(
        new GridData(GridData.VERTICAL_ALIGN_FILL | GridData.HORIZONTAL_ALIGN_CENTER));

    wText.addListener(SWT.Modify, e -> updateTooltip(wText));

    controlSpaceKeyAdapter = new ControlSpaceKeyAdapter(variables, wText, null, null);
    wText.addKeyListener(controlSpaceKeyAdapter);
  }

  protected void openExpressionDialog() {
    ExpressionEditorDialog dialog = new ExpressionEditorDialog(this.getShell());
    String expression = dialog.open(wText.getText(), variables, mode, rowMetaFutur);
    if (expression != null) {
      wText.setText(expression);
    }
  }

  protected void updateTooltip(final Text textField) {
    if (textField.getEchoChar() == '\0') { // Can't show passwords ;-)

      String tip = textField.getText();

      if (!Utils.isEmpty(tip) && !Utils.isEmpty(getToolTipText())) {
        tip += Const.CR + Const.CR + getToolTipText();
      }

      if (Utils.isEmpty(tip)) {
        tip = getToolTipText();
      }
      textField.setToolTipText(variables.resolve(tip));
    }
  }

  /**
   * @return the text in the Text widget
   */
  public String getText() {
    return wText.getText();
  }

  /**
   * @param text the text in the Text widget to set.
   */
  public void setText(String text) {
    wText.setText(text);
  }

  public Text getTextWidget() {
    return wText;
  }

  @Override
  public void addListener(int eventType, Listener listener) {
    wText.addListener(eventType, listener);
  }

  /**
   * Add a modify listener to the text widget
   *
   * @param listener the listener to add
   */
  public void addModifyListener(ModifyListener listener) {
    wText.addModifyListener(listener);
  }

  public void addSelectionListener(SelectionListener listener) {
    wText.addSelectionListener(listener);
  }

  @Override
  public void addKeyListener(KeyListener listener) {
    wText.addKeyListener(listener);
  }

  @Override
  public void addFocusListener(FocusListener lsFocus) {
    wText.addFocusListener(lsFocus);
  }

  public void setEchoChar(char c) {
    wText.setEchoChar(c);
  }

  @Override
  public void setEnabled(boolean flag) {
    wText.setEnabled(flag);
    wToolBar.setEnabled(flag);
  }

  @Override
  public boolean setFocus() {
    return wText.setFocus();
  }

  @Override
  public void addTraverseListener(TraverseListener tl) {
    wText.addTraverseListener(tl);
  }

  @Override
  public void setToolTipText(String toolTipText) {
    super.setToolTipText(toolTipText);
    wText.setToolTipText(toolTipText);
  }

  public void setEditable(boolean editable) {
    wText.setEditable(editable);
  }

  public void setSelection(int i) {
    wText.setSelection(i);
  }

  public void selectAll() {
    wText.selectAll();
  }

  public void showSelection() {
    wText.showSelection();
  }

  public void setVariables(IVariables vars) {
    variables = vars;
    controlSpaceKeyAdapter.setVariables(variables);
  }
}
