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

import org.apache.hop.core.Const;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.ui.core.PropsUi;
import org.apache.hop.ui.core.widget.ControlSpaceKeyAdapter;
import org.apache.hop.ui.core.widget.IGetCaretPosition;
import org.apache.hop.ui.core.widget.IInsertText;
import org.apache.hop.ui.util.SwtSvgImageUtil;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import java.util.concurrent.CompletableFuture;

/**
 * A Widget that combines a Text widget with a Variable button that will insert an Environment
 * variable. The tool tip of the text widget shows the content of the Text widget with expanded
 * variables.
 */
public class ExpressionText extends Composite {
  protected static final Class<?> PKG = ExpressionText.class; // for i18n purposes, needed by Translator!!

  //protected String toolTipText;

  protected IGetCaretPosition getCaretPositionInterface;

  protected IInsertText insertTextInterface;

  protected ControlSpaceKeyAdapter controlSpaceKeyAdapter;

  protected IVariables variables;

  protected IRowMeta rowMeta;

  protected Text wText;

  protected ModifyListener modifyListenerTooltipText;

  protected ToolBar wToolBar;

  protected boolean isUseField;

  public ExpressionText(IVariables variables, Composite composite, int flags, boolean isUseField) {
    this(variables, composite, flags, null, null, null);

    this.isUseField = isUseField;
  }

  public ExpressionText(IVariables variables, Composite composite, int flags, String toolTipText) {
    this(variables, composite, flags, toolTipText, null, null);
  }

  public ExpressionText(IVariables variables, Composite composite, int flags,
      IGetCaretPosition getCaretPositionInterface, IInsertText insertTextInterface) {
    this(variables, composite, flags, null, getCaretPositionInterface, insertTextInterface);
  }

  public ExpressionText(IVariables variables, Composite composite, int flags, String toolTipText,
      IGetCaretPosition getCaretPositionInterface, IInsertText insertTextInterface) {
    super(composite, SWT.NONE);
    initialize(variables, composite, flags, toolTipText, getCaretPositionInterface,
        insertTextInterface, null);
  }

  public ExpressionText(Composite composite, IVariables variables, int flags,
      IGetCaretPosition getCaretPositionInterface, IInsertText insertTextInterface,
      SelectionListener selectionListener) {
    this(variables, composite, flags, null, getCaretPositionInterface, insertTextInterface,
        selectionListener);
  }

  public ExpressionText(IVariables variables, Composite composite, int flags, String toolTipText,
      IGetCaretPosition getCaretPositionInterface, IInsertText insertTextInterface,
      SelectionListener selectionListener) {
    super(composite, SWT.NONE);
    initialize(variables, composite, flags, toolTipText, getCaretPositionInterface,
        insertTextInterface, selectionListener);
  }

  protected void initialize(IVariables variables, Composite composite, int flags,
      String toolTipText, IGetCaretPosition getCaretPositionInterface,
      IInsertText insertTextInterface, SelectionListener selectionListener) {


    this.getCaretPositionInterface = getCaretPositionInterface;
    this.insertTextInterface = insertTextInterface;
    this.variables = variables;

    PropsUi.getInstance().setLook(this);

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
    Image image = SwtSvgImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(),
        "expression.svg", 14, // $NON-NLS-1$
        14);

    wToolBar = new ToolBar(this, SWT.FLAT);
    final ToolItem toolItem = new ToolItem(wToolBar, SWT.NONE);
    toolItem.setImage(image);
    toolItem.setToolTipText("Browse");
    toolItem.addSelectionListener(new SelectionAdapter() {
      @Override
      public void widgetSelected(SelectionEvent e) {
        openExpressionDialog(composite.getShell());
      }
    });

    wToolBar.setLayoutData(
        new GridData(GridData.VERTICAL_ALIGN_CENTER | GridData.HORIZONTAL_ALIGN_CENTER));

    modifyListenerTooltipText = getModifyListenerTooltipText(wText);
    wText.addModifyListener(modifyListenerTooltipText);

    controlSpaceKeyAdapter = new ControlSpaceKeyAdapter(variables, wText, getCaretPositionInterface,
        insertTextInterface);
    wText.addKeyListener(controlSpaceKeyAdapter);
    
    this.setToolTipText(toolTipText);
  }

  protected void openExpressionDialog(Shell shell) {
    ExpressionDialog dialog = new ExpressionDialog(shell);
    CompletableFuture<IRowMeta> rowMeta = null;
    String expression = dialog.open(wText.getText(), variables, rowMeta);
    if (expression != null) {
      wText.setText(expression);
    }
  }

  /** @return the getCaretPositionInterface */
  public IGetCaretPosition getGetCaretPositionInterface() {
    return getCaretPositionInterface;
  }

  /** @param getCaretPositionInterface the getCaretPositionInterface to set */
  public void setGetCaretPositionInterface(IGetCaretPosition getCaretPositionInterface) {
    this.getCaretPositionInterface = getCaretPositionInterface;
  }

  /** @return the insertTextInterface */
  public IInsertText getInsertTextInterface() {
    return insertTextInterface;
  }

  /** @param insertTextInterface the insertTextInterface to set */
  public void setInsertTextInterface(IInsertText insertTextInterface) {
    this.insertTextInterface = insertTextInterface;
  }

  protected ModifyListener getModifyListenerTooltipText(final Text textField) {
    return new ModifyListener() {
      public void modifyText(ModifyEvent e) {
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
    };
  }

  /** @return the text in the Text widget */
  public String getText() {
    return wText.getText();
  }

  /** @param text the text in the Text widget to set. */
  public void setText(String text) {
    wText.setText(text);
    modifyListenerTooltipText.modifyText(null);
  }

  public Text getTextWidget() {
    return wText;
  }

  public void setRowMeta(IRowMeta rowMeta) {
    this.rowMeta = rowMeta;
  }

  @Override
  public void addListener(int eventType, Listener listener) {
    wText.addListener(eventType, listener);
  }

  /**
   * Add a modify listener to the text widget
   *
   * @param modifyListener
   */
  public void addModifyListener(ModifyListener modifyListener) {
    wText.addModifyListener(modifyListener);
  }

  public void addSelectionListener(SelectionAdapter lsDef) {
    wText.addSelectionListener(lsDef);
  }

  @Override
  public void addKeyListener(KeyListener lsKey) {
    wText.addKeyListener(lsKey);
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
    modifyListenerTooltipText.modifyText(null);
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
    modifyListenerTooltipText.modifyText(null);
  }
}
