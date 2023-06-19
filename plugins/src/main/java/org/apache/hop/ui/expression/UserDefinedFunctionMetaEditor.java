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

import org.apache.hop.core.Const;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.gui.plugin.GuiPlugin;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.FunctionArgument;
import org.apache.hop.expression.UserDefinedFunction;
import org.apache.hop.expression.UserDefinedFunctionMeta;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.ui.core.PropsUi;
import org.apache.hop.ui.core.metadata.MetadataEditor;
import org.apache.hop.ui.core.metadata.MetadataManager;
import org.apache.hop.ui.core.widget.ColumnInfo;
import org.apache.hop.ui.core.widget.TableView;
import org.apache.hop.ui.hopgui.HopGui;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

@GuiPlugin(description = "This is the editor for User Defined Function (UDF) metadata")
public class UserDefinedFunctionMetaEditor extends MetadataEditor<UserDefinedFunctionMeta> {
  private static final Class<?> PKG = UserDefinedFunctionMetaEditor.class; // For Translator

  private Text wName;
  private Text wDescription;
  private TableView wArguments;
  private ExpressionEditor wExpression;

  public UserDefinedFunctionMetaEditor(HopGui hopGui,
      MetadataManager<UserDefinedFunctionMeta> manager, UserDefinedFunctionMeta udf) {
    super(hopGui, manager, udf);
  }

  @Override
  public void createControl(Composite parent) {

    PropsUi props = PropsUi.getInstance();

    int margin = PropsUi.getMargin();

    // Add listener to detect change after loading data
    Listener changedListener = e -> setChanged();

    // The icon
    //
    Label wIcon = new Label(parent, SWT.RIGHT);
    wIcon.setImage(getImage());
    FormData fdlicon = new FormData();
    fdlicon.top = new FormAttachment(0, 0);
    fdlicon.right = new FormAttachment(100, 0);
    wIcon.setLayoutData(fdlicon);
    PropsUi.setLook(wIcon);

    // The name
    //
    Label wlName = new Label(parent, SWT.RIGHT);
    PropsUi.setLook(wlName);
    wlName.setText(BaseMessages.getString(PKG, "UdfDialog.Name.Label"));
    FormData fdlName = new FormData();
    fdlName.top = new FormAttachment(0, 0);
    fdlName.left = new FormAttachment(0, 0);
    wlName.setLayoutData(fdlName);

    wName = new Text(parent, SWT.SINGLE | SWT.LEFT | SWT.BORDER);
    PropsUi.setLook(wName);
    FormData fdName = new FormData();
    fdName.top = new FormAttachment(wlName, 5);
    fdName.left = new FormAttachment(0, 0);
    fdName.right = new FormAttachment(wIcon, -5);
    wName.setLayoutData(fdName);
    wName.addListener(SWT.Modify, changedListener);

    Label spacer = new Label(parent, SWT.HORIZONTAL | SWT.SEPARATOR);
    FormData fdSpacer = new FormData();
    fdSpacer.left = new FormAttachment(0, 0);
    fdSpacer.top = new FormAttachment(wName, 15);
    fdSpacer.right = new FormAttachment(100, 0);
    spacer.setLayoutData(fdSpacer);

    // The description of the user function...
    //
    Label wlDescription = new Label(parent, SWT.LEFT);
    PropsUi.setLook(wlDescription);
    wlDescription.setText(BaseMessages.getString(PKG, "UdfDialog.Description.Label"));
    FormData fdlDescription = new FormData();
    fdlDescription.top = new FormAttachment(spacer, margin);
    fdlDescription.left = new FormAttachment(0, 0);
    fdlDescription.right = new FormAttachment(100, 0);
    wlDescription.setLayoutData(fdlDescription);
    wDescription = new Text(parent, SWT.MULTI | SWT.LEFT | SWT.V_SCROLL | SWT.BORDER);
    PropsUi.setLook(wDescription);
    FormData fdDescription = new FormData();
    fdDescription.height = 50;
    fdDescription.top = new FormAttachment(wlDescription, margin);
    fdDescription.left = new FormAttachment(0, 0);
    fdDescription.right = new FormAttachment(100, 0);
    wDescription.setLayoutData(fdDescription);
    wDescription.addListener(SWT.Modify, changedListener);

    // The arguments of the user function
    //
    Label wlArguments = new Label(parent, SWT.NONE);
    wlArguments.setText(BaseMessages.getString(PKG, "UdfDialog.Arguments.Label"));
    PropsUi.setLook(wlArguments);
    FormData fdlParams = new FormData();
    fdlParams.left = new FormAttachment(0, 0);
    fdlParams.top = new FormAttachment(wDescription, margin * 2);
    wlArguments.setLayoutData(fdlParams);

    ColumnInfo[] columns = new ColumnInfo[] {
        new ColumnInfo(BaseMessages.getString(PKG, "UdfDialog.ColumnInfo.Name"),
            ColumnInfo.COLUMN_TYPE_CCOMBO, new String[] {""}, false),
        new ColumnInfo(BaseMessages.getString(PKG, "UdfDialog.ColumnInfo.Type"),
            ColumnInfo.COLUMN_TYPE_CCOMBO, TypeName.ALL_NAMES.toArray(new String[0]), false)};

    wArguments = new TableView(new Variables(), parent,
        SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL, columns,
        getMetadata().getArguments().size(), e -> {
          updateField();
          setChanged();
        }, props);

    FormData fdArguments = new FormData();
    fdArguments.left = new FormAttachment(0, 0);
    fdArguments.top = new FormAttachment(wlArguments, margin);
    fdArguments.right = new FormAttachment(100, 0);
    fdArguments.bottom = new FormAttachment(40, 0);
    wArguments.setLayoutData(fdArguments);
    wArguments.addListener(SWT.Modify, changedListener);

    // The expression
    //
    Label wlExpression = new Label(parent, SWT.NONE);
    wlExpression.setText(BaseMessages.getString(PKG, "UdfDialog.Expression.Label"));
    PropsUi.setLook(wlExpression);
    FormData fdlSource = new FormData();
    fdlSource.left = new FormAttachment(0, 0);
    fdlSource.top = new FormAttachment(wArguments, margin * 2);
    wlExpression.setLayoutData(fdlSource);

    wExpression =
        new ExpressionEditor(parent, SWT.BORDER, manager.getVariables(), ExpressionMode.UDF, null);
    FormData fdExression = new FormData();
    fdExression.left = new FormAttachment(0, 0);
    fdExression.top = new FormAttachment(wlExpression, margin);
    fdExression.right = new FormAttachment(100, 0);
    fdExression.bottom = new FormAttachment(100, -2 * margin);
    wExpression.setLayoutData(fdExression);
    wExpression.addListener(SWT.Modify, changedListener);

    this.setWidgetsContent();

    // Some widget set changed
    this.resetChanged();
  }

  protected void updateField() {

    UserDefinedFunctionMeta meta = new UserDefinedFunctionMeta();
    this.getWidgetsContent(meta);

    wExpression.setRowMeta(UserDefinedFunction.createRowMetaFromArguments(meta.getArguments()));
  }

  @Override
  public void setWidgetsContent() {
    UserDefinedFunctionMeta udf = getMetadata();

    wName.setText(Const.NVL(udf.getName(), ""));
    wDescription.setText(Const.NVL(udf.getDescription(), ""));
    wExpression.setText(Const.NVL(udf.getSource(), ""));
    for (int i = 0; i < udf.getArguments().size(); i++) {
      FunctionArgument argument = udf.getArguments().get(i);
      wArguments.setText(Const.NVL(argument.getName(), ""), 1, i);
      if (argument.getType() != null) {
        wArguments.setText(Const.NVL(argument.getType().name(), ""), 2, i);
      }
    }

    this.updateField();
  }

  @Override
  public void getWidgetsContent(UserDefinedFunctionMeta udf) {
    udf.setName(wName.getText());
    udf.setDescription(wDescription.getText());
    udf.setSource(wExpression.getText());
    udf.getArguments().clear();
    int nrFields = wArguments.nrNonEmpty();
    for (int i = 0; i < nrFields; i++) {
      TableItem item = wArguments.getNonEmpty(i);
      String name = item.getText(1);
      TypeName dataType = TypeName.of(item.getText(2));
      FunctionArgument argument = new FunctionArgument(name, dataType);
      udf.getArguments().add(argument);
    }
  }

  @Override
  public void save() throws HopException {

    UserDefinedFunctionMeta meta = getMetadata();
    getWidgetsContent(meta);

    super.save();
  }

  @Override
  public boolean setFocus() {
    if (wName == null || wName.isDisposed()) {
      return false;
    }
    return wName.setFocus();
  }
}
