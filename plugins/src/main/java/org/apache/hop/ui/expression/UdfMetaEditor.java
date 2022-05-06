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
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.Argument;
import org.apache.hop.expression.DataType;
import org.apache.hop.expression.UdfMeta;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.ui.core.PropsUi;
import org.apache.hop.ui.core.dialog.ErrorDialog;
import org.apache.hop.ui.core.metadata.MetadataEditor;
import org.apache.hop.ui.core.metadata.MetadataManager;
import org.apache.hop.ui.core.widget.ColumnInfo;
import org.apache.hop.ui.core.widget.TableView;
import org.apache.hop.ui.hopgui.HopGui;
import org.apache.hop.ui.hopgui.perspective.metadata.MetadataPerspective;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

public class UdfMetaEditor extends MetadataEditor<UdfMeta> {
  private static final Class<?> PKG = UdfMetaEditor.class; // For Translator

  private Text wName;
  private Text wDescription;
  private TableView wParams;
  private ExpressionEditor wExpression;

  public UdfMetaEditor(HopGui hopGui, MetadataManager<UdfMeta> manager, UdfMeta udf) {
    super(hopGui, manager, udf);
  }

  @Override
  public void createControl(Composite parent) {

    PropsUi props = PropsUi.getInstance();

    int margin = Const.MARGIN;

    // Add listener to detect change after loading data
    Listener modifyListener =
        event -> {
          setChanged();
          MetadataPerspective.getInstance().updateEditor(this);
        };
    
    // The icon
    //
    Label wIcon = new Label(parent, SWT.RIGHT);
    wIcon.setImage(getImage());
    FormData fdlicon = new FormData();
    fdlicon.top = new FormAttachment(0, 0);
    fdlicon.right = new FormAttachment(100, 0);
    wIcon.setLayoutData(fdlicon);
    props.setLook(wIcon);

    // The name
    //
    Label wlName = new Label(parent, SWT.RIGHT);
    props.setLook(wlName);
    wlName.setText(BaseMessages.getString(PKG, "UdfDialog.Name.Label"));
    FormData fdlName = new FormData();
    fdlName.top = new FormAttachment(0, 0);
    fdlName.left = new FormAttachment(0, 0);
    wlName.setLayoutData(fdlName);

    wName = new Text(parent, SWT.SINGLE | SWT.LEFT | SWT.BORDER);
    props.setLook(wName);
    FormData fdName = new FormData();
    fdName.top = new FormAttachment(wlName, 5);
    fdName.left = new FormAttachment(0, 0);
    fdName.right = new FormAttachment(wIcon, -5);
    wName.setLayoutData(fdName);
    wName.addListener(SWT.Modify, modifyListener);

    Label spacer = new Label(parent, SWT.HORIZONTAL | SWT.SEPARATOR);
    FormData fdSpacer = new FormData();
    fdSpacer.left = new FormAttachment(0, 0);
    fdSpacer.top = new FormAttachment(wName, 15);
    fdSpacer.right = new FormAttachment(100, 0);
    spacer.setLayoutData(fdSpacer);

    // The description of the user function...
    //
    Label wlDescription = new Label(parent, SWT.LEFT);
    props.setLook(wlDescription);
    wlDescription.setText(BaseMessages.getString(PKG, "UdfDialog.Description.Label"));
    FormData fdlDescription = new FormData();
    fdlDescription.top = new FormAttachment(spacer, margin);
    fdlDescription.left = new FormAttachment(0, 0);
    fdlDescription.right = new FormAttachment(100, 0);
    wlDescription.setLayoutData(fdlDescription);
    wDescription = new Text(parent, SWT.MULTI | SWT.LEFT | SWT.V_SCROLL | SWT.BORDER);
    props.setLook(wDescription);
    FormData fdDescription = new FormData();
    fdDescription.height = 50;
    fdDescription.top = new FormAttachment(wlDescription, margin);
    fdDescription.left = new FormAttachment(0, 0);
    fdDescription.right = new FormAttachment(100, 0);
    wDescription.setLayoutData(fdDescription);
    wDescription.addListener(SWT.Modify, modifyListener);

    // The arguments of the user function
    //
    Label wlArguments = new Label(parent, SWT.NONE);
    wlArguments.setText(BaseMessages.getString(PKG, "UdfDialog.Arguments.Label"));
    props.setLook(wlArguments);
    FormData fdlParams = new FormData();
    fdlParams.left = new FormAttachment(0, 0);
    fdlParams.top = new FormAttachment(wDescription, margin * 2);
    wlArguments.setLayoutData(fdlParams);

    ColumnInfo[] columns = new ColumnInfo[] {
        new ColumnInfo(BaseMessages.getString(PKG, "UdfDialog.ColumnInfo.Name"),
            ColumnInfo.COLUMN_TYPE_CCOMBO, new String[] {""}, false),
        new ColumnInfo(BaseMessages.getString(PKG, "UdfDialog.ColumnInfo.Type"),
            ColumnInfo.COLUMN_TYPE_CCOMBO, DataType.getDisplayNames(), false)};

    wParams = new TableView(new Variables(), parent,
        SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL, columns, getMetadata().getArguments().size(), null,
        props);

    FormData fdArguments = new FormData();
    fdArguments.left = new FormAttachment(0, 0);
    fdArguments.top = new FormAttachment(wlArguments, margin);
    fdArguments.right = new FormAttachment(100, 0);
    fdArguments.bottom = new FormAttachment(40, 0);
    wParams.setLayoutData(fdArguments);
    wParams.addListener(SWT.Modify, modifyListener);

    // The expression
    //
    Label wlExpression = new Label(parent, SWT.NONE);
    wlExpression.setText(BaseMessages.getString(PKG, "UdfDialog.Expression.Label"));
    props.setLook(wlExpression);
    FormData fdlSource = new FormData();
    fdlSource.left = new FormAttachment(0, 0);
    fdlSource.top = new FormAttachment(wParams, margin * 2);
    wlExpression.setLayoutData(fdlSource);
    
    wExpression = new ExpressionEditor(parent, SWT.BORDER, this.getVariables(), null);
    FormData fdExression = new FormData();
    fdExression.left = new FormAttachment(0, 0);
    fdExression.top = new FormAttachment(wlExpression, margin);
    fdExression.right = new FormAttachment(100, 0);
    fdExression.bottom = new FormAttachment(100, -2 * margin);
    wExpression.setLayoutData(fdExression);
    wExpression.addListener(SWT.Modify, modifyListener);

    this.setWidgetsContent();
    
    // Some widget set changed
    this.resetChanged();
  }


  @Override
  public void setWidgetsContent() {
    UdfMeta udf = getMetadata();

    wName.setText(Const.NVL(udf.getName(), ""));
    wDescription.setText(Const.NVL(udf.getDescription(), ""));
    wExpression.setText(Const.NVL(udf.getSource(), ""));    
    for (int i = 0; i < udf.getArguments().size(); i++) {
      Argument argument = udf.getArguments().get(i);
      wParams.setText(Const.NVL(argument.getName(), ""), 1, i);
      if ( argument.getType()!=null ) {
        wParams.setText(Const.NVL(argument.getType().name(), ""), 2, i);
      }
    }
  }

  @Override
  public void getWidgetsContent(UdfMeta udf) {
    udf.setName(wName.getText());
    udf.setDescription(wDescription.getText());
    udf.setSource(wExpression.getText());
    udf.getArguments().clear();
    int nrFields = wParams.nrNonEmpty();
    for (int i = 0; i < nrFields; i++) {
      TableItem item = wParams.getNonEmpty(i);
      String name = item.getText(1);
      DataType dataType = null;
      try {
        dataType = DataType.of(item.getText(2));
      } catch (Exception e) {
       
      }
      Argument argument = new Argument(name, dataType);
      udf.getArguments().add(argument);
    }
  }

  @Override
  public void save() throws HopException {

    try {
      // verifySettings();
    } catch (Exception e) {
      new ErrorDialog(getShell(), "Error",
          BaseMessages.getString(PKG, "UdfDialog.Error.ValidationError"), e);
    }

    getWidgetsContent(getMetadata());

    super.save();;
  }

  @Override
  public boolean setFocus() {
    if (wName == null || wName.isDisposed()) {
      return false;
    }
    return wName.setFocus();
  }
}
