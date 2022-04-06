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

package org.apache.hop.ui.expression;

import org.apache.hop.core.Const;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.row.value.ValueMetaFactory;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.Udf;
import org.apache.hop.expression.experimental.Param;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.ui.core.PropsUi;
import org.apache.hop.ui.core.dialog.ErrorDialog;
import org.apache.hop.ui.core.metadata.MetadataEditor;
import org.apache.hop.ui.core.metadata.MetadataManager;
import org.apache.hop.ui.core.widget.ColumnInfo;
import org.apache.hop.ui.core.widget.TableView;
import org.apache.hop.ui.hopgui.HopGui;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

public class UdfEditor extends MetadataEditor<Udf> {
  private static final Class<?> PKG = UdfEditor.class; // For Translator

  private Text wName;
  private Text wDescription;
  private TableView wParams;
  private ExpressionEditor wSource;
  
  public UdfEditor(HopGui hopGui, MetadataManager<Udf> manager, Udf udf) {
    super(hopGui, manager, udf);
  }

  @Override
  public void createControl(Composite parent) {

    PropsUi props = PropsUi.getInstance();

    int margin = Const.MARGIN;

    // The name of the group...
    //
    Label wIcon = new Label(parent, SWT.RIGHT);
    wIcon.setImage(getImage());
    FormData fdlicon = new FormData();
    fdlicon.top = new FormAttachment(0, 0);
    fdlicon.right = new FormAttachment(100, 0);
    wIcon.setLayoutData(fdlicon);
    props.setLook(wIcon);

    // What's the name
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

    Label spacer = new Label(parent, SWT.HORIZONTAL | SWT.SEPARATOR);
    FormData fdSpacer = new FormData();
    fdSpacer.left = new FormAttachment(0, 0);
    fdSpacer.top = new FormAttachment(wName, 15);
    fdSpacer.right = new FormAttachment(100, 0);
    spacer.setLayoutData(fdSpacer);

    // The description of the group...
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

        
    // The field mapping from the input to the data set...
    //
    Label wlParams = new Label(parent, SWT.NONE);
    wlParams.setText(BaseMessages.getString(PKG, "UdfDialog.Params.Label"));
    props.setLook(wlParams);
    FormData fdlParams = new FormData();
    fdlParams.left = new FormAttachment(0, 0);
    fdlParams.top = new FormAttachment(wDescription, margin * 2);
    wlParams.setLayoutData(fdlParams);

    // the field mapping grid in between
    //
    ColumnInfo[] columns =
        new ColumnInfo[] {
          new ColumnInfo(
              BaseMessages.getString(PKG, "UdfDialog.ColumnInfo.Name"),
              ColumnInfo.COLUMN_TYPE_CCOMBO,
              new String[] {""},
              false),
          new ColumnInfo(
              BaseMessages.getString(PKG, "UdfDialog.ColumnInfo.Type"),
              ColumnInfo.COLUMN_TYPE_CCOMBO,
              ValueMetaFactory.getAllValueMetaNames(),
              false)          
        };

    wParams =
        new TableView(
            new Variables(),
            parent,
            SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL,
            columns,
            0, //etadata().getFields().size(),
            null,
            props);

    FormData fdParams = new FormData();
    fdParams.left = new FormAttachment(0, 0);
    fdParams.top = new FormAttachment(wlParams, margin);
    fdParams.right = new FormAttachment(100, 0);
    //fdParams.bottom = new FormAttachment(100, -2 * margin);
    wParams.setLayoutData(fdParams);

    wSource = new ExpressionEditor(parent, SWT.NONE, new Variables(), null);
    FormData fdSource = new FormData();
    fdSource.left = new FormAttachment(0, 0);
    fdSource.top = new FormAttachment(wParams, margin);
    fdSource.right = new FormAttachment(100, 0);
    fdSource.bottom = new FormAttachment(100, -2 * margin);
    wSource.setLayoutData(fdSource);

    
    this.setWidgetsContent();

    // Add listener to detect change after loading data
    ModifyListener lsMod = e -> setChanged();
    wName.addModifyListener(lsMod);
    wDescription.addModifyListener(lsMod);
    wParams.addModifyListener(lsMod);
  }


  @Override
  public void setWidgetsContent() {
    Udf udf = getMetadata();

    wName.setText(Const.NVL(udf.getName(), ""));
    wDescription.setText(Const.NVL(udf.getDescription(), ""));
    wSource.setText(Const.NVL(udf.getSource(),""));
    for (int i = 0; i < udf.getParams().size(); i++) {
      Param param = udf.getParams().get(i);
      int colNr = 1;
      wParams.setText(Const.NVL(param.getName(), ""), colNr++, i);
  //    wFieldMapping.setText(ValueMetaFactory.getValueMetaName(field.getType()), colNr++, i);
    }
  }

  @Override
  public void getWidgetsContent(Udf udf) {
    udf.setName(wName.getText());
    udf.setDescription(wDescription.getText());
    udf.setSource(wSource.getText());
    udf.getParams().clear();
    int nrFields = wParams.nrNonEmpty();
    for (int i = 0; i < nrFields; i++) {
      TableItem item = wParams.getNonEmpty(i);      
      String name = item.getText(1);
      Param param = new Param(name);
      udf.getParams().add(param);
    }
  }

  @Override
  public void save() throws HopException {

    try {
   //   verifySettings();
    } catch (Exception e) {
      new ErrorDialog(
          getShell(),
          "Error",
          BaseMessages.getString(PKG, "UdfDialog.Error.ValidationError"),
          e);
    }

    getWidgetsContent(getMetadata());

    super.save();
    ;
  }

  @Override
  public boolean setFocus() {
    if (wName == null || wName.isDisposed()) {
      return false;
    }
    return wName.setFocus();
  }
}
