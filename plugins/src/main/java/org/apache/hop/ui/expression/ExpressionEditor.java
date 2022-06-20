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

import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.VariableRegistry;
import org.apache.hop.expression.ExpressionBuilder;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.Udf;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.ui.core.FormDataBuilder;
import org.apache.hop.ui.core.PropsUi;
import org.apache.hop.ui.core.gui.GuiResource;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.ITextOperationTarget;
import org.eclipse.jface.text.rules.FastPartitioner;
import org.eclipse.jface.text.source.CompositeRuler;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.LineNumberRulerColumn;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceAdapter;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.CompletableFuture;

public class ExpressionEditor extends Composite {
  private static final Class<?> PKG = ExpressionEditor.class;
  
  private boolean showUdf;
  private boolean showField;
  private ExpressionLabelProvider labelProvider;
  private IVariables variables;
  private CompletableFuture<IRowMeta> rowMetaProvider;
  private SourceViewer sourceViewer;
  private SashForm sashForm;
  private Tree tree;
  private TreeItem treeItemField;

  public ExpressionEditor(Composite parent, int style, IVariables variables, boolean showField, boolean showUdf, CompletableFuture<IRowMeta> rowMetaProvider) {
    super(parent, style);
    this.variables = variables;
    this.showField = showField;
    this.showUdf = showUdf;
    this.rowMetaProvider = rowMetaProvider;
    this.labelProvider = new ExpressionLabelProvider();

    this.setLayout(new FormLayout());
    this.sashForm = new SashForm(this, SWT.HORIZONTAL);
    this.sashForm.setLayoutData(new FormDataBuilder().fullSize().result());
    this.createTree(sashForm);
    this.createEditor(sashForm);

    // When IRowMeta is ready   
    if ( rowMetaProvider!=null ) {
      rowMetaProvider.thenAccept(this::setRowMeta);
    }
    
    sashForm.setWeights(25, 75);    
  }

  protected void createEditor(final Composite parent) {

    PropsUi.getInstance().setLook(this);
    
    CompositeRuler ruler = new CompositeRuler(24);
    ruler.addDecorator(0, new LineNumberRulerColumn());
    sourceViewer = new SourceViewer(parent, ruler, SWT.H_SCROLL | SWT.V_SCROLL | SWT.MULTI);
    sourceViewer.getTextWidget().setFont(GuiResource.getInstance().getFontFixed());
    sourceViewer.getTextWidget()
        .setLayoutData(new FormDataBuilder().top().fullWidth().bottom().result());

    // In Chinese window, Ctrl-SPACE is reserved by system for input Chinese character.
    // Use Ctrl-ALT-SPACE instead.
    final int modifierKeys =
        (System.getProperty("user.language").equals("zh")) ? SWT.CTRL | SWT.ALT : SWT.CTRL;

    sourceViewer.getTextWidget().addListener(SWT.KeyDown, event -> {
      if (event.keyCode == SWT.SPACE && (event.stateMask & SWT.MODIFIER_MASK) == modifierKeys) {
        sourceViewer.doOperation(ISourceViewer.CONTENTASSIST_PROPOSALS);
      } else if (event.keyCode == SWT.F1) {
        // TODO: Help
        event.doit = false;
      }
    });
    PropsUi.getInstance().setLook(sourceViewer.getTextWidget());
    
    
    Menu menu = new Menu(getShell(), SWT.POP_UP);
    MenuItem undoItem = new MenuItem(menu, SWT.PUSH);
    undoItem.setText(BaseMessages.getString(PKG, "ExpressionEditor.Undo"));
    undoItem.addListener(SWT.Selection, e -> sourceViewer.doOperation(ITextOperationTarget.UNDO));
    MenuItem redoItem = new MenuItem(menu, SWT.PUSH);
    redoItem.setText(BaseMessages.getString(PKG, "ExpressionEditor.Redo"));
    redoItem.addListener(SWT.Selection, e -> sourceViewer.doOperation(ITextOperationTarget.REDO));
    new MenuItem(menu, SWT.SEPARATOR);
    MenuItem cutItem = new MenuItem(menu, SWT.PUSH);
    cutItem.setText(BaseMessages.getString(PKG, "ExpressionEditor.Cut"));
    cutItem.addListener(SWT.Selection, e -> sourceViewer.doOperation(ITextOperationTarget.CUT));
    MenuItem copyItem = new MenuItem(menu, SWT.PUSH);
    copyItem.setText(BaseMessages.getString(PKG, "ExpressionEditor.Copy"));
    copyItem.addListener(SWT.Selection, e -> sourceViewer.doOperation(ITextOperationTarget.COPY));
    MenuItem pasteItem = new MenuItem(menu, SWT.PUSH);
    pasteItem.setText(BaseMessages.getString(PKG, "ExpressionEditor.Paste"));
    pasteItem.addListener(SWT.Selection, e -> sourceViewer.doOperation(ITextOperationTarget.PASTE));
    new MenuItem(menu, SWT.SEPARATOR);
    MenuItem selectAllItem = new MenuItem(menu, SWT.PUSH);
    selectAllItem.setText(BaseMessages.getString(PKG, "ExpressionEditor.SelectAll"));
    selectAllItem.addListener(SWT.Selection, e -> sourceViewer.doOperation(ITextOperationTarget.SELECT_ALL));

    sourceViewer.getTextWidget().setMenu(menu);    
    sourceViewer.getTextWidget().addListener(SWT.MenuDetect, event -> {
      undoItem.setEnabled(sourceViewer.canDoOperation(ITextOperationTarget.UNDO));
      redoItem.setEnabled(sourceViewer.canDoOperation(ITextOperationTarget.REDO));
      cutItem.setEnabled(sourceViewer.canDoOperation(ITextOperationTarget.CUT));
      copyItem.setEnabled(sourceViewer.canDoOperation(ITextOperationTarget.COPY));
      pasteItem.setEnabled(sourceViewer.canDoOperation(ITextOperationTarget.PASTE));
    });

    Document doc = new Document("");
    ExpressionEditorConfiguration configuration =
        new ExpressionEditorConfiguration(variables, rowMetaProvider);
    IDocumentPartitioner partitioner = new FastPartitioner(new ExpressionPartitionScanner(),
        configuration.getConfiguredContentTypes(sourceViewer));
    partitioner.connect(doc);
    doc.setDocumentPartitioner(partitioner);
    sourceViewer.setDocument(doc);
    sourceViewer.configure(configuration);
  }

  protected void createTree(final Composite parent) {

    // Tree
    tree = new Tree(parent, SWT.H_SCROLL | SWT.V_SCROLL);
    tree.setLayoutData(new FormDataBuilder().top().fullWidth().bottom().result());
    PropsUi.getInstance().setLook(tree);

    // Create the drag source on the tree
    DragSource ds = new DragSource(tree, DND.DROP_MOVE);
    ds.setTransfer(TextTransfer.getInstance());
    ds.addDragListener(new DragSourceAdapter() {
      @Override
      public void dragStart(DragSourceEvent event) {
        TreeItem item = tree.getSelection()[0];

        if (item != null && item.getData() != null) {
          event.doit = true;
        } else
          event.doit = false;
      }

      @Override
      public void dragSetData(DragSourceEvent event) {        
        // Set the data to be the first selected item's text
        event.data = labelProvider.getText(tree.getSelection()[0].getData());
      }
    });

    if (this.showField) {
      treeItemField = new TreeItem(tree, SWT.NULL);
      treeItemField.setImage(GuiResource.getInstance().getImageFolder());
      treeItemField.setText(BaseMessages.getString(PKG, "ExpressionEditor.Tree.Fields.Label"));
    }

    TreeItem treeItemOperator = new TreeItem(tree, SWT.NULL);
    treeItemOperator.setImage(GuiResource.getInstance().getImageFolder());
    treeItemOperator.setText(BaseMessages.getString(PKG, "ExpressionEditor.Tree.Operators.Label"));

    Set<String> categories = new TreeSet<>();
    List<Operator> primaryOperators = new ArrayList<>();
    HashMap<String, String> mapDisplay = new HashMap<>();

    Set<Operator> operators = Operators.getOperators();         
   
    // Inventory operator unique identifier and category
    for (Operator operator : operators) {

      if ( !showUdf && operator instanceof Udf ) {
        continue;
      }
      
      if (!categories.contains(operator.getCategory())) {
        categories.add(operator.getCategory());
      }

      if (operator.getId().equals(operator.getName())) {
        primaryOperators.add(operator);
        mapDisplay.put(operator.getId(), operator.getName());
      }      
    }

    // Alias operator
    for (Operator operator : operators) {
      if (!operator.getId().equals(operator.getName())) {
        if (mapDisplay.containsKey(operator.getId())) {
          String str = mapDisplay.get(operator.getId());
          mapDisplay.replace(operator.getId(), String.join(", ", str, operator.getName()));
        } else {
          primaryOperators.add(operator);
          mapDisplay.put(operator.getId(), operator.getName());
        }
      }
    }

    // Create tree item category
    Map<String, TreeItem> items = new HashMap<>();
    for (String category : categories) {
      TreeItem item = new TreeItem(treeItemOperator, SWT.NULL);
      item.setImage(GuiResource.getInstance().getImageFolder());
      item.setText(category);
      items.put(category, item);
    }

    // Create tree item operator
    for (Operator operator : primaryOperators) {

      TreeItem parentItem = items.get(operator.getCategory());

      TreeItem item;
      if (parentItem == null)
        item = new TreeItem(tree, SWT.NULL);
      else
        item = new TreeItem(parentItem, SWT.NULL);
      item.setImage(labelProvider.getImage(operator));
      item.setText(mapDisplay.get(operator.getId()));
      item.setData(operator);
    }

    TreeItem treeItemVariable = new TreeItem(tree, SWT.NULL);
    treeItemVariable.setImage(GuiResource.getInstance().getImageFolder());
    treeItemVariable.setText(BaseMessages.getString(PKG, "ExpressionEditor.Tree.Variables.Label"));

    if (variables != null) {
      String[] names = this.variables.getVariableNames();
      Arrays.sort(names);

      treeItemVariable.removeAll();
      for (String name : names) {
        boolean isDeprecated = VariableRegistry.getInstance().getDeprecatedVariableNames().contains(name);

        String data = "${" + name + '}';

        TreeItem item = new TreeItem(treeItemVariable, SWT.NULL);
        item.setImage(labelProvider.getImage(name));
        item.setText(name);
        item.setGrayed(isDeprecated);
        item.setData(data);
      }
    }

    // Tooltip for syntax and help
    BrowserToolTip toolTip = new BrowserToolTip(tree, labelProvider);
    toolTip.activate();
  }

  public void setText(String expression) {
    if (expression == null)
      return;

    sourceViewer.getDocument().set(expression);
  }

  public String getText() {
    return sourceViewer.getDocument().get();
  }

  @Override
  public void dispose() {
    this.labelProvider.dispose();
    super.dispose();
  }

  public void setRowMeta(final IRowMeta rowMeta) {
    Display.getDefault().asyncExec(() -> {
      // Remove existing fields
      treeItemField.removeAll();
            
      if (rowMeta != null) {
        for (int i = 0; i < rowMeta.size(); i++) {
          IValueMeta valueMeta = rowMeta.getValueMeta(i);

          // Escape field name matching reserved words or function name
          String name = valueMeta.getName();
          if (ExpressionBuilder.isReservedWord(name) || FunctionRegistry.isFunction(name)) {
            name = '[' + name + ']';
          }

          TreeItem item = new TreeItem(treeItemField, SWT.NULL);
          item.setImage(labelProvider.getImage(valueMeta));
          item.setText(valueMeta.getName());
          item.setData(name);
        }
      }
    });
  }

  @Override
  public void addListener(int eventType, Listener listener) {
    sourceViewer.getTextWidget().addListener(eventType, listener);
  }
}
