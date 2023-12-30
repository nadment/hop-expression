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

import org.apache.hop.core.Props;
import org.apache.hop.core.gui.plugin.GuiPlugin;
import org.apache.hop.core.gui.plugin.toolbar.GuiToolbarElement;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.VariableRegistry;
import org.apache.hop.expression.AggregateFunction;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionParser;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.expression.UserDefinedFunction;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.ui.core.ConstUi;
import org.apache.hop.ui.core.FormDataBuilder;
import org.apache.hop.ui.core.PropsUi;
import org.apache.hop.ui.core.gui.GuiResource;
import org.apache.hop.ui.core.gui.GuiToolbarWidgets;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.ITextOperationTarget;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.rules.FastPartitioner;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.AnnotationModel;
import org.eclipse.jface.text.source.AnnotationRulerColumn;
import org.eclipse.jface.text.source.CompositeRuler;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.jface.text.source.LineNumberRulerColumn;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyledText;
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
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.CompletableFuture;

@GuiPlugin
public class ExpressionEditor extends Composite implements IDocumentListener {
  private static final Class<?> PKG = ExpressionEditor.class;

  public static final String ID_TOOLBAR = "expression-editor-toolbar";
  public static final String ID_TOOLBAR_SELECT_ALL = "expression-editor-toolbar-10300-select-all";
  public static final String ID_TOOLBAR_COPY = "expression-editor-toolbar-10400-copy";
  public static final String ID_TOOLBAR_PASTE = "expression-editor-toolbar-10410-paste";
  public static final String ID_TOOLBAR_CUT = "expression-editor-toolbar-10420-cut";

  private static final String ANNOTATION_ERROR_TYPE = "org.hop.expression.error";

  private ExpressionMode mode = ExpressionMode.NONE;
  private ExpressionLabelProvider labelProvider;
  private IVariables variables;
  private CompletableFuture<IRowMeta> rowMetaFutur;
  private SourceViewer viewer;
  private SashForm sashForm;
  private Tree tree;
  private GuiToolbarWidgets toolbarWidgets;
  private IRowMeta rowMeta;

  public ExpressionEditor(Composite parent, int style, IVariables variables, ExpressionMode mode,
      CompletableFuture<IRowMeta> rowMetaFutur) {
    super(parent, style);
    this.variables = variables;
    this.mode = mode;
    this.rowMetaFutur = rowMetaFutur;
    this.labelProvider = new ExpressionLabelProvider();

    PropsUi.setLook(this);

    this.setLayout(new FormLayout());
    this.sashForm = new SashForm(this, SWT.HORIZONTAL);
    this.sashForm.setLayoutData(new FormDataBuilder().fullSize().result());
    this.createTree(sashForm);
    this.createEditor(sashForm);

    // When IRowMeta is ready
    if (rowMetaFutur != null) {
      rowMetaFutur.thenAccept(this::setRowMeta);
    }

    sashForm.setWeights(25, 75);
  }

  protected void createEditor(final Composite parent) {

    Composite composite = new Composite(parent, SWT.NONE);
    composite.setLayout(new FormLayout());

    ToolBar toolbar = new ToolBar(composite, SWT.FLAT | SWT.HORIZONTAL);
    toolbar.setLayoutData(new FormDataBuilder().top().fullWidth().result());
    PropsUi.setLook(toolbar, Props.WIDGET_STYLE_TOOLBAR);

    // Create an empty place to make it easier for plugins to use the toolbar widgets.
    //
    toolbarWidgets = new GuiToolbarWidgets();
    toolbarWidgets.registerGuiPluginObject(this);
    toolbarWidgets.createToolbarWidgets(toolbar, ID_TOOLBAR);

    viewer =
        new SourceViewer(composite, createVerticalRuler(), SWT.H_SCROLL | SWT.V_SCROLL | SWT.MULTI);
    viewer.getControl()
        .setLayoutData(new FormDataBuilder().top(toolbar).bottom().fullWidth().result());

    final StyledText widget = viewer.getTextWidget();

    widget.setFont(GuiResource.getInstance().getFontFixed());


    // In Chinese window, Ctrl-SPACE is reserved by system for input Chinese character.
    // Use Ctrl-ALT-SPACE instead.
    final int modifierKeys =
        (System.getProperty("user.language").equals("zh")) ? SWT.CTRL | SWT.ALT : SWT.CTRL;

    widget.addListener(SWT.KeyDown, event -> {
      if (event.keyCode == SWT.SPACE && (event.stateMask & SWT.MODIFIER_MASK) == modifierKeys) {
        viewer.doOperation(ISourceViewer.CONTENTASSIST_PROPOSALS);
      } else if (event.keyCode == SWT.F1) {
        // TODO: Help
        event.doit = false;
      }
    });

    Menu menu = new Menu(getShell(), SWT.POP_UP);
    MenuItem undoItem = new MenuItem(menu, SWT.PUSH);
    undoItem.setText(BaseMessages.getString(PKG, "ExpressionEditor.Menu.Undo.Label"));
    undoItem.setImage(GuiResource.getInstance().getImage("ui/images/undo.svg",
        ConstUi.SMALL_ICON_SIZE, ConstUi.SMALL_ICON_SIZE));
    undoItem.addListener(SWT.Selection, e -> viewer.doOperation(ITextOperationTarget.UNDO));

    MenuItem redoItem = new MenuItem(menu, SWT.PUSH);
    redoItem.setText(BaseMessages.getString(PKG, "ExpressionEditor.Menu.Redo.Label"));
    redoItem.setImage(GuiResource.getInstance().getImage("ui/images/redo.svg",
        ConstUi.SMALL_ICON_SIZE, ConstUi.SMALL_ICON_SIZE));
    redoItem.addListener(SWT.Selection, e -> viewer.doOperation(ITextOperationTarget.REDO));
    new MenuItem(menu, SWT.SEPARATOR);
    MenuItem cutItem = new MenuItem(menu, SWT.PUSH);
    cutItem.setText(BaseMessages.getString(PKG, "ExpressionEditor.Menu.Cut.Label"));
    cutItem.setImage(GuiResource.getInstance().getImage("ui/images/cut.svg",
        ConstUi.SMALL_ICON_SIZE, ConstUi.SMALL_ICON_SIZE));
    cutItem.addListener(SWT.Selection, e -> viewer.doOperation(ITextOperationTarget.CUT));
    MenuItem copyItem = new MenuItem(menu, SWT.PUSH);
    copyItem.setText(BaseMessages.getString(PKG, "ExpressionEditor.Menu.Copy.Label"));
    copyItem.setImage(GuiResource.getInstance().getImage("ui/images/copy.svg",
        ConstUi.SMALL_ICON_SIZE, ConstUi.SMALL_ICON_SIZE));
    copyItem.addListener(SWT.Selection, e -> doCopy());
    MenuItem pasteItem = new MenuItem(menu, SWT.PUSH);
    pasteItem.setText(BaseMessages.getString(PKG, "ExpressionEditor.Menu.Paste.Label"));
    pasteItem.setImage(GuiResource.getInstance().getImage("ui/images/paste.svg",
        ConstUi.SMALL_ICON_SIZE, ConstUi.SMALL_ICON_SIZE));
    pasteItem.addListener(SWT.Selection, e -> viewer.doOperation(ITextOperationTarget.PASTE));
    new MenuItem(menu, SWT.SEPARATOR);
    MenuItem selectAllItem = new MenuItem(menu, SWT.PUSH);
    selectAllItem.setText(BaseMessages.getString(PKG, "ExpressionEditor.Menu.SelectAll.Label"));
    selectAllItem.setImage(GuiResource.getInstance().getImage("ui/images/select-all.svg",
        ConstUi.SMALL_ICON_SIZE, ConstUi.SMALL_ICON_SIZE));
    selectAllItem.addListener(SWT.Selection,
        e -> viewer.doOperation(ITextOperationTarget.SELECT_ALL));

    widget.setMenu(menu);
    widget.addListener(SWT.MenuDetect, event -> {
      undoItem.setEnabled(viewer.canDoOperation(ITextOperationTarget.UNDO));
      redoItem.setEnabled(viewer.canDoOperation(ITextOperationTarget.REDO));
      cutItem.setEnabled(viewer.canDoOperation(ITextOperationTarget.CUT));
      copyItem.setEnabled(viewer.canDoOperation(ITextOperationTarget.COPY));
      pasteItem.setEnabled(viewer.canDoOperation(ITextOperationTarget.PASTE));
    });

    Document document = new Document();
    document.addDocumentListener(this);


    ExpressionEditorConfiguration configuration =
        new ExpressionEditorConfiguration(variables, rowMetaFutur, mode);
    IDocumentPartitioner partitioner = new FastPartitioner(new ExpressionPartitionScanner(),
        configuration.getConfiguredContentTypes(viewer));
    partitioner.connect(document);
    document.setDocumentPartitioner(partitioner);

    viewer.setDocument(document, new AnnotationModel());
    viewer.configure(configuration);
  }

  protected void createTree(final Composite parent) {

    Composite composite = new Composite(parent, SWT.NONE);
    composite.setLayout(new FormLayout());
    // composite.setLayoutData(new FormDataBuilder().fullSize().result());

    ToolBar toolbar = new ToolBar(composite, SWT.FLAT | SWT.HORIZONTAL);
    toolbar.setLayoutData(new FormDataBuilder().top().fullWidth().result());
    PropsUi.setLook(toolbar, Props.WIDGET_STYLE_TOOLBAR);

    ToolItem toolbarItem = new ToolItem(toolbar, SWT.PUSH);
    toolbarItem.setToolTipText("Collapse all");
    toolbarItem.setImage(GuiResource.getInstance().getImageCollapseAll());
    toolbarItem.addListener(SWT.Selection, e -> treeExpandCollapseAll(false));

    toolbarItem = new ToolItem(toolbar, SWT.PUSH);
    toolbarItem.setToolTipText("Expand all");
    toolbarItem.setImage(GuiResource.getInstance().getImageExpandAll());
    toolbarItem.addListener(SWT.Selection, e -> treeExpandCollapseAll(true));

    toolbar.pack();

    // Tree widget
    tree = new Tree(composite, SWT.H_SCROLL | SWT.V_SCROLL);
    tree.setLayoutData(new FormDataBuilder().top(toolbar).fullWidth().bottom().result());
    PropsUi.setLook(tree);

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

    if (mode == ExpressionMode.ROW || mode == ExpressionMode.COLUMN || mode == ExpressionMode.UDF) {
      TreeItem item = new TreeItem(tree, SWT.NULL);
      item.setImage(GuiResource.getInstance().getImageFolder());
      String text = (mode == ExpressionMode.UDF) ? "ExpressionEditor.Tree.Arguments.Label"
          : "ExpressionEditor.Tree.Fields.Label";
      item.setText(BaseMessages.getString(PKG, text));
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

      if (mode != ExpressionMode.COLUMN && operator instanceof AggregateFunction) {
        continue;
      }

      if (mode == ExpressionMode.UDF && operator instanceof UserDefinedFunction) {
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
        boolean isDeprecated =
            VariableRegistry.getInstance().getDeprecatedVariableNames().contains(name);

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

  /**
   * Create the vertical ruler for the source viewer.
   * 
   * @return the vertical ruler
   */
  protected IVerticalRuler createVerticalRuler() {
    LineNumberRulerColumn lineNumberRulerColumn = new LineNumberRulerColumn();
    lineNumberRulerColumn.setBackground(GuiResource.getInstance().getColorLightGray());

    AnnotationRulerColumn annotationRulerColumn =
        new AnnotationRulerColumn(ConstUi.SMALL_ICON_SIZE + 2, new ExpressionAnnotationAccess());
    annotationRulerColumn.addAnnotationType(ANNOTATION_ERROR_TYPE);

    CompositeRuler ruler = new CompositeRuler(1);
    ruler.addDecorator(0, lineNumberRulerColumn);
    ruler.addDecorator(1, annotationRulerColumn);

    return ruler;
  }

  public void setText(String expression) {
    if (expression == null)
      return;

    viewer.getDocument().set(expression);
  }

  public String getText() {
    return viewer.getDocument().get();
  }

  @GuiToolbarElement(root = ID_TOOLBAR, id = ID_TOOLBAR_COPY, image = "ui/images/copy.svg",
      toolTip = "i18n::ExpressionEditor.ToolBarWidget.Copy.ToolTip", separator = true)
  public void doCopy() {
    viewer.doOperation(ITextOperationTarget.COPY);
  }

  @GuiToolbarElement(root = ID_TOOLBAR, id = ID_TOOLBAR_PASTE, image = "ui/images/paste.svg",
      toolTip = "i18n::ExpressionEditor.ToolBarWidget.Paste.ToolTip")
  public void doPaste() {
    viewer.doOperation(ITextOperationTarget.PASTE);
  }

  @GuiToolbarElement(root = ID_TOOLBAR, id = ID_TOOLBAR_CUT, image = "ui/images/cut.svg",
      toolTip = "i18n::ExpressionEditor.ToolBarWidget.Cut.ToolTip")
  public void doCut() {
    viewer.doOperation(ITextOperationTarget.CUT);
  }

  @GuiToolbarElement(root = ID_TOOLBAR, id = ID_TOOLBAR_SELECT_ALL,
      image = "ui/images/select-all.svg",
      toolTip = "i18n::ExpressionEditor.ToolBarWidget.SelectAll.ToolTip", separator = true)
  public void doSelectAll() {
    viewer.doOperation(ITextOperationTarget.SELECT_ALL);
  }

  protected void treeExpandCollapseAll(boolean expanded) {
    // Stop redraw until operation complete
    tree.setRedraw(false);
    for (TreeItem item : tree.getItems()) {
      item.setExpanded(expanded);
      if (item.getItemCount() > 0) {
        for (TreeItem i : item.getItems()) {
          i.setExpanded(expanded);
        }
      }
    }
    tree.setRedraw(true);
  }

  @Override
  public void dispose() {
    this.labelProvider.dispose();
    super.dispose();
  }

  public void setRowMeta(final IRowMeta rowMeta) {
    this.rowMeta = rowMeta;

    Display.getDefault().asyncExec(() -> {
      // Remove existing fields

      TreeItem parentItem = tree.getItem(0);

      parentItem.removeAll();

      if (rowMeta != null) {
        for (int i = 0; i < rowMeta.size(); i++) {
          IValueMeta valueMeta = rowMeta.getValueMeta(i);

          // Escape field name matching reserved words or function name
          String name = valueMeta.getName();
          if (ExpressionParser.isReservedWord(name) || FunctionRegistry.isFunction(name)) {
            name = '[' + name + ']';
          }

          TreeItem item = new TreeItem(parentItem, SWT.NULL);
          item.setImage(labelProvider.getImage(valueMeta));
          item.setText(valueMeta.getName());
          item.setData(name);
        }
      }
    });
  }

  @Override
  public void addListener(int eventType, Listener listener) {
    viewer.getTextWidget().addListener(eventType, listener);
  }

  @Override
  public void documentAboutToBeChanged(DocumentEvent event) {}

  @Override
  public void documentChanged(DocumentEvent event) {
    // Remove all annotations
    IAnnotationModel annotationModel = viewer.getAnnotationModel();
    Iterator<Annotation> iter = annotationModel.getAnnotationIterator();
    while (iter.hasNext()) {
      annotationModel.removeAnnotation(iter.next());
    }

    String expression = event.getDocument().get();
    try {
      IExpressionContext context;
      if ( rowMeta==null) {
        context = new ExpressionContext(variables);   
      }
      else {
        context = new RowExpressionContext(variables, rowMeta);
      }
      
      context.createExpression(expression);
    } catch (ExpressionException e) {
      Annotation annotation = new Annotation(ANNOTATION_ERROR_TYPE, false, e.getMessage());
      annotationModel.addAnnotation(annotation, new Position(e.getPosition(), 0));
    }
  }
}
