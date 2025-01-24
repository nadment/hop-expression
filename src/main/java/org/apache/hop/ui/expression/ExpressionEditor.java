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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.CompletableFuture;
import org.apache.hop.core.Props;
import org.apache.hop.core.gui.plugin.GuiPlugin;
import org.apache.hop.core.gui.plugin.toolbar.GuiToolbarElement;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.variables.DescribedVariable;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.VariableRegistry;
import org.apache.hop.expression.AggregateFunction;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionFactory;
import org.apache.hop.expression.ExpressionParseException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Identifier;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorComparator;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.expression.UserDefinedFunction;
import org.apache.hop.expression.operator.AddOperator;
import org.apache.hop.expression.operator.BetweenAsymmetricOperator;
import org.apache.hop.expression.operator.BitAndFunction;
import org.apache.hop.expression.operator.BitNotFunction;
import org.apache.hop.expression.operator.BitOrFunction;
import org.apache.hop.expression.operator.BitXorFunction;
import org.apache.hop.expression.operator.BoolAndOperator;
import org.apache.hop.expression.operator.BoolNotOperator;
import org.apache.hop.expression.operator.BoolOrOperator;
import org.apache.hop.expression.operator.BoolXorOperator;
import org.apache.hop.expression.operator.CaseSearchOperator;
import org.apache.hop.expression.operator.CastOperator;
import org.apache.hop.expression.operator.ConcatFunction;
import org.apache.hop.expression.operator.DivOperator;
import org.apache.hop.expression.operator.ElementAtOperator;
import org.apache.hop.expression.operator.EqualOperator;
import org.apache.hop.expression.operator.GreaterThanOperator;
import org.apache.hop.expression.operator.GreaterThanOrEqualOperator;
import org.apache.hop.expression.operator.ILikeOperator;
import org.apache.hop.expression.operator.InOperator;
import org.apache.hop.expression.operator.IsDistinctFromOperator;
import org.apache.hop.expression.operator.IsFalseOperator;
import org.apache.hop.expression.operator.IsNullOperator;
import org.apache.hop.expression.operator.IsTrueOperator;
import org.apache.hop.expression.operator.LessThanOperator;
import org.apache.hop.expression.operator.LessThanOrEqualOperator;
import org.apache.hop.expression.operator.LikeOperator;
import org.apache.hop.expression.operator.ModFunction;
import org.apache.hop.expression.operator.MultiplyOperator;
import org.apache.hop.expression.operator.NotEqualOperator;
import org.apache.hop.expression.operator.SimilarToOperator;
import org.apache.hop.expression.operator.SubtractOperator;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.ui.core.ConstUi;
import org.apache.hop.ui.core.FormDataBuilder;
import org.apache.hop.ui.core.PropsUi;
import org.apache.hop.ui.core.dialog.MessageBox;
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
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.DropTargetListener;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

@GuiPlugin
public class ExpressionEditor extends Composite implements IDocumentListener {
  private static final Class<?> PKG = ExpressionEditor.class;

  public static final String ID_TOOLBAR = "expression-editor-toolbar";
  public static final String ID_TOOLBAR_SELECT_ALL = "expression-editor-toolbar-10300-select-all";
  public static final String ID_TOOLBAR_COPY = "expression-editor-toolbar-10400-copy";
  public static final String ID_TOOLBAR_PASTE = "expression-editor-toolbar-10410-paste";
  public static final String ID_TOOLBAR_CUT = "expression-editor-toolbar-10420-cut";
  public static final String ID_TOOLBAR_UNDO = "expression-editor-toolbar-10430-undo";
  public static final String ID_TOOLBAR_REDO = "expression-editor-toolbar-10440-redo";
  public static final String ID_TOOLBAR_OPTIMIZE = "expression-editor-toolbar-10450-simplify";

  private static final String ANNOTATION_ERROR_TYPE = "org.hop.expression.error";

  /** Set of scalar operators without NOT variation (IS_NOT_TRUE, NOT_SIMILAR_TO...). */
  private static final Set<Operator> OPERATORS =
      Set.of(
          // BITWISE OPERATORS
          BitAndFunction.INSTANCE,
          BitOrFunction.INSTANCE,
          BitNotFunction.INSTANCE,
          BitXorFunction.INSTANCE,
          // CAST OPERATOR
          CastOperator.INSTANCE,
          // ARITHMETIC OPERATORS
          ModFunction.INSTANCE,
          AddOperator.INSTANCE,
          SubtractOperator.INSTANCE,
          MultiplyOperator.INSTANCE,
          DivOperator.INSTANCE,
          // STRING OPERATOR
          ConcatFunction.INSTANCE,
          new ConcatFunction("||"),
          // ARRAY OPERATOR
          ElementAtOperator.INSTANCE,
          // COMPARISON OPERATORS
          EqualOperator.INSTANCE,
          NotEqualOperator.INSTANCE,
          new NotEqualOperator("<>"),
          GreaterThanOperator.INSTANCE,
          GreaterThanOrEqualOperator.INSTANCE,
          LessThanOperator.INSTANCE,
          LessThanOrEqualOperator.INSTANCE,
          BetweenAsymmetricOperator.INSTANCE,
          InOperator.INSTANCE,
          IsDistinctFromOperator.INSTANCE,
          IsNullOperator.INSTANCE,
          SimilarToOperator.INSTANCE,
          IsFalseOperator.INSTANCE,
          IsTrueOperator.INSTANCE,
          LikeOperator.INSTANCE,
          ILikeOperator.INSTANCE,
          // CONDITIONAL OPERATORS
          CaseSearchOperator.INSTANCE,
          // LOGICAL OPERATORS
          BoolAndOperator.INSTANCE,
          BoolNotOperator.INSTANCE,
          BoolOrOperator.INSTANCE,
          BoolXorOperator.INSTANCE);

  private final ExpressionMode mode;
  private final ExpressionLabelProvider labelProvider;
  private final IVariables variables;
  private final CompletableFuture<IRowMeta> rowMetaFutur;
  private SourceViewer wViewer;
  private Tree wTree;
  private GuiToolbarWidgets toolbarWidgets;
  private IRowMeta rowMeta;

  public ExpressionEditor(
      Composite parent,
      int style,
      IVariables variables,
      ExpressionMode mode,
      CompletableFuture<IRowMeta> rowMetaFutur) {
    super(parent, style);
    this.variables = variables;
    this.mode = mode;
    this.rowMetaFutur = rowMetaFutur;
    this.labelProvider = new ExpressionLabelProvider();

    PropsUi.setLook(this);

    this.setLayout(new FormLayout());
    SashForm wSashForm = new org.eclipse.swt.custom.SashForm(this, org.eclipse.swt.SWT.HORIZONTAL);
    wSashForm.setLayoutData(new FormDataBuilder().fullSize().result());
    this.createTree(wSashForm);
    this.createEditor(wSashForm);

    // When IRowMeta is ready
    if (rowMetaFutur != null) {
      rowMetaFutur.thenAccept(this::setRowMeta);
    }

    wSashForm.setWeights(25, 75);
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

    wViewer =
        new SourceViewer(composite, createVerticalRuler(), SWT.H_SCROLL | SWT.V_SCROLL | SWT.MULTI);
    wViewer
        .getControl()
        .setLayoutData(new FormDataBuilder().top(toolbar).bottom().fullWidth().result());

    final StyledText widget = wViewer.getTextWidget();

    widget.setFont(GuiResource.getInstance().getFontFixed());

    // In Chinese window, Ctrl-SPACE is reserved by system for input Chinese character.
    // Use Ctrl-ALT-SPACE instead.
    final int modifierKeys =
        (System.getProperty("user.language").equals("zh")) ? SWT.CTRL | SWT.ALT : SWT.CTRL;

    widget.addListener(
        SWT.KeyDown,
        event -> {
          if (event.keyCode == SWT.SPACE && (event.stateMask & SWT.MODIFIER_MASK) == modifierKeys) {
            wViewer.doOperation(ISourceViewer.CONTENTASSIST_PROPOSALS);
          }
          if (event.keyCode == 'a' && (event.stateMask & SWT.MOD1) != 0) {
            doSelectAll();
          } else if (event.keyCode == SWT.F1) {
            // TODO: Help
            event.doit = false;
          }
        });

    // Allow data to be copied or moved to the drop target
    DropTarget dropTarget = new DropTarget(widget, DND.DROP_MOVE | DND.DROP_COPY);

    // Receive data in Text or File format
    final TextTransfer textTransfer = TextTransfer.getInstance();
    dropTarget.setTransfer(textTransfer);

    dropTarget.addDropListener(
        new DropTargetListener() {
          public void dragEnter(DropTargetEvent event) {
            if (event.detail == DND.DROP_DEFAULT) {
              if ((event.operations & DND.DROP_COPY) != 0) {
                event.detail = DND.DROP_COPY;
              } else {
                event.detail = DND.DROP_NONE;
              }
            }
          }

          public void dragOver(DropTargetEvent event) {

            event.feedback = DND.FEEDBACK_SELECT | DND.FEEDBACK_SCROLL;
            if (textTransfer.isSupportedType(event.currentDataType)) {
              // NOTE: on unsupported platforms this will return null
              String str = (String) textTransfer.nativeToJava(event.currentDataType);

              // if (t != null) System.out.println(t);
            }
          }

          public void dragOperationChanged(DropTargetEvent event) {}

          public void dragLeave(DropTargetEvent event) {}

          public void dropAccept(DropTargetEvent event) {}

          public void drop(DropTargetEvent event) {
            if (textTransfer.isSupportedType(event.currentDataType)) {
              String str = (String) event.data;
              StyledText styledText = wViewer.getTextWidget();
              styledText.insert(str);
            }
          }
        });

    Menu menu = new Menu(getShell(), SWT.POP_UP);
    MenuItem undoItem = new MenuItem(menu, SWT.PUSH);
    undoItem.setText(BaseMessages.getString(PKG, "ExpressionEditor.Menu.Undo.Label"));
    undoItem.setImage(
        GuiResource.getInstance()
            .getImage("ui/images/undo.svg", ConstUi.SMALL_ICON_SIZE, ConstUi.SMALL_ICON_SIZE));
    undoItem.addListener(SWT.Selection, e -> doUndo());

    MenuItem redoItem = new MenuItem(menu, SWT.PUSH);
    redoItem.setText(BaseMessages.getString(PKG, "ExpressionEditor.Menu.Redo.Label"));
    redoItem.setImage(
        GuiResource.getInstance()
            .getImage("ui/images/redo.svg", ConstUi.SMALL_ICON_SIZE, ConstUi.SMALL_ICON_SIZE));
    redoItem.addListener(SWT.Selection, e -> doRedo());
    new MenuItem(menu, SWT.SEPARATOR);
    MenuItem cutItem = new MenuItem(menu, SWT.PUSH);
    cutItem.setText(BaseMessages.getString(PKG, "ExpressionEditor.Menu.Cut.Label"));
    cutItem.setImage(
        GuiResource.getInstance()
            .getImage("ui/images/cut.svg", ConstUi.SMALL_ICON_SIZE, ConstUi.SMALL_ICON_SIZE));
    cutItem.addListener(SWT.Selection, e -> doCut());
    MenuItem copyItem = new MenuItem(menu, SWT.PUSH);
    copyItem.setText(BaseMessages.getString(PKG, "ExpressionEditor.Menu.Copy.Label"));
    copyItem.setImage(
        GuiResource.getInstance()
            .getImage("ui/images/copy.svg", ConstUi.SMALL_ICON_SIZE, ConstUi.SMALL_ICON_SIZE));
    copyItem.addListener(SWT.Selection, e -> doCopy());
    MenuItem pasteItem = new MenuItem(menu, SWT.PUSH);
    pasteItem.setText(BaseMessages.getString(PKG, "ExpressionEditor.Menu.Paste.Label"));
    pasteItem.setImage(
        GuiResource.getInstance()
            .getImage("ui/images/paste.svg", ConstUi.SMALL_ICON_SIZE, ConstUi.SMALL_ICON_SIZE));
    pasteItem.addListener(SWT.Selection, e -> doPaste());
    new MenuItem(menu, SWT.SEPARATOR);
    MenuItem selectAllItem = new MenuItem(menu, SWT.PUSH);
    selectAllItem.setText(BaseMessages.getString(PKG, "ExpressionEditor.Menu.SelectAll.Label"));
    selectAllItem.setImage(
        GuiResource.getInstance()
            .getImage(
                "ui/images/select-all.svg", ConstUi.SMALL_ICON_SIZE, ConstUi.SMALL_ICON_SIZE));
    selectAllItem.addListener(SWT.Selection, e -> doSelectAll());

    widget.setMenu(menu);
    widget.addListener(
        SWT.MenuDetect,
        event -> {
          undoItem.setEnabled(wViewer.canDoOperation(ITextOperationTarget.UNDO));
          redoItem.setEnabled(wViewer.canDoOperation(ITextOperationTarget.REDO));
          cutItem.setEnabled(wViewer.canDoOperation(ITextOperationTarget.CUT));
          copyItem.setEnabled(wViewer.canDoOperation(ITextOperationTarget.COPY));
          pasteItem.setEnabled(wViewer.canDoOperation(ITextOperationTarget.PASTE));
        });

    Document document = new Document();
    document.addDocumentListener(this);

    ExpressionEditorConfiguration configuration =
        new ExpressionEditorConfiguration(variables, rowMetaFutur, mode);
    IDocumentPartitioner partitioner =
        new FastPartitioner(
            new ExpressionPartitionScanner(), configuration.getConfiguredContentTypes(wViewer));
    partitioner.connect(document);
    document.setDocumentPartitioner(partitioner);

    wViewer.setDocument(document, new AnnotationModel());
    wViewer.configure(configuration);
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
    wTree = new Tree(composite, SWT.H_SCROLL | SWT.V_SCROLL);
    wTree.setLayoutData(new FormDataBuilder().top(toolbar).fullWidth().bottom().result());
    PropsUi.setLook(wTree);
    wTree.addListener(SWT.MouseDoubleClick, this::onTreeDoubleClick);

    // Create the drag source on the tree
    DragSource dragSource = new DragSource(wTree, DND.DROP_MOVE | DND.DROP_COPY);
    dragSource.setTransfer(TextTransfer.getInstance());
    dragSource.addDragListener(
        new DragSourceAdapter() {
          @Override
          public void dragStart(DragSourceEvent event) {
            TreeItem item = wTree.getSelection()[0];

            event.doit = item != null && item.getData() != null;
          }

          @Override
          public void dragSetData(DragSourceEvent event) {
            TreeItem item = wTree.getSelection()[0];
            String str = String.valueOf(item.getData());
            if (item.getData() instanceof Operator operator) {
              str = operator.getName();
              if (operator instanceof Function) {
                str += "()";
              }
            }
            if (item.getData() instanceof DescribedVariable variable) {
              str = "${" + variable.getName() + '}';
            }
            if (item.getData() instanceof IValueMeta meta) {
              str = Identifier.quoteIfNeeded(meta.getName());
            }
            // Set the data to be the first selected item's text
            event.data = str;
          }
        });

    if (mode == ExpressionMode.ROW || mode == ExpressionMode.COLUMN || mode == ExpressionMode.UDF) {
      TreeItem item = new TreeItem(wTree, SWT.NULL);
      item.setImage(GuiResource.getInstance().getImageFolder());
      String text =
          (mode == ExpressionMode.UDF)
              ? "ExpressionEditor.Tree.Arguments.Label"
              : "ExpressionEditor.Tree.Fields.Label";
      item.setText(BaseMessages.getString(PKG, text));
    }

    TreeItem treeItemOperator = new TreeItem(wTree, SWT.NULL);
    treeItemOperator.setImage(GuiResource.getInstance().getImageFolder());
    treeItemOperator.setText(BaseMessages.getString(PKG, "ExpressionEditor.Tree.Operators.Label"));

    Set<String> categories = new TreeSet<>();
    List<Operator> primaryOperators = new ArrayList<>();
    HashMap<String, String> mapDisplay = new HashMap<>();

    // Set of operators without NOT variation (IS_NOT_TRUE, NOT_SIMILAR_TO...)
    Set<Operator> operators = new TreeSet<>(new OperatorComparator());
    operators.addAll(OPERATORS);
    operators.addAll(FunctionRegistry.getFunctions());

    // Inventory operator unique identifier and category
    for (Operator operator : operators) {

      if (mode != ExpressionMode.COLUMN && operator instanceof AggregateFunction) {
        continue;
      }

      if (mode == ExpressionMode.UDF && operator instanceof UserDefinedFunction) {
        continue;
      }

      categories.add(operator.getCategory());

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

    // Create tree item for operators
    for (Operator operator : primaryOperators) {

      TreeItem parentItem = items.get(operator.getCategory());

      TreeItem item;
      if (parentItem == null) item = new TreeItem(wTree, SWT.NULL);
      else item = new TreeItem(parentItem, SWT.NULL);
      item.setImage(labelProvider.getImage(operator));
      item.setText(mapDisplay.get(operator.getId()));
      item.setData(operator);
    }

    // Create tree item for variables
    TreeItem treeItemVariable = new TreeItem(wTree, SWT.NULL);
    treeItemVariable.setImage(GuiResource.getInstance().getImageFolder());
    treeItemVariable.setText(BaseMessages.getString(PKG, "ExpressionEditor.Tree.Variables.Label"));

    if (variables != null) {
      String[] names = this.variables.getVariableNames();
      Arrays.sort(names);

      VariableRegistry variableRegistry = VariableRegistry.getInstance();
      treeItemVariable.removeAll();
      for (String name : names) {
        boolean isDeprecated = variableRegistry.getDeprecatedVariableNames().contains(name);

        DescribedVariable variable = variableRegistry.findDescribedVariable(name);
        if (variable == null) {
          variable = new DescribedVariable(name, null, null);
        }
        variable.setValue("${" + name + '}');

        TreeItem item = new TreeItem(treeItemVariable, SWT.NULL);
        item.setImage(GuiResource.getInstance().getImageVariable());
        item.setText(name);
        item.setGrayed(isDeprecated);
        item.setData(variable);
      }
    }

    // Tooltip for syntax and help
    BrowserToolTip toolTip = new BrowserToolTip(wTree, labelProvider);
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

  // Adds the Current item to the current Position
  private void onTreeDoubleClick(Event event) {
    Point point = new Point(event.x, event.y);
    TreeItem item = wTree.getItem(point);

    if (item != null && item.getData() != null) {
      StyledText styledText = wViewer.getTextWidget();

      // When a selection is already there we need to subtract the position
      int start = styledText.getCaretOffset() - styledText.getSelectionCount();
      if (start < 0) {
        start = 0;
      }

      String str = item.getText();
      if (item.getData() instanceof Function function) {
        str = function.getName() + "()";
      }
      if (item.getData() instanceof DescribedVariable variable) {
        str = "${" + variable.getName() + '}';
      }
      if (item.getData() instanceof IValueMeta meta) {
        str = Identifier.quoteIfNeeded(meta.getName());
      }
      styledText.insert(str);
      styledText.setSelection(start, start + str.length());
    }
  }

  public void setText(String expression) {
    if (expression == null) return;

    wViewer.getDocument().set(expression);
  }

  public String getText() {
    return wViewer.getDocument().get();
  }

  @GuiToolbarElement(
      root = ID_TOOLBAR,
      id = ID_TOOLBAR_COPY,
      image = "ui/images/copy.svg",
      toolTip = "i18n::ExpressionEditor.ToolBarWidget.Copy.ToolTip",
      separator = true)
  public void doCopy() {
    wViewer.doOperation(ITextOperationTarget.COPY);
  }

  @GuiToolbarElement(
      root = ID_TOOLBAR,
      id = ID_TOOLBAR_PASTE,
      image = "ui/images/paste.svg",
      toolTip = "i18n::ExpressionEditor.ToolBarWidget.Paste.ToolTip")
  public void doPaste() {
    wViewer.doOperation(ITextOperationTarget.PASTE);
  }

  @GuiToolbarElement(
      root = ID_TOOLBAR,
      id = ID_TOOLBAR_CUT,
      image = "ui/images/cut.svg",
      toolTip = "i18n::ExpressionEditor.ToolBarWidget.Cut.ToolTip")
  public void doCut() {
    wViewer.doOperation(ITextOperationTarget.CUT);
  }

  @GuiToolbarElement(
      root = ID_TOOLBAR,
      id = ID_TOOLBAR_SELECT_ALL,
      image = "ui/images/select-all.svg",
      toolTip = "i18n::ExpressionEditor.ToolBarWidget.SelectAll.ToolTip",
      separator = true)
  public void doSelectAll() {
    wViewer.doOperation(ITextOperationTarget.SELECT_ALL);
  }

  @GuiToolbarElement(
      root = ID_TOOLBAR,
      id = ID_TOOLBAR_UNDO,
      image = "ui/images/undo.svg",
      toolTip = "i18n::ExpressionEditor.ToolBarWidget.Undo.ToolTip",
      separator = true)
  public void doUndo() {
    wViewer.doOperation(ITextOperationTarget.UNDO);
  }

  @GuiToolbarElement(
      root = ID_TOOLBAR,
      id = ID_TOOLBAR_REDO,
      image = "ui/images/redo.svg",
      toolTip = "i18n::ExpressionEditor.ToolBarWidget.Redo.ToolTip")
  public void doRedo() {
    wViewer.doOperation(ITextOperationTarget.REDO);
  }

  @GuiToolbarElement(
      root = ID_TOOLBAR,
      id = ID_TOOLBAR_OPTIMIZE,
      image = "evaluate.svg",
      toolTip = "i18n::ExpressionEditor.ToolBarWidget.Evaluate.ToolTip",
      separator = true)
  public void doEvaluate() {

    String source = wViewer.getTextWidget().getText();

    IExpression expression =
        ExpressionFactory.create(new RowExpressionContext(variables, rowMeta), source);

    MessageBox dialog = new MessageBox(getShell());
    dialog.setText("Expression simplified");
    dialog.setMessage(expression.toString());
    dialog.open();
  }

  protected void treeExpandCollapseAll(boolean expanded) {
    // Stop redraw until operation complete
    wTree.setRedraw(false);
    for (TreeItem item : wTree.getItems()) {
      item.setExpanded(expanded);
      if (item.getItemCount() > 0) {
        for (TreeItem i : item.getItems()) {
          i.setExpanded(expanded);
        }
      }
    }
    wTree.setRedraw(true);
  }

  public void setRowMeta(final IRowMeta rowMeta) {
    this.rowMeta = rowMeta;

    Display.getDefault()
        .asyncExec(
            () -> {
              // Remove existing fields

              TreeItem parentItem = wTree.getItem(0);

              parentItem.removeAll();

              if (rowMeta != null) {
                for (int i = 0; i < rowMeta.size(); i++) {
                  IValueMeta valueMeta = rowMeta.getValueMeta(i);

                  TreeItem item = new TreeItem(parentItem, SWT.NULL);
                  item.setImage(GuiResource.getInstance().getImage(valueMeta));
                  item.setText(valueMeta.getName());
                  item.setData(valueMeta);
                }
              }
            });
  }

  @Override
  public void addListener(int eventType, Listener listener) {
    wViewer.getTextWidget().addListener(eventType, listener);
  }

  @Override
  public void documentAboutToBeChanged(DocumentEvent event) {}

  @Override
  public void documentChanged(DocumentEvent event) {
    // Remove all annotations
    IAnnotationModel annotationModel = wViewer.getAnnotationModel();
    Iterator<Annotation> iter = annotationModel.getAnnotationIterator();
    while (iter.hasNext()) {
      annotationModel.removeAnnotation(iter.next());
    }

    String expression = event.getDocument().get();
    try {
      IExpressionContext context;
      if (rowMeta == null) {
        context = new ExpressionContext(variables);
      } else {
        context = new RowExpressionContext(variables, rowMeta);
      }

      ExpressionFactory.create(context, expression);
    } catch (ExpressionParseException e) {
      Annotation annotation = new Annotation(ANNOTATION_ERROR_TYPE, false, e.getMessage());
      annotationModel.addAnnotation(annotation, new Position(e.getPosition(), 0));
    } catch (ExpressionException e) {
      Annotation annotation = new Annotation(ANNOTATION_ERROR_TYPE, false, e.getMessage());
      annotationModel.addAnnotation(annotation, new Position(0, 0));
    }
  }
}
