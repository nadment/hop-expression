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
import org.apache.hop.core.gui.plugin.key.GuiKeyboardShortcut;
import org.apache.hop.core.gui.plugin.key.GuiOsxKeyboardShortcut;
import org.apache.hop.core.gui.plugin.menu.GuiMenuElement;
import org.apache.hop.core.gui.plugin.toolbar.GuiToolbarElement;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.variables.DescribedVariable;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.VariableRegistry;
import org.apache.hop.expression.AggregateFunction;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Identifier;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorComparator;
import org.apache.hop.expression.ParseExpressionException;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.expression.UserDefinedFunction;
import org.apache.hop.expression.operator.AddOperator;
import org.apache.hop.expression.operator.ArrayElementAtOperator;
import org.apache.hop.expression.operator.BetweenAsymmetricOperator;
import org.apache.hop.expression.operator.BoolAndOperator;
import org.apache.hop.expression.operator.BoolNotFunction;
import org.apache.hop.expression.operator.BoolOrOperator;
import org.apache.hop.expression.operator.BoolXorOperator;
import org.apache.hop.expression.operator.CaseSearchOperator;
import org.apache.hop.expression.operator.CastOperator;
import org.apache.hop.expression.operator.ConcatFunction;
import org.apache.hop.expression.operator.DivOperator;
import org.apache.hop.expression.operator.EqualOperator;
import org.apache.hop.expression.operator.GreaterThanOperator;
import org.apache.hop.expression.operator.GreaterThanOrEqualOperator;
import org.apache.hop.expression.operator.ILikeOperator;
import org.apache.hop.expression.operator.InListOperator;
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
import org.apache.hop.ui.core.gui.GuiMenuWidgets;
import org.apache.hop.ui.core.gui.GuiResource;
import org.apache.hop.ui.core.gui.GuiToolbarWidgets;
import org.apache.hop.ui.core.gui.IToolbarContainer;
import org.apache.hop.ui.hopgui.ToolbarFacade;
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
import org.eclipse.swt.browser.Browser;
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
import org.eclipse.swt.graphics.Color;
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
public class ExpressionEditor extends Composite
    implements /*IFindReplaceTarget,*/ IDocumentListener {
  public static final String ID_TOOLBAR = "ExpressionEditor-Toolbar";
  public static final String ID_TOOLBAR_UNDO = "ExpressionEditor-Toolbar-10000-undo";
  public static final String ID_TOOLBAR_REDO = "ExpressionEditor-Toolbar-10010-redo";
  public static final String ID_TOOLBAR_SELECT_ALL = "ExpressionEditor-Toolbar-10100-select-all";
  public static final String ID_TOOLBAR_UNSELECT_ALL =
      "ExpressionEditor-Toolbar-10110-unselect-all";
  public static final String ID_TOOLBAR_COPY = "ExpressionEditor-Toolbar-10200-copy";
  public static final String ID_TOOLBAR_PASTE = "ExpressionEditor-Toolbar-10210-paste";
  public static final String ID_TOOLBAR_CUT = "ExpressionEditor-Toolbar-10230-cut";
  public static final String ID_TOOLBAR_FIND = "ExpressionEditor-Toolbar-10300-find";
  public static final String ID_TOOLBAR_FIND_REPLACE =
      "ExpressionEditor-Toolbar-10310-find-replace";
  public static final String ID_TOOLBAR_OPTIMIZE = "ExpressionEditor-Toolbar-10500-simplify";

  private static final Class<?> PKG = ExpressionEditor.class;
  public static final String GUI_PLUGIN_CONTEXT_MENU_PARENT_ID = "ExpressionEditor-ContextMenu";
  public static final String ID_CONTEXT_MENU_UNDO = "ExpressionEditor-ContextMenu-10000-undo";
  public static final String ID_CONTEXT_MENU_REDO = "ExpressionEditor-ContextMenu-10010-redo";
  public static final String ID_CONTEXT_MENU_SELECT_ALL =
      "ExpressionEditor-ContextMenu-20000-select-all";
  public static final String ID_CONTEXT_MENU_UNSELECT_ALL =
      "ExpressionEditor-ContextMenu-20010-unselect-all";
  public static final String ID_CONTEXT_MENU_COPY = "ExpressionEditor-ContextMenu-30000-copy";
  public static final String ID_CONTEXT_MENU_PASTE = "ExpressionEditor-ContextMenu-30010-paste";
  public static final String ID_CONTEXT_MENU_CUT = "ExpressionEditor-ContextMenu-30020-cut";
  public static final String ID_CONTEXT_MENU_FIND = "ExpressionEditor-ContextMenu-40000-find";
  public static final String ID_CONTEXT_MENU_FIND_REPLACE =
      "ExpressionEditor-ContextMenu-40010-find-replace";

  private static final String ANNOTATION_ERROR_TYPE = "org.hop.expression.error";

  /** Set of scalar operators without NOT variation (IS_NOT_TRUE, NOT_SIMILAR_TO...). */
  private static final Set<Operator> OPERATORS =
      Set.of(
          // CAST OPERATOR
          CastOperator.INSTANCE,
          // ARITHMETIC OPERATORS
          ModFunction.INSTANCE,
          AddOperator.INSTANCE,
          SubtractOperator.INSTANCE,
          MultiplyOperator.INSTANCE,
          DivOperator.INSTANCE,
          // STRING OPERATOR
          new ConcatFunction("||"),
          // ARRAY OPERATOR
          ArrayElementAtOperator.INSTANCE,
          // COMPARISON OPERATORS
          EqualOperator.INSTANCE,
          NotEqualOperator.INSTANCE,
          new NotEqualOperator("<>"),
          GreaterThanOperator.INSTANCE,
          GreaterThanOrEqualOperator.INSTANCE,
          LessThanOperator.INSTANCE,
          LessThanOrEqualOperator.INSTANCE,
          BetweenAsymmetricOperator.INSTANCE,
          InListOperator.INSTANCE,
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
          new BoolNotFunction("NOT"),
          BoolOrOperator.INSTANCE,
          BoolXorOperator.INSTANCE);

  private final ExpressionMode mode;
  private final ExpressionLabelProvider labelProvider;
  private final IVariables variables;
  private final CompletableFuture<IRowMeta> rowMetaFutur;
  private SourceViewer wViewer;
  private Tree wTree;
  private GuiToolbarWidgets toolbarWidgets;
  private GuiMenuWidgets contextMenuWidgets;
  private IRowMeta rowMeta;
  private Browser wBrowser;
  private SashForm wEditorSashForm;

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
    SashForm wSashForm = new SashForm(this, SWT.HORIZONTAL);
    wSashForm.setLayoutData(new FormDataBuilder().fullSize().result());
    this.createTree(wSashForm);
    this.createSash(wSashForm);
    this.createContextMenu();

    // When IRowMeta is ready
    if (rowMetaFutur != null) {
      rowMetaFutur.thenAccept(this::setRowMeta);
    }

    wSashForm.setWeights(25, 75);

    updateToolbar();
  }

  protected void createSash(final Composite parent) {
    wEditorSashForm = new SashForm(parent, SWT.VERTICAL);
    wEditorSashForm.setLayoutData(new FormDataBuilder().fullSize().result());

    this.createEditor(wEditorSashForm);
    this.createHelp(wEditorSashForm);

    wEditorSashForm.setWeights(100, 0);
  }

  protected void createHelp(final Composite parent) {
    Composite composite = new Composite(parent, SWT.BORDER);
    composite.setLayout(new FormLayout());

    // Create toolbar
    ToolBar toolbar = new ToolBar(composite, SWT.FLAT | SWT.HORIZONTAL | SWT.RIGHT_TO_LEFT);
    toolbar.setLayoutData(new FormDataBuilder().top().fullWidth().result());
    PropsUi.setLook(toolbar, Props.WIDGET_STYLE_TOOLBAR);

    ToolItem toolbarItem = new ToolItem(toolbar, SWT.PUSH);
    toolbarItem.setToolTipText("Close");
    toolbarItem.setImage(GuiResource.getInstance().getImageClose());
    toolbarItem.addListener(SWT.Selection, e -> hideHelp());

    // Create the widget browser
    wBrowser = new Browser(composite, SWT.NONE);
    wBrowser.setJavascriptEnabled(false);
    wBrowser.setLayoutData(new FormDataBuilder().top(toolbar).bottom().fullWidth().result());

    // Cancel opening of new windows
    wBrowser.addOpenWindowListener(e -> e.required = true);

    // Replace the browser's built-in context menu with none
    wBrowser.setMenu(new Menu(parent.getShell(), SWT.NONE));
  }

  protected void createEditor(final Composite parent) {

    Composite composite = new Composite(parent, SWT.BORDER);
    composite.setLayout(new FormLayout());

    IToolbarContainer toolbarContainer =
        ToolbarFacade.createToolbarContainer(composite, SWT.FLAT | SWT.HORIZONTAL);
    toolbarContainer.getControl().setLayoutData(new FormDataBuilder().top().fullWidth().result());
    PropsUi.setLook(toolbarContainer.getControl(), Props.WIDGET_STYLE_TOOLBAR);

    // Create an empty place to make it easier for plugins to use the toolbar widgets.
    //
    toolbarWidgets = new GuiToolbarWidgets();
    toolbarWidgets.registerGuiPluginObject(this);
    toolbarWidgets.createToolbarWidgets(toolbarContainer, ID_TOOLBAR);

    wViewer =
        new SourceViewer(composite, createVerticalRuler(), SWT.H_SCROLL | SWT.V_SCROLL | SWT.MULTI);

    wViewer
        .getControl()
        .setLayoutData(
            new FormDataBuilder().top(toolbarContainer.getControl()).bottom().fullWidth().result());
    wViewer.addSelectionChangedListener(event -> updateToolbar());

    final StyledText widget = wViewer.getTextWidget();

    widget.setFont(GuiResource.getInstance().getFontFixed());

    // In the Chinese window, Ctrl-SPACE is reserved by system for input Chinese character.
    // Use Ctrl-ALT-SPACE instead.
    final int modifierKeys =
        (System.getProperty("user.language").equals("zh")) ? SWT.CTRL | SWT.ALT : SWT.CTRL;

    widget.setTabs(4);
    widget.addListener(SWT.FocusIn, e -> hideHelp());
    widget.addListener(
        SWT.KeyDown,
        event -> {
          if (event.keyCode == SWT.SPACE && (event.stateMask & SWT.MODIFIER_MASK) == modifierKeys) {
            wViewer.doOperation(ISourceViewer.CONTENTASSIST_PROPOSALS);
          }
          if ((event.stateMask & SWT.MOD1) == 0 || (event.stateMask & SWT.MOD2) != 0) {
            return;
          }
          if (event.keyCode == 'a') {
            selectAll();
            event.doit = false;
          } else if (event.keyCode == 'f') {
            find();
            event.doit = false;
          } else if (event.keyCode == 'h') {
            if (wViewer.isEditable()) {
              findReplace();
            } else {
              find();
            }
            event.doit = false;
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

    Composite composite = new Composite(parent, SWT.BORDER);
    composite.setLayout(new FormLayout());

    ToolBar toolbar = new ToolBar(composite, SWT.FLAT | SWT.HORIZONTAL);
    toolbar.setLayoutData(new FormDataBuilder().top().fullWidth().result());
    PropsUi.setLook(toolbar, Props.WIDGET_STYLE_TOOLBAR);

    ToolItem toolbarItem = new ToolItem(toolbar, SWT.PUSH);
    toolbarItem.setToolTipText(BaseMessages.getString(PKG, "System.Tooltip.CollapseALl"));
    toolbarItem.setImage(GuiResource.getInstance().getImageCollapseAll());
    toolbarItem.addListener(SWT.Selection, e -> onTreeExpandCollapseAll(false));

    toolbarItem = new ToolItem(toolbar, SWT.PUSH);
    toolbarItem.setToolTipText(BaseMessages.getString(PKG, "System.Tooltip.ExpandAll"));
    toolbarItem.setImage(GuiResource.getInstance().getImageExpandAll());
    toolbarItem.addListener(SWT.Selection, e -> onTreeExpandCollapseAll(true));

    toolbar.pack();

    // Tree widget
    wTree = new Tree(composite, SWT.H_SCROLL | SWT.V_SCROLL);
    wTree.setLayoutData(new FormDataBuilder().top(toolbar).fullWidth().bottom().result());
    wTree.addListener(SWT.MouseDoubleClick, this::onTreeDoubleClick);
    PropsUi.setLook(wTree);

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

    wTree.addMenuDetectListener(
        event -> {
          if (wTree.getSelectionCount() == 0) {
            return;
          }
          TreeItem item = wTree.getSelection()[0];

          if (item != null && item.getData() != null) {
            // Context menu to display help
            Menu menu = new Menu(wTree);
            MenuItem menuItem = new MenuItem(menu, SWT.PUSH);
            menuItem.setText(BaseMessages.getString(PKG, "ExpressionEditor.Menu.Help.Label"));
            menuItem.setImage(GuiResource.getInstance().getImageHelp());
            menuItem.setData(item.getData());
            menuItem.addListener(SWT.Selection, e -> showHelp(item.getData()));
            menu.setVisible(true);
          }
        });
  }

  protected void onTreeDoubleClick(Event event) {
    Point point = new Point(event.x, event.y);
    TreeItem item = wTree.getItem(point);
    if (item == null || item.getData() == null) {
      return;
    }

    showHelp(item.getData());
  }

  protected void hideHelp() {
    wEditorSashForm.setWeights(100, 0);
  }

  /** Display help of the element. */
  protected void showHelp(Object element) {
    String text = labelProvider.getToolTipText(element);
    if (text != null) {
      this.wBrowser.setText(text);

      // If the help panel is too small (less than 20%), reset to 60%.
      int[] weights = this.wEditorSashForm.getWeights();
      if (((float) weights[1] / (weights[0] + weights[1])) < 0.2f) {
        this.wEditorSashForm.setWeights(40, 60);
      }
    }
  }

  /**
   * Create the vertical ruler for the source viewer.
   *
   * @return the vertical ruler
   */
  protected IVerticalRuler createVerticalRuler() {
    LineNumberRulerColumn lineNumberRulerColumn = new LineNumberRulerColumn();

    boolean dark = PropsUi.getInstance().isDarkMode();
    if (dark) {
      lineNumberRulerColumn.setForeground(new Color(this.getDisplay(), 130, 130, 130));
      lineNumberRulerColumn.setBackground(new Color(this.getDisplay(), 40, 40, 40));
    } else {
      lineNumberRulerColumn.setForeground(new Color(this.getDisplay(), 120, 120, 120));
    }

    // ineNumberRulerColumn.setBackground(GuiResource.getInstance().getColorLightGray());
    lineNumberRulerColumn.setFont(GuiResource.getInstance().getFontFixed());

    PropsUi props = PropsUi.getInstance();
    // Adapte annotation ruler to zoom factor
    AnnotationRulerColumn annotationRulerColumn =
        new AnnotationRulerColumn(
            (int) (ConstUi.SMALL_ICON_SIZE * props.getZoomFactor() + 2),
            new ExpressionAnnotationAccess());
    annotationRulerColumn.addAnnotationType(ANNOTATION_ERROR_TYPE);

    CompositeRuler ruler = new CompositeRuler(1);
    ruler.addDecorator(0, lineNumberRulerColumn);
    ruler.addDecorator(1, annotationRulerColumn);

    return ruler;
  }

  void createContextMenu() {
    StyledText styledText = wViewer.getTextWidget();

    Menu contextMenu = new Menu(styledText);
    contextMenuWidgets = new GuiMenuWidgets();
    contextMenuWidgets.registerGuiPluginObject(this);
    contextMenuWidgets.createMenuWidgets(
        GUI_PLUGIN_CONTEXT_MENU_PARENT_ID, styledText.getShell(), contextMenu);
    styledText.setMenu(contextMenu);
    styledText.addListener(
        SWT.MenuDetect,
        event -> {
          // Update the context menu items...
          contextMenuWidgets.enableMenuItem(
              ID_CONTEXT_MENU_UNDO, wViewer.canDoOperation(ITextOperationTarget.UNDO));
          contextMenuWidgets.enableMenuItem(
              ID_CONTEXT_MENU_REDO, wViewer.canDoOperation(ITextOperationTarget.REDO));
          contextMenuWidgets.enableMenuItem(
              ID_CONTEXT_MENU_CUT, wViewer.canDoOperation(ITextOperationTarget.CUT));
          contextMenuWidgets.enableMenuItem(
              ID_CONTEXT_MENU_COPY, wViewer.canDoOperation(ITextOperationTarget.COPY));
          contextMenuWidgets.enableMenuItem(
              ID_CONTEXT_MENU_PASTE, wViewer.canDoOperation(ITextOperationTarget.PASTE));
          contextMenuWidgets.enableMenuItem(ID_CONTEXT_MENU_FIND, false);
          contextMenuWidgets.enableMenuItem(
              ID_CONTEXT_MENU_FIND_REPLACE, false); // wViewer.isEditable());
        });
  }

  public boolean isEditable() {
    return wViewer.isEditable();
  }

  public String getText() {
    return wViewer.getDocument().get();
  }

  public void setText(String expression) {
    if (expression == null) return;

    wViewer.getDocument().set(expression);
  }

  public String getSelectionText() {
    return wViewer.getTextWidget().getSelectionText();
  }

  public int getSelectionCount() {
    return wViewer.getTextWidget().getSelectionCount();
  }

  public int getCaretPosition() {
    return wViewer.getTextWidget().getCaretOffset();
  }

  public void setCaretPosition(int position) {
    wViewer.getTextWidget().setCaretOffset(position);
  }

  public void insert(String text) {
    wViewer.getTextWidget().insert(text);
  }

  public boolean setFocus() {
    return wViewer.getTextWidget().setFocus();
  }

  public void updateToolbar() {
    boolean canUndo = wViewer.canDoOperation(ITextOperationTarget.UNDO);
    boolean canRedo = wViewer.canDoOperation(ITextOperationTarget.REDO);
    boolean canCut = wViewer.canDoOperation(ITextOperationTarget.CUT);
    boolean canCopy = wViewer.canDoOperation(ITextOperationTarget.COPY);
    boolean canPaste = wViewer.canDoOperation(ITextOperationTarget.PASTE);

    // Update the toolbar items...
    if (toolbarWidgets != null) {
      toolbarWidgets.enableToolbarItem(ID_TOOLBAR_UNDO, canUndo);
      toolbarWidgets.enableToolbarItem(ID_TOOLBAR_REDO, canRedo);
      toolbarWidgets.enableToolbarItem(ID_TOOLBAR_CUT, canCut);
      toolbarWidgets.enableToolbarItem(ID_TOOLBAR_COPY, canCopy);
      toolbarWidgets.enableToolbarItem(ID_TOOLBAR_PASTE, canPaste);
      toolbarWidgets.enableToolbarItem(ID_TOOLBAR_FIND, false);
      toolbarWidgets.enableToolbarItem(ID_TOOLBAR_FIND_REPLACE, false); // wViewer.isEditable());
    }
  }

  @GuiToolbarElement(
      root = ID_TOOLBAR,
      id = ID_TOOLBAR_COPY,
      image = "ui/images/copy.svg",
      toolTip = "i18n::ExpressionEditor.ToolBar.Copy.ToolTip",
      separator = true)
  @GuiMenuElement(
      root = GUI_PLUGIN_CONTEXT_MENU_PARENT_ID,
      parentId = GUI_PLUGIN_CONTEXT_MENU_PARENT_ID,
      id = ID_CONTEXT_MENU_COPY,
      label = "i18n::ExpressionEditor.Menu.Copy.Label",
      image = "ui/images/copy.svg",
      separator = true)
  @GuiKeyboardShortcut(control = true, key = 'c')
  @GuiOsxKeyboardShortcut(command = true, key = 'c')
  public void copy() {
    wViewer.doOperation(ITextOperationTarget.COPY);
  }

  @GuiToolbarElement(
      root = ID_TOOLBAR,
      id = ID_TOOLBAR_PASTE,
      image = "ui/images/paste.svg",
      toolTip = "i18n::ExpressionEditor.ToolBar.Paste.ToolTip")
  @GuiMenuElement(
      root = GUI_PLUGIN_CONTEXT_MENU_PARENT_ID,
      parentId = GUI_PLUGIN_CONTEXT_MENU_PARENT_ID,
      id = ID_CONTEXT_MENU_PASTE,
      label = "i18n::ExpressionEditor.Menu.Paste.Label",
      image = "ui/images/paste.svg")
  @GuiKeyboardShortcut(control = true, key = 'v')
  @GuiOsxKeyboardShortcut(command = true, key = 'v')
  public void paste() {
    wViewer.doOperation(ITextOperationTarget.PASTE);
  }

  @GuiToolbarElement(
      root = ID_TOOLBAR,
      id = ID_TOOLBAR_CUT,
      image = "ui/images/cut.svg",
      toolTip = "i18n::ExpressionEditor.ToolBar.Cut.ToolTip")
  @GuiMenuElement(
      root = GUI_PLUGIN_CONTEXT_MENU_PARENT_ID,
      parentId = GUI_PLUGIN_CONTEXT_MENU_PARENT_ID,
      id = ID_CONTEXT_MENU_CUT,
      label = "i18n::ExpressionEditor.Menu.Cut.Label",
      image = "ui/images/cut.svg")
  @GuiKeyboardShortcut(control = true, key = 'x')
  @GuiOsxKeyboardShortcut(command = true, key = 'x')
  public void cut() {
    wViewer.doOperation(ITextOperationTarget.CUT);
  }

  @GuiToolbarElement(
      root = ID_TOOLBAR,
      id = ID_TOOLBAR_SELECT_ALL,
      image = "ui/images/select-all.svg",
      toolTip = "i18n::ExpressionEditor.ToolBar.SelectAll.ToolTip",
      separator = true)
  @GuiMenuElement(
      root = GUI_PLUGIN_CONTEXT_MENU_PARENT_ID,
      parentId = GUI_PLUGIN_CONTEXT_MENU_PARENT_ID,
      id = ID_CONTEXT_MENU_SELECT_ALL,
      label = "i18n::ExpressionEditor.Menu.SelectAll.Label",
      image = "ui/images/select-all.svg",
      separator = true)
  @GuiKeyboardShortcut(control = true, key = 'a')
  @GuiOsxKeyboardShortcut(command = true, key = 'a')
  public void selectAll() {
    wViewer.doOperation(ITextOperationTarget.SELECT_ALL);
  }

  @GuiToolbarElement(
      root = ID_TOOLBAR,
      id = ID_TOOLBAR_UNSELECT_ALL,
      image = "ui/images/unselect-all.svg",
      toolTip = "i18n::ExpressionEditor.ToolBar.UnselectAll.ToolTip")
  @GuiMenuElement(
      root = GUI_PLUGIN_CONTEXT_MENU_PARENT_ID,
      parentId = GUI_PLUGIN_CONTEXT_MENU_PARENT_ID,
      id = ID_CONTEXT_MENU_UNSELECT_ALL,
      label = "i18n::ExpressionEditor.Menu.UnselectAll.Label",
      image = "ui/images/unselect-all.svg")
  public void unselectAll() {
    wViewer.setSelectedRange(0, 0);
  }

  @GuiToolbarElement(
      root = ID_TOOLBAR,
      id = ID_TOOLBAR_UNDO,
      image = "ui/images/undo.svg",
      toolTip = "i18n::ExpressionEditor.ToolBar.Undo.ToolTip")
  @GuiMenuElement(
      root = GUI_PLUGIN_CONTEXT_MENU_PARENT_ID,
      parentId = GUI_PLUGIN_CONTEXT_MENU_PARENT_ID,
      id = ID_CONTEXT_MENU_UNDO,
      label = "i18n::ExpressionEditor.Menu.Undo.Label",
      image = "ui/images/undo.svg")
  @GuiKeyboardShortcut(control = true, key = 'z')
  @GuiOsxKeyboardShortcut(command = true, key = 'z')
  public void undo() {
    wViewer.doOperation(ITextOperationTarget.UNDO);
  }

  @GuiToolbarElement(
      root = ID_TOOLBAR,
      id = ID_TOOLBAR_REDO,
      image = "ui/images/redo.svg",
      toolTip = "i18n::ExpressionEditor.ToolBar.Redo.ToolTip")
  @GuiMenuElement(
      root = GUI_PLUGIN_CONTEXT_MENU_PARENT_ID,
      parentId = GUI_PLUGIN_CONTEXT_MENU_PARENT_ID,
      id = ID_CONTEXT_MENU_REDO,
      label = "i18n::ExpressionEditor.Menu.Redo.Label",
      image = "ui/images/redo.svg")
  @GuiKeyboardShortcut(control = true, shift = true, key = 'z')
  @GuiOsxKeyboardShortcut(command = true, shift = true, key = 'z')
  public void redo() {
    wViewer.doOperation(ITextOperationTarget.REDO);
  }

  @GuiToolbarElement(
      root = ID_TOOLBAR,
      id = ID_TOOLBAR_FIND,
      image = "ui/images/search.svg",
      toolTip = "i18n::ExpressionEditor.ToolBar.Find.ToolTip",
      separator = true)
  @GuiMenuElement(
      root = GUI_PLUGIN_CONTEXT_MENU_PARENT_ID,
      parentId = GUI_PLUGIN_CONTEXT_MENU_PARENT_ID,
      id = ID_CONTEXT_MENU_FIND,
      label = "i18n::ExpressionEditor.Menu.Find.Label",
      image = "ui/images/search.svg",
      separator = true)
  @GuiKeyboardShortcut(control = true, key = 'f')
  @GuiOsxKeyboardShortcut(command = true, key = 'f')
  public static void find() {
    // FindReplaceDialog.open(control.getShell(), this, false);
  }

  @GuiToolbarElement(
      root = ID_TOOLBAR,
      id = ID_TOOLBAR_FIND_REPLACE,
      image = "ui/images/find-replace.svg",
      toolTip = "i18n::ExpressionEditor.ToolBar.FindAndReplace.ToolTip")
  @GuiMenuElement(
      root = GUI_PLUGIN_CONTEXT_MENU_PARENT_ID,
      parentId = GUI_PLUGIN_CONTEXT_MENU_PARENT_ID,
      id = ID_CONTEXT_MENU_FIND_REPLACE,
      label = "i18n::ExpressionEditor.Menu.FindAndReplace.Label",
      image = "ui/images/find-replace.svg")
  @GuiKeyboardShortcut(control = true, key = 'h')
  @GuiOsxKeyboardShortcut(command = true, key = 'h')
  public static void findReplace() {
    // FindReplaceDialog.open(control.getShell(), this, true);
  }

  @GuiToolbarElement(
      root = ID_TOOLBAR,
      id = ID_TOOLBAR_OPTIMIZE,
      image = "evaluate.svg",
      toolTip = "i18n::ExpressionEditor.ToolBar.Evaluate.ToolTip",
      separator = true)
  public void evaluate() {

    String source = wViewer.getTextWidget().getText();

    IExpression expression = IExpression.of(new RowExpressionContext(variables, rowMeta), source);

    MessageBox dialog = new MessageBox(getShell());
    dialog.setText("Expression simplified");
    dialog.setMessage(expression.toString());
    dialog.open();
  }

  protected void onTreeExpandCollapseAll(boolean expanded) {
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

      IExpression.of(context, expression);
    } catch (ParseExpressionException e) {
      Annotation annotation = new Annotation(ANNOTATION_ERROR_TYPE, false, e.getMessage());
      annotationModel.addAnnotation(annotation, new Position(e.getPosition(), 0));
    } catch (ExpressionException e) {
      Annotation annotation = new Annotation(ANNOTATION_ERROR_TYPE, false, e.getMessage());
      annotationModel.addAnnotation(annotation, new Position(0, 0));
    }

    updateToolbar();
  }
}
