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
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicReference;
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
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension3;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextOperationTarget;
import org.eclipse.jface.text.IUndoManager;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.rules.FastPartitioner;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.AnnotationModel;
import org.eclipse.jface.text.source.AnnotationPainter;
import org.eclipse.jface.text.source.AnnotationRulerColumn;
import org.eclipse.jface.text.source.CompositeRuler;
import org.eclipse.jface.text.source.DefaultCharacterPairMatcher;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.IAnnotationModelExtension;
import org.eclipse.jface.text.source.ICharacterPairMatcher;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.jface.text.source.LineNumberRulerColumn;
import org.eclipse.jface.text.source.MatchingCharacterPainter;
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
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Editor of an expression, with the syntax highlighting, the content assist, the errors underlined
 * and a tree of the fields, operators, functions and variables to insert.
 */
@GuiPlugin
@NullMarked
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
  public static final String ID_TOOLBAR_COMMENT = "ExpressionEditor-Toolbar-10400-comment";
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
  public static final String ID_CONTEXT_MENU_COMMENT = "ExpressionEditor-ContextMenu-40000-comment";
  public static final String ID_CONTEXT_MENU_FIND = "ExpressionEditor-ContextMenu-50000-find";
  public static final String ID_CONTEXT_MENU_FIND_REPLACE =
      "ExpressionEditor-ContextMenu-50010-find-replace";

  private static final String ANNOTATION_ERROR_TYPE = ExpressionAnnotationAccess.ERROR_TYPE;

  /** The delay in milliseconds after the last change before the expression is validated. */
  private static final int VALIDATION_DELAY = 300;

  /** The prefix of a line comment written by {@link #toggleComment()}. */
  private static final String LINE_COMMENT = "//";

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
  private final @Nullable IVariables variables;
  private SourceViewer wViewer;
  private Tree wTree;
  private GuiToolbarWidgets toolbarWidgets;
  private GuiMenuWidgets contextMenuWidgets;

  /** The fields, set by the thread of the row meta future and read by the UI thread. */
  private final AtomicReference<@Nullable IRowMeta> rowMeta = new AtomicReference<>();

  private Browser wBrowser;
  private SashForm wEditorSashForm;

  /** The tree item of the fields or arguments, or null if the mode has no fields. */
  private @Nullable TreeItem wTreeItemFields;

  /** The configuration of the viewer, which highlights the fields and proposes them. */
  private ExpressionEditorConfiguration configuration;

  /** Validates the expression after the delay of the typing, unless the editor is closed. */
  private final Runnable delayedValidation =
      () -> {
        if (!isDisposed()) {
          validate();
        }
      };

  /**
   * Creates the expression editor, with the tree of the elements and the help panel.
   *
   * @param parent the parent composite
   * @param style the style of the editor
   * @param variables the variables to list and resolve, or null
   * @param mode the mode of the expression, which defines the elements available
   * @param rowMetaFuture the future fields of the row, or null if they are set with {@link
   *     #setRowMeta(IRowMeta)}
   */
  public ExpressionEditor(
      Composite parent,
      int style,
      @Nullable IVariables variables,
      ExpressionMode mode,
      @Nullable CompletableFuture<IRowMeta> rowMetaFuture) {
    super(parent, style);
    this.variables = variables;
    this.mode = mode;
    this.labelProvider = new ExpressionLabelProvider();

    PropsUi.setLook(this);

    this.setLayout(new FormLayout());
    SashForm wSashForm = new SashForm(this, SWT.HORIZONTAL);
    wSashForm.setLayoutData(new FormDataBuilder().fullSize().result());
    this.createTree(wSashForm);
    this.createSash(wSashForm);
    this.createContextMenu();

    // When IRowMeta is ready
    if (rowMetaFuture != null) {
      rowMetaFuture.thenAccept(this::setRowMeta);
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

    // The fixed font, the background and the text color of the theme, dark or light, like the other
    // text widgets of Hop. The editor look is set before the text is created.
    PropsUi.setLook(widget, Props.WIDGET_STYLE_FIXED);

    // In the Chinese window, Ctrl-SPACE is reserved by system for input Chinese character.
    // Use Ctrl-ALT-SPACE instead.
    final int modifierKeys = isAlternateContentAssistShortcut() ? SWT.CTRL | SWT.ALT : SWT.CTRL;

    widget.setTabs(4);
    widget.addListener(SWT.FocusIn, e -> hideHelp());
    widget.addListener(
        SWT.KeyDown,
        event -> {
          if (event.keyCode == SWT.SPACE && (event.stateMask & SWT.MODIFIER_MASK) == modifierKeys) {
            wViewer.doOperation(ISourceViewer.CONTENTASSIST_PROPOSALS);
          }
          if ((event.stateMask & SWT.MOD1) == 0) {
            return;
          }
          // The text widget has no undo, the shortcuts are handled here
          if (event.keyCode == 'z') {
            if ((event.stateMask & SWT.MOD2) != 0) {
              redo();
            } else {
              undo();
            }
            event.doit = false;
            return;
          }
          // Toggles the comment of the lines with Ctrl+/, or Ctrl+Shift+C like in Eclipse for the
          // keyboards where the slash needs Shift, like AZERTY
          if (event.keyCode == '/'
              || event.keyCode == SWT.KEYPAD_DIVIDE
              || (event.keyCode == 'c' && (event.stateMask & SWT.MOD2) != 0)) {
            toggleComment();
            event.doit = false;
            return;
          }
          if ((event.stateMask & SWT.MOD2) != 0) {
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
          }

          public void dragOperationChanged(DropTargetEvent event) {
            // Nothing to do
          }

          public void dragLeave(DropTargetEvent event) {
            // Nothing to do
          }

          public void dropAccept(DropTargetEvent event) {
            // Nothing to do
          }

          public void drop(DropTargetEvent event) {
            // A read-only editor accepts no drop
            if (wViewer.isEditable() && textTransfer.isSupportedType(event.currentDataType)) {
              String str = (String) event.data;
              StyledText styledText = wViewer.getTextWidget();

              // Insert at the drop location, or at the caret if not over the text
              Point point = styledText.toControl(event.x, event.y);
              int offset = styledText.getOffsetAtPoint(point);
              if (offset < 0) {
                offset = styledText.getCaretOffset();
              }
              styledText.replaceTextRange(offset, 0, str);
              styledText.setCaretOffset(offset + str.length());
            }
          }
        });

    Document document = new Document();
    document.addDocumentListener(this);

    configuration = new ExpressionEditorConfiguration(variables, mode);
    // Ctrl+click on a function shows its documentation in the help panel
    configuration.setFunctionDocumentationOpener(this::showHelp);
    IDocumentPartitioner partitioner =
        new FastPartitioner(
            new ExpressionPartitionScanner(), configuration.getConfiguredContentTypes(wViewer));
    partitioner.connect(document);
    document.setDocumentPartitioner(partitioner);

    wViewer.setDocument(document, new AnnotationModel());
    wViewer.configure(configuration);
    wViewer.addPainter(createErrorPainter());
    wViewer.addPainter(createBracketPainter());
  }

  /**
   * Highlights the bracket matching the bracket next to the caret, a box is drawn around it.
   *
   * <p>The brackets are matched in the same partition, a bracket in a string or a comment is not
   * matched with a bracket of the expression.
   */
  private MatchingCharacterPainter createBracketPainter() {
    ICharacterPairMatcher matcher =
        new DefaultCharacterPairMatcher(
            new char[] {'(', ')', '[', ']'}, IDocumentExtension3.DEFAULT_PARTITIONING);
    MatchingCharacterPainter painter = new MatchingCharacterPainter(wViewer, matcher);
    GuiResource resource = GuiResource.getInstance();
    painter.setColor(
        PropsUi.getInstance().isDarkMode()
            ? resource.getColor(150, 150, 150)
            : resource.getColor(128, 128, 128));
    return painter;
  }

  /**
   * Underlines the errors in the text, the ruler shows their icon.
   *
   * <p>The deprecated SquigglesStrategy is used because the text style strategy
   * UnderlineStrategy(SWT.UNDERLINE_SQUIGGLE) shows no underline in this editor, even with the
   * painter added as text presentation listener.
   */
  @SuppressWarnings("deprecation")
  private AnnotationPainter createErrorPainter() {
    AnnotationPainter painter = new AnnotationPainter(wViewer, new ExpressionAnnotationAccess());
    painter.addDrawingStrategy(ANNOTATION_ERROR_TYPE, new AnnotationPainter.SquigglesStrategy());
    // painter.paint(AnnotationPainter.TEXT_CHANGE);
    // painter.addTextStyleStrategy(ANNOTATION_ERROR_TYPE, new
    // AnnotationPainter.UnderlineStrategy(SWT.UNDERLINE_SQUIGGLE));
    painter.addAnnotationType(ANNOTATION_ERROR_TYPE, ANNOTATION_ERROR_TYPE);
    painter.setAnnotationTypeColor(ANNOTATION_ERROR_TYPE, GuiResource.getInstance().getColorRed());
    return painter;
  }

  /**
   * Checks if the content assist uses Ctrl+Alt+Space instead of Ctrl+Space. In the Chinese window,
   * Ctrl+Space is reserved by system for input Chinese character.
   *
   * @return true if the shortcut is Ctrl+Alt+Space
   */
  static boolean isAlternateContentAssistShortcut() {
    return "zh".equals(System.getProperty("user.language"));
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
    // TODO: in 2.20 use HopTree
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
            event.doit = wTree.getSelectionCount() > 0 && wTree.getSelection()[0].getData() != null;
          }

          @Override
          public void dragSetData(DragSourceEvent event) {
            if (wTree.getSelectionCount() == 0) {
              return;
            }
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
      wTreeItemFields = new TreeItem(wTree, SWT.NULL);
      wTreeItemFields.setImage(GuiResource.getInstance().getImageFolder());
      String text =
          (mode == ExpressionMode.UDF)
              ? "ExpressionEditor.Tree.Arguments.Label"
              : "ExpressionEditor.Tree.Fields.Label";
      wTreeItemFields.setText(BaseMessages.getString(PKG, text));
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
      for (String name : names) {
        boolean isDeprecated = variableRegistry.getDeprecatedVariableNames().contains(name);

        // The described variable is shared by Hop, it must not be modified
        DescribedVariable variable = variableRegistry.findDescribedVariable(name);
        if (variable == null) {
          variable = new DescribedVariable(name, null, null);
        }

        TreeItem item = new TreeItem(treeItemVariable, SWT.NULL);
        item.setImage(GuiResource.getInstance().getImageVariable());
        item.setText(name);
        item.setGrayed(isDeprecated);
        item.setData(variable);
      }
    }

    // Context menu to display help, created once and shown only for an element
    Menu menu = new Menu(wTree);
    MenuItem menuItem = new MenuItem(menu, SWT.PUSH);
    menuItem.setText(BaseMessages.getString(PKG, "ExpressionEditor.Menu.Help.Label"));
    menuItem.setImage(GuiResource.getInstance().getImageHelp());
    menuItem.addListener(
        SWT.Selection,
        e -> {
          if (wTree.getSelectionCount() > 0) {
            showHelp(wTree.getSelection()[0].getData());
          }
        });
    wTree.setMenu(menu);
    wTree.addMenuDetectListener(
        event ->
            event.doit =
                wTree.getSelectionCount() > 0 && wTree.getSelection()[0].getData() != null);
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
    PropsUi props = PropsUi.getInstance();
    LineNumberRulerColumn lineNumberRulerColumn = new LineNumberRulerColumn();

    GuiResource resource = GuiResource.getInstance();
    if (props.isDarkMode()) {
      lineNumberRulerColumn.setForeground(resource.getColor(130, 130, 130));
      lineNumberRulerColumn.setBackground(resource.getColor(40, 40, 40));
    } else {
      lineNumberRulerColumn.setForeground(resource.getColor(120, 120, 120));
    }

    // lineNumberRulerColumn.setBackground(GuiResource.getInstance().getColorLightGray());
    lineNumberRulerColumn.setFont(GuiResource.getInstance().getFontFixed());

    // Adapt the annotation ruler to the zoom factor
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

  /**
   * Checks if the expression can be edited.
   *
   * @return true if the expression is editable
   */
  public boolean isEditable() {
    return wViewer.isEditable();
  }

  /**
   * Gets the source of the expression.
   *
   * @return the expression
   */
  public String getText() {
    return wViewer.getDocument().get();
  }

  /**
   * Loads the source of the expression, which can't be undone.
   *
   * @param expression the expression, ignored if null
   */
  public void setText(@Nullable String expression) {
    if (expression == null) return;

    wViewer.getDocument().set(expression);

    // The errors of the loaded expression are shown at once, without the delay of the typing
    getDisplay().timerExec(-1, delayedValidation);
    validate();

    // The loaded expression must not be undone
    IUndoManager undoManager = wViewer.getUndoManager();
    if (undoManager != null) {
      undoManager.reset();
    }
    updateToolbar();
  }

  /**
   * Gets the selected text.
   *
   * @return the selected text, empty if there is no selection
   */
  public String getSelectionText() {
    return wViewer.getTextWidget().getSelectionText();
  }

  /**
   * Gets the number of selected characters.
   *
   * @return the length of the selection
   */
  public int getSelectionCount() {
    return wViewer.getTextWidget().getSelectionCount();
  }

  /**
   * Gets the position of the caret.
   *
   * @return the offset of the caret in the expression
   */
  public int getCaretPosition() {
    return wViewer.getTextWidget().getCaretOffset();
  }

  /**
   * Moves the caret.
   *
   * @param position the offset of the caret in the expression
   */
  public void setCaretPosition(int position) {
    wViewer.getTextWidget().setCaretOffset(position);
  }

  /**
   * Replaces the selection with the text, the caret does not move.
   *
   * @param text the text to insert
   */
  public void insert(String text) {
    wViewer.getTextWidget().insert(text);
  }

  /**
   * {@inheritDoc}
   *
   * <p>The focus is set on the text of the expression.
   */
  @Override
  public boolean setFocus() {
    return wViewer.getTextWidget().setFocus();
  }

  /** Enables the items of the toolbar that apply to the state of the text. */
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
      toolbarWidgets.enableToolbarItem(
          ID_TOOLBAR_FIND_REPLACE, false); // TODO in 2.20 : wViewer.isEditable());
    }
  }

  /** Copies the selected text to the clipboard. */
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

  /** Replaces the selection with the text of the clipboard. */
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

  /** Moves the selected text to the clipboard. */
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

  /** Selects the whole expression. */
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

  /** Clears the selection, the caret stays in place. */
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
    wViewer.setSelectedRange(getCaretPosition(), 0);
  }

  /**
   * Comments or uncomments the lines of the selection, or the line of the caret.
   *
   * <p>If all the lines that are not blank start with a line comment {@code //} or {@code --},
   * after spaces, the comment is removed, otherwise {@code // } is added at the start of each line
   * that is not blank. The change is undone at once.
   */
  @GuiToolbarElement(
      root = ID_TOOLBAR,
      id = ID_TOOLBAR_COMMENT,
      image = "comment.svg",
      toolTip = "i18n::ExpressionEditor.ToolBar.Comment.ToolTip",
      separator = true)
  @GuiMenuElement(
      root = GUI_PLUGIN_CONTEXT_MENU_PARENT_ID,
      parentId = GUI_PLUGIN_CONTEXT_MENU_PARENT_ID,
      id = ID_CONTEXT_MENU_COMMENT,
      label = "i18n::ExpressionEditor.Menu.Comment.Label",
      image = "comment.svg",
      separator = true)
  @GuiKeyboardShortcut(control = true, key = '/')
  @GuiOsxKeyboardShortcut(command = true, key = '/')
  public void toggleComment() {
    if (!wViewer.isEditable()) return;

    Point range = wViewer.getSelectedRange();
    IUndoManager undoManager = wViewer.getUndoManager();
    if (undoManager != null) undoManager.beginCompoundChange();
    try {
      toggleComment(wViewer.getDocument(), range.x, range.y);
    } catch (BadLocationException e) {
      // The selection is outside the document, nothing to do
    } finally {
      if (undoManager != null) undoManager.endCompoundChange();
    }
  }

  /**
   * Comments or uncomments the lines of a range of the document, see {@link #toggleComment()}.
   *
   * @param document the document
   * @param offset the start of the range
   * @param length the length of the range, 0 for the line of the offset
   * @throws BadLocationException if the range is outside the document
   */
  static void toggleComment(IDocument document, int offset, int length)
      throws BadLocationException {
    int startLine = document.getLineOfOffset(offset);
    int endLine = document.getLineOfOffset(offset + length);
    // A selection that ends at the start of a line does not include this line
    if (endLine > startLine && document.getLineOffset(endLine) == offset + length) {
      endLine--;
    }

    boolean commented = false;
    for (int line = startLine; line <= endLine; line++) {
      String text = getLineText(document, line);
      if (text.isBlank()) continue;
      if (findLineComment(text) < 0) {
        commented = false;
        break;
      }
      commented = true;
    }

    // From the last line, the offsets of the previous lines do not change
    for (int line = endLine; line >= startLine; line--) {
      String text = getLineText(document, line);
      int lineOffset = document.getLineOffset(line);
      if (commented) {
        int index = findLineComment(text);
        if (index >= 0) {
          // Remove the comment prefix and the space written after it
          int prefixLength = text.startsWith(" ", index + 2) ? 3 : 2;
          document.replace(lineOffset + index, prefixLength, "");
        }
      } else if (!text.isBlank() || startLine == endLine) {
        document.replace(lineOffset, 0, LINE_COMMENT + " ");
      }
    }
  }

  /** Gets the text of a line without its delimiter. */
  private static String getLineText(IDocument document, int line) throws BadLocationException {
    IRegion region = document.getLineInformation(line);
    return document.get(region.getOffset(), region.getLength());
  }

  /**
   * Finds the line comment {@code //} or {@code --} at the start of a line, after spaces.
   *
   * @return the index of the comment, or -1 if the line does not start with a line comment
   */
  private static int findLineComment(String text) {
    int index = 0;
    while (index < text.length() && Character.isWhitespace(text.charAt(index))) {
      index++;
    }
    return (text.startsWith(LINE_COMMENT, index) || text.startsWith("--", index)) ? index : -1;
  }

  /** Undoes the last change of the expression. */
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

  /** Redoes the last undone change of the expression. */
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

  /** Finds a text in the expression, not available yet. */
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
    // TODO: only available in 2.20, for now button is disabled
    // FindReplaceDialog.open(control.getShell(), this, false);
  }

  /** Finds and replaces a text in the expression, not available yet. */
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
    // TODO: only available in 2.20, for now button is disabled
    // FindReplaceDialog.open(control.getShell(), this, true);
  }

  /** Compiles the expression and shows its optimized form, or the error if it is invalid. */
  @GuiToolbarElement(
      root = ID_TOOLBAR,
      id = ID_TOOLBAR_OPTIMIZE,
      image = "evaluate.svg",
      toolTip = "i18n::ExpressionEditor.ToolBar.Evaluate.ToolTip",
      separator = true)
  public void evaluate() {

    String source = wViewer.getTextWidget().getText();

    MessageBox dialog;
    try {
      IExpression expression = IExpression.of(createExpressionContext(), source);
      dialog = new MessageBox(getShell(), SWT.ICON_INFORMATION | SWT.OK);
      dialog.setText(BaseMessages.getString(PKG, "ExpressionEditor.Evaluate.Title"));
      dialog.setMessage(expression.toString());
    } catch (RuntimeException e) {
      // An invalid expression, ExpressionException is a RuntimeException
      dialog = new MessageBox(getShell(), SWT.ICON_ERROR | SWT.OK);
      dialog.setText(BaseMessages.getString(PKG, "ExpressionEditor.Evaluate.Error.Title"));
      dialog.setMessage(String.valueOf(e.getMessage()));
    }
    dialog.open();
  }

  /**
   * Creates the context to compile the expression, with the fields when they are known.
   *
   * @return the expression context
   */
  private IExpressionContext createExpressionContext() {
    IRowMeta fields = rowMeta.get();
    if (fields == null) {
      return new ExpressionContext(variables);
    }
    return new RowExpressionContext(variables, fields);
  }

  protected void onTreeExpandCollapseAll(boolean expanded) {
    // Stop redraw until operation complete
    wTree.setRedraw(false);
    for (TreeItem item : wTree.getItems()) {
      item.setExpanded(expanded);
      for (TreeItem i : item.getItems()) {
        i.setExpanded(expanded);
      }
    }
    wTree.setRedraw(true);
  }

  /**
   * Sets the fields of the row, listed in the tree, highlighted and used to validate the
   * expression. It can be called from any thread.
   *
   * @param rowMeta the fields, or null if they are unknown
   */
  public void setRowMeta(final @Nullable IRowMeta rowMeta) {
    this.rowMeta.set(rowMeta);

    // The row meta can be ready after the editor is closed, or the mode has no fields
    if (isDisposed() || wTreeItemFields == null) {
      return;
    }

    getDisplay()
        .asyncExec(
            () -> {
              if (wTreeItemFields.isDisposed()) {
                return;
              }

              // Highlight the fields in the expression
              configuration.setRowMeta(rowMeta);
              wViewer.invalidateTextPresentation();

              // Validate again, the fields were unresolved identifiers before
              validate();

              // Remove existing fields
              wTreeItemFields.removeAll();

              if (rowMeta != null) {
                for (int i = 0; i < rowMeta.size(); i++) {
                  IValueMeta valueMeta = rowMeta.getValueMeta(i);

                  TreeItem item = new TreeItem(wTreeItemFields, SWT.NULL);
                  item.setImage(GuiResource.getInstance().getImage(valueMeta));
                  item.setText(valueMeta.getName());
                  item.setData(valueMeta);
                }
              }
            });
  }

  /**
   * {@inheritDoc}
   *
   * <p>The events of the text, like {@code SWT.Modify}, are listened on the text widget, the other
   * events like {@code SWT.Dispose} or {@code SWT.Resize} on the editor itself.
   */
  @Override
  public void addListener(int eventType, Listener listener) {
    if (isTextEvent(eventType)) {
      wViewer.getTextWidget().addListener(eventType, listener);
    } else {
      super.addListener(eventType, listener);
    }
  }

  /** {@inheritDoc} */
  @Override
  public void removeListener(int eventType, Listener listener) {
    if (isTextEvent(eventType)) {
      wViewer.getTextWidget().removeListener(eventType, listener);
    } else {
      super.removeListener(eventType, listener);
    }
  }

  /**
   * Checks if an event is an event of the text widget, the text widget must be created.
   *
   * @param eventType the type of event
   * @return true if the event is listened on the text widget
   */
  private boolean isTextEvent(int eventType) {
    return wViewer != null
        && switch (eventType) {
          case SWT.Modify, SWT.Verify, SWT.KeyDown, SWT.KeyUp, SWT.FocusIn, SWT.FocusOut -> true;
          default -> false;
        };
  }

  /** {@inheritDoc} */
  @Override
  public void documentAboutToBeChanged(DocumentEvent event) {
    // Only the changed document is handled
  }

  /**
   * {@inheritDoc}
   *
   * <p>Underlines the errors of the changed expression and updates the toolbar.
   */
  @Override
  public void documentChanged(DocumentEvent event) {
    // The expression is compiled when the user pauses, not for each key typed. A runnable is
    // scheduled once, the delay restarts with each change.
    getDisplay().timerExec(VALIDATION_DELAY, delayedValidation);

    // The undo manager, notified after this listener, records the change later
    getDisplay()
        .asyncExec(
            () -> {
              if (!isDisposed()) {
                updateToolbar();
              }
            });
  }

  /** Underlines the errors of the expression, compiled with the fields when they are known. */
  private void validate() {
    // Remove all annotations at once, the model is the AnnotationModel set with the document
    IAnnotationModel annotationModel = wViewer.getAnnotationModel();
    ((IAnnotationModelExtension) annotationModel).removeAllAnnotations();

    IDocument document = wViewer.getDocument();
    String expression = document.get();
    try {
      IExpression.of(createExpressionContext(), expression);
    } catch (ParseExpressionException e) {
      Annotation annotation = new Annotation(ANNOTATION_ERROR_TYPE, false, e.getMessage());
      annotationModel.addAnnotation(annotation, findErrorPosition(document, e.getPosition()));
    } catch (RuntimeException e) {
      // An ExpressionException, or an unexpected error, must not break the editing.
      // Without position, the whole expression is underlined.
      Annotation annotation =
          new Annotation(ANNOTATION_ERROR_TYPE, false, String.valueOf(e.getMessage()));
      annotationModel.addAnnotation(annotation, new Position(0, expression.length()));
    }
  }

  /**
   * Finds the position to underline for an error: the word at the position of the error, or the
   * character at this position, or the last character if the error is at the end.
   *
   * @param document the document
   * @param offset the position of the error
   * @return the position to underline
   */
  private static Position findErrorPosition(IDocument document, int offset) {
    int length = document.getLength();
    if (length == 0) {
      return new Position(0, 0);
    }
    // At the end of the expression, for example a missing parenthesis
    if (offset >= length) {
      return new Position(length - 1, 1);
    }
    try {
      IRegion word = ExpressionTextHover.findWordRegion(document, Math.max(offset, 0));
      if (word.getLength() > 0) {
        return new Position(word.getOffset(), word.getLength());
      }
    } catch (BadLocationException e) {
      // Underline the character
    }
    return new Position(Math.max(offset, 0), 1);
  }
}
