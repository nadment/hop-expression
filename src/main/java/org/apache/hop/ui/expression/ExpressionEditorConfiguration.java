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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.Keyword;
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.ui.core.PropsUi;
import org.apache.hop.ui.core.gui.GuiResource;
import org.eclipse.jface.internal.text.html.HTMLTextPresenter;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.IUndoManager;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.TextViewerUndoManager;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.hyperlink.DefaultHyperlinkPresenter;
import org.eclipse.jface.text.hyperlink.IHyperlinkDetector;
import org.eclipse.jface.text.hyperlink.IHyperlinkPresenter;
import org.eclipse.jface.text.hyperlink.URLHyperlinkDetector;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.PatternRule;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.SingleLineRule;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.rules.WhitespaceRule;
import org.eclipse.jface.text.rules.WordRule;
import org.eclipse.jface.text.source.DefaultAnnotationHover;
import org.eclipse.jface.text.source.IAnnotationHover;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.jspecify.annotations.Nullable;

public class ExpressionEditorConfiguration extends SourceViewerConfiguration {

  private static final Class<?> PKG = ExpressionEditor.class;
  private static final Set<String> RESERVED_LITERALS = Set.of("NULL", "TRUE", "FALSE");
  private static final int MAX_UNDO_LEVEL = 25;

  private final IVariables variables;
  private final ExpressionMode mode;

  /** The fields or the arguments of a user-defined function, null until they are known. */
  private IRowMeta rowMeta;

  /** The names of the fields, case-sensitive, empty until the row meta is known. */
  private Set<String> fieldNames = Set.of();

  /** Opens the documentation of a function with Ctrl+click, or null for no link. */
  private @Nullable Consumer<Function> functionDocumentationOpener;

  public ExpressionEditorConfiguration(IVariables variables, ExpressionMode mode) {
    super();
    this.variables = variables;
    this.mode = mode;
  }

  /**
   * Sets the fields to highlight in the expression and to propose in the content assist, the viewer
   * must invalidate its text presentation to apply them.
   *
   * @param rowMeta the fields, or null if they are unknown
   */
  public void setRowMeta(IRowMeta rowMeta) {
    this.rowMeta = rowMeta;

    Set<String> names = new HashSet<>();
    if (rowMeta != null) {
      for (IValueMeta valueMeta : rowMeta.getValueMetaList()) {
        if (valueMeta != null && !Utils.isEmpty(valueMeta.getName())) {
          names.add(valueMeta.getName());
        }
      }
    }
    this.fieldNames = names;
  }

  @Override
  public String[] getConfiguredContentTypes(ISourceViewer sourceViewer) {
    return new String[] {
      IDocument.DEFAULT_CONTENT_TYPE,
      ExpressionPartitionScanner.COMMENT,
      ExpressionPartitionScanner.STRING
    };
  }

  @Override
  public IUndoManager getUndoManager(ISourceViewer sourceViewer) {
    // FIXME: Don't work
    return new TextViewerUndoManager(MAX_UNDO_LEVEL);
  }

  @Override
  public IContentAssistant getContentAssistant(ISourceViewer sourceViewer) {

    ExpressionCompletionProcessor expressionProcessor =
        new ExpressionCompletionProcessor(variables, () -> rowMeta, mode);
    ExpressionCompletionProcessor variableProcessor = new ExpressionCompletionProcessor(variables);

    ContentAssistant assistant = new ContentAssistant();
    assistant.setContentAssistProcessor(expressionProcessor, IDocument.DEFAULT_CONTENT_TYPE);
    assistant.setContentAssistProcessor(variableProcessor, ExpressionPartitionScanner.COMMENT);
    assistant.setContentAssistProcessor(variableProcessor, ExpressionPartitionScanner.STRING);
    assistant.setAutoActivationDelay(300);
    assistant.setShowEmptyList(false);
    assistant.setInformationControlCreator(getInformationControlCreator(sourceViewer));

    assistant.enablePrefixCompletion(true);
    assistant.enableAutoInsert(true);
    assistant.enableAutoActivation(true);
    // TODO implements colored labels
    assistant.enableColoredLabels(false);

    assistant.setProposalPopupOrientation(IContentAssistant.PROPOSAL_REMOVE);
    assistant.setProposalSelectorForeground(GuiResource.getInstance().getColorBlack());
    assistant.setProposalSelectorBackground(GuiResource.getInstance().getColorBackground());

    assistant.setContextSelectorForeground(GuiResource.getInstance().getColorBlack());
    assistant.setContextSelectorBackground(GuiResource.getInstance().getColorBackground());

    assistant.setContextInformationPopupOrientation(IContentAssistant.CONTEXT_INFO_ABOVE);
    assistant.setContextInformationPopupForeground(GuiResource.getInstance().getColorBlack());
    assistant.setContextInformationPopupBackground(GuiResource.getInstance().getColorBackground());

    assistant.setStatusLineVisible(true);
    assistant.setStatusMessage(
        BaseMessages.getString(
            PKG,
            "ExpressionEditor.ContentAssist.Status",
            ExpressionEditor.isAlternateContentAssistShortcut() ? "Ctrl+Alt+Space" : "Ctrl+Space"));
    return assistant;
  }

  @Override
  public IInformationControlCreator getInformationControlCreator(ISourceViewer sourceViewer) {
    return parent -> new DefaultInformationControl(parent, new HTMLTextPresenter());
  }

  @Override
  public IPresentationReconciler getPresentationReconciler(ISourceViewer sourceViewer) {
    PresentationReconciler reconciler = new PresentationReconciler();

    DefaultDamagerRepairer dr = new DefaultDamagerRepairer(createSourceScanner());
    reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
    reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);

    dr = new DefaultDamagerRepairer(createCommentScanner());
    reconciler.setDamager(dr, ExpressionPartitionScanner.COMMENT);
    reconciler.setRepairer(dr, ExpressionPartitionScanner.COMMENT);

    dr = new DefaultDamagerRepairer(createStringScanner());
    reconciler.setDamager(dr, ExpressionPartitionScanner.STRING);
    reconciler.setRepairer(dr, ExpressionPartitionScanner.STRING);

    return reconciler;
  }

  protected RuleBasedScanner createSourceScanner() {
    GuiResource resource = GuiResource.getInstance();
    Token identifier =
        new Token(new TextAttribute(resource.getColor(255, 127, 80), null, SWT.BOLD));
    Token keyword = new Token(new TextAttribute(resource.getColor(30, 144, 255)));
    // The dark violet is not readable on a dark background, use the lighter violet of the SQL
    // editor
    // of Hop in dark mode
    Color functionColor =
        PropsUi.getInstance().isDarkMode()
            ? resource.getColor(177, 102, 218)
            : resource.getColor(148, 0, 211);
    Token function = new Token(new TextAttribute(functionColor));
    Token number = new Token(new TextAttribute(resource.getColorOrange()));
    Token extra = new Token(new TextAttribute(resource.getColor(255, 0, 255)));
    Token variable = new Token(new TextAttribute(resource.getColorBlack(), null, SWT.BOLD));
    Token undefined = new Token(new TextAttribute(resource.getColorBlack()));

    List<IRule> rules = new ArrayList<>();

    // Add rule for variables
    rules.add(new SingleLineRule("${", "}", variable));

    // Add rule for quoted identifier
    rules.add(new PatternRule("\"", "\"", identifier, (char) 0, true));

    // Add rule for numbers
    rules.add(new LiteralNumberRule(number));

    // Add generic whitespace rule.
    rules.add(new WhitespaceRule(new WhitespaceDetector()));

    // Add rule for operator
    rules.add(new OperatorRule(keyword));

    // Add case-sensitive rule for identifier without double quote, the fields are checked when
    // the text is scanned because they are known after the editor is created
    rules.add(
        new NameRule(new IdentifierDetector(), name -> fieldNames.contains(name), identifier));

    // Add case-insensitive rule for reserved world, data type and time unit
    WordRule rule = new WordRule(new IdentifierDetector(), Token.UNDEFINED, true);
    for (String word : Keyword.get()) {
      if (RESERVED_LITERALS.contains(word)) rule.addWord(word, extra);
      else rule.addWord(word, keyword);
    }
    for (TypeName type : TypeName.values()) {
      rule.addWord(type.name(), extra);
    }
    for (TimeUnit unit : TimeUnit.values()) {
      for (String name : unit.names()) {
        if (!name.isEmpty()) {
          rule.addWord(name, extra);
        }
      }
    }
    rules.add(rule);

    // Add case-insensitive rule for function name, after the reserved words that have priority.
    // The functions are checked when the text is scanned, a user-defined function can be created
    // after the editor.
    rules.add(
        new NameRule(
            new IdentifierDetector(),
            name -> FunctionRegistry.getFunction(name) != null,
            function));

    RuleBasedScanner scanner = new RuleBasedScanner();
    scanner.setRules(rules.toArray(new IRule[0]));
    scanner.setDefaultReturnToken(undefined);
    return scanner;
  }

  protected RuleBasedScanner createCommentScanner() {
    Color color = GuiResource.getInstance().getColorDarkGray();
    Token token = new Token(new TextAttribute(color, null, SWT.ITALIC));
    Token variable = new Token(new TextAttribute(color, null, SWT.ITALIC | SWT.BOLD));

    IRule[] rules = {
      // Add rule for variables
      new PatternRule("${", "}", variable, (char) 0, false) // ,
      // new LinkRule(link)
    };

    RuleBasedScanner scanner = new RuleBasedScanner();
    scanner.setRules(rules);
    scanner.setDefaultReturnToken(token);
    return scanner;
  }

  protected RuleBasedScanner createStringScanner() {
    Color color = GuiResource.getInstance().getColor(8, 154, 0);
    Token token = new Token(new TextAttribute(color));
    Token variable = new Token(new TextAttribute(color, null, SWT.BOLD));

    IRule[] rules = {
      // Add rule for variables
      new PatternRule("${", "}", variable, (char) 0, false)
    };
    RuleBasedScanner scanner = new RuleBasedScanner();
    scanner.setRules(rules);
    scanner.setDefaultReturnToken(token);
    return scanner;
  }

  @Override
  public ITextDoubleClickStrategy getDoubleClickStrategy(
      ISourceViewer sourceViewer, String contentType) {
    return new DoubleClickStrategy();
  }

  @Override
  public ITextHover getTextHover(ISourceViewer sourceViewer, String contentType) {
    // In a string, a word is not a function or a field, only the variables are shown
    if (ExpressionPartitionScanner.STRING.equals(contentType)) {
      return new ExpressionTextHover(variables, () -> rowMeta, true);
    }
    // No hover in the comments
    if (!IDocument.DEFAULT_CONTENT_TYPE.equals(contentType)) {
      return null;
    }
    return new ExpressionTextHover(variables, () -> rowMeta, false);
  }

  @Override
  public IAnnotationHover getAnnotationHover(ISourceViewer sourceViewer) {
    return new DefaultAnnotationHover();
  }

  @Override
  public IHyperlinkPresenter getHyperlinkPresenter(ISourceViewer sourceViewer) {
    return new DefaultHyperlinkPresenter(GuiResource.getInstance().getColorLightBlue());
  }

  /**
   * Sets how the documentation of a function is opened with Ctrl+click, it must be set before the
   * viewer is configured.
   *
   * @param opener opens the documentation of a function, or null for no link on the functions
   */
  public void setFunctionDocumentationOpener(@Nullable Consumer<Function> opener) {
    this.functionDocumentationOpener = opener;
  }

  /**
   * {@inheritDoc}
   *
   * <p>The name of a function opens its documentation, an URL opens the browser.
   */
  @Override
  public IHyperlinkDetector[] getHyperlinkDetectors(ISourceViewer sourceViewer) {
    if (functionDocumentationOpener == null) {
      return super.getHyperlinkDetectors(sourceViewer);
    }
    return new IHyperlinkDetector[] {
      new FunctionHyperlinkDetector(functionDocumentationOpener), new URLHyperlinkDetector()
    };
  }
}
