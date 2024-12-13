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
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.expression.ExpressionParser;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.ui.core.gui.GuiResource;
import org.eclipse.jface.internal.text.html.HTMLTextPresenter;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.IUndoManager;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.TextViewerUndoManager;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.hyperlink.DefaultHyperlinkPresenter;
import org.eclipse.jface.text.hyperlink.IHyperlinkPresenter;
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
import org.eclipse.swt.widgets.Shell;

public class ExpressionEditorConfiguration extends SourceViewerConfiguration {

  private static final Set<String> RESERVED_LITERALS = Set.of("NULL", "TRUE", "FALSE");
  private static final int MAX_UNDO_LEVEL = 25;

  private IVariables variables;
  private ExpressionMode mode;
  private CompletableFuture<IRowMeta> futurRowMeta;

  public ExpressionEditorConfiguration(
      IVariables variables, CompletableFuture<IRowMeta> futurRowMeta, ExpressionMode mode) {
    super();
    this.variables = variables;
    this.futurRowMeta = futurRowMeta;
    this.mode = mode;
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
        new ExpressionCompletionProcessor(variables, futurRowMeta, mode);
    ExpressionCompletionProcessor variableProcessor = new ExpressionCompletionProcessor(variables);

    ContentAssistant assistant = new ContentAssistant();
    assistant.setContentAssistProcessor(expressionProcessor, IDocument.DEFAULT_CONTENT_TYPE);
    assistant.setContentAssistProcessor(variableProcessor, ExpressionPartitionScanner.COMMENT);
    assistant.setContentAssistProcessor(variableProcessor, ExpressionPartitionScanner.STRING);
    assistant.setAutoActivationDelay(300);
    assistant.setShowEmptyList(false);
    assistant.setProposalPopupOrientation(IContentAssistant.PROPOSAL_STACKED);
    assistant.setInformationControlCreator(getInformationControlCreator(sourceViewer));
    assistant.enableAutoInsert(true);
    assistant.enableAutoActivation(true);
    assistant.enableColoredLabels(true);

    assistant.setContextSelectorBackground(GuiResource.getInstance().getColor(8, 154, 0));
    assistant.setContextInformationPopupOrientation(IContentAssistant.CONTEXT_INFO_ABOVE);
    // assistant.setContextInformationPopupBackground(GuiResource.getInstance().getColor(8, 154,
    // 0));
    assistant.setStatusLineVisible(true);
    assistant.setStatusMessage("Press 'Ctrl+Space' to show variables");
    return assistant;
  }

  @Override
  public IInformationControlCreator getInformationControlCreator(ISourceViewer sourceViewer) {
    return new IInformationControlCreator() {
      public IInformationControl createInformationControl(Shell parent) {
        return new DefaultInformationControl(parent, new HTMLTextPresenter());
      }
    };
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
    Token function = new Token(new TextAttribute(resource.getColor(148, 0, 211)));
    Token string = new Token(new TextAttribute(resource.getColor(8, 154, 0)));
    Token number = new Token(new TextAttribute(resource.getColorOrange()));
    Token extra = new Token(new TextAttribute(resource.getColor(255, 0, 255)));
    Token variable = new Token(new TextAttribute(resource.getColorBlack(), null, SWT.BOLD));
    Token undefined = new Token(new TextAttribute(resource.getColorBlack()));

    List<IRule> rules = new ArrayList<>();

    // Add rule for string
    rules.add(new SingleLineRule("'", "'", string, '\'', false));

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

    // Add case sensitive rule for identifier without double quote
    if (futurRowMeta != null) {
      WordRule rule = new WordRule(new IndentifierDetector(), Token.UNDEFINED, false);
      try {
        IRowMeta rowMeta = futurRowMeta.get();
        if (rowMeta != null) {
          for (IValueMeta vm : rowMeta.getValueMetaList()) {
            rule.addWord(vm.getName(), identifier);
          }
        }
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
      } catch (ExecutionException e) {
        // Ignore
      }
      rules.add(rule);
    }

    // Add case insensitive rule for reserved world and function name
    // If word not found use Token.WHITESPACE to signal problem
    WordRule rule = new WordRule(new WordDetector(), Token.UNDEFINED, true);
    for (String name : FunctionRegistry.getFunctionNames()) {
      rule.addWord(name, function);
    }
    for (String word : ExpressionParser.getReservedWords()) {
      if (RESERVED_LITERALS.contains(word)) rule.addWord(word, extra);
      else rule.addWord(word, keyword);
    }
    for (TypeName type : TypeName.values()) {
      rule.addWord(type.name(), extra);
    }
    for (TimeUnit unit : TimeUnit.values()) {
      for (String name : unit.names()) {
        rule.addWord(name, extra);
      }
    }
    rules.add(rule);

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
    return new ExpressionTextHover();
  }

  @Override
  public IAnnotationHover getAnnotationHover(ISourceViewer sourceViewer) {
    return new DefaultAnnotationHover();
  }

  @Override
  public IHyperlinkPresenter getHyperlinkPresenter(ISourceViewer sourceViewer) {
    return new DefaultHyperlinkPresenter(GuiResource.getInstance().getColorLightBlue());
  }
}
