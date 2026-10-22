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

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.function.Supplier;
import org.apache.commons.lang3.StringUtils;
import org.apache.hop.core.Const;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.variables.DescribedVariable;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.VariableRegistry;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.Identifier;
import org.apache.hop.expression.Keyword;
import org.apache.hop.expression.UserDefinedFunction;
import org.apache.hop.ui.core.gui.GuiResource;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ContextInformationValidator;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.swt.graphics.Image;

public class ExpressionCompletionProcessor implements IContentAssistProcessor {

  private final Supplier<IRowMeta> rowMeta;
  private final IVariables variables;
  private final ExpressionMode mode;
  private String message;

  public ExpressionCompletionProcessor(IVariables variables) {
    this(variables, null, ExpressionMode.NONE);
  }

  /**
   * Creates the processor of the proposals of an expression.
   *
   * @param variables the variables to propose
   * @param rowMeta the supplier of the fields or the arguments to propose, which returns null while
   *     they are not known. It is called in the UI thread, it must not wait for the fields. Null to
   *     propose no field.
   * @param mode the mode of the expression
   */
  public ExpressionCompletionProcessor(
      IVariables variables, Supplier<IRowMeta> rowMeta, ExpressionMode mode) {
    this.variables = variables;
    this.rowMeta = rowMeta;
    this.mode = mode;
  }

  @Override
  public ICompletionProposal[] computeCompletionProposals(ITextViewer textViewer, int offset) {

    // Clear the error message of a previous completion
    message = null;

    try {
      IDocument document = textViewer.getDocument();

      // Find significant characters entered by the user to restrict the number of
      // proposals
      int start = offset;
      char ch = 0;
      while (start > 0) {
        ch = document.getChar(start - 1);
        if (Character.isJavaIdentifierPart(ch) || ch == '.') start--;
        else break;
      }

      boolean quoted = false;
      if (ch == '\"') {
        quoted = true;
        start--;
      }
      if (ch == '{') {
        start--;
        if (start > 0) ch = document.getChar(start - 1);
      }
      if (ch == '$') {
        start--;
      }

      int end = offset;
      char next = 0;
      while (end < document.getLength()) {
        next = document.getChar(end);
        if (Character.isJavaIdentifierPart(next) || next == '.') end++;
        else break;
      }

      // A quoted identifier is replaced up to its closing quote on the same line. Without closing
      // quote, only the identifier characters are replaced, to not erase the end of the expression.
      // The lexer has no escape, the first double quote ends the identifier.
      if (quoted) {
        for (int index = end; index < document.getLength(); index++) {
          char c = document.getChar(index);
          if (c == '\"') {
            end = index + 1;
            break;
          }
          if (c == '\r' || c == '\n') break;
        }
      } else if (next == '}') {
        end++;
      }

      String prefix = document.get(start, offset - start);

      List<ICompletionProposal> proposals = new ArrayList<>();
      if (!prefix.isEmpty()) {
        if (prefix.charAt(0) == '$') {

          // If not a variable replacement then insert
          if (document.getChar(end - 1) != '}') {
            end = offset;
          }
          if (prefix.startsWith("${")) {
            prefix = prefix.substring(2);
          } else {
            prefix = prefix.substring(1);
          }

          computeVariableProposals(proposals, prefix, start, end);
        } else {
          if (rowMeta != null) {
            computeIdentifierProposals(proposals, prefix, start, end, quoted);
          }
          computeReservedWordProposals(proposals, prefix, start, end);
        }
      }
      proposals.sort(new ProposalComparator());
      return proposals.toArray(new ICompletionProposal[0]);
    } catch (Exception e) {
      message = e.getMessage();
    }
    return null;
  }

  /**
   * Checks if a name starts with the prefix entered by the user, ignoring case.
   *
   * @param name the name of the proposal
   * @param prefix the prefix entered by the user
   * @return true if the name starts with the prefix
   */
  static boolean startsWithIgnoreCase(String name, String prefix) {
    return name.regionMatches(true, 0, prefix, 0, prefix.length());
  }

  protected void computeVariableProposals(
      List<ICompletionProposal> proposals, String prefix, int start, int end) {
    // No variables to propose
    if (variables == null) {
      return;
    }
    for (String variableName : variables.getVariableNames()) {

      // Add to proposal if variable name start with the qualifier
      if (startsWithIgnoreCase(variableName, prefix)) {

        boolean isDeprecated =
            VariableRegistry.getInstance().getDeprecatedVariableNames().contains(variableName);

        String content = "${" + variableName + '}';
        // A deprecated variable is flagged in the list of proposals
        String displayName =
            isDeprecated ? variableName + Const.getDeprecatedPrefix() : variableName;
        Image image = GuiResource.getInstance().getImageVariable();

        String additionalInfo = null;
        DescribedVariable variable =
            VariableRegistry.getInstance().findDescribedVariable(variableName);
        if (variable != null) {
          additionalInfo = variable.getDescription();
        }

        CompletionProposal proposal =
            new CompletionProposal(
                content,
                start,
                end - start,
                content.length(),
                image,
                displayName,
                null,
                additionalInfo);

        proposals.add(proposal);
      }
    }
  }

  protected void computeReservedWordProposals(
      List<ICompletionProposal> proposals, String prefix, int start, int end) {
    // Keywords
    for (String word : Keyword.get()) {
      if (startsWithIgnoreCase(word, prefix)) {
        CompletionProposal proposal =
            new CompletionProposal(word, start, end - start, word.length(), null, word, null, null);
        proposals.add(proposal);
      }
    }

    // Functions
    Image image = GuiResource.getInstance().getImageFunction();
    for (String name : FunctionRegistry.getFunctionNames()) {
      if (startsWithIgnoreCase(name, prefix)) {

        Function function = FunctionRegistry.getFunction(name);
        if (function == null) {
          continue;
        }

        // Skip UDF proposal in UDF mode
        if (mode == ExpressionMode.UDF && function instanceof UserDefinedFunction) {
          continue;
        }

        String replacement = name;
        // TODO: show the arguments of the function in the proposal, like ABS(number)
        String displayName = name;
        CompletionProposal proposal =
            new CompletionProposal(
                replacement,
                start,
                end - start,
                replacement.length(),
                image,
                displayName,
                null,
                function.getDescription());
        proposals.add(proposal);
      }
    }
  }

  protected void computeIdentifierProposals(
      List<ICompletionProposal> proposals, String prefix, int start, int end, boolean quoted) {
    // Value meta
    IRowMeta fields = rowMeta.get();
    if (fields == null) {
      return;
    }

    if (prefix.charAt(0) == '\"') prefix = prefix.substring(1);
    for (IValueMeta valueMeta : fields.getValueMetaList()) {
      String name = valueMeta.getName();
      if (startsWithIgnoreCase(name, prefix)) {

        // If identifier name contains space or a special character, is a reserved word or a
        // function name must be quoted, same rules as when an expression is written
        String content = quoted ? '\"' + name + '\"' : Identifier.quoteIfNeeded(name);

        Image image = GuiResource.getInstance().getImage(valueMeta);
        String description =
            "<b>Type:</b> "
                + valueMeta.getTypeDesc()
                + "<br><b>Transform origin:</b> "
                + valueMeta.getOrigin()
                + "<br><b>Comment:</b> "
                + StringUtils.defaultString(valueMeta.getComments());

        CompletionProposal proposal =
            new CompletionProposal(
                content, start, end - start, content.length(), image, name, null, description);
        proposals.add(proposal);
      }
    }
  }

  @Override
  public char[] getCompletionProposalAutoActivationCharacters() {
    return new char[] {'$'};
  }

  @Override
  public char[] getContextInformationAutoActivationCharacters() {
    return new char[] {' ', '(', '$'};
  }

  @Override
  public IContextInformationValidator getContextInformationValidator() {
    return new ContextInformationValidator(this);
  }

  @Override
  public IContextInformation[] computeContextInformation(ITextViewer viewer, int offset) {
    return null;
  }

  @Override
  public String getErrorMessage() {
    return message;
  }

  static class ProposalComparator implements Comparator<ICompletionProposal>, Serializable {
    @Serial private static final long serialVersionUID = 1L;

    public int compare(ICompletionProposal p1, ICompletionProposal p2) {
      // Ignore case, the fields in lower case are not sorted after the functions
      return String.CASE_INSENSITIVE_ORDER.compare(p1.getDisplayString(), p2.getDisplayString());
    }
  }
}
