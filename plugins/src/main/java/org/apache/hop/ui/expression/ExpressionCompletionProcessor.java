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

import org.apache.commons.lang.StringUtils;
import org.apache.hop.core.Const;
import org.apache.hop.core.HopVariablesList;
import org.apache.hop.core.config.DescribedVariable;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.expression.DatePart;
import org.apache.hop.expression.ExpressionScanner;
import org.apache.hop.expression.OperatorRegistry;
import org.apache.hop.expression.Type;
import org.apache.hop.expression.operator.Function;
import org.apache.hop.ui.core.gui.GuiResource;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ContextInformation;
import org.eclipse.jface.text.contentassist.ContextInformationValidator;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.swt.graphics.Image;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

public class ExpressionCompletionProcessor implements IContentAssistProcessor {

  private CompletableFuture<IRowMeta> rowMeta;
  private IVariables variables;
  private String message;

  public ExpressionCompletionProcessor(IVariables variables) {
    this(variables, null);
  }

  public ExpressionCompletionProcessor(IVariables variables, CompletableFuture<IRowMeta> rowMeta) {
    this.variables = variables;
    this.rowMeta = rowMeta;
  }

  @Override
  public ICompletionProposal[] computeCompletionProposals(ITextViewer textViewer, int offset) {

    try {
      IDocument document = textViewer.getDocument();

      // Find significant characters entered by the user to restrict the number of
      // proposals
      int start = offset;
      char ch = 0;
      while (start > 0) {
        ch = document.getChar(start - 1);
        if (Character.isJavaIdentifierPart(ch) || ch == '.')
          start--;
        else
          break;
      }

      boolean quoted = false;
      if (ch == '\"') {
        quoted = true;
        start--;
      }
      if (ch == '{') {
        start--;
        if (start > 0)
          ch = document.getChar(start - 1);
      }
      if (ch == '$') {
        start--;
      }


      int end = offset;
      while (end < document.getLength()) {
        ch = document.getChar(end);

        if (Character.isJavaIdentifierPart(ch) || ch == '.') {
          end++;
          continue;
        }

        if (!quoted)
          break;

        if (ch == '\"') {
          end++;
          // If use another double quote to escape it
          if (end < document.getLength() && document.getChar(end) != '\"')
            break;
        }

        // Reset to not erase to end of the expression
        if (ch == '\r' || ch == '\n') {
          end = offset;
          break;
        }

        end++;
      }

      if (ch == '}')
        end++;

      String prefix = document.get(start, offset - start);

      List<ICompletionProposal> proposals = new LinkedList<>();
      if (prefix.length() > 0) {
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
      return proposals.toArray(new ICompletionProposal[0]);
    } catch (Exception e) {
      message = e.getMessage();
    }
    return null;
  }


  protected void computeVariableProposals(List<ICompletionProposal> proposals, String prefix,
      int start, int end) {
    for (String variableName : variables.getVariableNames()) {

      // Add to proposal if variable name start with the qualifier
      if (variableName.length() >= prefix.length()
          && variableName.substring(0, prefix.length()).equalsIgnoreCase(prefix)) {
        boolean isDeprecated = Arrays.asList(Const.DEPRECATED_VARIABLES).contains(variableName);

        String content = "${" + variableName + '}';
        ContextInformation contextInfo =
            new ContextInformation("Context display test", "information display test");
        String additionalProposalInfo = (isDeprecated) ? Const.getDeprecatedPrefix() : null;
        Image image = GuiResource.getInstance().getImageVariable();

        String additionalInfo = null;
        DescribedVariable variable =
            HopVariablesList.getInstance().findEnvironmentVariable(variableName);
        if (variable != null) {
          additionalInfo = variable.getDescription();
        }

        CompletionProposal proposal = new CompletionProposal(content, start, end - start,
            content.length(), image, variableName, contextInfo, additionalInfo);

        proposals.add(proposal);
      }
    }
  }

  protected void computeReservedWordProposals(List<ICompletionProposal> proposals, String prefix,
      int start, int end) {
    // Keywords
    for (String word : ExpressionScanner.getReservedWords()) {
      if (word.length() >= prefix.length()
          && word.substring(0, prefix.length()).equalsIgnoreCase(prefix)) {
        CompletionProposal proposal =
            new CompletionProposal(word, start, end - start, word.length(), null, word, null, null);
        proposals.add(proposal);
      }
    }

    // Functions
    Image image = GuiResource.getInstance().getImageFunction();
    for (String name : OperatorRegistry.getFunctionNames()) {
      if (name.length() >= prefix.length()
          && name.substring(0, prefix.length()).equalsIgnoreCase(prefix)) {

        Function function = OperatorRegistry.getFunction(name);
        CompletionProposal proposal = new CompletionProposal(name, start, end - start,
            name.length(), image, name, null, function.getDescription());
        proposals.add(proposal);
      }
    }
  }

  protected void computeIdentifierProposals(List<ICompletionProposal> proposals, String prefix,
      int start, int end, boolean quoted) {
    // Value meta

    try {
      if (prefix.charAt(0) == '\"')
        prefix = prefix.substring(1);
      for (IValueMeta valueMeta : rowMeta.get().getValueMetaList()) {
        String name = valueMeta.getName();
        if (name.length() >= prefix.length()
            && name.substring(0, prefix.length()).equalsIgnoreCase(prefix)) {

          String content = name;
          // If identifier name contains space, is a reserved word or a function name must be
          // quoted
          if (quoted || name.indexOf(' ') >= 0 || ExpressionScanner.isReservedWord(name)
              || Type.exist(name) || DatePart.exist(name)
              || OperatorRegistry.isFunctionName(name)) {
            content = '\"' + name + '\"';
          }

          Image image = GuiResource.getInstance().getImage(valueMeta);
          StringBuilder description = new StringBuilder();
          description.append("Type: ");
          description.append(valueMeta.getTypeDesc());
          description.append("<br>Step origin: ");
          description.append(valueMeta.getOrigin());
          description.append("<br>Comment: ");
          description.append(StringUtils.defaultString(valueMeta.getComments()));

          CompletionProposal proposal = new CompletionProposal(content, start, end - start,
              content.length(), image, name, null, description.toString());
          proposals.add(proposal);
        }
      }
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
    } catch (ExecutionException e) {
      // Ignore
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

}