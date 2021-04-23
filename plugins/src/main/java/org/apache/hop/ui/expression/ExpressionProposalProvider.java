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

import org.apache.commons.lang.StringUtils;
import org.apache.hop.core.Const;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.ExpressionRegistry;
import org.apache.hop.expression.util.Characters;
import org.apache.hop.ui.expression.ExpressionProposal.Type;
import org.eclipse.jface.fieldassist.IContentProposal;
import org.eclipse.jface.fieldassist.IContentProposalProvider;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ExpressionProposalProvider implements IContentProposalProvider {

  private IVariables variables;

  private IRowMeta rowMeta;

  public ExpressionProposalProvider() {
    super();
  }

  public void setRowMeta(final IRowMeta rowMeta) {
    this.rowMeta = rowMeta;
  }

  public void setVariables(final IVariables variables) {
    this.variables = variables;
  }

  @Override
  public IContentProposal[] getProposals(String contents, int position) {

    // System.out.println(position + ":>" + contents);

    // Find significant characters entered by the user to restrict the number of
    // proposals
    String qualifier = contents.substring(0, position);

    int start = position;
    while (start > 0) {
      char ch = contents.charAt(start - 1);
      if (!Character.isAlphabetic(ch) && ch != '.')
        break;
      start--;
    }

    if (start > 0 && contents.charAt(start - 1) == '{')
      start--;
    if (start > 0 && contents.charAt(start - 1) == '$')
      start--;

    qualifier = qualifier.substring(start, position);

    int end = position;
    while (end < contents.length() && !Character.isWhitespace(contents.charAt(end))
        && contents.charAt(end) != '}')
      end++;

    ArrayList<IContentProposal> list = new ArrayList<>();

    if (qualifier.length() > 0 && qualifier.charAt(0) == '$')
      this.buildVariableProposals(list, qualifier, position - start);
    // else if (start > 2 && contents.charAt(start - 1) == '{')
    // this.buildVariableProposals(list, qualifier, position - start - 1);

    else {
      this.buildFieldProposals(list, qualifier, position - start);
      this.buildOperatorProposals(list, qualifier, position - start);
    }

    // System.out.println('\r');
    return list.toArray(new IContentProposal[list.size()]);
  }

  protected void buildVariableProposals(List<IContentProposal> list, String qualifier,
      int position) {

    // System.out.println("list variables [" + qualifier + "] " + position);

    if (variables != null) {

      String name = qualifier;
      if (name.startsWith("${"))
        name = name.substring(2);
      else if (name.startsWith("$"))
        name = name.substring(1);

      for (String variable : variables.getVariableNames()) {
        // Add to proposal if variable name start with the qualifier

        if (variable.length() >= name.length()
            && variable.substring(0, name.length()).equalsIgnoreCase(name)) {
          boolean isDeprecated = Arrays.asList(Const.DEPRECATED_VARIABLES).contains(variable);

          String content = "${" + variable + '}';
          String description = (isDeprecated) ? Const.getDeprecatedPrefix() : null;

          list.add(new ExpressionProposal(Type.VARIABLE, content.substring(position), variable,
              description));
        }
      }
    }
  }

  protected void buildOperatorProposals(List<IContentProposal> list, String qualifier,
      int position) {

    // System.out.println("list operators [" + qualifier + "] " + position);

    for (Operator operator : ExpressionRegistry.getInstance().getOperators()) {

      String name = (operator.getAlias() == null) ? operator.getName() : operator.getAlias();

      // Only function name or alphabetic operator
      if (Characters.isAlpha(name.charAt(0))) {

        // elements.add(new ExpressionProposal(Type.Function, name, name,
        // operator.getDescription()));

        // Add to proposal if name start with the qualifier
        if (name.length() >= qualifier.length()
            && name.substring(0, qualifier.length()).equalsIgnoreCase(qualifier)) {

          list.add(new ExpressionProposal(Type.FUNCTION, name.substring(position), name, operator));
        }
      }
    }
  }

  protected void buildFieldProposals(List<IContentProposal> list, String qualifier, int position) {

    // System.out.println("list field [" + qualifier + "] " + position)

    if (rowMeta != null) {
      for (int i = 0; i < rowMeta.size(); i++) {
        IValueMeta valueMeta = rowMeta.getValueMeta(i);

        String name = valueMeta.getName();

        // Add to proposal if field name start with the qualifier
        if (name.length() >= qualifier.length()
            && name.substring(0, qualifier.length()).equalsIgnoreCase(qualifier)) {

          StringBuilder description = new StringBuilder();
          description.append("Type: ");
          description.append(valueMeta.getTypeDesc());
          description.append("\nStep origin: ");
          description.append(valueMeta.getOrigin());
          description.append("\nComment: ");
          description.append(StringUtils.defaultString(valueMeta.getComments()));

          list.add(new ExpressionProposal(Type.FIELD, name.substring(position), name, valueMeta));
        }
      }
    }
  }
}
