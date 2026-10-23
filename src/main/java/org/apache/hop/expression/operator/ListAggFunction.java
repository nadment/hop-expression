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
package org.apache.hop.expression.operator;

import java.io.StringWriter;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import org.apache.hop.expression.AggregateFunction;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IAggregateProcessor;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.IExpressionParser;
import org.apache.hop.expression.Token;
import org.apache.hop.expression.Token.Id;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

@FunctionPlugin
@NullMarked
public class ListAggFunction extends AggregateFunction {
  public static final ListAggFunction LISTAGG_ALL = new ListAggFunction(ListAgg.ALL);
  public static final ListAggFunction LISTAGG_DISTINCT = new ListAggFunction(ListAgg.DISTINCT);

  private final ListAgg option;

  /**
   * Default constructor to register function but not used. The different options are detected by
   * parser.
   */
  public ListAggFunction() {
    this(ListAgg.ALL);
  }

  public ListAggFunction(ListAgg option) {
    super(
        "LISTAGG",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING.or(OperandTypes.STRING_STRING),
        "/docs/listagg.html");
    this.option = option;
  }

  @Override
  public Set<String> getKeywords() {
    return Set.of("ALL", "DISTINCT");
  }

  /**
   * Parses <code>LISTAGG([ALL | DISTINCT] expression [, delimiter] )</code>, the call uses the
   * function of the option.
   */
  @Override
  public IExpression parse(IExpressionParser parser, Token token) throws ExpressionException {

    AggregateFunction aggregator = ListAggFunction.LISTAGG_ALL;

    if (parser.acceptKeyword("DISTINCT")) {
      aggregator = ListAggFunction.LISTAGG_DISTINCT;
    } else {
      parser.acceptKeyword("ALL");
    }

    List<IExpression> operands = parser.parseExpressionList();

    parser.expect(Id.RPARENTHESIS, ErrorCode.MISSING_RIGHT_PARENTHESIS);

    return new Call(token.start(), aggregator, operands);
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append(this.getName());
    writer.append('(');
    switch (option) {
      case ALL:
        writer.append("ALL ");
        operands[0].unparse(writer, 0, 0);
        break;
      case DISTINCT:
        writer.append("DISTINCT ");
        operands[0].unparse(writer, 0, 0);
        break;
    }
    if (operands.length == 2) {
      writer.append(",");
      operands[1].unparse(writer, 0, 0);
    }
    writer.append(')');
  }

  /** {@inheritDoc} The option is not an operand, it distinguishes the functions. */
  @Override
  public boolean equals(@Nullable Object other) {
    return other instanceof ListAggFunction function
        && option == function.option
        && super.equals(other);
  }

  @Override
  public int hashCode() {
    return Objects.hash(super.hashCode(), option);
  }

  @Override
  public IAggregateProcessor createProcessor(IExpressionContext context, IExpression[] operands)
      throws ExpressionException {

    String delimiter = ",";
    if (operands.length == 2) {
      delimiter = operands[1].getValue(String.class);
      if (delimiter == null) {
        delimiter = ",";
      }
    }

    if (option == ListAgg.DISTINCT) {
      return new ListAggDistinctProcessor(delimiter);
    }
    return new ListAggAllProcessor(delimiter);
  }

  public enum ListAgg {
    ALL,
    DISTINCT
  }
}
