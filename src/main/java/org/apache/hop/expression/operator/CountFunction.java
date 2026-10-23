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
import java.util.ArrayList;
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
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Token;
import org.apache.hop.expression.Token.Id;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

@FunctionPlugin
@NullMarked
public class CountFunction extends AggregateFunction {
  public static final CountFunction COUNT_ALL = new CountFunction(Count.ALL);
  public static final CountFunction COUNT_DISTINCT = new CountFunction(Count.DISTINCT);
  public static final CountFunction COUNT_ROW = new CountFunction(Count.ROW);
  private final Count count;

  /**
   * Default constructor to register function but not used. The parser detects the different count
   * modes.
   */
  public CountFunction() {
    this(Count.ROW);
  }

  public CountFunction(Count count) {
    super(
        "COUNT",
        ReturnTypes.INTEGER_NOT_NULL,
        OperandTypes.ANY,
        "/docs/hop-user-manual/expression/count.html");
    this.count = count;
  }

  @Override
  public Set<String> getKeywords() {
    return Set.of("ALL", "DISTINCT");
  }

  /**
   * Parses <code>COUNT(*) | COUNT([ALL | DISTINCT] expression)</code>, the call uses the function
   * of the count mode.
   */
  @Override
  public IExpression parse(IExpressionParser parser, Token token) throws ExpressionException {

    AggregateFunction aggregator = CountFunction.COUNT_ALL;
    List<IExpression> operands = new ArrayList<>();

    if (parser.accept(Id.STAR)) {
      aggregator = CountFunction.COUNT_ROW;
      // COUNT(*)  use fictive operand
      operands.add(Literal.NULL);
    } else if (parser.acceptKeyword("DISTINCT")) {
      operands = parser.parseExpressionList();
      aggregator = CountFunction.COUNT_DISTINCT;
    } else {
      parser.acceptKeyword("ALL");
      operands = parser.parseExpressionList();
    }

    parser.expect(Id.RPARENTHESIS, ErrorCode.MISSING_RIGHT_PARENTHESIS);

    return new Call(token.start(), aggregator, operands);
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append(this.getName());
    writer.append('(');
    switch (count) {
      case ALL:
        writer.append("ALL ");
        operands[0].unparse(writer, 0, 0);
        break;
      case DISTINCT:
        writer.append("DISTINCT ");
        operands[0].unparse(writer, 0, 0);
        break;
      case ROW:
        writer.append('*');
        break;
    }
    writer.append(')');
  }

  /** {@inheritDoc} The count mode is not an operand, it distinguishes the functions. */
  @Override
  public boolean equals(@Nullable Object other) {
    return other instanceof CountFunction function
        && count == function.count
        && super.equals(other);
  }

  @Override
  public int hashCode() {
    return Objects.hash(super.hashCode(), count);
  }

  @Override
  public IAggregateProcessor createProcessor(IExpressionContext context, IExpression[] operands) {
    return switch (count) {
      case ALL -> new CountAllProcessor();
      case DISTINCT -> new CountDistinctProcessor();
      case ROW -> new CountRowProcessor();
    };
  }

  public enum Count {
    ALL,
    DISTINCT,
    ROW
  }
}
