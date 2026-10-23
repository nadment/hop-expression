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
package org.apache.hop.expression;

import java.util.List;
import java.util.Set;
import org.apache.hop.expression.Token.Id;
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.IReturnTypeInference;
import org.jspecify.annotations.NullMarked;

@NullMarked
public abstract class AggregateFunction extends Function {

  protected AggregateFunction(
      String id,
      IReturnTypeInference returnTypeInference,
      IOperandTypeChecker operandTypeChecker,
      String documentationUrl) {
    super(
        id,
        returnTypeInference,
        operandTypeChecker,
        OperatorCategory.AGGREGATION,
        documentationUrl);
  }

  @Override
  public boolean isAggregate() {
    return true;
  }

  public abstract IAggregateProcessor createProcessor(
      IExpressionContext context, IExpression[] operands) throws ExpressionException;

  /** The reserved words of the NULL treatment clause, see {@link #parseWithNullTreatment}. */
  protected static final Set<String> NULL_TREATMENT_KEYWORDS = Set.of("IGNORE", "RESPECT", "NULLS");

  /**
   * Parses aggregate function with NULL treatment clause <code>
   * NAME(expression) [ IGNORE NULLS | RESPECT NULLS ]</code>
   *
   * @param parser the parser to read the arguments
   * @param token the token of the function name
   * @param ignoreNulls the function of the call with IGNORE NULLS
   * @param respectNulls the function of the call with RESPECT NULLS or without clause
   * @return the call of the function
   */
  protected static IExpression parseWithNullTreatment(
      IExpressionParser parser, Token token, Function ignoreNulls, Function respectNulls)
      throws ExpressionException {

    List<IExpression> operands = parser.parseExpressionList();

    parser.expect(Id.RPARENTHESIS, ErrorCode.MISSING_RIGHT_PARENTHESIS);

    // NULL treatment clause
    Function function =
        parseIgnoreOrRespectNulls(parser, respectNulls) ? ignoreNulls : respectNulls;

    return new Call(token.start(), function, operands);
  }

  /**
   * Parse NULL treatment clause <code>[ IGNORE NULLS | RESPECT NULLS ]</code>
   *
   * @return true if the function should ignore null values
   */
  private static boolean parseIgnoreOrRespectNulls(IExpressionParser parser, Function function)
      throws ExpressionException {
    if (parser.acceptKeyword("IGNORE")) {
      if (parser.acceptKeyword("NULLS")) {
        return true;
      }
      throw new ParseExpressionException(
          parser.getPosition(), ErrorCode.SYNTAX_ERROR_FUNCTION, function);
    }
    if (parser.acceptKeyword("RESPECT")) {
      if (parser.acceptKeyword("NULLS")) {
        return false;
      }
      throw new ParseExpressionException(
          parser.getPosition(), ErrorCode.SYNTAX_ERROR_FUNCTION, function);
    }

    return false;
  }
}
