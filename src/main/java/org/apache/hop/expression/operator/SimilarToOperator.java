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
package org.apache.hop.expression.operator;

import java.io.StringWriter;
import java.util.regex.Pattern;
import org.apache.hop.core.util.Utils;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Regexp;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Comparison SIMILAR TO operator. <br>
 * <strong>Syntax:</strong> <code>'abc' SIMILAR TO 'regexp'</code>
 */
@NullMarked
public class SimilarToOperator extends BinaryOperator {

  public static final SimilarToOperator INSTANCE = new SimilarToOperator();

  /** The pattern compiled ahead of evaluation when it is constant, otherwise {@code null}. */
  private final @Nullable Pattern compiledPattern;

  public SimilarToOperator() {
    this(null);
  }

  private SimilarToOperator(@Nullable Pattern compiledPattern) {
    super(
        "SIMILAR TO",
        "SIMILAR TO",
        80,
        Associativity.LEFT,
        ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.EXACT_STRING_EXACT_STRING,
        OperatorCategory.COMPARISON,
        "/docs/similar-to.html");
    this.compiledPattern = compiledPattern;
  }

  /**
   * Compiles a SIMILAR TO pattern.
   *
   * @param pattern the SIMILAR TO pattern
   * @return the compiled pattern
   */
  static Pattern createPattern(String pattern) {
    return Pattern.compile(Regexp.toSimilarTo(pattern, '\\'), Pattern.UNICODE_CASE);
  }

  /**
   * Compiles the pattern of a call ahead of evaluation, when it is constant.
   *
   * <p>An invalid pattern is not reported here, the error is raised at evaluation.
   *
   * @param call the call of a SIMILAR TO operator
   * @return the compiled pattern, or {@code null} if it must be compiled at evaluation
   */
  static @Nullable Pattern createConstantPattern(Call call) {
    IExpression operand = call.getOperand(1);
    if (!operand.isConstant()) {
      return null;
    }
    try {
      String pattern = operand.getValue(String.class);
      // A null or empty pattern is handled at evaluation
      if (Utils.isEmpty(pattern)) {
        return null;
      }
      return createPattern(pattern);
    } catch (RuntimeException e) {
      return null;
    }
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    if (compiledPattern == null) {
      Pattern pattern = createConstantPattern(call);
      if (pattern != null) {
        return new Call(new SimilarToOperator(pattern), call.getOperands());
      }
    }
    return call;
  }

  @Override
  public Operator not() {
    return NotSimilarToOperator.INSTANCE;
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null) {
      return null;
    }

    if (compiledPattern != null) {
      return compiledPattern.matcher(value).matches();
    }

    String pattern = operands[1].getValue(String.class);
    if (pattern == null) {
      return null;
    }

    if (Utils.isEmpty(pattern)) {
      return false;
    }

    return createPattern(pattern).matcher(value).matches();
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, getLeftPrec(), getRightPrec());
    writer.append(" SIMILAR TO ");
    operands[1].unparse(writer, getLeftPrec(), getRightPrec());
  }
}
