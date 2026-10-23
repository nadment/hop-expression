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
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Comparison NOT SIMILAR TO operator. <br>
 * <strong>Syntax:</strong> <code>'abc' NOT SIMILAR TO 'regexp'</code>
 */
@NullMarked
public class NotSimilarToOperator extends BinaryOperator {
  public static final NotSimilarToOperator INSTANCE = new NotSimilarToOperator();

  /** The pattern compiled ahead of evaluation when it is constant, otherwise {@code null}. */
  private final @Nullable Pattern compiledPattern;

  public NotSimilarToOperator() {
    this(null);
  }

  private NotSimilarToOperator(@Nullable Pattern compiledPattern) {
    super(
        "NOT SIMILAR TO",
        "NOT SIMILAR TO",
        10,
        Associativity.LEFT,
        ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.EXACT_STRING_EXACT_STRING,
        OperatorCategory.COMPARISON,
        "/docs/hop-user-manual/expression/similar-to.html");
    this.compiledPattern = compiledPattern;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    if (compiledPattern == null) {
      Pattern pattern = SimilarToOperator.createConstantPattern(call);
      if (pattern != null) {
        return new Call(new NotSimilarToOperator(pattern), call.getOperands());
      }
    }
    return call;
  }

  @Override
  public Operator not() {
    return SimilarToOperator.INSTANCE;
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null) {
      return null;
    }

    if (compiledPattern != null) {
      return !compiledPattern.matcher(value).matches();
    }

    String pattern = operands[1].getValue(String.class);
    if (pattern == null) {
      return null;
    }

    return !SimilarToOperator.createPattern(pattern).matcher(value).matches();
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, getLeftPrec(), getRightPrec());
    writer.append(" NOT SIMILAR TO ");
    operands[1].unparse(writer, getLeftPrec(), getRightPrec());
  }
}
