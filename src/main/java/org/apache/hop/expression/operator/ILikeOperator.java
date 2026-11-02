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
import org.apache.commons.lang3.StringUtils;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.LikeMatcher;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** The ILIKE case-insensitive operator. */
@NullMarked
public class ILikeOperator extends Operator {
  public static final ILikeOperator INSTANCE = new ILikeOperator();

  /**
   * The matcher of the pattern, compiled once at compilation.
   *
   * <p><b>NOTE</b> It is only used when the pattern (and the escape character) is defined by a
   * constant, otherwise it is {@code null} and the pattern is compiled at each evaluation.
   */
  private final @Nullable LikeMatcher matcher;

  public ILikeOperator() {
    this(null);
  }

  private ILikeOperator(@Nullable LikeMatcher matcher) {
    super(
        "ILIKE",
        80,
        Associativity.NONE,
        ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.EXACT_STRING_EXACT_STRING.or(
            OperandTypes.EXACT_STRING_EXACT_STRING_EXACT_STRING),
        OperatorCategory.COMPARISON,
        "/docs/hop-user-manual/expression/ilike.html");
    this.matcher = matcher;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    // Already compiled
    if (matcher != null) return call;

    // Optimize NULL ILIKE FIELD → NULL
    IExpression value = call.getOperand(0);
    if (value.isNull()) return value;

    if (call.getOperand(1).isConstant()) {
      String pattern = call.getOperand(1).getValue(String.class);

      // Simplify x ILIKE NULL → NULL
      if (pattern == null) return Literal.NULL_BOOLEAN;

      char escape = '\\';
      if (call.getOperandCount() == 3) {
        String escapeString = call.getOperand(2).getValue(String.class);
        if (StringUtils.isNotEmpty(escapeString)) {
          escape = escapeString.charAt(0);
        }
      }

      // Multiple consecutive '%' in the string matched by ILIKE should simplify to a single '%'
      String simplifiedPattern = LikeMatcher.simplify(pattern, escape);
      if (!simplifiedPattern.equals(pattern)) {
        // Keep the ESCAPE clause, the simplified pattern still depends on it
        if (call.getOperandCount() == 3) {
          return new Call(INSTANCE, value, Literal.of(simplifiedPattern), call.getOperand(2));
        }
        return new Call(INSTANCE, value, Literal.of(simplifiedPattern));
      }
    }

    LikeMatcher constantMatcher = LikeOperator.compileConstantMatcher(call, true);
    if (constantMatcher != null) {
      return new Call(new ILikeOperator(constantMatcher), call.getOperands());
    }

    return call;
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null) {
      return null;
    }

    if (matcher != null) {
      return matcher.matches(value);
    }

    String pattern = operands[1].getValue(String.class);
    if (pattern == null) {
      return null;
    }

    String escape = LikeOperator.DEFAULT_ESCAPE;
    if (operands.length == 3) {
      escape = operands[2].getValue(String.class);
      if (escape == null) {
        return null;
      }
    }

    return LikeMatcher.compile(pattern, escape, true).matches(value);
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, 0, getLeftPrecedence());
    writer.append(" ILIKE ");
    operands[1].unparse(writer, getRightPrecedence(), getLeftPrecedence());
    if (operands.length == 3) {
      writer.append(" ESCAPE ");
      operands[2].unparse(writer, getRightPrecedence(), 0);
    }
  }
}
