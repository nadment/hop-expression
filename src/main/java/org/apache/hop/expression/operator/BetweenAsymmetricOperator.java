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

import static java.util.Objects.requireNonNull;

import java.io.StringWriter;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** <code>BETWEEN ASYMMETRIC</code> operator. */
@NullMarked
public class BetweenAsymmetricOperator extends Operator {
  public static final BetweenAsymmetricOperator INSTANCE = new BetweenAsymmetricOperator();

  public BetweenAsymmetricOperator() {
    super(
        "BETWEEN",
        80,
        Associativity.NONE,
        ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.BETWEEN,
        OperatorCategory.COMPARISON,
        "/docs/hop-user-manual/expression/between.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression value = call.getOperand(0);
    IExpression start = call.getOperand(1);
    IExpression end = call.getOperand(2);

    // x BETWEEN a AND a → x=a
    if (isSameExpression(start, end)) {
      return new Call(EqualOperator.INSTANCE, value, start);
    }

    // x BETWEEN 5 AND 3 → NVL2(x,FALSE,NULL), no value is in the range, but NULL if x is NULL
    if (isEmptyRange(value.getType(), start, end)) {
      if (!value.getType().isNullable()) {
        return Literal.FALSE;
      }
      return new Call(Nvl2Function.INSTANCE, value, Literal.FALSE, Literal.NULL_BOOLEAN);
    }

    return call;
  }

  /**
   * Returns whether the start of a range is a constant greater than its end, only for constants of
   * the type of the value, before a coercion 5 is not ordered like '5'.
   */
  private static boolean isEmptyRange(Type type, IExpression start, IExpression end) {
    if (!(start instanceof Literal startLiteral && end instanceof Literal endLiteral)
        || start.isNull()
        || end.isNull()
        || !startLiteral.getType().isName(type.getName())
        || !endLiteral.getType().isName(type.getName())
        || !(Types.isNumeric(type) || Types.isString(type) || Types.isTemporal(type))) {
      return false;
    }
    Object startValue = requireNonNull(startLiteral.getValue());
    Object endValue = requireNonNull(endLiteral.getValue());
    return type.compare(startValue, endValue) > 0;
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coercionComparisonOperator(call);
  }

  @Override
  public @Nullable Object eval(IExpression[] operands) {
    Object value = operands[0].getValue();
    Object start = operands[1].getValue();
    Object end = operands[2].getValue();

    if (value == null) {
      return null;
    }

    // Equivalent to (value >= start AND value <= end), so returns FALSE if one comparison is
    // FALSE, even if the other bound is NULL
    Type type = operands[0].getType();
    if (start != null && type.compare(value, start) < 0) {
      return Boolean.FALSE;
    }
    if (end != null && type.compare(value, end) > 0) {
      return Boolean.FALSE;
    }
    if (start == null || end == null) {
      return null;
    }
    return Boolean.TRUE;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, 0, getLeftPrecedence());
    writer.append(" BETWEEN ");
    operands[1].unparse(writer, getRightPrecedence(), getLeftPrecedence());
    writer.append(" AND ");
    operands[2].unparse(writer, getRightPrecedence(), 0);
  }
}
