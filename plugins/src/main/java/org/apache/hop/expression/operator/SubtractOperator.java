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
import java.math.BigDecimal;
import java.time.ZonedDateTime;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.type.Interval;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.TypeFamily;
import org.apache.hop.expression.type.TypeId;
import org.apache.hop.expression.type.Types;

/**
 * Generic subtraction operator. <br>
 * <strong>Syntax:</strong> <code>x - y</code>
 */
public class SubtractOperator extends Operator {
  private static final SubtractOperator IntegerSubtractOperator = new IntegerSubtractOperator();
  private static final SubtractOperator NumberSubtractOperator = new NumberSubtractOperator();
  private static final SubtractOperator IntervalSubtractOperator = new IntervalSubtractOperator();

  public SubtractOperator() {
    super(
        "SUBTRACT",
        "-",
        100,
        true,
        ReturnTypes.ADDITIVE_OPERATOR,
        OperandTypes.NUMERIC_NUMERIC
            .or(OperandTypes.TEMPORAL_INTERVAL)
            .or(OperandTypes.TEMPORAL_NUMERIC)
            .or(OperandTypes.INTERVAL_INTERVAL),
        OperatorCategory.MATHEMATICAL,
        "/docs/subtract.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    if (left.getType().isFamily(TypeFamily.TEMPORAL)) {
      // Supports the basic subtraction of days to DATE values
      if (right.getType().isFamily(TypeFamily.NUMERIC)) {
        return new Call(
            AddDaysFunction.INSTANCE, left, new Call(call.getPosition(), Operators.NEGATE, right));
      }
      return new Call(IntervalSubtractOperator, call.getOperands());
    }

    // Simplify arithmetic A-0 → A
    if (Literal.ZERO.equals(right)) {
      return left;
    }

    // Simplify arithmetic 0-A → -A
    if (Literal.ZERO.equals(left)) {
      return new Call(Operators.NEGATE, right);
    }

    // Simplify arithmetic A-(-B) → A+B
    if (right.is(Operators.NEGATE)) {
      return new Call(Operators.ADD, left, right.asCall().getOperand(0));
    }

    // Optimize data type
    if (call.getType().is(TypeId.INTEGER)) {
      return new Call(IntegerSubtractOperator, call.getOperands());
    }

    return new Call(NumberSubtractOperator, call.getOperands());
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append('-');
    operands[1].unparse(writer);
  }

  private static final class IntegerSubtractOperator extends SubtractOperator {
    @Override
    public Object eval(final IExpression[] operands) {
      Long left = operands[0].getValue(Long.class);
      if (left == null) return null;
      Long right = operands[1].getValue(Long.class);
      if (right == null) return null;

      return left - right;
    }
  }

  private static final class NumberSubtractOperator extends SubtractOperator {
    @Override
    public Object eval(final IExpression[] operands) {
      BigDecimal left = operands[0].getValue(BigDecimal.class);
      if (left == null) return null;
      BigDecimal right = operands[1].getValue(BigDecimal.class);
      if (right == null) return null;

      return left.subtract(right);
    }

    @Override
    public boolean coerceOperandsType(Call call) {
      return Types.coercionArithmeticOperator(call);
    }
  }

  /** Subtracts a specified interval to a date or timestamp */
  public static final class IntervalSubtractOperator extends SubtractOperator {

    @Override
    public Object eval(final IExpression[] operands) {
      ZonedDateTime datetime = operands[0].getValue(ZonedDateTime.class);
      if (datetime == null) return null;

      Interval interval = operands[1].getValue(Interval.class);
      if (interval == null) return null;

      return interval.subtractFrom(datetime);
    }

    @Override
    public boolean coerceOperandsType(Call call) {
      return Types.coercionArithmeticOperator(call);
    }
  }
}
