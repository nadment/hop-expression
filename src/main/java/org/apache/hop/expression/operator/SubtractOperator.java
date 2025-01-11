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

import java.math.BigDecimal;
import java.time.ZonedDateTime;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.TypeFamily;
import org.apache.hop.expression.type.Types;

/**
 * Generic subtraction operator.<br>
 * <strong>Syntax:</strong> <code>x - y</code>
 */
public class SubtractOperator extends BinaryOperator {
  public SubtractOperator() {
    super(
        "SUBTRACT",
        "-",
        100,
        true,
        ReturnTypes.ADDITIVE_OPERATOR,
        OperandTypes.NUMERIC_NUMERIC.or(OperandTypes.DATE_INTERVAL).or(OperandTypes.DATE_INTEGER),
        // .or(OperandTypes.INTERVAL_INTERVAL),
        OperatorCategory.MATHEMATICAL,
        "/docs/subtract.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    if (Types.isDate(left.getType())) {
      // Supports the basic subtraction of days to DATE values
      if (right.getType().isFamily(TypeFamily.NUMERIC)) {
        return new Call(
            AddDaysFunction.INSTANCE, left, new Call(call.getPosition(), Operators.NEGATE, right));
      }
      return new Call(IntervalFromTemporalSubtractOperator.INSTANCE, call.getOperands());
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
    if (right.isOperator(Operators.NEGATE)) {
      return new Call(Operators.ADD, left, call(right).getOperand(0));
    }

    // Optimize data type
    if (Types.isInteger(call.getType())) {
      return new Call(IntegerSubtractOperator.INSTANCE, call.getOperands());
    }

    return new Call(NumberSubtractOperator.INSTANCE, call.getOperands());
  }

  private static final class IntegerSubtractOperator extends SubtractOperator {
    private static final IntegerSubtractOperator INSTANCE = new IntegerSubtractOperator();

    @Override
    public boolean coerceOperandsType(Call call) {
      return Types.coercionArithmeticOperator(call);
    }

    @Override
    public Object eval(final IExpression[] operands) {
      Long left = operands[0].getValue(Long.class);
      if (left == null) return null;
      Long right = operands[1].getValue(Long.class);
      if (right == null) return null;

      try {
        return Math.subtractExact(left, right);
      } catch (ArithmeticException e) {
        throw new ExpressionException(ErrorCode.ARITHMETIC_OVERFLOW, getName());
      }
    }
  }

  private static final class NumberSubtractOperator extends SubtractOperator {
    private static final NumberSubtractOperator INSTANCE = new NumberSubtractOperator();

    @Override
    public boolean coerceOperandsType(Call call) {
      return Types.coercionArithmeticOperator(call);
    }

    @Override
    public Object eval(final IExpression[] operands) {
      BigDecimal left = operands[0].getValue(BigDecimal.class);
      if (left == null) return null;
      BigDecimal right = operands[1].getValue(BigDecimal.class);
      if (right == null) return null;

      return left.subtract(right);
    }
  }

  /** Subtracts a specified interval to a date or timestamp */
  public static final class IntervalFromTemporalSubtractOperator extends SubtractOperator {
    private static final IntervalFromTemporalSubtractOperator INSTANCE =
        new IntervalFromTemporalSubtractOperator();

    @Override
    public boolean coerceOperandsType(Call call) {
      return Types.coercionArithmeticOperator(call);
    }

    @Override
    public Object eval(final IExpression[] operands) {
      ZonedDateTime datetime = operands[0].getValue(ZonedDateTime.class);
      if (datetime == null) return null;

      Interval interval = operands[1].getValue(Interval.class);
      if (interval == null) return null;

      return interval.subtractFrom(datetime);
    }
  }

  public static final class IntervalFromIntervalSubtractOperator extends SubtractOperator {
    private static final IntervalFromIntervalSubtractOperator INSTANCE =
        new IntervalFromIntervalSubtractOperator();

    @Override
    public Object eval(final IExpression[] operands) {
      Interval interval0 = operands[0].getValue(Interval.class);
      if (interval0 == null) return null;

      Interval interval1 = operands[1].getValue(Interval.class);
      if (interval1 == null) return null;

      // return interval0.subtract(interval1);
      throw new ExpressionException(ErrorCode.INTERNAL_ERROR);
    }

    @Override
    public boolean coerceOperandsType(Call call) {
      return Types.coercionArithmeticOperator(call);
    }
  }
}
