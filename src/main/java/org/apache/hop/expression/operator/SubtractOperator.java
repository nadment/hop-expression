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
import java.time.DateTimeException;
import java.time.temporal.Temporal;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.TypeFamily;
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Generic subtraction operator.<br>
 * <strong>Syntax:</strong> <code>x - y</code>
 */
@NullMarked
public class SubtractOperator extends BinaryOperator {

  public static final SubtractOperator INSTANCE = new SubtractOperator();

  public SubtractOperator() {
    super(
        "SUBTRACT",
        "-",
        70,
        Associativity.LEFT,
        ReturnTypes.ADDITIVE_OPERATOR,
        OperandTypes.NUMERIC_NUMERIC
            .or(OperandTypes.DATE_INTERVAL)
            .or(OperandTypes.DATE_INTEGER)
            .or(OperandTypes.INTERVAL_INTERVAL),
        OperatorCategory.MATHEMATICAL,
        "/docs/hop-user-manual/expression/subtract.html");
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, 0, getLeftPrecedence());
    writer.append('-');
    unparseAfterSign(writer, operands[1], getRightPrecedence());
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    if (Types.isTemporal(left.getType())) {
      // Supports the basic subtraction of days to DATE values
      if (right.getType().isFamily(TypeFamily.NUMERIC)) {
        return new Call(
            AddDaysFunction.INSTANCE,
            left,
            new Call(call.getPosition(), NegativeOperator.INSTANCE, right));
      }
      return new Call(IntervalFromTemporalSubtractOperator.INSTANCE, call.getOperands());
    }

    if (Types.isInterval(left.getType())) {
      return new Call(
          call.getPosition(), IntervalFromIntervalSubtractOperator.INSTANCE, call.getOperands());
    }

    // Simplify arithmetic A-0 → A
    if (Literal.ZERO.equals(right)) {
      return left;
    }

    // Simplify arithmetic 0-A → -A
    if (Literal.ZERO.equals(left)) {
      return new Call(NegativeOperator.INSTANCE, right);
    }

    // Simplify arithmetic A-(-B) → A+B
    if (right.isOperator(NegativeOperator.INSTANCE)) {
      return new Call(AddOperator.INSTANCE, left, call(right).getOperand(0));
    }

    // Simplify arithmetic A-A → 0 (if A is not nullable), an integer or a number like A
    if (left.isSameExpression(right)
        && !left.getType().isNullable()
        && Types.isNumeric(left.getType())) {
      return Types.isInteger(left.getType()) ? Literal.ZERO : Literal.of(BigDecimal.ZERO);
    }

    // Optimize data type, derived from the compiled operands: the type of the call was inferred
    // before its operands were simplified, like CASE ... THEN 1%NULL ELSE 3 END to the integer 3
    if (Types.isInteger(ReturnTypes.deriveAdditiveType(left.getType(), right.getType()))) {
      return new Call(IntegerSubtractOperator.INSTANCE, call.getOperands());
    }

    return new Call(NumberSubtractOperator.INSTANCE, call.getOperands());
  }

  public static final class IntegerSubtractOperator extends SubtractOperator {
    public static final IntegerSubtractOperator INSTANCE = new IntegerSubtractOperator();

    @Override
    public boolean coerceOperandsType(Call call) {
      return Types.coercionArithmeticOperator(call);
    }

    @Override
    public @Nullable Object eval(final IExpression[] operands) {
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

  public static final class NumberSubtractOperator extends SubtractOperator {
    public static final NumberSubtractOperator INSTANCE = new NumberSubtractOperator();

    @Override
    public boolean coerceOperandsType(Call call) {
      return Types.coercionArithmeticOperator(call);
    }

    @Override
    public @Nullable Object eval(final IExpression[] operands) {
      BigDecimal left = operands[0].getValue(BigDecimal.class);
      if (left == null) return null;
      BigDecimal right = operands[1].getValue(BigDecimal.class);
      if (right == null) return null;

      return checkNumberOverflow(left.subtract(right));
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
    public @Nullable Object eval(final IExpression[] operands) {
      Temporal temporal = operands[0].getValue(Temporal.class);
      if (temporal == null) return null;

      Interval interval = operands[1].getValue(Interval.class);
      if (interval == null) return null;

      try {
        return interval.subtractFrom(temporal);
      } catch (DateTimeException | ArithmeticException e) {
        throw new ExpressionException(ErrorCode.TEMPORAL_OVERFLOW, getName());
      }
    }
  }

  /** Subtracts an interval from an interval */
  public static final class IntervalFromIntervalSubtractOperator extends SubtractOperator {
    private static final IntervalFromIntervalSubtractOperator INSTANCE =
        new IntervalFromIntervalSubtractOperator();

    @Override
    public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

      // Simplify arithmetic INTERVAL A-INTERVAL 0 → INTERVAL A
      IExpression operand = call.getOperand(1);
      if (operand.isConstant() && !operand.isNull()) {
        Interval interval = operand.getValue(Interval.class);
        if (interval != null && interval.isZero()) {
          return call.getOperand(0);
        }
      }

      return call;
    }

    @Override
    public @Nullable Object eval(final IExpression[] operands) {
      Interval interval0 = operands[0].getValue(Interval.class);
      if (interval0 == null) return null;

      Interval interval1 = operands[1].getValue(Interval.class);
      if (interval1 == null) return null;

      try {
        return interval0.subtract(interval1);
      } catch (ArithmeticException e) {
        throw new ExpressionException(ErrorCode.INTERVAL_OVERFLOW, getName());
      }
    }
  }
}
