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
import java.util.PriorityQueue;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionComparator;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Types;

/**
 * Generic addition operator. <br>
 * <strong>Syntax:</strong> <code>x + y</code>
 */
public class AddOperator extends BinaryOperator {

  public AddOperator() {
    super(
        "ADD",
        "+",
        100,
        true,
        ReturnTypes.ADDITIVE_OPERATOR,
        OperandTypes.NUMERIC_NUMERIC
            .or(OperandTypes.DATE_INTERVAL)
            .or(OperandTypes.DATE_INTEGER)
            .or(OperandTypes.INTEGER_DATE)
            .or(OperandTypes.INTERVAL_DATE)
            .or(OperandTypes.INTERVAL_INTERVAL),
        OperatorCategory.MATHEMATICAL,
        "/docs/add.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Rebuild chained operator
    PriorityQueue<IExpression> operands = new PriorityQueue<>(new ExpressionComparator());
    operands.addAll(call.getChainedOperands(true));
    IExpression operand = operands.poll();
    while (!operands.isEmpty()) {
      call = new Call(this, operand, operands.poll());
      call.inferReturnType();
      operand = call;
    }

    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    if (Types.isDate(left.getType())) {
      // Supports the basic addition and subtraction of days to DATE values, in the form of { + |
      // - } <integer>
      if (Types.isNumeric(right.getType())) {
        return new Call(call.getPosition(), AddDaysFunction.INSTANCE, call.getOperands());
      }

      return new Call(
          call.getPosition(), IntervalToTemporalAddOperator.INSTANCE, call.getOperands());
    } else if (Types.isDate(right.getType())) {

      // Normalize operands order DATE+NUMERIC
      if (Types.isNumeric(left.getType())) {
        return new Call(
            call.getPosition(), AddDaysFunction.INSTANCE, call.getOperand(1), call.getOperand(0));
      }

      // Normalize operands order DATE+INTERVAL
      if (Types.isInterval(left.getType())) {
        return new Call(
            call.getPosition(),
            IntervalToTemporalAddOperator.INSTANCE,
            call.getOperand(1),
            call.getOperand(0));
      }

    } else if (Types.isInterval(left.getType())) {

      if (Types.isInterval(right.getType())) {
        return new Call(
            call.getPosition(), IntervalToIntervalAddOperator.INSTANCE, call.getOperands());
      }

      // Normalize operands order DATE+INTERVAL
      return new Call(
          call.getPosition(),
          IntervalToTemporalAddOperator.INSTANCE,
          call.getOperand(1),
          call.getOperand(0));
    }

    // Simplify arithmetic 0+A → A
    if (Literal.ZERO.equals(left)) {
      return right;
    }

    // Simplify arithmetic A+(-B) → A-B
    if (right.isOperator(Operators.NEGATE)) {
      return new Call(Operators.SUBTRACT, left, call(right).getOperand(0));
    }

    // Optimize data type
    if (Types.isInteger(call.getType())) {
      return new Call(IntegerAddOperator.INSTANCE, call.getOperands());
    }

    return new Call(call.getPosition(), NumberAddOperator.INSTANCE, call.getOperands());
  }

  private static final class IntegerAddOperator extends AddOperator {
    private static final IntegerAddOperator INSTANCE = new IntegerAddOperator();

    private IntegerAddOperator() {
      super();
    }

    @Override
    public boolean isSymmetrical() {
      return true;
    }

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
        return Math.addExact(left, right);
      } catch (ArithmeticException e) {
        throw new ExpressionException(ErrorCode.ARITHMETIC_OVERFLOW, getName());
      }
    }
  }

  private static final class NumberAddOperator extends AddOperator {

    private static final NumberAddOperator INSTANCE = new NumberAddOperator();

    private NumberAddOperator() {
      super();
    }

    @Override
    public boolean isSymmetrical() {
      return true;
    }

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

      return left.add(right);
    }
  }

  /** Adds a specified interval to a date or timestamp */
  private static final class IntervalToTemporalAddOperator extends AddOperator {
    private static final IntervalToTemporalAddOperator INSTANCE =
        new IntervalToTemporalAddOperator();

    private IntervalToTemporalAddOperator() {
      super();
    }

    @Override
    public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

      // Simplify arithmetic A+INTERVAL 0 → A
      IExpression operand = call.getOperand(1);
      if (operand.isConstant() && !operand.isNull()) {
        Interval interval = operand.getValue(Interval.class);
        if (interval.isZero()) {
          return call.getOperand(0);
        }
      }

      return call;
    }

    @Override
    public Object eval(final IExpression[] operands) {
      ZonedDateTime datetime = operands[0].getValue(ZonedDateTime.class);
      if (datetime == null) return null;

      Interval interval = operands[1].getValue(Interval.class);
      if (interval == null) return null;

      return interval.addTo(datetime);
    }
  }

  private static final class IntervalToIntervalAddOperator extends AddOperator {
    private static final AddOperator INSTANCE = new IntervalToIntervalAddOperator();

    private IntervalToIntervalAddOperator() {
      super();
    }

    @Override
    public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

      // Simplify arithmetic INTERVAL A+INTERVAL 0 → INTERVAL A
      IExpression operand = call.getOperand(1);
      if (operand.isConstant() && !operand.isNull()) {
        Interval interval = operand.getValue(Interval.class);
        if (interval.isZero()) {
          return call.getOperand(0);
        }
      }

      return call;
    }

    @Override
    public Object eval(final IExpression[] operands) {
      Interval interval0 = operands[0].getValue(Interval.class);
      if (interval0 == null) return null;

      Interval interval1 = operands[1].getValue(Interval.class);
      if (interval1 == null) return null;

      return interval0.add(interval1);
    }
  }
}
