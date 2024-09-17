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
import org.apache.hop.expression.ExpressionComparator;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.type.Interval;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.TypeFamily;
import org.apache.hop.expression.type.TypeId;
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
            .or(OperandTypes.TEMPORAL_INTERVAL)
            .or(OperandTypes.TEMPORAL_NUMERIC)
            .or(OperandTypes.NUMERIC_TEMPORAL)
            .or(OperandTypes.INTERVAL_TEMPORAL)
            .or(OperandTypes.INTERVAL_INTERVAL),
        OperatorCategory.MATHEMATICAL,
        "/docs/add.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Rebuild chained operator
    PriorityQueue<IExpression> operands = new PriorityQueue<>(new ExpressionComparator());
    operands.addAll(this.getChainedOperands(call, true));
    IExpression operand = operands.poll();
    while (!operands.isEmpty()) {
      call = new Call(this, operand, operands.poll());
      call.inferReturnType();
      operand = call;
    }

    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    if (left.getType().isFamily(TypeFamily.TEMPORAL)) {
      // Supports the basic addition and subtraction of days to DATE values, in the form of { + |
      // - } <integer>
      if (right.getType().isFamily(TypeFamily.NUMERIC)) {
        return new Call(call.getPosition(), AddDaysFunction.INSTANCE, call.getOperands());
      }

      return new Call(call.getPosition(), AddIntervalToTemporal.INSTANCE, call.getOperands());
    } else if (right.getType().isFamily(TypeFamily.TEMPORAL)) {
      // Normalize operands order DATE+NUMERIC
      if (left.getType().isFamily(TypeFamily.NUMERIC)) {
        return new Call(
            call.getPosition(), AddDaysFunction.INSTANCE, call.getOperand(1), call.getOperand(0));
      }

      // Normalize operands order DATE+NUMERIC
      if (left.getType().isFamily(TypeFamily.INTERVAL)) {
        return new Call(
            call.getPosition(),
            AddIntervalToTemporal.INSTANCE,
            call.getOperand(1),
            call.getOperand(0));
      }

    } else if (left.getType().isFamily(TypeFamily.INTERVAL)) {

      if (right.getType().isFamily(TypeFamily.INTERVAL)) {
        return new Call(call.getPosition(), AddIntervalToInterval.INSTANCE, call.getOperands());
      }

      // Normalize operands order DATE+INTERVAL
      return new Call(
          call.getPosition(),
          AddIntervalToTemporal.INSTANCE,
          call.getOperand(1),
          call.getOperand(0));
    }

    // Simplify arithmetic 0+A → A
    if (Literal.ZERO.equals(left)) {
      return right;
    }

    // Simplify arithmetic A+(-B) → A-B
    if (right.is(Operators.NEGATE)) {
      return new Call(Operators.SUBTRACT, left, right.asCall().getOperand(0));
    }

    // Optimize data type
    if (call.getType().is(TypeId.INTEGER)) {
      return new Call(AddInteger.INSTANCE, call.getOperands());
    }

    return new Call(call.getPosition(), AddNumber.INSTANCE, call.getOperands());
  }

  private static final class AddInteger extends AddOperator {
    private static final AddOperator INSTANCE = new AddInteger();

    private AddInteger() {
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

      return left + right;
    }
  }

  private static final class AddNumber extends AddOperator {

    private static final AddOperator INSTANCE = new AddNumber();

    private AddNumber() {
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
  private static final class AddIntervalToTemporal extends AddOperator {
    private static final AddOperator INSTANCE = new AddIntervalToTemporal();

    private AddIntervalToTemporal() {
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

  private static final class AddIntervalToInterval extends AddOperator {
    private static final AddOperator INSTANCE = new AddIntervalToInterval();

    private AddIntervalToInterval() {
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
