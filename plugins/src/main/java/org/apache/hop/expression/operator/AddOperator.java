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

import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionComparator;
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
import java.io.StringWriter;
import java.math.BigDecimal;
import java.time.ZonedDateTime;
import java.util.PriorityQueue;


/**
 * Generic addition operator.
 * <br>
 * <strong>Syntax:</strong> <code>x + y</code>
 */
public class AddOperator extends Operator {
  private static final AddOperator IntervalAddOperator = new IntervalAddOperator();
  private static final AddOperator IntegerAddOperator = new IntegerAddOperator();
  private static final AddOperator NumberAddOperator = new NumberAddOperator();
  
  public AddOperator() {
    super("ADD", "+", 100, true, ReturnTypes.ADDITIVE_OPERATOR,
        OperandTypes.NUMERIC_NUMERIC.or(OperandTypes.TEMPORAL_INTERVAL)
            .or(OperandTypes.INTERVAL_TEMPORAL).or(OperandTypes.TEMPORAL_NUMERIC),
        OperatorCategory.MATHEMATICAL, "/docs/add.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    if (left.getType().isFamily(TypeFamily.TEMPORAL)) {
      // Supports the basic addition and subtraction of days to DATE values, in the form of { + |
      // - } <integer>
      if (right.getType().isFamily(TypeFamily.NUMERIC)) {
        return new Call(call.getPosition(), AddDaysFunction.INSTANCE, call.getOperands());
      }

      return new Call(call.getPosition(), IntervalAddOperator, call.getOperands());
    } else if (left.getType().isFamily(TypeFamily.INTERVAL)) {
      // Normalize operands order DATE+INTERVAL
      return new Call(call.getPosition(), IntervalAddOperator, call.getOperand(1),
          call.getOperand(0));
    }
    
    // Rebuild chained operator 
    PriorityQueue<IExpression> operands = new PriorityQueue<>(new ExpressionComparator());
    operands.addAll(this.getChainedOperands(call, true));
    IExpression operand = operands.poll();
    while (!operands.isEmpty()) {
      call = new Call(this, operand, operands.poll());
      call.inferReturnType();
      operand = call;
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
    if (call.getType().is(TypeId.INTEGER) ) {
      return new Call(IntegerAddOperator, call.getOperands());
    }
    
    return new Call(call.getPosition(), NumberAddOperator, call.getOperands());
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append('+');
    operands[1].unparse(writer);
  }
  
  private static final class IntegerAddOperator extends AddOperator {
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
      if (left == null)
        return null;
      Long right = operands[1].getValue(Long.class);
      if (right == null)
        return null;

      return left+right;
    }
  }

  private static final class NumberAddOperator extends AddOperator {
    
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
      if (left == null)
        return null;
      BigDecimal right = operands[1].getValue(BigDecimal.class);
      if (right == null)
        return null;

      return left.add(right);
    }
  }
  
  /**
   * Adds a specified interval to a date or timestamp
   */
  private static final class IntervalAddOperator extends AddOperator {

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
      if (datetime == null)
        return null;

      Interval interval = operands[1].getValue(Interval.class);
      if (interval == null)
        return null;

      return interval.addTo(datetime);
    }
  }
}
