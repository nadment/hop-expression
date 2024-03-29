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
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.type.Interval;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeFamily;
import org.apache.hop.expression.type.TypeId;
import java.io.StringWriter;
import java.math.BigDecimal;

/**
 * Prefix arithmetic minus (negative) operator '<code>-</code>' for numeric or interval.
 */
public class NegateOperator extends Operator {
  private static final NegateOperator IntervalNegateOperator = new IntervalNegateOperator();  
  private static final NegateOperator IntegerNegateOperator = new IntegerNegateOperator();
  private static final NegateOperator NumberNegateOperator = new NumberNegateOperator();
  
  public NegateOperator() {
    super("NEGATE", "-", 30, true, ReturnTypes.ARG0, OperandTypes.NUMERIC.or(OperandTypes.INTERVAL),
        OperatorCategory.MATHEMATICAL, "/docs/negate.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression operand = call.getOperand(0);
    
    // Simplify -(-(A)) → A
    if (operand.is(Operators.NEGATE)) {
      return operand.asCall().getOperand(0);
    }
    
    Type type = call.getOperand(0).getType();
    if (type.isFamily(TypeFamily.INTERVAL)) {
      return new Call(call.getPosition(), IntervalNegateOperator, call.getOperands());
    }
    
    // Simplify arithmetic -(A-B) → B-A
    if (operand.is(Operators.SUBTRACT)) {
      Call subtract = operand.asCall();
      return new Call(Operators.SUBTRACT, subtract.getOperand(1), subtract.getOperand(0));
    }
    
    NegateOperator operator = NumberNegateOperator;
    if (type.is(TypeId.INTEGER)) {
        operator = IntegerNegateOperator;
    }
    
    return new Call(call.getPosition(), operator, call.getOperands());
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append('-');
    operands[0].unparse(writer);
  }
  
  private static final class IntegerNegateOperator extends NegateOperator {
    @Override
    public Object eval(final IExpression[] operands) {
      Long value = operands[0].getValue(Long.class);
      if (value == null)
        return null;
      
      if (value == Long.MIN_VALUE) {
        throw new ArithmeticException(ErrorCode.ARITHMETIC_OVERFLOW.message(value));
      }
      return Long.valueOf(-value);      
    }
  }

  private static final class NumberNegateOperator extends NegateOperator {
    @Override
    public Object eval(final IExpression[] operands) {
      BigDecimal value = operands[0].getValue(BigDecimal.class);
      if (value == null)
        return null;      
      return value.negate();      
    }
  }
  
  private static final class IntervalNegateOperator extends NegateOperator {
    @Override
    public Object eval(final IExpression[] operands) {
      Interval interval = operands[0].getValue(Interval.class);
      if (interval == null)
        return null;

      return interval.negate();
    }
  }

}
