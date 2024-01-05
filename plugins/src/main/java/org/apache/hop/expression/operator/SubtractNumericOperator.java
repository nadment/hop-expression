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
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.TypeId;
import org.apache.hop.expression.type.Types;
import java.math.BigDecimal;

/**
 * Arithmetic subtraction operator.
 * <br>
 * <strong>Syntax:</strong> <code>x - y</code>
 */
public class SubtractNumericOperator extends SubtractOperator {
  public static final SubtractNumericOperator INSTANCE = new SubtractNumericOperator();
  private static final SubtractNumericOperator SubtractInteger = new SubtractIntegerOperator();
  private static final SubtractNumericOperator SubtractNumber = new SubtractNumberOperator();
  
  public SubtractNumericOperator() {
    super();
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Simplify arithmetic A-0 → A
    if (Literal.ZERO.equals(right)) {
      return left;
    }

    // Simplify arithmetic 0-A → -A
    if (Literal.ZERO.equals(left)) {
      return new Call(Operators.NEGATIVE, right);
    }

    // Simplify arithmetic A-(-B) → A+B
    if (right.is(Operators.NEGATIVE)) {
      return new Call(AddNumericOperator.INSTANCE, left, right.asCall().getOperand(0));
    }

    // Optimize data type
    if (call.getType().is(TypeId.INTEGER) ) {
      return new Call(SubtractInteger, call.getOperands());
    }
    
    return new Call(SubtractNumber, call.getOperands());
  }
  
  @Override
  public boolean coerceType(Call call) {
    return Types.coercionArithmeticOperator(call);    
  }
  
  private static final class SubtractIntegerOperator extends SubtractNumericOperator {
    @Override
    public Object eval(final IExpression[] operands) {
      Long left = operands[0].getValue(Long.class);
      if (left == null)
        return null;
      Long right = operands[1].getValue(Long.class);
      if (right == null)
        return null;

      return left-right;
    }
  }

  private static final class SubtractNumberOperator extends SubtractNumericOperator {
    @Override
    public Object eval(final IExpression[] operands) {
      BigDecimal left = operands[0].getValue(BigDecimal.class);
      if (left == null)
        return null;
      BigDecimal right = operands[1].getValue(BigDecimal.class);
      if (right == null)
        return null;

      return left.subtract(right);
    }
  } 
}
