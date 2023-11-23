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
import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.NumberType;

/**
 * Arithmetic unary minus (negative) operator '<code>-</code>'.
 */
public class NegateNumericOperator extends NegateOperator {

  public static final NegateNumericOperator INSTANCE = new NegateNumericOperator();

  public NegateNumericOperator() {
    super();
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression operand = call.getOperand(0);

    // Simplify arithmetic -(-(A)) → A
    if (operand.is(Operators.NEGATIVE)) {
      return operand.asCall().getOperand(0);
    }

    // Simplify arithmetic -(A-B) → B-A
    if (operand.is(Operators.SUBTRACT_NUMERIC)) {
      Call subtract = operand.asCall();
      return new Call(Operators.SUBTRACT_NUMERIC, subtract.getOperand(1), subtract.getOperand(0));
    }

    return call;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Object v0 = operands[0].getValue();
    if (v0 == null)
      return null;

    if (v0 instanceof Long) {
      Long value = (Long) v0;
      if (value == Long.MIN_VALUE) {
        throw new ArithmeticException(ExpressionError.ARITHMETIC_OVERFLOW.message(value));
      }
      return Long.valueOf(-value);
    }

    return NumberType.coerce(v0).negate();
  }
}
