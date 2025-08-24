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
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Types;

/**
 * Arithmetic division operator. <br>
 * <strong>Syntax:</strong> <code>x / y</code>
 *
 * @see Div0Function
 */
public class DivOperator extends BinaryOperator {
  public static final DivOperator INSTANCE = new DivOperator();

  public DivOperator() {
    super(
        "DIV",
        "/",
        50,
        Associativity.LEFT,
        ReturnTypes.DIVIDE_OPERATOR,
        OperandTypes.NUMERIC_NUMERIC,
        OperatorCategory.MATHEMATICAL,
        "/docs/div.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Simplify x / NULL → NULL
    if (right.isNull()) {
      return Literal.NULL;
    }

    // Simplify NULL / x → NULL
    if (left.isNull()) {
      return Literal.NULL;
    }

    // Simplify arithmetic A / 1 → A
    if (Literal.ONE.equals(right)) {
      return call.getOperand(0);
    }

    // Simplify arithmetic (-A) / (-B) → A / B
    if (left.isOperator(NegateOperator.INSTANCE) && right.isOperator(NegateOperator.INSTANCE)) {
      return new Call(DivOperator.INSTANCE, call(left).getOperand(0), call(right).getOperand(0));
    }

    if (left.isOperator(DivOperator.INSTANCE)) {
      // Simplify arithmetic (A / B) / C → A / (B * C) (if B and C are constants)
      if (call(left).getOperand(1).isConstant()) {
        if (right.isConstant()) {
          Call denominator = new Call(MultiplyOperator.INSTANCE, call(left).getOperand(1), right);
          return new Call(DivOperator.INSTANCE, call(left).getOperand(0), denominator);
        } else {
          // Move constant up (A / B) / C → ( A / C ) / B (if B is constant)
          Call numerator = new Call(DivOperator.INSTANCE, call(left).getOperand(0), right);
          numerator.inferReturnType();
          return new Call(DivOperator.INSTANCE, numerator, call(left).getOperand(1));
        }
      }
    }

    // Simplify arithmetic (A * B) / C → (A / C) * B (if A and C are constants)
    if (right.isConstant()
        && left.isOperator(MultiplyOperator.INSTANCE)
        && call(left).getOperand(0).isConstant()) {
      Call numerator = new Call(DivOperator.INSTANCE, call(left).getOperand(0), right);
      return new Call(MultiplyOperator.INSTANCE, numerator, call(left).getOperand(1));
    }

    return call;
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coercionMultiplyOperator(call);
  }

  @Override
  public Object eval(final IExpression[] operands) {
    BigDecimal value = operands[0].getValue(BigDecimal.class);
    if (value == null) return null;
    BigDecimal divisor = operands[1].getValue(BigDecimal.class);
    if (divisor == null) return null;

    // Prevent a division by zero ..
    if (divisor.signum() == 0) throw new ExpressionException(ErrorCode.DIVISION_BY_ZERO);

    return value.divide(divisor, MATH_CONTEXT);
  }
}
