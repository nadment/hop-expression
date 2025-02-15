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
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Types;

/**
 * Arithmetic division function. <br>
 * <strong>Syntax:</strong> <code>DIV0(x,y)</code>
 *
 * @see DivOperator
 */
@FunctionPlugin
public class Div0Function extends Function {

  public Div0Function() {
    super(
        "DIV0",
        ReturnTypes.DIVIDE_OPERATOR,
        OperandTypes.NUMBER_NUMBER,
        OperatorCategory.MATHEMATICAL,
        "/docs/div0.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Simplify DIV0(x,NULL) → NULL
    if (right.isNull()) {
      return Literal.NULL;
    }

    // Simplify DIV0(NULL,x) → NULL
    if (left.isNull()) {
      return Literal.NULL;
    }

    // Simplify arithmetic DIV0(A,0) → 0 if x is not nullable
    if (Literal.ZERO.equals(right) && !left.getType().isNullable()) {
      return Literal.ZERO;
    }

    // Simplify arithmetic DIV0(A,1) → A
    if (Literal.ONE.equals(right)) {
      return call.getOperand(0);
    }

    // Simplify arithmetic DIV0(-A,-B) → DIV0(A,B)
    if (left.isOperator(NegateOperator.INSTANCE) && right.isOperator(NegateOperator.INSTANCE)) {
      return new Call(call.getOperator(), call(left).getOperand(0), call(right).getOperand(0));
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

    // Prevent a division by zero and return zero
    if (divisor.signum() == 0) return BigDecimal.ZERO;

    return value.divide(divisor, MATH_CONTEXT);
  }
}
