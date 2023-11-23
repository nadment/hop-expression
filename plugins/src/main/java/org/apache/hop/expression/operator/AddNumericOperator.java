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
import java.math.BigDecimal;


/**
 * Arithmetic addition operator.
 * <br>
 * <strong>Syntax:</strong> <code>x + y</code>
 */
public class AddNumericOperator extends AddOperator {
  public static final AddNumericOperator INSTANCE = new AddNumericOperator();

  public AddNumericOperator() {
    super();
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Simplify arithmetic 0+A → A
    if (Literal.ZERO.equals(left)) {
      return right;
    }

    // Simplify arithmetic A+(-B) → A-B
    if (right.is(Operators.NEGATIVE)) {
      return new Call(Operators.SUBTRACT, left, right.asCall().getOperand(0));
    }

    // Pull up literal (1+A)+2 → 3+A
    if (left.isConstant() && right.is(AddNumericOperator.INSTANCE)
        && right.asCall().getOperand(0).isConstant()) {
      Call expression = new Call(AddNumericOperator.INSTANCE, left, right.asCall().getOperand(0));

      Literal literal = Literal.of(expression.getValue());

      return new Call(AddNumericOperator.INSTANCE, literal, right.asCall().getOperand(1));
    }

    return call;
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

  @Override
  public boolean isSymmetrical() {
    return true;
  }
}
