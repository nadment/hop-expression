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
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.math.BigDecimal;

/**
 * Arithmetic division function.
 * <br>
 * <strong>Syntax:</strong> <code>DIV0(x,y)</code>
 * 
 * @see {@link DivOperator}
 */
@FunctionPlugin
public class Div0Function extends Function {

  public Div0Function() {
    super("DIV0", ReturnTypes.DIVIDE_OPERATOR, OperandTypes.NUMERIC_NUMERIC,
        OperatorCategory.MATHEMATICAL, "/docs/div0.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Simplify arithmetic "DIV0(A,1)" to "A"
    if (Literal.ONE.equals(right)) {
      return call.getOperand(0);
    }
    
    // Simplify arithmetic "DIV0(-A,-B)" to "DIV0(A,B)"
    if (left.is(Operators.NEGATIVE) && right.is(Operators.NEGATIVE)) {
      return new Call(call.getOperator(), left.asCall().getOperand(0), right.asCall().getOperand(0));
    }
    
    return call;
  }
  
  @Override
  public Object eval(IExpression[] operands) throws Exception {
    BigDecimal value = operands[0].getValue(BigDecimal.class);
    if (value == null)
      return null;
    BigDecimal divisor = operands[1].getValue(BigDecimal.class);
    if (divisor == null)
      return null;

    // Prevent a division by zero and return zero
    if (divisor.signum() == 0)
      return BigDecimal.ZERO;

    return value.divide(divisor, DECIMAL128);
  }
}
