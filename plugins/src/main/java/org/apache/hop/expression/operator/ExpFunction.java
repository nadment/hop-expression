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

import org.apache.commons.math3.util.FastMath;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.DataType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.math.BigDecimal;

/**
 * Returns the exponential value of a numeric expression.
 */
@FunctionPlugin
public class ExpFunction extends Function {

  public ExpFunction() {
    super("EXP", ReturnTypes.NUMBER, OperandTypes.NUMERIC, OperatorCategory.MATHEMATICAL,
        "/docs/exp.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call)
      throws ExpressionException {

    // Simplify arithmetic "EXP(1)" to Euler number "E"
    if (Literal.ONE.equals(call.getOperand(0))) {
      return new Literal(BigDecimal.valueOf(Math.E), DataType.NUMBER);
    }

    return call;
  }
  
  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    Double value = operands[0].getValue(context, Double.class);
    if (value == null)
      return null;
    return FastMath.exp(value);
  }

}
