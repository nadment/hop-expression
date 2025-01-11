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

import ch.obermuhlner.math.big.BigDecimalMath;
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

/** Returns a number raised to the specified power exponent. */
@FunctionPlugin(names = "POW")
public class PowerFunction extends Function {

  public PowerFunction() {
    super(
        "POWER",
        ReturnTypes.NUMBER_NULLABLE,
        OperandTypes.NUMBER_NUMBER,
        OperatorCategory.MATHEMATICAL,
        "/docs/power.html");
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coerceOperandsType(call, call.getType());
  }

  @Override
  public Object eval(final IExpression[] operands) {
    BigDecimal number = operands[0].getValue(BigDecimal.class);
    if (number == null) {
      return null;
    }
    BigDecimal exponent = operands[1].getValue(BigDecimal.class);
    if (exponent == null) {
      return null;
    }

    return BigDecimalMath.pow(number, exponent, MATH_CONTEXT);
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Simplify arithmetic "POWER(X,1)" to "X"
    if (Literal.ONE.equals(call.getOperand(1))) {
      return call.getOperand(0);
    }

    return call;
  }
}
