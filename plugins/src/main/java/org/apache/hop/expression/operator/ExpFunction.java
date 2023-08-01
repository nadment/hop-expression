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
import org.apache.hop.expression.Category;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.math.BigDecimal;
import ch.obermuhlner.math.big.BigDecimalMath;

/**
 * Returns the exponential value of a numeric expression.
 */
@FunctionPlugin
public class ExpFunction extends Function {

  private static final Literal E = new Literal(BigDecimalMath.e(DECIMAL128), NumberType.NUMBER);

  public ExpFunction() {
    super("EXP", ReturnTypes.NUMBER, OperandTypes.NUMERIC, Category.MATHEMATICAL, "/docs/exp.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Simplify arithmetic "EXP(1)" to Euler number "E"
    if (Literal.ONE.equals(call.getOperand(0))) {
      return E;
    }

    return call;
  }

  @Override
    public Object eval(final IExpression[] operands) {
    BigDecimal value = operands[0].getValue(BigDecimal.class);
    if (value == null)
      return null;
    return BigDecimalMath.exp(value, DECIMAL128);
  }
}
