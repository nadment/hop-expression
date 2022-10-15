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

import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Coerse;
import java.math.BigDecimal;
import java.math.MathContext;

/**
 * 
 */
@FunctionPlugin
public class Div0Function extends Function {

  public Div0Function() {
    super("DIV0", true, ReturnTypes.ARG0, OperandTypes.NUMERIC_NUMERIC,
        "i18n::Operator.Category.Mathematical", "/docs/div0.html");
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands) throws Exception {
    Object left = operands[0].getValue(context);
    if (left == null)
      return null;
    Object right = operands[1].getValue(context);
    if (right == null)
      return null;

    if (left instanceof BigDecimal || right instanceof BigDecimal) {
      BigDecimal divisor = Coerse.toBigNumber(right);

      // prevent a division by zero and return zero
      if (divisor.signum() == 0)
        return divisor;

      return Coerse.toBigNumber(left).divide(Coerse.toBigNumber(right), MathContext.DECIMAL128);
    }
    if (left instanceof Double || right instanceof Double) {
      double divisor = Coerse.toNumber(right);
      // prevent a division by zero and return zero
      if (divisor == 0D)
        return 0D;
      return Coerse.toNumber(left) / divisor;
    }
    if (left instanceof Long || right instanceof Long) {
      long divisor = Coerse.toInteger(right);
      // prevent a division by zero and return zero
      if (divisor == 0L)
        return 0L;

      return Coerse.toInteger(left) / divisor;
    }

    BigDecimal divisor = Coerse.toBigNumber(right);
    // prevent a division by zero and return zero
    if (divisor.signum() == 0)
      return divisor;

    return Coerse.toBigNumber(left).divide(divisor);
  }

}
