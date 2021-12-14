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

import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.math.MathContext;

/** Arithmetic division operator '<code>/</code>'. */
public class Divide extends Operator {

  public Divide() {
    super("DIVIDE", "/", 50, true, true, "i18n::Operator.Category.Mathematical",
        "/docs/divide.html");
  }

  // @ScalarFunction(id = "DIVIDE", minArgs = 2, maxArgs = 2,
  // category = "i18n::Operator.Category.Mathematical")
  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    Object left = operands[0].eval(context);
    if (left == null)
      return null;
    Object right = operands[1].eval(context);
    if (right == null)
      return null;

    if (left instanceof BigDecimal || right instanceof BigDecimal) {
      BigDecimal divisor = coerceToBigNumber(right);
      // prevent a division by zero ..
      if (divisor.signum() == 0)
        throw ExpressionException.createDivisionByZero();
      return coerceToBigNumber(left).divide(coerceToBigNumber(right), MathContext.DECIMAL128);
    }
    if (left instanceof Double || right instanceof Double) {
      double divisor = coerceToNumber(right);
      // prevent a division by zero ..
      if (divisor == 0D)
        throw ExpressionException.createDivisionByZero();
      return coerceToNumber(left) / divisor;
    }
    if (left instanceof Long || right instanceof Long) {
      long divisor = coerceToInteger(right);
      // prevent a division by zero ..
      if (divisor == 0L)
        throw ExpressionException.createDivisionByZero();

      return coerceToInteger(left) / divisor;
    }

    return coerceToBigNumber(left).divide(coerceToBigNumber(right));
  }

  @Override
  public void write(StringWriter writer, IExpression[] operands) {
    operands[0].write(writer);
    writer.append('/');
    operands[1].write(writer);
  }
}
