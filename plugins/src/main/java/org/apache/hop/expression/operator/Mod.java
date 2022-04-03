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

import org.apache.hop.expression.DataType;
import org.apache.hop.expression.Error;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.ScalarFunction;
import java.io.StringWriter;
import java.math.BigDecimal;

/** 
 * Arithmetic modulus operator.
 * <br><strong>Syntax:</strong> <code>x % y</code>
 */
public class Mod extends Operator {

  public Mod() {
    super("MOD", "%", 50, true, true, "i18n::Operator.Category.Mathematical", "/docs/mod.html");
  }

  @ScalarFunction(id = "MOD", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Mathematical", documentationUrl="/docs/mod.html")
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
      BigDecimal divisor = DataType.toBigNumber(right);
      // prevent a division by zero ..
      if (divisor.signum() == 0)
        throw new ExpressionException(Error.DIVISION_BY_ZERO);
      return DataType.toBigNumber(left).remainder(divisor);
    }
    if (left instanceof Double || right instanceof Double) {
      double divisor = DataType.toNumber(right);
      // prevent a division by zero ..
      if (divisor == 0L)
        throw new ExpressionException(Error.DIVISION_BY_ZERO);
      return DataType.toNumber(left) % divisor;
    }
    if (left instanceof Long || right instanceof Long) {
      long divisor = DataType.toInteger(right);
      // prevent a division by zero ..
      if (divisor == 0L)
        throw new ExpressionException(Error.DIVISION_BY_ZERO);
      return DataType.toInteger(left) % divisor;
    }

    return DataType.toBigNumber(left).remainder(DataType.toBigNumber(right));
  }

  @Override
  public void write(StringWriter writer, IExpression[] operands) {
    operands[0].write(writer);
    writer.append('%');
    operands[1].write(writer);
  }
}
