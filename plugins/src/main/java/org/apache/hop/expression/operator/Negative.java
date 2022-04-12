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
import java.io.StringWriter;

/** 
 * Arithmetic unary minus (negative) operator '<code>-</code>'.
 */
public class Negative extends Operator {
  public Negative() {
    super("NEGATIVE", "-", 30, true, true, "i18n::Operator.Category.Mathematical", "/docs/negative.html");
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);

    if (v0 == null)
      return null;

    if (v0 instanceof Double) {
      Double value = (Double) v0;      
      if (value == Double.MIN_VALUE) {
        throw new ExpressionException(Error.ARITHMETIC_OVERFLOW, value);
      }
      return Double.valueOf(-value);
    }

    if (v0 instanceof Long) {
      Long value = (Long) v0;
      if (value == Long.MIN_VALUE) {
        throw new ExpressionException(Error.ARITHMETIC_OVERFLOW, value);
      }
      return Long.valueOf(-value);
    }

    return DataType.toBigNumber(v0).negate();
  }

  @Override
  public void write(StringWriter writer, IExpression[] operands) {
    writer.append('-');
    operands[0].write(writer);
  }
}
