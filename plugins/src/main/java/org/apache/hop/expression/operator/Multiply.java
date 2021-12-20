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
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import java.io.StringWriter;
import java.math.BigDecimal;

/**
 * Arithmetic multiplication operator '<code>*</code>'.
 */
public class Multiply extends Operator {

  public Multiply() {
    super("MULTIPLY", "*", 50, true, true, "i18n::Operator.Category.Mathematical", "/docs/multiply.html");
  }

//  @ScalarFunction(id = "MULTIPLY", minArgs = 2, maxArgs = 2,
//      category = "i18n::Operator.Category.Mathematical")
  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object left = operands[0].eval(context);
    if (left == null)
      return null;
    Object right = operands[1].eval(context);
    if (right == null)
      return null;

    if (left instanceof BigDecimal || right instanceof BigDecimal) {
      return DataType.toBigNumber(left).multiply(DataType.toBigNumber(right));
    }
    if (left instanceof Double || right instanceof Double) {
      return DataType.toNumber(left) * DataType.toNumber(right);
    }
    if (left instanceof Long || right instanceof Long) {
      return DataType.toInteger(left) * DataType.toInteger(right);
    }

    return DataType.toBigNumber(left).multiply(DataType.toBigNumber(right));
  }

  @Override
  public void write(StringWriter writer, IExpression[] operands) {
    operands[0].write(writer);
    writer.append('*');
    operands[1].write(writer);
  }
}
