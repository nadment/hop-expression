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
import org.apache.hop.expression.ScalarFunction;
import org.apache.hop.expression.Type;
import java.io.StringWriter;

public class TryCast extends Operator {

  public TryCast() {
    super("TRY_CAST", 10, true, "i18n::Operator.Category.Conversion");
  }

  /**
   * Converts a Object of one data type into another data type if the cast succeeds; otherwise,
   * returns null.
   */
  @ScalarFunction(name = "TRY_CAST", minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.Conversion")
  public Object eval(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null) {
      return null;
    }
    try {
      Type type = (Type) operands[1].eval(context);

      if (operands.length == 3) {
        Object format = operands[2].eval(context);
        return convertTo(value, type, coerceToString(format));
      }

      return convertTo(value, type);
    } catch (Exception e) {
      return null;
    }
  }

  @Override
  public void write(StringWriter writer, IExpression[] operands) {
    writer.append("TRY_CAST(");
    operands[0].write(writer);
    writer.append(" AS ");
    writer.append(operands[1].toString());
    if (operands.length == 3) {
      writer.append(" FORMAT ");
      operands[2].write(writer);
    }
    writer.append(')');
  }
}
