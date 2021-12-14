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
import java.io.StringWriter;

/** 
 * String concatenation operator '<code>||</code>'
 */
public class Concat extends Operator {

  public Concat() {
    super("CONCAT", "||", 110, true, true, "i18n::Operator.Category.String", "/docs/concat.html");
  }

  @ScalarFunction(id = "CONCAT", minArgs = 2, maxArgs = Integer.MAX_VALUE,
      category = "i18n::Operator.Category.String", documentationUrl="/docs/concat.html")
  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    StringBuilder builder = new StringBuilder();
    for (IExpression operand : operands) {
      Object value = operand.eval(context);
      if (value != null)
        builder.append(coerceToString(value));
    }

    if (builder.length() == 0)
      return null;

    return builder.toString();
  }

  @Override
  public void write(StringWriter writer, IExpression[] operands) {
    boolean concatFirst = true;
    for (IExpression operand : operands) {
      if (concatFirst)
        concatFirst = false;
      else
        writer.append("||");
      operand.write(writer);
    }
  }
}
