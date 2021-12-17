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
 * Returns the position in the string that is the first character of a specified occurrence of the
 * substring.
 */
public class Position extends Operator {

  public Position() {
    super("POSITON", 10, true, true, "i18n::Operator.Category.String", "/docs/position.html");
  }

  @ScalarFunction(id = "POSITON", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.String", documentationUrl="/docs/position.html")
  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    Object v1 = operands[1].eval(context);

    if (v0 == null || v1 == null) {
      return null;
    }

    String substr = v0.toString();
    String str = v1.toString();

    return Long.valueOf(str.indexOf(substr, 0) + 1L);
  }

  @Override
  public void write(StringWriter writer, IExpression[] operands) {
    writer.append("POSITION(");
    operands[0].write(writer);
    writer.append(" IN ");
    operands[1].write(writer);
    writer.append(')');
  }
}
