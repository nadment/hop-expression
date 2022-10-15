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
import java.io.ByteArrayOutputStream;
import java.io.IOException;


/**
 * The function repeats a string as many times as specified.
 */
@FunctionPlugin
public class RepeatFunction extends Function {

  public RepeatFunction() {
    super("REPEAT", true, ReturnTypes.STRING, OperandTypes.STRING_NUMERIC_OR_BINARY_NUMERIC,
        "i18n::Operator.Category.String", "/docs/repeat.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    Object v0 = operands[0].getValue(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].getValue(context);
    if (v1 == null)
      return null;
    int count = Coerse.toInteger(v1).intValue();

    if (v0 instanceof byte[]) {
      byte[] bytes = (byte[]) v0;
      try {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        for (int i = 0; i < count; i++) {
          buffer.write(bytes);
        }
        return buffer.toByteArray();
      } catch (IOException e) {
        return null;
      }
    }

    String value = Coerse.toString(v0);
    StringBuilder builder = new StringBuilder(value.length() * count);
    while (count-- > 0) {
      builder.append(value);
    }
    return builder.toString();
  }
}
