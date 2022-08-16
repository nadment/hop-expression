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
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Coerse;

/**
 * The function returns TRUE if the first value starts with second value. Both values must be data
 * type string or binary.
 *
 * @see {@link EndsWithFunction}
 */
@FunctionPlugin(id = "STARTSWITH", category = "i18n::Operator.Category.Comparison", documentationUrl = "/docs/startswith.html")
public class StartsWithFunction extends Function {

  public StartsWithFunction() {
    super("STARTSWITH", true, ReturnTypes.BOOLEAN, OperandTypes.STRING_STRING_OR_BINARY_BINARY, "i18n::Operator.Category.Comparison", "/docs/startswith.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {

    Object v0 = operands[0].getValue(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].getValue(context);
    if (v1 == null)
      return null;

    if (v0 instanceof byte[]) {
      byte[] data = Coerse.toBinary(v0);
      byte[] prefix = Coerse.toBinary(v1);
      if (prefix.length > data.length) {
        return Boolean.TRUE;
      } else {
        int end = prefix.length;
        for (int i = 0; i < end; i++) {
          if (data[i] != prefix[i]) {
            return Boolean.FALSE;
          }
        }
      }
      return Boolean.TRUE;
    }

    return Coerse.toString(v0).startsWith(Coerse.toString(v1));
  }
}