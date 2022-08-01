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
 * The function extracts a number of characters from a string (starting from right)
 * 
 * @See {@link LeftFunction}
 */
@FunctionPlugin(id = "RIGHT", category = "i18n::Operator.Category.String", documentationUrl = "/docs/right.html")
public class RightFunction extends Function {

  public RightFunction() {
    super("RIGHT", true, ReturnTypes.ARG0, OperandTypes.STRING_NUMERIC_OR_BINARY_NUMERIC, "i18n::Operator.Category.String", "/docs/right.html");
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
    int length = Coerse.toInteger(v1).intValue();
    if (length < 0) {
      length = 0;
    }

    if (v0 instanceof byte[]) {
      byte[] bytes = (byte[]) v0;
      if (bytes.length <= length)
        return bytes;
      byte[] result = new byte[length];
      System.arraycopy(bytes, bytes.length - length, result, 0, length);
      return result;
    }

    String str = Coerse.toString(v0);
    if (str.length() <= length) {
      return str;
    }
    return str.substring(str.length() - length);
  }
}
