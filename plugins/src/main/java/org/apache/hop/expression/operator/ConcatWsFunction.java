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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeFamily;

/** String or binary concatenation function with separator */
@FunctionPlugin
public class ConcatWsFunction extends Function {

  public ConcatWsFunction() {
    super(
        "CONCAT_WS",
        ReturnTypes.CONCATWS_FUNCTION,
        OperandTypes.STRING_STRING_VARIADIC.or(OperandTypes.BINARY_BINARY_VARIADIC),
        OperatorCategory.STRING,
        "/docs/concat_ws.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Type type = call.getOperand(0).getType();
    if (type.isFamily(TypeFamily.BINARY)) {
      return new Call(ConcatWsBinary.INSTANCE, call.getOperands());
    }

    return new Call(ConcatWsString.INSTANCE, call.getOperands());
  }

  /** String concatenation function with separator */
  private static final class ConcatWsString extends ConcatWsFunction {
    public static final ConcatWsFunction INSTANCE = new ConcatWsString();

    @Override
    public Object eval(final IExpression[] operands) {

      String separator = operands[0].getValue(String.class);
      if (separator == null) return null;

      StringBuilder builder = new StringBuilder();
      for (int i = 1; i < operands.length; i++) {
        String value = operands[i].getValue(String.class);
        if (value != null) {
          if (builder.length() > 0) {
            builder.append(separator);
          }
          builder.append(value);
        }
      }

      if (builder.length() == 0) return null;

      return builder.toString();
    }
  }

  /** Binary concatenation function with separator */
  private static final class ConcatWsBinary extends ConcatWsFunction {
    public static final ConcatWsFunction INSTANCE = new ConcatWsBinary();

    @Override
    public Object eval(final IExpression[] operands) {

      byte[] separator = operands[0].getValue(byte[].class);
      if (separator == null) return null;

      boolean notFirstValue = false;

      try {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        for (int i = 1; i < operands.length; i++) {
          byte[] value = operands[i].getValue(byte[].class);

          if (value != null) {
            if (notFirstValue) {
              output.write(separator);
            }
            notFirstValue = true;
            output.write(value);
          }
        }

        if (output.size() == 0) return null;

        return output.toByteArray();
      } catch (IOException e) {
        throw new ExpressionException(e.getMessage());
      }
    }
  }
}
