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

import org.apache.hop.expression.Call;
import org.apache.hop.expression.Category;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Type;
import java.io.ByteArrayOutputStream;
import java.io.IOException;


/**
 * String or binary concatenation function with separator
 */
@FunctionPlugin
public class ConcatWsFunction extends Function {

  public ConcatWsFunction() {
    super("CONCAT_WS", ReturnTypes.FIRST_KNOWN,
        OperandTypes.STRING_STRING_VARIADIC.or(OperandTypes.BINARY_BINARY_VARIADIC),
        Category.STRING, "/docs/concat_ws.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Type type = call.getOperand(0).getType();

    if (type.isSameFamily(StringType.STRING)) {
      return new Call(ConcatWsStringFunction.INSTANCE, call.getOperands());
    }

    if (type.isSameFamily(BinaryType.BINARY)) {
      return new Call(ConcatWsBinaryFunction.INSTANCE, call.getOperands());
    }

    return call;
  }

  @Override
    public Object eval(final IExpression[] operands) {

    Object v0 = operands[0].getValue();
    if (v0 == null)
      return null;

    boolean notFirstValue = false;

    // Concat Binary
    if (v0 instanceof byte[]) {
      try {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        byte[] separator = BinaryType.coerce(v0);
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

        if (output.size() == 0)
          return null;

        return output.toByteArray();
      } catch (IOException e) {
        // TODO: Create specific function for binary
        throw new ExpressionException("CONCATWS ERROR");
      }
    }

    // Concat String
    StringBuilder builder = new StringBuilder();
    String separator = StringType.coerce(v0);
    for (int i = 1; i < operands.length; i++) {
      String value = operands[i].getValue(String.class);
      if (value != null) {
        if (notFirstValue) {
          builder.append(separator);
        }
        notFirstValue = true;
        builder.append(value);
      }
    }

    if (builder.length() == 0)
      return null;

    return builder.toString();
  }
}
