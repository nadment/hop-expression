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

import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.DataTypeFamily;
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Coerse;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

/**
 * Replaces a substring of the specified length, starting at the specified position, with a new
 * string or binary value.
 */
@FunctionPlugin
public class InsertFunction extends Function {

  public static final IOperandTypeChecker OTC = OperandTypes.or(
      OperandTypes.family(DataTypeFamily.STRING, DataTypeFamily.NUMERIC, DataTypeFamily.NUMERIC,
          DataTypeFamily.STRING),
      OperandTypes.family(DataTypeFamily.STRING, DataTypeFamily.NUMERIC, DataTypeFamily.NUMERIC,
          DataTypeFamily.STRING));

  public InsertFunction() {
    super("INSERT", true, ReturnTypes.ARG0, OTC, "i18n::Operator.Category.String",
        "/docs/insert.html");
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
    Object v2 = operands[2].getValue(context);
    if (v2 == null)
      return null;
    Object v3 = operands[3].getValue(context);
    if (v3 == null)
      return null;

    int position = Coerse.toInteger(v1).intValue();
    int length = Coerse.toInteger(v2).intValue();

    if (v0 instanceof byte[]) {
      byte[] bytes = (byte[]) v0;
      int start = Math.min(Math.max(0, position - 1), bytes.length);
      length = Math.min(length, bytes.length);
      if (length < 0) {
        throw new ExpressionException(ExpressionError.ARGUMENT_OUT_OF_RANGE, length);
      }

      try {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        buffer.write(bytes, 0, start);
        buffer.write(Coerse.toBinary(v3));
        buffer.write(bytes, start + length, bytes.length - start - length);
        return buffer.toByteArray();
      } catch (IOException e) {
        throw new ExpressionException(ExpressionError.INTERNAL_ERROR, e);
      }
    }

    String str = Coerse.toString(v0);
    int start = Math.min(Math.max(0, position - 1), str.length());

    length = Math.min(length, str.length());
    if (length < 0)
      throw new ExpressionException(ExpressionError.ARGUMENT_OUT_OF_RANGE, length);

    StringBuilder builder = new StringBuilder();
    builder.append(str.substring(0, start));
    builder.append(Coerse.toString(v3));
    builder.append(str.substring(start + length));
    return builder.toString();
  }

}
