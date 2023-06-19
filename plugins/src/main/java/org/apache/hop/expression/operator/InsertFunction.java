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
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.TypeFamily;
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.StringType;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

/**
 * Replaces a substring of the specified length, starting at the specified position, with a new
 * string or binary value.
 */
@FunctionPlugin
public class InsertFunction extends Function {

  public static final IOperandTypeChecker OTC = OperandTypes.or(
      OperandTypes.family(TypeFamily.STRING, TypeFamily.NUMERIC, TypeFamily.NUMERIC,
          TypeFamily.STRING),
      OperandTypes.family(TypeFamily.BINARY, TypeFamily.NUMERIC, TypeFamily.NUMERIC,
          TypeFamily.BINARY));

  public InsertFunction() {
    super("INSERT", ReturnTypes.ARG0, OTC, OperatorCategory.STRING, "/docs/insert.html");
  }

  @Override
  public Object eval(IExpression[] operands)
      throws Exception {
    Object v0 = operands[0].getValue();
    if (v0 == null)
      return null;
    Long v1 = operands[1].getValue(Long.class);
    if (v1 == null)
      return null;
    Long v2 = operands[2].getValue(Long.class);
    if (v2 == null)
      return null;
    Object v3 = operands[3].getValue();
    if (v3 == null)
      return null;

    int position = v1.intValue();
    int length = v2.intValue();

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
        buffer.write(BinaryType.coerce(v3));
        buffer.write(bytes, start + length, bytes.length - start - length);
        return buffer.toByteArray();
      } catch (IOException e) {
        throw new ExpressionException(ExpressionError.INTERNAL_ERROR, e);
      }
    }

    String str = StringType.coerce(v0);
    int start = Math.min(Math.max(0, position - 1), str.length());

    length = Math.min(length, str.length());
    if (length < 0)
      throw new ExpressionException(ExpressionError.ARGUMENT_OUT_OF_RANGE, length);

    StringBuilder builder = new StringBuilder();
    builder.append(str.substring(0, start));
    builder.append(StringType.coerce(v3));
    builder.append(str.substring(start + length));
    return builder.toString();
  }

}
