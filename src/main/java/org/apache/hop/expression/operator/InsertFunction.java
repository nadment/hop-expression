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
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeTransforms;
import org.apache.hop.expression.type.Types;

/**
 * Replaces a substring of the specified length, starting at the specified position, with a new
 * string or binary value.
 */
@FunctionPlugin
public class InsertFunction extends Function {

  public InsertFunction() {
    super(
        "INSERT",
        ReturnTypes.ARG0
            .andThen(TypeTransforms.TO_MAX_PRECISION)
            .andThen(TypeTransforms.TO_NULLABLE),
        OperandTypes.STRING_INTEGER_INTEGER_STRING.or(OperandTypes.BINARY_INTEGER_INTEGER_BINARY),
        OperatorCategory.STRING,
        "/docs/insert.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Type type = call.getType();
    if (Types.isString(type)) {
      return new Call(StringInsertFunction.INSTANCE, call.getOperands());
    }
    if (Types.isBinary(type)) {
      return new Call(BinaryInsertFunction.INSTANCE, call.getOperands());
    }

    return call;
  }

  /**
   * Replaces a substring of the specified length, starting at the specified position, with a new
   * string value.
   */
  public static final class StringInsertFunction extends InsertFunction {
    public static final InsertFunction INSTANCE = new StringInsertFunction();

    @Override
    public Object eval(final IExpression[] operands) {
      String original = operands[0].getValue(String.class);
      if (original == null) return null;
      Long pos = operands[1].getValue(Long.class);
      if (pos == null) return null;
      Long len = operands[2].getValue(Long.class);
      if (len == null) return null;
      String insert = operands[3].getValue(String.class);
      if (insert == null) return null;

      // Valid values are between 1 and one more than the length of the string (inclusive).
      if (pos <= 0 || pos > original.length() + 1)
        throw new IllegalArgumentException(ErrorCode.ARGUMENT_OUT_OF_RANGE.message(2, pos));

      // Valid values range from 0 to the number of characters between pos and the end of the
      // string.
      if (len < 0 || len > original.length() - pos + 1)
        throw new IllegalArgumentException(ErrorCode.ARGUMENT_OUT_OF_RANGE.message(3, len));

      int start = Math.min(Math.max(0, pos.intValue() - 1), original.length());
      int length = Math.min(len.intValue(), original.length());

      return original.substring(0, start) + insert + original.substring(start + length);
    }
  }

  /**
   * Replaces a substring of the specified length, starting at the specified position, with a new
   * binary value.
   */
  public static final class BinaryInsertFunction extends InsertFunction {

    public static final InsertFunction INSTANCE = new BinaryInsertFunction();

    @Override
    public Object eval(final IExpression[] operands) {
      byte[] value = operands[0].getValue(byte[].class);
      if (value == null) return null;
      Long pos = operands[1].getValue(Long.class);
      if (pos == null) return null;
      Long len = operands[2].getValue(Long.class);
      if (len == null) return null;
      byte[] insert = operands[3].getValue(byte[].class);
      if (insert == null) return null;

      // Valid values are between 1 and one more than the length of the binary (inclusive).
      if (pos <= 0 || pos > value.length + 1)
        throw new IllegalArgumentException(ErrorCode.ARGUMENT_OUT_OF_RANGE.message(2, pos));

      // Valid values range from 0 to the number of byte between pos and the end of the binary.
      if (len < 0 || len > value.length - pos + 1)
        throw new IllegalArgumentException(ErrorCode.ARGUMENT_OUT_OF_RANGE.message(3, len));

      int position = pos.intValue() - 1;
      int start = Math.min(position, value.length);
      int length = Math.min(len.intValue(), insert.length);

      byte[] result = new byte[value.length - length + insert.length];
      System.arraycopy(value, 0, result, 0, start);
      System.arraycopy(insert, 0, result, start, insert.length);
      System.arraycopy(
          value, start + length, result, start + insert.length, value.length - start - length);
      return result;
    }
  }
}
