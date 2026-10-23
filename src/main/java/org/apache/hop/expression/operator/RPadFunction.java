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
import org.apache.hop.expression.util.Characters;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * The function right-pads a string or binary with another string or binary, to a certain length.
 *
 * @see LPadFunction
 */
@FunctionPlugin
@NullMarked
public class RPadFunction extends Function {

  public RPadFunction() {
    super(
        "RPAD",
        ReturnTypes.ARG0
            .andThen(TypeTransforms.TO_MAX_PRECISION)
            .andThen(TypeTransforms.TO_NULLABLE),
        OperandTypes.STRING_INTEGER
            .or(OperandTypes.STRING_INTEGER_STRING)
            .or(OperandTypes.BINARY_INTEGER_BINARY),
        OperatorCategory.STRING,
        "/docs/hop-user-manual/expression/rpad.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Type type = call.getType();
    if (Types.isBinary(type)) {
      return new Call(BinaryRPadFunction.INSTANCE, call.getOperands());
    }

    return new Call(StringRPadFunction.INSTANCE, call.getOperands());
  }

  /** The function right-pads a string with another string, to a certain length. */
  public static final class StringRPadFunction extends RPadFunction {
    public static final RPadFunction INSTANCE = new StringRPadFunction();

    @Override
    public @Nullable Object eval(final IExpression[] operands) {
      String value = operands[0].getValue(String.class);
      if (value == null) return null;

      Long v1 = operands[1].getValue(Long.class);
      if (v1 == null) return null;

      // Clamp to the int range, a negative length or one above the limit keeps its effect
      int length = Math.clamp(v1, -1, MAX_STRING_RESULT_SIZE + 1);

      // If this parameter is omitted, the function will pad spaces
      String pad = null;
      if (operands.length == 3) {
        pad = operands[2].getValue(String.class);
      }
      if (pad == null) {
        pad = " ";
      }

      // If the length is a negative number, the result is an empty string.
      if (length < 0) {
        return "";
      }
      if (length > MAX_STRING_RESULT_SIZE) {
        throw new ExpressionException(ErrorCode.RESULT_SIZE_TOO_LARGE, v1, MAX_STRING_RESULT_SIZE);
      }

      // The lengths are in characters, a surrogate pair is a single character
      final int index = length - Characters.length(value);
      if (index <= 0) {
        return value.substring(0, Characters.offset(value, length));
      }
      return value.concat(Characters.fill(pad, index));
    }
  }

  /** The function right-pads a binary with another binary, to a certain length. */
  public static final class BinaryRPadFunction extends RPadFunction {
    public static final RPadFunction INSTANCE = new BinaryRPadFunction();

    private static final byte[] DEFAULT = {0x00};

    @Override
    public @Nullable Object eval(final IExpression[] operands) {
      byte[] value = operands[0].getValue(byte[].class);
      if (value == null) return null;

      Long v1 = operands[1].getValue(Long.class);
      if (v1 == null) return null;

      // Clamp to the int range, a negative length or one above the limit keeps its effect
      int length = Math.clamp(v1, -1, MAX_BINARY_RESULT_SIZE + 1);

      // If this parameter is omitted, the function will pad 0x00
      byte[] pad = null;
      if (operands.length == 3) {
        pad = operands[2].getValue(byte[].class);
      }
      if (pad == null) {
        pad = DEFAULT;
      }
      // If the length is a negative number, the result is an empty array.
      if (length < 0) {
        return new byte[0];
      } else if (length > MAX_BINARY_RESULT_SIZE) {
        throw new ExpressionException(ErrorCode.RESULT_SIZE_TOO_LARGE, v1, MAX_BINARY_RESULT_SIZE);
      }

      // nothing to pad, but a value longer than the length must still be truncated
      if (pad.length == 0 && value.length <= length) {
        return value;
      }

      final int index = length - value.length;
      final byte[] result = new byte[length];

      if (index <= 0) {
        System.arraycopy(value, 0, result, 0, length);
      } else {
        System.arraycopy(value, 0, result, 0, value.length);
        for (int i = 0, pos = value.length; i < index; i++, pos++) {
          result[pos] = pad[i % pad.length];
        }
      }

      return result;
    }
  }
}
