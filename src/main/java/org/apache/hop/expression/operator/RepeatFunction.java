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

/** The function repeats a string or binary as many times as specified. */
@FunctionPlugin
@NullMarked
public class RepeatFunction extends Function {

  public RepeatFunction() {
    super(
        "REPEAT",
        ReturnTypes.ARG0
            .andThen(TypeTransforms.TO_MAX_PRECISION)
            .andThen(TypeTransforms.TO_NULLABLE),
        OperandTypes.STRING_INTEGER.or(OperandTypes.BINARY_INTEGER),
        OperatorCategory.STRING,
        "/docs/repeat.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Type type = call.getType();
    if (Types.isBinary(type)) {
      return new Call(BinaryRepeatFunction.INSTANCE, call.getOperands());
    }

    return new Call(StringRepeatFunction.INSTANCE, call.getOperands());
  }

  /** The function repeats a string as many times as specified. */
  public static final class StringRepeatFunction extends RepeatFunction {
    public static final StringRepeatFunction INSTANCE = new StringRepeatFunction();

    @Override
    public @Nullable Object eval(final IExpression[] operands) {
      String value = operands[0].getValue(String.class);
      if (value == null) return null;
      Long repeat = operands[1].getValue(Long.class);
      if (repeat == null) return null;
      // Nothing to repeat if the number of repetitions is zero or negative
      if (repeat <= 0) return "";

      final int len = value.length();
      // Nothing to repeat
      if (len == 0) return "";

      // The maximum size of the result, in characters
      checkSize(Characters.length(value), repeat, MAX_STRING_RESULT_SIZE);
      final int size = (int) (len * repeat);

      final char[] array = new char[size];
      value.getChars(0, len, array, 0);
      for (int n = 0; n < size; n += len) {
        System.arraycopy(array, 0, array, n, len);
      }
      return new String(array);
    }
  }

  /** The function repeats a binary as many times as specified. */
  public static final class BinaryRepeatFunction extends RepeatFunction {
    public static final BinaryRepeatFunction INSTANCE = new BinaryRepeatFunction();

    @Override
    public @Nullable Object eval(final IExpression[] operands) {
      byte[] value = operands[0].getValue(byte[].class);
      if (value == null) return null;
      Long repeat = operands[1].getValue(Long.class);
      if (repeat == null) return null;
      // Nothing to repeat if the number of repetitions is zero or negative
      if (repeat <= 0) return new byte[0];

      final int len = value.length;
      // Nothing to repeat
      if (len == 0) return new byte[0];

      // The maximum size of the result, in bytes
      checkSize(len, repeat, MAX_BINARY_RESULT_SIZE);
      final int size = (int) (len * repeat);

      final byte[] array = new byte[size];
      for (int n = 0; n < size; n += len) {
        System.arraycopy(value, 0, array, n, len);
      }
      return array;
    }
  }

  /**
   * Checks that the size of the result is not greater than the maximum size of a result.
   *
   * @param length the length of the value to repeat, greater than 0
   * @param repeat the number of repetitions, greater than 0
   * @param maxSize the maximum size of the result
   * @throws ExpressionException if the result is too large
   */
  private static void checkSize(final int length, final long repeat, final int maxSize) {
    // A saturated product, a huge number of repetitions would overflow the long range
    long size = (repeat > Long.MAX_VALUE / length) ? Long.MAX_VALUE : length * repeat;
    if (size > maxSize) {
      throw new ExpressionException(ErrorCode.RESULT_SIZE_TOO_LARGE, size, maxSize);
    }
  }
}
