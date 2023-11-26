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
import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeFamily;

/**
 * The function left-pads a string or binary with another string or binary, to a certain length.
 *
 * @see {@link RPadFunction}
 */
@FunctionPlugin
public class LPadFunction extends Function {

  public static final LPadFunction LPadStringFunction = new LPadString();
  public static final LPadFunction LPadBinaryFunction = new LPadBinary();

  /** The maximum size to which the padding can expand. */
  protected static final int PAD_LIMIT = 8192;

  public LPadFunction() {
    super("LPAD", ReturnTypes.ARG0_MAX_PRECISION, OperandTypes.STRING_NUMERIC
        .or(OperandTypes.STRING_NUMERIC_STRING).or(OperandTypes.BINARY_NUMERIC_BINARY),
        Category.STRING, "/docs/lpad.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Type type = call.getOperand(0).getType();
    if (type.isFamily(TypeFamily.BINARY)) {
      return new Call(LPadBinaryFunction, call.getOperands());
    }

    return new Call(LPadStringFunction, call.getOperands());
  }

  /**
   * The function left-pads a string with another string, to a certain length.
   */
  private static final class LPadString extends LPadFunction {
    @Override
    public Object eval(final IExpression[] operands) {
      String value = operands[0].getValue(String.class);
      if (value == null)
        return null;

      Long v1 = operands[1].getValue(Long.class);
      int length = v1.intValue();

      // If this parameter is omitted, the function will pad spaces
      String pad = null;
      if (operands.length == 3) {
        pad = operands[2].getValue(String.class);
      }
      if (pad == null) {
        pad = " ";
      }

      if (length < 0) {
        length = 0;
      } else if (length > PAD_LIMIT) {
        throw new IllegalArgumentException(ExpressionError.ILLEGAL_ARGUMENT
            .message("Paddind length exceeds maximum limit: " + PAD_LIMIT));
      }

      final int size = pad.length();
      final int index = length - value.length();

      if (index <= 0) {
        value = value.substring(0, length);
      } else if (size == 0) {
        // nothing to do
      } else if (index == size) {
        value = pad.concat(value);
      } else if (index < size) {
        value = pad.substring(0, index).concat(value);
      } else {
        final char[] padding = new char[index];
        final char[] padChars = pad.toCharArray();
        for (int i = 0; i < index; i++) {
          padding[i] = padChars[i % size];
        }
        value = new String(padding).concat(value);
      }

      return value;
    }

  }

  /**
   * The function left-pads a binary with another binary, to a certain length.
   */
  private static final class LPadBinary extends LPadFunction {

    private static final byte[] DEFAULT = new byte[] {0x00};

    @Override
    public Object eval(final IExpression[] operands) {
      byte[] value = operands[0].getValue(byte[].class);
      if (value == null)
        return null;

      Long v1 = operands[1].getValue(Long.class);
      int length = v1.intValue();

      // If this parameter is omitted, the function will pad 0x00
      byte[] pad = null;
      if (operands.length == 3) {
        pad = operands[2].getValue(byte[].class);
      }
      if (pad == null) {
        pad = DEFAULT;
      }
      // If length is a negative number, the result is an empty array.
      if (length < 0) {
        return new byte[0];
      } else if (length > PAD_LIMIT) {
        throw new IllegalArgumentException(ExpressionError.ILLEGAL_ARGUMENT
            .message("Paddind length exceeds maximum limit: " + PAD_LIMIT));
      }

      // nothing to pad
      if (pad.length == 0) {
        return value;
      }

      final int index = length - value.length;
      final byte[] result = new byte[length];

      if (index <= 0) {
        System.arraycopy(value, 0, result, 0, length);
      } else {
        System.arraycopy(value, 0, result, index, value.length);
        for (int i = 0, pos = 0; i < index; i++, pos++) {
          result[pos] = pad[i % pad.length];
        }
      }

      return result;
    }
  }
}
