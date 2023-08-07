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
import org.apache.hop.expression.IExpression;

/**
 * The function left-pads a binary with another binary, to a certain length.
 */
public class LPadBinaryFunction extends LPadFunction {

  private static final byte[] DEFAULT = new byte[] {0x00};
  static final LPadBinaryFunction INSTANCE = new LPadBinaryFunction();

  public LPadBinaryFunction() {
    super();
  }

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
