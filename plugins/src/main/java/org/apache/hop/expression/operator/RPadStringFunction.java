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
 * The function right-pads a string with another string, to a certain length.
 *
 * @see {@link LPadFunction}
 */
public class RPadStringFunction extends RPadFunction {

  static final RPadStringFunction INSTANCE = new RPadStringFunction();

  public RPadStringFunction() {
    super();
  }

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

    // If length is a negative number, the result is an empty string.
    if (length < 0) {
      return "";
    }
    if (length > PAD_LIMIT) {
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
      value = value.concat(pad);
    } else if (index < size) {
      value = value.concat(pad.substring(0, index));
    } else {
      final char[] padding = new char[index];
      final char[] padChars = pad.toCharArray();
      for (int i = 0; i < index; i++) {
        padding[i] = padChars[i % size];
      }
      value = value.concat(new String(padding));
    }

    return value;
  }
}
