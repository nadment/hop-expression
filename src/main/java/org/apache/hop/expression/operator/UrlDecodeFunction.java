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

import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Characters;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * The function decodes the URL to a string.
 *
 * @see UrlEncodeFunction
 */
@FunctionPlugin
@NullMarked
public class UrlDecodeFunction extends Function {

  public UrlDecodeFunction() {
    super(
        "URL_DECODE",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING,
        OperatorCategory.STRING,
        "/docs/url_decode.html");
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null) return null;

    // Fast path, nothing to decode
    if (value.indexOf('%') < 0 && value.indexOf('+') < 0) {
      return value;
    }

    // Decoded here rather than with URLDecoder, which silently replaces the bytes that are not
    // valid UTF-8 with U+FFFD
    int length = value.length();
    StringBuilder builder = new StringBuilder(length);
    byte[] bytes = null;
    int i = 0;
    while (i < length) {
      char ch = value.charAt(i);
      if (ch == '+') {
        builder.append(' ');
        i++;
      } else if (ch == '%') {
        // The consecutive escaped bytes are decoded together, a character can be several bytes
        if (bytes == null) {
          bytes = new byte[(length - i) / 3];
        }
        int count = 0;
        while (i < length && value.charAt(i) == '%') {
          if (i + 2 >= length) {
            throw new ExpressionException(ErrorCode.TRUNCATED_DECODING_SEQUENCE, "URL");
          }
          bytes[count++] = (byte) (hexDigit(value, i + 1) << 4 | hexDigit(value, i + 2));
          i += 3;
        }
        builder.append(Characters.decodeUtf8(bytes, 0, count, "URL"));
      } else {
        builder.append(ch);
        i++;
      }
    }
    return builder.toString();
  }

  private static int hexDigit(final String value, final int index) {
    int digit = Character.digit(value.charAt(index), 16);
    if (digit < 0) {
      throw new ExpressionException(
          ErrorCode.INVALID_DECODING_SYMBOL, Character.toString(value.codePointAt(index)), "URL");
    }
    return digit;
  }
}
