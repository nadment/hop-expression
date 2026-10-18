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
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Converts an encoded string using the Java string literal encoding format. Special characters are
 * \b, \t, \n, \f, \r, \", \\, \\000, \\u0000
 *
 * @see StringEncodeFunction
 */
@FunctionPlugin
@NullMarked
public class StringDecodeFunction extends Function {

  public StringDecodeFunction() {
    super(
        "STRING_DECODE",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING,
        OperatorCategory.STRING,
        "/docs/string_decode.html");
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null) return null;

    // Fast path, nothing to decode
    int index = value.indexOf('\\');
    if (index < 0) {
      return value;
    }

    // Decoded here rather than with StringEscapeUtils.unescapeJava(), which silently drops the
    // backslash of an unknown escape sequence and of a trailing backslash
    int length = value.length();
    StringBuilder builder = new StringBuilder(length);
    builder.append(value, 0, index);
    while (index < length) {
      char ch = value.charAt(index++);
      if (ch != '\\') {
        builder.append(ch);
        continue;
      }
      if (index == length) {
        throw invalidSequence("\\");
      }
      ch = value.charAt(index++);
      switch (ch) {
        case 'b' -> builder.append('\b');
        case 't' -> builder.append('\t');
        case 'n' -> builder.append('\n');
        case 'f' -> builder.append('\f');
        case 'r' -> builder.append('\r');
        case '"', '\'', '\\' -> builder.append(ch);
        case 'u' -> {
          // Several 'u' are allowed, like in a Java source
          while (index < length && value.charAt(index) == 'u') {
            index++;
          }
          if (index + 4 > length) {
            throw invalidSequence("\\u" + value.substring(index));
          }
          int code = 0;
          for (int end = index + 4; index < end; index++) {
            int digit = Character.digit(value.charAt(index), 16);
            if (digit < 0) {
              throw invalidSequence("\\u" + value.substring(end - 4, end));
            }
            code = (code << 4) | digit;
          }
          builder.append((char) code);
        }
        case '0', '1', '2', '3', '4', '5', '6', '7' -> {
          // Up to 3 octal digits, the third one only if the code stays below \377
          int code = ch - '0';
          int max = (ch <= '3') ? 2 : 1;
          for (int i = 0; i < max && index < length && isOctalDigit(value.charAt(index)); i++) {
            code = (code << 3) | (value.charAt(index++) - '0');
          }
          builder.append((char) code);
        }
        default -> throw invalidSequence("\\" + Character.toString(value.codePointAt(index - 1)));
      }
    }
    return builder.toString();
  }

  private static boolean isOctalDigit(char ch) {
    return ch >= '0' && ch <= '7';
  }

  private static ExpressionException invalidSequence(String sequence) {
    return new ExpressionException(ErrorCode.INVALID_DECODING_SYMBOL, sequence, "Java string");
  }
}
