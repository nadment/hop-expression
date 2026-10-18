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

import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Encodes special characters in a string using the Java string literal encoding format.
 *
 * @see StringDecodeFunction
 */
@FunctionPlugin
@NullMarked
public class StringEncodeFunction extends Function {

  private static final char[] HEX = "0123456789ABCDEF".toCharArray();

  public StringEncodeFunction() {
    super(
        "STRING_ENCODE",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING,
        OperatorCategory.STRING,
        "/docs/string_encode.html");
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null) return null;

    // Fast path, nothing to encode
    int index = 0;
    int length = value.length();
    while (index < length && !isEscaped(value.charAt(index))) {
      index++;
    }
    if (index == length) {
      return value;
    }

    // The escape sequences decoded by StringDecodeFunction
    StringBuilder builder = new StringBuilder(length + 16);
    builder.append(value, 0, index);
    for (; index < length; index++) {
      char ch = value.charAt(index);
      switch (ch) {
        case '\b' -> builder.append("\\b");
        case '\t' -> builder.append("\\t");
        case '\n' -> builder.append("\\n");
        case '\f' -> builder.append("\\f");
        case '\r' -> builder.append("\\r");
        case '"', '\\' -> builder.append('\\').append(ch);
        default -> {
          if (isEscaped(ch)) {
            // A control or a non-ASCII character, a surrogate pair is encoded as two escapes
            builder.append("\\u").append(HEX[(ch >> 12) & 0xF]).append(HEX[(ch >> 8) & 0xF]);
            builder.append(HEX[(ch >> 4) & 0xF]).append(HEX[ch & 0xF]);
          } else {
            builder.append(ch);
          }
        }
      }
    }
    return builder.toString();
  }

  /** Returns whether the character is encoded, a control, a non-ASCII character, '"' or '\'. */
  private static boolean isEscaped(char ch) {
    return ch < 32 || ch > 0x7F || ch == '"' || ch == '\\';
  }
}
