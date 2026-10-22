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

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.text.StringEscapeUtils;
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
 * The function decode special HTML entities.
 *
 * @see HtmlEncodeFunction
 */
@FunctionPlugin
@NullMarked
public class HtmlDecodeFunction extends Function {

  /** A decimal or hexadecimal numeric entity, the semicolon is required like by the decoder. */
  private static final Pattern NUMERIC_ENTITY = Pattern.compile("&#([xX][0-9a-fA-F]+|\\d+);");

  public HtmlDecodeFunction() {
    super(
        "HTML_DECODE",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING,
        OperatorCategory.STRING,
        "/docs/html_decode.html");
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null) return null;

    try {
      return StringEscapeUtils.unescapeHtml4(value);
    } catch (IllegalArgumentException e) {
      // A numeric entity above U+10FFFF is not a character, report the first one
      Matcher matcher = NUMERIC_ENTITY.matcher(value);
      while (matcher.find()) {
        if (!isValidCodePoint(matcher.group(1))) {
          throw new ExpressionException(ErrorCode.INVALID_DECODING_SYMBOL, matcher.group(), "HTML");
        }
      }
      throw new ExpressionException(ErrorCode.INVALID_DECODING_SYMBOL, value, "HTML");
    }
  }

  /**
   * Returns whether the number of a numeric entity is a valid code point. A number that does not
   * fit in an int is left unchanged by the decoder, so it is considered valid.
   */
  private static boolean isValidCodePoint(String number) {
    try {
      boolean hex = number.charAt(0) == 'x' || number.charAt(0) == 'X';
      int codePoint =
          hex ? Integer.parseInt(number, 1, number.length(), 16) : Integer.parseInt(number);
      return Character.isValidCodePoint(codePoint);
    } catch (NumberFormatException e) {
      return true;
    }
  }
}
