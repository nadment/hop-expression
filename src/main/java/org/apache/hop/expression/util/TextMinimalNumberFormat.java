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
package org.apache.hop.expression.util;

import java.math.BigDecimal;
import org.apache.hop.expression.ExpressionException;

final class TextMinimalNumberFormat extends NumberFormat {

  // Original format
  private final String format;

  // Case of exponent
  private char e;

  public TextMinimalNumberFormat(final String pattern) {

    this.e = 0;
    this.format = pattern;

    // TM9 or TMR, TMe
    if (pattern.length() > 2) {
      char c = pattern.charAt(2);

      // Preserve case for exponent case 'E' or 'e'
      if (c == 'e' || c == 'E') {
        this.e = c;
      }
    }
  }

  @Override
  public String format(final BigDecimal number) {

    // Text-minimal number in scientific notation
    if (e > 0) {
      int power = number.precision() - number.scale() - 1;
      BigDecimal value = number.movePointLeft(power);

      // Case of exponent
      return value.toPlainString()
          + e
          + (power < 0 ? '-' : '+')
          + (Math.abs(power) < 10 ? "0" : "")
          + Math.abs(power);
    }

    String s = number.stripTrailingZeros().toPlainString();
    // TODO: To be compatible with SQL
    // if (s.startsWith("0.")) {
    // we want ".1" not "0.1"
    // return s.substring(1);
    // } else if (s.startsWith("-0.")) {
    // we want "-.1" not "-0.1"
    // return "-" + s.substring(2);
    // }
    return s;
  }

  @Override
  public BigDecimal parse(String text) throws ExpressionException {

    int start = 0; // first not white space symbol
    int position = 0; // first not white space symbol
    try {
      int end = text.length(); // length of parsed string

      // Skip start space
      while (position < end && Characters.isSpace(text.charAt(position))) position++;

      // Skip end space
      while (position < end && Characters.isSpace(text.charAt(end - 1))) end--;

      StringBuilder builder = new StringBuilder();
      char previous = '_';
      while (position < end) {
        char c = text.charAt(position);

        // Consecutive underscore is not allowed
        if (c == '_') {
          if (c == previous) {
            createUnparsableNumber(format, text, position);
          }
          position++;
          continue;
        }
        previous = c;
        builder.append(c);
        position++;
      }

      // Last char should not be an underscore
      if (previous == '_') {
        createUnparsableNumber(format, text, position);
      }

      // Text-minimal number in scientific notation
      //      if (e > 0) {
      //        return new BigDecimal(builder.toString());
      //      }

      // Text-minimal number
      return new BigDecimal(builder.toString());
    } catch (Exception exception) {
      throw createUnparsableNumber(format, text, start);
    }
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (this == obj) {
      return true;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    TextMinimalNumberFormat other = (TextMinimalNumberFormat) obj;
    return format.equals(other.format);
  }

  @Override
  public int hashCode() {
    return format.hashCode();
  }

  @Override
  public String toString() {
    return format;
  }
}
