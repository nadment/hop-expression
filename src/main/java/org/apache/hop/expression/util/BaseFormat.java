/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.apache.hop.expression.util;

import java.text.ParsePosition;
import org.apache.hop.expression.IExpression;

public abstract class BaseFormat {

  protected BaseFormat() {
    super();
  }

  protected static final Class<?> PKG = IExpression.class; // for i18n purposes

  protected static boolean startsWithIgnoreCase(String str, int offset, String prefix) {
    if (prefix.length() > str.length() - offset) {
      return false;
    }

    return str.regionMatches(true, offset, prefix, 0, prefix.length());
  }

  protected static boolean startsWithIgnoreCase(String str, int offset, String... substrings) {
    for (String substring : substrings) {
      if (str.regionMatches(true, offset, substring, 0, substring.length())) return true;
    }
    return false;
  }

  protected static boolean endsWithIgnoreCase(String str, String suffix) {
    if (suffix.length() > str.length()) {
      return false;
    }
    int offset = str.length() - suffix.length();
    return str.regionMatches(true, offset, suffix, 0, suffix.length());
  }

  /**
   * Parse an integer at the given position in a string
   *
   * @param value the string to parse
   * @param position the start index for the integer in the string
   * @param length number of digits to parse in the string
   * @return the int
   * @throws NumberFormatException if the value is not a number
   */
  protected static int parseInt(String value, ParsePosition position, int length)
      throws NumberFormatException {
    int index = position.getIndex();
    int result = 0;
    if (index + length > value.length()) length = value.length() - index;

    for (int i = 0; i < length; i++) {
      int digit = Character.digit(value.charAt(index), 10);
      if (digit < 0) {
        if (index == position.getIndex()) {
          position.setErrorIndex(index);
          throw new NumberFormatException("Invalid number: " + value);
        }
        break;
      }

      index++;
      result *= 10;
      result += digit;
    }

    position.setIndex(index);

    return result;
  }

  /**
   * Parse an integer at the given position in a string
   *
   * @param value the string to parse
   * @param position the start index for the integer in the string
   * @param length number of digits to parse in the string
   * @return the signed int
   * @throws NumberFormatException if the value is not a number
   */
  protected static int parseSignedInt(String value, ParsePosition position, int length)
      throws NumberFormatException {
    int index = position.getIndex();
    int result = 0;
    if (index + length > value.length()) length = value.length() - index;

    char sign = value.charAt(index);
    if (sign == '-' || sign == '+') {
      index++;
      length--;
    } else {
      sign = '+';
    }

    for (int i = 0; i < length; i++) {
      int digit = Character.digit(value.charAt(index++), 10);
      if (digit < 0) {
        break;
        // position.setErrorIndex(index);
        // throw new NumberFormatException(
        // "Invalid number: " + value.substring(position.getIndex(), position.getIndex() + i));
      }
      result *= 10;
      result += digit;
    }

    position.setIndex(index);

    return (sign == '-') ? -result : result;
  }

  protected static String parseString(String value, ParsePosition position, String... substrings) {
    int index = position.getIndex();
    for (String substring : substrings) {
      if (value.regionMatches(true, index, substring, 0, substring.length())) {
        position.setIndex(index + substring.length());
        return substring;
      }
    }

    position.setErrorIndex(index);

    return null;
  }

  /**
   * Returns a capitalization strategy if the specified string contains any of the specified
   * substrings at the specified index. The capitalization strategy indicates the casing of the
   * substring that was found. If none of the specified substrings are found, this method returns
   * <code>null</code> .
   *
   * @param s the string to check
   * @param index the index to check at
   * @param substrings the substrings to check for within the string
   * @return a capitalization strategy if the specified string contains any of the specified
   *     substrings at the specified index, <code>null</code> otherwise
   */
  protected static Capitalization match(String s, int index, String... substrings) {
    for (String substring : substrings) {
      if (index + substring.length() <= s.length()) {
        boolean found = true;
        Boolean up1 = null;
        Boolean up2 = null;
        for (int i = 0; i < substring.length(); i++) {
          char c1 = s.charAt(index + i);
          char c2 = substring.charAt(i);
          if (c1 != c2 && Character.toUpperCase(c1) != Character.toUpperCase(c2)) {
            found = false;
            break;
          } else if (Character.isLetter(c1)) {
            if (up1 == null) {
              up1 = Character.isUpperCase(c1);
            } else if (up2 == null) {
              up2 = Character.isUpperCase(c1);
            }
          }
        }
        if (found) {
          return Capitalization.of(up1, up2);
        }
      }
    }
    return null;
  }
}
