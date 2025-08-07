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

public class Characters {

  private static final byte[] FLAGS = new byte[256];
  private static final byte IS_DIGIT = 0x01;
  private static final byte IS_HEX_DIGIT = 0x02;
  private static final byte IS_ALPHA = 0x04;
  private static final byte IS_DELIMITER = 0x08;
  private static final byte IS_OCT_DIGIT = 0x10;
  private static final byte IS_BIT = 0x20;

  static {
    FLAGS['0'] |= IS_BIT;
    FLAGS['1'] |= IS_BIT;

    for (int ch = '0'; ch <= '9'; ch++) {
      FLAGS[ch] |= IS_DIGIT | IS_HEX_DIGIT;
    }
    for (int ch = '0'; ch <= '7'; ch++) {
      FLAGS[ch] |= IS_OCT_DIGIT;
    }
    for (int ch = 'A'; ch <= 'F'; ch++) {
      FLAGS[ch] |= IS_HEX_DIGIT;
    }
    for (int ch = 'a'; ch <= 'f'; ch++) {
      FLAGS[ch] |= IS_HEX_DIGIT;
    }
    for (int ch = 'A'; ch <= 'Z'; ch++) {
      FLAGS[ch] |= IS_ALPHA;
    }
    for (int ch = 'a'; ch <= 'z'; ch++) {
      FLAGS[ch] |= IS_ALPHA;
    }

    char[] delimiters = {
      ' ', '.', ',', ':', ';', '\t', '\'', '?', '!', '@', '"', '^', '#', '$', '&', '~', '_', '=',
      '+', '-', '*', '%', '/', '|', '\\', '[', ']', '(', ')', '{', '}', '<', '>'
    };
    for (int ch : delimiters) {
      FLAGS[ch] |= IS_DELIMITER;
    }
  }

  /** Private constructor since this is a utility class. */
  private Characters() {}

  /**
   * Determines if the specified character is a digit.
   *
   * @param ch the character to check
   */
  public static boolean isDigit(char ch) {
    if (ch > 255) {
      return false;
    }
    return (FLAGS[ch] & IS_DIGIT) != 0;
  }

  public static boolean isBitDigit(char ch) {
    if (ch > 255) {
      return false;
    }
    return (FLAGS[ch] & IS_BIT) != 0;
  }

  public static boolean isHexDigit(char ch) {
    if (ch > 255) {
      return false;
    }
    return (FLAGS[ch] & IS_HEX_DIGIT) != 0;
  }

  public static boolean isOctDigit(char ch) {
    if (ch > 255) {
      return false;
    }
    return (FLAGS[ch] & IS_OCT_DIGIT) != 0;
  }

  /**
   * Determines if the specified character is alphabetic
   *
   * @param ch the character to check
   */
  public static boolean isAlpha(char ch) {
    if (ch > 255) {
      return false;
    }
    return (FLAGS[ch] & IS_ALPHA) != 0;
  }

  /**
   * Determines if the specified character is alphabetic or digit
   *
   * @param ch the character to check
   */
  public static boolean isAlphaOrDigit(char ch) {
    if (ch > 255) {
      return false;
    }
    return (FLAGS[ch] & (IS_ALPHA | IS_DIGIT)) != 0;
  }

  /**
   * Is the character a word delimiter
   *
   * <p>! ? @ " ^ # $ & ~ _ , . : ; = + - * % / | \ [ ] ( ) { } < >
   *
   * @param ch the character to check
   * @return true if it is a delimiter
   */
  public static boolean isWordDelimiter(char ch) {
    if (ch > 255) {
      return false;
    }
    return (FLAGS[ch] & (IS_DELIMITER)) != 0;
  }

  /**
   * Determines if the specified character is space or tab
   *
   * @param ch the character to check
   */
  public static boolean isSpace(char ch) {
    return ch == ' '
        || ch == '\t'
        || ch == '\n'
        || ch == '\r'
        // No-break space
        || ch == '\u00a0'
        // Narrow no-break space
        || ch == '\u202f';
  }

  public static boolean isExponent(char ch) {
    return ch == 'e' || ch == 'E';
  }
}
