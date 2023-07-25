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

import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.ExpressionException;
import java.util.regex.Pattern;

public class Regexp {
  private static final String JAVA_REGEXP_SPECIALS = "\\.[]{}()<>*+-=!?^$|";

  /**
   * Private constructor since this is a utility class.
   */
  private Regexp() {}

  /** Translates a LIKE pattern to Java regexp pattern, with optional escape string. */
  public static String toRegexLike(String pattern, CharSequence escapeStr)
      throws ExpressionException {
    final char escapeChar;
    if (escapeStr != null) {

      if (escapeStr.length() != 1) {
        throw new ExpressionException(ExpressionError.ILLEGAL_ARGUMENT, escapeStr);
      }

      escapeChar = escapeStr.charAt(0);
    } else {
      escapeChar = 0;
    }
    return toRegexLike(pattern, escapeChar);
  }

  /** Translates a LIKE pattern to Java regex pattern. */
  public static String toRegexLike(String pattern, char escapeChar) throws ExpressionException {
    int i;
    final int len = pattern.length();
    final StringBuilder javaPattern = new StringBuilder(len + len);
    for (i = 0; i < len; i++) {
      char c = pattern.charAt(i);
      if (JAVA_REGEXP_SPECIALS.indexOf(c) >= 0) {
        javaPattern.append('\\');
      }

      if (c == escapeChar) {
        if (i == (pattern.length() - 1)) {
          throw new ExpressionException(ExpressionError.INVALID_REGEXP_ESCAPE, pattern, i);
        }
        char nextChar = pattern.charAt(i + 1);
        if ((nextChar == '_') || (nextChar == '%') || (nextChar == escapeChar)) {
          if (JAVA_REGEXP_SPECIALS.indexOf(nextChar) >= 0) {
            javaPattern.append('\\');
          }
          javaPattern.append(nextChar);
          i++;
        } else {
          throw new ExpressionException(ExpressionError.INVALID_REGEXP_ESCAPE, pattern, i);
        }
      } else if (c == '_') {
        javaPattern.append('.');
      } else if (c == '%') {
        javaPattern.append("(?s:.*)");
      } else {
        javaPattern.append(c);
      }
    }
    return javaPattern.toString();
  }


  public static int parseFlags(String str) {
    int flags = Pattern.UNICODE_CASE;
    if (str != null) {
      for (int i = 0; i < str.length(); ++i) {
        switch (str.charAt(i)) {
          // Enables case-insensitive matching
          case 'i':
            flags |= Pattern.CASE_INSENSITIVE;
            break;
          // Enables case-sensitive matching
          case 'c':
            flags &= ~Pattern.CASE_INSENSITIVE;
            break;
          // Enables dotall mode, that allows the period (.) to match the newline character.
          case 'n':
            flags |= Pattern.DOTALL;
            break;
          // Enables multiline mode.
          case 'm':
            flags |= Pattern.MULTILINE;
            break;
          default:
            throw new IllegalArgumentException(ExpressionError.ILLEGAL_ARGUMENT.message(str));
        }
      }
    }
    return flags;
  }

  /** Translates a SIMILAR TO pattern to Java regex pattern. */
  public static String toSimilarTo(String pattern, char escapeChar) throws ExpressionException {
    final int len = pattern.length();
    final StringBuilder javaPattern = new StringBuilder(len + len);
    for (int i = 0; i < len; i++) {
      char c = pattern.charAt(i);
      if (c == escapeChar) {
        if (i == (pattern.length() - 1)) {
          throw new ExpressionException(ExpressionError.INVALID_REGEXP_ESCAPE, pattern, i);
        }
        char nextChar = pattern.charAt(i + 1);
        if ((nextChar == '_') || (nextChar == '%') || (nextChar == escapeChar)) {
          if (JAVA_REGEXP_SPECIALS.indexOf(nextChar) >= 0) {
            javaPattern.append('\\');
          }
          javaPattern.append(nextChar);
          i++;
        } else {
          throw new ExpressionException(ExpressionError.INVALID_REGEXP_ESCAPE, pattern, i);
        }
      } else if (c == '_') {
        javaPattern.append('.');
      } else if (c == '%') {
        javaPattern.append("(?s:.*)");
      } else if (c == '[') {
        if (i == (len - 1)) {
          throw new ExpressionException(ExpressionError.INVALID_REGEXP_ESCAPE, pattern, i);
        }
        char nextChar = pattern.charAt(i + 1);
        if (nextChar == ':') {
          int end = pattern.indexOf(']', i + 1);
          if (end < 0) {
            throw new ExpressionException(ExpressionError.INVALID_REGEXP_PATTERN, pattern, i);
          }
          
          String cls = pattern.substring(i + 2, end - 1).toLowerCase();
          // Alphabetic characters
          if ("alpha".equals(cls))
            javaPattern.append("\\p{Alpha}");
          // Alphanumeric characters
          else if ("alnum".equals(cls))
            javaPattern.append("\\p{Alnum}");
          else if ("cntrl".equals(cls))
            javaPattern.append("\\p{Cntrl}");
          // Blank space and tab characters
          else if ("blank".equals(cls))
            javaPattern.append("\\h");
          // Punctuation and symbols characters
          else if ("punct".equals(cls))
            javaPattern.append("\\p{Punct}");
          // Numeric digits
          else if ("digit".equals(cls))
            javaPattern.append("\\d");
          // Hexadecimal digits
          else if ("xdigit".equals(cls))
            javaPattern.append("\\p{XDigit}");
          // All whitespace characters, including line breaks
          else if ("space".equals(cls))
            javaPattern.append("\\s");
          // Uppercase letters
          else if ("upper".equals(cls))
            javaPattern.append("\\u");
          // Lowercase letters
          else if ("lower".equals(cls))
            javaPattern.append("\\l");
          // Word characters (letters, numbers and underscores)
          else if ("word".equals(cls))
            javaPattern.append("\\w");
          
          i = end;
        }
        else
          javaPattern.append(c);
      } else {
        javaPattern.append(c);
      }
    }
    return javaPattern.toString();
  }
}

