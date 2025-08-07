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

import java.util.regex.Pattern;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;

public class Regexp {
  private static final String JAVA_REGEXP_SPECIALS = "\\.[]{}()<>*+-=!?^$|";

  /** Private constructor since this is a utility class. */
  private Regexp() {}

  /** Translates a LIKE pattern to Java regexp pattern, with optional escape string. */
  public static String toRegexLike(final String pattern, final CharSequence escapeStr)
      throws ExpressionException {
    final char escapeChar;
    if (escapeStr != null) {

      if (escapeStr.length() != 1) {
        throw new ExpressionException(ErrorCode.INVALID_ARGUMENT, escapeStr);
      }

      escapeChar = escapeStr.charAt(0);
    } else {
      escapeChar = 0;
    }
    return toRegexLike(pattern, escapeChar);
  }

  /** Translates a LIKE pattern to Java regex pattern. */
  public static String toRegexLike(final String pattern, final char escapeChar)
      throws ExpressionException {
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
          throw new ExpressionException(ErrorCode.INVALID_REGEXP_ESCAPE, pattern, i);
        }
        char nextChar = pattern.charAt(i + 1);
        if ((nextChar == '_') || (nextChar == '%') || (nextChar == escapeChar)) {
          if (JAVA_REGEXP_SPECIALS.indexOf(nextChar) >= 0) {
            javaPattern.append('\\');
          }
          javaPattern.append(nextChar);
          i++;
        } else {
          throw new ExpressionException(ErrorCode.INVALID_REGEXP_ESCAPE, pattern, i);
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
            throw new IllegalArgumentException(ErrorCode.INVALID_ARGUMENT.message(str));
        }
      }
    }
    return flags;
  }

  /** Translates a SIMILAR TO pattern to Java regex pattern. */
  public static String toSimilarTo(final String pattern, final char escapeChar)
      throws ExpressionException {
    final int len = pattern.length();
    final StringBuilder javaPattern = new StringBuilder(len + len);
    for (int i = 0; i < len; i++) {
      char c = pattern.charAt(i);
      if (c == escapeChar) {
        if (i == (pattern.length() - 1)) {
          throw new ExpressionException(ErrorCode.INVALID_REGEXP_ESCAPE, pattern, i);
        }
        char nextChar = pattern.charAt(i + 1);
        if ((nextChar == '_') || (nextChar == '%') || (nextChar == escapeChar)) {
          if (JAVA_REGEXP_SPECIALS.indexOf(nextChar) >= 0) {
            javaPattern.append('\\');
          }
          javaPattern.append(nextChar);
          i++;
        } else {
          throw new ExpressionException(ErrorCode.INVALID_REGEXP_ESCAPE, pattern, i);
        }
      } else if (c == '_') {
        javaPattern.append('.');
      } else if (c == '%') {
        javaPattern.append("(?s:.*)");
      } else if (c == '[') {
        if (i == (len - 1)) {
          throw new ExpressionException(ErrorCode.INVALID_REGEXP_ESCAPE, pattern, i);
        }
        char nextChar = pattern.charAt(i + 1);
        if (nextChar == ':') {
          int end = pattern.indexOf(']', i + 1);
          if (end < 0) {
            throw new ExpressionException(ErrorCode.INVALID_REGEXP_PATTERN, pattern, i);
          }

          String cls = pattern.substring(i + 2, end - 1).toLowerCase();
          // Alphabetic characters
          if ("alpha".equals(cls)) javaPattern.append("\\p{Alpha}");
          // Alphanumeric characters
          else if ("alnum".equals(cls)) javaPattern.append("\\p{Alnum}");
          else if ("cntrl".equals(cls)) javaPattern.append("\\p{Cntrl}");
          // Blank space and tab characters
          else if ("blank".equals(cls)) javaPattern.append("\\h");
          // Punctuation and symbols characters
          else if ("punct".equals(cls)) javaPattern.append("\\p{Punct}");
          // Numeric digits
          else if ("digit".equals(cls)) javaPattern.append("\\d");
          // Hexadecimal digits
          else if ("xdigit".equals(cls)) javaPattern.append("\\p{XDigit}");
          // All whitespace characters, including line breaks
          else if ("space".equals(cls)) javaPattern.append("\\s");
          // Uppercase letters
          else if ("upper".equals(cls)) javaPattern.append("\\u");
          // Lowercase letters
          else if ("lower".equals(cls)) javaPattern.append("\\l");
          // Word characters (letters, numbers and underscores)
          else if ("word".equals(cls)) javaPattern.append("\\w");

          i = end;
        } else javaPattern.append(c);
      } else {
        javaPattern.append(c);
      }
    }
    return javaPattern.toString();
  }

  /**
   * Simplifies like string with escape. A like '%%#%%A%%' escape '#' should simplify to A like
   * '%#%%A%' escape '#'.
   */
  public static String simplifyLikeString(String content, char escape, char wildcard) {
    int escapeCount = 0;
    int wildcardCount = 0;
    StringBuilder builder = new StringBuilder();
    for (int index = 0; index < content.length(); index++) {
      char c = content.charAt(index);
      if (c == escape) {
        builder.append(c);
        escapeCount++;
        wildcardCount = 0;
        continue;
      }
      if (c == wildcard) {
        if (escapeCount % 2 == 1) {
          builder.append(wildcard);
        } else if (wildcardCount == 0) {
          builder.append(wildcard);
          wildcardCount++;
        }
        escapeCount = 0;
        continue;
      }
      builder.append(c);
      escapeCount = 0;
      wildcardCount = 0;
    }
    return builder.toString();
  }
}
