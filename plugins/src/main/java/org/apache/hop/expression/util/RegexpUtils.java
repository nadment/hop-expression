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

import org.apache.hop.expression.Error;
import org.apache.hop.expression.ExpressionException;
import java.util.regex.Pattern;

public class RegexpUtils {
  private static final String JAVA_REGEX_SPECIALS = "\\.[]{}()<>*+-=!?^$|";

  /** Translates a LIKE pattern to Java regex pattern, with optional escape string. */
  public static String toRegexLike(String pattern, CharSequence escapeStr) throws ExpressionException {
    final char escapeChar;
    if (escapeStr != null) {

      if (escapeStr.length() != 1) {
        throw new ExpressionException(Error.ILLEGAL_ARGUMENT, escapeStr.toString());
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
      if (JAVA_REGEX_SPECIALS.indexOf(c) >= 0) {
        javaPattern.append('\\');
      }

      if (c == escapeChar) {
        if (i == (pattern.length() - 1)) {
          throw new ExpressionException(Error.INVALID_REGEXP_ESCAPE, pattern, i);
        }
        char nextChar = pattern.charAt(i + 1);
        if ((nextChar == '_') || (nextChar == '%') || (nextChar == escapeChar)) {
          if (JAVA_REGEX_SPECIALS.indexOf(nextChar) >= 0) {
            javaPattern.append('\\');
          }
          javaPattern.append(nextChar);
          i++;
        } else {
          throw new ExpressionException(Error.INVALID_REGEXP_ESCAPE, pattern, i);
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
  
  
  public static int parseFlags(String str) throws ExpressionException {
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
          case 'n':
            flags |= Pattern.DOTALL;
            break;
          case 'm':
            flags |= Pattern.MULTILINE;
            break;
          default:
            throw new ExpressionException(Error.ILLEGAL_ARGUMENT, str);
        }
      }
    }
    return flags;
  }
}

