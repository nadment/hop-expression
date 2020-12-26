/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression.util;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.regex.Pattern;

import org.apache.commons.math3.util.FastMath;
import org.apache.hop.expression.Expression;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.i18n.BaseMessages;

public class Functions {

  protected static final Class<?> PKG = Expression.class; // for i18n purposes

  private static final char[] HEX = "0123456789abcdef".toCharArray();

  /** The maximum size to which the padding can expand. */
  private static final int PAD_LIMIT = 8192;

  private static final char[] SOUNDEX = new char[128];
  private static final int SOUNDEX_LENGTH = 4;

  static {
    // SOUNDEX_INDEX
    String index = "7AEIOUY8HW1BFPV2CGJKQSXZ3DT4L5MN6R";
    char number = 0;
    for (int i = 0, length = index.length(); i < length; i++) {
      char c = index.charAt(i);
      if (c < '9') {
        number = c;
      } else {
        SOUNDEX[c] = number;
        SOUNDEX[Character.toLowerCase(c)] = number;
      }
    }
  }

  private Functions() {
    // utility class
  }

  public static String soundex(String s) {
    int len = s.length();
    char[] chars = {'0', '0', '0', '0'};
    char lastDigit = '0';
    for (int i = 0, j = 0; i < len && j < 4; i++) {
      char c = s.charAt(i);
      char newDigit = c > SOUNDEX.length ? 0 : SOUNDEX[c];
      if (newDigit != 0) {
        if (j == 0) {
          chars[j++] = c;
          lastDigit = newDigit;
        } else if (newDigit <= '6') {
          if (newDigit != lastDigit) {
            chars[j++] = newDigit;
            lastDigit = newDigit;
          }
        } else if (newDigit == '7') {
          lastDigit = newDigit;
        }
      }
    }
    return new String(chars);
  }

  public static int difference(String s0, String s1) {
    String result0 = soundex(s0);
    String result1 = soundex(s1);
    for (int i = 0; i < SOUNDEX_LENGTH; i++) {
      if (result0.charAt(i) != result1.charAt(i)) {
        return i;
      }
    }
    return SOUNDEX_LENGTH;
  }

  public static String lpad(String str, int length, String pad) {

    if (length < 0) {
      length = 0;
    } else if (length > PAD_LIMIT) {
      new ExpressionException("Paddind length exceeds maximum limit: " + PAD_LIMIT);
    }

    // If this parameter is omitted, the function will pad spaces
    if (pad == null) {
      pad = " ";
    }

    final int size = pad.length();
    final int index = length - str.length();

    if (index <= 0) {
      return str.substring(0, length);
    } else if (size == 0) {
      // nothing to do
    } else if (index == size) {
      return pad.concat(str);
    } else if (index < size) {
      return pad.substring(0, index).concat(str);
    } else {
      final char[] padding = new char[index];
      final char[] padChars = pad.toCharArray();
      for (int i = 0; i < index; i++) {
        padding[i] = padChars[i % size];
      }
      str = new String(padding).concat(str);
    }

    return str;
  }

  public static String rpad(String str, int length, String pad) {

    if (length < 0) {
      length = 0;
    }
    if (length > PAD_LIMIT) {
      new ExpressionException("Paddind length exceeds maximum limit: " + PAD_LIMIT);
    }

    // If this parameter is omitted, the function will pad spaces
    if (pad == null) {
      pad = " ";
    }

    final int size = pad.length();
    final int index = length - str.length();

    if (index <= 0) {
      return str.substring(0, length);
    } else if (size == 0) {
      // nothing to do
    } else if (index == size) {
      return str.concat(pad);
    } else if (index < size) {
      return str.concat(pad.substring(0, index));
    } else {
      final char[] padding = new char[index];
      final char[] padChars = pad.toCharArray();
      for (int i = 0; i < index; i++) {
        padding[i] = padChars[i % size];
      }
      str = str.concat(new String(padding));
    }

    return str;
  }

  public static BigDecimal truncate(BigDecimal b0, int b1) {
    return b0.movePointRight(b1).setScale(0, RoundingMode.DOWN).movePointLeft(b1);
  }

  public static double acosh(double x) {
    // return Math.log(x + Math.sqrt(x*x - 1.0d));
    return FastMath.acosh(x);
  }

  public static double asinh(double x) {
    // return Math.log(x + Math.sqrt(1 + x * x));
    return FastMath.asinh(x);
  }

  public static double atanh(double x) {
    // return Math.log(Math.sqrt(1 + x) / Math.sqrt(1 - x));
    return FastMath.atanh(x);
  }

  private static int makeRegexpFlags(String stringFlags) {
    int flags = 0;
    if (stringFlags != null) {
      for (int i = 0; i < stringFlags.length(); ++i) {
        switch (stringFlags.charAt(i)) {
          case 'i':
            flags |= Pattern.CASE_INSENSITIVE;
            break;
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
            throw new ExpressionException("Invalid input for Regexp");
        }
      }
    }
    return flags;
  }

  public static String left(String s, int count) {
    if (count < 0) {
      count = 0;
      // throw new ExpressionException("LEFT: Length must be greater than or equal to 0");
    } else if (count > s.length()) {
      count = s.length();
    }
    return s.substring(0, count);
  }

  public static String right(String s, int count) {
    if (count < 0) {
      count = 0;
      // throw new ExpressionException("RIGHT: Length must be greater than or equal to 0");
    } else if (count > s.length()) {
      count = s.length();
    }
    return s.substring(s.length() - count);
  }

  private static ExpressionException createFormatException(String s, int i) {
    return new ExpressionException(
        BaseMessages.getString(PKG, "Bad format {0} at position {1}", s, i));
  }

  /**
   * Decode a text that is encoded as a Java string literal. The Java properties file format and
   * Java source code format is supported.
   *
   * @param s the encoded string
   * @return the string
   */
  public static String stringDecode(String s) {
    int length = s.length();
    StringBuilder buff = new StringBuilder(length);
    for (int i = 0; i < length; i++) {
      char c = s.charAt(i);
      if (c == '\\') {
        if (i + 1 >= s.length()) {
          throw createFormatException(s, i);
        }
        c = s.charAt(++i);
        switch (c) {
          case 't':
            buff.append('\t');
            break;
          case 'r':
            buff.append('\r');
            break;
          case 'n':
            buff.append('\n');
            break;
          case 'b':
            buff.append('\b');
            break;
          case 'f':
            buff.append('\f');
            break;
          case '#':
            // for properties files
            buff.append('#');
            break;
          case '=':
            // for properties files
            buff.append('=');
            break;
          case ':':
            // for properties files
            buff.append(':');
            break;
          case '"':
            buff.append('"');
            break;
          case '\\':
            buff.append('\\');
            break;
          case 'u':
            {
              try {
                c = (char) (Integer.parseInt(s.substring(i + 1, i + 5), 16));
              } catch (NumberFormatException e) {
                throw createFormatException(s, i);
              }
              i += 4;
              buff.append(c);
              break;
            }
          default:
            if (c >= '0' && c <= '9') {
              try {
                c = (char) (Integer.parseInt(s.substring(i, i + 3), 8));
              } catch (NumberFormatException e) {
                throw createFormatException(s, i);
              }
              i += 2;
              buff.append(c);
            } else {
              throw createFormatException(s, i);
            }
        }
      } else {
        buff.append(c);
      }
    }
    return buff.toString();
  }

  /**
   * Convert a string to a Java literal using the correct escape sequences. The literal is not
   * enclosed in double quotes. The result can be used in properties files or in Java source code.
   *
   * @param s the text to convert
   */
  public static String stringEncode(String s) {
    StringBuilder builder = new StringBuilder(s.length());
    int length = s.length();
    for (int i = 0; i < length; i++) {
      char c = s.charAt(i);
      switch (c) {
        case '\t':
          // HT horizontal tab
          builder.append("\\t");
          break;
        case '\n':
          // LF linefeed
          builder.append("\\n");
          break;
        case '\f':
          // FF form feed
          builder.append("\\f");
          break;
        case '\r':
          // CR carriage return
          builder.append("\\r");
          break;
        case '"':
          // double quote
          builder.append("\\\"");
          break;
        case '\'':
          builder.append('\'');
          break;
        case '\\':
          // backslash
          builder.append("\\\\");
          break;
        default:
          if (c >= ' ' && (c < 0x80)) {
            builder.append(c);
          } else {
            builder
                .append("\\u")
                .append(HEX[c >>> 12])
                .append(HEX[c >>> 8 & 0xf])
                .append(HEX[c >>> 4 & 0xf])
                .append(HEX[c & 0xf]);
          }
      }
    }

    return builder.toString();
  }
}
