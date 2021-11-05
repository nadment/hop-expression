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

public class RomanNumeral {

  private static final int[] VALUES = {1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1};

  private static final String[] NUMERALS =
      {"M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"};

  private RomanNumeral() {}

  /**
   * Convert an arabic integer value into a roman numeral string
   *
   * @param n The arabic integer value
   * @return The roman numeral string
   */
  public static String format(int number) {
    if ((number < 1) || (number > 3999))
      throw new IllegalArgumentException("Roman numbers can only be 1 - 3999, provided: " + number);

    StringBuilder result = new StringBuilder();
    for (int i = 0; i < VALUES.length; i++) {
      int value = VALUES[i];
      String numeral = NUMERALS[i];
      while (number >= value) {
        result.append(numeral);
        number -= value;
      }
    }
    return result.toString();
  }

  /**
   * Convert a roman numeral string into an arabic long value.
   * 
   * @param str The roman numeral string
   * @param start the beginning index, inclusive.
   * @param end the ending index, exclusive.
   * @return The arabic long value
   */
  public static long parse(String str, int start, int end) {
    long result = 0L;

    for (int i = start; i < end; i++) {
      // Getting value of symbol s[i]
      int s1 = parse(str.charAt(i));

      // Getting value of symbol s[i+1]
      if (i + 1 < str.length()) {
        int s2 = parse(str.charAt(i + 1));
        if (s1 >= s2) {
          // Current symbol is greater or equal to the next symbol
          result = result + s1;
        } else {
          // Current symbol is less than the next symbol
          result = result + s2 - s1;
          i++;
        }
      } else {
        result = result + s1;
      }
    }
    return result;
  }

  /**
   * This function returns value of a Roman symbol
   */
  private static int parse(char r) {
    switch (r) {
      case 'I':
        return 1;
      case 'V':
        return 5;
      case 'X':
        return 10;
      case 'L':
        return 50;
      case 'C':
        return 100;
      case 'D':
        return 500;
      case 'M':
        return 1000;
      default:
        return -1;
    }
  }
}
