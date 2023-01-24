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

public class NumberWords {

  private NumberWords() {}

  private static final String[] UNITS = {"zero", "one", "two", "three", "four", "five", "six",
      "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
      "sixteen", "seventeen", "eighteen", "nineteen"};
  private static final String[] TENS =
      {"zero", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"};

  protected static String convertOrdinal(int number) {
    // variable to hold string representation of number
    StringBuilder words = new StringBuilder();

    if (number == 0) {
      return "zero";
    }

    // check if number is divisible by 1 million
    if ((number / 1000000) > 0) {
      words.append(convertOrdinal(number / 1000000)).append(" million ");
      number %= 1000000;
    }
    // check if number is divisible by 1 thousand
    if ((number / 1000) > 0) {
      words.append(convertOrdinal(number / 1000)).append(" thousand ");
      number %= 1000;
    }
    // check if number is divisible by 1 hundred
    if ((number / 100) > 0) {
      words.append(convertOrdinal(number / 100)).append(" hundred ");
      number %= 100;
    }

    convert(words, number);

    return words.toString();
  }

  protected static String convertYear(int number) {
    // variable to hold string representation of number
    StringBuilder words = new StringBuilder();

    if (number == 0) {
      return "";
    }

    if (number > 9999) {
      return convertOrdinal(number);
    }

    // check if number is divisible by 1 hundred
    if ((number / 100) > 0) {
      convert(words, number / 100);
      number %= 100;

      words.append(' ');

      if (number == 0) {
        words.append("hundred");
        return words.toString();
      }
    }

    convert(words, number);

    return words.toString();
  }


  private static StringBuilder convert(StringBuilder words, int number) {
    if (number > 0) {
      // check if number is within teens
      if (number < 20) {
        // fetch the appropriate value from unit array
        words.append(UNITS[number]);
      } else {
        // fetch the appropriate value from tens array
        words.append(TENS[number / 10]);
        if ((number % 10) > 0) {
          words.append('-').append(UNITS[number % 10]);
        }
      }
    }

    return words;
  }
}
