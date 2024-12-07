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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.hop.expression.ErrorCode;

/**
 * Expression number format model for <code>TO_NUMBER(string, format)</code> and <code>
 * TO_CHAR(number, format)</code> functions.
 *
 * <p>
 *
 * <table border="1">
 * <th>
 * <td>Input</td>
 * <td>Output</td></th>
 * <tr>
 * <td>, (comma)</td>
 * <td>Grouping separator.</td>
 * <td>,</td>
 * </tr>
 * <tr>
 * <td>. (periode)</td>
 * <td>Decimal separator.</td>
 * <td>.</td>
 * </tr>
 * <tr>
 * <td>$</td>
 * <td>Leading dollar sign.</td>
 * <td>$</td>
 * </tr>
 * <tr>
 * <td>0</td>
 * <td>Leading or trailing zeroes.</td>
 * <td>0</td>
 * </tr>
 * <tr>
 * <td>9</td>
 * <td>Digit.</td>
 * <td>#</td>
 * </tr>
 * <tr>
 * <td>B</td>
 * <td>Blanks integer part of a fixed point number less than 1.</td>
 * <td>#</td>
 * </tr>
 * <tr>
 * <td>C</td>
 * <td>ISO currency abbreviation.</td>
 * <td>\u00A4</td>
 * </tr>
 * <tr>
 * <td>D</td>
 * <td>Local decimal separator.</td>
 * <td>.</td>
 * </tr>
 * <tr>
 * <td>EEEE</td>
 * <td>Returns a value in scientific notation (case sensitive).</td>
 * <td>E</td>
 * </tr>
 * <tr>
 * <td>FM</td>
 * <td>Returns values with no leading or trailing spaces.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>G</td>
 * <td>Local grouping separator.</td>
 * <td>,</td>
 * </tr>
 * <tr>
 * <td>L</td>
 * <td>Local currency symbol.</td>
 * <td>\u00A4</td>
 * </tr>
 * <tr>
 * <td>MI</td>
 * <td>Negative values get trailing minus sign, positive get trailing space.</td>
 * <td>-</td>
 * </tr>
 * <tr>
 * <td>PR</td>
 * <td>Negative values get enclosing angle brackets, positive get spaces.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>RN</td>
 * <td>Returns values in Roman numerals (case sensitive).</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>S</td>
 * <td>Returns values with leading/trailing +/- signs.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>TM</td>
 * <td>Returns smallest number of characters possible.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>U</td>
 * <td>Returns the dual currency symbol.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>V</td>
 * <td>Returns a value multiplied by 10^n.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>X</td>
 * <td>Hex value.</td>
 * <td>None.</td>
 * </tr>
 * </table>
 */
public abstract class NumberFormat extends BaseFormat {

  private static final Map<String, NumberFormat> cache = new ConcurrentHashMap<>();

  public static NumberFormat of(final String pattern) {

    if (pattern == null || pattern.length() == 0) {
      throw new NumberFormatException("Pattern is null or empty");
    }

    return cache.computeIfAbsent(pattern, NumberFormat::create);
  }

  private static NumberFormat create(final String pattern) {

    // Composite format
    if (pattern.indexOf('|') >= 0) {
      List<NumberFormat> formats = new ArrayList<>();
      for (String p : pattern.split("\\|")) {
        NumberFormat format = new PatternNumberFormat(p);
        formats.add(format);
      }
      return new CompositeNumberFormat(pattern, formats.toArray(new NumberFormat[0]));
    }

    // Short-circuit logic for formats that don't follow decimal logic below

    // Text minimal format
    if (pattern.equalsIgnoreCase("TM")
        || pattern.equalsIgnoreCase("TME")
        || pattern.equalsIgnoreCase("TM9")) {
      return new TextMinimalNumberFormat(pattern);
    }

    // Roman numerals
    if (pattern.equalsIgnoreCase("RN") || pattern.equalsIgnoreCase("fmRN")) {
      return new RomanNumberFormat(pattern);
    }

    return new PatternNumberFormat(pattern);
  }

  /**
   * Parses text from a string to produce a <code>Number</code>.
   *
   * @param text the string to be parsed
   * @return the parsed value
   * @throws FormatParseException
   */
  public abstract BigDecimal parse(String text) throws FormatParseException;

  /**
   * Format number with number format.
   *
   * @param number the number to format
   * @param locale the locale to use
   * @return the formatted number
   */
  public abstract String format(BigDecimal number);

  protected final FormatParseException createInvalidFormat(final String format) {
    return new FormatParseException(ErrorCode.INVALID_NUMBER_FORMAT, format);
  }

  protected final FormatParseException createUnparsableNumber(
      final String format, final String text, int index) {
    return new FormatParseException(ErrorCode.UNPARSABLE_NUMBER_WITH_FORMAT, text, format, index);
  }
}
