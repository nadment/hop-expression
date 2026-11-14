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

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.jspecify.annotations.NullMarked;

/**
 * The AUTO datetime format ISO 8601
 *
 * <ul>
 *   <li>yyyy-[m]m-[d]d hh:mm:ss[.f...][ ][tz].
 *   <li>yyyymmddhhmmss[.f...][tz]
 * </ul>
 *
 * <ul>
 *   <li>The year of the first format has more than 4 digits beyond the year 9999, and a minus sign
 *       before the year 0.
 *   <li>The leading zero for mm and dd may be omitted.
 *   <li>The separator between date part and time part can be one space or 'T'.
 *   <li>The fractional seconds may be omitted or can separated by comma(,) or dot(.).
 *   <li>The time zone offset ±HH:mm ±HHmm ±HH or Z
 * </ul>
 */
@NullMarked
public class AutoDateTimeFormat extends DateTimeFormat {

  private final DateTimeFormat defaultFormat;

  public AutoDateTimeFormat() {
    super();

    defaultFormat = of("YYYY-MM-DD HH:MI:SS.FF");
  }

  @Override
  public String format(ZonedDateTime value) {
    return defaultFormat.format(value);
  }

  @Override
  public ZonedDateTime parse(String str) throws ExpressionException {
    DateTimeParser parser = new DateTimeParser(this, str);

    parser.skipSpace();

    // Extended format
    int yearLength = extendedYearLength(parser);
    if (yearLength > 0) {

      // Year (need 4 digits or more), with a minus sign before the year 0
      boolean negative = parser.isChar('-');
      if (negative) {
        parser.index++;
      }
      parser.year = parser.parseExactInt(yearLength);
      if (negative) {
        parser.year = -parser.year;
      }

      if (parser.isChar('-')) {
        parser.index++;
      }

      // Month
      parser.month = parser.parseInt(2);
      if (parser.isChar('-')) {
        parser.index++;
      }

      // Day
      parser.day = parser.parseInt(2);

      if (parser.isAllCharParsed()) return parser.build();

      // Separator space or T
      if (parser.isSpace() || parser.isChar('T')) {
        parser.index++;
      }

      // Hour
      parser.hour = parser.parseInt(2);
      if (parser.isChar(':')) {
        parser.index++;
        // Minute
        parser.minute = parser.parseInt(2);
        if (parser.isChar(':')) {
          parser.index++;
          // Second
          parser.second = parser.parseInt(2);
          // Nano (optional)
          if (parser.isChar('.') || parser.isChar(',')) {
            parser.index++;
            parser.nano = parser.parseNano();
          }
        }
      }

      if (parser.isAllCharParsed()) return parser.build();

      if (parser.isSpace()) {
        parser.index++;
      }
    }
    // Short format
    else {
      parser.year = parser.parseExactInt(4);
      parser.month = parser.parseExactInt(2);
      parser.day = parser.parseExactInt(2);
      // Separator T can be omitted
      if (parser.isChar('T')) {
        parser.index++;
      }
      parser.hour = parser.parseInt(2);
      parser.minute = parser.parseInt(2);
      parser.second = parser.parseInt(2);
      // Nano (optional)
      if (parser.isChar('.') || parser.isChar(',')) {
        parser.index++;
        parser.nano = parser.parseNano();
      }
    }

    // Time zone hour
    if (parser.isSign()) {
      parser.parseTimeZoneHour();

      // Time zone offset separator can be omitted
      if (parser.isChar(':')) {
        parser.index++;
      }
      // Time zone minute
      if (parser.isDigit()) {
        parser.timeZoneMinute = parser.parseSignedInt(2);
      }
    }
    // No offset from UTC (or 0 hour offset).
    else if (parser.isChar('Z')) {
      parser.index++;
      // Without offset, the date would be in the system time zone
      parser.zoneId = ZoneOffset.UTC;
    }

    parser.skipSpace();

    if (!parser.isAllCharParsed()) {
      throw new ExpressionException(ErrorCode.UNPARSABLE_DATE_WITH_FORMAT, str, this);
    }

    // Build the date
    return parser.build();
  }

  /**
   * Returns the number of digits of the year of an extended format, with an optional minus sign
   * before the year, like {@code 2024-01-31}, {@code -0099-01-01} or {@code 10000-01-06}.
   *
   * @param parser the parser at the start of the date
   * @return the number of digits of the year, or 0 if the format is not extended
   */
  private static int extendedYearLength(final DateTimeParser parser) {
    int start = parser.isChar('-') ? parser.index + 1 : parser.index;
    int end = start;
    while (Characters.isDigit(parser.charAt(end))) {
      end++;
    }
    int length = end - start;
    if (length < 4 || length > 9 || parser.charAt(end) != '-') {
      return 0;
    }
    if (length == 4) {
      return length;
    }

    // Beyond 4 digits, a month and a second separator follow the year of an extended format, a
    // short format is followed by a time zone offset like 20240131-0200
    int month = end + 1;
    while (Characters.isDigit(parser.charAt(month))) {
      month++;
    }
    int monthLength = month - end - 1;
    return (monthLength == 1 || monthLength == 2) && parser.charAt(month) == '-' ? length : 0;
  }

  @Override
  public String toString() {
    return "AUTO";
  }
}
