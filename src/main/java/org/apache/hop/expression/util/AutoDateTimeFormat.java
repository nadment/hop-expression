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

import java.time.ZonedDateTime;
import org.apache.hop.expression.ErrorCode;

/**
 * The AUTO datetime format ISO 8601
 *
 * <ul>
 *   <li>yyyy-[m]m-[d]d hh:mm:ss[.f...][ ][tz].
 *   <li>yyyymmddhhmmss[.f...][tz]</il>
 * </ul>
 *
 * <ul>
 *   <li>The leading zero for mm and dd may be omitted.
 *   <li>The separator between date part and time part can be one space or 'T'.
 *   <li>The fractional seconds may be omitted or can separated by comma(,) or dot(.).
 *   <li>The time zone offset ±HH:mm ±HHmm ±HH or Z
 * </ul>
 */
public class AutoDateTimeFormat extends DateTimeFormat {

  private final DateTimeFormat defaultFormat;

  public AutoDateTimeFormat() {
    super();

    defaultFormat = of("YYYY-MM-DD HH:MI:SS.FF");
  }

  @Override
  public String format(final ZonedDateTime value) {
    return defaultFormat.format(value);
  }

  @Override
  public ZonedDateTime parse(final String str) throws FormatParseException {
    DateTimeParser parser = new DateTimeParser(this, str);

    parser.skipSpace();

    // Extended format
    int separator = parser.charAt(parser.index + 4);
    if (separator == '-') {

      // Year (need 4 digits)
      parser.year = parser.parseExactInt(4);

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
      parser.parseChar('T');
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
      parser.isTimeZoneOffset = true;
      parser.timeZoneHour = parser.parseSignedInt(3);

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
    }

    parser.skipSpace();

    if (!parser.isAllCharParsed()) {
      throw new FormatParseException(ErrorCode.UNPARSABLE_DATE_WITH_FORMAT, str, this);
    }

    // Build the date
    return parser.build();
  }

  @Override
  public String toString() {
    return "AUTO";
  }
}
