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

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import org.apache.commons.math3.util.FastMath;
import org.apache.hop.expression.ErrorCode;

/* package */ class DateTimeParser {

  public DateTimeParser(final DateTimeFormat format, final String text) {
    this.format = format;
    this.text = text;
    this.length = text.length();
    this.index = 0;
  }

  public final DateTimeFormat format;
  public final String text;
  public final int length;
  public int index;

  public boolean bc = false;
  public long epochDay = 0;
  // Default minimum year if omitted
  public int year = 1970;
  // Default minimum month if omitted
  public int month = 1;
  // Default minimum day if omitted
  public int day = 1;
  public int dayOfYear = 0;
  public int hour = 0;
  public int minute = 0;
  public int second = 0;
  public int nano = 0;
  public int timeZoneHour = 0;
  public int timeZoneMinute = 0;

  public boolean isPM = false;
  public boolean isHourFormat12 = false;
  public boolean isEpochDay = false;
  public boolean isDayOfYear = false;
  public boolean isTimeZoneOffset = false;
  public ZoneId zoneId = ZoneOffset.UTC;

  protected boolean isAllCharParsed() {
    return index == length;
  }

  protected boolean isChar(final char c) {
    if (index == length) return false;
    return text.charAt(index) == c;
  }

  protected boolean isSign() {
    if (index == length) return false;
    char c = text.charAt(index);
    return c == '+' || c == '-';
  }

  protected boolean isSpace() {
    if (index == length) return false;
    char c = text.charAt(index);
    return Characters.isSpace(c);
  }

  protected boolean isDigit() {
    if (index == length) return false;
    char c = text.charAt(index);
    return Characters.isDigit(c);
  }

  protected char charAt(final int i) {
    if (i >= length) return 0;
    return text.charAt(i);
  }

  protected char parseChar() {
    if (index == length) return 0;
    return text.charAt(index++);
  }

  protected char parseChar(char expectedChar) throws FormatParseException {
    if (index == length) return 0;
    char c = text.charAt(index);
    if (c != expectedChar) {
      throw new FormatParseException(ErrorCode.UNPARSABLE_DATE_WITH_FORMAT, text, index);
    }
    index++;

    return c;
  }

  protected int parseNano() throws FormatParseException {
    int result = 0;
    int initialIndex = index;
    int scale = 0;

    for (; scale < 9 && index < length; scale++, index++) {
      char ch = text.charAt(index);
      int digit = Character.digit(ch, 10);
      if (digit < 0) {
        if (index == initialIndex) {
          throw new FormatParseException(ErrorCode.UNPARSABLE_DATE_WITH_FORMAT, text, format);
        }
        break;
      }

      result *= 10;
      result += digit;
    }

    result *= (int) FastMath.pow(10d, 9 - scale);

    return result;
  }

  /**
   * Parse an integer at the given position in a string
   *
   * @param lenght number of digits to parse in the string
   * @return the int
   * @throws FormatParseException if the value is not a number
   */
  protected int parseInt(int len) throws FormatParseException {
    // start at the first not white space symbol
    for (; index < length; index++) {
      if (!Characters.isSpace(text.charAt(index))) {
        break;
      }
    }

    int result = 0;
    if (index + len > length) len = length - index;

    int initialIndex = index;

    for (int i = 0; i < len; i++, index++) {
      char ch = text.charAt(index);

      int digit = Character.digit(ch, 10);
      if (digit < 0) {
        if (index == initialIndex) {
          throw new FormatParseException(ErrorCode.UNPARSABLE_DATE_WITH_FORMAT, text, format);
        }
        break;
      }

      result *= 10;
      result += digit;
    }

    return result;
  }

  protected int parseExactInt(final int len) throws FormatParseException {
    int result = 0;
    if (index + len > length)
      throw new FormatParseException(ErrorCode.UNPARSABLE_DATE_WITH_FORMAT, text, format);

    for (int i = 0; i < len; i++, index++) {
      char ch = text.charAt(index);

      int digit = Character.digit(ch, 10);
      if (digit < 0) {
        throw new FormatParseException(ErrorCode.UNPARSABLE_DATE_WITH_FORMAT, text, format);
      }

      result *= 10;
      result += digit;
    }

    return result;
  }

  /** Skip white space symbol */
  protected void skipSpace() {
    for (; index < length; index++) {
      char c = text.charAt(index);
      if (!Characters.isSpace(c)) {
        return;
      }
    }
  }

  /**
   * Parse an integer at the given position in a string
   *
   * @param value the string to parse
   * @param position the start index for the integer in the string
   * @param len number of digits to parse in the string
   * @return the signed int
   * @throws NumberFormatException if the value is not a number
   */
  protected int parseSignedInt(int len) throws NumberFormatException {

    // start at the first not white space symbol
    for (; index < length; index++) {
      if (!Characters.isSpace(text.charAt(index))) {
        break;
      }
    }
    int result = 0;
    if (index + len > length) len = length - index;

    char sign = text.charAt(index);
    if (sign == '-' || sign == '+') {
      index++;
      len--;
    } else {
      sign = '+';
    }

    for (int i = 0; i < len; i++) {
      char c = text.charAt(index);
      int digit = Character.digit(c, 10);
      if (digit < 0) {
        break;
      }
      index++;
      result *= 10;
      result += digit;
    }

    return (sign == '-') ? -result : result;
  }

  protected String parseString(String... substrings) {
    for (String substring : substrings) {
      if (text.regionMatches(true, index, substring, 0, substring.length())) {
        index += substring.length();
        return substring;
      }
    }

    return null;
  }

  public ZonedDateTime build() {
    try {
      // Build the date
      LocalDate date = null;
      if (isEpochDay) {
        date = LocalDate.ofEpochDay(epochDay);
      } else {
        if (bc) {
          year = 1 - year;
        }
        if (isDayOfYear) {
          date = LocalDate.ofYearDay(year, dayOfYear);
        } else {
          date = LocalDate.of(year, month, day);
        }
      }

      if (isHourFormat12) {
        hour = hour % 12;
        if (isPM) {
          hour += 12;
        }
      }
      LocalTime time = LocalTime.of(hour, minute, second, nano);
      LocalDateTime localDatetime = LocalDateTime.of(date, time);
      if (isTimeZoneOffset) {
        zoneId = ZoneOffset.ofHoursMinutes(timeZoneHour, timeZoneMinute);
      }
      return ZonedDateTime.of(localDatetime, zoneId);
    } catch (Exception e) {
      throw new FormatParseException(ErrorCode.UNPARSABLE_DATE_WITH_FORMAT, text, format);
    }
  }
}
