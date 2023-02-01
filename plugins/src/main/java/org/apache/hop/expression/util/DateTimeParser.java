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

/* package */ class DateTimeParser {

  public DateTimeParser(String text) {
    this.text = text;
    this.index = 0;
  }

  public final String text;
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
  public ZoneId zoneId = null;

  protected char parseChar() {

    if (index == text.length())
      return 0;

    return text.charAt(index++);
  }

  /**
   * Parse an integer at the given position in a string
   *
   * @param lenght number of digits to parse in the string
   * @return the int
   * @throws NumberFormatException if the value is not a number
   */
  protected int parseInt(int length) throws NumberFormatException {

    int end = text.length();

    // start at the first not white space symbol
    for (; index < end; index++) {
      if (!Characters.isSpace(text.charAt(index))) {
        break;
      }
    }

    int result = 0;
    if (index + length > end)
      length = end - index;

    int initialIndex = index;

    for (int i = 0; i < length; i++, index++) {
      char ch = text.charAt(index);

      int digit = Character.digit(ch, 10);
      if (digit < 0) {
        if (index == initialIndex) {
          throw new NumberFormatException("Invalid number: " + text);
        }
        break;
      }

      result *= 10;
      result += digit;
    }

    return result;
  }

  protected int parseExactInt(int length) throws NumberFormatException {

    int end = text.length();

    int result = 0;
    if (index + length > end)
      throw new NumberFormatException("Invalid number: " + text);

    for (int i = 0; i < length; i++, index++) {
      char ch = text.charAt(index);

      int digit = Character.digit(ch, 10);
      if (digit < 0) {
        throw new NumberFormatException("Invalid number: " + text);
      }

      result *= 10;
      result += digit;
    }

    return result;
  }

  /**
   * Parse an integer at the given position in a string
   *
   * @param value the string to parse
   * @param position the start index for the integer in the string
   * @param lenght number of digits to parse in the string
   * @return the signed int
   * @throws NumberFormatException if the value is not a number
   */
  protected int parseSignedInt(int length) throws NumberFormatException {

    int end = text.length();

    // start at the first not white space symbol
    for (; index < end; index++) {
      if (!Characters.isSpace(text.charAt(index))) {
        break;
      }
    }

    int result = 0;
    if (index + length > end)
      length = end - index;

    char sign = text.charAt(index);
    if (sign == '-' || sign == '+') {
      index++;
      length--;
    } else {
      sign = '+';
    }

    for (int i = 0; i < length; i++) {
      int digit = Character.digit(text.charAt(index++), 10);
      if (digit < 0) {
        break;
        // position.setErrorIndex(index);
        // throw new NumberFormatException(
        // "Invalid number: " + value.substring(position.getIndex(), position.getIndex() + i));
      }
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
    if (zoneId == null) {
      if (isTimeZoneOffset) {
        zoneId = ZoneOffset.ofHoursMinutes(timeZoneHour, timeZoneMinute);
      } else {
        zoneId = ZoneId.systemDefault();
      }
    }
    return ZonedDateTime.of(localDatetime, zoneId);
  }
}
