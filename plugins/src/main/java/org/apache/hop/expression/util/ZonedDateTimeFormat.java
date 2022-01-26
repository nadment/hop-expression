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

import org.apache.commons.lang.StringUtils;
import org.apache.commons.math3.util.FastMath;
import org.apache.hop.i18n.BaseMessages;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.ParseException;
import java.text.ParsePosition;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;
import java.time.format.TextStyle;
import java.time.temporal.ChronoField;
import java.time.temporal.IsoFields;
import java.time.temporal.JulianFields;
import java.util.IllegalFormatFlagsException;
import java.util.Locale;

/**
 * Expression date/time format model for <code>TO_DATE(string, format)</code> and
 * <code>TO_CHAR(datetime, format)</code> functions..
 *
 * <p>
 *
 * <table border="1">
 * <th>
 * <td>Input</td>
 * <td>Output</td>
 * <td>Closest {@link ZonedDateTimeFormat} Equivalent</td></th>
 * <tr>
 * <td>- / , . ; : "text"</td>
 * <td>Reproduced verbatim.</td>
 * <td>'text'</td>
 * </tr>
 * <tr>
 * <td>A.D. AD B.C. BC</td>
 * <td>Era designator, with or without periods.</td>
 * <td>G</td>
 * </tr>
 * <tr>
 * <td>A.M. AM P.M. PM</td>
 * <td>AM/PM marker.</td>
 * <td>a</td>
 * </tr>
 * <tr>
 * <td>CC SCC</td>
 * <td>Century.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>D</td>
 * <td>Day of week.</td>
 * <td>u</td>
 * </tr>
 * <tr>
 * <td>DAY</td>
 * <td>Name of day.</td>
 * <td>EEEE</td>
 * </tr>
 * <tr>
 * <td>DY</td>
 * <td>Abbreviated day name.</td>
 * <td>EEE</td>
 * </tr>
 * <tr>
 * <td>DD</td>
 * <td>Day of month.</td>
 * <td>d</td>
 * </tr>
 * <tr>
 * <td>DDD</td>
 * <td>Day of year.</td>
 * <td>D</td>
 * </tr>
 * <tr>
 * <td>DL</td>
 * <td>Long date format.</td>
 * <td>EEEE, MMMM d, yyyy</td>
 * </tr>
 * <tr>
 * <td>DS</td>
 * <td>Short date format.</td>
 * <td>MM/dd/yyyy</td>
 * </tr>
 * <tr>
 * <td>E</td>
 * <td>Abbreviated era name (Japanese, Chinese, Thai)</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>EE</td>
 * <td>Full era name (Japanese, Chinese, Thai)</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>FF[1-9]</td>
 * <td>Fractional seconds.</td>
 * <td>S</td>
 * </tr>
 * <tr>
 * <td>FM</td>
 * <td>Returns values with no leading or trailing spaces.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>FX</td>
 * <td>Requires exact matches between character data and format model.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>HH HH12</td>
 * <td>Hour in AM/PM (1-12).</td>
 * <td>hh</td>
 * </tr>
 * <tr>
 * <td>HH24</td>
 * <td>Hour in day (0-23).</td>
 * <td>HH</td>
 * </tr>
 * <tr>
 * <td>IW</td>
 * <td>Week in year.</td>
 * <td>w</td>
 * </tr>
 * <tr>
 * <td>WW</td>
 * <td>Week in year.</td>
 * <td>w</td>
 * </tr>
 * <tr>
 * <td>W</td>
 * <td>Week in month.</td>
 * <td>W</td>
 * </tr>
 * <tr>
 * <td>IYYY IYY IY I</td>
 * <td>Last 4/3/2/1 digit(s) of ISO year.</td>
 * <td>yyyy yyy yy y</td>
 * </tr>
 * <tr>
 * <td>RRRR RR</td>
 * <td>Last 4/2 digits of year.</td>
 * <td>yyyy yy</td>
 * </tr>
 * <tr>
 * <td>Y,YYY</td>
 * <td>Year with comma.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>YEAR SYEAR</td>
 * <td>Year spelled out (S prefixes BC years with minus sign).</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>YYYY SYYYY</td>
 * <td>4-digit year (S prefixes BC years with minus sign).</td>
 * <td>yyyy</td>
 * </tr>
 * <tr>
 * <td>YYY YY Y</td>
 * <td>Last 3/2/1 digit(s) of year.</td>
 * <td>yyy yy y</td>
 * </tr>
 * <tr>
 * <td>J</td>
 * <td>Julian day (number of days since January 1, 4712 BC).</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>MI</td>
 * <td>Minute in hour.</td>
 * <td>mm</td>
 * </tr>
 * <tr>
 * <td>MM</td>
 * <td>Month in year.</td>
 * <td>MM</td>
 * </tr>
 * <tr>
 * <td>MON</td>
 * <td>Abbreviated name of month.</td>
 * <td>MMM</td>
 * </tr>
 * <tr>
 * <td>MONTH</td>
 * <td>Name of month, padded with spaces.</td>
 * <td>MMMM</td>
 * </tr>
 * <tr>
 * <td>RM</td>
 * <td>Roman numeral month.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>Q</td>
 * <td>Quarter of year.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>SS</td>
 * <td>Seconds in minute.</td>
 * <td>ss</td>
 * </tr>
 * <tr>
 * <td>SSSSS</td>
 * <td>Seconds in day.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>TS</td>
 * <td>Short time format.</td>
 * <td>h:mm:ss aa</td>
 * </tr>
 * <tr>
 * <td>TZD</td>
 * <td>Daylight savings time zone abbreviation.</td>
 * <td>z</td>
 * </tr>
 * <tr>
 * <td>TZR</td>
 * <td>Time zone region information.</td>
 * <td>zzzz</td>
 * </tr>
 * <tr>
 * <td>X</td>
 * <td>Local radix character.</td>
 * <td>None.</td>
 * </tr>
 * </table>
 */
/* package */ class ZonedDateTimeFormat extends DateTimeFormat {

  // TODO: Specifies the “century start” year for 2-digit years. This parameter prevents
  // ambiguous dates when importing or converting data with the YY date format
  // component.
  private static final int TWO_DIGIT_CENTURY_START = 1970;

  /** The offset from Julian to EPOCH DAY. */
  private static final long JULIAN_DAY_OFFSET = 2440588L;

  private static final String[] ROMAN_MONTHS =
      {"I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII"};

  private static final String[] SHORT_MONTHS =
      {"JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"};
  private static final String[] MONTHS = {"JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE",
      "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER"};

  private static final String[] AM_PM = {"AM", "A.M.", "PM", "P.M."};

  private static final String[] AD_BC = {"AD", "A.D.", "BC", "B.C."};

  /** Minutes per hour. */
  private static final int MINUTES_PER_HOUR = 60;
  /** Seconds per minute. */
  private static final int SECONDS_PER_MINUTE = 60;
  /** Seconds per hour. */
  private static final int SECONDS_PER_HOUR = SECONDS_PER_MINUTE * MINUTES_PER_HOUR;

  private final String pattern;

  public ZonedDateTimeFormat(String pattern) {
    this.pattern = pattern;
  }

  public ZonedDateTime parse(String text) throws ParseException {

    ParsePosition position = new ParsePosition(0);

    // start at the first not white space symbol
    for (int start = position.getIndex(); start < text.length(); start++) {
      if (!Character.isSpaceChar(text.charAt(start))) {
        position.setIndex(start);
        break;
      }
    }

    boolean bc = false;
    long epochDay = 0;
    // Default minimum year if omitted
    int year = 1970;
    // Default minimum month if omitted
    int month = 1;
    // Default minimum day if omitted
    int day = 1;
    int dayOfYear = 0;
    int hour = 0;
    int minute = 0;
    int second = 0;
    int nanos = 0;
    int timeZoneHour = 0;
    int timeZoneMinute = 0;

    boolean isPM = false;
    boolean isHourFormat12 = false;
    boolean isEpochDay = false;
    boolean isDayOfYear = false;
    boolean isTimeZoneOffset = false;
    ZoneId zoneId = null;
    
    int length = pattern.length();
    int index = 0;
    while (index < length) {

      // Ignore case for parsing
      char c = Character.toUpperCase(pattern.charAt(index));

      // Use first letter for optimization
      switch (c) {

        // Ignore space and punctuation such as hyphen (-), slash (/), comma (,), period
        // (.) and colons (:)
        case ' ':
        case '-':
        case '_':
        case '/':
        case ',':
        case '.':
        case ';':
        case ':':
        case '\\':
          index++;
          position.setIndex(position.getIndex() + 1);
          continue;

        case 'A':
          // Meridian indicator
          if (startsWithIgnoreCase(pattern, index, "AM")) {
            String str = parseString(text, position, AM_PM);
            if (str == null)
              break;
            if (str.charAt(0) == 'P')
              isPM = true;
            isHourFormat12 = true;
            index += 2;
            continue;
          }

          // Meridian indicator with period
          if (startsWithIgnoreCase(pattern, index, "A.M.")) {
            String str = parseString(text, position, AM_PM);
            if (str == null)
              break;
            if (str.charAt(0) == 'P')
              isPM = true;
            isHourFormat12 = true;
            index += 4;
            continue;
          }

          // Era designator
          if (startsWithIgnoreCase(pattern, index, "AD")) {
            String str = parseString(text, position, AD_BC);
            if (str == null)
              break;
            if (str.charAt(0) == 'B')
              bc = true;
            isDayOfYear = false;
            isEpochDay = false;
            index += 2;
            continue;
          }

          // Era designator with period
          if (startsWithIgnoreCase(pattern, index, "A.D.")) {
            String str = parseString(text, position, AD_BC);
            if (str == null)
              break;
            if (str.charAt(0) == 'B')
              bc = true;
            isDayOfYear = false;
            isEpochDay = false;
            index += 4;
            continue;
          }
          break;

        case 'B':
          // Era designator
          if (startsWithIgnoreCase(pattern, index, "BC")) {
            String str = parseString(text, position, AD_BC);
            if (str == null)
              break;
            if (str.charAt(0) == 'B')
              bc = true;
            isDayOfYear = false;
            isEpochDay = false;
            index += 2;
            continue;
          }

          // Era designator with period
          if (startsWithIgnoreCase(pattern, index, "B.C.")) {
            String str = parseString(text, position, AD_BC);
            if (str == null)
              break;
            if (str.charAt(0) == 'B')
              bc = true;
            isDayOfYear = false;
            isEpochDay = false;
            index += 4;
            continue;
          }
          break;

        case 'D':
          // Day of year (1-366)
          if (startsWithIgnoreCase(pattern, index, "DDD")) {
            dayOfYear = parseInt(text, position, "DDD".length());
            isDayOfYear = true;
            isEpochDay = false;
            index += 3;
            continue;
          }

          // Day of month (1-31)
          if (startsWithIgnoreCase(pattern, index, "DD")) {
            day = parseInt(text, position, "DD".length());
            isDayOfYear = false;
            isEpochDay = false;
            index += 2;
            continue;
          }

        // TODO: Day of week (1-7)
        {
          //day = parseInt(text, position, "D".length());
          //isDayOfYear = false;
          //isEpochDay = false;
          //index += 1;
          /* NOT supported yet */
          throw new ParseException("Parsing format D not supported yet", index);
          //continue;
        }

        // Fractional seconds FF[0-9]
        case 'F':
          if (startsWithIgnoreCase(pattern, index, "FF")) {
            index += 2;
            int scale = 6;
            if (index < length) {
              c = pattern.charAt(index);
              if (Characters.isDigit(c)) {
                scale = c - '0';
                index++;
              }
            }
            nanos = parseInt(text, position, scale);
            if (scale < 9) {
              nanos = (int) (nanos * FastMath.pow(10d, 9d - scale));
            }
            continue;
          }

          throw new ParseException("Parsing format F not supported yet", index);

        case 'J': {
          // Julian day; the number of days since Jan 1, 4712 BC.

          index += 1;
          epochDay = parseInt(text, position, 7) - JULIAN_DAY_OFFSET;
          isDayOfYear = false;
          isEpochDay = true;
          continue;
        }
        case 'M':
          // Minutes (0-59)
          if (startsWithIgnoreCase(pattern, index, "MI")) {
            minute = parseInt(text, position, 2);
            index += 2;
            continue;
          }

          // Month number (1-12)
          if (startsWithIgnoreCase(pattern, index, "MM")) {
            index += 2;

            try {
              month = parseInt(text, position, 2);
            } catch (NumberFormatException e) {
              // Rule to try alternate format MONTH and MON
              month = parseMonthName(text, position);
            }
            isDayOfYear = false;
            isEpochDay = false;
            continue;
          }
          // Full name of month (parse before MON)
          if (startsWithIgnoreCase(pattern, index, "MONTH")) {
            index += 5;
            month = parseMonthName(text, position);
            isDayOfYear = false;
            isEpochDay = false;
            continue;
          }
          // Abbreviated name of month (parse after MONTH)
          if (startsWithIgnoreCase(pattern, index, "MON")) {
            index += 3;
            month = parseMonthName(text, position);
            isDayOfYear = false;
            isEpochDay = false;
            continue;
          }
          break;

        case 'H':
          // Hour of day (1-23)
          if (startsWithIgnoreCase(pattern, index, "HH24")) {
            hour = parseInt(text, position, 2);
            isHourFormat12 = false;
            index += 4;
            continue;
          }

          // Hour of day (1-12)
          if (startsWithIgnoreCase(pattern, index, "HH12")) {
            hour = parseInt(text, position, 2);
            isHourFormat12 = true;
            index += 4;
            continue;
          }

          // Hour of day (1-12)
          if (startsWithIgnoreCase(pattern, index, "HH")) {
            hour = parseInt(text, position, 2);
            isHourFormat12 = true;
            index += 2;
            continue;
          }
          break;

        case 'Q':
          /* NOT supported yet */
          throw new ParseException("Parsing format Q not supported yet", index);

        case 'R':
          // Roman numeral month (I-XII; January = I).
          if (startsWithIgnoreCase(pattern, index, "RM")) {
            index += 2;
            month = parseMonthRoman(text, position);
            continue;
          }

          // 4-digit year
          if (startsWithIgnoreCase(pattern, index, "RRRR")) {
            year = parseInt(text, position, 4);
            // Years between 00-49 will be given the 21st century (the year 2000)
            if (year >= 0 && year <= 49)
              year += 2000;
            // Years between 50-99 will be given the 20th century (the year 1900).
            else if (year >= 50 && year <= 99)
              year += 1900;
            isEpochDay = false;
            index += 4;
            continue;
          }
          break;

        case 'S':
          // Seconds
          if (startsWithIgnoreCase(pattern, index, "SS")) {
            second = parseInt(text, position, 2);
            index += 2;
            continue;
          }

          // 4-digit year; S prefixes BC dates with a minus sign
          if (startsWithIgnoreCase(pattern, index, "SYYYY")) {
            year = parseSignedInt(text, position, 5);
            isEpochDay = false;
            index += 5;
            continue;
          }
          break;

        case 'T':
          // Time zone hour [+-][0]0
          if (startsWithIgnoreCase(pattern, index, "TZH")) {
            // Skip space
            int i = position.getIndex();
            if ( Characters.isSpace(text.charAt(i)) ) position.setIndex(++i);
            isTimeZoneOffset = true;
            timeZoneHour = parseSignedInt(text, position, 3);
            index += 3;
          }

          // Time zone minute
          if (startsWithIgnoreCase(pattern, index, "TZM")) {
            isTimeZoneOffset = true;
            timeZoneMinute = parseInt(text, position, 2);
            index += 3;
          }
          
          // Time zone region
          if (startsWithIgnoreCase(pattern, index, "TZR")) {
            int i = position.getIndex();
            char ch;
            while ( i<text.length() ) {
              ch = text.charAt(i);
              if ( ! (Character.isLetter(ch) || ch=='/') ) break;
              i++;            
            }
            
            String zone = text.substring(position.getIndex(),i);
            zoneId = ZoneId.of(zone);
            position.setIndex(i);
            index += 3;
          }
          break;

        case 'Y':
          // 4-digit year
          if (startsWithIgnoreCase(pattern, index, "YYYY")) {
            year = parseInt(text, position, 4);
            isEpochDay = false;
            index += 4;
            continue;
          }

          // Last 2-digit year
          if (startsWithIgnoreCase(pattern, index, "YY")) {
            year = parseInt(text, position, 2);
            year += (year < TWO_DIGIT_CENTURY_START - 1900) ? 2000 : 1900;
            isEpochDay = false;
            index += 2;
            continue;
          }
          break;

        case '"':
          index++;
          int pos = position.getIndex();
          while (true) {
            if (index == pattern.length()) {
              throw createUnparsableDate(text, pos);
            }
            char s = pattern.charAt(index++);
            if (s == '"')
              break;
            pos++;
          }
          position.setIndex(pos);
          break;

        default:
          throw createUnparsableDate(text, position.getErrorIndex());
      }
    }

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
        hour += 12;;
      }
    }
    LocalTime time = LocalTime.of(hour, minute, second, nanos);
    LocalDateTime localDatetime = LocalDateTime.of(date, time);
    if ( zoneId==null ) {
      if ( isTimeZoneOffset ) {
        zoneId = ZoneOffset.ofHoursMinutes(timeZoneHour, timeZoneMinute);
      }
      else {
        zoneId = ZoneId.systemDefault();        
      }
    }
    return ZonedDateTime.of(localDatetime, zoneId);
  }

  /**
   * <p>
   * See also TO_CHAR(datetime) and datetime format models in the Oracle documentation.
   *
   * @param value the date-time value to format
   * @param format the format pattern to use (if any)
   * @return the formatted timestamp
   */
  public String format(ZonedDateTime value) {

    StringBuilder output = new StringBuilder();
    boolean fillMode = true;
    int index = 0;

    while (index < pattern.length()) {

      Capitalization cap;

      // Ignore case for parsing
      char c = Character.toUpperCase(pattern.charAt(index));

      // Use first letter for optimization
      switch (c) {
        case '\"':
          // Literal text
          index++;
          for (; index < pattern.length(); index++) {
            char ch = pattern.charAt(index);
            if (ch == '"') {
              index++;
              break;
            }
            output.append(ch);
          }
          continue;

        case 'A':
          // AD indicator without periods
          if ((cap = match(pattern, index, "AD")) != null) {
            String era = (value.getYear() > 0) ? "AD" : "BC";
            output.append(cap.apply(era));
            index += 2;
            continue;
          }

          // AD indicator with periods
          if ((cap = match(pattern, index, "A.D.")) != null) {
            String era = (value.getYear() > 0) ? "A.D." : "B.C.";
            output.append(cap.apply(era));
            index += 4;
            continue;
          }

          if ((cap = match(pattern, index, "AM")) != null) {
            String am = (value.getHour() < 12) ? "AM" : "PM";
            output.append(cap.apply(am));
            index += 2;
            continue;
          }

          if ((cap = match(pattern, index, "A.M.")) != null) {

            String am = (value.getHour() < 12) ? "A.M." : "P.M.";
            output.append(cap.apply(am));
            index += 4;
            continue;
          }
          break;

        case 'B':
          // AD indicator without periods
          if ((cap = match(pattern, index, "BC")) != null) {
            String era = (value.getYear() > 0) ? "AD" : "BC";
            output.append(cap.apply(era));
            index += 2;
            continue;
          }

          // AD indicator with periods
          if ((cap = match(pattern, index, "B.C.")) != null) {
            String era = (value.getYear() > 0) ? "A.D." : "B.C.";
            output.append(cap.apply(era));
            index += 4;
            continue;
          }
          break;

        case 'C':
          // Century
          if (startsWithIgnoreCase(pattern, index, "CC")) {
            int year = FastMath.abs(value.getYear());
            int century = year / 100;
            if ((year % 100) != 0) {
              century += 1;
            }
            appendZeroPadded(output, century, "CC".length());
            index += 2;
            continue;
          }
          break;

        case 'D':
          // Day of year (1-366)
          if (startsWithIgnoreCase(pattern, index, "DDD")) {
            if (fillMode) {
              appendZeroPadded(output, value.getDayOfYear(), "DDD".length());
            } else {
              output.append(value.getDayOfYear());
            }
            index += 3;
            continue;
          }

          // Day of month (1-31)
          if (startsWithIgnoreCase(pattern, index, "DD")) {

            if (fillMode) {
              appendZeroPadded(output, value.getDayOfMonth(), "DD".length());
            } else {
              output.append(value.getDayOfMonth());
            }
            index += 2;
            continue;
          }

          // Long date format 'Tuesday, April 12, 1952 AD'
          if (startsWithIgnoreCase(pattern, index, "DL")) {
            DateTimeFormatter formatter = DateTimeFormatter.ofLocalizedDateTime(FormatStyle.FULL); // .withLocale(Locale.ENGLISH);
            output.append(value.format(formatter));
            index += 2;
            continue;
          }

          // Short date format 'MM/DD/RRRR'.
          if (startsWithIgnoreCase(pattern, index, "DS")) {
            appendZeroPadded(output, value.getMonthValue(), "DD".length());
            output.append('/');
            appendZeroPadded(output, value.getDayOfMonth(), "MM".length());
            output.append('/');
            appendZeroPadded(output, FastMath.abs(value.getYear()), "YYYY".length());
            index += 2;
            continue;
          }

          // Abbreviated name of day
          if ((cap = match(pattern, index, "DY")) != null) {
            String day = value.getDayOfWeek().getDisplayName(TextStyle.SHORT, Locale.ENGLISH);
            output.append(cap.apply(day));
            index += 2;
            continue;
          }

          // Name of day
          if ((cap = match(pattern, index, "DAY")) != null) {
            String day =
                cap.apply(value.getDayOfWeek().getDisplayName(TextStyle.FULL, Locale.ENGLISH));
            if (fillMode) {
              day = StringUtils.rightPad(day, "Wednesday".length(), " ");
            }
            output.append(day);
            index += 3;
            continue;
          }

          // Day of week (1=Sunday-7)
          if (startsWithIgnoreCase(pattern, index, "D")) {
            output.append((value.getDayOfWeek().getValue() + 1) % 7);
            index += 1;
            continue;
          }
          break;

        case 'F':
          // Fractional seconds
          if (startsWithIgnoreCase(pattern, index, "FF0", "FF1", "FF3", "FF6", "FF9")) {
            int length = pattern.charAt(index + 2) - '0';

            int nanos = value.getNano();

            int scale = (int) (nanos * FastMath.pow(10d, length - 9d));
            appendZeroPadded(output, scale, length);
            index += 3;
            continue;
          }
          // FF is equivalent to FF6
          if (startsWithIgnoreCase(pattern, index, "FF")) {
            appendZeroPadded(output, value.getNano(), 6);
            index += 2;
            continue;
          }

          // Fill mode modifier; toggles between compact and fill modes for any elements
          // following the modifier in the model.
          if (startsWithIgnoreCase(pattern, index, "FM")) {
            fillMode = !fillMode;
            index += 2;
            continue;
          }

          // TODO: Exact match modifier; toggles between lax and exact match modes for any
          // elements following the modifier in the model.
          if (startsWithIgnoreCase(pattern, index, "FX")) {
            index += 2;
            continue;
          }
          break;

        case 'H':
          // Hour of day in 24 hour format (0-23)
          if (startsWithIgnoreCase(pattern, index, "HH24")) {
            appendZeroPadded(output, value.getHour(), 2);
            index += 4;
            continue;
          }
          // Hour of day in 12 hour format (1-12)
          if (startsWithIgnoreCase(pattern, index, "HH12")) {
            int h12 = (value.getHour() + 11) % 12 + 1;
            appendZeroPadded(output, h12, 2);
            index += 4;
            continue;
          }
          // Hour of day in 12 hour format (1-12)
          if (startsWithIgnoreCase(pattern, index, "HH")) {
            int h12 = (value.getHour() + 11) % 12 + 1;
            appendZeroPadded(output, h12, "HH".length());
            index += 2;
            continue;
          }
          break;

        case 'I':
          // 4-digit year based on the ISO standard.
          if (startsWithIgnoreCase(pattern, index, "IYYY")) {
            int weekYear = FastMath.abs(value.get(IsoFields.WEEK_BASED_YEAR));
            appendZeroPadded(output, weekYear, 4);
            index += 4;
            continue;
          }

          // Last 3 digits of ISO year.
          if (startsWithIgnoreCase(pattern, index, "IYY")) {
            int weekYear = FastMath.abs(value.get(IsoFields.WEEK_BASED_YEAR));
            appendZeroPadded(output, weekYear % 1000, 3);
            index += 3;
            continue;
          }

          // Last 2 digits of ISO year.
          if (startsWithIgnoreCase(pattern, index, "IY")) {
            int weekYear = FastMath.abs(value.get(IsoFields.WEEK_BASED_YEAR));
            appendZeroPadded(output, weekYear % 100, 2);
            index += 2;
            continue;
          }

          // Week of year (1-52 or 1-53) based on the ISO standard
          if (startsWithIgnoreCase(pattern, index, "IW")) {
            int week = value.get(IsoFields.WEEK_OF_WEEK_BASED_YEAR);                       
            if (fillMode) {
              appendZeroPadded(output,week,2);
            } else {
              output.append(week);
            }
            index += 2;
            continue;
          }

        // Last 1 digit of ISO year "I".
        {
          int weekYear = FastMath.abs(value.get(IsoFields.WEEK_BASED_YEAR));
          output.append(weekYear % 10);
          index += 1;
          continue;
        }
        // break;

        case 'J':
        // Julian day; the number of days since January 1, 4712 BC
        {
          long julianDay = value.getLong(JulianFields.JULIAN_DAY);
          output.append(julianDay);
          index += 1;
          continue;
        }

        case 'M':
          // Minute (0-59)
          if (startsWithIgnoreCase(pattern, index, "MI")) {
            appendZeroPadded(output, value.getMinute(), "MI".length());
            index += 2;
            continue;
          }

          // Month (01-12; January = 01)
          if (startsWithIgnoreCase(pattern, index, "MM")) {
            if (fillMode) {
              appendZeroPadded(output, value.getMonthValue(), "MM".length());
            } else {
              output.append(value.getMonthValue());
            }
            index += 2;
            continue;
          }

          // Name of month, padded with blanks
          if ((cap = match(pattern, index, "MONTH")) != null) {
            String month =
                cap.apply(value.getMonth().getDisplayName(TextStyle.FULL, Locale.ENGLISH));
            if (fillMode) {
              month = StringUtils.rightPad(month, "September".length(), " ");
            }
            output.append(month);
            index += 5;
            continue;
          }

          // Abbreviated name of month
          if ((cap = match(pattern, index, "MON")) != null) {
            String month = value.getMonth().getDisplayName(TextStyle.SHORT, Locale.ENGLISH);
            output.append(cap.apply(month));
            index += 3;
            continue;
          }
          break;

        case 'P':
          if ((cap = match(pattern, index, "PM")) != null) {
            String am = (value.getHour() < 12) ? "AM" : "PM";
            output.append(cap.apply(am));
            index += 2;
            continue;
          }

          if ((cap = match(pattern, index, "P.M.")) != null) {
            boolean isAM = value.getHour() < 12;
            String am = isAM ? "A.M." : "P.M.";
            output.append(cap.apply(am));
            index += 4;
            continue;
          }
          break;

        case 'Q':
        // Quarter of year (1, 2, 3, 4; January - March = 1)
        // if (match(format, index, "Q"))
        {
          int q = value.get(IsoFields.QUARTER_OF_YEAR);
          output.append(q);
          index += 1;
          continue;
        }

        case 'R':
          // Roman numeral month (I-XII; January = I)
          if ((cap = match(pattern, index, "RM")) != null) {
            output.append(cap.apply(RomanNumeral.format(value.getMonthValue())));
            index += 2;
            continue;
          }
          break;

        case 'S':
          // Seconds past midnight (0-86399)
          if (startsWithIgnoreCase(pattern, index, "SSSSS")) {
            int seconds = value.getNano() / 1_000_000_000;
            output.append(seconds);
            index += 5;
            continue;
          }

          // Second (0-59)
          if (startsWithIgnoreCase(pattern, index, "SS")) {
            appendZeroPadded(output, value.getSecond(), "SS".length());
            index += 2;
            continue;
          }

          // Signed century
          if (startsWithIgnoreCase(pattern, index, "SCC")) {
            int year = value.getYear();
            int century = year / 100;
            if ((year % 100) != 0) {
              century += 1;
            }

            if (fillMode) {
              output.append(year < 0 ? '-' : ' ');
              appendZeroPadded(output, FastMath.abs(century), "CC".length());
            } else {
              output.append(century);
            }

            index += 3;
            continue;
          }

          // 4-digit year; S prefixes BC dates with a minus sign.
          if (startsWithIgnoreCase(pattern, index, "SYYYY")) {
            int year = value.getYear();
            if (fillMode) {
              output.append(year < 0 ? '-' : ' ');
              appendZeroPadded(output, FastMath.abs(year), 4);
            } else {
              output.append(year);
            }
            index += 5;
            continue;
          }

          if ((cap = match(pattern, index, "SYEAR")) != null) {
            int year = value.getYear();
            if ( year <0 ) {
              output.append('-');
              year = FastMath.abs(year);
            }
            else if (fillMode) {
              output.append(' ');
            }
            output.append(cap.apply(NumberWords.convertYear(year)));
            index += 5;
            continue;
          }
          break;

        case 'T':
          // Time zone region
          if (startsWithIgnoreCase(pattern, index, "TZR")) {
            output.append(value.getZone().toString());
            index += 3;
            continue;
          }

          // Time zone region abbreviated with Daylight Saving Time information included
          if (startsWithIgnoreCase(pattern, index, "TZD")) {
            output.append(value.getZone().getDisplayName(TextStyle.SHORT_STANDALONE, Locale.ENGLISH));            
            index += 3;
            continue;
          }

          // Time zone hour
          if (startsWithIgnoreCase(pattern, index, "TZH")) {
            ZoneOffset offset = value.getOffset();
            int hours = offset.getTotalSeconds() / SECONDS_PER_HOUR;
            output.append(hours < 0 ? '-' : '+');
            appendZeroPadded(output, FastMath.abs(hours), "HH".length());
            index += 3;
            continue;
          }

          // Time zone minute
          if (startsWithIgnoreCase(pattern, index, "TZM")) {
            ZoneOffset offset = value.getOffset();
            int minutes = (offset.getTotalSeconds() / SECONDS_PER_MINUTE) % MINUTES_PER_HOUR;
            appendZeroPadded(output, FastMath.abs(minutes), "MM".length());
            index += 3;
            continue;
          }

          // Short time format
          if ((cap = match(pattern, index, "TS")) != null) {
            int h12 = (value.getHour() + 11) % 12 + 1;
            output.append(h12).append(':');
            appendZeroPadded(output, value.getMinute(), "MI".length());
            output.append(':');
            appendZeroPadded(output, value.getSecond(), "SS".length());
            output.append(' ');
            String am = (value.getHour() < 12) ? "AM" : "PM";
            output.append(cap.apply(am));
            index += 2;
            continue;
          }
          break;

        case 'W':
          // Week of year (1-53) where week 1 starts on the first day of the year and
          // continues to the seventh day of the year.
          if (startsWithIgnoreCase(pattern, index, "WW")) {
            int weekOfYear = value.get(ChronoField.ALIGNED_WEEK_OF_YEAR);            
            if (fillMode) {
              appendZeroPadded(output,weekOfYear,2);
            } else {
              output.append(weekOfYear);
            }
            index += 2;
            continue;
          }
          break;

        case 'X':
        // Local radix character
        // if (match(format, index, "X"))
        {
          output.append(DecimalFormatSymbols.getInstance().getDecimalSeparator());
          index += 1;
          continue;
        }

        case 'Y':
          // 4-digit year
          if (startsWithIgnoreCase(pattern, index, "YYYY", "RRRR")) {
            int year = FastMath.abs(value.getYear());
            if (fillMode) {
              appendZeroPadded(output, year, 4);
            } else {
              output.append(year);
            }
            index += 4;
            continue;
          }

          // Last 3 digits of year.
          if (startsWithIgnoreCase(pattern, index, "YYY")) {
            int year = FastMath.abs(value.getYear());
            appendZeroPadded(output, year % 1000, 3);
            index += 3;
            continue;
          }
          // Last 2 digits of year.
          if (startsWithIgnoreCase(pattern, index, "YY", "RR")) {
            int year = FastMath.abs(value.getYear());
            appendZeroPadded(output, year % 100, 2);
            index += 2;
            continue;
          }

          // Year with comma in this position.
          if (startsWithIgnoreCase(pattern, index, "Y,YYY")) {
            int year = FastMath.abs(value.getYear());
            output.append(new DecimalFormat("#,###").format(year));
            index += 5;
            continue;
          }

          // Year
          if ((cap = match(pattern, index, "YEAR")) != null) {
            int year = FastMath.abs(value.getYear());
            output.append(cap.apply(NumberWords.convertYear(year)));
            index += 4;
            continue;
          }

          // Last 1 digit of year.
          // if (match(format, index, "Y"))
          int year = FastMath.abs(value.getYear());
          output.append(year % 10);
          index += "Y".length();
          continue;

        case ' ':
        case '-':
        case '/':
        case '=':
        case ':':
        case ',':
        case '.':
        case ';':
        case '(':
        case ')':
          output.append(c);
          index += 1;
          continue;

        default:
          break;
      }

      throw new IllegalFormatFlagsException(pattern);
    }

    return output.toString();
  }

  protected static int parseMonthName(String value, ParsePosition position) throws ParseException {

    // Rule to try alternate format MONTH
    // String str = parseString(value, position, MONTHS);

    int index = position.getIndex();
    int month = 1;
    for (String name : MONTHS) {
      if (value.regionMatches(true, index, name, 0, name.length())) {
        position.setIndex(index + name.length());
        return month;
      }
      month++;
    }

    // Rule to try alternate format MON
    month = 1;
    for (String name : SHORT_MONTHS) {
      if (value.regionMatches(true, index, name, 0, name.length())) {
        position.setIndex(index + name.length());
        return month;
      }
      month++;
    }

    position.setErrorIndex(index);
    
    throw new ParseException(BaseMessages.getString(PKG, "Expression.InvalidMonthName"), position.getIndex());
  }

  protected static int parseMonthRoman(String value, ParsePosition position) throws ParseException {
    int index = position.getIndex();

    int len = 0;
    while (len < 4) {
      char c = Character.toUpperCase(value.charAt(index + len));
      if (c != 'I' && c != 'V' && c != 'X') {
        break;
      }
      len++;
    }

    for (int i = 0; i < ROMAN_MONTHS.length; i++) {
      if (value.regionMatches(true, index, ROMAN_MONTHS[i], 0, len)) {
        position.setIndex(index + len);
        return i + 1;
      }
    }

    throw new ParseException("Invalid roman month when parsing date with format RM", index);
  }

  protected final IllegalArgumentException createInvalidDateFormat(final String error) {
    return new IllegalArgumentException(
        BaseMessages.getString(PKG, "Expression.InvalidDateFormat", error));
  }
  
  protected final ParseException createUnparsableDate(final String text, int index) {
    return new ParseException(
        BaseMessages.getString(PKG, "Expression.UnparsableDateWithFormat", text, pattern), index);
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (this == obj) {
      return true;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    ZonedDateTimeFormat other = (ZonedDateTimeFormat) obj;
    return pattern.equals(other.pattern);
  }

  @Override
  public int hashCode() {
    return pattern.hashCode();
  }

}
