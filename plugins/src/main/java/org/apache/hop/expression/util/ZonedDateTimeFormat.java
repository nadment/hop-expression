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
import org.apache.hop.expression.ExpressionError;
import org.apache.hop.i18n.BaseMessages;
import java.text.DecimalFormatSymbols;
import java.text.ParseException;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;
import java.time.format.TextStyle;
import java.time.temporal.ChronoField;
import java.time.temporal.IsoFields;
import java.time.temporal.JulianFields;
import java.util.ArrayList;
import java.util.List;
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

  private static final String[] SHORT_MONTHS =
      {"JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"};
  private static final String[] MONTHS = {"JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE",
      "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER"};

  private static abstract class Format {
    protected final boolean fillMode;
    protected final boolean exactMode;
    
    public Format(boolean fillMode, boolean exactMode) {
      this.fillMode = fillMode;
      this.exactMode = exactMode;
    }
    
    /**
     * Appends the value of the specified calendar to the output buffer based on the rule
     * implementation.
     *
     * @param buffer the output buffer
     * @param calendar calendar to be appended
     * @throws Exception if an error occurs.
     */
    public abstract void append(StringBuilder buffer, ZonedDateTime datetime) throws Exception;

    public void parse(DateTimeParser parser) throws ParseException {

    }
  }

  private static class StringFormat extends Format {
    private final String value;

    public StringFormat(final String value) {
      super(true, true);
      this.value = value;
    }

    @Override
    public void append(final StringBuilder buffer, final ZonedDateTime datetime) throws Exception {
      buffer.append(value);
    }

    @Override
    public void parse(final DateTimeParser parser) throws ParseException {
      parser.index += value.length();
    }
  }

  private static class CharFormat extends Format {
    private final char ch;

    public CharFormat(boolean exactMode, final char ch) {
      super(true, exactMode);
      this.ch = ch;
    }

    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      output.append(ch);
    }

    @Override
    public void parse(final DateTimeParser parser) throws ParseException {
      char c = parser.parseChar();
      if (exactMode && ch != c) {
        // TODO: Translate
        throw new ParseException("Parse date error", parser.index);
      }
    }
  }

  private static class AdBcFormat extends Format {
    private static final String[] AD_BC = {"AD", "A.D.", "BC", "B.C."};

    private String ad;
    private String bc;
    
    public AdBcFormat(Capitalization cap, String ad, String bc) {
      super(true, true);
      this.ad = cap.apply(ad);
      this.bc = cap.apply(bc);
    }

    @Override
    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      output.append((datetime.getYear() > 0) ? ad : bc);
    }

    @Override
    public void parse(DateTimeParser parser) throws ParseException {
      String str = parser.parseString(AD_BC);
      if (str != null) {
        if (str.charAt(0) == 'B') {
          parser.bc = true;
        }
        parser.isDayOfYear = false;
        parser.isEpochDay = false;
      }
    }
  }

  private static class CenturyFormat extends Format {
    private final boolean signed;

    public CenturyFormat(boolean fillMode, boolean signed) {
      super(fillMode, true);
      this.signed = signed;
    }

    @Override
    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      int year = datetime.getYear();
      int century = FastMath.abs(year) / 100;
      if ((year % 100) != 0) {
        century += 1;
      }

      if (signed) {
        if (year < 0) {
          output.append('-');
        } else if (fillMode) {
          output.append(' ');
        }
      }

      if (fillMode) {
        appendDigits(output, century);
      } else {
        output.append(century);
      }
    }
  }

  private static class WordYearFormat extends Format {
    private final Capitalization cap;
    private final boolean signed;

    public WordYearFormat(Capitalization cap, boolean fillMode, boolean signed) {
      super(fillMode, true);
      this.cap = cap;
      this.signed = signed;
    }

    @Override
    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      int year = datetime.getYear();
      int absyear = FastMath.abs(year);

      if (signed) {
        if (year < 0) {
          output.append('-');
        } else if (fillMode) {
          output.append(' ');
        }
      }
      output.append(cap.apply(NumberWords.convertYear(absyear)));
    }
  }

  private static class RomanMonthFormat extends Format {
    private static final String[] ROMAN_MONTHS =
        {"I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII"};

    private final Capitalization cap;

    public RomanMonthFormat(Capitalization cap) {
      super(true, true);
      this.cap = cap;
    }

    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      output.append(cap.apply(RomanNumeral.format(datetime.getMonthValue())));
    }

    @Override
    public void parse(final DateTimeParser parser) throws ParseException {
      int index = parser.index;

      int len = 0;
      while (len < 4) {
        char c = Character.toUpperCase(parser.text.charAt(index + len));
        if (c != 'I' && c != 'V' && c != 'X') {
          break;
        }
        len++;
      }

      for (int i = 0; i < ROMAN_MONTHS.length; i++) {
        if (parser.text.regionMatches(true, index, ROMAN_MONTHS[i], 0, len)) {
          parser.index += len;
          parser.month = i + 1;
          return;
        }
      }

      throw new ParseException("Invalid roman month when parsing date with format RM", index);
    }
  }

  private static class MeridianFormat extends Format {
    private static final String[] AM_PM = {"AM", "A.M.", "PM", "P.M."};

    private final String am;
    private final String pm;

    public MeridianFormat(Capitalization cap, String am, String pm) {
      super(true, true);
      this.am = cap.apply(am);
      this.pm = cap.apply(pm);
    }

    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      output.append((datetime.getHour() < 12) ? am : pm);
    }

    @Override
    public void parse(final DateTimeParser parser) throws ParseException {
      String str = parser.parseString(AM_PM);
      if (str != null) {
        if (str.charAt(0) == 'P')
          parser.isPM = true;
        parser.isHourFormat12 = true;
      }
    }
  }

  private static class TimeZoneRegionFormat extends Format {
    public TimeZoneRegionFormat() {
      super(true, true);
    }
    
    @Override
    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      output.append(datetime.getZone().toString());
    }

    @Override
    public void parse(final DateTimeParser parser) throws ParseException {
      int i = parser.index;
      char ch;
      while (i < parser.text.length()) {
        ch = parser.text.charAt(i);
        if (!(Character.isLetter(ch) || ch == '/'))
          break;
        i++;
      }

      String zone = parser.text.substring(parser.index, i);
      parser.zoneId = ZoneId.of(zone);
      parser.index = i;
    }
  }

  private static class TimeZoneAbbreviatedRegionFormat extends Format {
    public TimeZoneAbbreviatedRegionFormat() {
      super(true, true);
    }
    
    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      output.append(datetime.getZone().getDisplayName(TextStyle.SHORT_STANDALONE, Locale.ENGLISH));
    }
  }

  // Time zone hour [+-][0]0
  private static class TimeZoneHourFormat extends Format {

    public TimeZoneHourFormat() {
      super(true, true);
    }
    
    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      ZoneOffset offset = datetime.getOffset();
      int hours = offset.getTotalSeconds() / SECONDS_PER_HOUR;
      output.append(hours < 0 ? '-' : '+');
      appendDigits(output, FastMath.abs(hours));
    }

    @Override
    public void parse(final DateTimeParser parser) throws ParseException {

      // Skip space
      // int i = parser.position.getIndex();
      // if (Characters.isSpace(text.charAt(i)))
      // position.setIndex(++i);

      parser.timeZoneHour = parser.parseSignedInt(3);
      parser.isTimeZoneOffset = true;
    }
  }

  private static class TimeZoneMinuteFormat extends Format {
    
    public TimeZoneMinuteFormat() {
      super(true, true);
    }
    
    @Override
    public void append(final StringBuilder buffer, final ZonedDateTime datetime) throws Exception {
      ZoneOffset offset = datetime.getOffset();
      int minutes = (offset.getTotalSeconds() / SECONDS_PER_MINUTE) % MINUTES_PER_HOUR;
      appendDigits(buffer, FastMath.abs(minutes));
    }

    @Override
    public void parse(final DateTimeParser parser) throws ParseException {
      parser.timeZoneMinute = parser.parseInt(2);
      parser.isTimeZoneOffset = true;
    }
  }

  private static class DateLongFormat extends Format {
    private final DateTimeFormatter formatter;

    public DateLongFormat() {
      super(true, true);
      this.formatter = DateTimeFormatter.ofLocalizedDateTime(FormatStyle.FULL); // .withLocale(Locale.ENGLISH);
    }

    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      output.append(datetime.format(formatter));
    }
  }

  private static class DateShortFormat extends Format {
    private final DateTimeFormatter formatter;

    public DateShortFormat() {
      super(true, true);
      formatter = DateTimeFormatter.ofLocalizedDate(FormatStyle.MEDIUM); // .withLocale(Locale.ENGLISH);
    }

    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      output.append(datetime.format(formatter));
    }
  }

  private static class TimeFormat extends Format {
    private final DateTimeFormatter formatter;
    private final Capitalization cap;

    public TimeFormat(Capitalization cap) {
      super(true, true);
      this.cap = cap;
      this.formatter = DateTimeFormatter.ofLocalizedTime(FormatStyle.MEDIUM); // .withLocale(Locale.ENGLISH);
    }

    @Override
    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      output.append(cap.apply(datetime.format(formatter)));
    }
  }

  private static class JulianDayFormat extends Format {
    /** The offset from Julian to EPOCH DAY. */
    private static final long JULIAN_DAY_OFFSET = 2440588L;

    public JulianDayFormat() {
      super(true, true);
    }
    
    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      long julianDay = datetime.getLong(JulianFields.JULIAN_DAY);
      output.append(julianDay);
    }

    @Override
    public void parse(final DateTimeParser parser) {
      parser.epochDay = parser.parseInt(7) - JULIAN_DAY_OFFSET;
      parser.isDayOfYear = false;
      parser.isEpochDay = true;
    }
  }

  /**
   * Inner class to output the twelve hour field.
   */
  private static class Hour12Format extends Format {
    public Hour12Format() {
      super(true, true);
    }
    
    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      int h12 = (datetime.getHour() + 11) % 12 + 1;
      appendDigits(output, h12);
    }

    @Override
    public void parse(final DateTimeParser parser) {
      parser.hour = parser.parseInt(2);
      parser.isHourFormat12 = true;
    }
  }

  // Hour of day (1-23)
  private static class Hour24Format extends Format {
    public Hour24Format() {
      super(true, true);
    }
    
    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      appendDigits(output, datetime.getHour());
    }

    @Override
    public void parse(final DateTimeParser parser) throws ParseException {
      parser.hour = parser.parseInt(2);
      parser.isHourFormat12 = false;
    }
  }

  private static class MinuteFormat extends Format {
    public MinuteFormat() {
      super(true, true);
    }

    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      appendDigits(output, datetime.getMinute());
    }

    @Override
    public void parse(final DateTimeParser parser) {
      parser.minute = parser.parseInt(2);
    }
  }

  private static class DayOfMonthFormat extends Format {
    public DayOfMonthFormat(boolean fillMode, boolean exactMode) {
      super(fillMode, exactMode);
    }

    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      int day = datetime.getDayOfMonth();
      if (fillMode) {
        appendDigits(output, day);
      } else {
        output.append(day);
      }
    }

    @Override
    public void parse(final DateTimeParser parser) {
      if (exactMode) {
        parser.day = parser.parseExactInt(2);
      } else {
        parser.day = parser.parseInt(2);
      }
      parser.isDayOfYear = false;
      parser.isEpochDay = false;
    }
  }

  private static class MonthFormat extends Format {
    public MonthFormat(boolean fillMode, boolean exactMode) {
      super(fillMode, exactMode);
    }

    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      int month = datetime.getMonthValue();
      if (fillMode) {
        appendDigits(output, month);
      } else {
        output.append(month);
      }
    }

    @Override
    public void parse(final DateTimeParser parser) throws ParseException {
      if (exactMode) {
        parser.month = parser.parseExactInt(2);
      } else try {
        parser.month = parser.parseInt(2);
      } catch (NumberFormatException e) {
        // Rule to try alternate format MONTH and MON
        parseNameOfMonth(parser);
      }
      parser.isDayOfYear = false;
      parser.isEpochDay = false;
    }

    public void parseNameOfMonth(final DateTimeParser parser) throws ParseException {
      // Rule to try alternate format MONTH
      int index = parser.index;
      int month = 1;
      for (String name : MONTHS) {
        if (parser.text.regionMatches(true, index, name, 0, name.length())) {
          parser.index += name.length();
          parser.month = month;
          return;
        }
        month++;
      }

      // Rule to try alternate format MON
      month = 1;
      for (String name : SHORT_MONTHS) {
        if (parser.text.regionMatches(true, index, name, 0, name.length())) {
          parser.index += name.length();
          parser.month = month;
          return;
        }
        month++;
      }

      throw new ParseException(BaseMessages.getString(PKG, "Expression.InvalidMonthName"), index);
    }
  }

  private static class SecondOfDayFormat extends Format {
    public SecondOfDayFormat() {
      super(true, true);
    }
    
    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      int seconds = datetime.get(ChronoField.SECOND_OF_DAY);
      appendZeroPadded(output, seconds, 5);
    }
  }

  private static class SecondOfMinuteFormat extends Format {
    public SecondOfMinuteFormat() {
      super(true, true);
    }
    
    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      appendDigits(output, datetime.getSecond());
    }

    @Override
    public void parse(final DateTimeParser parser) {
      parser.second = parser.parseInt(2);
    }
  }

  // Day of year (1-366)
  private static class DayOfYearFormat extends Format {
    public DayOfYearFormat(boolean fillMode) {
      super(fillMode, true);
    }

    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      int doy = datetime.getDayOfYear();
      if (fillMode) {
        appendZeroPadded(output, doy, 3);
      } else {
        output.append(doy);
      }
    }

    @Override
    public void parse(final DateTimeParser parser) throws ParseException {
      parser.dayOfYear = parser.parseInt(3);
      parser.isDayOfYear = true;
      parser.isEpochDay = false;
    }
  }

  private static class DayOfWeekFormat extends Format {
    public DayOfWeekFormat() {
      super(true, true);
    }
    
    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      int dof = (datetime.getDayOfWeek().getValue() + 1) % 7;
      output.append((char) (dof + '0'));
    }
  }

  private static class WeekOfMonthFormat extends Format {
    public WeekOfMonthFormat() {
      super(true, true);
    }
    
    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      int week = datetime.get(ChronoField.ALIGNED_WEEK_OF_MONTH);
      output.append((char) (week + '0'));
    }
  }

  private static class WeekOfYearFormat extends Format {
    public WeekOfYearFormat(boolean fillMode) {
      super(fillMode, true);
    }

    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      int week = datetime.get(ChronoField.ALIGNED_WEEK_OF_YEAR);
      if (fillMode) {
        appendDigits(output, week);
      } else {
        output.append((char) (week + '0'));
      }
    }
  }

  private static class QuarterFormat extends Format {
    public QuarterFormat() {
      super(true, true);
    }
    
    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      int quarter = datetime.get(IsoFields.QUARTER_OF_YEAR);
      output.append((char) (quarter + '0'));
    }
  }

  private static class SignedYearFormat extends Format {
    public SignedYearFormat(boolean fillMode) {
      super(fillMode, true);
    }

    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      int year = datetime.getYear();
      int value = FastMath.abs(year);

      if (year < 0) {
        output.append('-');
      } else if (fillMode) {
        output.append(' ');
      }

      if (fillMode) {
        appendZeroPadded(output, value, 4);
      } else {
        output.append(value);
      }
    }

    @Override
    public void parse(final DateTimeParser parser) throws ParseException {
      parser.year = parser.parseSignedInt(5);
      parser.isEpochDay = false;
    }
  }

  private class YearFormat extends Format {
    private final int length;

    public YearFormat(boolean fillMode, boolean exactMode, int length) {
      super(fillMode, exactMode);
      this.length = length;
    }

    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      int year = datetime.getYear();
      int value = FastMath.abs(year);

      switch (length) {
        case 4:
          if (fillMode) {
            appendZeroPadded(output, value, 4);
          } else {
            output.append(value);
          }
          break;
        case 3:
          if (fillMode) {
            appendZeroPadded(output, value % 1000, 3);
          } else {
            output.append(value % 1000);
          }
          break;
        case 2:
          if (fillMode) {
            appendDigits(output, value % 100);
          } else {
            output.append(value % 100);
          }
          break;
        case 1:
          output.append(value % 10);
          break;
      }
    }

    @Override
    public void parse(final DateTimeParser parser) throws ParseException {
      if (length == 4) {
        parser.year = parser.parseInt(4);
      } else {
        int year = parser.parseInt(2);
        year += (year < twoDigitYearStart - 1900) ? 2000 : 1900;
        parser.year = year;

      }

      parser.isEpochDay = false;
    }
  }
  private class RoundYearFormat extends Format {
    private final int length;

    public RoundYearFormat(boolean fillMode, int length) {
      super(fillMode, true);
      this.length = length;
    }

    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      int year = datetime.getYear();
      int value = FastMath.abs(year);

      if (length == 4) {
        if (fillMode) {
          appendZeroPadded(output, value, 4);
        } else {
          output.append(value);
        }
      } else {
        if (fillMode) {
          appendDigits(output, value % 100);
        } else {
          output.append(value % 100);
        }
      }
    }

    @Override
    public void parse(final DateTimeParser parser) throws ParseException {

      int year = parser.parseInt(length);

      // Years between 00-49 will be given the 21st century (the year 2000)
      if (year >= 0 && year <= 49)
        year += 2000;
      // Years between 50-99 will be given the 20th century (the year 1900).
      else if (year >= 50 && year <= 99)
        year += 1900;

      parser.year = year;
      parser.isEpochDay = false;
    }
  }

  private static class IsoYearFormat extends Format {
    private final int length;

    public IsoYearFormat(boolean fillMode, int length) {
      super(fillMode, true);
      this.length = length;
    }

    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      int year = FastMath.abs(datetime.get(IsoFields.WEEK_BASED_YEAR));

      switch (length) {
        case 4:
          if (fillMode) {
            appendZeroPadded(output, year, 4);
          } else {
            output.append(year);
          }
          break;
        case 3:
          if (fillMode) {
            appendZeroPadded(output, year % 1000, 3);
          } else {
            output.append(year % 1000);
          }
          break;
        case 2:
          if (fillMode) {
            appendDigits(output, year % 100);
          } else {
            output.append(year % 100);
          }
          break;
        case 1:
          output.append(year % 10);
          break;
      }
    }
  }

  private static class IsoWeekFormat extends Format {
    public IsoWeekFormat(boolean fillMode) {
      super(fillMode, true);
    }

    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      int week = datetime.get(IsoFields.WEEK_OF_WEEK_BASED_YEAR);
      if (fillMode) {
        appendDigits(output, week);
      } else {
        output.append((char) (week + '0'));
      }
    }
  }

  private static class NanoFormat extends Format {
    private final int scale;

    public NanoFormat(int scale) {
      super(true, true);
      this.scale = scale;
    }

    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      int nano = datetime.getNano();
      if (scale < 9) {
        nano /= (int) FastMath.pow(10d, 9 - scale);
      }
      appendZeroPadded(output, nano, scale);
    }

    @Override
    public void parse(final DateTimeParser parser) {
      int nano = parser.parseInt(scale);
      if (scale < 9) {
        nano *= (int) FastMath.pow(10d, 9 - scale);
      }
      parser.nano = nano;
    }
  }

  private static class NameOfDayFormat extends Format {
    private final Capitalization cap;
    private final TextStyle style;

    public NameOfDayFormat(Capitalization cap, boolean fillMode, TextStyle style) {
     super(fillMode, true);
      this.cap = cap;
      this.style = style;
    }

    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      String name = cap.apply(datetime.getDayOfWeek().getDisplayName(style, Locale.ENGLISH));
      if (fillMode) {
        name = StringUtils.rightPad(name, "Wednesday".length(), ' ');
      }

      output.append(cap.apply(name));
    }
  }

  private static class NameOfMonthFormat extends MonthFormat {
    private final Capitalization cap;
    private final TextStyle style;

    public NameOfMonthFormat(Capitalization cap, boolean fillMode, boolean exactMode,
        TextStyle style) {
      super(fillMode, exactMode);
      this.cap = cap;
      this.style = style;
    }

    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      String name = datetime.getMonth().getDisplayName(style, Locale.ENGLISH);
      name = cap.apply(name);
      // Abbreviated mode doesn't use fillMode
      if (fillMode) {
        name = StringUtils.rightPad(name, "September".length(), ' ');
      }
      output.append(name);
    }

    @Override
    public void parse(final DateTimeParser parser) throws ParseException {
      super.parseNameOfMonth(parser);
    }
  }

  /** Minutes per hour. */
  private static final int MINUTES_PER_HOUR = 60;
  /** Seconds per minute. */
  private static final int SECONDS_PER_MINUTE = 60;
  /** Seconds per hour. */
  private static final int SECONDS_PER_HOUR = SECONDS_PER_MINUTE * MINUTES_PER_HOUR;

  /**
   * The pattern.
   */
  private final String pattern;

  /**
   * The locale.
   */
  private final Locale locale;

  /**
   * The parsed formats.
   */
  private transient Format[] formats;

  private int twoDigitYearStart = 1970;

  public ZonedDateTimeFormat(final String pattern) {
    this(pattern, Locale.getDefault());
    this.formats = formats();
  }

  public ZonedDateTimeFormat(final String pattern, final Locale locale) {
    this.pattern = pattern;
    this.locale = locale;
    this.formats = formats();
  }

  protected Format[] formats() {
    final List<Format> list = new ArrayList<>();
    int length = pattern.length();
    int index = 0;
    boolean fillMode = true;
    boolean exactMode = false;
    Capitalization cap;

    while (index < length) {

      // Fill mode modifier; toggles between compact and fill modes for any elements
      // following the modifier in the model
      if (startsWithIgnoreCase(pattern, index, "FM")) {
        fillMode = !fillMode;
        index += 2;
        continue;
      }

      // Exact match modifier; toggles between lax and exact match modes for any
      // elements following the modifier in the model
      if (startsWithIgnoreCase(pattern, index, "FX")) {
        exactMode = !exactMode;
        index += 2;
        continue;
      }

      // Special characters
      char ch = pattern.charAt(index);
      if (" =/\\\\-_:,.;()".indexOf(ch) >= 0) {
        list.add(new CharFormat(exactMode, ch));
        index++;
        continue;
      }

      // Literal text
      if (ch == '"') {
        index++;
        for (int start = index; index < length; index++) {
          ch = pattern.charAt(index);
          if (ch == '"') {
            list.add(new StringFormat(pattern.substring(start, index)));
            index++;
            break;
          }
        }
        continue;
      }

      // AD indicator without periods
      if ((cap = match(pattern, index, "AD", "BC")) != null) {
        list.add(new AdBcFormat(cap, "AD", "BC"));
        index += 2;
        continue;
      }

      // AD indicator with periods
      if ((cap = match(pattern, index, "A.D.", "B.C.")) != null) {
        list.add(new AdBcFormat(cap, "A.D.", "B.C."));
        index += 4;
        continue;
      }

      if ((cap = match(pattern, index, "AM", "PM")) != null) {
        list.add(new MeridianFormat(cap, "AM", "PM"));
        index += 2;
        continue;
      }

      if ((cap = match(pattern, index, "A.M.", "P.M.")) != null) {
        list.add(new MeridianFormat(cap, "A.M.", "P.M."));
        index += 4;
        continue;
      }

      // Fractional seconds
      if (startsWithIgnoreCase(pattern, index, "FF1", "FF2", "FF3", "FF4", "FF5", "FF6", "FF7",
          "FF8", "FF9")) {
        int digit = pattern.charAt(index + 2) - '0';
        list.add(new NanoFormat(digit));
        index += 3;
        continue;
      }

      // FF is equivalent to FF6
      if (startsWithIgnoreCase(pattern, index, "FF")) {
        list.add(new NanoFormat(6));
        index += 2;
        continue;
      }

      // Hour of day in 24 hour format (0-23)
      if (startsWithIgnoreCase(pattern, index, "HH24")) {
        list.add(new Hour24Format());
        index += 4;
        continue;
      }
      // Hour of day in 12 hour format (1-12)
      if (startsWithIgnoreCase(pattern, index, "HH12")) {
        list.add(new Hour12Format());
        index += 4;
        continue;
      }

      // Hour of day in 12 hour format (1-12)
      if (startsWithIgnoreCase(pattern, index, "HH")) {
        list.add(new Hour12Format());
        index += 2;
        continue;
      }

      // Week of year (1-52 or 1-53) based on the ISO standard
      if (startsWithIgnoreCase(pattern, index, "IW")) {
        list.add(new IsoWeekFormat(fillMode));
        index += 2;
        continue;
      }

      // 4-digit year based on the ISO standard
      if (startsWithIgnoreCase(pattern, index, "IYYY")) {
        list.add(new IsoYearFormat(fillMode, 4));
        index += 4;
        continue;
      }

      // Last 3 digits of ISO year
      if (startsWithIgnoreCase(pattern, index, "IYY")) {
        list.add(new IsoYearFormat(fillMode, 3));
        index += 3;
        continue;
      }

      // Last 2 digits of ISO year
      if (startsWithIgnoreCase(pattern, index, "IY")) {
        list.add(new IsoYearFormat(fillMode, 2));
        index += 2;
        continue;
      }

      // Last 1 digits of ISO year
      if (startsWithIgnoreCase(pattern, index, "I")) {
        list.add(new IsoYearFormat(fillMode, 1));
        index += 2;
        continue;
      }

      // Minute (0-59)
      if (startsWithIgnoreCase(pattern, index, "MI")) {
        list.add(new MinuteFormat());
        index += 2;
        continue;
      }

      // Quarter of year (1, 2, 3, 4; January - March = 1)
      if (startsWithIgnoreCase(pattern, index, "Q")) {
        list.add(new QuarterFormat());
        index += 1;
        continue;
      }

      // Month (01-12; January = 01)
      if (startsWithIgnoreCase(pattern, index, "MM")) {
        list.add(new MonthFormat(fillMode, exactMode));
        index += 2;
        continue;
      }

      // Full name of month, padded with blanks
      if ((cap = match(pattern, index, "MONTH")) != null) {
        list.add(new NameOfMonthFormat(cap, fillMode, exactMode, TextStyle.FULL));
        index += 5;
        continue;
      }

      // Abbreviated name of month
      if ((cap = match(pattern, index, "MON")) != null) {
        list.add(new NameOfMonthFormat(cap, false, exactMode, TextStyle.SHORT));
        index += 3;
        continue;
      }

      // Roman numeral month (I-XII; January = I)
      if ((cap = match(pattern, index, "RM")) != null) {
        list.add(new RomanMonthFormat(cap));
        index += 2;
        continue;
      }

      // Rounded 4-digit year
      if (startsWithIgnoreCase(pattern, index, "RRRR")) {
        list.add(new RoundYearFormat(fillMode, 4));
        index += 4;
        continue;
      }

      // Rounded 2-digit year
      if (startsWithIgnoreCase(pattern, index, "RR")) {
        list.add(new RoundYearFormat(fillMode, 2));
        index += 2;
        continue;
      }

      // Seconds of day (0-86399)
      if (startsWithIgnoreCase(pattern, index, "SSSSS")) {
        list.add(new SecondOfDayFormat());
        index += 5;
        continue;
      }

      // Second of minute (0-59)
      if (startsWithIgnoreCase(pattern, index, "SS")) {
        list.add(new SecondOfMinuteFormat());
        index += 2;
        continue;
      }

      // Signed century
      if (startsWithIgnoreCase(pattern, index, "SCC")) {
        list.add(new CenturyFormat(fillMode, true));
        index += 3;
        continue;
      }

      // 4-digit year; S prefixes BC dates with a minus sign.
      if (startsWithIgnoreCase(pattern, index, "SYYYY")) {
        list.add(new SignedYearFormat(fillMode));
        index += 5;
        continue;
      }

      if ((cap = match(pattern, index, "SYEAR")) != null) {
        list.add(new WordYearFormat(cap, fillMode, true));
        index += 5;
        continue;
      }

      // Century
      if (startsWithIgnoreCase(pattern, index, "CC")) {
        list.add(new CenturyFormat(fillMode, false));
        index += 2;
        continue;
      }

      // Abbreviated name of day
      if ((cap = match(pattern, index, "DY")) != null) {
        list.add(new NameOfDayFormat(cap, false, TextStyle.SHORT));
        index += 2;
        continue;
      }

      // Full name of day
      if ((cap = match(pattern, index, "DAY")) != null) {
        list.add(new NameOfDayFormat(cap, fillMode, TextStyle.FULL));
        index += 3;
        continue;
      }

      // Long date format 'Tuesday, April 12, 1952 AD'
      if (startsWithIgnoreCase(pattern, index, "DL")) {
        list.add(new DateLongFormat());
        index += 2;
        continue;
      }

      // Short date format 'MM/DD/RRRR'.
      if (startsWithIgnoreCase(pattern, index, "DS")) {
        list.add(new DateShortFormat());
        index += 2;
        continue;
      }

      // Day of year (1-366)
      if (startsWithIgnoreCase(pattern, index, "DDD")) {
        list.add(new DayOfYearFormat(fillMode));
        index += 3;
        continue;
      }

      // Day of month (1-31)
      if (startsWithIgnoreCase(pattern, index, "DD")) {
        list.add(new DayOfMonthFormat(fillMode, exactMode));
        index += 2;
        continue;
      }

      // Day of week (1=Sunday-7)
      if (startsWithIgnoreCase(pattern, index, "D")) {
        list.add(new DayOfWeekFormat());
        index += 1;
        continue;
      }

      // Julian day; the number of days since January 1, 4712 BC
      if (startsWithIgnoreCase(pattern, index, "J")) {
        list.add(new JulianDayFormat());
        index += 1;
        continue;
      }

      // Year
      if ((cap = match(pattern, index, "YEAR")) != null) {
        list.add(new WordYearFormat(cap, fillMode, false));
        index += 4;
        continue;
      }

      // 4-digit year
      if (startsWithIgnoreCase(pattern, index, "YYYY")) {
        list.add(new YearFormat(fillMode, exactMode, 4));
        index += 4;
        continue;
      }

      // Last 3 digits of year.
      if (startsWithIgnoreCase(pattern, index, "YYY")) {
        list.add(new YearFormat(fillMode, exactMode, 3));
        index += 3;
        continue;
      }
      // Last 2 digits of year.
      if (startsWithIgnoreCase(pattern, index, "YY", "RR")) {
        list.add(new YearFormat(fillMode, exactMode, 2));
        index += 2;
        continue;
      }

      // Last 1 digit of year.
      if (startsWithIgnoreCase(pattern, index, "Y", "R")) {
        list.add(new YearFormat(fillMode, exactMode, 1));
        index += 1;
        continue;
      }

      // Aligned week of year (1-53) where week 1 starts on the first day of the year and
      // continues to the seventh day of the year.
      if (startsWithIgnoreCase(pattern, index, "WW")) {
        list.add(new WeekOfYearFormat(fillMode));
        index += 2;
        continue;
      }

      // Aligned week of month (1-5) where week 1 starts on the first day of the month and ends on
      // the seventh.
      if (startsWithIgnoreCase(pattern, index, "W")) {
        list.add(new WeekOfMonthFormat());
        index += 2;
        continue;
      }

      // Time format
      if ((cap = match(pattern, index, "TS")) != null) {
        list.add(new TimeFormat(cap));
        index += 2;
        continue;
      }

      // Time zone region
      if (startsWithIgnoreCase(pattern, index, "TZR")) {
        list.add(new TimeZoneRegionFormat());
        index += 3;
        continue;
      }

      // Time zone region abbreviated with Daylight Saving Time information included
      if (startsWithIgnoreCase(pattern, index, "TZD")) {
        list.add(new TimeZoneAbbreviatedRegionFormat());
        index += 3;
        continue;
      }

      // Time zone hour
      if (startsWithIgnoreCase(pattern, index, "TZH")) {
        list.add(new TimeZoneHourFormat());
        index += 3;
        continue;
      }

      // Time zone minute
      if (startsWithIgnoreCase(pattern, index, "TZM")) {
        list.add(new TimeZoneMinuteFormat());
        index += 3;
        continue;
      }

      // Local radix character
      if (startsWithIgnoreCase(pattern, index, "X")) {
        list.add(new CharFormat(exactMode,
            DecimalFormatSymbols.getInstance(locale).getDecimalSeparator()));
        index += 1;
        continue;
      }

      throw new IllegalArgumentException(
          ExpressionError.INVALID_DATE_FORMAT.message(pattern, index));
    }

    return list.toArray(new Format[0]);
  }

  public ZonedDateTime parse(String text) throws ParseException {

    DateTimeParser parser = new DateTimeParser(text);

    // start at the first not white space symbol
    // for (int start = position.getIndex(); start < text.length(); start++) {
    // if (!Character.isSpaceChar(text.charAt(start))) {
    // position.setIndex(start);
    // break;
    // }
    // }

    for (Format format : formats) {
      format.parse(parser);
    }

    // Build the date
    return parser.build();
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
    try {
      StringBuilder output = new StringBuilder();
      for (Format format : formats) {
        format.append(output, value);
      }
      return output.toString();
    } catch (Exception e) {
      throw new RuntimeException("Error formating datetime " + value + " with pattern " + pattern);
    }
  }

  protected final ParseException createUnparsableDate(final String text, int index) {
    return new ParseException(ExpressionError.UNPARSABLE_DATE_WITH_FORMAT.message(text, pattern), index);
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

  @Override
  public void setTwoDigitYearStart(int year) {
    this.twoDigitYearStart = year;
  }

  /**
   * Appends two digits to the given buffer.
   *
   * @param buffer the buffer to append to.
   * @param value the value to append digits from.
   */
  private static void appendDigits(final StringBuilder buffer, final int value) {
    buffer.append((char) (value / 10 + '0'));
    buffer.append((char) (value % 10 + '0'));
  }

  /**
   * Append a zero-padded number to a string builder.
   *
   * @param buffer the string builder
   * @param value the positive number to append
   * @param length the number of characters to append
   */
  private static void appendZeroPadded(final StringBuilder buffer, int value, int length) {
    String s = Integer.toString(value);
    length -= s.length();
    for (; length > 0; length--) {
      buffer.append('0');
    }
    buffer.append(s);
  }
}
