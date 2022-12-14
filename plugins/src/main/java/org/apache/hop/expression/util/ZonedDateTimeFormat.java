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
import java.time.temporal.TemporalField;
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

  private interface Format {
    /**
     * Appends the value of the specified calendar to the output buffer based on the rule
     * implementation.
     *
     * @param buffer the output buffer
     * @param calendar calendar to be appended
     * @throws Exception if an error occurs.
     */
    void append(StringBuilder buffer, ZonedDateTime datetime) throws Exception;
  }

  private static class StringFormat implements Format {
    private final String value;

    StringFormat(final String value) {
      this.value = value;
    }

    @Override
    public void append(final StringBuilder buffer, final ZonedDateTime datetime) throws Exception {
      buffer.append(value);
    }
  }

  private static class CharFormat implements Format {
    private final char ch;

    CharFormat(final char ch) {
      this.ch = ch;
    }

    @Override
    public void append(final StringBuilder output, final ZonedDateTime datetime) throws Exception {
      output.append(ch);
    }
  }

  private static class AdBcFormat implements Format {
    private String ad;
    private String bc;

    public AdBcFormat(Capitalization cap, String ad, String bc) {
      this.ad = cap.apply(ad);
      this.bc = cap.apply(bc);
    }

    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      output.append((datetime.getYear() > 0) ? ad : bc);
    }
  }

  private static class CenturyFormat implements Format {
    private final boolean fillMode;
    private final boolean signed;

    public CenturyFormat(boolean fillMode, boolean signed) {
      this.fillMode = fillMode;
      this.signed = signed;
    }

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

  private static class YearWordFormat implements Format {
    private final Capitalization cap;
    private final boolean fillMode;
    private final boolean signed;

    public YearWordFormat(Capitalization cap, boolean fillMode, boolean signed) {
      this.cap = cap;
      this.fillMode = fillMode;
      this.signed = signed;
    }

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

  private static class RomanMonthFormat implements Format {
    private final Capitalization cap;

    public RomanMonthFormat(Capitalization cap) {
      this.cap = cap;
    }

    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      output.append(cap.apply(RomanNumeral.format(datetime.getMonthValue())));
    }
  }

  private static class MeridianFormat implements Format {
    private final String am;
    private final String pm;

    public MeridianFormat(Capitalization cap, String am, String pm) {
      this.am = cap.apply(am);
      this.pm = cap.apply(pm);
    }

    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      output.append((datetime.getHour() < 12) ? am : pm);
    }
  }

  private static class TimeZoneRegionFormat implements Format {
    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      output.append(datetime.getZone().toString());
    }
  }

  private static class TimeZoneAbbreviatedRegionFormat implements Format {
    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      output.append(datetime.getZone().getDisplayName(TextStyle.SHORT_STANDALONE, Locale.ENGLISH));
    }
  }

  private static class TimeZoneHourFormat implements Format {
    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      ZoneOffset offset = datetime.getOffset();
      int hours = offset.getTotalSeconds() / SECONDS_PER_HOUR;
      output.append(hours < 0 ? '-' : '+');
      appendDigits(output, FastMath.abs(hours));
    }
  }

  private static class TimeZoneMinuteFormat implements Format {
    public void append(StringBuilder buffer, ZonedDateTime datetime) throws Exception {
      ZoneOffset offset = datetime.getOffset();
      int minutes = (offset.getTotalSeconds() / SECONDS_PER_MINUTE) % MINUTES_PER_HOUR;
      appendDigits(buffer, FastMath.abs(minutes));
    }
  }

  private static class DateLongFormat implements Format {
    private final DateTimeFormatter formatter;

    public DateLongFormat() {
      this.formatter = DateTimeFormatter.ofLocalizedDateTime(FormatStyle.FULL); // .withLocale(Locale.ENGLISH);
    }

    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      output.append(datetime.format(formatter));
    }
  }

  private static class DateShortFormat implements Format {
    private final DateTimeFormatter formatter;

    public DateShortFormat() {
      formatter = DateTimeFormatter.ofLocalizedDate(FormatStyle.MEDIUM); // .withLocale(Locale.ENGLISH);
    }

    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      output.append(datetime.format(formatter));
    }
  }

  private static class TimeFormat implements Format {
    private final DateTimeFormatter formatter;
    private final Capitalization cap;

    public TimeFormat(Capitalization cap) {
      this.cap = cap;
      this.formatter = DateTimeFormatter.ofLocalizedTime(FormatStyle.MEDIUM); // .withLocale(Locale.ENGLISH);
    }

    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      output.append(cap.apply(datetime.format(formatter)));
    }
  }

  private static class JulianDayFormat implements Format {
    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      long julianDay = datetime.getLong(JulianFields.JULIAN_DAY);
      output.append(julianDay);
    }
  }

  /**
   * Inner class to output the twelve hour field.
   */
  private static class Hour12Format implements Format {
    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      int h12 = (datetime.getHour() + 11) % 12 + 1;
      appendDigits(output, h12);
    }
  }

  private static class Hour24Format implements Format {
    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      appendDigits(output, datetime.getHour());
    }
  }

  private static class DayOfYearFormat implements Format {
    private final boolean fillMode;

    public DayOfYearFormat(boolean fillMode) {
      this.fillMode = fillMode;
    }

    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      int doy = datetime.getDayOfYear();
      if (fillMode) {
        appendZeroPadded(output, doy, 3);
      } else {
        output.append(doy);
      }
    }
  }

  private static class DayOfWeekFormat implements Format {
    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      int dof = (datetime.getDayOfWeek().getValue() + 1) % 7;
      output.append((char) (dof + '0'));
    }
  }

  private static class YearFormat implements Format {
    private final TemporalField field;
    private final boolean fillMode;
    private final boolean signed;
    private final int length;

    public YearFormat(TemporalField field, boolean fillMode, boolean signed, int length) {
      this.field = field;
      this.fillMode = fillMode;
      this.signed = signed;
      this.length = length;
    }

    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      int year = datetime.get(field);
      int value = FastMath.abs(year);

      if (signed) {
        if (year < 0) {
          output.append('-');
        } else if (fillMode) {
          output.append(' ');
        }
      }

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
  }

  private static class NanoFormat implements Format {
    private final int length;

    public NanoFormat(int length) {
      this.length = length;
    }

    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      int nanos = datetime.getNano();
      int scale = (int) (nanos * FastMath.pow(10d, length - 9d));
      appendZeroPadded(output, scale, length);
    }
  }

  private static class FieldFormat implements Format {
    private final TemporalField field;
    private final boolean fillMode;
    private final int length;

    public FieldFormat(TemporalField field, boolean fillMode) {
      this(field, fillMode, 2);
    }

    public FieldFormat(TemporalField field, boolean fillMode, int length) {
      this.field = field;
      this.fillMode = fillMode;
      this.length = length;
    }


    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      int value = datetime.get(field);
      if (fillMode) {
        if (length == 2)
          appendDigits(output, value);
        else
          appendZeroPadded(output, value, length);
      } else {
        output.append(value);
      }
    }
  }

  private static class NameOfDayFormat implements Format {
    private final Capitalization cap;
    private final boolean fillMode;
    private final TextStyle style;

    public NameOfDayFormat(Capitalization cap, boolean fillMode, boolean abbreviated) {
      this.cap = cap;
      this.fillMode = fillMode;
      this.style = (abbreviated) ? TextStyle.SHORT : TextStyle.FULL;
    }

    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      String name = cap.apply(datetime.getDayOfWeek().getDisplayName(style, Locale.ENGLISH));
      if (fillMode) {
        name = StringUtils.rightPad(name, "Wednesday".length(), ' ');
      }

      output.append(cap.apply(name));
    }
  }

  private static class NameOfMonthFormat implements Format {
    private final Capitalization cap;
    private final boolean fillMode;
    private final TextStyle style;

    public NameOfMonthFormat(Capitalization cap, boolean fillMode, boolean abbreviated) {
      this.cap = cap;
      this.fillMode = fillMode;
      this.style = (abbreviated) ? TextStyle.SHORT : TextStyle.FULL;
    }

    public void append(StringBuilder output, ZonedDateTime datetime) throws Exception {
      String name = datetime.getMonth().getDisplayName(style, Locale.ENGLISH);
      name = cap.apply(name);
      // Abbreviated mode doesn't use fillMode
      if (fillMode) {
        name = StringUtils.rightPad(name, "September".length(), ' ');
      }
      output.append(name);
    }
  }

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
    boolean laxMode = true;
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
        laxMode = !laxMode;
        index += 2;
        continue;
      }

      // Special characters
      char ch = pattern.charAt(index);
      if (" =/\\\\-_:,.;()".indexOf(ch) >= 0) {
        list.add(new CharFormat(ch));
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
        list.add(new FieldFormat(IsoFields.WEEK_OF_WEEK_BASED_YEAR, fillMode));
        index += 2;
        continue;
      }

      // 4-digit year based on the ISO standard
      if (startsWithIgnoreCase(pattern, index, "IYYY")) {
        list.add(new YearFormat(IsoFields.WEEK_BASED_YEAR, fillMode, false, 4));
        index += 4;
        continue;
      }

      // Last 3 digits of ISO year
      if (startsWithIgnoreCase(pattern, index, "IYY")) {
        list.add(new YearFormat(IsoFields.WEEK_BASED_YEAR, fillMode, false, 3));
        index += 3;
        continue;
      }

      // Last 2 digits of ISO year
      if (startsWithIgnoreCase(pattern, index, "IY")) {
        list.add(new YearFormat(IsoFields.WEEK_BASED_YEAR, fillMode, false, 2));
        index += 2;
        continue;
      }

      // Last 1 digits of ISO year
      if (startsWithIgnoreCase(pattern, index, "I")) {
        list.add(new YearFormat(IsoFields.WEEK_BASED_YEAR, fillMode, false, 1));
        index += 2;
        continue;
      }

      // Minute (0-59)
      if (startsWithIgnoreCase(pattern, index, "MI")) {
        list.add(new FieldFormat(ChronoField.MINUTE_OF_HOUR, true));
        index += 2;
        continue;
      }

      // Quarter of year (1, 2, 3, 4; January - March = 1)
      if (startsWithIgnoreCase(pattern, index, "Q")) {
        list.add(new FieldFormat(IsoFields.QUARTER_OF_YEAR, false));
        index += 1;
        continue;
      }

      // Month (01-12; January = 01)
      if (startsWithIgnoreCase(pattern, index, "MM")) {
        list.add(new FieldFormat(ChronoField.MONTH_OF_YEAR, fillMode));
        index += 2;
        continue;
      }

      // Full name of month, padded with blanks
      if ((cap = match(pattern, index, "MONTH")) != null) {
        list.add(new NameOfMonthFormat(cap, fillMode, false));
        index += 5;
        continue;
      }

      // Abbreviated name of month
      if ((cap = match(pattern, index, "MON")) != null) {
        list.add(new NameOfMonthFormat(cap, false, true));
        index += 3;
        continue;
      }

      // Roman numeral month (I-XII; January = I)
      if ((cap = match(pattern, index, "RM")) != null) {
        list.add(new RomanMonthFormat(cap));
        index += 2;
        continue;
      }

      // Seconds of day (0-86399)
      if (startsWithIgnoreCase(pattern, index, "SSSSS")) {
        list.add(new FieldFormat(ChronoField.SECOND_OF_DAY, true, 5));
        index += 5;
        continue;
      }

      // Second of minute (0-59)
      if (startsWithIgnoreCase(pattern, index, "SS")) {
        list.add(new FieldFormat(ChronoField.SECOND_OF_MINUTE, true));
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
        list.add(new YearFormat(ChronoField.YEAR, fillMode, true, 4));
        index += 5;
        continue;
      }

      if ((cap = match(pattern, index, "SYEAR")) != null) {
        list.add(new YearWordFormat(cap, fillMode, true));
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
        list.add(new NameOfDayFormat(cap, false, true));
        index += 2;
        continue;
      }

      // Full name of day
      if ((cap = match(pattern, index, "DAY")) != null) {
        list.add(new NameOfDayFormat(cap, fillMode, false));
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
        list.add(new FieldFormat(ChronoField.DAY_OF_MONTH, fillMode));
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
        list.add(new YearWordFormat(cap, fillMode, false));
        index += 4;
        continue;
      }

      // 4-digit year
      if (startsWithIgnoreCase(pattern, index, "YYYY", "RRRR")) {
        list.add(new YearFormat(ChronoField.YEAR, fillMode, false, 4));
        index += 4;
        continue;
      }

      // Last 3 digits of year.
      if (startsWithIgnoreCase(pattern, index, "YYY")) {
        list.add(new YearFormat(ChronoField.YEAR, fillMode, false, 3));
        index += 3;
        continue;
      }
      // Last 2 digits of year.
      if (startsWithIgnoreCase(pattern, index, "YY", "RR")) {
        list.add(new YearFormat(ChronoField.YEAR, fillMode, false, 2));
        index += 2;
        continue;
      }

      // Last 1 digit of year.
      if (startsWithIgnoreCase(pattern, index, "Y", "R")) {
        list.add(new YearFormat(ChronoField.YEAR, fillMode, false, 1));
        index += 1;
        continue;
      }

      // Aligned week of year (1-53) where week 1 starts on the first day of the year and
      // continues to the seventh day of the year.
      if (startsWithIgnoreCase(pattern, index, "WW")) {
        list.add(new FieldFormat(ChronoField.ALIGNED_WEEK_OF_YEAR, fillMode));
        index += 2;
        continue;
      }

      // Aligned week of month (1-5) where week 1 starts on the first day of the month and ends on
      // the seventh.
      if (startsWithIgnoreCase(pattern, index, "W")) {
        list.add(new FieldFormat(ChronoField.ALIGNED_WEEK_OF_MONTH, false));
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
        list.add(new CharFormat(DecimalFormatSymbols.getInstance(locale).getDecimalSeparator()));
        index += 1;
        continue;
      }

      throw new IllegalArgumentException(
          ExpressionError.INVALID_DATE_FORMAT.message(pattern, index));
    }

    return list.toArray(new Format[0]);
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
          // day = parseInt(text, position, "D".length());
          // isDayOfYear = false;
          // isEpochDay = false;
          // index += 1;
          /* NOT supported yet */
          throw new ParseException("Parsing format D not supported yet", index);
          // continue;
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
            if (Characters.isSpace(text.charAt(i)))
              position.setIndex(++i);
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
            while (i < text.length()) {
              ch = text.charAt(i);
              if (!(Character.isLetter(ch) || ch == '/'))
                break;
              i++;
            }

            String zone = text.substring(position.getIndex(), i);
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
            year += (year < twoDigitYearStart - 1900) ? 2000 : 1900;
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
    if (zoneId == null) {
      if (isTimeZoneOffset) {
        zoneId = ZoneOffset.ofHoursMinutes(timeZoneHour, timeZoneMinute);
      } else {
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

    throw new ParseException(BaseMessages.getString(PKG, "Expression.InvalidMonthName"),
        position.getIndex());
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
