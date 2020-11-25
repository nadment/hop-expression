package org.apache.hop.expression.util;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.ParseException;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;
import java.time.format.TextStyle;
import java.time.temporal.IsoFields;
import java.time.temporal.JulianFields;
import java.time.temporal.WeekFields;
import java.util.IllegalFormatFlagsException;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;

public class DateTimeFormat extends BaseFormat {

  // Specifies the “century start” year for 2-digit years. This parameter prevents
  // ambiguous dates when importing or converting data with the YY date format
  // component.
  private static final int TWO_DIGIT_CENTURY_START = 1970;

  /** The offset from Julian to EPOCH DAY. */
  private static final long JULIAN_DAY_OFFSET = 2440588L;

  private static final String[] ROMAN_MONTHS = {
    "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII"
  };

  private static final String[] SHORT_MONTHS = {
    "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
  };
  private static final String[] MONTHS = {
    "JANUARY",
    "FEBRUARY",
    "MARCH",
    "APRIL",
    "MAY",
    "JUNE",
    "JULY",
    "AUGUST",
    "SEPTEMBER",
    "OCTOBER",
    "NOVEMBER",
    "DECEMBER"
  };

  private static final String[] AM_PM = {"AM", "A.M.", "PM", "P.M."};

  private static final String[] AD_BC = {"AD", "A.D.", "BC", "B.C."};

  //	/**
  //	 * Hours per day.
  //	 */
  //	private static final int HOURS_PER_DAY = 24;
  /** Minutes per hour. */
  private static final int MINUTES_PER_HOUR = 60;
  //	/**
  //	 * Minutes per day.
  //	 */
  //	private static final int MINUTES_PER_DAY = MINUTES_PER_HOUR * HOURS_PER_DAY;
  /** Seconds per minute. */
  private static final int SECONDS_PER_MINUTE = 60;
  /** Seconds per hour. */
  private static final int SECONDS_PER_HOUR = SECONDS_PER_MINUTE * MINUTES_PER_HOUR;

  private final String format;

  private int currentYear;
  private int currentMonth;

  public static Instant parse(String value, String format) throws ParseException {

    DateTimeFormat parser = new DateTimeFormat(format);
    Instant instant = parser.parse(value);
    // System.out.println("ToDate('" + value + "','" + format + "')=" + instant);

    return instant;
  }

  public static String format(ZonedDateTime value, String format, Locale local) {
    DateTimeFormat formatter = new DateTimeFormat(format);
    return formatter.format(value, local);
  }

  public DateTimeFormat(String format) {
    this.format = format;
  }

  public Instant parse(String value) throws ParseException {

    // start at the first not white space symbol
    int start = 0;
    for (; start < value.length(); start++) {
      if (value.charAt(start) != ' ') break;
    }
    ParsePosition position = new ParsePosition(start);

    boolean bc = false;
    long epochDay = 0;
    int year = 0;
    int month = 0;
    int day = 0;
    int dayOfYear = 0;
    int hour = 0;
    int minute = 0;
    int second = 0;
    int nanos = 0;
    // TODO: parse time zone hour and minute
    int timeZoneHour = 0;
    int timeZoneMinute = 0;

    boolean isPM = false;
    boolean isTimeFormat12 = false;
    boolean isEpochDay = false;
    boolean isDayOfYear = false;
    boolean isTimeZoneHHMM = false;

    for (int index = 0; index < format.length(); ) {

      // Ignore case for parsing
      char c = Character.toUpperCase(format.charAt(index));

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
          // index++;
          position.setIndex(position.getIndex() + 1);
          continue;

          // TODO: Character string literals enclosed in double quotation marks.

        case 'A':
          // Meridian indicator
          if (match(format, index, "AM")) {
            String str = parseString(value, position, AM_PM);
            if (str == null) break;
            if (str.charAt(0) == 'P') isPM = true;
            isTimeFormat12 = true;
            index += 2;
            continue;
          }

          // Meridian indicator with period
          if (match(format, index, "A.M.")) {
            String str = parseString(value, position, AM_PM);
            if (str == null) break;
            if (str.charAt(0) == 'P') isPM = true;
            isTimeFormat12 = true;
            index += 4;
            continue;
          }

          // Era designator
          if (match(format, index, "AD")) {
            String str = parseString(value, position, AD_BC);
            if (str == null) break;
            if (str.charAt(0) == 'B') bc = true;
            isDayOfYear = false;
            isEpochDay = false;
            index += 2;
            continue;
          }

          // Era designator with period
          if (match(format, index, "A.D.")) {
            String str = parseString(value, position, AD_BC);
            if (str == null) break;
            if (str.charAt(0) == 'B') bc = true;
            isDayOfYear = false;
            isEpochDay = false;
            index += 4;
            continue;
          }
          break;

        case 'B':
          // Era designator
          if (match(format, index, "BC")) {
            String str = parseString(value, position, AD_BC);
            if (str == null) break;
            if (str.charAt(0) == 'B') bc = true;
            isDayOfYear = false;
            isEpochDay = false;
            index += 2;
            continue;
          }

          // Era designator with period
          if (match(format, index, "B.C.")) {
            String str = parseString(value, position, AD_BC);
            if (str == null) break;
            if (str.charAt(0) == 'B') bc = true;
            isDayOfYear = false;
            isEpochDay = false;
            index += 4;
            continue;
          }
          break;

        case 'D':
          // Day of year (1-366)
          if (match(format, index, "DDD")) {
            dayOfYear = parseInt(value, position, "DDD".length());
            isDayOfYear = true;
            isEpochDay = false;
            index += 3;
            continue;
          }

          // Day of month (1-31)
          if (match(format, index, "DD")) {
            day = parseInt(value, position, "DD".length());
            isDayOfYear = false;
            isEpochDay = false;
            index += 2;
            continue;
          }

          // FIXME: Day of week (1-7)
          {
            day = parseInt(value, position, "D".length());
            isDayOfYear = false;
            isEpochDay = false;
            index += 1;
            continue;
          }

        case 'F':
          // Fractional seconds FF("^(FF[0-9]?)"),
          throw new ParseException("Parsing format F not supported yet", 0);

        case 'J':
          {
            // Julian day; the number of days since Jan 1, 4712 BC.

            index += 1;
            epochDay = parseInt(value, position, 7) - JULIAN_DAY_OFFSET;
            isDayOfYear = false;
            isEpochDay = true;
            continue;
          }
        case 'M':
          // Minutes (0-59)
          if (match(format, index, "MI")) {
            minute = parseInt(value, position, 2);
            index += 2;
            continue;
          }

          // Month number (1-12)
          if (match(format, index, "MM")) {
            index += 2;

            try {
              month = parseInt(value, position, 2);
            } catch (NumberFormatException e) {
              // Rule to try alternate format MONTH and MON
              month = parseMonthName(value, position);
            }
            isDayOfYear = false;
            isEpochDay = false;
            continue;
          }
          // Full name of month (parse before MON)
          else if (match(format, index, "MONTH")) {
            index += 5;
            month = parseMonthName(value, position);
            isDayOfYear = false;
            isEpochDay = false;
            continue;
          }
          // Abbreviated name of month (parse after MONTH)
          else if (match(format, index, "MON")) {
            index += 3;
            month = parseMonthName(value, position);
            isDayOfYear = false;
            isEpochDay = false;
            continue;
          }
          break;

        case 'H':
          // Hour of day (1-23)
          if (match(format, index, "HH24")) {
            hour = parseInt(value, position, 2);
            isTimeFormat12 = false;
            index += 4;
            continue;
          }

          // Hour of day (1-12)
          if (match(format, index, "HH12")) {
            hour = parseInt(value, position, 2);
            isTimeFormat12 = true;
            index += 4;
            continue;
          }

          // Hour of day (1-12)
          if (match(format, index, "HH")) {
            hour = parseInt(value, position, 2);
            isTimeFormat12 = true;
            index += 2;
            continue;
          }
          break;

        case 'Q':
          /* NOT supported yet */
          throw new IllegalFormatFlagsException("Parsing format Q not supported yet");

        case 'R':
          // Roman numeral month (I-XII; January = I).
          if (match(format, index, "RM")) {
            index += 2;
            month = parseMonthRoman(value, position);
            continue;
          }

          // 4-digit year
          if (match(format, index, "RRRR")) {
            year = parseInt(value, position, 4);
            // Years between 00-49 will be given the 21st century (the year 2000)
            if (year >= 0 && year <= 49) year += 2000;
            // Years between 50-99 will be given the 20th century (the year 1900).
            else if (year >= 50 && year <= 99) year += 1900;
            isEpochDay = false;
            index += 4;
            continue;
          }

        case 'S':
          // Seconds
          if (match(format, index, "SS")) {
            second = parseInt(value, position, 2);
            index += 2;
            continue;
          }

          // 4-digit year; S prefixes BC dates with a minus sign
          if (match(format, index, "SYYYY")) {
            year = parseSignedInt(value, position, 5);
            isEpochDay = false;
            index += 5;
            continue;
          }
          break;

        case 'Y':
          // 4-digit year
          if (match(format, index, "YYYY")) {
            year = parseInt(value, position, 4);
            isEpochDay = false;
            index += 4;
            continue;
          }

          // Last 2-digit year
          if (match(format, index, "YY")) {
            year = parseInt(value, position, 2);
            year += (1900 + year > TWO_DIGIT_CENTURY_START) ? 2000 : 1900;
            isEpochDay = false;
            index += 2;
            continue;
          }
          break;
      }

      throw new ParseException(
          "Error parsing date '" + value + "' with format '" + format + '\'',
          position.getErrorIndex());
    }

    // Build the date
    LocalDate date = null;
    if (isEpochDay) {
      date = LocalDate.ofEpochDay(epochDay);
    } else {
      if (year == 0) {
        year = getCurrentYear();
      }
      if (bc) {
        year = 1 - year;
      }
      if (isDayOfYear) {
        date = LocalDate.ofYearDay(year, dayOfYear);
      } else {
        if (month == 0) {
          // Oracle uses current month as default
          month = getCurrentMonth();
        }

        if (day == 0) {
          // Oracle uses current day as default
          day = 1; // LocalDate.now().getDayOfMonth();
        }

        date = LocalDate.of(year, month, day);
      }
    }

    if (isTimeFormat12) {
      hour = hour % 12;
      if (isPM) {
        hour += 12;
      }
    }
    LocalTime time = LocalTime.of(hour, minute, second, nanos);

    LocalDateTime dt = LocalDateTime.of(date, time);

    if (isTimeZoneHHMM) {
      return dt.toInstant(ZoneOffset.ofHoursMinutes(timeZoneHour, timeZoneMinute));
    }

    return dt.toInstant(ZoneOffset.UTC);
  }

  private void queryCurrentYearAndMonth() {

    LocalDate date = LocalDate.now();

    currentYear = date.getYear();
    currentMonth = date.getMonthValue();
  }

  private int getCurrentYear() {
    if (currentYear == 0) {
      queryCurrentYearAndMonth();
    }
    return currentYear;
  }

  private int getCurrentMonth() {
    if (currentMonth == 0) {
      queryCurrentYearAndMonth();
    }
    return currentMonth;
  }

  /**
   * Emulates Oracle's TO_CHAR(datetime) function.
   *
   * <p>
   *
   * <table border="1">
   * <th>
   * <td>Input</td>
   * <td>Output</td>
   * <td>Closest {@link SimpleDateFormat} Equivalent</td></th>
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
   *
   * <p>See also TO_CHAR(datetime) and datetime format models in the Oracle documentation.
   *
   * @param value the date-time value to format
   * @param format the format pattern to use (if any)
   * @return the formatted timestamp
   */
  public String format(ZonedDateTime value, Locale local) {

    // System.out.println("to_char(" + value + "," + format + ")");

    StringBuilder output = new StringBuilder();
    boolean fillMode = true;

    for (int index = 0; index < format.length(); ) {

      Capitalization cap;

      // Ignore case for parsing
      char c = Character.toUpperCase(format.charAt(index));

      // Use first letter for optimization
      switch (c) {
        case '\"':
          // Literal text
          for (index = index + 1; index < format.length(); index++) {
            char ch = format.charAt(index);
            if (ch != '"') {
              output.append(ch);
            } else {
              index++;
              continue;
            }
          }
          break;

        case 'A':
          // AD indicator without periods
          if ((cap = matchCapitalization(format, index, "AD")) != null) {
            String era = (value.getYear() > 0) ? "AD" : "BC";
            output.append(cap.apply(era));
            index += 2;
            continue;
          }

          // AD indicator with periods
          if ((cap = matchCapitalization(format, index, "A.D.")) != null) {
            String era = (value.getYear() > 0) ? "A.D." : "B.C.";
            output.append(cap.apply(era));
            index += 4;
            continue;
          }

          if ((cap = matchCapitalization(format, index, "AM")) != null) {
            String am = (value.getHour() < 12) ? "AM" : "PM";
            output.append(cap.apply(am));
            index += 2;
            continue;
          }

          if ((cap = matchCapitalization(format, index, "A.M.")) != null) {
            boolean isAM = value.getHour() < 12;
            String am = isAM ? "A.M." : "P.M.";
            output.append(cap.apply(am));
            index += 4;
            continue;
          }
          break;

        case 'B':
          // AD indicator without periods
          if ((cap = matchCapitalization(format, index, "BC")) != null) {
            String era = (value.getYear() > 0) ? "AD" : "BC";
            output.append(cap.apply(era));
            index += 2;
            continue;
          }

          // AD indicator with periods
          if ((cap = matchCapitalization(format, index, "B.C.")) != null) {
            String era = (value.getYear() > 0) ? "A.D." : "B.C.";
            output.append(cap.apply(era));
            index += 4;
            continue;
          }
          break;

        case 'C':
          // Century
          if (match(format, index, "CC")) {
            int year = Math.abs(value.getYear());
            int century = year / 100;
            if (((int) year % 100) != 0) {
              century += 1;
            }
            padInt(output, century, "CC".length());
            index += 2;
            continue;
          }
          break;

        case 'D':
          // Day of year (1-366)
          if (match(format, index, "DDD")) {
            if (fillMode) {
              padInt(output, value.getDayOfYear(), "DDD".length());
            } else {
              output.append(value.getDayOfYear());
            }
            index += 3;
            continue;
          }

          // Day of month (1-31)
          if (match(format, index, "DD")) {

            if (fillMode) {
              padInt(output, value.getDayOfMonth(), "DD".length());
            } else {
              output.append(value.getDayOfMonth());
            }
            index += 2;
            continue;
          }

          // Long date format 'Tuesday, April 12, 1952 AD'
          if (match(format, index, "DL")) {
            DateTimeFormatter formatter =
                DateTimeFormatter.ofLocalizedDateTime(
                    FormatStyle.FULL); // .withLocale(Locale.ENGLISH);
            output.append(value.format(formatter));
            index += 2;
            continue;
          }

          // Short date format 'MM/DD/RRRR'.
          if (match(format, index, "DS")) {
            padInt(output, value.getMonthValue(), "DD".length());
            output.append('/');
            padInt(output, value.getDayOfMonth(), "MM".length());
            output.append('/');
            padInt(output, Math.abs(value.getYear()), "YYYY".length());
            index += 2;
            continue;
          }

          // Abbreviated name of day
          if ((cap = matchCapitalization(format, index, "DY")) != null) {
            String day = value.getDayOfWeek().getDisplayName(TextStyle.SHORT, Locale.ENGLISH);
            output.append(cap.apply(day));
            index += 2;
            continue;
          }

          // Name of day
          if ((cap = matchCapitalization(format, index, "DAY")) != null) {
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
          if (match(format, index, "D")) {
            output.append((value.getDayOfWeek().getValue() + 1) % 7);
            index += 1;
            continue;
          }
          break;

        case 'F':
          // Fractional seconds
          if (matchCapitalization(
                  format, index, "FF1", "FF2", "FF3", "FF4", "FF5", "FF6", "FF7", "FF8", "FF9")
              != null) {
            int x = format.charAt(index + 2) - '0';

            int nanos = value.getNano();

            int ff = (int) (nanos * Math.pow(10, x - 9));
            padInt(output, x, ff);
            index += 3;
            continue;
          }
          if (match(format, index, "FF")) {
            padInt(output, value.getNano(), 9);
            index += 2;
            continue;
          }

          // Fill mode modifier; toggles between compact and fill modes for any elements
          // following the modifier in the model.
          if (match(format, index, "FM")) {
            fillMode = !fillMode;
            index += 2;
            continue;
          }

          // TODO: Exact match modifier; toggles between lax and exact match modes for any
          // elements following the modifier in the model.
          if (match(format, index, "FX")) {
            index += 2;
            continue;
          }
          break;

        case 'H':
          // Hour of day in 24 hour format (0-23)
          if (match(format, index, "HH24")) {
            padInt(output, value.getHour(), 2);
            index += 4;
            continue;
          }
          // Hour of day in 12 hour format (1-12)
          if (match(format, index, "HH12")) {
            int h12 = (value.getHour() + 11) % 12 + 1;
            padInt(output, h12, 2);
            index += 4;
            continue;
          }
          // Hour of day in 12 hour format (1-12)
          if (match(format, index, "HH")) {
            int h12 = (value.getHour() + 11) % 12 + 1;
            padInt(output, h12, "HH".length());
            index += 2;
            continue;
          }
          break;

        case 'I':
          // 4-digit year based on the ISO standard.
          if (match(format, index, "IYYY")) {
            int weekYear = Math.abs(value.get(IsoFields.WEEK_BASED_YEAR));
            padInt(output, weekYear, 4);
            index += 4;
            continue;
          }

          // Last 3 digits of ISO year.
          if (match(format, index, "IYY")) {
            int weekYear = Math.abs(value.get(IsoFields.WEEK_BASED_YEAR));
            padInt(output, weekYear % 1000, 3);
            index += 3;
            continue;
          }

          // Last 2 digits of ISO year.
          if (match(format, index, "IY")) {
            int weekYear = Math.abs(value.get(IsoFields.WEEK_BASED_YEAR));
            padInt(output, weekYear % 100, 2);
            index += 2;
            continue;
          }

          // Week of year (1-52 or 1-53) based on the ISO standard
          if (match(format, index, "IW")) {
            int week = value.get(WeekFields.ISO.weekOfYear());
            output.append(week);
            index += 2;
            continue;
          }

          // Last 1 digit of ISO year.
          // if (match(format, index, "I"))
          {
            int weekYear = Math.abs(value.get(IsoFields.WEEK_BASED_YEAR));
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
          if (match(format, index, "MI")) {
            padInt(output, value.getMinute(), "MI".length());
            index += 2;
            continue;
          }

          // Month (01-12; January = 01)
          if (match(format, index, "MM")) {
            if (fillMode) {
              padInt(output, value.getMonthValue(), "MM".length());
            } else {
              output.append(value.getMonthValue());
            }
            index += 2;
            continue;
          }

          // Name of month, padded with blanks
          if ((cap = matchCapitalization(format, index, "MONTH")) != null) {
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
          if ((cap = matchCapitalization(format, index, "MON")) != null) {
            String month = value.getMonth().getDisplayName(TextStyle.SHORT, Locale.ENGLISH);
            output.append(cap.apply(month));
            index += 3;
            continue;
          }
          break;

        case 'P':
          if ((cap = matchCapitalization(format, index, "PM")) != null) {
            String am = (value.getHour() < 12) ? "AM" : "PM";
            output.append(cap.apply(am));
            index += 2;
            continue;
          }

          if ((cap = matchCapitalization(format, index, "P.M.")) != null) {
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
          if ((cap = matchCapitalization(format, index, "RM")) != null) {
            output.append(cap.apply(formatRomanNumeral(value.getMonthValue())));
            index += 2;
            continue;
          }
          break;

        case 'S':
          // Seconds past midnight (0-86399)
          if (match(format, index, "SSSSS")) {
            int seconds = (int) (value.getNano() / 1_000_000_000);
            output.append(seconds);
            index += 5;
            continue;
          }

          // Second (0-59)
          if (match(format, index, "SS")) {
            padInt(output, value.getSecond(), "SS".length());
            index += 2;
            continue;
          }

          // Signed century
          if (match(format, index, "SCC")) {
            int year = value.getYear();
            int century = year / 100;
            if (((int) year % 100) != 0) {
              century += 1;
            }

            if (fillMode) {
              output.append(year < 0 ? '-' : ' ');
              padInt(output, Math.abs(century), "CC".length());
            } else {
              output.append(century);
            }

            index += 3;
            continue;
          }

          // 4-digit year; S prefixes BC dates with a minus sign.
          if (match(format, index, "SYYYY")) {
            int year = value.getYear();
            if (fillMode) {
              output.append(year < 0 ? '-' : ' ');
              padInt(output, Math.abs(year), 4);
            } else {
              output.append(year);
            }
            index += 5;
            continue;
          }

          if ((cap = matchCapitalization(format, index, "SYEAR")) != null) {
            int year = value.getYear();
            output.append(year < 0 ? '-' : ' ');
            output.append(cap.apply(formatWord(year)));
            index += 5;
            continue;
          }
          break;

        case 'T':
          // Time zone region
          if (match(format, index, "TZR")) {
            output.append(value.getZone().toString());
            index += 3;
            continue;
          }

          // TODO: Time zone region with Daylight Saving Time information included
          if (match(format, index, "TZD")) {
            // output.append(getTimeZone(value, true));
            index += 3;
            continue;
          }

          // Time zone hour
          if (match(format, index, "TZH")) {
            ZoneOffset offset = value.getOffset();
            int hours = offset.getTotalSeconds() / SECONDS_PER_HOUR;
            output.append(hours < 0 ? '-' : '+');
            padInt(output, Math.abs(hours), "HH".length());
            index += 3;
            continue;
          }

          // Time zone minute
          if (match(format, index, "TZM")) {
            ZoneOffset offset = value.getOffset();
            int minutes = (offset.getTotalSeconds() / SECONDS_PER_MINUTE) % MINUTES_PER_HOUR;
            padInt(output, Math.abs(minutes), "MM".length());
            index += 3;
            continue;
          }

          // Short time format
          if ((cap = matchCapitalization(format, index, "TS")) != null) {
            int h12 = (value.getHour() + 11) % 12 + 1;
            output.append(h12).append(':');
            padInt(output, value.getMinute(), "MI".length());
            output.append(':');
            padInt(output, value.getSecond(), "SS".length());
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
          if (match(format, index, "WW")) {
            int weekOfYear = value.get(WeekFields.SUNDAY_START.weekOfYear());
            output.append(weekOfYear);
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
          if (match(format, index, "YYYY") || match(format, index, "RRRR")) {
            int year = Math.abs(value.getYear());
            if (fillMode) {
              padInt(output, year, 4);
            } else {
              output.append(year);
            }
            index += 4;
            continue;
          }

          // Last 3 digits of year.
          if (match(format, index, "YYY")) {
            int year = Math.abs(value.getYear());
            padInt(output, year % 1000, 3);
            index += 3;
            continue;
          }
          // Last 2 digits of year.
          if (match(format, index, "YY") || match(format, index, "RR")) {
            int year = Math.abs(value.getYear());
            padInt(output, year % 100, 2);
            index += 2;
            continue;
          }

          // Year with comma in this position.
          if (match(format, index, "Y,YYY")) {
            int year = Math.abs(value.getYear());
            output.append(new DecimalFormat("#,###").format(year));
            index += 5;
            continue;
          }

          // Year
          if ((cap = matchCapitalization(format, index, "YEAR")) != null) {
            int year = Math.abs(value.getYear());
            output.append(cap.apply(formatWord(year)));
            index += 4;
            continue;
          }

          // Last 1 digit of year.
          // if (match(format, index, "Y"))
          int year = Math.abs(value.getYear());
          output.append(year % 10);
          index += "Y".length();
          continue;

        default:
          // Anything else
          if (!Characters.isAlphaOrDigit(c)) {
            output.append(c);
            index += 1;
            continue;
          }
      }

      throw new IllegalFormatFlagsException(format);
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

    throw new ParseException("Invalid month name", position.getIndex());
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
}
