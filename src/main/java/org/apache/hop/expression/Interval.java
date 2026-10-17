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
package org.apache.hop.expression;

import java.io.Serial;
import java.io.Serializable;
import java.math.BigInteger;
import java.time.ZonedDateTime;
import java.util.EnumSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.StringUtils;
import org.apache.hop.expression.util.Characters;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * An interval represents a duration of time which can be used in date/time arithmetic.
 *
 * <p>Like PostgreSQL, an interval has three independent signed parts: a number of months, a number
 * of days and a time (seconds and nanoseconds). The parts can have different signs, for example
 * {@code 1 month -1 day}. A day is a calendar day, so adding one day to a timestamp keeps its local
 * time across a daylight saving time change, while adding 24 hours adds an exact duration.
 *
 * <p>For comparison, a month is 30 days and a day is 24 hours, so {@code 1 month} is equal to
 * {@code 30 days}.
 */
@NullMarked
public class Interval implements Serializable, Comparable<Interval> {
  @Serial private static final long serialVersionUID = 2L;

  /** The number of months per year. */
  static final long MONTHS_PER_YEAR = 12;

  /** The number of days per month, used to compare intervals. */
  static final long DAYS_PER_MONTH = 30;

  /** The number of seconds per minute. */
  static final long SECONDS_PER_MINUTE = 60;

  /** The number of seconds per hour. */
  static final long SECONDS_PER_HOUR = 60 * SECONDS_PER_MINUTE;

  /** The number of seconds per day. */
  static final long SECONDS_PER_DAY = 24 * SECONDS_PER_HOUR;

  /** The number of nanoseconds per second. */
  static final long NANOS_PER_SECOND = 1_000_000_000L;

  private static final EnumSet<TimeUnit> UNITS =
      EnumSet.of(
          TimeUnit.YEAR,
          TimeUnit.QUARTER,
          TimeUnit.MONTH,
          TimeUnit.WEEK,
          TimeUnit.DAY,
          TimeUnit.HOUR,
          TimeUnit.MINUTE,
          TimeUnit.SECOND);

  /**
   * The format of {@link #toString()}, the days and the time can have their own sign, otherwise
   * they take the leading sign.
   */
  private static final Pattern PATTERN_YTS =
      Pattern.compile(
          "^([+-])?(\\d+)-(\\d+) (?:([+-])?(\\d+) )?([+-])?(\\d+):(\\d+):(\\d+)(?:\\.(\\d+))?$");

  private static final Pattern PATTERN_YTM = Pattern.compile("^([+-])?(\\d+)-(\\d+)$");
  private static final Pattern PATTERN_DTS =
      Pattern.compile("^([+-])?(?:(\\d+) )?(\\d+):(\\d+):(\\d+)(?:\\.(\\d+))?$");
  private static final Pattern PATTERN_DTM = Pattern.compile("^([+-])?(?:(\\d+) )?(\\d+):(\\d+)$");
  private static final Pattern PATTERN_DTH = Pattern.compile("^([+-])?(?:(\\d+) )?(\\d+)$");
  private static final Pattern PATTERN_HTS =
      Pattern.compile("^([+-])?(\\d+):(\\d+):(\\d+)(?:\\.(\\d+))?$");
  private static final Pattern PATTERN_HTM = Pattern.compile("^([+-])?(\\d+):(\\d+)$");
  private static final Pattern PATTERN_MTS =
      Pattern.compile("^([+-])?(\\d+):(\\d+)(?:\\.(\\d+))?$");

  /** The signed number of months. */
  private final long months;

  /** The signed number of days. */
  private final long days;

  /** The signed number of seconds of the time part. */
  private final long seconds;

  /** The signed number of nanoseconds of the time part, with the same sign as the seconds. */
  private final int nanos;

  /** Create a new zero interval. */
  public Interval() {
    this(0, 0, 0, 0);
  }

  /** Create an interval from normalized parts, the nanoseconds have the sign of the seconds. */
  private Interval(long months, long days, long seconds, int nanos) {
    this.months = months;
    this.days = days;
    this.seconds = seconds;
    this.nanos = nanos;
  }

  /**
   * Create an interval from its fields, all with the same sign.
   *
   * @param negative {@code true} to create a negative interval
   */
  private static Interval of(
      long years,
      long months,
      long days,
      long hours,
      long minutes,
      long seconds,
      long nanos,
      boolean negative) {
    Interval interval = of(years, months, days, hours, minutes, seconds, nanos);
    return negative ? interval.negate() : interval;
  }

  /**
   * Create an interval from signed fields. The years and months give the month part, the hours,
   * minutes, seconds and nanoseconds give the time part.
   *
   * @throws ArithmeticException if a part overflows
   */
  private static Interval of(
      long years, long months, long days, long hours, long minutes, long seconds, long nanos) {
    long totalMonths = Math.addExact(Math.multiplyExact(years, MONTHS_PER_YEAR), months);
    long totalSeconds =
        Math.addExact(
            Math.addExact(
                Math.multiplyExact(hours, SECONDS_PER_HOUR),
                Math.multiplyExact(minutes, SECONDS_PER_MINUTE)),
            seconds);
    return ofSigned(totalMonths, days, totalSeconds, nanos);
  }

  /**
   * Creates an interval from signed months, days, seconds and nanoseconds. The parts can have
   * different signs, the nanoseconds are carried to the seconds and take the sign of the seconds.
   *
   * @param months the signed number of months
   * @param days the signed number of days
   * @param seconds the signed number of seconds
   * @param nanos the signed number of nanoseconds
   * @return the interval
   * @throws ArithmeticException if the seconds overflow
   */
  public static Interval ofSigned(long months, long days, long seconds, long nanos) {
    // Carry the nanoseconds to the seconds, then give them the sign of the seconds
    seconds = Math.addExact(seconds, Math.floorDiv(nanos, NANOS_PER_SECOND));
    nanos = Math.floorMod(nanos, NANOS_PER_SECOND);
    if (seconds < 0 && nanos > 0) {
      seconds++;
      nanos -= NANOS_PER_SECOND;
    }
    return new Interval(months, days, seconds, (int) nanos);
  }

  /**
   * Parse a standard SQL string representation of a <code>INTERVAL YEAR</code>.
   *
   * @param string A string representation of the form <code>[+|-][years]</code>
   * @return The parsed <code>YEAR</code> object, or <code>null</code> if the string could not be
   *     parsed.
   */
  public static @Nullable Interval year(@Nullable String string) {
    try {
      return string == null ? null : of(Integer.parseInt(string), 0, 0, 0, 0, 0, 0);
    } catch (NumberFormatException ignore) {
      return null;
    }
  }

  /**
   * Parse a standard SQL string representation of a <code>INTERVAL YEAR TO MONTH</code>.
   *
   * @param string A string representation of the form <code>[+|-][years]-[months]</code>
   * @return The parsed <code>YEAR TO MONTH</code> object, or <code>null</code> if the string could
   *     not be parsed.
   */
  public static @Nullable Interval yearToMonth(@Nullable String string) {
    if (string != null) {
      Matcher matcher = PATTERN_YTM.matcher(string);

      if (matcher.find()) {
        boolean negative = "-".equals(matcher.group(1));
        int years = parseField(matcher.group(2));
        int months = parseField(matcher.group(3));
        return of(years, months, 0, 0, 0, 0, 0, negative);
      }
    }

    return null;
  }

  /**
   * Parse a string representation of a <code>INTERVAL QUARTER</code>.
   *
   * @param string A string representation of the form <code>[+|-][quarters]</code>
   * @return The parsed <code>INTERVAL QUARTER</code> object, or <code>null</code> if the string
   *     could not be parsed.
   */
  public static @Nullable Interval quarter(@Nullable String string) {
    try {

      return string == null ? null : of(0, 3L * Integer.parseInt(string), 0, 0, 0, 0, 0);
    } catch (NumberFormatException ignore) {
      return null;
    }
  }

  /**
   * Parse a standard SQL string representation of a <code>INTERVAL MONTH</code>.
   *
   * @param string A string representation of the form <code>[+|-][months]</code>
   * @return The parsed <code>MONTH</code> object, or <code>null</code> if the string could not be
   *     parsed.
   */
  public static @Nullable Interval month(@Nullable String string) {
    try {
      return string == null ? null : of(0, Integer.parseInt(string), 0, 0, 0, 0, 0);
    } catch (NumberFormatException ignore) {
      return null;
    }
  }

  /**
   * Parse a string representation of a <code>INTERVAL WEEK</code>.
   *
   * @param string A string representation of the form <code>[+|-][weeks]</code>
   * @return The parsed <code>INTERVAL WEEK</code> object, or <code>null</code> if the string could
   *     not be parsed.
   */
  public static @Nullable Interval week(@Nullable String string) {
    try {

      return string == null ? null : of(0, 0, 7L * Integer.parseInt(string), 0, 0, 0, 0);
    } catch (NumberFormatException ignore) {
      return null;
    }
  }

  /**
   * Parse a string representation of a <code>INTERVAL DAY</code>.
   *
   * @param string A string representation of the form <code>[+|-][days]</code>
   * @return The parsed <code>INTERVAL DAY</code> object, or <code>null</code> if the string could
   *     not be parsed.
   */
  public static @Nullable Interval day(@Nullable String string) {
    try {
      return string == null ? null : of(0, 0, Integer.parseInt(string), 0, 0, 0, 0);
    } catch (NumberFormatException ignore) {
      return null;
    }
  }

  /**
   * Parse a string representation of a <code>INTERVAL DAY TO HOUR</code>.
   *
   * @param string A string representation of the form <code>[+|-][days] [hours]</code>
   * @return The parsed <code>INTERVAL DAY TO HOUR</code> object, or <code>null</code> if the string
   *     could not be parsed.
   */
  public static @Nullable Interval dayToHour(@Nullable String string) {
    if (string != null) {
      Matcher matcher = PATTERN_DTH.matcher(string);

      if (matcher.find()) {
        boolean negative = "-".equals(matcher.group(1));
        int days = parseField(matcher.group(2));
        int hours = parseField(matcher.group(3));

        return of(0, 0, days, hours, 0, 0, 0, negative);
      }
    }

    return null;
  }

  /**
   * Parse a string representation of a <code>INTERVAL DAY TO MINUTE</code>.
   *
   * @param string A string representation of the form <code>[+|-][days] [hours]:[minutes]</code>
   * @return The parsed <code>INTERVAL DAY TO MINUTE</code> object, or <code>null</code> if the
   *     string could not be parsed.
   */
  public static @Nullable Interval dayToMinute(@Nullable String string) {
    if (string != null) {
      Matcher matcher = PATTERN_DTM.matcher(string);

      if (matcher.find()) {
        boolean negative = "-".equals(matcher.group(1));
        int days = parseField(matcher.group(2));
        int hours = parseField(matcher.group(3));
        int minutes = parseField(matcher.group(4));

        return of(0, 0, days, hours, minutes, 0, 0, negative);
      }
    }

    return null;
  }

  /**
   * Parse a string representation of a <code>INTERVAL DAY TO SECOND</code>.
   *
   * @param string A string representation of the form <code>
   *               [+|-][days] [hours]:[minutes]:[seconds].[fractional seconds]</code>
   * @return The parsed <code>INTERVAL DAY TO MINUTE</code> object, or <code>null</code> if the
   *     string could not be parsed.
   */
  public static @Nullable Interval dayToSecond(@Nullable String string) {
    if (string != null) {
      Matcher matcher = PATTERN_DTS.matcher(string);

      if (matcher.find()) {
        boolean negative = "-".equals(matcher.group(1));
        int days = parseField(matcher.group(2));
        int hours = parseField(matcher.group(3));
        int minutes = parseField(matcher.group(4));
        int seconds = parseField(matcher.group(5));
        int nanos = parseField(StringUtils.rightPad(matcher.group(6), 9, "0"));

        return of(0, 0, days, hours, minutes, seconds, nanos, negative);
      }
    }

    return null;
  }

  /**
   * Parse a string representation of a <code>INTERVAL HOUR</code>.
   *
   * @param string A string representation of the form <code>[+|-][hours]</code>
   * @return The parsed <code>INTERVAL HOUR</code> object, or <code>null</code> if the string could
   *     not be parsed.
   */
  public static @Nullable Interval hour(@Nullable String string) {
    try {
      return string == null ? null : of(0, 0, 0, Integer.parseInt(string), 0, 0, 0);
    } catch (NumberFormatException ignore) {
      return null;
    }
  }

  /**
   * Parse a string representation of a <code>INTERVAL HOUR TO MINUTE</code>.
   *
   * @param string A string representation of the form <code>[+|-][hours]:[minutes]</code>
   * @return The parsed <code>INTERVAL HOUR TO MINUTE</code> object, or <code>null</code> if the
   *     string could not be parsed.
   */
  public static @Nullable Interval hourToMinute(@Nullable String string) {
    if (string != null) {
      Matcher matcher = PATTERN_HTM.matcher(string);

      if (matcher.find()) {

        boolean negative = "-".equals(matcher.group(1));
        int hours = parseField(matcher.group(2));
        int minutes = parseField(matcher.group(3));

        return of(0, 0, 0, hours, minutes, 0, 0, negative);
      }
    }

    return null;
  }

  /**
   * Parse a string representation of a <code>INTERVAL HOUR TO SECOND</code>.
   *
   * @param string A string representation of the form <code>
   *               [+|-][hours]:[minutes]:[seconds].[fractional seconds]</code>
   * @return The parsed <code>INTERVAL HOUR TO SECOND</code> object, or <code>null</code> if the
   *     string could not be parsed.
   */
  public static @Nullable Interval hourToSecond(@Nullable String string) {
    if (string != null) {
      Matcher matcher = PATTERN_HTS.matcher(string);

      if (matcher.find()) {
        boolean negative = "-".equals(matcher.group(1));
        int hours = parseField(matcher.group(2));
        int minutes = parseField(matcher.group(3));
        int seconds = parseField(matcher.group(4));
        int nanos = parseField(StringUtils.rightPad(matcher.group(5), 9, "0"));

        return of(0, 0, 0, hours, minutes, seconds, nanos, negative);
      }
    }

    return null;
  }

  /**
   * Parse a string representation of a <code>INTERVAL MINUTE</code>.
   *
   * @param string A string representation of the form <code>[+|-][minutes]</code>
   * @return The parsed <code>INTERVAL MINUTE</code> object, or <code>null</code> if the string
   *     could not be parsed.
   */
  public static @Nullable Interval minute(@Nullable String string) {
    try {
      return string == null ? null : of(0, 0, 0, 0, Integer.parseInt(string), 0, 0);
    } catch (NumberFormatException ignore) {
      return null;
    }
  }

  /**
   * Parse a string representation of a <code>INTERVAL MINUTE TO SECOND</code>.
   *
   * @param string A string representation of the form <code>
   *               [+|-][[minutes]:[seconds].[fractional seconds]</code>
   * @return The parsed <code>INTERVAL MINUTE TO SECOND</code> object, or <code>null</code> if the
   *     string could not be parsed.
   */
  public static @Nullable Interval minuteToSecond(@Nullable String string) {
    if (string != null) {
      Matcher matcher = PATTERN_MTS.matcher(string);

      if (matcher.find()) {
        boolean negative = "-".equals(matcher.group(1));
        int minutes = parseField(matcher.group(2));
        int seconds = parseField(matcher.group(3));
        int nanos = parseField(StringUtils.rightPad(matcher.group(4), 9, "0"));

        return of(0, 0, 0, 0, minutes, seconds, nanos, negative);
      }
    }

    return null;
  }

  /**
   * Parse a string representation of a <code>INTERVAL SECOND</code>.
   *
   * @param string A string representation of the form <code>[+|-][seconds].[fractional seconds]
   *               </code>
   * @return The parsed <code>INTERVAL SECOND</code> object, or <code>null</code> if the string
   *     could not be parsed.
   */
  public static @Nullable Interval second(@Nullable String string) {
    if (string == null) return null;

    try {
      double value = Double.parseDouble(string);
      double abs = Math.abs(value);
      long seconds = (long) abs;
      int nanos = (int) ((abs - seconds) * NANOS_PER_SECOND);

      return of(0, 0, 0, 0, 0, seconds, nanos, value < 0);
    } catch (NumberFormatException ignore) {
      return null;
    }
  }

  /** Create a new year interval. */
  public static Interval of(int years) {
    return of(years, 0, 0, 0, 0, 0, 0);
  }

  /** Create a new year-month interval. */
  public static Interval of(int years, int months) {
    return of(years, months, 0, 0, 0, 0, 0);
  }

  /** Create a new year-day interval. */
  public static Interval of(int years, int months, int days) {
    return of(years, months, days, 0, 0, 0, 0);
  }

  /** Create a new year-hour interval. */
  public static Interval of(int years, int months, int days, int hours) {
    return of(years, months, days, hours, 0, 0, 0);
  }

  /** Create a new year-minute interval. */
  public static Interval of(int years, int months, int days, int hours, int minutes) {
    return of(years, months, days, hours, minutes, 0, 0);
  }

  /** Create a new year-second interval. */
  public static Interval of(int years, int months, int days, int hours, int minutes, int seconds) {
    return of(years, months, days, hours, minutes, seconds, 0);
  }

  /** Create a new year-second with nanoseconds interval. */
  public static Interval of(
      int years, int months, int days, int hours, int minutes, int seconds, int nanos) {
    return of((long) years, months, days, hours, minutes, seconds, nanos);
  }

  /**
   * Parse a string representation of a <code>INTERVAL YEAR TO SECOND</code>
   *
   * @param string A string representation of the form <code>
   *               [+|-][years]-[months] [+|-][days] [+|-][hours]:[minutes]:[seconds].[fractional seconds]
   *     </code>, the days and the time take the leading sign if they have no sign, or a verbose
   *     form like <code>1 year -2 days</code>
   * @return The parsed <code>YEAR TO SECOND</code> object
   * @throws ExpressionException if the string could not be parsed.
   */
  public static @Nullable Interval of(@Nullable String string) {
    if (string == null) {
      return null;
    }

    try {
      return parseString(string);
    } catch (NumberFormatException | ArithmeticException e) {
      // A field or a quantity out of range
      throw new ExpressionException(ErrorCode.INVALID_INTERVAL, string);
    }
  }

  /** Parse the short syntax, or the verbose syntax if the string does not match it. */
  private static Interval parseString(final String string) {
    Matcher matcher = PATTERN_YTS.matcher(string);
    if (!matcher.find()) {
      return parse(string);
    }

    // More than 9 digits of fractional seconds are not supported
    if (matcher.group(10) != null && matcher.group(10).length() > 9) {
      throw new ExpressionException(ErrorCode.INVALID_INTERVAL, string);
    }

    String sign = matcher.group(1);
    long signMonths = "-".equals(sign) ? -1 : 1;
    long signDays = "-".equals(matcher.group(4) == null ? sign : matcher.group(4)) ? -1 : 1;
    long signTime = "-".equals(matcher.group(6) == null ? sign : matcher.group(6)) ? -1 : 1;
    int years = parseField(matcher.group(2));
    int months = parseField(matcher.group(3));
    int days = parseField(matcher.group(5));
    int hours = parseField(matcher.group(7));
    int minutes = parseField(matcher.group(8));
    int seconds = parseField(matcher.group(9));
    int nanos = parseField(StringUtils.rightPad(matcher.group(10), 9, "0"));

    return of(
        signMonths * years,
        signMonths * months,
        signDays * days,
        signTime * hours,
        signTime * minutes,
        signTime * seconds,
        signTime * nanos);
  }

  // -------------------------------------------------------------------------
  // Interval API
  // -------------------------------------------------------------------------

  protected static int parseField(@Nullable String str) {
    if (str == null || str.isEmpty()) return 0;
    return Integer.parseInt(str);
  }

  /** Parse a verbose format like {@code 1 year 2 months -3 days}, each quantity can be signed. */
  protected static Interval parse(final String text) {
    long years = 0;
    long months = 0;
    long days = 0;
    long hours = 0;
    long minutes = 0;
    long seconds = 0;
    long nanos = 0;

    // An empty string is not an interval
    if (text.isBlank()) {
      throw new ExpressionException(ErrorCode.INVALID_INTERVAL, text);
    }

    int length = text.length();
    int index = 0;
    while (index < length) {

      // Skip space
      if (Characters.isSpace(text.charAt(index))) {
        index++;
        continue;
      }

      // Optional sign of the quantity
      int sign = 1;
      char first = text.charAt(index);
      if (first == '-' || first == '+') {
        if (first == '-') sign = -1;
        index++;
      }

      long quantity = 0;
      int start = index;
      while (index < length) {
        char ch = text.charAt(index);
        int digit = Character.digit(ch, 10);
        if (digit < 0) {
          break;
        }
        index++;
        quantity = Math.addExact(Math.multiplyExact(quantity, 10), digit);
      }

      if (index == start) {
        throw new ExpressionException(ErrorCode.INVALID_INTERVAL, text);
      }
      quantity *= sign;

      // Skip space
      while (index < length && Characters.isSpace(text.charAt(index))) {
        index++;
      }

      boolean noMatch = true;
      for (TimeUnit unit : UNITS) {
        if (text.regionMatches(true, index, unit.name(), 0, unit.name().length())) {
          switch (unit) {
            case YEAR:
              years = Math.addExact(years, quantity);
              break;
            case QUARTER:
              months = Math.addExact(months, Math.multiplyExact(3, quantity));
              break;
            case MONTH:
              months = Math.addExact(months, quantity);
              break;
            case WEEK:
              days = Math.addExact(days, Math.multiplyExact(7, quantity));
              break;
            case DAY:
              days = Math.addExact(days, quantity);
              break;
            case HOUR:
              hours = Math.addExact(hours, quantity);
              break;
            case MINUTE:
              minutes = Math.addExact(minutes, quantity);
              break;
            case SECOND:
              seconds = Math.addExact(seconds, quantity);
              break;
            default:
              break;
          }

          index += unit.name().length();
          if (index < length) {
            char ch = text.charAt(index);
            if (ch == 's' || ch == 'S') index++;
          }

          noMatch = false;
          break;
        }
      }

      if (noMatch) throw new ExpressionException(ErrorCode.INVALID_INTERVAL, text);

      if (index < length && text.charAt(index) == ',') {
        index++;
      }
    }

    return of(years, months, days, hours, minutes, seconds, nanos);
  }

  /**
   * Negate the interval (change the sign of each part)
   *
   * @throws ArithmeticException if a part is the smallest long value, its negation overflows
   */
  public final Interval negate() {
    return new Interval(
        Math.negateExact(months), Math.negateExact(days), Math.negateExact(seconds), -nanos);
  }

  /** Abs the interval, negate it if it is less than zero */
  public final Interval abs() {
    return (signum() < 0) ? negate() : this;
  }

  /** Get the signed years part of the interval. */
  public final long getYears() {
    return months / MONTHS_PER_YEAR;
  }

  /** Get the signed months part of the interval, without the years. */
  public final long getMonths() {
    return months % MONTHS_PER_YEAR;
  }

  /** Get the signed days part of the interval. */
  public final long getDays() {
    return days;
  }

  /** Get the signed hours of the time part of the interval, it can exceed 24 hours. */
  public final long getHours() {
    return seconds / SECONDS_PER_HOUR;
  }

  /** Get the signed minutes of the time part of the interval, without the hours. */
  public final long getMinutes() {
    return (seconds % SECONDS_PER_HOUR) / SECONDS_PER_MINUTE;
  }

  /** Get the signed seconds of the time part of the interval, without the minutes. */
  public final long getSeconds() {
    return seconds % SECONDS_PER_MINUTE;
  }

  /** Get the signed milliseconds part within seconds of the interval. */
  public final long getMilliseconds() {
    return nanos / 1000000;
  }

  /** Get the signed microseconds part within seconds of the interval. */
  public final long getMicroseconds() {
    return nanos / 1000;
  }

  // -------------------------------------------------------------------------
  // Number API
  // -------------------------------------------------------------------------

  /** Get the signed nanoseconds part within seconds of the interval. */
  public final long getNanoseconds() {
    return nanos;
  }

  // -------------------------------------------------------------------------
  // Comparable and Object API
  // -------------------------------------------------------------------------

  /**
   * The sign of the interval, a month is 30 days and a day is 24 hours.
   *
   * @return <code>1</code> for positive or zero, <code>-1</code> for negative
   */
  public final int getSign() {
    return signum() < 0 ? -1 : 1;
  }

  /** Returns the signum of the total length of the interval. */
  private int signum() {
    return toTotalNanos().signum();
  }

  /**
   * Returns the total length of the interval in nanoseconds, a month is 30 days and a day is 24
   * hours, like PostgreSQL.
   */
  private BigInteger toTotalNanos() {
    BigInteger totalDays =
        BigInteger.valueOf(months)
            .multiply(BigInteger.valueOf(DAYS_PER_MONTH))
            .add(BigInteger.valueOf(days));
    BigInteger totalSeconds =
        totalDays.multiply(BigInteger.valueOf(SECONDS_PER_DAY)).add(BigInteger.valueOf(seconds));
    return totalSeconds
        .multiply(BigInteger.valueOf(NANOS_PER_SECOND))
        .add(BigInteger.valueOf(nanos));
  }

  /** Returns the total length of the interval in seconds, a month is 30 days. */
  public final double doubleValue() {
    return toTotalNanos().doubleValue() / NANOS_PER_SECOND;
  }

  @Override
  public final int compareTo(Interval that) {
    return toTotalNanos().compareTo(that.toTotalNanos());
  }

  @Override
  public final int hashCode() {
    return toTotalNanos().hashCode();
  }

  /**
   * Two intervals are equal if they have the same total length, a month is 30 days and a day is 24
   * hours, like PostgreSQL.
   */
  @Override
  public final boolean equals(@Nullable Object obj) {
    if (this == obj) return true;
    if (obj instanceof Interval other) {
      return compareTo(other) == 0;
    }
    return false;
  }

  /**
   * Checks if this interval is zero length.
   *
   * <p>A {@code Interval} represents a directed distance between two points on the time-line and
   * can therefore be positive, zero or negative. This method checks whether the length is zero.
   *
   * @return true if this interval has all its parts equal to zero
   */
  public boolean isZero() {
    return months == 0L && days == 0L && seconds == 0L && nanos == 0L;
  }

  /**
   * Checks if the parts of this interval have different signs, like {@code 1 month -1 day}.
   *
   * @return true if two non zero parts have opposite signs
   */
  public boolean isMixedSign() {
    int sign = 0;
    for (long part : new long[] {months, days, seconds != 0 ? seconds : nanos}) {
      int partSign = Long.signum(part);
      if (partSign != 0) {
        if (sign != 0 && partSign != sign) return true;
        sign = partSign;
      }
    }
    return false;
  }

  /**
   * Returns the format <code>[+|-]Y-M [+|-]D [+|-]HH:MM:SS.FFFFFFFFF</code>. The leading sign is
   * the sign of the first non zero part, the days and the time have their own sign only if it is
   * different.
   */
  @Override
  public final String toString() {
    long timeSign = Long.signum(seconds != 0 ? seconds : nanos);
    long leadSign = Long.signum(months);
    if (leadSign == 0) leadSign = Long.signum(days);
    if (leadSign == 0) leadSign = timeSign;
    boolean negative = leadSign < 0;

    StringBuilder sb = new StringBuilder();
    sb.append(negative ? '-' : '+');
    sb.append(Math.abs(getYears()));
    sb.append('-');
    sb.append(Math.abs(getMonths()));
    sb.append(' ');
    if (days != 0 && (days < 0) != negative) {
      sb.append(days < 0 ? '-' : '+');
    }
    sb.append(absToString(days));
    sb.append(' ');
    if (timeSign != 0 && (timeSign < 0) != negative) {
      sb.append(timeSign < 0 ? '-' : '+');
    }
    toStringHour(sb);
    sb.append(':');
    toStringMinute(sb);
    sb.append(':');
    long s = Math.abs(getSeconds());
    if (s < 10) {
      sb.append('0');
    }
    sb.append(s);
    sb.append('.');
    sb.append(StringUtils.leftPad("" + Math.abs(nanos), 9, "0"));

    return sb.toString();
  }

  /**
   * Returns the format of a qualifier, with a single sign. The interval must not have parts with
   * different signs.
   */
  public final String toString(final IntervalQualifier qualifier) {
    StringBuilder sb = new StringBuilder();
    boolean negative = getSign() < 0;

    switch (qualifier) {
      case YEAR:
        if (negative) sb.append('-');
        sb.append(Math.abs(getYears()));
        break;

      case YEAR_TO_MONTH:
        sb.append(negative ? '-' : '+');
        sb.append(Math.abs(getYears()));
        sb.append('-');
        sb.append(Math.abs(getMonths()));
        break;

      case MONTH:
        if (negative) sb.append('-');
        sb.append(Math.abs(getMonths()));
        break;

      case DAY:
        if (negative) sb.append('-');
        sb.append(absToString(getDays()));
        break;

      case HOUR:
        if (negative) sb.append('-');
        sb.append(Math.abs(getHours()));
        break;

      case MINUTE:
        if (negative) sb.append('-');
        sb.append(Math.abs(getMinutes()));
        break;

      case HOUR_TO_MINUTE:
        sb.append(negative ? '-' : '+');
        toStringHour(sb);
        sb.append(':');
        toStringMinute(sb);
        break;

      case DAY_TO_HOUR:
        sb.append(negative ? '-' : '+');
        sb.append(absToString(getDays()));
        sb.append(' ');
        toStringHour(sb);
        break;

      case DAY_TO_SECOND:
        sb.append(negative ? '-' : '+');
        sb.append(absToString(getDays()));
        sb.append(' ');
        toStringHour(sb);
        sb.append(':');
        toStringMinute(sb);
        sb.append(':');
        toStringSecond(sb);
        break;

      case HOUR_TO_SECOND:
        sb.append(negative ? '-' : '+');
        toStringHour(sb);
        sb.append(':');
        toStringMinute(sb);
        sb.append(':');
        toStringSecond(sb);
        break;

      case MINUTE_TO_SECOND:
        sb.append(negative ? '-' : '+');
        toStringMinute(sb);
        sb.append(':');
        toStringSecond(sb);
        break;

      case SECOND:
        if (negative) sb.append('-');
        toStringSecond(sb);
        break;

      default:
        break;
    }

    return sb.toString();
  }

  /**
   * Returns the digits of the absolute value, the sign is written apart. Math.abs of the smallest
   * long value is still negative, its digits are written as an unsigned value to not write two
   * minus signs.
   */
  private static String absToString(final long value) {
    return Long.toUnsignedString(Math.abs(value));
  }

  private void toStringHour(final StringBuilder sb) {
    long h = Math.abs(getHours());
    if (h < 10) {
      sb.append('0');
    }
    sb.append(h);
  }

  private void toStringMinute(final StringBuilder sb) {
    long m = Math.abs(getMinutes());
    if (m < 10) {
      sb.append('0');
    }
    sb.append(m);
  }

  private void toStringSecond(final StringBuilder sb) {
    long s = Math.abs(getSeconds());
    if (s < 10) {
      sb.append('0');
    }
    sb.append(s);

    // TODO: Use precision for nanoseconds
    if (nanos != 0) {
      sb.append('.');
      sb.append(StringUtils.leftPad("" + Math.abs(nanos), 9, "0"));
    }
  }

  /**
   * Subtracts the specified interval from this interval, part by part like PostgreSQL.
   *
   * @param interval the interval to subtract, not null
   * @return the difference of the intervals
   * @throws ArithmeticException if a part overflows
   */
  public Interval subtract(final Interval interval) {
    return add(interval.negate());
  }

  /**
   * Adds the specified interval to this interval, part by part like PostgreSQL.
   *
   * @param interval the interval to add, not null
   * @return the sum of the intervals
   * @throws ArithmeticException if a part overflows
   */
  public Interval add(final Interval interval) {
    return ofSigned(
        Math.addExact(months, interval.months),
        Math.addExact(days, interval.days),
        Math.addExact(seconds, interval.seconds),
        (long) nanos + interval.nanos);
  }

  /**
   * Adds this interval to the specified temporal object. The months are added first, then the
   * calendar days, which keep the local time across a daylight saving time change, then the time.
   *
   * @param temporal the temporal object to adjust, not null
   */
  public ZonedDateTime addTo(ZonedDateTime temporal) {
    if (months != 0) {
      temporal = temporal.plusMonths(months);
    }
    if (days != 0) {
      temporal = temporal.plusDays(days);
    }
    if (seconds != 0) {
      temporal = temporal.plusSeconds(seconds);
    }
    if (nanos != 0) {
      temporal = temporal.plusNanos(nanos);
    }
    return temporal;
  }

  /**
   * Subtracts this interval from the specified temporal object, like adding the negated interval.
   *
   * @param temporal the temporal object to adjust, not null
   */
  public ZonedDateTime subtractFrom(ZonedDateTime temporal) {
    return negate().addTo(temporal);
  }
}
