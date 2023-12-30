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
package org.apache.hop.expression.type;

import org.apache.commons.lang3.StringUtils;
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.util.Characters;
import java.io.Serializable;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.EnumSet;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A interval represents a duration of time which can be used in date/time arithmetic.
 */
@SuppressWarnings("serial")
public class Interval implements Serializable, Comparable<Interval> {

  private static final EnumSet<TimeUnit> UNITS = EnumSet.of(TimeUnit.YEAR, TimeUnit.QUARTER, TimeUnit.MONTH, TimeUnit.WEEK, TimeUnit.DAY,
      TimeUnit.HOUR, TimeUnit.MINUTE, TimeUnit.SECOND);

  private static final Pattern PATTERN_YTS = Pattern.compile("^([+-])?(\\d+)-(\\d+) (?:(\\d+) )?(\\d+):(\\d+):(\\d+)(?:\\.(\\d+))?$");
  private static final Pattern PATTERN_YTM = Pattern.compile("^([+-])?(\\d+)-(\\d+)$");
  private static final Pattern PATTERN_DTS = Pattern.compile("^([+-])?(?:(\\d+) )?(\\d+):(\\d+):(\\d+)(?:\\.(\\d+))?$");
  private static final Pattern PATTERN_DTM = Pattern.compile("^([+-])?(?:(\\d+) )?(\\d+):(\\d+)$");
  private static final Pattern PATTERN_DTH = Pattern.compile("^([+-])?(?:(\\d+) )?(\\d+)$");
  private static final Pattern PATTERN_HTS = Pattern.compile("^([+-])?(\\d+):(\\d+):(\\d+)(?:\\.(\\d+))?$");
  private static final Pattern PATTERN_HTM = Pattern.compile("^([+-])?(\\d+):(\\d+)$");
  private static final Pattern PATTERN_MTS = Pattern.compile("^([+-])?(\\d+):(\\d+)(?:\\.(\\d+))?$");
  /**
   * The number of months per year.
   */
  static final long MONTHS_PER_YEAR = 12;
  /**
   * The number of seconds per minute.
   */
  static final long SECONDS_PER_MINUTE = 60;

  /**
   * The number of seconds per hour.
   */
  static final long SECONDS_PER_HOUR = 60 * SECONDS_PER_MINUTE;

  /**
   * The number of seconds per day.
   */
  static final long SECONDS_PER_DAY = 24 * SECONDS_PER_HOUR;

  /**
   * The number of milliseconds per day.
   */
  static final long MILLIS_PER_DAY = 1000 * SECONDS_PER_DAY;

  /**
   * The number of nanoseconds per second.
   */
  static final long NANOS_PER_SECOND = 1_000_000_000L;

  /**
   * The number of nanoseconds per minute.
   */
  static final long NANOS_PER_MINUTE = 60 * NANOS_PER_SECOND;

  /**
   * The number of nanoseconds per hour.
   */
  static final long NANOS_PER_HOUR = 60 * NANOS_PER_MINUTE;

  /**
   * The number of nanoseconds per day.
   */
  static final long NANOS_PER_DAY = MILLIS_PER_DAY * 1_000_000;

  /**
   * Parse a standard SQL string representation of a
   * <code>INTERVAL YEAR</code>.
   *
   * @param string A string representation of the form
   *        <code>[+|-][years]</code>
   * @return The parsed <code>YEAR</code> object, or <code>null</code> if the
   *         string could not be parsed.
   */
  public static Interval year(final String string) {
    try {
      return string == null ? null : new Interval(Integer.parseInt(string), 0, 0, 0, 0, 0, 0);
    } catch (NumberFormatException ignore) {
      return null;
    }
  }

  /**
   * Parse a standard SQL string representation of a
   * <code>INTERVAL YEAR TO MONTH</code>.
   *
   * @param string A string representation of the form
   *        <code>[+|-][years]-[months]</code>
   * @return The parsed <code>YEAR TO MONTH</code> object, or
   *         <code>null</code> if the string could not be parsed.
   */
  public static Interval yearToMonth(final String string) {
    if (string != null) {
      Matcher matcher = PATTERN_YTM.matcher(string);

      if (matcher.find()) {
        boolean negative = "-".equals(matcher.group(1));
        int years = parseField(matcher.group(2));
        int months = parseField(matcher.group(3));
        return new Interval(years, months, 0, 0, 0, 0, 0, negative);
      }
    }

    return null;
  }

  /**
   * Parse a string representation of a <code>INTERVAL QUARTER</code>.
   *
   * @param string A string representation of the form
   *        <code>[+|-][quarters]</code>
   * @return The parsed <code>INTERVAL QUARTER</code> object, or <code>null</code>
   *         if the string could not be parsed.
   */
  public static Interval quarter(final String string) {
    try {

      return string == null ? null : new Interval(0, 3 * Integer.parseInt(string), 0, 0, 0, 0, 0);
    } catch (NumberFormatException ignore) {
      return null;
    }
  }

  /**
   * Parse a standard SQL string representation of a
   * <code>INTERVAL MONTH</code>.
   *
   * @param string A string representation of the form
   *        <code>[+|-][months]</code>
   * @return The parsed <code>MONTH</code> object, or <code>null</code> if the
   *         string could not be parsed.
   */
  public static Interval month(final String string) {
    try {
      return string == null ? null : new Interval(0, Integer.parseInt(string), 0, 0, 0, 0, 0);
    } catch (NumberFormatException ignore) {
      return null;
    }
  }

  /**
   * Parse a string representation of a <code>INTERVAL WEEK</code>.
   *
   * @param string A string representation of the form
   *        <code>[+|-][weeks]</code>
   * @return The parsed <code>INTERVAL WEEK</code> object, or <code>null</code>
   *         if the string could not be parsed.
   */
  public static Interval week(final String string) {
    try {

      return string == null ? null : new Interval(0, 0, 7 * Integer.parseInt(string), 0, 0, 0, 0);
    } catch (NumberFormatException ignore) {
      return null;
    }
  }

  /**
   * Parse a string representation of a <code>INTERVAL DAY</code>.
   *
   * @param string A string representation of the form
   *        <code>[+|-][days]</code>
   * @return The parsed <code>INTERVAL DAY</code> object, or <code>null</code>
   *         if the string could not be parsed.
   */
  public static Interval day(final String string) {
    try {
      return string == null ? null : new Interval(0, 0, Integer.parseInt(string), 0, 0, 0, 0);
    } catch (NumberFormatException ignore) {
      return null;
    }
  }

  /**
   * Parse a string representation of a <code>INTERVAL DAY TO HOUR</code>.
   *
   * @param string A string representation of the form
   *        <code>[+|-][days] [hours]</code>
   * @return The parsed <code>INTERVAL DAY TO HOUR</code> object, or
   *         <code>null</code> if the string could not be parsed.
   */
  public static Interval dayToHour(final String string) {
    if (string != null) {
      Matcher matcher = PATTERN_DTH.matcher(string);

      if (matcher.find()) {        
        boolean negative = "-".equals(matcher.group(1));
        int days = parseField(matcher.group(2));
        int hours = parseField(matcher.group(3));

        return new Interval(0, 0, days, hours, 0, 0, 0, negative);
      }
    }

    return null;
  }

  /**
   * Parse a string representation of a <code>INTERVAL DAY TO MINUTE</code>.
   *
   * @param string A string representation of the form
   *        <code>[+|-][days] [hours]:[minutes]</code>
   * @return The parsed <code>INTERVAL DAY TO MINUTE</code> object, or
   *         <code>null</code> if the string could not be parsed.
   */
  public static Interval dayToMinute(final String string) {
    if (string != null) {
      Matcher matcher = PATTERN_DTM.matcher(string);

      if (matcher.find()) {
        boolean negative = "-".equals(matcher.group(1));
        int days = parseField(matcher.group(2));
        int hours = parseField(matcher.group(3));
        int minutes = parseField(matcher.group(4));

        return new Interval(0, 0, days, hours, minutes, 0, 0, negative);
      }
    }

    return null;
  }

  /**
   * Parse a string representation of a <code>INTERVAL DAY TO SECOND</code>.
   *
   * @param string A string representation of the form
   *        <code>[+|-][days] [hours]:[minutes]:[seconds].[fractional seconds]</code>
   * @return The parsed <code>INTERVAL DAY TO MINUTE</code> object, or
   *         <code>null</code> if the string could not be parsed.
   */
  public static Interval dayToSecond(final String string) {
    if (string != null) {
      Matcher matcher = PATTERN_DTS.matcher(string);

      if (matcher.find()) {
        boolean negative = "-".equals(matcher.group(1));
        int days = parseField(matcher.group(2));
        int hours = parseField(matcher.group(3));
        int minutes = parseField(matcher.group(4));
        int seconds = parseField(matcher.group(5));
        int nanos = parseField(StringUtils.rightPad(matcher.group(6), 9, "0"));

        return new Interval(0, 0, days, hours, minutes, seconds, nanos, negative);
      }
    }

    return null;
  }

  /**
   * Parse a string representation of a <code>INTERVAL HOUR</code>.
   *
   * @param string A string representation of the form
   *        <code>[+|-][hours]</code>
   * @return The parsed <code>INTERVAL HOUR</code> object, or
   *         <code>null</code> if the string could not be parsed.
   */
  public static Interval hour(final String string) {
    try {
      return string == null ? null : new Interval(0, 0, 0, Integer.parseInt(string), 0, 0, 0);
    } catch (NumberFormatException ignore) {
      return null;
    }
  }

  /**
   * Parse a string representation of a <code>INTERVAL HOUR TO MINUTE</code>.
   *
   * @param string A string representation of the form
   *        <code>[+|-][hours]:[minutes]</code>
   * @return The parsed <code>INTERVAL HOUR TO MINUTE</code> object, or
   *         <code>null</code> if the string could not be parsed.
   */
  public static Interval hourToMinute(final String string) {
    if (string != null) {
      Matcher matcher = PATTERN_HTM.matcher(string);

      if (matcher.find()) {

        boolean negative = "-".equals(matcher.group(1));
        int hours = parseField(matcher.group(2));
        int minutes = parseField(matcher.group(3));

        return new Interval(0, 0, 0, hours, minutes, 0, 0, negative);        
      }
    }

    return null;
  }

  /**
   * Parse a string representation of a <code>INTERVAL HOUR TO SECOND</code>.
   *
   * @param string A string representation of the form
   *        <code>[+|-][hours]:[minutes]:[seconds].[fractional seconds]</code>
   * @return The parsed <code>INTERVAL HOUR TO SECOND</code> object, or
   *         <code>null</code> if the string could not be parsed.
   */
  public static Interval hourToSecond(final String string) {
    if (string != null) {
      Matcher matcher = PATTERN_HTS.matcher(string);

      if (matcher.find()) {
        boolean negative = "-".equals(matcher.group(1));
        int hours = parseField(matcher.group(2));
        int minutes = parseField(matcher.group(3));
        int seconds = parseField(matcher.group(4));
        int nanos = parseField(StringUtils.rightPad(matcher.group(5), 9, "0"));

        return new Interval(0, 0, 0, hours, minutes, seconds, nanos, negative);
      }
    }

    return null;
  }

  /**
   * Parse a string representation of a <code>INTERVAL MINUTE</code>.
   *
   * @param string A string representation of the form
   *        <code>[+|-][minutes]</code>
   * @return The parsed <code>INTERVAL MINUTE</code> object, or
   *         <code>null</code> if the string could not be parsed.
   */
  public static Interval minute(final String string) {
    try {
      return string == null ? null : new Interval(0, 0, 0, 0, Integer.parseInt(string), 0, 0);
    } catch (NumberFormatException ignore) {
      return null;
    }
  }

  /**
   * Parse a string representation of a <code>INTERVAL MINUTE TO SECOND</code>.
   *
   * @param string A string representation of the form
   *        <code>[+|-][[minutes]:[seconds].[fractional seconds]</code>
   * @return The parsed <code>INTERVAL MINUTE TO SECOND</code> object, or
   *         <code>null</code> if the string could not be parsed.
   */
  public static Interval minuteToSecond(final String string) {
    if (string != null) {
      Matcher matcher = PATTERN_MTS.matcher(string);

      if (matcher.find()) {
        boolean negative = "-".equals(matcher.group(1));
        int minutes = parseField(matcher.group(2));
        int seconds = parseField(matcher.group(3));
        int nanos = parseField(StringUtils.rightPad(matcher.group(4), 9, "0"));

        return new Interval(0, 0, 0, 0, minutes, seconds, nanos, negative);
      }
    }

    return null;
  }

  /**
   * Parse a string representation of a <code>INTERVAL SECOND</code>.
   *
   * @param string A string representation of the form
   *        <code>[+|-][seconds].[fractional seconds]</code>
   * @return The parsed <code>INTERVAL SECOND</code> object, or
   *         <code>null</code> if the string could not be parsed.
   */
  public static Interval second(final String string) {
    if (string == null)
      return null;

    try {
      double value = Double.parseDouble(string);
      double abs = Math.abs(value);
      long seconds = (long) abs;
      int nanos = (int) ((abs - seconds) * NANOS_PER_SECOND);

      return new Interval(0, seconds, nanos, value < 0);
    } catch (NumberFormatException ignore) {
      return null;
    }
  }

  /**
   * Create a new year interval.
   */
  public static Interval of(int years) {
    return new Interval(years, 0, 0, 0, 0, 0, 0, false);
  }

  /**
   * Create a new year-month interval.
   */
  public static Interval of(int years, int months) {
    return new Interval(years, months, 0, 0, 0, 0, 0, false);
  }


  /**
   * Create a new year-day interval.
   */
  public static Interval of(int years, int months, int days) {
    return new Interval(years, months, days, 0, 0, 0, 0, false);
  }

  /**
   * Create a new year-hour interval.
   */
  public static Interval of(int years, int months, int days, int hours) {
    return new Interval(years, months, days, hours, 0, 0, 0, false);
  }

  /**
   * Create a new year-minute interval.
   */
  public static Interval of(int years, int months, int days, int hours, int minutes) {
    return new Interval(years, months, days, hours, minutes, 0, 0, false);
  }


  /**
   * Create a new year-second interval.
   */
  public static Interval of(int years, int months, int days, int hours, int minutes, int seconds) {
    return new Interval(years, months, days, hours, minutes, seconds, 0, false);
  }


  /**
   * Create a new year-second with nanoseconds interval.
   */
  public static Interval of(int years, int months, int days, int hours, int minutes, int seconds,
      int nanos) {
    return new Interval(years, months, days, hours, minutes, seconds, nanos, false);
  }

  /**
   * {@code false} for zero or positive intervals, {@code true} for negative
   * intervals.
   */
  private final boolean negative;
  private final long months;
  private final long seconds;
  private final int nanos;

  /**
   * Create a new zero interval.
   */
  public Interval() {
    this(0, 0, 0, 0, 0, 0, 0, false);
  }


  /**
   * Create a new year-second interval.
   */
  Interval(int years, int months, int days, int hours, int minutes, int seconds, int nanos) {
    this(years, months, days, hours, minutes, seconds, 0, false);
  }

  Interval(int years, int months, int days, int hours, int minutes, int seconds, int nanos,
      boolean negative) {


    // All part must be positive


    this.months = years * MONTHS_PER_YEAR + months;
    this.seconds =
        days * SECONDS_PER_DAY + hours * SECONDS_PER_HOUR + minutes * SECONDS_PER_MINUTE + seconds;
    this.nanos = nanos;
    this.negative = negative;
  }

  Interval(long months, long seconds, int nanos, boolean negative) {
    this.months = months;
    this.seconds = seconds;
    this.negative = negative;
    this.nanos = nanos;
  }

  /**
   * Load a {@link Double} representation of a
   * <code>INTERVAL YEAR TO SECOND</code> by assuming standard 24 hour days and
   * 60 second minutes.
   *
   * @param milli The number of milliseconds as a fractional number
   * @return The loaded <code>INTERVAL DAY TO SECOND</code> object
   */
  // public static Interval valueOf(BigDecimal value) {
  //
  // BigDecimal abs = value.abs();
  // long decimal = abs.longValue();
  //
  // int decimals = 10;
  //
  // BigInteger INTEGER = abs.toBigInteger();
  // BigInteger DECIMAL = ((value.subtract(new BigDecimal(INTEGER))).multiply(new
  // BigDecimal(14).pow(decimals))).toBigInteger();
  //
  //
  // double fractional = abs.doubleValue()-decimal;
  //
  // long seconds = INTEGER.longValue() * SECONDS_PER_DAY;
  //
  // long nanos = 0L;
  //
  // //long seconds = abs.longValue() * SECONDS_PER_DAY;
  //
  //
  // return new Interval(0L, seconds, nanos, value.signum()<0);
  // }


  /**
   * Parse a string representation of a <code>INTERVAL YEAR TO SECOND</code>
   *
   * @param string A string representation of the form
   *        <code>[+|-][years]-[months] [+|-][days] [hours]:[minutes]:[seconds].[fractional seconds]</code>
   * @return The parsed <code>YEAR TO SECOND</code> object, or
   *         <code>null</code> if the string could not be parsed.
   */
  public static Interval valueOf(String string) {
    if (string != null) {

      Matcher matcher = PATTERN_YTS.matcher(string);
      if (matcher.find()) {
        boolean negative = "-".equals(matcher.group(1));
        int years = parseField(matcher.group(2));
        int months = parseField(matcher.group(3));
        int days = parseField(matcher.group(4));
        int hours = parseField(matcher.group(5));
        int minutes = parseField(matcher.group(6));
        int seconds = parseField(matcher.group(7));
        int nanos = parseField(StringUtils.rightPad(matcher.group(8), 9, "0"));

        return new Interval(years, months, days, hours, minutes, seconds, nanos, negative);
      }
      else {
        return parse(string);
      }
    }

    return null;
  }


  // -------------------------------------------------------------------------
  // Interval API
  // -------------------------------------------------------------------------

  /**
   * Negate the interval (change its sign)
   */
  public final Interval negate() {
    return new Interval(months, seconds, nanos, !negative);
  }

  /**
   * Abs the interval
   */
  public final Interval abs() {
    return new Interval(months, seconds, nanos, false);
  }
  
  /**
   * Get the absolute years part of the interval.
   */
  public final long getYears() {
    return months / MONTHS_PER_YEAR;
  }

  /**
   * Get the absolute months part of the interval.
   */
  public final long getMonths() {
    return months % MONTHS_PER_YEAR;
  }

  /**
   * Get the absolute days part of the interval.
   */
  public final long getDays() {
    return seconds / SECONDS_PER_DAY;
  }

  /**
   * Get the absolute hours part of the interval.
   */
  public final long getHours() {
    return (seconds % SECONDS_PER_DAY) / SECONDS_PER_HOUR;
  }

  /**
   * Get the absolute minutes part of the interval.
   */
  public final long getMinutes() {
    return (seconds % SECONDS_PER_HOUR) / SECONDS_PER_MINUTE;
  }

  /**
   * Get the absolute seconds part of the interval.
   */
  public final long getSeconds() {
    return seconds % SECONDS_PER_MINUTE;
  }

  /**
   * Get the absolute milliseconds part within seconds of the interval.
   */
  public final long getMilliseconds() {
    return nanos / 1000000;
  }

  /**
   * Get the absolute microseconds part within seconds of the interval.
   */
  public final long getMicroseconds() {
    return nanos / 1000;
  }

  /**
   * Get the absolute nanoseconds part within seconds of the interval.
   */
  public final long getNanoseconds() {
    return nanos;
  }

  /**
   * The sign of the interval.
   *
   * @return <code>1</code> for positive or zero, <code>-1</code> for negative
   */
  public final int getSign() {
    return negative ? -1 : 1;
  }


  // public final long toMonths() {
  // return months;
  // }
  //
  // public final long toSeconds() {
  // return seconds;
  // }
  //
  // public final long toNanos() {
  // return nanos;
  // }

  // -------------------------------------------------------------------------
  // Number API
  // -------------------------------------------------------------------------

  public final double doubleValue() {
    return ((getYears() * 365.25 + getMonths() * 30) * SECONDS_PER_DAY + seconds) * getSign();
  }

  // -------------------------------------------------------------------------
  // Comparable and Object API
  // -------------------------------------------------------------------------

  @Override
  public final int compareTo(Interval that) {
    return Double.compare(doubleValue(), that.doubleValue());
  }

  @Override
  public final int hashCode() {
    return Objects.hash(months, seconds, nanos, negative);
  }

  @Override
  public final boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() == obj.getClass()) {
      Interval other = (Interval) obj;
      if (months != other.months)
        return false;
      if (seconds != other.seconds)
        return false;
      if (nanos != other.nanos)
        return false;
      if (negative != other.negative && !isZero())
        return false;
      return true;
    } else
      return false;
  }

  /**
   * Checks if this interval is zero length.
   * <p>
   * A {@code Interval} represents a directed distance between two points on
   * the time-line and can therefore be positive, zero or negative.
   * This method checks whether the length is zero.
   *
   * @return true if this interval has a total length equal to zero
   */
  public boolean isZero() {
    return months == 0L && seconds == 0L && nanos == 0L;
  }

  @Override
  public final String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append(negative ? '-' : '+');
    sb.append(getYears());
    sb.append('-');
    sb.append(getMonths());
    sb.append(' ');
    sb.append(getDays());
    sb.append(' ');
    long h = getHours();
    if (h < 10) {
      sb.append('0');
    }
    sb.append(h);
    sb.append(':');
    long m = getMinutes();
    if (m < 10) {
      sb.append('0');
    }
    sb.append(m);
    sb.append(':');
    long s = getSeconds();
    if (s < 10) {
      sb.append('0');
    }
    sb.append(s);
    sb.append('.');
    sb.append(StringUtils.leftPad("" + nanos, 9, "0"));

    return sb.toString();
  }

  public final String toString(final IntervalQualifier qualifier) {
    StringBuilder sb = new StringBuilder();

    switch (qualifier) {
      case YEAR:
        if (negative)
          sb.append('-');
        sb.append(getYears());
        break;

      case YEAR_TO_MONTH:
        sb.append(negative ? '-' : '+');
        sb.append(getYears());
        sb.append('-');
        sb.append(getMonths());
        break;

      case MONTH:
        if (negative)
          sb.append('-');
        sb.append(getMonths());
        break;

      case DAY:
        if (negative)
          sb.append('-');
        sb.append(getDays());
        break;

      case HOUR:
        if (negative)
          sb.append('-');
        sb.append(getHours());
        break;

      case MINUTE:
        if (negative)
          sb.append('-');
        sb.append(getMinutes());
        break;

      case HOUR_TO_MINUTE:
        sb.append(negative ? '-' : '+');
        toStringHour(sb);
        sb.append(':');
        toStringMinute(sb);
        break;

      case DAY_TO_HOUR:
        sb.append(negative ? '-' : '+');
        sb.append(getDays());
        sb.append(' ');
        toStringHour(sb);
        break;

      case DAY_TO_SECOND:
        sb.append(negative ? '-' : '+');
        sb.append(getDays());
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
        if (negative)
          sb.append('-');
        toStringSecond(sb);
        break;

      default:
    }

    return sb.toString();
  }

  private void toStringHour(final StringBuilder sb) {
    long h = getHours();
    if (h < 10) {
      sb.append('0');
    }
    sb.append(h);
  }

  private void toStringMinute(final StringBuilder sb) {
    long m = getMinutes();
    if (m < 10) {
      sb.append('0');
    }
    sb.append(m);
  }

  private void toStringSecond(final StringBuilder sb) {
    long s = getSeconds();
    if (s < 10) {
      sb.append('0');
    }
    sb.append(s);

    // TODO: Use precision for nanoseconds
    if (nanos != 0) {
      sb.append('.');
      sb.append(StringUtils.leftPad("" + nanos, 9, "0"));
    }
  }

  public Interval minus(Interval other) {
    if (other.negative) {
      return new Interval(months + other.months, seconds + other.seconds, nanos + other.nanos,
          negative);
    }
    return new Interval(months - other.months, seconds - other.seconds, nanos - other.nanos,
        negative);
  }

  public Interval plus(Interval other) {
    if (other.negative) {
      return new Interval(months - other.months, seconds - other.seconds, nanos - other.nanos,
          negative);
    }
    return new Interval(months + other.months, seconds + other.seconds, nanos + other.nanos,
        negative);
  }

  /**
   * Adds this interval to the specified temporal object.
   * 
   * @param temporal the temporal object to adjust, not null
   */
  public ZonedDateTime addTo(ZonedDateTime temporal) {

    if (negative) {
      if (months != 0) {
        temporal = temporal.minus(months, ChronoUnit.MONTHS);
      }
      if (seconds != 0) {
        temporal = temporal.minus(seconds, ChronoUnit.SECONDS);
      }
      if (nanos != 0) {
        temporal = temporal.minus(nanos, ChronoUnit.NANOS);
      }
    } else {
      if (months != 0) {
        temporal = temporal.plus(months, ChronoUnit.MONTHS);
      }
      if (seconds != 0) {
        temporal = temporal.plus(seconds, ChronoUnit.SECONDS);
      }
      if (nanos != 0) {
        temporal = temporal.plus(nanos, ChronoUnit.NANOS);
      }
    }
    return temporal;
  }

  /**
   * Subtracts this interval from the specified temporal object.
   * 
   * @param temporal the temporal object to adjust, not null
   */
  public ZonedDateTime subtractFrom(ZonedDateTime temporal) {

    if (negative) {
      if (months != 0) {
        temporal = temporal.plus(months, ChronoUnit.MONTHS);
      }
      if (seconds != 0) {
        temporal = temporal.plus(seconds, ChronoUnit.SECONDS);
      }
      if (nanos != 0) {
        temporal = temporal.plus(nanos, ChronoUnit.NANOS);
      }
    } else {
      if (months != 0) {
        temporal = temporal.minus(months, ChronoUnit.MONTHS);
      }
      if (seconds != 0) {
        temporal = temporal.minus(seconds, ChronoUnit.SECONDS);
      }
      if (nanos != 0) {
        temporal = temporal.minus(nanos, ChronoUnit.NANOS);
      }
    }
    return temporal;
  }

  protected static int parseField(String str) {
    if (str == null || str.length() == 0)
      return 0;
    return Integer.parseInt(str);
  }

  protected static Interval parse(final String text) {
    int years = 0;
    int months = 0;
    int days = 0;
    int hours = 0;
    int minutes = 0;
    int seconds = 0;
    int nanos = 0;

    int length = text.length();
    int index = 0;
    while (index < length) {

      // Skip space
      if (Characters.isSpace(text.charAt(index))) {
        index++;
        continue;
      }

      int quantity = 0;
      int start = index;
      while (index < length) {
        char ch = text.charAt(index);
        int digit = Character.digit(ch, 10);
        if (digit < 0) {
          break;
        }
        index++;
        quantity *= 10;
        quantity += digit;
      }

      if (index == start) {     
        return null;
      }

      // Skip space
      while (index < length && Characters.isSpace(text.charAt(index))) {
        index++;
      }

      boolean noMatch = true;;
      for (TimeUnit unit : UNITS) {
        if (text.regionMatches(true, index, unit.name(), 0, unit.name().length())) {
          switch (unit) {
            case YEAR:
              years += quantity;
              break;
            case QUARTER:
              months += 3 * quantity;
              break;
            case MONTH:
              months += quantity;
              break;
            case WEEK:
              days += 7 * quantity;
              break;
            case DAY:
              days += quantity;
              break;
            case HOUR:
              hours += quantity;
              break;
            case MINUTE:
              minutes += quantity;
              break;
            case SECOND:
              seconds += quantity;
              break;
            default:
              return null;
          }

          index += unit.name().length();
          if (index < length) {
            char ch = text.charAt(index);
            if (ch == 's' || ch == 'S')
              index++;
          }

          noMatch = false;
          break;
        }
      }

      if (noMatch)
        return null;

      if (index < length && text.charAt(index) == ',') {
        index++;
      }
    }

    return new Interval(years, months, days, hours, minutes, seconds, nanos, false);
  }
}
