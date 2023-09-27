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

import org.apache.commons.lang3.StringUtils;
import java.io.Serializable;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.Duration;
import java.time.YearMonth;
import java.time.ZonedDateTime;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoUnit;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A interval represents a duration of time which can be used in date/time arithmetic.
 */
@SuppressWarnings("serial")
public class Interval implements Serializable, Comparable<Interval> {

  private static final Pattern PATTERN_YTS = Pattern
      .compile("^([+-])?(\\d+)-(\\d+) (?:(\\d+) )?(\\d+):(\\d+):(\\d+)(?:\\.(\\d+))?$");
  private static final Pattern PATTERN_YTM = Pattern.compile("^([+-])?(\\d+)-(\\d+)$");
  private static final Pattern PATTERN_DTS =
      Pattern.compile("^([+-])?(?:(\\d+) )?(\\d+):(\\d+):(\\d+)(?:\\.(\\d+))?$");
  private static final Pattern PATTERN_DTM =
      Pattern.compile("^([+-])?(?:(\\d+) )?(\\d+):(\\d+)()()$");
  private static final Pattern PATTERN_DTH = Pattern.compile("^([+-])?(?:(\\d+) )?(\\d+)()()()$");
  private static final Pattern PATTERN_HTS =
      Pattern.compile("^([+-])?()(\\d+):(\\d+):(\\d+)(?:\\.(\\d+))?$");
  private static final Pattern PATTERN_HTM = Pattern.compile("^([+-])?()(\\d+):(\\d+)()()$");
  private static final Pattern PATTERN_MTS =
      Pattern.compile("^([+-])?()()(\\d+):(\\d+)(?:\\.(\\d+))?$");

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
  public static Interval year(String string) {
    try {
      return string == null ? null : new Interval(Integer.parseInt(string));
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
  public static Interval yearToMonth(String string) {
    if (string != null) {
      Matcher matcher;

      if ((matcher = PATTERN_YTM.matcher(string)).find())
        return parseYM(matcher);
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
  public static Interval quarter(String string) {
    try {
      
      return string == null ? null : new Interval(0, 3 * Integer.parseInt(string));
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
  public static Interval month(String string) {
    try {
      return string == null ? null : new Interval(0, Integer.parseInt(string));
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
  public static Interval week(String string) {
    try {
      
      return string == null ? null : new Interval(0, 0, 7 * Integer.parseInt(string));
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
  public static Interval day(String string) {
    try {
      return string == null ? null : new Interval(0, 0, Integer.parseInt(string));
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
  public static Interval dayToHour(String string) {
    if (string != null) {
      Matcher matcher = PATTERN_DTH.matcher(string);

      if (matcher.find())
        return parseDS(matcher);
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
  public static Interval dayToMinute(String string) {
    if (string != null) {
      Matcher matcher = PATTERN_DTM.matcher(string);

      if (matcher.find())
        return parseDS(matcher);
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
  public static Interval dayToSecond(String string) {
    if (string != null) {
      Matcher matcher = PATTERN_DTS.matcher(string);

      if (matcher.find())
        return parseDS(matcher);
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
  public static Interval hour(String string) {
    try {
      return string == null ? null : new Interval(0, 0, 0, Integer.parseInt(string));
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
  public static Interval hourToMinute(String string) {
    if (string != null) {
      Matcher matcher = PATTERN_HTM.matcher(string);

      if (matcher.find())
        return parseDS(matcher);
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
  public static Interval hourToSecond(String string) {
    if (string != null) {
      Matcher matcher = PATTERN_HTS.matcher(string);

      if (matcher.find())
        return parseDS(matcher);
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
  public static Interval minute(String string) {
    try {
      return string == null ? null : new Interval(0, 0, 0, 0, Integer.parseInt(string));
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
  public static Interval minuteToSecond(String string) {
    if (string != null) {
      Matcher matcher = PATTERN_MTS.matcher(string);

      if (matcher.find())
        return parseDS(matcher);
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
  public static Interval second(String string) {
    try {
      return string == null ? null : new Interval(0, 0, 0, 0, 0, Integer.parseInt(string));
    } catch (NumberFormatException ignore) {
      return null;
    }
  }

  /**
   * {@code false} for zero or positive intervals, {@code true} for negative
   * intervals.
   */
  private final boolean negative;
  private final long months;
  private final long seconds;
  private final long nanos;

  /**
   * Create a new interval.
   */
  public Interval() {
    this(0, 0, 0, 0, 0, 0, 0, false);
  }

  /**
   * Create a new year interval.
   */
  public Interval(int years) {
    this(years, 0, 0, 0, 0, 0, 0, false);
  }


  /**
   * Create a new year-month interval.
   */
  public Interval(int years, int months) {
    this(years, months, 0, 0, 0, 0, 0, false);
  }

  /**
   * Create a new year-day interval.
   */
  public Interval(int years, int months, int days) {
    this(years, months, days, 0, 0, 0, 0, false);
  }

  /**
   * Create a new year-hour interval.
   */
  public Interval(int years, int months, int days, int hours) {
    this(years, months, days, hours, 0, 0, 0, false);
  }

  /**
   * Create a new year-minute interval.
   */
  public Interval(int years, int months, int days, int hours, int minutes) {
    this(years, months, days, hours, minutes, 0, 0, false);
  }

  /**
   * Create a new year-second interval.
   */
  public Interval(int years, int months, int days, int hours, int minutes, int seconds) {
    this(years, months, days, hours, minutes, seconds, 0, false);
  }

  /**
   * Create a new year-second with nanoseconds interval.
   */
  public Interval(int years, int months, int days, int hours, int minutes, int seconds, int nanos) {
    this(years, months, days, hours, minutes, seconds, nanos, false);
  }
  
  Interval(int years, int months, int days, int hours, int minutes, int seconds, int nanos,
      boolean negative) {
    this.months = years * 12L + months;
    this.seconds =
        days * SECONDS_PER_DAY + hours * SECONDS_PER_HOUR + minutes * SECONDS_PER_MINUTE + seconds;
    this.nanos = nanos;
    this.negative = negative;
  }

  Interval(long months, long seconds, long nanos, boolean negative) {
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
//   public static Interval valueOf(BigDecimal value) {
//  
//   BigDecimal abs = value.abs();
//   long decimal = abs.longValue();
//   
//   int decimals = 10;
//
//   BigInteger INTEGER = abs.toBigInteger();
//   BigInteger DECIMAL = ((value.subtract(new BigDecimal(INTEGER))).multiply(new BigDecimal(14).pow(decimals))).toBigInteger();
//   
//
//   double fractional  = abs.doubleValue()-decimal;
//      
//   long seconds = INTEGER.longValue() * SECONDS_PER_DAY;
//   
//   long nanos = 0L;
//      
//   //long seconds = abs.longValue() * SECONDS_PER_DAY;
//   
//   
//   return new Interval(0L, seconds, nanos, value.signum()<0);
//  }

  /**
   * Transform a {@link Duration} into a {@link Interval} interval by
   * taking its number of milliseconds.
   */
  // public static Interval2 valueOf(Duration duration) {
  // return duration == null ? null : valueOf(duration.toMillis());
  // }

  /**
   * Get a duration representation of this interval.
   * <p>
   * There is an obvious {@link Duration} representation for
   * {@link Interval} intervals. If the interval contains {@link YearMonth}
   * information, then the corresponding duration will use:
   * <p>
   * <ul>
   * <li>1 year = 365.25 days</li>
   * <li>1 month = 30 days</li>
   * </ul>
   * <p>
   * This corresponds to PostgreSQL's
   * <code>EXTRACT(EPOCH FROM my_interval)</code> behaviour.
   */
  public final Duration toDuration() {
    return null;
  }

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

      if (matcher.find())
        return parseYS(matcher);
      else try {
       // return Interval.valueOf(Duration.parse(string));
      }
      catch (DateTimeParseException ignore) {
      }
    }

    return null;

    // TODO: Accept also doubles as the number of milliseconds
    // try {
    // return valueOf(Double.parseDouble(string));
    // } catch (NumberFormatException e) {
    // Matcher matcher = PATTERN.matcher(string);
    //
    // if (matcher.find()) {
    // return new Interval(parseYM(matcher, 0), parseDS(matcher, 3));
    // } else {
    // try {
    // return Interval.valueOf(Duration.parse(string));
    // } catch (DateTimeParseException ignore) {
    // }
    // }
    // }
    // }
  }


  // -------------------------------------------------------------------------
  // Interval API
  // -------------------------------------------------------------------------

  /**
   * Negate the interval (change its sign)
   */
  public final Interval negated() {
    return new Interval(months, seconds, nanos, !negative);
  }

  /**
   * Get the absolute value of the interval (set its sign to positive)
   */
  public final Interval abs() {
    return new Interval(months, seconds, nanos, false);
  }

  /**
   * Get the years part of the interval.
   */
  public final long getYears() {
    return months / 12;
  }

  /**
   * Get the months part of the interval.
   */
  public final long getMonths() {
    return months % 12;
  }

  /**
   * Get the days part of the interval.
   */
  public final long getDays() {
    return seconds / SECONDS_PER_DAY;
  }

  /**
   * Get the hours part of the interval.
   */
  public final long getHours() {
    return (seconds % SECONDS_PER_DAY) / SECONDS_PER_HOUR;
  }

  /**
   * Get the minutes part of the interval.
   */
  public final long getMinutes() {
    return (seconds % SECONDS_PER_HOUR) / SECONDS_PER_MINUTE;
  }

  /**
   * Get the seconds part of the interval.
   */
  public final long getSeconds() {
    return seconds % SECONDS_PER_MINUTE;
  }

  /**
   * Get the milliseconds part within seconds of the interval.
   */
  public final long getMillis() {
    return nanos / 1000000;
  }

  /**
   * Get the microseconds part within seconds of the interval.
   */
  public final long getMicros() {
    return nanos / 1000;
  }
  
  /**
   * Get the nanoseconds part within seconds of the interval.
   */
  public final long getNanos() {
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

  public Interval minus(Interval other) {
    if (other.negative) {
      return new Interval(months+other.months, seconds+other.seconds, nanos+other.nanos, negative);
    }    
    return new Interval(months-other.months, seconds-other.seconds, nanos-other.nanos, negative);
  }

  public Interval plus(Interval other) {
    if (other.negative) {
      return new Interval(months-other.months, seconds-other.seconds, nanos-other.nanos, negative);
    }
    return new Interval(months+other.months, seconds+other.seconds, nanos+other.nanos, negative);
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

  protected static int parseInt(String str) {
    if (str == null || str.length() == 0)
      return 0;
    return Integer.parseInt(str);
  }

  protected static Interval parseYM(Matcher matcher) {
    boolean negative = "-".equals(matcher.group(1));
    int years = parseInt(matcher.group(2));
    int months = parseInt(matcher.group(3));

    return new Interval(years, months, 0, 0, 0, 0, 0, negative);
  }

  protected static Interval parseDS(Matcher matcher) {
    boolean negative = "-".equals(matcher.group(1));
    int days = parseInt(matcher.group(2));
    int hours = parseInt(matcher.group(3));
    int minutes = parseInt(matcher.group(4));
    int seconds = parseInt(matcher.group(5));
    int nanos = parseInt(StringUtils.rightPad(matcher.group(6), 9, "0"));

    return new Interval(0, 0, days, hours, minutes, seconds, nanos, negative);
  }

  protected static Interval parseYS(Matcher matcher) {
    boolean negative = "-".equals(matcher.group(1));
    int years = parseInt(matcher.group(2));
    int months = parseInt(matcher.group(3));
    int days = parseInt(matcher.group(4));
    int hours = parseInt(matcher.group(5));
    int minutes = parseInt(matcher.group(6));
    int seconds = parseInt(matcher.group(7));
    int nanos = parseInt(StringUtils.rightPad(matcher.group(8), 9, "0"));

    return new Interval(years, months, days, hours, minutes, seconds, nanos, negative);
  }
}
