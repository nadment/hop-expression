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
import java.time.Duration;
import java.time.ZonedDateTime;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@SuppressWarnings("serial")
public final class DayToSecond extends Interval implements Serializable, Comparable<DayToSecond> {

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
   * The number of milliseconds per day.
   */
  public static final long MILLIS_PER_DAY = 24 * 60 * 60 * 1000L;

  /**
   * The number of seconds per hour.
   */
  public static final long SECONDS_PER_HOUR = 60 * 60;
    
  /**
   * The number of seconds per day.
   */
  public static final long SECONDS_PER_DAY = 24 * SECONDS_PER_HOUR;

  /**
   * The number of nanoseconds per second.
   */
  public static final long NANOS_PER_SECOND = 1_000_000_000;

  /**
   * The number of nanoseconds per minute.
   */
  public static final long NANOS_PER_MINUTE = 60 * NANOS_PER_SECOND;

  /**
   * The number of nanoseconds per hour.
   */
  public static final long NANOS_PER_HOUR = 60 * NANOS_PER_MINUTE;

  /**
   * The number of nanoseconds per day.
   */
  public static final long NANOS_PER_DAY = MILLIS_PER_DAY * 1_000_000;

  /**
   * {@code false} for zero or positive intervals, {@code true} for negative
   * intervals.
   */
  private final boolean negative;
  private final int days;
  private final int hours;
  private final int minutes;
  private final int seconds;
  private final int nanos;

  /**
   * Create a new interval.
   */
  public DayToSecond() {
    this(0, 0, 0, 0, 0, false);
  }

  /**
   * Create a new day interval.
   */
  public DayToSecond(int days) {
    this(days, 0, 0, 0, 0, false);
  }

  /**
   * Create a new day-hour interval.
   */
  public DayToSecond(int days, int hours) {
    this(days, hours, 0, 0, 0, false);
  }

  /**
   * Create a new day-minute interval.
   */
  public DayToSecond(int days, int hours, int minutes) {
    this(days, hours, minutes, 0, 0, false);
  }

  /**
   * Create a new day-second interval.
   */
  public DayToSecond(int days, int hours, int minutes, int seconds) {
    this(days, hours, minutes, seconds, 0, false);
  }

  /**
   * Create a new day-nanoseconds interval.
   */
  public DayToSecond(int days, int hours, int minutes, int seconds, int nanos) {
    this(days, hours, minutes, seconds, nanos, false);
  }

  DayToSecond(int days, int hours, int minutes, int seconds, int nanos, boolean negative) {

    // Perform normalization
    if (Math.abs(nanos) >= NANOS_PER_SECOND) {
      seconds += (nanos / NANOS_PER_SECOND);
      nanos %= NANOS_PER_SECOND;
    }
    if (Math.abs(seconds) >= 60) {
      minutes += (seconds / 60);
      seconds %= 60;
    }
    if (Math.abs(minutes) >= 60) {
      hours += (minutes / 60);
      minutes %= 60;
    }
    if (Math.abs(hours) >= 24) {
      days += (hours / 24);
      hours %= 24;
    }

    this.negative = negative;
    this.days = days;
    this.hours = hours;
    this.minutes = minutes;
    this.seconds = seconds;
    this.nanos = nanos;
  }

  /**
   * Parse a string representation of a <code>INTERVAL DAY TO SECOND</code>.
   *
   * @param string A string representation of the form
   *        <code>[+|-][days] [hours]:[minutes]:[seconds].[fractional seconds]</code>
   * @return The parsed <code>INTERVAL DAY TO SECOND</code> object, or
   *         <code>null</code> if the string could not be parsed.
   */
  public static DayToSecond valueOf(String string) {
    if (string != null) {

      // Accept also doubles as the number of milliseconds
      try {
        return valueOf(Double.parseDouble(string));
      } catch (NumberFormatException e) {
        DayToSecond result = dayToSecond(string);

        if (result != null)
          return result;
        else {
          try {
            return DayToSecond.valueOf(Duration.parse(string));
          } catch (DateTimeParseException ignore) {
          }
        }
      }
    }

    return null;
  }

  /**
   * Parse a string representation of a <code>INTERVAL DAY TO HOUR</code>.
   *
   * @param string A string representation of the form
   *        <code>[+|-][days]</code>
   * @return The parsed <code>INTERVAL DAY</code> object, or <code>null</code>
   *         if the string could not be parsed.
   */
  public static DayToSecond day(String string) {
    try {
      return string == null ? null : new DayToSecond(Integer.parseInt(string));
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
  public static DayToSecond dayToHour(String string) {
    if (string != null) {
      Matcher matcher = PATTERN_DTH.matcher(string);

      if (matcher.find())
        return parse(matcher, 0);
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
  public static DayToSecond dayToMinute(String string) {
    if (string != null) {
      Matcher matcher = PATTERN_DTM.matcher(string);

      if (matcher.find())
        return parse(matcher, 0);
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
  public static DayToSecond dayToSecond(String string) {
    if (string != null) {
      Matcher matcher = PATTERN_DTS.matcher(string);

      if (matcher.find())
        return parse(matcher, 0);
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
  public static DayToSecond hour(String string) {
    try {
      return string == null ? null : new DayToSecond(0, Integer.parseInt(string));
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
  public static DayToSecond hourToMinute(String string) {
    if (string != null) {
      Matcher matcher = PATTERN_HTM.matcher(string);

      if (matcher.find())
        return parse(matcher, 0);
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
  public static DayToSecond hourToSecond(String string) {
    if (string != null) {
      Matcher matcher = PATTERN_HTS.matcher(string);

      if (matcher.find())
        return parse(matcher, 0);
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
  public static DayToSecond minute(String string) {
    try {
      return string == null ? null : new DayToSecond(0, 0, Integer.parseInt(string));
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
  public static DayToSecond minuteToSecond(String string) {
    if (string != null) {
      Matcher matcher = PATTERN_MTS.matcher(string);

      if (matcher.find())
        return parse(matcher, 0);
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
  public static DayToSecond second(String string) {
    try {
      return string == null ? null : valueOf(Double.parseDouble(string) * 1000.0);
    } catch (NumberFormatException ignore) {
      return null;
    }
  }

  /**
   * Load a {@link Double} representation of a
   * <code>INTERVAL DAY TO SECOND</code> by assuming standard 24 hour days and
   * 60 second minutes.
   *
   * @param milli The number of milliseconds as a fractional number
   * @return The loaded <code>INTERVAL DAY TO SECOND</code> object
   */
  public static DayToSecond valueOf(double milli) {
    double abs = Math.abs(milli);

    int n = (int) ((abs % 1000) * 1000000.0);
    abs = Math.floor(abs / 1000);
    int s = (int) (abs % 60);
    abs = Math.floor(abs / 60);
    int m = (int) (abs % 60);
    abs = Math.floor(abs / 60);
    int h = (int) (abs % 24);
    abs = Math.floor(abs / 24);
    int d = (int) abs;

    DayToSecond result = new DayToSecond(d, h, m, s, n);

    if (milli < 0)
      result = result.negated();

    return result;
  }

  /**
   * Load a {@link Double} representation of a
   * <code>INTERVAL DAY TO SECOND</code> by assuming standard 24 hour days and
   * 60 second minutes.
   *
   * @param second The number of seconds
   * @param nanos The number of nano seconds
   * @return The loaded <code>INTERVAL DAY TO SECOND</code> object
   */
  public static DayToSecond valueOf(long second, int nanos) {
    long abs = Math.abs(second);

    int s = (int) (abs % 60L);
    abs = abs / 60L;
    int m = (int) (abs % 60L);
    abs = abs / 60L;
    int h = (int) (abs % 24L);
    abs = abs / 24L;
    int d = (int) abs;

    DayToSecond result = new DayToSecond(d, h, m, s, nanos);

    if (second < 0)
      result = result.negated();

    return result;
  }

  /**
   * Transform a {@link Duration} into a {@link DayToSecond} interval by
   * taking its number of milliseconds.
   */
  public static DayToSecond valueOf(Duration duration) {
    if (duration == null)
      return null;

    long s = duration.get(ChronoUnit.SECONDS);
    int n = (int) duration.get(ChronoUnit.NANOS);

    if (s < 0) {
      n = 1_000_000_000 - n;
      s++;
    }

    return valueOf(s, n);
  }

  // -------------------------------------------------------------------------
  // Interval API
  // -------------------------------------------------------------------------

  public final DayToSecond negated() {
    return new DayToSecond(days, hours, minutes, seconds, nanos, !negative);
  }

  public final DayToSecond abs() {
    return new DayToSecond(days, hours, minutes, seconds, nanos, false);
  }

  /**
   * Get the day-part of this interval.
   * 
   * @return days
   */
  public final int getDays() {
    return days;
  }

  /**
   * Get the hour-part of this interval.
   * 
   * @return hours
   */
  public final int getHours() {
    return hours;
  }

  /**
   * Get the minute-part of this interval.
   * 
   * @return minutes
   */
  public final int getMinutes() {
    return minutes;
  }

  /**
   * Get the second-part of this interval.
   * 
   * @return seconds
   */
  public final int getSeconds() {
    return seconds;
  }

  /**
   * Get the (truncated) milli-part of this interval.
   */
  public final int getMilli() {
    return nanos / 1000000;
  }

  /**
   * Get the (truncated) micro-part of this interval.
   */
  public final int getMicro() {
    return nanos / 1000;
  }

  /**
   * Get the nano-part of this interval.
   * 
   * @return nanoseconds
   */
  public final int getNanos() {
    return nanos;
  }

  /**
   * Get the whole interval in days
   */
  public final double getTotalDays() {
    return getSign() * (nanos / NANOS_PER_DAY + seconds / (24.0 * 3600.0) + minutes / (24.0 * 60.0)
        + hours / 24.0 + days);
  }

  /**
   * Get the whole interval in hours
   */
  public final double getTotalHours() {
    return getSign()
        * (nanos / NANOS_PER_HOUR + seconds / 3600.0 + minutes / 60.0 + hours + 24.0 * days);
  }

  /**
   * Get the whole interval in minutes
   */
  public final double getTotalMinutes() {
    return getSign()
        * (nanos / NANOS_PER_MINUTE + seconds / 60.0 + minutes + 60.0 * hours + 60.0 * 24.0 * days);
  }

  /**
   * Get the whole interval in seconds
   */
  public final double getTotalSeconds() {
    return getSign() * (nanos / NANOS_PER_SECOND + seconds + 60.0 * minutes + 3600.0 * hours
        + 3600.0 * 24.0 * days);
  }

  /**
   * Get the whole interval in milli-seconds
   */
  public final double getTotalMilli() {
    return getSign() * (nanos / 1000000.0 + 1000.0 * seconds + 1000.0 * 60.0 * minutes
        + 1000.0 * 3600.0 * hours + 1000.0 * 3600.0 * 24.0 * days);
  }

  /**
   * Get the whole interval in micro-seconds
   */
  public final double getTotalMicro() {
    return getSign() * (nanos / 1000.0 + 1000000.0 * seconds + 1000000.0 * 60.0 * minutes
        + 1000000.0 * 3600.0 * hours + 1000000.0 * 3600.0 * 24.0 * days);
  }

  /**
   * Get the whole interval in nano-seconds
   */
  public final double getTotalNanos() {
    return getSign() * (nanos + NANOS_PER_SECOND * seconds + NANOS_PER_MINUTE * minutes
        + NANOS_PER_HOUR * hours + NANOS_PER_DAY * days);
  }

  public final int getSign() {
    return negative ? -1 : 1;
  }

  @Override
  public final Duration toDuration() {
    return Duration.ofSeconds((long) getTotalSeconds(), getSign() * getNanos());
  }

  // -------------------------------------------------------------------------
  // Comparable and Object API
  // -------------------------------------------------------------------------

  @Override
  public final int compareTo(DayToSecond that) {
    if (days < that.days)
      return -1;
    else if (days > that.days)
      return 1;
    else if (hours < that.hours)
      return -1;
    else if (hours > that.hours)
      return 1;
    else if (minutes < that.minutes)
      return -1;
    else if (minutes > that.minutes)
      return 1;
    else if (seconds < that.seconds)
      return -1;
    else if (seconds > that.seconds)
      return 1;
    else
      return Integer.compare(nanos, that.nanos);
  }

  @Override
  public final int hashCode() {
    final int prime = 31;
    int result = 0;
    if (days != 0)
      result = prime * result + days;
    if (hours != 0)
      result = prime * result + hours;
    if (minutes != 0)
      result = prime * result + minutes;
    if (nanos != 0)
      result = prime * result + nanos;
    if (seconds != 0)
      result = prime * result + seconds;
    return result;
  }

  @Override
  public final boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() == obj.getClass()) {
      DayToSecond other = (DayToSecond) obj;
      if (days != other.days)
        return false;
      if (hours != other.hours)
        return false;
      if (minutes != other.minutes)
        return false;
      if (nanos != other.nanos)
        return false;
      if (seconds != other.seconds)
        return false;
      if (negative != other.negative && getTotalMilli() != 0.0)
        return false;
      return true;
    } else
      return false;
  }

  @Override
  public ZonedDateTime addTo(ZonedDateTime temporal) {
    long totalSeconds = (seconds + 60 * minutes + SECONDS_PER_HOUR * hours + SECONDS_PER_DAY * days);
    if (totalSeconds != 0) {
      return temporal.plus(getSign() * totalSeconds, ChronoUnit.SECONDS);
    }
    if (nanos != 0) {
      return temporal.plus(getSign() * nanos, ChronoUnit.NANOS);
    }

    return temporal;
  }
  
  @Override
  public ZonedDateTime subtractTo(ZonedDateTime temporal) {
    long totalSeconds = (seconds + 60 * minutes + SECONDS_PER_HOUR * hours + SECONDS_PER_DAY * days);
    if (totalSeconds != 0) {
      return temporal.minus(getSign() * totalSeconds, ChronoUnit.SECONDS);
    }
    if (nanos != 0) {
      return temporal.minus(getSign() * nanos, ChronoUnit.NANOS);
    }

    return temporal;    
  }

  @Override
  public final String toString() {
    StringBuilder sb = new StringBuilder();

    sb.append(negative ? '-' : '+');
    sb.append(days);
    sb.append(' ');
    if (hours < 10) {
      sb.append('0');
    }
    sb.append(hours);
    sb.append(':');
    if (minutes < 10) {
      sb.append('0');
    }
    sb.append(minutes);
    sb.append(':');
    if (seconds < 10) {
      sb.append('0');
    }
    sb.append(seconds);
    sb.append('.');
    sb.append(StringUtils.leftPad("" + nanos, 9, "0"));

    return sb.toString();
  }

  protected static DayToSecond parse(Matcher matcher, int groupOffset) {
    boolean negative = "-".equals(matcher.group(groupOffset + 1));
    int days = parseIntZero(matcher.group(groupOffset + 2));
    int hours = parseIntZero(matcher.group(groupOffset + 3));
    int minutes = parseIntZero(matcher.group(groupOffset + 4));
    int seconds = parseIntZero(matcher.group(groupOffset + 5));
    int nano = parseIntZero(StringUtils.rightPad(matcher.group(groupOffset + 6), 9, "0"));

    return new DayToSecond(days, hours, minutes, seconds, nano, negative);
  }

}
