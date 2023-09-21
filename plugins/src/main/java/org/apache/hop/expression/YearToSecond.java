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
import java.time.Period;
import java.time.ZonedDateTime;
import java.time.format.DateTimeParseException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@SuppressWarnings("serial")
public class YearToSecond extends Interval implements Serializable, Comparable<YearToSecond> {

  private static final Pattern PATTERN = Pattern.compile("^([+-])?(\\d+)-(\\d+) ([+-])?(?:(\\d+) )?(\\d+):(\\d+):(\\d+)(?:\\.(\\d+))?$");

  private final YearToMonth yearToMonth;
  private final DayToSecond dayToSecond;
  

  public YearToSecond(YearToMonth yearToMonth, DayToSecond dayToSecond) {
      this.yearToMonth = yearToMonth == null ? new YearToMonth() : yearToMonth;
      this.dayToSecond = dayToSecond == null ? new DayToSecond() : dayToSecond;
  }

  /**
   * Load a {@link Double} representation of a
   * <code>INTERVAL YEAR TO SECOND</code> by assuming standard 24 hour days and
   * 60 second minutes.
   *
   * @param milli The number of milliseconds as a fractional number
   * @return The loaded <code>INTERVAL DAY TO SECOND</code> object
   */
  public static YearToSecond valueOf(double milli) {
      double abs = Math.abs(milli);

      int y = (int) (abs / (365.25 * 86400000.0)); abs = abs % (365.25 * 86400000.0);
      int m = (int) (abs / (30.0 * 86400000.0)); abs = abs % (30.0 * 86400000.0);

      YearToSecond result = new YearToSecond(new YearToMonth(y, m), DayToSecond.valueOf(abs));

      if (milli < 0)
          result = result.negated();

      return result;
  }

  /**
   * Transform a {@link Duration} into a {@link YearToSecond} interval by
   * taking its number of milliseconds.
   */
  public static YearToSecond valueOf(Duration duration) {
      return duration == null ? null : valueOf(duration.toMillis());
  }

  @Override
  public final Duration toDuration() {
      return yearToMonth.toDuration().plus(dayToSecond.toDuration());
  }

  /**
   * Transform a {@link Period} into a {@link YearToSecond} interval.
   */
  public static YearToSecond valueOf(Period period) {
      return period == null ? null : new YearToSecond(
          new YearToMonth(period.getYears(), period.getMonths()),
          new DayToSecond(period.getDays())
      );
  }

  /**
   * Parse a string representation of a <code>INTERVAL YEAR TO SECOND</code>
   *
   * @param string A string representation of the form
   *            <code>[+|-][years]-[months] [+|-][days] [hours]:[minutes]:[seconds].[fractional seconds]</code>
   * @return The parsed <code>YEAR TO SECOND</code> object, or
   *         <code>null</code> if the string could not be parsed.
   */
  public static YearToSecond valueOf(String string) {
      if (string != null) {

          // Accept also doubles as the number of milliseconds
          try {
              return valueOf(Double.parseDouble(string));
          }
          catch (NumberFormatException e) {
              Matcher matcher = PATTERN.matcher(string);

              if (matcher.find()) {
                  return new YearToSecond(parseYM(matcher, 0), parseDS(matcher, 3));
              }
              else {
                  try {
                      return YearToSecond.valueOf(Duration.parse(string));
                  }
                  catch (DateTimeParseException ignore) {}
              }
          }
      }

      return null;
  }

  static YearToMonth parseYM(Matcher matcher, int groupOffset) {
      boolean negativeYM = "-".equals(matcher.group(groupOffset + 1));
      int years = Integer.parseInt(matcher.group(groupOffset + 2));
      int months = Integer.parseInt(matcher.group(groupOffset + 3));

      return new YearToMonth(years, months, negativeYM);
  }

  static DayToSecond parseDS(Matcher matcher, int groupOffset) {
      boolean negativeDS = "-".equals(matcher.group(groupOffset + 1));

      int days = parseIntZero(matcher.group(groupOffset + 2));
      int hours = parseIntZero(matcher.group(groupOffset + 3));
      int minutes = parseIntZero(matcher.group(groupOffset + 4));
      int seconds = parseIntZero(matcher.group(groupOffset + 5));
      int nano = parseIntZero(StringUtils.rightPad(matcher.group(groupOffset + 6), 9, "0"));

      return new DayToSecond(days, hours, minutes, seconds, nano, negativeDS);
  }

  // -------------------------------------------------------------------------
  // Interval API
  // -------------------------------------------------------------------------

  @Override
  public final YearToSecond negated() {
      return new YearToSecond(yearToMonth.negated(), dayToSecond.negated());
  }

  @Override
  public final YearToSecond abs() {
      return new YearToSecond(yearToMonth.abs(), dayToSecond.abs());
  }

  public final YearToMonth getYearToMonth() {
      return yearToMonth;
  }

  public final DayToSecond getDayToSecond() {
      return dayToSecond;
  }

  public final int getYears() {
      return yearToMonth.getYears();
  }

  public final int getMonths() {
      return yearToMonth.getMonths();
  }

  /**
   * Get the day-part of this interval
   */
  public final int getDays() {
      return dayToSecond.getDays();
  }

  /**
   * Get the hour-part of this interval
   */
  public final int getHours() {
      return dayToSecond.getHours();
  }

  /**
   * Get the minute-part of this interval
   */
  public final int getMinutes() {
      return dayToSecond.getMinutes();
  }

  /**
   * Get the second-part of this interval
   */
  public final int getSeconds() {
      return dayToSecond.getSeconds();
  }

  /**
   * Get the nano-part of this interval
   */
  public final int getNanos() {
      return dayToSecond.getNanos();
  }

  @Override
  public final int getSign() {
      double value = doubleValue();

      return value > 0
           ? 1
           : value < 0
           ? -1
           : 0;
  }

  // -------------------------------------------------------------------------
  // Number API
  // -------------------------------------------------------------------------

  public final double doubleValue() {
      return (yearToMonth.getYears() * 365.25
            + yearToMonth.getMonths() * 30) * 86400000 * yearToMonth.getSign()
            + dayToSecond.getTotalSeconds();
  }

  // -------------------------------------------------------------------------
  // Comparable and Object API
  // -------------------------------------------------------------------------

  @Override
  public final int compareTo(YearToSecond that) {
      return Double.compare(doubleValue(), that.doubleValue());
  }

  @Override
  public int hashCode() {
      final int prime = 31;
      int result = 0;
      int h1 = dayToSecond.hashCode();
      int h2 = yearToMonth.hashCode();
      if (h1 != 0)
          result = prime * result + h1;
      if (h2 != 0)
          result = prime * result + h2;
      return result;
  }

  @Override
  public boolean equals(Object obj) {
      if (this == obj)
          return true;
      if (obj == null)
          return false;
      if (getClass() == obj.getClass()) {
          YearToSecond other = (YearToSecond) obj;
          if (dayToSecond == null) {
              if (other.dayToSecond != null)
                  return false;
          }
          else if (!dayToSecond.equals(other.dayToSecond))
              return false;
          if (yearToMonth == null) {
              if (other.yearToMonth != null)
                  return false;
          }
          else if (!yearToMonth.equals(other.yearToMonth))
              return false;
          return true;
      }
      else if (obj instanceof YearToMonth) {
          YearToMonth other = (YearToMonth) obj;
          return getDayToSecond().getTotalSeconds() == 0 &&
                 getYearToMonth().equals(other);
      }
      else if (obj instanceof DayToSecond) {
          DayToSecond other = (DayToSecond) obj;
          return getYearToMonth().getTotalMonths() == 0 &&
                 getDayToSecond().equals(other);
      }
      else
          return false;
  }

  @Override
  public final String toString() {
      StringBuilder sb = new StringBuilder();
      sb.append(yearToMonth);
      sb.append(' ');
      sb.append(dayToSecond);
      return sb.toString();
  }

  @Override
  public ZonedDateTime addTo(ZonedDateTime temporal) {    
    return yearToMonth.addTo(dayToSecond.addTo(temporal));
  }

  @Override
  public ZonedDateTime subtractTo(ZonedDateTime temporal) {
    return yearToMonth.subtractTo(dayToSecond.subtractTo(temporal));
  }
}
