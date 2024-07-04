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

import org.apache.hop.expression.TimeUnit;

public enum IntervalQualifier {
  YEAR,
  YEAR_TO_MONTH,
  QUARTER,
  MONTH,
  WEEK,
  DAY,
  DAY_TO_HOUR,
  DAY_TO_MINUTE,
  DAY_TO_SECOND,
  HOUR,
  HOUR_TO_MINUTE,
  HOUR_TO_SECOND,
  MINUTE,
  MINUTE_TO_SECOND,
  SECOND;

  /**
   * Returns a {@link IntervalQualifier} with a given start and end {@link TimeUnit}.
   *
   * @param startUnit The start time unit
   * @param endUnit The end time unit or null if not applicable
   * @return qualifier, or null if not valid
   */
  public static IntervalQualifier of(TimeUnit startUnit, TimeUnit endUnit) {
    if (startUnit == null) return null;

    switch (startUnit) {
      case YEAR:
        if (endUnit == null) return YEAR;
        if (endUnit == TimeUnit.MONTH) return YEAR_TO_MONTH;
        break;
      case MONTH:
        if (endUnit == null) return MONTH;
        break;
      case QUARTER:
        if (endUnit == null) return QUARTER;
        break;
      case WEEK:
        if (endUnit == null) return WEEK;
        break;
      case DAY:
        if (endUnit == null) return DAY;
        if (endUnit == TimeUnit.HOUR) return DAY_TO_HOUR;
        if (endUnit == TimeUnit.MINUTE) return DAY_TO_MINUTE;
        if (endUnit == TimeUnit.SECOND) return DAY_TO_SECOND;
        break;
      case HOUR:
        if (endUnit == null) return HOUR;
        if (endUnit == TimeUnit.MINUTE) return HOUR_TO_MINUTE;
        if (endUnit == TimeUnit.SECOND) return HOUR_TO_SECOND;
        break;
      case MINUTE:
        if (endUnit == null) return MINUTE;
        if (endUnit == TimeUnit.SECOND) return MINUTE_TO_SECOND;
        break;
      case SECOND:
        if (endUnit == null) return SECOND;
        break;

      default:
    }
    return null;
  }

  public static IntervalQualifier of(final Interval interval) {
    TimeUnit start = null;
    TimeUnit end = null;

    if (interval.getYears() != 0) {
      start = TimeUnit.YEAR;
    }
    if (interval.getMonths() != 0) {
      if (start == null) start = TimeUnit.MONTH;
      else end = TimeUnit.MONTH;
    }
    if (interval.getDays() != 0) {
      if (start == null) start = TimeUnit.DAY;
      else end = TimeUnit.DAY;
    }
    if (interval.getHours() != 0) {
      if (start == null) start = TimeUnit.HOUR;
      else end = TimeUnit.HOUR;
    }
    if (interval.getMinutes() != 0) {
      if (start == null) start = TimeUnit.MINUTE;
      else end = TimeUnit.MINUTE;
    }
    if (interval.getSeconds() != 0) {
      if (start == null) start = TimeUnit.SECOND;
      else end = TimeUnit.SECOND;
    }

    return of(start, end);
  }

  private final String string;

  private IntervalQualifier() {
    this.string = name().replace('_', ' ').intern();
  }

  /**
   * Returns {@code HOUR} for {@code HOUR TO SECOND} and{@code HOUR}, {@code SECOND} for {@code
   * SECOND}.
   */
  public TimeUnit getStartUnit() {
    switch (this) {
      case YEAR:
      case YEAR_TO_MONTH:
        return TimeUnit.YEAR;
      case QUARTER:
        return TimeUnit.QUARTER;
      case MONTH:
        return TimeUnit.MONTH;
      case WEEK:
        return TimeUnit.WEEK;
      case DAY:
      case DAY_TO_HOUR:
      case DAY_TO_MINUTE:
      case DAY_TO_SECOND:
        return TimeUnit.DAY;
      case HOUR:
      case HOUR_TO_MINUTE:
      case HOUR_TO_SECOND:
        return TimeUnit.HOUR;
      case MINUTE:
      case MINUTE_TO_SECOND:
        return TimeUnit.MINUTE;
      case SECOND:
        return TimeUnit.SECOND;
      default:
        throw new AssertionError(this);
    }
  }

  /**
   * Returns {@code SECOND} for both {@code HOUR TO SECOND} and {@code SECOND} for {@code SECOND}.
   */
  public TimeUnit getEndUnit() {
    switch (this) {
      case YEAR:
        return TimeUnit.YEAR;
      case QUARTER:
        return TimeUnit.QUARTER;
      case YEAR_TO_MONTH:
      case MONTH:
        return TimeUnit.MONTH;
      case WEEK:
        return TimeUnit.WEEK;
      case DAY:
        return TimeUnit.DAY;
      case DAY_TO_HOUR:
      case HOUR:
        return TimeUnit.HOUR;
      case DAY_TO_MINUTE:
      case HOUR_TO_MINUTE:
      case MINUTE:
        return TimeUnit.MINUTE;
      case DAY_TO_SECOND:
      case HOUR_TO_SECOND:
      case MINUTE_TO_SECOND:
      case SECOND:
        return TimeUnit.SECOND;
      default:
        throw new AssertionError(this);
    }
  }

  public Interval parse(final String text) {
    switch (this) {
      case YEAR:
        return Interval.year(text);
      case YEAR_TO_MONTH:
        return Interval.yearToMonth(text);
      case QUARTER:
        return Interval.quarter(text);
      case MONTH:
        return Interval.month(text);
      case WEEK:
        return Interval.week(text);
      case DAY:
        return Interval.day(text);
      case DAY_TO_HOUR:
        return Interval.dayToHour(text);
      case DAY_TO_MINUTE:
        return Interval.dayToMinute(text);
      case DAY_TO_SECOND:
        return Interval.dayToSecond(text);
      case HOUR:
        return Interval.hour(text);
      case HOUR_TO_MINUTE:
        return Interval.hourToMinute(text);
      case HOUR_TO_SECOND:
        return Interval.hourToSecond(text);
      case MINUTE:
        return Interval.minute(text);
      case MINUTE_TO_SECOND:
        return Interval.minuteToSecond(text);
      case SECOND:
        return Interval.second(text);
      default:
    }
    return null;
  }

  /**
   * Returns whether interval with this type has years.
   *
   * @return whether interval with this type has years
   */
  public boolean hasYears() {
    switch (this) {
      case YEAR:
      case YEAR_TO_MONTH:
        return true;
      default:
        return false;
    }
  }

  /**
   * Returns whether interval with this type has months.
   *
   * @return whether interval with this type has months
   */
  public boolean hasMonths() {
    switch (this) {
      case MONTH:
      case QUARTER:
      case YEAR_TO_MONTH:
        return true;
      default:
        return false;
    }
  }

  /**
   * Returns whether interval with this type has days.
   *
   * @return whether interval with this type has days
   */
  public boolean hasDays() {
    switch (this) {
      case DAY:
      case DAY_TO_HOUR:
      case DAY_TO_MINUTE:
      case DAY_TO_SECOND:
      case WEEK:
        return true;
      default:
        return false;
    }
  }

  /**
   * Returns whether interval with this type has hours.
   *
   * @return whether interval with this type has hours
   */
  public boolean hasHours() {
    switch (this) {
      case HOUR:
      case DAY_TO_HOUR:
      case DAY_TO_MINUTE:
      case DAY_TO_SECOND:
      case HOUR_TO_MINUTE:
      case HOUR_TO_SECOND:
        return true;
      default:
        return false;
    }
  }

  /**
   * Returns whether interval with this type has minutes.
   *
   * @return whether interval with this type has minutes
   */
  public boolean hasMinutes() {
    switch (this) {
      case MINUTE:
      case DAY_TO_MINUTE:
      case DAY_TO_SECOND:
      case HOUR_TO_MINUTE:
      case HOUR_TO_SECOND:
      case MINUTE_TO_SECOND:
        return true;
      default:
        return false;
    }
  }

  /**
   * Returns whether interval with this qualifier has seconds.
   *
   * @return whether interval with this qualifier has seconds
   */
  public boolean hasSeconds() {
    switch (this) {
      case SECOND:
      case DAY_TO_SECOND:
      case HOUR_TO_SECOND:
      case MINUTE_TO_SECOND:
        return true;
      default:
        return false;
    }
  }

  @Override
  public String toString() {
    return string;
  }
}
