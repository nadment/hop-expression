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

  private final String string;

  IntervalQualifier() {
    this.string = name().replace('_', ' ').intern();
  }

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

  /**
   * Returns {@code HOUR} for {@code HOUR TO SECOND} and{@code HOUR}, {@code SECOND} for {@code
   * SECOND}.
   */
  public TimeUnit getStartUnit() {
    return switch (this) {
      case YEAR, YEAR_TO_MONTH -> TimeUnit.YEAR;
      case QUARTER -> TimeUnit.QUARTER;
      case MONTH -> TimeUnit.MONTH;
      case WEEK -> TimeUnit.WEEK;
      case DAY, DAY_TO_HOUR, DAY_TO_MINUTE, DAY_TO_SECOND -> TimeUnit.DAY;
      case HOUR, HOUR_TO_MINUTE, HOUR_TO_SECOND -> TimeUnit.HOUR;
      case MINUTE, MINUTE_TO_SECOND -> TimeUnit.MINUTE;
      case SECOND -> TimeUnit.SECOND;
    };
  }

  /**
   * Returns {@code SECOND} for both {@code HOUR TO SECOND} and {@code SECOND} for {@code SECOND}.
   */
  public TimeUnit getEndUnit() {
    return switch (this) {
      case YEAR -> TimeUnit.YEAR;
      case QUARTER -> TimeUnit.QUARTER;
      case YEAR_TO_MONTH, MONTH -> TimeUnit.MONTH;
      case WEEK -> TimeUnit.WEEK;
      case DAY -> TimeUnit.DAY;
      case DAY_TO_HOUR, HOUR -> TimeUnit.HOUR;
      case DAY_TO_MINUTE, HOUR_TO_MINUTE, MINUTE -> TimeUnit.MINUTE;
      case DAY_TO_SECOND, HOUR_TO_SECOND, MINUTE_TO_SECOND, SECOND -> TimeUnit.SECOND;
    };
  }

  public Interval parse(final String text) {
    return switch (this) {
      case YEAR -> Interval.year(text);
      case YEAR_TO_MONTH -> Interval.yearToMonth(text);
      case QUARTER -> Interval.quarter(text);
      case MONTH -> Interval.month(text);
      case WEEK -> Interval.week(text);
      case DAY -> Interval.day(text);
      case DAY_TO_HOUR -> Interval.dayToHour(text);
      case DAY_TO_MINUTE -> Interval.dayToMinute(text);
      case DAY_TO_SECOND -> Interval.dayToSecond(text);
      case HOUR -> Interval.hour(text);
      case HOUR_TO_MINUTE -> Interval.hourToMinute(text);
      case HOUR_TO_SECOND -> Interval.hourToSecond(text);
      case MINUTE -> Interval.minute(text);
      case MINUTE_TO_SECOND -> Interval.minuteToSecond(text);
      case SECOND -> Interval.second(text);
    };
  }

  /**
   * Returns whether interval with this type has years.
   *
   * @return whether interval with this type has years
   */
  public boolean hasYears() {
    return this == YEAR || this == YEAR_TO_MONTH;
  }

  /**
   * Returns whether interval with this type has months.
   *
   * @return whether interval with this type has months
   */
  public boolean hasMonths() {
    return this == MONTH || this == QUARTER || this == YEAR_TO_MONTH;
  }

  /**
   * Returns whether interval with this type has days.
   *
   * @return whether interval with this type has days
   */
  public boolean hasDays() {
    return this == DAY
        || this == DAY_TO_HOUR
        || this == DAY_TO_MINUTE
        || this == DAY_TO_SECOND
        || this == WEEK;
  }

  /**
   * Returns whether interval with this type has hours.
   *
   * @return whether interval with this type has hours
   */
  public boolean hasHours() {
    return this == HOUR
        || this == DAY_TO_HOUR
        || this == DAY_TO_MINUTE
        || this == DAY_TO_SECOND
        || this == HOUR_TO_MINUTE
        || this == HOUR_TO_SECOND;
  }

  /**
   * Returns whether interval with this type has minutes.
   *
   * @return whether interval with this type has minutes
   */
  public boolean hasMinutes() {
    return this == MINUTE
        || this == DAY_TO_MINUTE
        || this == DAY_TO_SECOND
        || this == HOUR_TO_MINUTE
        || this == HOUR_TO_SECOND
        || this == MINUTE_TO_SECOND;
  }

  /**
   * Returns whether interval with this qualifier has seconds.
   *
   * @return whether interval with this qualifier has seconds
   */
  public boolean hasSeconds() {
    return this == SECOND
        || this == DAY_TO_SECOND
        || this == HOUR_TO_SECOND
        || this == MINUTE_TO_SECOND;
  }

  @Override
  public String toString() {
    return string;
  }
}
