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

import org.apache.hop.expression.type.TypeName;

public enum IntervalQualifier {
  YEAR(TypeName.YEAR_TO_MONTH),
  YEAR_TO_MONTH(TypeName.YEAR_TO_MONTH), 
  MONTH(TypeName.YEAR_TO_MONTH),
  DAY(TypeName.DAY_TO_SECOND),
  DAY_TO_HOUR(TypeName.DAY_TO_SECOND),
  DAY_TO_MINUTE(TypeName.DAY_TO_SECOND),
  DAY_TO_SECOND(TypeName.DAY_TO_SECOND),
  HOUR(TypeName.DAY_TO_SECOND),
  HOUR_TO_MINUTE(TypeName.DAY_TO_SECOND),
  HOUR_TO_SECOND(TypeName.DAY_TO_SECOND),
  MINUTE(TypeName.DAY_TO_SECOND),
  MINUTE_TO_SECOND(TypeName.DAY_TO_SECOND),
  SECOND(TypeName.DAY_TO_SECOND);

  /**
   * Returns a {@link IntervalQualifier} with a given start and end {@link TimeUnit}.
   * 
   * @param startUnit The start time unit
   * @param endUnit The end time  unit or null if not applicable
   * @return qualifier, or null if not valid
   */
  public static IntervalQualifier of(TimeUnit startUnit, TimeUnit endUnit) {
    if ( startUnit==null)
      return null;
    
    switch (startUnit) {
      case YEAR:
        if (endUnit == TimeUnit.MONTH)
          return YEAR_TO_MONTH;
        return YEAR;
      case MONTH:
        return MONTH;

      case DAY:
        if (endUnit == TimeUnit.HOUR)
          return DAY_TO_HOUR;
        if (endUnit == TimeUnit.MINUTE)
          return DAY_TO_MINUTE;
        if (endUnit == TimeUnit.SECOND)
          return DAY_TO_SECOND;
        return DAY;

      case HOUR:
        if (endUnit == TimeUnit.MINUTE)
          return HOUR_TO_MINUTE;
        if (endUnit == TimeUnit.SECOND)
          return HOUR_TO_SECOND;
        return HOUR;

      case MINUTE:
        if (endUnit == TimeUnit.SECOND)
          return MINUTE_TO_SECOND;
        return MINUTE;

      case SECOND:
        return SECOND;

      default:
        return null;
    }
  }
  
  private final String string;
  private final TypeName type;
  
  private IntervalQualifier(TypeName type) {
    this.type = type;
    this.string = name().replace('_', ' ').intern();
  }

  public TypeName getTypeName() {
    return type;
  }
  
  /**
   * Returns {@code HOUR} for {@code HOUR TO SECOND} and{@code HOUR}, {@code SECOND} for
   * {@code SECOND}.
   */
  public TimeUnit getStartUnit() {
    switch (this) {
      case YEAR:
      case YEAR_TO_MONTH:
        return TimeUnit.YEAR;
      case MONTH:
        return TimeUnit.MONTH;
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
      case YEAR_TO_MONTH:
      case MONTH:
        return TimeUnit.MONTH;
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
        return YearToMonth.year(text);
      case YEAR_TO_MONTH:
        return YearToMonth.yearToMonth(text);
      case MONTH:
        return YearToMonth.month(text);
      case DAY:
        return DayToSecond.day(text);
      case DAY_TO_HOUR:
        return DayToSecond.dayToHour(text);
      case DAY_TO_MINUTE:
        return DayToSecond.dayToMinute(text);
      case DAY_TO_SECOND:
        return DayToSecond.dayToSecond(text);
      case HOUR:
        return DayToSecond.hour(text);
      case HOUR_TO_MINUTE:
        return DayToSecond.hourToMinute(text);
      case HOUR_TO_SECOND:
        return DayToSecond.hourToSecond(text);
      case MINUTE:
        return DayToSecond.minute(text);
      case MINUTE_TO_SECOND:
        return DayToSecond.minuteToSecond(text);
      case SECOND:
        return DayToSecond.second(text);
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
