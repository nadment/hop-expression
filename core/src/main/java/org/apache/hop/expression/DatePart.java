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
package org.apache.hop.expression;

import java.time.DayOfWeek;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoField;
import java.time.temporal.IsoFields;
import java.time.temporal.WeekFields;

/** Date part */
public enum DatePart {
  /** The number of seconds since 1970-01-01 00:00:00.00 */
  EPOCH,

  /** First day of its millennium. */
  MILLENNIUM,

  /** First day of its century. */
  CENTURY,

  /** First day of its decade. */
  DECADE,

  /** The years */
  YEAR("YY"),

  /** The years of week */
  YEAROFWEEK("YOW"),

  /** The years of week ISO */
  YEAROFWEEKISO("YOWISO"),

  /** The number (1 - 12) of the month */
  MONTH("MM"),

  /** The number (1 - 31) of the day */
  DAY("DD"),

  /** A number (1 for Sunday, 2 for Monday, …, 7 for Saturday) indicating the day of the week */
  DAYOFWEEK("DOW"),

  /**
   * A number (1 for Monday, …, 7 for Sunday) indicating the day of the week following the ISO-8601
   * standard
   */
  DAYOFWEEKISO("DOWISO"),

  /** A number (1 - 366) indicating the day of the year */
  DAYOFYEAR("DOY"),

  /** The number (1 - 54) of the week of the year */
  WEEK("WEEKOFYEAR"),

  /** Week of the year (number from 1-53). */
  WEEKISO("WEEKOFYEARISO"),

  /** Week from the beginning of the month (0-5) */
  WEEKOFMONTH,

  /** The quarter (1, 2, 3, or 4) of the year */
  QUARTER("Q"),

  /** The hour (0-23). */
  HOUR("HH"),

  /** The minute (0-59). */
  MINUTE("MI"),
  /** The second (0-59). */
  SECOND("SS"),

  MILLISECOND("MS"),

  MICROSECOND("MCS"),

  NANOSECOND("NS"),

  // TIMEZONE_REGION,

  /** The hour component of the time zone offset */
  TIMEZONE_HOUR,
  /** The minute component of the time zone offset */
  TIMEZONE_MINUTE

  ;

  private final String alias;

  private DatePart() {
    this.alias = null;
  }

  private DatePart(final String alias) {
    this.alias = alias;
  }

  /**
   * Create date part for the given name or throws exception if not exist.
   *
   * <p>
   * This method ignore case and search alias too
   *
   * @param str
   * @return DatePart
   */
  public static DatePart of(String str) {
    for (DatePart part : DatePart.values()) {
      if (part.name().equalsIgnoreCase(str)) {
        return part;
      }

      if (part.alias != null && part.alias.equalsIgnoreCase(str)) {
        return part;
      }
    }

    throw new IllegalArgumentException("Invalid date part: " + str);
  }

  /**
   * Check if date part exist.
   * 
   * @param str the name to check
   * @return
   */
  public static boolean exist(final String str) {
    for (DatePart part : DatePart.values()) {
      if (part.name().equalsIgnoreCase(str)) {
        return true;
      }
      if (part.alias != null && part.alias.equalsIgnoreCase(str)) {
        return true;
      }
    }
    return false;
  }

  private static int millennium(int year) {
    return year > 0 ? (year + 999) / 1000 : year / 1000;
  }

  private static int century(int year) {
    return year > 0 ? (year + 99) / 100 : year / 100;
  }

  private static int decade(int year) {
    return year >= 0 ? year / 10 : (year - 9) / 10;
  }

  public long get(ZonedDateTime dt) {
    switch (this) {
      case DAY:
        return dt.getDayOfMonth();
      case DAYOFYEAR:
        return dt.getDayOfYear();
      case DAYOFWEEK:
        int dow = dt.getDayOfWeek().getValue() + 1;
        if (dow == 8)
          dow = 1;
        return dow;
      case DAYOFWEEKISO:
        return dt.getDayOfWeek().getValue();
      case WEEK:
        return dt.get(ChronoField.ALIGNED_WEEK_OF_YEAR);
      case WEEKISO:
        return dt.get(IsoFields.WEEK_OF_WEEK_BASED_YEAR);
      case WEEKOFMONTH:
        return dt.get(ChronoField.ALIGNED_WEEK_OF_MONTH);
      case MONTH:
        return dt.getMonthValue();
      case QUARTER:
        return dt.get(IsoFields.QUARTER_OF_YEAR);
      case YEAR:
        return dt.getYear();
      case YEAROFWEEK:
        return dt.get(WeekFields.of(DayOfWeek.SUNDAY, 1).weekBasedYear());
      case YEAROFWEEKISO:
        // TODO: Verify DAYOFWEEKISO
        return dt.get(WeekFields.of(DayOfWeek.MONDAY, 1).weekBasedYear());
      case DECADE:
        return decade(dt.getYear());
      case CENTURY:
        return century(dt.getYear());
      case MILLENNIUM:
        return millennium(dt.getYear());
      case HOUR:
        return dt.getHour();
      case MINUTE:
        return dt.getMinute();
      case SECOND:
        return dt.getSecond();
      case MILLISECOND:
        return dt.get(ChronoField.MILLI_OF_SECOND);
      case MICROSECOND:
        return dt.get(ChronoField.MICRO_OF_SECOND);
      case NANOSECOND:
        return dt.getNano();
      case EPOCH:
        return dt.toEpochSecond();
      case TIMEZONE_HOUR:
      case TIMEZONE_MINUTE:
      default:
        throw new ExpressionException("Invalid date part: " + this);
    }
  }
}
