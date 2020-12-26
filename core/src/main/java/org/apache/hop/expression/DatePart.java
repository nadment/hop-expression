/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression;

import java.time.ZonedDateTime;
import java.time.temporal.ChronoField;
import java.time.temporal.IsoFields;

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
  DAYOFWEEK_ISO("DOW_ISO"),

  /** A number (1 - 366) indicating the day of the year */
  DAYOFYEAR("DOY"),

  /** The number (0 - 54) of the week of the year */
  WEEK("WEEKOFYEAR"),

  /** Week of the year (number from 1-53). */
  WEEK_ISO("WEEKOFYEAR_ISO"),

  /** Week from the beginning of the month (0-5) */
  WEEKOFMONTH,

  /** The quarter (1, 2, 3, or 4) of the year */
  QUARTER,

  /** The hour (0-23). */
  HOUR("HH"),

  /** The minute (0-59). */
  MINUTE("MI"),
  /** The second (0-59). */
  SECOND("SS"),
  MILLISECOND("MS"),
  MICROSECOND("MCS"),
  NANOSECOND("NS");

  private final String alias;

  private DatePart() {
    this.alias = null;
  }

  private DatePart(final String alias) {
    this.alias = alias;
  }

  public static DatePart of(int ordinal) {
    return values()[ordinal];
  }

  /**
   * Create date part, or null if not found
   *
   * <p>This method ignore case and search alias too
   *
   * @param s
   * @return
   */
  public static DatePart of(String s) {
    for (DatePart part : DatePart.values()) {
      if (part.name().equalsIgnoreCase(s)) {
        return part;
      }

      if (part.alias != null && part.alias.equalsIgnoreCase(s)) {
        return part;
      }
    }
    return null;
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
        if (dow == 8) dow = 1;
        return dow;
      case DAYOFWEEK_ISO:
        return dt.getDayOfWeek().getValue();
      case WEEK:
        return dt.get(ChronoField.ALIGNED_WEEK_OF_YEAR);
      case WEEK_ISO:
        return dt.get(IsoFields.WEEK_OF_WEEK_BASED_YEAR);
      case WEEKOFMONTH:
        return dt.get(ChronoField.ALIGNED_WEEK_OF_MONTH);
      case MONTH:
        return dt.getMonthValue();
      case QUARTER:
        return dt.get(IsoFields.QUARTER_OF_YEAR);
      case YEAR:
        return dt.getYear();
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
    }

    throw new ExpressionException("Invalid date part: " + this);
  }
}
