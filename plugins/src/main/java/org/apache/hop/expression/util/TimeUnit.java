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
package org.apache.hop.expression.util;

import org.apache.hop.expression.ExpressionError;

/**
 * A time unit can be used with functions such as EXTRACT, FIRST_DAY...
 * It describes a part of a date / datetime value
 */
public enum TimeUnit {
  /** The epoch. The number of seconds since 1970-01-01 00:00:00.00 */
  EPOCH,

  /** The millennium. The year 2000 is in the 2nd millennium, the year 2001 in the 3rd.*/
  MILLENNIUM,

  /** The century. The year 2000 is in the 20th century, the year 2001 in the 21st. */
  CENTURY,

  /** First day of its decade. The year divided by 10. */
  DECADE,

  /** The years */
  YEAR,

  /** The years of week ISO. The ISO year starts at the first day (Monday) of week 01 */
  ISOYEAR,

  /** The number (1 - 12) of the month */
  MONTH,

  /** The number (1 - 31) of the day */
  DAY("DAYOFMONTH"),

  /** A number (1 = Sunday, 2 = Monday, 7 = Saturday) indicating the day of the week */
  DAYOFWEEK,

  /**
   * A number (1 = Monday, 7 = Sunday) indicating the day of the week following the ISO 8601
   * standard
   */
  ISODAYOFWEEK,

  /** A number (1 - 366) indicating the day of the year */
  DAYOFYEAR,

  /** 
   * The number (1 - 54) of the week of the year.
   * Weeks begin with Sunday, and dates prior to the first Sunday of the year are in week 0.
   */
  WEEK("WEEKOFYEAR"),

  /** 
   * The number (1 - 53) of the week of the year ISO 8601.
   * The first week of the ISO year is the week that contains January 4. 
   */
  ISOWEEK("ISOWEEKOFYEAR"),

  /** Week from the beginning of the month (0-5) */
  WEEKOFMONTH,

  /** Quarter. Jan-Mar = 1, Apr-Jun = 2, Jul-Sep = 3, Oct-Dec = 4. */
  QUARTER,

  /** Hour (0-23). */
  HOUR,

  /** Minute (0-59). */
  MINUTE,

  /** Second (0-59). */
  SECOND,

  /** Millisecond. */
  MILLISECOND,

  /** Microsecond. */
  MICROSECOND,

  /** The nanosecond. */
  NANOSECOND,

  /** Time zone region abbreviated */
  TIMEZONE_ABBR,
  
  /** Time zone region */
  TIMEZONE_REGION,

  /** Time zone offset's hour part. */
  TIMEZONE_HOUR,

  /** Time zone offset's minute part. */
  TIMEZONE_MINUTE  
  ;

  private final String[] alias;

  private TimeUnit() {
   this.alias = new String[0];
  }

  private TimeUnit(final String... alias) {
    this.alias = alias;
  }

  /**
   * Create date part for the given name or throws exception if not exist.
   * <p>
   * This method ignore case and search alias too
   *
   * @param name
   * @return DatePart
   */
  public static TimeUnit of(final String name) {
    for (TimeUnit unit : TimeUnit.values()) {
      if (unit.name().equalsIgnoreCase(name)) {
        return unit;
      }

      for (String alias : unit.alias) {
        if (alias.equalsIgnoreCase(name)) {
          return unit;
        }
      }
    }

    throw new IllegalArgumentException(ExpressionError.INVALID_TIMEUNIT.message(name));
  }
  
  /**
   * Check if date part exist.
   * 
   * @param name the name to check
   * @return
   */
  public static boolean exist(final String name) {
    for (TimeUnit unit : TimeUnit.values()) {
      if (unit.name().equalsIgnoreCase(name)) {
        return true;
      }
      for (String alias : unit.alias) {
        if (alias.equalsIgnoreCase(name)) {
          return true;
        }
      }
    }
    return false;
  }
}
