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

/**
 * A date part can be used with expression functions such as extract(). It describes a
 * part of a date / datetime value
 */
public enum DatePart {
  /** The epoch. The number of seconds since 1970-01-01 00:00:00.00 */
  EPOCH,

  /** The millennium. The year 2000 is in the 2nd millennium, the year 2001 in the 3rd.*/
  MILLENNIUM,

  /** The century. The year 2000 is in the 20th century, the year 2001 in the 21st. */
  CENTURY,

  /** First day of its decade. The year divided by 10. */
  DECADE,

  /** The years */
  YEAR("Y","YY","YYYY"),

  /** The years of week ISO. The ISO year starts at the first day (Monday) of week 01 */
  YEAR_ISO("YOWISO","YEAROFWEEKISO"),

  /** The number (1 - 12) of the month */
  MONTH("MM"),

  /** The number (1 - 31) of the day */
  DAY("D","DD","DAYOFMONTH"),

  /** A number (1 = Sunday, 2 = Monday, …, 7 = Saturday) indicating the day of the week */
  DAYOFWEEK("DOW"),

  /**
   * A number (1 = Monday, …, 7 = Sunday) indicating the day of the week following the ISO 8601
   * standard
   */
  DAYOFWEEK_ISO("DOWISO","ISODOW"),

  /** A number (1 - 366) indicating the day of the year */
  DAYOFYEAR("DOY"),

  /** 
   * The number (1 - 54) of the week of the year.
   * Weeks begin with Sunday, and dates prior to the first Sunday of the year are in week 0.
   */
  WEEK("WW","WOY"),

  /** 
   * The number (1 - 53) of the week of the year ISO 8601.
   * The first week of the ISO year is the week that contains January 4. 
   */
  WEEK_ISO("WEEKOFYEARISO"),

  /** Week from the beginning of the month (0-5) */
  WEEKOFMONTH,

  /** The quarter. Jan-Mar = 1, Apr-Jun = 2, Jul-Sep = 3, Oct-Dec = 4. */
  QUARTER("Q","QQ"),

  /** The hour (0-23). */
  HOUR("HH"),

  /** The minute (0-59). */
  MINUTE("MI"),

  /** The second (0-59). */
  SECOND("SS"),

  /** The millisecond. */
  MILLISECOND("MS"),

  /** The microsecond. */
  MICROSECOND("MCS"),

  /** The nanosecond. */
  NANOSECOND("NS"),

  /** The time zone */
  TIMEZONE,

  /** The time zone offset's hour part. */
  TIMEZONE_HOUR("TZH"),

  /** The time zone offset's minute part. */
  TIMEZONE_MINUTE("TZM")
  
  ;

  private final String[] alias;

  private DatePart() {
   this.alias = new String[0];
  }

  private DatePart(final String... alias) {
    this.alias = alias;
  }

  /**
   * Create date part for the given name or throws exception if not exist.
   *
   * <p>
   * This method ignore case and search alias too
   *
   * @param name
   * @return DatePart
   */
  public static DatePart of(final String name) {
    for (DatePart part : DatePart.values()) {
      if (part.name().equalsIgnoreCase(name)) {
        return part;
      }

      for (String alias : part.alias) {
        if (alias.equalsIgnoreCase(name)) {
          return part;
        }
      }
    }

    throw new IllegalArgumentException("Invalid date part: " + name);
  }

  /**
   * Check if date part exist.
   * 
   * @param name the name to check
   * @return
   */
  public static boolean exist(final String name) {
    for (DatePart part : DatePart.values()) {
      if (part.name().equalsIgnoreCase(name)) {
        return true;
      }
      for (String alias : part.alias) {
        if (alias.equalsIgnoreCase(name)) {
          return true;
        }
      }
    }
    return false;
  }
}
