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

import java.io.Serializable;
import java.time.Duration;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@SuppressWarnings("serial")
public class YearToMonth extends Interval implements Serializable, Comparable<YearToMonth> {

  private static final int MAX_YEARS = Integer.MAX_VALUE / 12;
  private static final int MAX_MONTHS = 12 * MAX_YEARS;

  private static final Pattern PATTERN_SQL = Pattern.compile("^([+-])?(\\d+)-(\\d+)$");
  private static final Pattern PATTERN_ISO =
      Pattern.compile("^([+-])?P(?:([+-]?\\d+)Y)?(?:([+-]?\\d+)M)?$", Pattern.CASE_INSENSITIVE);

  private final boolean negative;
  private final int years;
  private final int months;

  /**
   * Create a new year-month interval.
   */
  public YearToMonth() {
    this(0, 0, false);
  }

  /**
   * Creates an {@link Interval} consisting of the given number of years.
   *
   * @param years
   *        number of years
   */
  public YearToMonth(int years) {
    this(years, 0, false);
  }

  /**
   * Creates an {@link Interval} consisting of the given number of years and months.
   *
   * @param years
   *        number of years
   * @param months
   *        number of months
   */
  public YearToMonth(int years, int months) {
    this(years, months, false);
  }

  YearToMonth(int years, int months, boolean negative) {

    // Validate
    validate(years, -MAX_YEARS, MAX_YEARS, "years");
    validate(months, -MAX_MONTHS, MAX_MONTHS, "months");

    // Perform normalization
    if (Math.abs(months) >= 12) {
      years += (months / 12);
      months %= 12;
    }

    this.negative = negative;
    this.years = years;
    this.months = months;
  }

  /**
   * Parse a string representation of a <code>INTERVAL YEAR TO MONTH</code>.
   *
   * @param string A string representation of the form
   *        <code>[+|-][years]-[months]</code>
   * @return The parsed <code>YEAR TO MONTH</code> object, or
   *         <code>null</code> if the string could not be parsed.
   */
  public static YearToMonth valueOf(String string) {
    if (string != null) {
      Matcher matcher;

      if ((matcher = PATTERN_SQL.matcher(string)).find())
        return parse(matcher, 0);

      if ((matcher = PATTERN_ISO.matcher(string)).find()) {
        boolean negative = "-".equals(matcher.group(1));

        String group2 = matcher.group(2);
        String group3 = matcher.group(3);

        int years = group2 == null ? 0 : Integer.parseInt(group2);
        int months = group3 == null ? 0 : Integer.parseInt(group3);

        return new YearToMonth(years, months, negative);
      }

      return yearToMonth(string);
    }

    return null;
  }

  /**
   * Parse a standard SQL string representation of a
   * <code>INTERVAL YEAR</code>.
   *
   * @param string A string representation of the form
   *        <code>[+|-][years]</code>
   * @return The parsed <code>YEAR</code> object, or <code>null</code> if the
   *         string could not be parsed.
   */
  public static YearToMonth year(String string) {
    try {
      return string == null ? null : new YearToMonth(Integer.parseInt(string));
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
  public static YearToMonth yearToMonth(String string) {
    if (string != null) {
      Matcher matcher;

      if ((matcher = PATTERN_SQL.matcher(string)).find())
        return parse(matcher, 0);
    }

    return null;
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
  public static YearToMonth month(String string) {
    try {
      return string == null ? null : new YearToMonth(0, Integer.parseInt(string));
    } catch (NumberFormatException ignore) {
      return null;
    }
  }


  public final int getTotalMonths() {
    return (negative ? -1 : 1) * (12 * years + months);
  }

  public final Duration toDuration() {
    long hours = years * 8766L // 365.25 * 24
        + months * 720L; // 30 * 24

    if (negative)
      hours = -hours;

    return Duration.ofHours(hours);
  }

  // -------------------------------------------------------------------------
  // Interval API
  // -------------------------------------------------------------------------
  @Override
  public final YearToMonth negated() {
    return new YearToMonth(years, months, !negative);
  }

  @Override
  public final YearToMonth abs() {
    return new YearToMonth(years, months, false);
  }

  /**
   * Returns years value, if any.
   *
   * @return years, or 0
   */
  public final int getYears() {
    return years;
  }

  /**
   * Returns months value, if any.
   *
   * @return months, or 0
   */
  public final int getMonths() {
    return months;
  }

  @Override
  public final int getSign() {
    return negative ? -1 : 1;
  }

  // -------------------------------------------------------------------------
  // Comparable and Object API
  // -------------------------------------------------------------------------

  @Override
  public final int compareTo(YearToMonth other) {
    if (years < other.years) {
      return -1;
    }
    if (years > other.years) {
      return 1;
    }
    if (months < other.months) {
      return -1;
    }
    if (months > other.months) {
      return 1;
    }

    return 0;
  }

  @Override
  public final int hashCode() {
    return years + Integer.rotateLeft(months, 8);
  }

  @Override
  public final boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() == obj.getClass()) {
      YearToMonth other = (YearToMonth) obj;
      if (months != other.months)
        return false;
      if (years != other.years)
        return false;
      if (negative != other.negative && getTotalMonths() != 0)
        return false;
      return true;
    }
    return false;
  }

  protected static YearToMonth parse(Matcher matcher, int groupOffset) {
    boolean negative = "-".equals(matcher.group(groupOffset + 1));
    int years = parseIntZero(matcher.group(groupOffset + 2));
    int months = parseIntZero(matcher.group(groupOffset + 3));

    return new YearToMonth(years, months, negative);
  }

  @Override
  public final String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append(negative ? '-':'+');
    sb.append(years);
    sb.append('-');
    sb.append(months);
    return sb.toString();
  }

  @Override
  public ZonedDateTime addTo(ZonedDateTime temporal) {
    if (months == 0) {
      if (years != 0) {
        return temporal.plus(years, ChronoUnit.YEARS);
      }
    } else {
      long totalMonths = getTotalMonths();
      if (totalMonths != 0) {
        return temporal.plus(totalMonths, ChronoUnit.MONTHS);
      }
    }

    return temporal;
  }

  @Override
  public ZonedDateTime subtractTo(ZonedDateTime temporal) {
    if (months == 0) {
      if (years != 0) {
        return temporal.minus(years, ChronoUnit.YEARS);
      }
    } else {
      long totalMonths = getTotalMonths();
      if (totalMonths != 0) {
        return temporal.minus(totalMonths, ChronoUnit.MONTHS);
      }
    }

    return temporal;
  }

}
