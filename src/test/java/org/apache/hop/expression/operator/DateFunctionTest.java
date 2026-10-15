/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression.operator;

import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.TimeZone;
import org.apache.hop.expression.Attribute;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionTest;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.IntervalType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.parallel.Execution;

@TestInstance(PER_CLASS)
@Execution(CONCURRENT)
class DateFunctionTest extends ExpressionTest {
  @Test
  void Current_Date() throws Exception {
    IExpressionContext context = createExpressionContext();
    ZonedDateTime today = Attribute.CURRENT_DATE.getDate(context);
    evalEquals(context, "Today()", today).returnType(DateType.DATE_NOT_NULL);
    evalEquals(context, "Current_Date()", today);

    // Check operands
    evalFails("Today(Null)", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void Current_Timestamp() throws Exception {
    IExpressionContext context = createExpressionContext();
    ZonedDateTime today = Attribute.CURRENT_TIMESTAMP.getDate(context);
    evalEquals(context, "Now()", today).returnType(DateType.DATE_NOT_NULL);
    evalEquals(context, "Current_Timestamp()", today);

    // Check operands
    evalFails("Now(Null)", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void Current_TimeZone() throws Exception {
    TimeZone.setDefault(TimeZone.getTimeZone("Europe/Paris"));
    evalEquals("Current_Timezone()", "Europe/Paris").returnType(StringType.of(12, false));
    TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
    evalEquals("Current_Timezone()", "UTC").returnType(StringType.of(3, false));

    // Check operands
    evalFails("Current_Timezone(Null)", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void Year() throws Exception {
    evalEquals("Year(DATE '2019-01-01')", 2019L).returnType(IntegerType.INTEGER_NOT_NULL);

    // Null handling
    evalNull("Year(NULL_DATE)").returnType(IntegerType.INTEGER);
    evalNull("Year(NULL_TIMESTAMP)");

    // Check operands
    evalFails("Year()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Year(12)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void MonthName() throws Exception {
    evalEquals("MonthName(TIMESTAMP '2019-12-28 10:00:00')", "December");
    evalEquals("MonthName(DATE '2019-01-01')", "January").returnType(StringType.STRING_NOT_NULL);
    evalEquals("MonthName(DATE '2019-12-28')", "December");

    // Null handling
    evalNull("MonthName(NULL_DATE)");

    // Check operands
    evalFails("MonthName()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("MonthName(12)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void DayName() throws Exception {
    evalEquals("DayName(TIMESTAMP '2019-01-01 15:28:59')", "Tuesday");
    evalEquals("DayName(DATE '2019-01-01')", "Tuesday").returnType(StringType.STRING_NOT_NULL);
    evalEquals("DayName(DATE '2019-12-28')", "Saturday");

    // Null handling
    evalNull("DayName(NULL_DATE)");

    // Check operands
    evalFails("DayName()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("DayName(12)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void Month() throws Exception {
    evalEquals("Month(DATE '2019-01-01')", 1L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Month(DATE '2020-02-23')", 2L);
    evalEquals("Month(DATE '2019-12-28')", 12L);

    // Null handling
    evalNull("Month(NULL_DATE)").returnType(IntegerType.INTEGER);
    evalNull("Month(NULL_TIMESTAMP)");

    // Check operands
    evalFails("Month()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Month(12)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void Date_Diff() throws Exception {
    evalEquals("Date_Diff(MILLENNIUM, DATE '1001-01-01',DATE '3150-01-01')", 2L)
        .returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Date_Diff(CENTURY, DATE '1001-01-01',DATE '2000-01-01')", 9L);
    evalEquals("Date_Diff(DECADE, DATE '1001-01-01',DATE '2000-01-01')", 99L);
    evalEquals("Date_Diff(YEAR, TIMESTAMP '2001-01-01 12:00:00',DATE '2000-01-01')", -1L);
    evalEquals("Date_Diff(YEAR, DATE '2022-12-31',DATE '2024-06-01')", 1L);
    evalEquals("Date_Diff(QUARTER, DATE '2022-12-31',DATE '2024-06-01')", 5L);
    evalEquals("Date_Diff(MONTH, TIMESTAMP '2001-01-01 12:00:00',DATE '2000-01-01')", -12L);
    evalEquals("Date_Diff(WEEK, TIMESTAMP '2001-01-01 12:00:00',DATE '2000-01-01')", -52L);
    evalEquals("Date_Diff(DAY, DATE '2021-11-09',DATE '2020-12-28')", -316L);
    evalEquals(
        "Date_Diff(HOUR, TIMESTAMP '2019-01-01 15:00:59',TIMESTAMP '2019-01-02 15:00:59')", 24L);
    evalEquals(
        "Date_Diff(MINUTE, TIMESTAMP '2019-01-01 15:00:59',TIMESTAMP '2019-01-02 15:00:59')",
        1440L);
    evalEquals(
        "Date_Diff(SECOND, TIMESTAMP '2019-01-01 15:00:59',TIMESTAMP '2019-01-02 15:00:59')",
        86400L);
    evalEquals(
        "Date_Diff(MILLISECOND, TIMESTAMP '2019-01-01 15:00:59',TIMESTAMP '2019-01-01 15:02:00')",
        61000L);
    evalEquals(
        "Date_Diff(MICROSECOND, TIMESTAMP '2019-01-01 15:00:00.000000',TIMESTAMP '2019-01-01 15:00:00.001234')",
        1234L);
    evalEquals(
        "Date_Diff(NANOSECOND, TIMESTAMP '2019-01-01 15:00:00.000000000',TIMESTAMP '2019-01-01 15:00:00.123456789')",
        123456789L);

    // With leap year
    evalEquals("Date_Diff(DAY,DATE '2015-12-15',DATE '2019-04-23')", 1225L);

    // Null handling
    evalNull("Date_Diff(YEAR, NULL_DATE, DATE '2007-11-09')").returnType(IntegerType.INTEGER);
    evalNull("Date_Diff(YEAR, DATE '2007-11-09',NULL_DATE)");
    evalNull("Date_Diff(YEAR, NULL_DATE, NULL_DATE)");

    // Check operands
    evalFails("Date_Diff(YEAR, DATE '2007-11-09')", ErrorCode.NOT_ENOUGH_ARGUMENTS);
  }

  @Test
  void Years_Between() throws Exception {
    // Only complete years are counted
    evalEquals("Years_Between(DATE '2000-01-01', DATE '2001-01-01')", 1L);
    evalEquals("Years_Between(DATE '2022-12-31', DATE '2024-06-01')", 1L);
    evalEquals(
        "Years_Between(TIMESTAMP '2001-01-01 12:00:00',TIMESTAMP '2000-01-01 00:00:00')", -1L);

    // Null handling
    evalNull("Years_Between(NULL_DATE, DATE '2007-11-09')");
    evalNull("Years_Between(DATE '2007-11-09',NULL_DATE)");
    evalNull("Years_Between(NULL_DATE, NULL_DATE)");

    // Check operands
    evalFails("Years_Between(DATE '2007-11-09')", ErrorCode.NOT_ENOUGH_ARGUMENTS);
  }

  @Test
  void Weeks_Between() throws Exception {
    // Only complete weeks are counted
    evalEquals("Weeks_Between(DATE '2019-01-01', DATE '2019-01-14')", 1L);
    evalEquals("Weeks_Between(DATE '2019-01-01', DATE '2019-01-15')", 2L);
    evalEquals(
            "Weeks_Between(TIMESTAMP '2001-01-01 12:00:00',TIMESTAMP '2000-01-01 00:00:00')", -52L)
        .returnType(IntegerType.INTEGER_NOT_NULL);
    evalNull("Weeks_Between(NULL_DATE, DATE '2007-11-09')");
    evalNull("Weeks_Between(DATE '2007-11-09',NULL_DATE)");
    evalNull("Weeks_Between(NULL_DATE, NULL_DATE)");

    // Check operands
    evalFails("Weeks_Between(DATE '2007-11-09')", ErrorCode.NOT_ENOUGH_ARGUMENTS);
  }

  @Test
  void Months_Between() throws Exception {
    evalEquals("Months_Between(DATE '2005-01-01',DATE '2005-02-02')", 1.032258064516129)
        .returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Months_Between(DATE '2007-11-09',DATE '2003-12-28')", -45.54838709677419);

    // The time difference is ignored because the day of the month is the same for both values.
    // evalEquals("Months_Between(TIMESTAMP '2007-12-13-09.40.30',TIMESTAMP '2007-11-13-08.40.30')",
    // 1.0);

    // evalEquals("Months_Between(DATE '2007-11-10',DATE '2007-12-09')", -0.967742);
    // TODO: If the months and days are identical, the result is an integer.
    evalEquals("Months_Between(DATE '2007-11-09',DATE '2007-12-09')", 0.967741935483871);

    // Null handling
    evalNull("Months_Between(DATE '2007-11-09',NULL_DATE)");
    evalNull("Months_Between(NULL_DATE, DATE '2007-11-09')");
    evalNull("Months_Between(NULL_DATE, NULL_DATE)");

    // Check operands
    evalFails("Months_Between(DATE '2007-11-09')", ErrorCode.NOT_ENOUGH_ARGUMENTS);
  }

  @Test
  void Days_Between() throws Exception {
    // Only complete days are counted
    evalEquals(
        "Days_Between(TIMESTAMP '2019-01-01 15:00:00', TIMESTAMP '2019-01-02 14:59:59')", 0L);
    evalEquals(
        "Days_Between(TIMESTAMP '2019-01-01 15:00:00', TIMESTAMP '2019-01-02 15:00:00')", 1L);
    evalEquals("Days_Between(DATE '2021-01-01',DATE '2021-01-01')", 0L);
    evalEquals("Days_Between(DATE '2021-11-09',DATE '2020-12-28')", -316L);
    evalEquals("Days_Between(DATE '2007-11-09',DATE '2007-12-09')", 30L);

    // With leap year
    evalEquals("Days_Between(DATE '2015-12-15',DATE '2019-04-23')", 1225L);

    // Null handling
    evalNull("Days_Between(DATE '2007-11-09',NULL_DATE)");
    evalNull("Days_Between(NULL_DATE, Date '2007-11-09')");
    evalNull("Days_Between(NULL_DATE, NULL_DATE)");

    // Check operands
    evalFails("Days_Between(DATE '2007-11-09')", ErrorCode.NOT_ENOUGH_ARGUMENTS);
  }

  @Test
  void Hours_Between() throws Exception {
    evalEquals(
        "Hours_Between(TIMESTAMP '2019-01-02 15:00:59', TIMESTAMP '2019-01-01 15:00:59')", -24L);
    evalEquals(
        "Hours_Between(TIMESTAMP '2019-01-01 15:00:59',TIMESTAMP '2019-01-01 15:28:59')", 0L);
    evalEquals(
        "Hours_Between(TIMESTAMP '2019-01-01 15:00:59',TIMESTAMP '2019-01-02 15:00:59')", 24L);

    // Null handling
    evalNull("Hours_Between(NULL_TIMESTAMP, TIMESTAMP '2019-01-01 15:00:59')");
    evalNull("Hours_Between(TIMESTAMP '2019-01-01 15:00:59', NULL_TIMESTAMP)");

    // Check operands
    evalFails("Hours_Between(DATE '2007-11-09')", ErrorCode.NOT_ENOUGH_ARGUMENTS);
  }

  @Test
  void Minutes_Between() throws Exception {
    // Only complete minutes are counted
    evalEquals(
        "Minutes_Between(TIMESTAMP '2019-01-01 15:00:59', TIMESTAMP '2019-01-01 15:28:58')", 27L);
    evalEquals(
        "Minutes_Between(TIMESTAMP '2019-01-01 15:28:59', TIMESTAMP '2019-01-01 15:00:59')", -28L);
    evalEquals(
        "Minutes_Between(TIMESTAMP '2019-01-01 15:00:59',TIMESTAMP '2019-01-01 15:28:59')", 28L);
    evalEquals(
        "Minutes_Between(TIMESTAMP '2019-01-01 15:00:59',TIMESTAMP '2019-01-02 15:00:59')", 1440L);

    // Null handling
    evalNull("Minutes_Between(NULL_DATE, TIMESTAMP '2019-01-01 15:00:59')");
    evalNull("Minutes_Between(TIMESTAMP '2019-01-01 15:00:59', NULL_DATE)");

    // Check operands
    evalFails("Minutes_Between(DATE '2007-11-09')", ErrorCode.NOT_ENOUGH_ARGUMENTS);
  }

  @Test
  void Seconds_Between() throws Exception {
    evalEquals(
        "Seconds_Between(TIMESTAMP '2019-01-02 15:00:59', TIMESTAMP '2019-01-01 15:00:59')",
        -86400L);
    evalEquals(
        "Seconds_Between(TIMESTAMP '2019-01-01 15:00:59',TIMESTAMP '2019-01-01 15:28:59')",
        28L * 60L);
    evalEquals(
        "Seconds_Between(TIMESTAMP '2019-01-01 15:00:59',TIMESTAMP '2019-01-02 15:00:59')", 86400L);

    // Null handling
    evalNull("Seconds_Between(NULL_DATE, TIMESTAMP '2019-01-01 15:00:59')");
    evalNull("Seconds_Between(TIMESTAMP '2019-01-01 15:00:59', NULL_DATE)");

    // Check operands
    evalFails("Seconds_Between(DATE '2007-11-09')", ErrorCode.NOT_ENOUGH_ARGUMENTS);
  }

  @Test
  void Quarter() throws Exception {
    evalEquals("Quarter(DATE '2019-01-01')", 1L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Quarter(DATE '2019-02-28')", 1L);
    evalEquals("Quarter(DATE '2019-04-28')", 2L);
    evalEquals("Quarter(DATE '2019-08-28')", 3L);
    evalEquals("Quarter(DATE '2019-12-28')", 4L);

    // Null handling
    evalNull("Quarter(NULL_DATE)").returnType(IntegerType.INTEGER);
    evalNull("Quarter(NULL_TIMESTAMP)");

    // Check operands
    evalFails("Quarter()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Quarter(FIELD_STRING)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Quarter(FIELD_INTEGER)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Quarter(FIELD_NUMBER)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void DayOfWeek() throws Exception {
    evalEquals("DayOfWeek(DATE '2019-01-01')", 3L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("DayOfWeek(DATE '2019-07-27')", 7L);
    evalEquals("DayOfWeek(DATE '2019-07-28')", 1L);
    evalEquals("DayOfWeek(DATE '2019-12-31')", 3L);
    evalEquals("DayOfWeek(FIELD_DATE)", 3L);
    evalEquals("DayOfWeek(FIELD_TIMESTAMP)", 3L);

    // Null handling
    evalNull("DayOfWeek(NULL_DATE)").returnType(IntegerType.INTEGER);

    // Check operands
    evalFails("DayOfWeek()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("DayOfWeek(FIELD_INTEGER)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("DayOfWeek(FIELD_NUMBER)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void Day() throws Exception {
    evalEquals("Day(DATE '2019-01-01')", 1L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Day(DATE '2019-02-28')", 28L);
    evalEquals("Day(DATE '2019-12-28')", 28L);
    evalEquals("Day(FIELD_DATE)", 23L);
    evalEquals("Day(FIELD_TIMESTAMP)", 28L);

    // Null handling
    evalNull("Day(NULL_DATE)").returnType(IntegerType.INTEGER);
    evalNull("Day(NULL_TIMESTAMP)");

    // Check operands
    evalFails("Day()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Day(123)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Day('text')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);

    optimize("DAY(DATE '2019-02-15')", "15");
    optimize("DAY(MAKE_DATE(2019,2,15))", "15");
  }

  @Test
  void DayOfYear() throws Exception {
    evalEquals("DayOfYear(DATE '2019-01-01')", 1L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("DayOfYear(DATE '2019-12-31')", 365L);
    evalEquals("DayOfYear(FIELD_DATE)", 174L);
    evalEquals("DayOfYear(FIELD_TIMESTAMP)", 59L);

    // Null handling
    evalNull("DayOfYear(NULL_DATE)").returnType(IntegerType.INTEGER);

    // Check operands
    evalFails("DayOfYear()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("DayOfYear(123)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void Julian_Day() throws Exception {
    evalEquals("Julian_Day(DATE '2021-06-23')", 2459389L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Julian_Day(TIMESTAMP  '2021-06-23 8:00:00' at time zone 'UTC+12')", 2459389L);

    // Null handling
    evalNull("Julian_Day(NULL_DATE)").returnType(IntegerType.INTEGER);

    // Check operands
    evalFails("Julian_Day()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Julian_Day(123)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void Week() throws Exception {
    evalEquals("Week(DATE '2015-12-31')", 53L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Week(DATE '2015-01-01')", 1L);
    evalEquals("Week(DATE '2015-01-02')", 1L);

    // Null handling
    evalNull("Week(NULL_DATE)").returnType(IntegerType.INTEGER);

    // Check operands
    evalFails("Week()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Week(123)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void IsoDayOfWeek() throws Exception {
    evalEquals("IsoDayOfWeek(DATE '2003-12-29')", 1L);
    evalEquals("IsoDayOfWeek(TIMESTAMP '2003-12-28 10:00:00')", 7L);
    evalEquals("IsoDayOfWeek(DATE '2003-12-28')", 7L).returnType(IntegerType.INTEGER_NOT_NULL);

    // Null handling
    evalNull("IsoDayOfWeek(NULL_DATE)").returnType(IntegerType.INTEGER);

    // Check operands
    evalFails("IsoDayOfWeek()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("IsoDayOfWeek(123)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void IsoWeek() throws Exception {
    evalEquals("IsoWeek(TIMESTAMP '2016-01-04 10:00:00')", 1L);
    evalEquals("IsoWeek(DATE '2015-12-31')", 53L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("IsoWeek(DATE '2016-01-01')", 53L);
    evalEquals("IsoWeek(DATE '2016-01-02')", 53L);
    evalEquals("IsoWeek(DATE '2016-01-03')", 53L);
    evalEquals("IsoWeek(DATE '2016-01-04')", 1L);

    // Null handling
    evalNull("IsoWeek(NULL_DATE)").returnType(IntegerType.INTEGER);

    // Check operands
    evalFails("IsoWeek()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("IsoWeek(123)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void IsoYear() throws Exception {
    evalEquals("IsoYear(DATE '2015-12-31')", 2015L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("IsoYear(DATE '2016-01-01')", 2015L);
    evalEquals("IsoYear(DATE '2016-01-02')", 2015L);
    evalEquals("IsoYear(DATE '2016-01-04')", 2016L);
    evalEquals("IsoYear(DATE '2042-12-31')", 2043L);

    // Null handling
    evalNull("IsoYear(NULL_DATE)").returnType(IntegerType.INTEGER);

    // Check operands
    evalFails("IsoYear('ERROR')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("IsoYear()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("IsoYear(123)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void Add_Years() throws Exception {
    // The decimal part is truncated
    evalEquals("Add_Years(DATE '2019-01-15', 1.9)", LocalDate.of(2020, Month.JANUARY, 15));
    evalEquals("Add_Years(DATE '2019-01-15', -1.9)", LocalDate.of(2018, Month.JANUARY, 15));
    evalEquals(
        "Add_Years(TIMESTAMP '2019-01-15 10:20:30', 1)",
        LocalDateTime.of(2020, Month.JANUARY, 15, 10, 20, 30));
    evalEquals("Add_Years(DATE '2019-01-15',1)", LocalDate.of(2020, Month.JANUARY, 15));
    evalEquals("Add_Years(DATE '2019-01-15',-2)", LocalDate.of(2017, Month.JANUARY, 15));
    evalEquals("Add_Years(DATE '2019-11-15',3)", LocalDate.of(2022, Month.NOVEMBER, 15));
    // the resulting month has fewer days
    evalEquals("Add_Years(DATE '2020-02-29',1)", LocalDate.of(2021, Month.FEBRUARY, 28));

    // Null handling
    evalNull("Add_Years(NULL_DATE,140)");
    evalNull("Add_Years(DATE '2019-01-15',NULL_INTEGER)");

    // Check operands
    evalFails("Add_Years()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Add_Years(DATE '2019-01-15')", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Simplify
    optimize("Add_Years(FIELD_DATE,0)", "FIELD_DATE");
  }

  @Test
  void Add_Quarters() throws Exception {
    evalEquals("Add_Quarters(DATE '2019-01-15', 1.7)", LocalDate.of(2019, Month.APRIL, 15));
    evalEquals("Add_Quarters(DATE '2019-11-30', 1)", LocalDate.of(2020, Month.FEBRUARY, 29));
    evalEquals("Add_Quarters(DATE '2019-01-15',1)", LocalDate.of(2019, Month.APRIL, 15));
    evalEquals("Add_Quarters(DATE '2019-01-15',-2)", LocalDate.of(2018, Month.JULY, 15));
    evalEquals("Add_Quarters(DATE '2019-11-15',3)", LocalDate.of(2020, Month.AUGUST, 15));

    // the resulting month has fewer days
    evalEquals("Add_Quarters(DATE '2019-01-31',1)", LocalDate.of(2019, Month.APRIL, 30));

    // Null handling
    evalNull("Add_Quarters(NULL_DATE,140)");
    evalNull("Add_Quarters(DATE '2019-01-15',NULL_INTEGER)");

    // Check operands
    evalFails("Add_Quarters(DATE '2019-01-15')", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Simplify
    optimize("Add_Quarters(FIELD_DATE,0)", "FIELD_DATE");
  }

  @Test
  void Add_Months() throws Exception {
    evalEquals("Add_Months(DATE '2019-01-15', 1.9)", LocalDate.of(2019, Month.FEBRUARY, 15));
    evalEquals("Add_Months(DATE '2020-01-31', 1)", LocalDate.of(2020, Month.FEBRUARY, 29));
    // Otherwise the day of month is kept
    evalEquals("Add_Months(DATE '2019-02-28', 1)", LocalDate.of(2019, Month.MARCH, 28));
    evalEquals("Add_Months(DATE '2019-01-15',1)", LocalDate.of(2019, Month.FEBRUARY, 15));
    evalEquals("Add_Months(DATE '2019-01-15',-2)", LocalDate.of(2018, Month.NOVEMBER, 15));
    evalEquals("Add_Months(DATE '2019-11-15',3)", LocalDate.of(2020, Month.FEBRUARY, 15));

    // the resulting month has fewer days
    evalEquals("Add_Months(DATE '2019-01-31',1)", LocalDate.of(2019, Month.FEBRUARY, 28));

    // Null handling
    evalNull("Add_Months(NULL_DATE,140)");
    evalNull("Add_Months(DATE '2019-01-15',NULL_INTEGER)");

    // Check operands
    evalFails("Add_Months(DATE '2019-01-15')", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Simplify
    optimize("Add_Months(FIELD_DATE,0)", "FIELD_DATE");
  }

  @Test
  void Add_Weeks() throws Exception {
    evalEquals("Add_Weeks(DATE '2019-01-15', 1.9)", LocalDate.of(2019, Month.JANUARY, 22));
    evalEquals("Add_Weeks(DATE '2019-01-15',1)", LocalDate.of(2019, Month.JANUARY, 22));
    evalEquals("Add_Weeks(DATE '2019-01-15',-3)", LocalDate.of(2018, Month.DECEMBER, 25));

    // Null handling
    evalNull("Add_Weeks(NULL_DATE,140)");
    evalNull("Add_Weeks(DATE '2019-01-15',NULL_INTEGER)");

    // Check operands
    evalFails("Add_Weeks(DATE '2019-01-15')", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Simplify
    optimize("Add_Weeks(FIELD_DATE,0)", "FIELD_DATE");
  }

  @Test
  void Add_Days() throws Exception {
    evalEquals("Add_Days(DATE '2019-01-15', 1.9)", LocalDate.of(2019, Month.JANUARY, 16));
    evalEquals("Add_Days(DATE '2019-01-15', -1.9)", LocalDate.of(2019, Month.JANUARY, 14));
    evalEquals("Add_Days(DATE '2022-05-24', -4)", LocalDate.of(2022, Month.MAY, 20));
    evalEquals("DATE '2022-05-24' - 4", LocalDate.of(2022, Month.MAY, 20));
    evalEquals("Add_Days(DATE '2019-01-15',1)", LocalDate.of(2019, Month.JANUARY, 16));
    evalEquals("Add_Days(DATE '2019-01-15',-20)", LocalDate.of(2018, Month.DECEMBER, 26));
    evalEquals(
        "Add_Days(TIMESTAMP '2021-01-01 15:28:59+02:00',20)",
        ZonedDateTime.of(2021, 1, 21, 15, 28, 59, 0, ZoneOffset.ofHoursMinutes(2, 0)));

    // Null handling
    evalNull("Add_Days(NULL_DATE,140)");
    evalNull("Add_Days(DATE '2019-01-15',NULL_INTEGER)");

    // Check operands
    evalFails("Add_Days(DATE '2019-01-15')", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Simplify
    optimize("Add_Days(FIELD_DATE,0)", "FIELD_DATE");
  }

  @Test
  void Add_Hours() throws Exception {
    evalEquals(
        "Add_Hours(TIMESTAMP '2024-02-18 22:58:59', 4)",
        LocalDateTime.of(2024, Month.FEBRUARY, 19, 2, 58, 59));
    evalEquals(
        "Add_Hours(TIMESTAMP '2024-02-19 02:58:59', -4)",
        LocalDateTime.of(2024, Month.FEBRUARY, 18, 22, 58, 59));
    evalEquals(
        "Add_Hours(DATE '2019-01-15', 1.9)", LocalDateTime.of(2019, Month.JANUARY, 15, 1, 0, 0));
    evalEquals(
        "Add_Hours(DATE '2019-01-15',1)", LocalDateTime.of(2019, Month.JANUARY, 15, 1, 0, 0, 0));
    evalEquals(
        "Add_Hours(TIMESTAMP '2021-01-01 15:28:59+02:00',2)",
        ZonedDateTime.of(2021, 1, 1, 17, 28, 59, 0, ZoneOffset.ofHoursMinutes(2, 0)));

    // Null handling
    evalNull("Add_Hours(NULL_DATE,140)");
    evalNull("Add_Hours(DATE '2019-01-15',NULL_INTEGER)");

    // Check operands
    evalFails("Add_Hours(DATE '2019-01-15')", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Simplify
    optimize("Add_Hours(FIELD_DATE,0)", "FIELD_DATE");
  }

  @Test
  void Add_Minutes() throws Exception {
    evalEquals(
        "Add_Minutes(TIMESTAMP '2024-02-18 22:58:59', 4)",
        LocalDateTime.of(2024, Month.FEBRUARY, 18, 23, 2, 59));
    evalEquals(
        "Add_Minutes(DATE '2019-01-15', -20)",
        LocalDateTime.of(2019, Month.JANUARY, 14, 23, 40, 0));
    evalEquals(
        "Add_Minutes(DATE '2019-01-15', 20.9)",
        LocalDateTime.of(2019, Month.JANUARY, 15, 0, 20, 0));
    evalEquals(
        "Add_Minutes(DATE '2019-01-15',20)",
        LocalDateTime.of(2019, Month.JANUARY, 15, 0, 20, 0, 0));

    // Null handling
    evalNull("Add_Minutes(NULL_DATE,140)");
    evalNull("Add_Minutes(DATE '2019-01-15',NULL_INTEGER)");

    // Check operands
    evalFails("Add_Minutes(DATE '2019-01-15')", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Simplify
    optimize("Add_Minutes(FIELD_DATE,0)", "FIELD_DATE");
  }

  @Test
  void Add_Seconds() throws Exception {
    evalEquals(
        "Add_Seconds(DATE '2019-01-15', -20)",
        LocalDateTime.of(2019, Month.JANUARY, 14, 23, 59, 40));
    evalEquals(
        "Add_Seconds(DATE '2019-01-15', 20.9)",
        LocalDateTime.of(2019, Month.JANUARY, 15, 0, 0, 20));
    evalEquals(
        "Add_Seconds(DATE '2019-01-15',20)", LocalDateTime.of(2019, Month.JANUARY, 15, 0, 0, 20));
    evalEquals(
        "Add_Seconds(DATE '2019-01-15',140)", LocalDateTime.of(2019, Month.JANUARY, 15, 0, 2, 20));

    // Null handling
    evalNull("Add_Seconds(NULL_DATE,140)");
    evalNull("Add_Seconds(DATE '2019-01-15',NULL_INTEGER)");

    // Check operands
    evalFails("Add_Seconds(DATE '2019-01-15')", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Simplify
    optimize("Add_Seconds(FIELD_DATE,0)", "FIELD_DATE");
  }

  @Test
  void Add_Nanoseconds() throws Exception {
    evalEquals(
        "Add_Nanoseconds(DATE '2019-01-15', -1)",
        LocalDateTime.of(2019, Month.JANUARY, 14, 23, 59, 59, 999999999));
    evalEquals(
        "Add_Nanoseconds(DATE '2019-01-15', 20.9)",
        LocalDateTime.of(2019, Month.JANUARY, 15, 0, 0, 0, 20));
    evalEquals(
        "Add_NanoSeconds(DATE '2019-01-15',20)",
        LocalDateTime.of(2019, Month.JANUARY, 15, 0, 0, 0, 20));
    evalEquals(
        "Add_NanoSeconds(DATE '2019-01-15',140)",
        LocalDateTime.of(2019, Month.JANUARY, 15, 0, 0, 0, 140));

    // Null handling
    evalNull("Add_NanoSeconds(NULL_DATE,140)");
    evalNull("Add_NanoSeconds(DATE '2019-01-15',NULL_INTEGER)");

    // Check operands
    evalFails("Add_NanoSeconds(DATE '2019-01-15')", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Simplify
    optimize("Add_NanoSeconds(FIELD_DATE,0)", "FIELD_DATE");
  }

  @Test
  void Date_Add() throws Exception {
    evalEquals("DATE_ADD(DAY, 5, DATE '2023-02-01')", LocalDate.of(2023, Month.FEBRUARY, 6));
    evalEquals(
        "DATE_ADD(HOUR, 4, TIMESTAMP '2024-02-18 22:58:59')",
        LocalDateTime.of(2024, Month.FEBRUARY, 19, 2, 58, 59));
    evalNull("DATE_ADD(DAY, NULL_INTEGER, DATE '2019-01-15')");
    evalNull("DATE_ADD(DAY, 1, NULL_DATE)");
    // Any other unit raises an error
    evalFails("DATE_ADD(MILLISECOND, 1, DATE '2019-01-15')", ErrorCode.INVALID_ARGUMENT);
    evalEquals("DATE_ADD(YEAR,1,DATE '2020-02-29')", LocalDate.of(2021, Month.FEBRUARY, 28));
    evalEquals("DATE_ADD(QUARTER,1,DATE '2019-11-15')", LocalDate.of(2020, Month.FEBRUARY, 15));
    evalEquals("DATE_ADD(MONTH,3,DATE '2019-11-15')", LocalDate.of(2020, Month.FEBRUARY, 15));
    evalEquals("DATE_ADD(WEEK,-3,DATE '2019-01-15')", LocalDate.of(2018, Month.DECEMBER, 25));
    evalEquals("DATE_ADD(DAY,-20,DATE '2019-01-15')", LocalDate.of(2018, Month.DECEMBER, 26));
    evalEquals(
        "DATE_ADD(HOUR,1,DATE '2019-01-15')",
        LocalDateTime.of(2019, Month.JANUARY, 15, 1, 0, 0, 0));
    evalEquals(
        "DATE_ADD(MINUTE,20,DATE '2019-01-15')",
        LocalDateTime.of(2019, Month.JANUARY, 15, 0, 20, 0, 0));
    evalEquals(
        "DATE_ADD(SECOND,140,DATE '2019-01-15')",
        LocalDateTime.of(2019, Month.JANUARY, 15, 0, 2, 20, 0));
    evalEquals(
        "DATE_ADD(NANOSECOND,23,DATE '2019-01-15')",
        LocalDateTime.of(2019, Month.JANUARY, 15, 0, 0, 0, 23));

    // Simplify
    optimize("DATE_ADD(YEAR,1,FIELD_DATE)", "ADD_YEARS(FIELD_DATE,1)");
    optimize("DATE_ADD(QUARTER,1,FIELD_DATE)", "ADD_QUARTERS(FIELD_DATE,1)");
    optimize("DATE_ADD(MONTH,1,FIELD_DATE)", "ADD_MONTHS(FIELD_DATE,1)");
    optimize("DATE_ADD(HOUR,1,FIELD_DATE)", "ADD_HOURS(FIELD_DATE,1)");
    optimize("DATE_ADD(MINUTE,1,FIELD_DATE)", "ADD_MINUTES(FIELD_DATE,1)");
    optimize("DATE_ADD(SECOND,1,FIELD_DATE)", "ADD_SECONDS(FIELD_DATE,1)");

    // Check operands
    evalFails("DATE_ADD(NULL,1,DATE '2019-01-15')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("DATE_ADD('YEAR',1,DATE '2019-01-15')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void Hour() throws Exception {
    // A date without time returns 0
    evalEquals("Hour(DATE '2019-01-01')", 0L);
    evalEquals("Hour(TIMESTAMP '2019-01-01 15:28:59')", 15L);
    evalEquals("HOUR(FIELD_TIMESTAMP)", 22L);

    // Null handling
    evalNull("Hour(NULL_DATE)");
    evalNull("Hour(NULL_TIMESTAMP)");

    // Check operands
    evalFails("Hour()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
  }

  @Test
  void Minute() throws Exception {
    // A date without time returns 0
    evalEquals("Minute(DATE '2019-01-01')", 0L);
    evalEquals("Minute(TIMESTAMP '2019-01-01 15:28:59')", 28L);
    evalEquals("MINUTE(FIELD_TIMESTAMP)", 11L);

    // Null handling
    evalNull("Minute(NULL_DATE)");
    evalNull("Minute(NULL_TIMESTAMP)");

    // Check operands
    evalFails("Minute()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
  }

  @Test
  void Second() throws Exception {
    // A date without time returns 0
    evalEquals("Second(DATE '2019-01-01')", 0L);
    evalEquals("Second(TIMESTAMP '2019-01-01 15:28:59')", 59L);
    evalEquals("SECOND(FIELD_TIMESTAMP)", 1L);

    // Null handling
    evalNull("Second(NULL_DATE)");
    evalNull("Second(NULL_TIMESTAMP)");

    // Check operands
    evalFails("Second()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
  }

  @Test
  void Date_Trunc() throws Exception {
    // A week starts on Monday
    evalEquals("Date_Trunc(WEEK, DATE '2020-05-31')", LocalDate.of(2020, Month.MAY, 25));
    evalEquals("Date_Trunc(WEEK, DATE '2020-05-25')", LocalDate.of(2020, Month.MAY, 25));
    evalEquals("Date_Trunc(MILLENNIUM, DATE '2020-05-08')", LocalDate.of(2001, Month.JANUARY, 1));
    evalEquals("Date_Trunc(CENTURY, DATE '2020-05-08')", LocalDate.of(2001, Month.JANUARY, 1));
    // As with EXTRACT, the 21st century and the 3rd millennium start in 2001
    evalEquals("Date_Trunc(CENTURY, DATE '2000-12-31')", LocalDate.of(1901, Month.JANUARY, 1));
    evalEquals("Date_Trunc(CENTURY, DATE '2001-01-01')", LocalDate.of(2001, Month.JANUARY, 1));
    evalEquals("Date_Trunc(CENTURY, DATE '2100-06-01')", LocalDate.of(2001, Month.JANUARY, 1));
    evalEquals("Date_Trunc(MILLENNIUM, DATE '2000-12-31')", LocalDate.of(1001, Month.JANUARY, 1));
    evalEquals("Date_Trunc(MILLENNIUM, DATE '2001-01-01')", LocalDate.of(2001, Month.JANUARY, 1));
    evalEquals("Date_Trunc(CENTURY, DATE '0001-06-01')", LocalDate.of(1, Month.JANUARY, 1));
    evalEquals("Date_Trunc(DECADE, DATE '2021-05-08')", LocalDate.of(2020, Month.JANUARY, 1));
    evalEquals("Date_Trunc(YEAR, DATE '2020-05-08')", LocalDate.of(2020, Month.JANUARY, 1));
    evalEquals("Date_Trunc(MONTH, DATE '2020-05-08')", LocalDate.of(2020, Month.MAY, 1));
    evalEquals("Date_Trunc(DAY, DATE '2020-05-25')", LocalDate.of(2020, Month.MAY, 25));
    evalEquals("Date_Trunc(DAYOFMONTH, DATE '2020-05-25')", LocalDate.of(2020, Month.MAY, 25));
    evalEquals("Date_Trunc(QUARTER, DATE '2020-05-25')", LocalDate.of(2020, Month.APRIL, 1));
    // First and last month of each quarter
    evalEquals("Date_Trunc(QUARTER, DATE '2020-01-15')", LocalDate.of(2020, Month.JANUARY, 1));
    evalEquals("Date_Trunc(QUARTER, DATE '2020-03-31')", LocalDate.of(2020, Month.JANUARY, 1));
    evalEquals("Date_Trunc(QUARTER, DATE '2020-04-01')", LocalDate.of(2020, Month.APRIL, 1));
    evalEquals("Date_Trunc(QUARTER, DATE '2020-06-30')", LocalDate.of(2020, Month.APRIL, 1));
    evalEquals("Date_Trunc(QUARTER, DATE '2020-07-01')", LocalDate.of(2020, Month.JULY, 1));
    evalEquals("Date_Trunc(QUARTER, DATE '2020-09-30')", LocalDate.of(2020, Month.JULY, 1));
    evalEquals("Date_Trunc(QUARTER, DATE '2020-10-01')", LocalDate.of(2020, Month.OCTOBER, 1));
    evalEquals("Date_Trunc(QUARTER, DATE '2020-12-31')", LocalDate.of(2020, Month.OCTOBER, 1));
    evalEquals("Date_Trunc(WEEK, DATE '2020-05-28')", LocalDate.of(2020, Month.MAY, 25));
    evalEquals("Date_Trunc(WEEK, DATE '2021-01-01')", LocalDate.of(2020, Month.DECEMBER, 28));
    evalEquals("Date_Trunc(WEEKOFYEAR, DATE '2020-05-28')", LocalDate.of(2020, Month.MAY, 25));

    // Null handling
    evalNull("Date_Trunc(DAY, NULL_TIMESTAMP)");
    evalNull("Date_Trunc(DAY, NULL_DATE)");

    evalFails("Date_Trunc(DAY)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Date_Trunc(DATE '2020-05-25')", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Date_Trunc(ISOWEEK, DATE '2020-05-25')", ErrorCode.INVALID_ARGUMENT);
    evalFails("Date_Trunc(NULL_DATE, DATE '2020-05-25')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Date_Trunc(123, DATE '2020-05-25')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Date_Trunc('xxx', DATE '2020-05-25')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);

    // Truncate timestamp
    evalEquals(
        "Date_Trunc(DAY, TIMESTAMP '2020-05-25 23:59:59')", LocalDate.of(2020, Month.MAY, 25));
    // Units of a day or more reset the time to midnight
    evalEquals(
        "Date_Trunc(MILLENNIUM, TIMESTAMP '2020-05-28 23:59:59')",
        LocalDateTime.of(2001, Month.JANUARY, 1, 0, 0, 0, 0));
    evalEquals(
        "Date_Trunc(CENTURY, TIMESTAMP '2020-05-28 23:59:59')",
        LocalDateTime.of(2001, Month.JANUARY, 1, 0, 0, 0, 0));
    evalEquals(
        "Date_Trunc(DECADE, TIMESTAMP '2021-05-28 23:59:59')",
        LocalDateTime.of(2020, Month.JANUARY, 1, 0, 0, 0, 0));
    evalEquals(
        "Date_Trunc(YEAR, TIMESTAMP '2020-05-28 23:59:59')",
        LocalDateTime.of(2020, Month.JANUARY, 1, 0, 0, 0, 0));
    evalEquals(
        "Date_Trunc(QUARTER, TIMESTAMP '2020-05-28 23:59:59')",
        LocalDateTime.of(2020, Month.APRIL, 1, 0, 0, 0, 0));
    evalEquals(
        "Date_Trunc(MONTH, TIMESTAMP '2020-05-28 23:59:59')",
        LocalDateTime.of(2020, Month.MAY, 1, 0, 0, 0, 0));
    evalEquals(
        "Date_Trunc(WEEK, TIMESTAMP '2020-05-28 23:59:59')",
        LocalDateTime.of(2020, Month.MAY, 25, 0, 0, 0, 0));
    evalEquals(
        "Date_Trunc(DAY, TIMESTAMP '2020-05-28 23:59:59.123456789')",
        LocalDateTime.of(2020, Month.MAY, 28, 0, 0, 0, 0));
    evalEquals(
        "Date_Trunc(HOUR, TIMESTAMP '2020-05-25 23:59:59')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 0, 0, 0));
    evalEquals(
        "Date_Trunc(MINUTE, TIMESTAMP '2020-05-25 23:59:59')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 59, 0, 0));
    evalEquals(
        "Date_Trunc(SECOND, TIMESTAMP '2020-05-25 23:59:59')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 59, 59, 0));
    evalEquals(
        "Date_Trunc(MILLISECOND, TIMESTAMP '2020-05-25 23:59:59.123456789')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 59, 59, 123000000));
    evalEquals(
        "Date_Trunc(MICROSECOND, TIMESTAMP '2020-05-25 23:59:59.123456789')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 59, 59, 123456000));
    evalEquals(
        "Date_Trunc(NANOSECOND, TIMESTAMP '2020-05-25 23:59:59.123456789')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 59, 59, 123456789));
  }

  @Test
  void Extract() throws Exception {
    // Synonyms
    evalEquals("Extract(DAYOFMONTH FROM DATE '2020-05-25')", 25L);
    evalEquals("Extract(DAYS FROM DATE '2020-05-25')", 25L);
    evalEquals("Extract(WEEKS FROM DATE '2020-05-25')", 21L);
    evalEquals("Extract(WEEKOFYEAR FROM DATE '2020-05-25')", 21L);
    evalEquals("Extract(ISOWEEKOFYEAR FROM DATE '2016-01-03')", 53L);
    evalEquals("Extract(MILLENNIUMS FROM DATE '2020-05-25')", 3L);
    evalEquals("Extract(YEARS FROM DATE '2020-05-25')", 2020L);
    evalEquals("Extract(DAYOFYEAR FROM DATE '2020-12-31')", 366L);
    evalEquals("Extract(WEEKOFMONTH FROM DATE '2020-05-31')", 5L);
    evalEquals("Extract(NANOSECOND FROM TIMESTAMP '2020-05-25 00:00:01.123456789')", 123456789L);
    // The fractional seconds are not returned
    evalEquals("Extract(SECOND FROM TIMESTAMP '2020-05-25 00:00:01.5')", 1L);
    evalEquals("Extract(TIMEZONE_MINUTE FROM TIMESTAMP '2021-01-01 15:28:59 -04:30')", -30L);
    evalEquals("Date_Part(YEAR, INTERVAL 10 YEAR)", 10L);
    evalNull("Extract(YEAR FROM NULL_TIMESTAMP)");
    // Extract part from temporal
    evalEquals("Extract(MILLENNIUM from TIMESTAMP '2020-05-25 23:48:59')", 3L)
        .returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Extract(CENTURY from TIMESTAMP '2000-12-25 23:48:59')", 20L);
    evalEquals("Extract(CENTURY from TIMESTAMP '2020-05-25 23:48:59')", 21L);
    evalEquals("Extract(CENTURY from Date '0001-01-01')", 1L);
    evalEquals("Extract(DECADE from TIMESTAMP '1999-02-16 20:38:40')", 199L);
    evalEquals("Extract(EPOCH from TIMESTAMP '1970-01-01 00:00:00')", 0L);
    evalEquals("Extract(EPOCH from TIMESTAMP '1970-01-02 00:00:00')", 86400L);
    evalEquals("Extract(YEAR from TIMESTAMP '2020-05-25 23:48:59')", 2020L);
    evalEquals("Extract(ISOYEAR from Date '2017-01-01')", 2016L);
    evalEquals("Extract(QUARTER from TIMESTAMP '2020-05-25 23:48:59')", 2L);
    evalEquals("Extract(MONTH from TIMESTAMP '2020-05-25 23:48:59')", 5L);
    evalEquals("Extract(WEEK from TIMESTAMP '2020-05-25 23:48:59')", 21L);
    evalEquals("Extract(WEEK from TIMESTAMP '2020-01-01 23:48:59')", 1L);
    evalEquals("Extract(WEEKOFMONTH from Date '2011-03-15')", 3L);
    evalEquals("Extract(ISOWEEK from Date '2016-01-03')", 53L);
    evalEquals("Extract(ISOWEEK from Date '2016-01-04')", 1L);
    evalEquals("Extract(ISODAYOFWEEK from Date '2003-12-28')", 7L);
    evalEquals("Extract(DAY from TIMESTAMP '2020-05-25 23:48:59')", 25L);
    evalEquals("Extract(DAYOFWEEK from TIMESTAMP '2020-05-24 23:48:59')", 1L);
    evalEquals("Extract(DAYOFWEEK from TIMESTAMP '2020-05-25 23:48:59')", 2L);
    evalEquals("Extract(DAYOFYEAR from TIMESTAMP '2020-05-25 23:48:59')", 146L);
    evalEquals("Extract(HOUR from TIMESTAMP '2020-05-25 23:48:59')", 23L);
    evalEquals("Extract(MINUTE from TIMESTAMP '2020-05-25 23:48:59')", 48L);
    evalEquals("Extract(SECOND from TIMESTAMP '2020-05-25 23:48:59')", 59L);
    evalEquals("Extract(MILLISECOND from TIMESTAMP '2020-05-25 00:00:01.123456')", 123L);
    evalEquals("Extract(MICROSECOND from TIMESTAMP '2020-05-25 00:00:01.123456')", 123456L);
    evalEquals("Extract(NANOSECOND from TIMESTAMP '2020-05-25 00:00:01.123456')", 123456000L);
    evalEquals("Extract(TIMEZONE_HOUR from TIMESTAMP '2021-01-01 15:28:59')", 0L);
    evalEquals(
        "Extract(TIMEZONE_HOUR from TIMESTAMP '2021-01-01 15:28:59' AT TIME ZONE 'Europe/Paris')",
        1L);
    evalEquals(
        "Extract(TIMEZONE_HOUR from TIMESTAMP '2021-01-01 15:28:59' AT TIME ZONE 'Asia/Manila')",
        8L);
    evalEquals("Extract(TIMEZONE_HOUR from TIMESTAMP '2021-01-01 15:28:59 +02:00')", 2L);
    evalEquals("Extract(TIMEZONE_HOUR from TIMESTAMP '2021-01-01 15:28:59 -04:00')", -4L);
    evalEquals("Extract(TIMEZONE_MINUTE from TIMESTAMP '2021-01-01 15:28:59 +01:28')", 28L);
    evalEquals("Extract(TIMEZONE from TIMESTAMP '2021-01-01 15:28:59 +01:28')", 5280L);
    evalEquals(
        "Extract(TIMEZONE_MINUTE from TIMESTAMP '2021-01-01 15:28:59' AT TIME ZONE 'Asia/Tokyo')",
        0L);

    // Null handling
    evalNull("Extract(SECOND from NULL_DATE)").returnType(IntegerType.INTEGER);

    // Alias
    evalEquals("Date_Part(HOUR,TIMESTAMP '2020-05-25 23:48:59')", 23L)
        .returnType(IntegerType.INTEGER_NOT_NULL);

    // Extract part from an interval
    evalEquals("Extract(MILLENNIUM from INTERVAL 1234 YEAR)", 1L)
        .returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Extract(CENTURY from INTERVAL 1234 YEAR)", 12L);
    evalEquals("Extract(DECADE from INTERVAL 1234 YEAR)", 123L);
    evalEquals("Extract(YEAR from INTERVAL 10 YEAR)", 10L);
    evalEquals("Extract(YEAR from -INTERVAL 10 YEAR)", -10L);
    evalEquals("Extract(YEAR from INTERVAL '-10' YEAR)", -10L);
    evalEquals("Extract(YEAR from INTERVAL '0-25' YEAR TO MONTH)", 2L);
    evalEquals("Extract(YEAR from INTERVAL 30 MINUTES)", 0L);
    evalEquals("Extract(MONTH from INTERVAL 30 MONTHS)", 6L);
    evalEquals("Extract(DAY from INTERVAL 30 MONTHS)", 0L);
    evalEquals("Extract(DAY from INTERVAL -45 DAYS)", -45L);
    evalEquals("Extract(HOUR from INTERVAL 30 HOURS)", 6L);
    evalEquals("Extract(MINUTE from INTERVAL 30 MINUTES)", 30L);
    evalEquals("Extract(SECOND from INTERVAL 59 SECONDS)", 59L);
    evalEquals("Extract(MILLISECOND from INTERVAL '14:38:56.987654321' HOUR TO SECOND)", 987L);
    evalEquals("Extract(MICROSECOND from INTERVAL '14:38:56.987654321' HOUR TO SECOND)", 987654L);
    evalEquals("Extract(NANOSECOND from INTERVAL '14:38:56.987654321' HOUR TO SECOND)", 987654321L);

    // TODO: detect unsupported time unit at compile time
    evalFails("Extract(EPOCH from INTERVAL -45 DAYS)", ErrorCode.INVALID_ARGUMENT);
    evalFails("Extract(ISOYEAR from INTERVAL -45 DAYS)", ErrorCode.INVALID_ARGUMENT);
    evalFails("Extract(ISOWEEK from INTERVAL -45 DAYS)", ErrorCode.INVALID_ARGUMENT);
    evalFails("Extract(ISODAYOFWEEK from INTERVAL -45 DAYS)", ErrorCode.INVALID_ARGUMENT);
    evalFails("Extract(WEEKOFMONTH from INTERVAL -45 DAYS)", ErrorCode.INVALID_ARGUMENT);
    evalFails("Extract(DAYOFWEEK from INTERVAL -45 DAYS)", ErrorCode.INVALID_ARGUMENT);
    evalFails("Extract(DAYOFYEAR from INTERVAL -45 DAYS)", ErrorCode.INVALID_ARGUMENT);

    evalFails("Extract(123 from DATE '2021-01-01')", ErrorCode.INVALID_TIME_UNIT);
    evalFails("Extract('TEST' from DATE '2021-01-01')", ErrorCode.INVALID_TIME_UNIT);
    evalFails("Extract(NULL from DATE '2021-01-01')", ErrorCode.INVALID_TIME_UNIT);
    evalFails("Extract(BIDON from NULL)", ErrorCode.INVALID_TIME_UNIT);
    evalFails("Extract(BIDON from DATE '2021-01-01')", ErrorCode.INVALID_TIME_UNIT);

    // Check operands
    evalFails("Extract()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Extract(MONTH)", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Check syntax
    evalFails("Extract(", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("Extract(MONTH DATE '2023-12-01')", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("Extract(DAY DATE '2021-01-01')", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("Extract(MONTH FROM DATE '2023-12-01'", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    // Replace EXTRACT with the corresponding function
    optimize("EXTRACT(CENTURY FROM FIELD_DATE)");
    optimize("EXTRACT(EPOCH FROM FIELD_DATE)", "EPOCH(FIELD_DATE)");
    optimize("EXTRACT(YEAR FROM FIELD_DATE)", "YEAR(FIELD_DATE)");
    optimize("EXTRACT(ISOYEAR FROM FIELD_DATE)", "ISOYEAR(FIELD_DATE)");
    optimize("EXTRACT(MONTH FROM FIELD_DATE)", "MONTH(FIELD_DATE)");
    optimize("EXTRACT(QUARTER FROM FIELD_DATE)", "QUARTER(FIELD_DATE)");
    optimize("EXTRACT(DAY FROM FIELD_DATE)", "DAY(FIELD_DATE)");
    optimize("EXTRACT(HOUR FROM FIELD_DATE)", "HOUR(FIELD_DATE)");
    optimize("EXTRACT(MINUTE FROM FIELD_DATE)", "MINUTE(FIELD_DATE)");
    optimize("EXTRACT(SECOND FROM FIELD_DATE)", "SECOND(FIELD_DATE)");
    optimize("EXTRACT(WEEK FROM FIELD_DATE)", "WEEK(FIELD_DATE)");
    optimize("EXTRACT(ISOWEEK FROM FIELD_DATE)", "ISOWEEK(FIELD_DATE)");
    optimize("EXTRACT(DAYOFYEAR FROM FIELD_DATE)", "DAYOFYEAR(FIELD_DATE)");
    optimize("EXTRACT(DAYOFWEEK FROM FIELD_DATE)", "DAYOFWEEK(FIELD_DATE)");
    optimize("EXTRACT(ISODAYOFWEEK FROM FIELD_DATE)", "ISODAYOFWEEK(FIELD_DATE)");
  }

  @Test
  void ConvertTimeZone() throws Exception {
    // Documentation examples
    evalEquals(
        "CONVERT_TIMEZONE('Europe/Paris', TIMESTAMP '2020-05-25 20:48:00')",
        LocalDateTime.of(2020, Month.MAY, 25, 22, 48, 0));
    evalEquals(
        "CONVERT_TIMEZONE('Europe/Paris', 'Asia/Tokyo', TIMESTAMP '2020-05-25 20:48:00')",
        LocalDateTime.of(2020, Month.MAY, 26, 3, 48, 0));
    // Time zone names are case-sensitive
    evalFails(
        "CONVERT_TIMEZONE('europe/paris', TIMESTAMP '2020-05-25 20:48:00')",
        ErrorCode.INVALID_TIMEZONE);
    evalFails(
        "CONVERT_TIMEZONE('Europe/Pariss', TIMESTAMP '2020-05-25 20:48:00')",
        ErrorCode.INVALID_TIMEZONE);
    evalEquals(
            "CONVERT_TIMEZONE('America/Los_Angeles', 'America/New_York', TIMESTAMP '2023-01-01 14:00:00')",
            LocalDateTime.of(2023, Month.JANUARY, 1, 17, 0, 0))
        .returnType(DateType.DATE_NOT_NULL);
    evalEquals(
        "CONVERT_TIMEZONE('America/Los_Angeles', TIMESTAMP '2023-01-01 14:00:00 +02:00')",
        LocalDateTime.of(2023, Month.JANUARY, 1, 4, 0, 0));
    evalEquals(
        "CONVERT_TIMEZONE('Europe/Paris',TIMESTAMP '2020-05-25 20:48:00')",
        ZonedDateTime.of(2020, 5, 25, 22, 48, 0, 0, ZoneId.of("Europe/Paris")));
    evalEquals(
        "CONVERT_TIMEZONE('Asia/Tokyo', TIMESTAMP '2023-01-01 14:00:00')",
        LocalDateTime.of(2023, Month.JANUARY, 1, 23, 0, 0));
    evalEquals(
        "CONVERT_TIMEZONE('+00:00','+10:00', TIMESTAMP '2023-01-01 12:00:00')",
        ZonedDateTime.of(2023, 1, 1, 22, 0, 0, 0, ZoneOffset.ofHoursMinutes(10, 0)));

    // Null handling
    evalNull("CONVERT_TIMEZONE('Europe/Paris', NULL_TIMESTAMP)").returnType(DateType.DATE);
    evalNull("CONVERT_TIMEZONE('Europe/Paris', 'America/New_York', NULL_TIMESTAMP)");

    // Check operands
    evalFails("CONVERT_TIMEZONE(Null, '2023-01-01 14:00:00')", ErrorCode.INVALID_TIMEZONE);

    // evalEquals("CONVERT_TIMEZONE('Asia/Singapore',TIMESTAMP '2020-05-25 20:48:00' AT TIME ZONE
    // 'UTC')", ZonedDateTime.of(2020, 5, 26, 18,48,00,0,ZoneId.of("Asia/Singapore")));

    optimize("CONVERT_TIMEZONE('Europe/Paris',FIELD_TIMESTAMP)");
    optimize(
        "CONVERT_TIMEZONE('Europe/Paris',TIMESTAMP '2020-05-25 20:48:00')",
        "TIMESTAMP '2020-05-25 22:48:00' AT TIME ZONE 'Europe/Paris'");
  }

  @Test
  void MakeDate() throws Exception {
    // Values out of the normal ranges
    evalEquals("MAKE_DATE(2020,13,1)", LocalDate.of(2021, Month.JANUARY, 1));
    evalEquals("MAKE_DATE(2020,12,32)", LocalDate.of(2021, Month.JANUARY, 1));
    evalEquals("MAKE_DATE(2020,13,0)", LocalDate.of(2020, Month.DECEMBER, 31));
    // A day beyond the end of the month overflows to the next month
    evalEquals("MAKE_DATE(2020,2,30)", LocalDate.of(2020, Month.MARCH, 1));
    evalEquals("MAKE_DATE(2019,2,29)", LocalDate.of(2019, Month.MARCH, 1));
    evalEquals("MAKE_DATE(2020,4,31)", LocalDate.of(2020, Month.MAY, 1));
    evalEquals("MAKE_DATE(2020,14,31)", LocalDate.of(2021, Month.MARCH, 3));
    evalEquals("MAKE_DATE(2019,01,1)", LocalDate.of(2019, Month.JANUARY, 1))
        .returnType(DateType.DATE_NOT_NULL);
    evalEquals("MAKE_DATE(2020,02,27)", LocalDate.of(2020, Month.FEBRUARY, 27));
    evalEquals("MAKE_DATE(2020,19,1)", LocalDate.of(2021, Month.JULY, 1));
    evalEquals("MAKE_DATE(2020, 0, 1)", LocalDate.of(2019, Month.DECEMBER, 1));
    evalEquals("MAKE_DATE(2020,-1,1)", LocalDate.of(2019, Month.NOVEMBER, 1));
    evalEquals("MAKE_DATE(2020,-2, 1)", LocalDate.of(2019, Month.OCTOBER, 1));
    evalEquals("MAKE_DATE(2020,-6,1)", LocalDate.of(2019, Month.JUNE, 1));
    evalEquals("MAKE_DATE(2020, 6, 50)", LocalDate.of(2020, Month.JULY, 20));
    evalEquals("MAKE_DATE(2020, 2, 0)", LocalDate.of(2020, Month.JANUARY, 31));
    evalEquals("MAKE_DATE(2020, 2, -1)", LocalDate.of(2020, Month.JANUARY, 30));

    // Null handling
    evalNull("MAKE_DATE(NULL_INTEGER,-1,1)").returnType(DateType.DATE);
    evalNull("MAKE_DATE(2020,NULL_INTEGER,1)");
    evalNull("MAKE_DATE(2020,-1,NULL_INTEGER)");

    // Check operands
    evalFails("MAKE_DATE()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("MAKE_DATE(2020)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("MAKE_DATE(2020,15)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("MAKE_DATE(2020,1,1,1)", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void MakeTimestamp() throws Exception {
    // Values out of the normal ranges
    evalEquals(
        "MAKE_TIMESTAMP(2020,1,1,24,0,0)", LocalDateTime.of(2020, Month.JANUARY, 2, 0, 0, 0));
    evalEquals(
        "MAKE_TIMESTAMP(2020,1,1,-1,0,0)", LocalDateTime.of(2019, Month.DECEMBER, 31, 23, 0, 0));
    evalEquals(
        "MAKE_TIMESTAMP(2020,1,1,23,60,0)", LocalDateTime.of(2020, Month.JANUARY, 2, 0, 0, 0));
    evalEquals(
        "MAKE_TIMESTAMP(2020,1,1,23,59,60)", LocalDateTime.of(2020, Month.JANUARY, 2, 0, 0, 0));
    evalEquals(
        "MAKE_TIMESTAMP(2020,1,1,0,0,-1)", LocalDateTime.of(2019, Month.DECEMBER, 31, 23, 59, 59));
    evalEquals(
        "MAKE_TIMESTAMP(2020,3,0,0,0,0)", LocalDateTime.of(2020, Month.FEBRUARY, 29, 0, 0, 0));
    evalEquals(
        "MAKE_TIMESTAMP(2020,13,1,0,0,0)", LocalDateTime.of(2021, Month.JANUARY, 1, 0, 0, 0));
    // A day beyond the end of the month overflows to the next month
    evalEquals(
        "MAKE_TIMESTAMP(2020,2,30,10,0,0)", LocalDateTime.of(2020, Month.MARCH, 1, 10, 0, 0));
    evalEquals("MAKE_TIMESTAMP(2020,4,31,10,0,0)", LocalDateTime.of(2020, Month.MAY, 1, 10, 0, 0));
    // A negative fractional second is subtracted
    evalEquals(
        "MAKE_TIMESTAMP(2020,1,1,0,0,-0.5)",
        LocalDateTime.of(2019, Month.DECEMBER, 31, 23, 59, 59, 500000000));
    evalEquals(
        "MAKE_TIMESTAMP(2020,1,1,0,0,-1.25)",
        LocalDateTime.of(2019, Month.DECEMBER, 31, 23, 59, 58, 750000000));
    evalEquals(
        "MAKE_TIMESTAMP(2020,2,30,0,0,-0.5)",
        LocalDateTime.of(2020, Month.FEBRUARY, 29, 23, 59, 59, 500000000));
    evalNull("MAKE_TIMESTAMP(2020,1,1,NULL_INTEGER,15,59)");
    evalEquals(
            "MAKE_TIMESTAMP(2019,01,1,23,15,59)",
            LocalDateTime.of(2019, Month.JANUARY, 1, 23, 15, 59))
        .returnType(DateType.DATE_NOT_NULL);
    evalEquals(
        "MAKE_TIMESTAMP(2020,-6,1,23,15,59)", LocalDateTime.of(2019, Month.JUNE, 1, 23, 15, 59));
    evalEquals(
        "MAKE_TIMESTAMP(2020,0,1,23,15,59)", LocalDateTime.of(2019, Month.DECEMBER, 1, 23, 15, 59));
    evalEquals(
        "MAKE_TIMESTAMP(2020,-1,1,23,15,59)",
        LocalDateTime.of(2019, Month.NOVEMBER, 1, 23, 15, 59));
    evalEquals(
        "MAKE_TIMESTAMP(2020,6,50,23,15,59)", LocalDateTime.of(2020, Month.JULY, 20, 23, 15, 59));
    evalEquals(
        "MAKE_TIMESTAMP(2020,6,50,23,15,59.123)",
        LocalDateTime.of(2020, Month.JULY, 20, 23, 15, 59, 123000000));
    evalEquals(
        "MAKE_TIMESTAMP(2020,6,50,23,15,59.123456)",
        LocalDateTime.of(2020, Month.JULY, 20, 23, 15, 59, 123456000));
    evalEquals(
        "MAKE_TIMESTAMP(2020,6,50,23,15,59.123456789)",
        LocalDateTime.of(2020, Month.JULY, 20, 23, 15, 59, 123456789));

    // Null handling
    evalNull("MAKE_TIMESTAMP(NULL_INTEGER,-1,1,23,15,59)").returnType(DateType.DATE);
    evalNull("MAKE_TIMESTAMP(2020,NULL_INTEGER,1,23,15,59)");
    evalNull("MAKE_TIMESTAMP(2020,-1,NULL_INTEGER,23,15,59)");

    // Check operands
    evalFails("MAKE_TIMESTAMP()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("MAKE_TIMESTAMP(2020)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("MAKE_TIMESTAMP(2020,15)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("MAKE_TIMESTAMP(2020,1,1,23,15,59.123456789,9999)", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void MakeInterval() throws Exception {
    // An interval can be added to a date
    evalEquals(
        "DATE '2020-01-01' + MAKE_INTERVAL(1,2,3,4,5,6)",
        LocalDateTime.of(2021, Month.MARCH, 4, 4, 5, 6));
    evalNull("MAKE_INTERVAL(NULL_INTEGER,1,1,23,15,59)");
    evalEquals("MAKE_INTERVAL(20,1,1,23,15,59)", Interval.of(20, 1, 1, 23, 15, 59))
        .returnType(IntervalType.INTERVAL_NOT_NULL);
    evalEquals("MAKE_INTERVAL(20,1,1,23,15,59.123)", Interval.of(20, 1, 1, 23, 15, 59, 123000000));

    // Null handling
    evalNull("MAKE_INTERVAL(20,1,NULL_INTEGER,23,15,59)");

    // Check operands
    evalFails("MAKE_INTERVAL()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("MAKE_INTERVAL(20)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("MAKE_INTERVAL(20,15)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("MAKE_INTERVAL(20,1,1,23,15,59.123456789,9999)", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void First_Day() throws Exception {
    evalEquals("First_Day(DATE '2021-06-15', QUARTER)", LocalDate.of(2021, Month.APRIL, 1));
    evalEquals("First_Day(DATE '2019-01-01')", LocalDate.of(2019, Month.JANUARY, 1))
        .returnType(DateType.DATE_NOT_NULL);
    evalEquals("First_Day(DATE '2020-02-27')", LocalDate.of(2020, Month.FEBRUARY, 1));
    evalEquals("First_Day(DATE '2020-02-27', YEAR)", LocalDate.of(2020, Month.JANUARY, 1));
    evalEquals("First_Day(DATE '2020-02-27', MONTH)", LocalDate.of(2020, Month.FEBRUARY, 1));
    evalEquals("First_Day(DATE '2020-02-27', QUARTER)", LocalDate.of(2020, Month.JANUARY, 1));
    evalEquals("First_Day(DATE '2020-05-27', QUARTER)", LocalDate.of(2020, Month.APRIL, 1));
    evalEquals("First_Day(DATE '2020-09-27', QUARTER)", LocalDate.of(2020, Month.JULY, 1));
    evalEquals("First_Day(DATE '2020-12-27', QUARTER)", LocalDate.of(2020, Month.OCTOBER, 1));
    evalEquals("First_Day(DATE '2020-02-01', WEEK)", LocalDate.of(2020, Month.JANUARY, 27));
    evalEquals("First_Day(DATE '2020-02-27', WEEK)", LocalDate.of(2020, Month.FEBRUARY, 24));
    evalEquals("First_Day(DATE '2020-12-31', WEEK)", LocalDate.of(2020, Month.DECEMBER, 28));
    evalEquals("First_Day(FIELD_DATE, YEAR)", LocalDate.of(1981, Month.JANUARY, 1));
    evalEquals("First_Day(FIELD_TIMESTAMP, YEAR)", LocalDate.of(2023, Month.JANUARY, 1));

    // The time of a timestamp is kept
    evalEquals(
        "First_Day(TIMESTAMP '2020-02-27 23:59:12', YEAR)",
        LocalDateTime.of(2020, Month.JANUARY, 1, 23, 59, 12));
    evalEquals(
        "First_Day(TIMESTAMP '2020-02-27 23:59:12', MONTH)",
        LocalDateTime.of(2020, Month.FEBRUARY, 1, 23, 59, 12));

    // Null handling
    evalNull("First_Day(NULL_DATE)").returnType(DateType.DATE);
    evalNull("First_day(NULL_DATE, WEEK)");
    evalNull("First_day(NULL_DATE, MONTH)");
    evalNull("First_day(NULL_DATE, QUARTER)");
    evalNull("First_day(NULL_DATE, YEAR)");

    // Check operands
    evalFails("First_Day()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("First_Day(FIELD_STRING)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("First_Day(FIELD_DATE, 1)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("First_Day(FIELD_DATE, NULL)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("First_Day(FIELD_DATE, HOUR)", ErrorCode.UNSUPPORTED_TIME_UNIT);
  }

  @Test
  void Last_Day() throws Exception {
    evalEquals("Last_Day(DATE '2021-06-15', QUARTER)", LocalDate.of(2021, Month.JUNE, 30));
    evalEquals("Last_Day(DATE '2019-01-01')", LocalDate.of(2019, Month.JANUARY, 31))
        .returnType(DateType.DATE_NOT_NULL);
    evalEquals("Last_Day(DATE '2020-02-27')", LocalDate.of(2020, Month.FEBRUARY, 29));
    evalEquals("Last_Day(DATE '2020-02-27', YEAR)", LocalDate.of(2020, Month.DECEMBER, 31));
    evalEquals("Last_Day(DATE '2022-02-27', MONTH)", LocalDate.of(2022, Month.FEBRUARY, 28));
    evalEquals("Last_Day(DATE '2020-02-27', MONTH)", LocalDate.of(2020, Month.FEBRUARY, 29));
    evalEquals("Last_Day(DATE '2020-02-27', QUARTER)", LocalDate.of(2020, Month.MARCH, 31));
    evalEquals("Last_Day(DATE '2020-04-27', QUARTER)", LocalDate.of(2020, Month.JUNE, 30));
    evalEquals("Last_Day(DATE '2020-07-27', QUARTER)", LocalDate.of(2020, Month.SEPTEMBER, 30));
    evalEquals("Last_Day(DATE '2020-10-27', QUARTER)", LocalDate.of(2020, Month.DECEMBER, 31));
    evalEquals("Last_Day(DATE '2020-12-31', WEEK)", LocalDate.of(2021, Month.JANUARY, 3));
    evalEquals("Last_Day(FIELD_DATE, YEAR)", LocalDate.of(1981, Month.DECEMBER, 31));
    evalEquals("Last_Day(FIELD_TIMESTAMP, YEAR)", LocalDate.of(2023, Month.DECEMBER, 31));

    // The time of a timestamp is kept
    evalEquals(
        "Last_Day(TIMESTAMP '2020-02-27 23:59:12')",
        LocalDateTime.of(2020, Month.FEBRUARY, 29, 23, 59, 12));
    evalEquals(
        "Last_Day(TIMESTAMP '2020-02-27 23:59:12', YEAR)",
        LocalDateTime.of(2020, Month.DECEMBER, 31, 23, 59, 12));

    // Null handling
    evalNull("Last_Day(NULL_DATE)").returnType(DateType.DATE);
    evalNull("Last_Day(NULL_DATE, WEEK)");
    evalNull("Last_Day(NULL_DATE, MONTH)");
    evalNull("Last_Day(NULL_DATE, QUARTER)");
    evalNull("Last_Day(NULL_DATE, YEAR)");

    // Check operands
    evalFails("Last_Day()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Last_Day(FIELD_INTEGER)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Last_Day(FIELD_STRING)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Last_Day(FIELD_DATE, 1)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Last_Day(FIELD_DATE, NULL)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Last_Day(FIELD_DATE, HOUR)", ErrorCode.UNSUPPORTED_TIME_UNIT);
  }

  @Test
  void Next_Day() throws Exception {
    evalEquals("Next_Day(DATE '2021-06-15', 'Friday')", LocalDate.of(2021, Month.JUNE, 18));
    evalEquals("Next_Day(DATE '2020-02-28', 'MONDAY')", LocalDate.of(2020, Month.MARCH, 2));
    // The next day is strictly after the date
    evalEquals("Next_Day(DATE '2020-03-02', 'monday')", LocalDate.of(2020, Month.MARCH, 9));
    evalEquals("Next_Day(DATE '2020-02-28','monday')", LocalDate.of(2020, Month.MARCH, 2))
        .returnType(DateType.DATE_NOT_NULL);
    evalEquals("Next_Day(FIELD_DATE,'monday')", LocalDate.of(1981, Month.JUNE, 29));
    evalEquals("Next_Day(FIELD_TIMESTAMP,'monday')", LocalDate.of(2023, Month.MARCH, 6));
    // The time of a timestamp is kept
    evalEquals(
        "Next_Day(TIMESTAMP '2023-02-28 22:11:01','monday')",
        LocalDateTime.of(2023, Month.MARCH, 6, 22, 11, 1));

    // Null handling
    evalNull("Next_Day(NULL_DATE, 'monday')").returnType(DateType.DATE);
    evalNull("Next_Day(FIELD_DATE, NULL_STRING)");

    // Check operands
    evalFails("Next_Day()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Next_Day(FIELD_DATE)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Next_Day(FIELD_DATE, 'bad')", ErrorCode.INVALID_ARGUMENT);
    evalFails("Next_Day(FIELD_DATE, HOUR)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Next_Day(FIELD_INTEGER, 'monday')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Next_Day(FIELD_STRING, 'monday')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void Previous_Day() throws Exception {
    evalEquals("Previous_Day(DATE '2021-06-15', 'Friday')", LocalDate.of(2021, Month.JUNE, 11));
    evalEquals("Previous_Day(DATE '2020-02-28', 'MONDAY')", LocalDate.of(2020, Month.FEBRUARY, 24));
    // The previous day is strictly before the date
    evalEquals("Previous_Day(DATE '2020-02-24', 'monday')", LocalDate.of(2020, Month.FEBRUARY, 17));
    evalFails("Previous_Day(FIELD_STRING, 'monday')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    // The time of a timestamp is kept
    evalEquals(
        "Previous_Day(TIMESTAMP '2023-02-28 22:11:01','monday')",
        LocalDateTime.of(2023, Month.FEBRUARY, 27, 22, 11, 1));
    evalEquals("Previous_Day(DATE '2020-02-28','monday')", LocalDate.of(2020, Month.FEBRUARY, 24))
        .returnType(DateType.DATE_NOT_NULL);

    // Null handling
    evalNull("Previous_Day(NULL_DATE, 'monday')").returnType(DateType.DATE);
    evalNull("Previous_Day(FIELD_DATE, NULL_STRING)");

    // Check operands
    evalFails("Previous_Day()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Previous_Day(FIELD_DATE)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Previous_Day(FIELD_DATE, 'bad')", ErrorCode.INVALID_ARGUMENT);
    evalFails("Previous_Day(FIELD_DATE, HOUR)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Previous_Day(FIELD_INTEGER, 'monday')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }
}
