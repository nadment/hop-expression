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

package org.apache.hop.core.expression;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.type.Interval;
import org.apache.hop.expression.type.IntervalQualifier;
import org.junit.Test;


public class IntervalTest extends ExpressionTest {

  @Test
  public void intervalZero() throws Exception {
    Interval interval = new Interval();
    assertEquals(0, interval.getDays());
    assertEquals(0, interval.getHours());
    assertEquals(0, interval.getMinutes());
    assertEquals(0, interval.getSeconds());
    assertEquals(0, interval.getMilliseconds());
    assertEquals(0, interval.getMicroseconds());
    assertEquals(0, interval.getNanoseconds());
    assertEquals(1, interval.getSign());
    assertEquals("+0-0 0 00:00:00.000000000", interval.toString());
    assertTrue(interval.isZero());
  }

  @Test
  public void intervalParseNull() throws Exception {
    assertNull(Interval.year(null));
    assertNull(Interval.yearToMonth(null));
    assertNull(Interval.quarter(null));
    assertNull(Interval.month(null));
    assertNull(Interval.week(null));
    assertNull(Interval.day(null));
    assertNull(Interval.dayToHour(null));
    assertNull(Interval.dayToMinute(null));
    assertNull(Interval.dayToSecond(null));
    assertNull(Interval.hour(null));
    assertNull(Interval.hourToMinute(null));
    assertNull(Interval.hourToSecond(null));
    assertNull(Interval.minute(null));
    assertNull(Interval.minuteToSecond(null));
    assertNull(Interval.second(null));
  }

  @Test
  public void intervalParseFail() throws Exception {
    assertNotEquals(null, Interval.yearToMonth("5-24"));
    assertNull(Interval.year("Z"));
    assertNull(Interval.yearToMonth("5-Z"));
    assertNull(Interval.quarter("Z"));
    assertNull(Interval.month("Z"));
    assertNull(Interval.week("Z"));
    assertNull(Interval.day("Z"));
    assertNull(Interval.hour("Z"));
    assertNull(Interval.minute("Z"));
    assertNull(Interval.second("1123.Z"));

    assertNull(Interval.valueOf("Z"));
    assertNull(Interval.valueOf("3"));
    assertNull(Interval.valueOf(" DAYS"));
  }

  @Test
  public void intervalField() throws Exception {
    Interval interval = Interval.of(5, 25, 44, 22, 30, 58, 123456789);
    assertEquals(7, interval.getYears());
    assertEquals(1, interval.getMonths());
    assertEquals(44, interval.getDays());
    assertEquals(22, interval.getHours());
    assertEquals(30, interval.getMinutes());
    assertEquals(58, interval.getSeconds());
    assertEquals(123, interval.getMilliseconds());
    assertEquals(123456, interval.getMicroseconds());
    assertEquals(123456789, interval.getNanoseconds());
    assertEquals(7, Interval.yearToMonth("7-3").getYears());
    assertEquals(3, Interval.yearToMonth("7-3").getMonths());
    assertEquals(5, Interval.dayToSecond("-5 14:38:56.987654321").getDays());
    assertEquals(14, Interval.dayToSecond("-5 14:38:56.987654321").getHours());
    assertEquals(38, Interval.dayToSecond("-5 14:38:56.987654321").getMinutes());
    assertEquals(56, Interval.dayToSecond("-5 14:38:56.987654321").getSeconds());
    assertEquals(987, Interval.dayToSecond("-5 14:38:56.987654321").getMilliseconds());
    assertEquals(987654, Interval.dayToSecond("-5 14:38:56.987654321").getMicroseconds());
    assertEquals(987654321, Interval.dayToSecond("-5 14:38:56.987654321").getNanoseconds());
  }

  @Test
  public void intervalParse() throws Exception {
    // Short format
    assertEquals(Interval.of(5), Interval.year("5"));
    assertEquals(Interval.of(1), Interval.yearToMonth("+0-12"));
    assertEquals(Interval.of(5, 25), Interval.yearToMonth("6-13"));
    assertNotEquals(Interval.of(5, 25), Interval.yearToMonth("-5-25 "));
    assertNotEquals(Interval.of(5, 25), Interval.yearToMonth("6-25"));
    assertNotEquals(Interval.of(5, 25), Interval.yearToMonth(" 5-24"));
    assertEquals(Interval.of(0, 6), Interval.quarter("2"));
    assertEquals(Interval.of(2, 1), Interval.month("25"));
    assertNotEquals(Interval.of(25), Interval.month("25"));
    assertEquals(Interval.of(0, 0, 5), Interval.day("5"));
    assertEquals(Interval.of(0, 0, 5, 14), Interval.dayToHour("+5 14"));
    assertEquals(Interval.of(0, 0, 5, 14, 38), Interval.dayToMinute("+5 14:38"));
    assertEquals(Interval.of(0, 0, 5, 14, 38, 56), Interval.dayToSecond("5 14:38:56"));
    assertEquals(Interval.of(0, 0, 5, 14, 38, 56, 987654321),
        Interval.dayToSecond("5 14:38:56.987654321"));
    assertEquals(Interval.of(0, 0, 5, 14, 38, 56, 987654321).negate(),
        Interval.dayToSecond("-5 14:38:56.987654321"));
    assertEquals(Interval.of(0, 0, 0, 14), Interval.hour("14"));
    assertEquals(Interval.of(0, 0, 0, 14, 38), Interval.hourToMinute("14:38"));
    assertEquals(Interval.of(0, 0, 0, 14, 38, 56), Interval.hourToSecond("14:38:56"));
    assertEquals(Interval.of(0, 0, 0, 0, 38), Interval.minute("38"));
    assertEquals(Interval.of(0, 0, 0, 0, 38, 56), Interval.minuteToSecond("38:56"));
    assertEquals(Interval.of(0, 0, 0, 0, 38, 56, 123456789),
        Interval.minuteToSecond("38:56.123456789"));
    assertEquals(Interval.of(0, 0, 0, 0, 38, 56, 123456789).negate(),
        Interval.minuteToSecond("-38:56.123456789"));
    assertEquals(Interval.of(0, 0, 0, 0, 0, 56, 123456789), Interval.second("56.123456789"));
    assertEquals(Interval.of(0, 0, 0, 0, 0, 56, 123456789).negate(),
        Interval.second("-56.123456789"));

    // Verbose format
    assertEquals(Interval.of(5, 6, 30, 12, 30, 58, 999000000), Interval.valueOf("5-6 30 12:30:58.999"));
    assertEquals(Interval.of(4), Interval.valueOf(" 4 year"));
    assertEquals(Interval.of(4), Interval.valueOf(" 4 years "));
    assertEquals(Interval.of(4, 6), Interval.valueOf(" 4 years 6 months"));
    assertEquals(Interval.of(0, 0, 14), Interval.valueOf("2 weeks"));
    assertEquals(Interval.of(0, 3), Interval.valueOf("1 quarter"));
    assertEquals(Interval.of(4, 6, 22), Interval.valueOf(" 4 years, 6 months,   22 days"));
    assertEquals(Interval.of(0, 0, 5), Interval.valueOf(" 5 days"));
    assertEquals(Interval.of(0, 0, 5, 23, 30), Interval.valueOf("5 days 23 hours 30 minute"));
    assertEquals(Interval.of(0, 0, 0, 0, 30), Interval.valueOf("30 MINUTES "));
    assertEquals(Interval.of(0, 0, 0, 0, 0, 58), Interval.valueOf("58 seconds"));

    // ISO format 8601
    // assertEquals( new YearToMonth(5), YearToMonth.year("P5Y"));
  }

  @Test
  public void intervalFormat() throws Exception {
    assertEquals("+7-1 44 22:30:58.123456789",
        Interval.of(5, 25, 44, 22, 30, 58, 123456789).toString());
  }

  @Test
  public void intervalSign() throws Exception {
    assertEquals(1, Interval.yearToMonth("5-24").getSign());
    assertEquals(1, Interval.yearToMonth("+5-24").getSign());
    assertEquals(-1, Interval.yearToMonth("-5-24").getSign());
    assertEquals(-1, Interval.dayToSecond("-5 14:38:56.987654321").getSign());
  }

  @Test
  public void intervalQualifierOf() throws Exception {
    assertNull(IntervalQualifier.of(TimeUnit.CENTURY, null));
  }

  @Test
  public void intervalQualifierStartUnit() throws Exception {
    assertEquals(TimeUnit.YEAR, IntervalQualifier.YEAR.getStartUnit());
    assertEquals(TimeUnit.YEAR, IntervalQualifier.YEAR_TO_MONTH.getStartUnit());
    assertEquals(TimeUnit.QUARTER, IntervalQualifier.QUARTER.getStartUnit());
    assertEquals(TimeUnit.MONTH, IntervalQualifier.MONTH.getStartUnit());
    assertEquals(TimeUnit.WEEK, IntervalQualifier.WEEK.getStartUnit());
    assertEquals(TimeUnit.DAY, IntervalQualifier.DAY.getStartUnit());
    assertEquals(TimeUnit.DAY, IntervalQualifier.DAY_TO_HOUR.getStartUnit());
    assertEquals(TimeUnit.DAY, IntervalQualifier.DAY_TO_MINUTE.getStartUnit());
    assertEquals(TimeUnit.DAY, IntervalQualifier.DAY_TO_SECOND.getStartUnit());
    assertEquals(TimeUnit.HOUR, IntervalQualifier.HOUR.getStartUnit());
    assertEquals(TimeUnit.HOUR, IntervalQualifier.HOUR_TO_MINUTE.getStartUnit());
    assertEquals(TimeUnit.HOUR, IntervalQualifier.HOUR_TO_SECOND.getStartUnit());
    assertEquals(TimeUnit.MINUTE, IntervalQualifier.MINUTE.getStartUnit());
    assertEquals(TimeUnit.MINUTE, IntervalQualifier.MINUTE_TO_SECOND.getStartUnit());
    assertEquals(TimeUnit.SECOND, IntervalQualifier.SECOND.getStartUnit());    
  }
  
  @Test
  public void intervalQualifierEndUnit() throws Exception {    
    assertEquals(TimeUnit.YEAR, IntervalQualifier.YEAR.getEndUnit());
    assertEquals(TimeUnit.MONTH, IntervalQualifier.YEAR_TO_MONTH.getEndUnit());
    assertEquals(TimeUnit.QUARTER, IntervalQualifier.QUARTER.getEndUnit());
    assertEquals(TimeUnit.MONTH, IntervalQualifier.MONTH.getEndUnit());
    assertEquals(TimeUnit.WEEK, IntervalQualifier.WEEK.getEndUnit());
    assertEquals(TimeUnit.DAY, IntervalQualifier.DAY.getEndUnit());
    assertEquals(TimeUnit.HOUR, IntervalQualifier.DAY_TO_HOUR.getEndUnit());
    assertEquals(TimeUnit.MINUTE, IntervalQualifier.DAY_TO_MINUTE.getEndUnit());
    assertEquals(TimeUnit.SECOND, IntervalQualifier.DAY_TO_SECOND.getEndUnit());
    assertEquals(TimeUnit.HOUR, IntervalQualifier.HOUR.getEndUnit());
    assertEquals(TimeUnit.MINUTE, IntervalQualifier.HOUR_TO_MINUTE.getEndUnit());
    assertEquals(TimeUnit.SECOND, IntervalQualifier.HOUR_TO_SECOND.getEndUnit());
    assertEquals(TimeUnit.MINUTE, IntervalQualifier.MINUTE.getEndUnit());
    assertEquals(TimeUnit.SECOND, IntervalQualifier.MINUTE_TO_SECOND.getEndUnit());
    assertEquals(TimeUnit.SECOND, IntervalQualifier.SECOND.getEndUnit());
  }

  @Test
  public void intervalQualifierHasTimeUnit() throws Exception {
    assertTrue(IntervalQualifier.YEAR.hasYears());
    assertFalse(IntervalQualifier.YEAR.hasMonths());
    assertFalse(IntervalQualifier.YEAR.hasDays());
    assertFalse(IntervalQualifier.YEAR.hasMinutes());
    assertFalse(IntervalQualifier.YEAR.hasHours());

    assertTrue(IntervalQualifier.YEAR_TO_MONTH.hasYears());
    assertTrue(IntervalQualifier.YEAR_TO_MONTH.hasMonths());
    assertFalse(IntervalQualifier.YEAR_TO_MONTH.hasDays());

    assertFalse(IntervalQualifier.MONTH.hasYears());
    assertTrue(IntervalQualifier.MONTH.hasMonths());
    assertFalse(IntervalQualifier.MONTH.hasDays());

    assertTrue(IntervalQualifier.WEEK.hasDays());
    assertFalse(IntervalQualifier.WEEK.hasHours());

    assertFalse(IntervalQualifier.DAY.hasYears());
    assertFalse(IntervalQualifier.DAY.hasMonths());
    assertTrue(IntervalQualifier.DAY.hasDays());
    assertFalse(IntervalQualifier.DAY.hasHours());

    assertTrue(IntervalQualifier.DAY_TO_HOUR.hasDays());
    assertTrue(IntervalQualifier.DAY_TO_HOUR.hasHours());
    assertFalse(IntervalQualifier.DAY_TO_HOUR.hasMinutes());

    assertFalse(IntervalQualifier.DAY_TO_MINUTE.hasMonths());
    assertTrue(IntervalQualifier.DAY_TO_MINUTE.hasDays());
    assertTrue(IntervalQualifier.DAY_TO_MINUTE.hasHours());
    assertTrue(IntervalQualifier.DAY_TO_MINUTE.hasMinutes());

    assertTrue(IntervalQualifier.DAY_TO_SECOND.hasDays());
    assertTrue(IntervalQualifier.DAY_TO_SECOND.hasHours());
    assertTrue(IntervalQualifier.DAY_TO_SECOND.hasMinutes());
    assertTrue(IntervalQualifier.DAY_TO_SECOND.hasSeconds());

    assertTrue(IntervalQualifier.HOUR.hasHours());
    assertFalse(IntervalQualifier.HOUR.hasMinutes());

    assertTrue(IntervalQualifier.HOUR_TO_SECOND.hasHours());
    assertTrue(IntervalQualifier.HOUR_TO_SECOND.hasMinutes());
    assertTrue(IntervalQualifier.HOUR_TO_SECOND.hasSeconds());

    assertFalse(IntervalQualifier.MINUTE.hasHours());
    assertTrue(IntervalQualifier.MINUTE.hasMinutes());
    assertFalse(IntervalQualifier.MINUTE.hasSeconds());

    assertFalse(IntervalQualifier.MINUTE_TO_SECOND.hasHours());
    assertTrue(IntervalQualifier.MINUTE_TO_SECOND.hasMinutes());
    assertTrue(IntervalQualifier.MINUTE_TO_SECOND.hasSeconds());

    assertFalse(IntervalQualifier.SECOND.hasMinutes());
    assertTrue(IntervalQualifier.SECOND.hasSeconds());
  }
  
  @Test
  public void intervalAbs() throws Exception {
    assertEquals(Interval.of(4, 6), Interval.valueOf(" 4 years 6 months").negate().abs());  
  }
  
}

