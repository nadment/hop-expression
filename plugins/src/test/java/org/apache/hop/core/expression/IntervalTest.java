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
import org.apache.hop.expression.DayToSecond;
import org.apache.hop.expression.IntervalQualifier;
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.YearToMonth;
import org.junit.Test;


public class IntervalTest extends ExpressionTest {


  @Test
  public void yearsToMonths() throws Exception {
    // SQL format
    assertEquals( new YearToMonth(5), YearToMonth.year("5"));
    assertEquals( new YearToMonth(1), YearToMonth.yearToMonth("+0-12"));
    assertEquals( new YearToMonth(5,25), YearToMonth.yearToMonth("6-13"));
    assertNotEquals( new YearToMonth(5,25), YearToMonth.yearToMonth("-5-25"));
    assertNotEquals( new YearToMonth(5,25), YearToMonth.yearToMonth("6-25"));
    assertNotEquals( new YearToMonth(5,25), YearToMonth.yearToMonth("5-24"));
    assertNotEquals( new YearToMonth(25), YearToMonth.month("25"));
    
    // ISO format
    //assertEquals( new YearToMonth(5), YearToMonth.year("P5Y"));
    
    
    assertNotEquals( null, YearToMonth.yearToMonth("5-24"));
    assertNotEquals( Long.MAX_VALUE, YearToMonth.yearToMonth("5-24"));
    
    YearToMonth ytm = new YearToMonth();  
    assertEquals(0, ytm.getYears());
    assertEquals(0, ytm.getMonths());
    assertEquals(1, ytm.getSign());
    assertEquals("+0-0", ytm.toString());
    
    assertNull(YearToMonth.year(null));
    assertNull(YearToMonth.yearToMonth(null));
    assertNull(YearToMonth.month(null));
    
    ytm = new YearToMonth(5,25);
    assertEquals(7, ytm.getYears());
    assertEquals(1, ytm.getMonths());
    assertEquals("+7-1", ytm.toString());
    
    assertEquals(1, YearToMonth.valueOf("5-24").getSign());
    assertEquals(1, YearToMonth.valueOf("+5-24").getSign());
    assertEquals(-1, YearToMonth.valueOf("-5-24").getSign());
  }
  
  @Test
  public void dayToSeconds() throws Exception {
    assertEquals( new DayToSecond(5), DayToSecond.day("5"));
    assertEquals( new DayToSecond(5,14), DayToSecond.dayToHour("+5 14"));
    assertEquals( new DayToSecond(5,14,38), DayToSecond.dayToMinute("+5 14:38"));
    assertEquals( new DayToSecond(5,14,38,56), DayToSecond.dayToSecond("5 14:38:56"));
    assertEquals( new DayToSecond(5,14,38,56, 987654321), DayToSecond.dayToSecond("5 14:38:56.987654321"));
    assertEquals( new DayToSecond(5,14,38,56, 987654321).negated(), DayToSecond.dayToSecond("-5 14:38:56.987654321"));
    assertEquals( new DayToSecond(0,14), DayToSecond.hour("14"));
    assertEquals( new DayToSecond(0,14,38), DayToSecond.hourToMinute("14:38"));
    assertEquals( new DayToSecond(0,14,38,56), DayToSecond.hourToSecond("14:38:56"));
    assertEquals( new DayToSecond(0,0,38), DayToSecond.minute("38"));
    assertEquals( new DayToSecond(0,0,38,56), DayToSecond.minuteToSecond("38:56"));
    
    assertNull(DayToSecond.day(null));
    assertNull(DayToSecond.dayToHour(null));
    assertNull(DayToSecond.dayToMinute(null));
    assertNull(DayToSecond.dayToSecond(null));
    assertNull(DayToSecond.hour(null));
    assertNull(DayToSecond.hourToMinute(null));
    assertNull(DayToSecond.hourToSecond(null));
    assertNull(DayToSecond.minute(null));
    assertNull(DayToSecond.minuteToSecond(null));
    assertNull(DayToSecond.second(null));
    
    DayToSecond dts = new DayToSecond();
    assertEquals(0, dts.getDays());
    assertEquals(0, dts.getHours());
    assertEquals(0, dts.getMinutes());
    assertEquals(0, dts.getSeconds());
    assertEquals(0, dts.getMilli());
    assertEquals(0, dts.getMicro());
    assertEquals(0, dts.getNanos());
    assertEquals(1, dts.getSign());
    assertEquals("+0 00:00:00.000000000", dts.toString());
    
    assertEquals(5, DayToSecond.dayToSecond("-5 14:38:56.987654321").getDays());
    assertEquals(14, DayToSecond.dayToSecond("-5 14:38:56.987654321").getHours());
    assertEquals(38, DayToSecond.dayToSecond("-5 14:38:56.987654321").getMinutes());
    assertEquals(56, DayToSecond.dayToSecond("-5 14:38:56.987654321").getSeconds());
    assertEquals(987, DayToSecond.dayToSecond("-5 14:38:56.987654321").getMilli());
    assertEquals(987654, DayToSecond.dayToSecond("-5 14:38:56.987654321").getMicro());
    assertEquals(987654321, DayToSecond.dayToSecond("-5 14:38:56.987654321").getNanos());
    assertEquals(-1, DayToSecond.dayToSecond("-5 14:38:56.987654321").getSign());    
  }

  @Test
  public void intervalQualifierOf() throws Exception {
    assertNull(IntervalQualifier.of(TimeUnit.CENTURY, null));
  }
  
  @Test
  public void intervalQualifierStartEndUnit() throws Exception {
    assertEquals(TimeUnit.YEAR, IntervalQualifier.YEAR.getStartUnit());    
    assertEquals(TimeUnit.YEAR, IntervalQualifier.YEAR.getEndUnit());
    assertEquals(TimeUnit.YEAR, IntervalQualifier.YEAR_TO_MONTH.getStartUnit());
    assertEquals(TimeUnit.MONTH, IntervalQualifier.YEAR_TO_MONTH.getEndUnit());    
    assertEquals(TimeUnit.MONTH, IntervalQualifier.MONTH.getStartUnit());
    assertEquals(TimeUnit.MONTH, IntervalQualifier.MONTH.getEndUnit());    
    assertEquals(TimeUnit.DAY, IntervalQualifier.DAY.getStartUnit());   
    assertEquals(TimeUnit.DAY,IntervalQualifier.DAY.getEndUnit());
    assertEquals(TimeUnit.DAY, IntervalQualifier.DAY_TO_HOUR.getStartUnit());
    assertEquals(TimeUnit.HOUR, IntervalQualifier.DAY_TO_HOUR.getEndUnit());    
    assertEquals(TimeUnit.DAY, IntervalQualifier.DAY_TO_MINUTE.getStartUnit());
    assertEquals(TimeUnit.MINUTE, IntervalQualifier.DAY_TO_MINUTE.getEndUnit());    
    assertEquals(TimeUnit.DAY, IntervalQualifier.DAY_TO_SECOND.getStartUnit());
    assertEquals(TimeUnit.SECOND, IntervalQualifier.DAY_TO_SECOND.getEndUnit());           
    assertEquals(TimeUnit.HOUR, IntervalQualifier.HOUR.getStartUnit());   
    assertEquals(TimeUnit.HOUR, IntervalQualifier.HOUR.getEndUnit());    
    assertEquals(TimeUnit.HOUR, IntervalQualifier.HOUR_TO_MINUTE.getStartUnit());
    assertEquals(TimeUnit.MINUTE, IntervalQualifier.HOUR_TO_MINUTE.getEndUnit());    
    assertEquals(TimeUnit.HOUR, IntervalQualifier.HOUR_TO_SECOND.getStartUnit());
    assertEquals(TimeUnit.SECOND, IntervalQualifier.HOUR_TO_SECOND.getEndUnit());
    assertEquals(TimeUnit.MINUTE, IntervalQualifier.MINUTE.getStartUnit());
    assertEquals(TimeUnit.MINUTE,IntervalQualifier.MINUTE.getEndUnit());
    assertEquals(TimeUnit.MINUTE, IntervalQualifier.MINUTE_TO_SECOND.getStartUnit());
    assertEquals(TimeUnit.SECOND, IntervalQualifier.MINUTE_TO_SECOND.getEndUnit());
    assertEquals(TimeUnit.SECOND, IntervalQualifier.SECOND.getStartUnit());
    assertEquals(TimeUnit.SECOND,IntervalQualifier.SECOND.getEndUnit());
  }
  
  @Test
  public void  intervalQualifierHasTimeUnit() throws Exception {
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
}

