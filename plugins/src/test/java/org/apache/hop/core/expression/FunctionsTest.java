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

import org.apache.hop.expression.Attribute;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.type.DataTypeName;
import org.apache.hop.expression.util.Converter;
import org.junit.Test;
import java.security.SecureRandom;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Locale;
import java.util.Random;
import java.util.TimeZone;

public class FunctionsTest extends BaseExpressionTest {

  @Test
  public void Abort() throws Exception {
    evalFails("Abort('Custom error message')");
  }
  
  @Test
  public void Try() throws Exception {

    // Division by zero
    evalNull("TRY(10/0)");
    
    // String to Boolean
    evalTrue("TRY(CAST('Yes' as Boolean))");
    evalFalse("TRY(CAST('False' as Boolean))");
    evalNull("TRY(CAST('Fake' as Boolean))");

    // Number to Boolean
    evalTrue("TRY(CAST(1 as Boolean))");
    evalTrue("TRY(CAST(-12.1 as Boolean))");
    evalNull("TRY(CAST('test' as Boolean))");

    // Date to String
    evalEquals("TRY(CAST(Date '2019-02-25' AS String FORMAT 'DD/MM/YYYY'))", "25/02/2019");
    evalNull("TRY(CAST('2019-99-25' AS Date))");
    evalNull("TRY(CAST('2019-99-25' AS DATE FORMAT 'YYYY-MM-DD'))");
    evalNull("TRY(CAST(NULL AS Date))");    
    evalNull("Try(To_Date('2019-13-13','YYYY-MM-DD'))");    
    evalEquals("Try(To_Date('2019-02-13','YYYY-MM-DD'))", LocalDate.of(2019, 2, 13));
    
    // Bad syntax
    evalFails("TRY(CAST('2020-01-021' AS NULL))");
    evalFails("TRY(CAST('2020-01-021' AS DATE FORMAT NULL))");
    evalFails("TRY(CAST('bad' AS))");
    evalFails("TRY(CAST(1234 AS STRING FORMAT ))");
    evalFails("TRY(CAST(Date '2019-02-25' AS String FORMAT ))");

    // Bad data type
    evalFails("Try(Cast(123 as Nill))");

    writeEquals("TRY(CAST(DATA AS BINARY))");
    writeEquals("TRY(CAST(AGE AS NUMBER))");
    writeEquals("TRY(CAST(FIELD_DATE AS DATE FORMAT 'YYYY-MM-DD'))");
    
    returnType("Try(TRUE)", DataTypeName.BOOLEAN);
    returnType("Try('String')", DataTypeName.STRING);
    returnType("Try(Date '2020-07-01')", DataTypeName.DATE);
  }
  
  @Test
  public void Coalesce() throws Exception {
    evalEquals("Coalesce(1,2,3)", 1);
    evalEquals("Coalesce(null,1,2)", 1);
    evalEquals("Coalesce(null,'TEST','BIDON')", "TEST");
    evalNull("Coalesce(null,null,null)");
    evalFails("Coalesce()");
    
    returnType("Coalesce(AGE, NULL, 5)", DataTypeName.INTEGER);
    returnType("Coalesce(NAME, NULL, 'XYZ')", DataTypeName.STRING);    
  }

  @Test
  public void If() throws Exception {
    evalEquals("If(True,'True','False')", "True");
    evalEquals("If(False,'True','False')", "False");
    evalNull("If(null,'A','B')");
    evalFails("If()");
    evalFails("If(true)");
    evalFails("If(true,2)");
    returnType("If(FLAG,'A','B')", DataTypeName.STRING);    
    returnType("If(FLAG,1,2)", DataTypeName.INTEGER);
  }

  @Test
  public void Nvl2() throws Exception {
    evalEquals("Nvl2(True,'ex1','ex2')", "ex1");
    evalEquals("Nvl2('test','ex1','ex2')", "ex1");
    evalEquals("Nvl2(NULL,'ex1','ex2')", "ex2");
    evalFails("Nvl2()");
    evalFails("Nvl2(true)");
    evalFails("Nvl2(true,2)");
  }

  @Test
  public void IfNull() throws Exception {
    evalEquals("IfNull(1,2)", 1);
    evalEquals("IfNull(null,1)", 1);
    evalEquals("IfNull(null,'TEST')", "TEST");
    evalFails("IfNull()");
    evalFails("IfNull(1)");
    evalFails("IfNull(1,2,3)");

    // Alias
    evalEquals("NVL(null,1)", 1);
  }

  @Test
  public void NullIf() throws Exception {
    evalEquals("NullIf(1,null)", 1);
    evalNull("NullIf(1,1)");
    evalNull("NullIf(NULL,1)");
    evalNull("NullIf('TEST','TEST')");
    evalNull("NullIf(Date '2019-01-01',Date '2019-01-01')");
    evalEquals("NullIf(1,2)", 1);
    evalEquals("NullIf('TEST','XXX')", "TEST");
    evalEquals("NullIf(Date '2019-01-01',Date '2018-12-31')", LocalDate.of(2019, Month.JANUARY, 1));
  }

  @Test
  public void ZeroIfNull() throws Exception {
    evalEquals("ZeroIfNull(1)", 1);
    evalEquals("ZeroIfNull(null)", 0);
    
    evalFails("ZeroIfNull()");
    evalFails("ZeroIfNull(1,2)");
  }

  @Test
  public void NullIfZero() throws Exception {
    evalEquals("NullIfZero(1)", 1);
    evalNull("NullIfZero(0)");
    evalNull("NullIfZero(0.000)");
    evalNull("NullIfZero(-0.0)");
    
    evalFails("NullIfZero()");
  }

  @Test
  public void Decode() throws Exception {
    evalEquals("Decode(1,1,'one',2,'two',Null,'<NULL>','other')", "one");
    evalEquals("Decode(2,1,'one',2,'two',Null,'<NULL>','other')", "two");
    evalEquals("Decode(NULL,1,'one',2,'two',Null,'<NULL>','other')", "<NULL>");
    evalEquals("Decode(9,1,'one',2,'two',Null,'<NULL>','other')", "other");
    
    // Support ABORT as default
    evalEquals("Decode(AGE,4,'Flag 1',40,'Flag 2',ABORT('Error generated with ABORT'))","Flag 2");
    
    evalNull("Decode(9,1,'one',2,'two',Null,'<NULL>')");
    
    evalFails("Decode()");
    evalFails("Decodo(1)");
    evalFails("Decode(1,2)");
  }

  @Test
  public void Pi() throws Exception {
    evalEquals("Pi()", Math.PI);
    evalFails("Pi(123)");
    returnType("Pi()", DataTypeName.NUMBER);
  }

  @Test
  public void Current_Date() throws Exception {
    ExpressionContext context = createExpressionContext();
    ZonedDateTime today = Attribute.CURRENT_DATE.get(context);
    evalEquals("Today()", today, context);
    evalEquals("Current_Date()", today, context);

    evalFails("Today(Null)");
    
    returnType("Current_Date()", DataTypeName.DATE);
  }

  @Test
  public void Current_Timestamp() throws Exception {
    ExpressionContext context = createExpressionContext();
    ZonedDateTime today = Attribute.CURRENT_TIMESTAMP.get(context);
    evalEquals("Now()", today, context);
    evalEquals("Current_Timestamp()", today, context);

    evalFails("Now(Null)");
    
    returnType("Current_Timestamp()", DataTypeName.DATE);
  }

  @Test
  public void Current_TimeZone() throws Exception {
    TimeZone.setDefault(TimeZone.getTimeZone("Europe/Paris"));
    evalEquals("Current_Timezone()", "Europe/Paris");
    TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
    evalEquals("Current_Timezone()", "UTC");
    evalFails("Current_Timezone(Null)");
    
    returnType("Current_Timezone()", DataTypeName.STRING);
  }

  @Test
  public void Date() throws Exception {
    evalEquals("Date(2019,01,1)", LocalDate.of(2019, Month.JANUARY, 1));
    evalEquals("Date(2020,02,27)", LocalDate.of(2020, Month.FEBRUARY, 27));
    evalEquals("Date(2020,19,1)", LocalDate.of(2021, Month.JULY, 1));
    evalEquals("Date(2020,-6,1)", LocalDate.of(2019, Month.JULY, 1));
    evalEquals("Date(2020,-1,1)", LocalDate.of(2019, Month.DECEMBER, 1));
    evalEquals("Date(2020, 6, 50)", LocalDate.of(2020, Month.JULY, 21));

    evalNull("Date(null,-1,1)");
    evalNull("Date(2020,null,1)");
    evalNull("Date(2020,-1,null)");

    evalFails("Date()");
    evalFails("Date(2020)");
    evalFails("Date(2020,15)");
    evalFails("Date(2020,1,1,1)");
  }

  @Test
  public void First_Day() throws Exception {
    evalEquals("First_Day(Date '2019-01-01')", LocalDate.of(2019, Month.JANUARY, 1));
    evalEquals("First_Day(Date '2020-02-27')", LocalDate.of(2020, Month.FEBRUARY, 1));
    evalEquals("First_Day(Date '2020-02-27', YEAR)", LocalDate.of(2020, Month.JANUARY, 1));
    evalEquals("First_Day(Date '2020-02-27', MONTH)", LocalDate.of(2020, Month.FEBRUARY, 1));
    evalEquals("First_Day(Date '2020-02-27', QUARTER)", LocalDate.of(2020, Month.JANUARY, 1));
    evalEquals("First_Day(Date '2020-05-27', QUARTER)", LocalDate.of(2020, Month.APRIL, 1));
    evalEquals("First_Day(Date '2020-02-01', WEEK)", LocalDate.of(2020, Month.JANUARY, 27));
    evalEquals("First_Day(Date '2020-02-27', WEEK)", LocalDate.of(2020, Month.FEBRUARY, 24));
    evalEquals("First_Day(Date '2020-12-31', WEEK)", LocalDate.of(2020, Month.DECEMBER, 28));
    
    // Remove time
    evalEquals("First_Day(Timestamp '2020-02-27 23:59:12', YEAR)", LocalDate.of(2020, Month.JANUARY, 1));
    evalEquals("First_Day(Timestamp '2020-02-27 23:59:12', MONTH)", LocalDate.of(2020, Month.FEBRUARY, 1));
    
    evalNull("First_Day(NULL)");
    evalNull("First_day(Date '2020-02-27', NULL)");
    
    evalFails("First_Day()");
    evalFails("First_Day('test')");
    evalFails("First_Day(Date '2020-12-31', AGE)");
  }

  @Test
  public void Last_Day() throws Exception {
    evalEquals("Last_Day(Date '2019-01-01')", LocalDate.of(2019, Month.JANUARY, 31));
    evalEquals("Last_Day(Date '2020-02-27')", LocalDate.of(2020, Month.FEBRUARY, 29));
    evalEquals("Last_Day(Date '2020-02-27', YEAR)", LocalDate.of(2020, Month.DECEMBER, 31));
    evalEquals("Last_Day(Date '2022-02-27', MONTH)", LocalDate.of(2022, Month.FEBRUARY, 28));
    evalEquals("Last_Day(Date '2020-02-27', MONTH)", LocalDate.of(2020, Month.FEBRUARY, 29));
    evalEquals("Last_Day(Date '2020-02-27', QUARTER)", LocalDate.of(2020, Month.MARCH, 31));
    evalEquals("Last_Day(Date '2020-07-27', QUARTER)", LocalDate.of(2020, Month.SEPTEMBER, 30));
    evalEquals("Last_Day(Date '2020-12-31', WEEK)", LocalDate.of(2021, Month.JANUARY, 3));
    
    // Remove time
    evalEquals("Last_Day(Timestamp '2020-02-27 23:59:12')", LocalDate.of(2020, Month.FEBRUARY, 29));
    evalEquals("Last_Day(Timestamp '2020-02-27 23:59:12', YEAR)", LocalDate.of(2020, Month.DECEMBER, 31));
    
    evalNull("Last_Day(NULL)");
    evalNull("Last_Day(Date '2020-02-27', NULL)");
    
    evalFails("Last_Day()");
    evalFails("Last_Day('test')");
    evalFails("Last_Day(Date '2020-12-31', AGE)");
  }

  @Test
  public void Next_Day() throws Exception {
    evalEquals("Next_Day(Date '2020-02-28','monday')", LocalDate.of(2020, Month.MARCH, 2));

    evalNull("Next_Day(null, 'monday')");
    evalNull("Next_Day(Date '2020-02-28', null)");

    evalFails("Next_Day()");
    evalFails("Next_Day(Date '2020-02-28')");
  }

  @Test
  public void Previous_Day() throws Exception {
    evalEquals("Previous_Day(Date '2020-02-28','monday')", LocalDate.of(2020, Month.FEBRUARY, 24));

    evalNull("Previous_Day(null, 'monday')");
    evalNull("Previous_Day(Date '2020-02-28', null)");

    evalFails("Previous_Day()");
    evalFails("Previous_Day(Date '2020-02-28')");
  }

  @Test
  public void Normalize() throws Exception {
    evalEquals("Normalize('\u00ea')", "ê");
    evalEquals("Normalize('\u0065\u0302')", "ê");
    evalEquals("Normalize('Jane\u2004Doe', 'NFKC')", "Jane Doe");
    evalEquals("Normalize('Jane\u2006Doe', 'NFKC')", "Jane Doe");
    evalEquals("Normalize('¼', 'NFKC')", "1⁄4");
    evalEquals("Normalize('i⁹', 'NFKC')", "i9");    
    evalNull("Normalize(NULL)");
    
    evalFails("Normalize()");
    evalFails("Normalize('\u00ea','BAD')");
    
    returnType("Normalize('\u00ea')", DataTypeName.STRING);
  }
  
  @Test
  public void Unaccent() throws Exception {
    evalEquals("Unaccent('ÁÀÂÃÄÅĀĄàáâãäåāą')", "AAAAAAAAaaaaaaaa");
    evalEquals("Unaccent('ÇĆČçćč')", "CCCccc");
    evalEquals("Unaccent('ĎḌḒďḍḓ')", "DDDddd");   
    evalEquals("Unaccent('ÈÉÊËĚĒĘèéêëěēę')", "EEEEEEEeeeeeee");
    evalEquals("Unaccent('ÌÍÎÏĪìíîïī')", "IIIIIiiiii");
    evalEquals("Unaccent('Łł')", "Ll");
    evalEquals("Unaccent('ÑŇŃñňń')", "NNNnnn");
    evalEquals("Unaccent('ÒÓÔÕÕÖŌòóôõöō')", "OOOOOOOoooooo");
    evalEquals("Unaccent('ÙÚÛÜŮŪùúûüůū')", "UUUUUUuuuuuu");
    evalEquals("Unaccent('Řř')", "Rr");
    evalEquals("Unaccent('ŠŚšś')", "SSss");
    evalEquals("Unaccent('Ťť')", "Tt");
    evalEquals("Unaccent('ÝŸÿý')", "YYyy");    
    evalEquals("Unaccent('ŽŻŹžżź')", "ZZZzzz");    
    evalEquals("Unaccent('άώὨ')", "αωΩ");

     // Keep ligature and special char
    evalEquals("Unaccent('ÆæØøµß¢©')", "ÆæØøµß¢©");
    
    evalNull("Unaccent(NULL)");
    evalFails("Unaccent()");
  }

  @Test
  public void Upper() throws Exception {
    evalEquals("Upper('test')", "TEST");
    evalNull("Upper(NULL)");
    evalFails("Upper()");

    // Alias
    // evalEquals("UCase('test')", "TEST");
  }

  @Test
  public void InitCap() throws Exception {
    evalEquals("InitCap('hello the wORLD')", "Hello The World");
    evalEquals("InitCap('tRy a littlE  ')", "Try A Little  ");
    evalEquals("InitCap('won''t it?no')", "Won'T It?No");
    evalEquals("InitCap('ÉéÀàè]çÂâ ÊêÎÔô ÛûËÏ ïÜŸÇç ŒœÆæ')", "Ééààè]Çââ Êêîôô Ûûëï Ïüÿçç Œœææ");
    evalNull("InitCap(NULL)");
    evalFails("InitCap()");
  }

  @Test
  public void Instr() throws Exception {
    evalEquals("Instr('CORPORATE FLOOR','OR')", 2);
    evalEquals("Instr('CORPORATE FLOOR','or')", 0);
    evalEquals("Instr('CORPORATE FLOOR','ORA')", 5);
    evalEquals("Instr('CORPORATE FLOOR','ORA',6)", 0);
    evalEquals("Instr('CORPORATE FLOOR','OR',5)", 5);
    evalEquals("Instr('CORPORATE FLOOR','OR',6)", 14);
    evalEquals("Instr('CORPORATE FLOOR','OR',3)", 5);
    evalEquals("Instr('CORPORATE FLOOR','OR',3, 1)", 5);
    evalEquals("Instr('CORPORATE FLOOR','OR',3, 2)", 14);
    evalEquals("Instr('CORPORATE FLOOR','OR',3, 3)", 0);

    evalEquals("Instr('CORPORATE FLOOR','O',-1)", 14);
    evalEquals("Instr('CORPORATE FLOOR','O',-2)", 14);
    evalEquals("Instr('CORPORATE FLOOR','O',-3)", 13);
    evalEquals("Instr('CORPORATE FLOOR','O',-4)", 5);
    evalEquals("Instr('CORPORATE FLOOR','OR',-1)", 14);
    evalEquals("Instr('CORPORATE FLOOR','OR',-3)", 5);
    evalEquals("Instr('CORPORATE FLOOR','OR',-12)", 2);
    evalEquals("Instr('CORPORATE FLOOR','OR',-3, 1)", 5);
    evalEquals("Instr('CORPORATE FLOOR','OR',-3, 2)", 2);
    evalEquals("Instr('CORPORATE FLOOR','OR',-3, 3)", 0);
     
    evalNull("Instr(NULL,'test')");
    evalNull("Instr('test',NULL)");
    evalNull("Instr(NULL,NULL)");
    
    evalFails("Instr('CORPORATE FLOOR','OR',-3, 0)");
    evalFails("Instr()");
  }

  @Test
  public void RPad() throws Exception {
    evalEquals("RPad('test',7)", "test   ");
    evalEquals("RPad('test',7,'*')", "test***");
    evalEquals("RPad('test',4,'*')", "test");
    evalEquals("RPad('test',3,'*')", "tes");
    evalEquals("RPad('test',12,'')", "test");
    evalEquals("RPad('test',8,'ABC')", "testABCA");
    evalEquals("RPad('test',-8)", "");
    evalNull("RPad(NULL,-8)");
    evalFails("RPad('test')");
    // Test PAD_LIMIT
    evalFails("RPad('test',10000)");
  }

  @Test
  public void LPad() throws Exception {
    evalEquals("LPad('test',6)", "  test");
    evalEquals("LPad('test',7,'*')", "***test");
    evalEquals("LPad('test',3,'*')", "tes");
    evalEquals("LPad('test',8,'ABC')", "ABCAtest");
    evalEquals("LPad('test',12,'')", "test");
    evalEquals("LPad('test',6,'ABC')", "ABtest");
    evalEquals("LPad('test',4,'ABC')", "test");
    evalEquals("LPad('test',-8)", "");
    evalNull("LPad(NULL,-8)");
    evalFails("LPad('test')");
    // Test PAD_LIMIT
    evalFails("LPad('test',10000)");
  }

  @Test
  public void Year() throws Exception {
    evalEquals("Year(Date '2019-01-01')", 2019);
    evalNull("Year(null)");
    evalFails("Year()");
  }

  @Test
  public void MonthName() throws Exception {
    evalEquals("MonthName(Date '2019-01-01')", "January");
    evalEquals("MonthName(Date '2019-12-28')", "December");
    evalNull("MonthName(NULL)");
    evalFails("MonthName()");
  }

  @Test
  public void DayName() throws Exception {
    evalEquals("DayName(Date '2019-01-01')", "Tuesday");
    evalEquals("DayName(Date '2019-12-28')", "Saturday");
    evalNull("DayName(NULL)");
    evalFails("DayName()");
  }

  @Test
  public void Month() throws Exception {
    evalEquals("Month(Date '2019-01-01')", 1);
    evalEquals("Month(Date '2020-02-23')", 2);
    evalEquals("Month(Date '2019-12-28')", 12);
    evalNull("Month(NULL)");
    evalFails("Month()");
  }

  @Test
  public void Years_Between() throws Exception {
    evalEquals("Years_Between(Timestamp '2001-01-01 12:00:00',Timestamp '2000-01-01 00:00:00')",
        -1);
    evalNull("Years_Between(NULL, Date '2007-11-09')");
    evalNull("Years_Between(Date '2007-11-09',NULL)");
    evalNull("Years_Between(NULL, NULL)");
    evalFails("Years_Between(Date '2007-11-09')");
  }

  @Test
  public void Months_Between() throws Exception {
    evalEquals("Months_Between(Date '2005-01-01',Date '2005-02-02')", 1.032258064516129);
    evalEquals("Months_Between(Date '2007-11-09',Date '2003-12-28')", -45.54838709677419);


    // evalEquals("Months_Between(Date '2007-11-10',Date '2007-12-09')", -0.967742);
    // TODO: If the months and days are identical, the result is an integer.
    evalEquals("Months_Between(Date '2007-11-09',Date '2007-12-09')", 0.967741935483871);

    evalNull("Months_Between(Date '2007-11-09',NULL)");
    evalNull("Months_Between(NULL, Date '2007-11-09')");
    evalNull("Months_Between(NULL, NULL)");
    evalFails("Months_Between(Date '2007-11-09')");
  }

  @Test
  public void Days_Between() throws Exception {
    evalEquals("Days_Between(Date '2021-01-01',Date '2021-01-01')", 0.0);
    evalEquals("Days_Between(Date '2021-11-09',Date '2020-12-28')", -316);
    evalEquals("Days_Between(Date '2007-11-09',Date '2007-12-09')", 30.0);

    evalNull("Days_Between(Date '2007-11-09',NULL)");
    evalNull("Days_Between(NULL, Date '2007-11-09')");
    evalNull("Days_Between(NULL, NULL)");
    evalFails("Days_Between(Date '2007-11-09')");
  }

  @Test
  public void Hours_Between() throws Exception {
    evalEquals("Hours_Between(Timestamp '2019-01-01 15:00:59',Timestamp '2019-01-01 15:28:59')", 0);
    evalEquals("Hours_Between(Timestamp '2019-01-01 15:00:59',Timestamp '2019-01-02 15:00:59')",
        24);
    evalNull("Hours_Between(NULL, Timestamp '2019-01-01 15:00:59')");
    evalNull("Hours_Between(Timestamp '2019-01-01 15:00:59', NULL)");
    evalFails("Hours_Between(Date '2007-11-09')");
  }

  @Test
  public void Minutes_Between() throws Exception {
    evalEquals("Minutes_Between(Timestamp '2019-01-01 15:00:59',Timestamp '2019-01-01 15:28:59')",
        28);
    evalEquals("Minutes_Between(Timestamp '2019-01-01 15:00:59',Timestamp '2019-01-02 15:00:59')",
        1440);
    evalNull("Minutes_Between(NULL, Timestamp '2019-01-01 15:00:59')");
    evalNull("Minutes_Between(Timestamp '2019-01-01 15:00:59', NULL)");
    evalFails("Minutes_Between(Date '2007-11-09')");
  }

  @Test
  public void Seconds_Between() throws Exception {
    evalEquals("Seconds_Between(Timestamp '2019-01-01 15:00:59',Timestamp '2019-01-01 15:28:59')",
        28 * 60);
    evalEquals("Seconds_Between(Timestamp '2019-01-01 15:00:59',Timestamp '2019-01-02 15:00:59')",
        86400);
    evalNull("Seconds_Between(NULL, Timestamp '2019-01-01 15:00:59')");
    evalNull("Seconds_Between(Timestamp '2019-01-01 15:00:59', NULL)");
    evalFails("Seconds_Between(Date '2007-11-09')");
  }

  @Test
  public void Quarter() throws Exception {
    evalEquals("Quarter(Date '2019-01-01')", 1);
    evalEquals("Quarter(Date '2019-02-28')", 1);
    evalEquals("Quarter(Date '2019-04-28')", 2);
    evalEquals("Quarter(Date '2019-08-28')", 3);
    evalEquals("Quarter(Date '2019-12-28')", 4);
    evalNull("Quarter(NULL)");
    evalFails("Quarter()");
  }

  @Test
  public void DayOfWeek() throws Exception {
    evalEquals("DayOfWeek(Date '2019-01-01')", 3);
    evalEquals("DayOfWeek(Date '2019-07-27')", 7);
    evalEquals("DayOfWeek(Date '2019-07-28')", 1);
    evalEquals("DayOfWeek(Date '2019-12-31')", 3);
    evalNull("DayOfWeek(NULL)");
    evalFails("DayOfWeek()");
  }

  @Test
  public void Day() throws Exception {
    evalEquals("Day(Date '2019-01-01')", 1);
    evalEquals("Day(Date '2019-02-28')", 28);
    evalEquals("Day(Date '2019-12-28')", 28);
    evalEquals("Day(BIRTHDATE)", 23);
    evalNull("Day(NULL)");
    evalFails("Day()");
  }

  @Test
  public void DayOfYear() throws Exception {
    evalEquals("DayOfYear(Date '2019-01-01')", 1);
    evalEquals("DayOfYear(Date '2019-02-02')", 33);
    evalEquals("DayOfYear(Date '2019-12-31')", 365);
    evalNull("DayOfYear(NULL)");
    evalFails("DayOfYear()");
  }

  @Test
  public void Week() throws Exception {
    evalEquals("Week(Date '2015-12-31')", 53);
    evalEquals("Week(Date '2015-01-01')", 1);
    evalEquals("Week(Date '2015-01-02')", 1);
    evalNull("Week(NULL)");
    evalFails("Week()");
  }
  
  @Test
  public void IsoDayOfWeek() throws Exception {
    evalEquals("IsoDayOfWeek(Date '2003-12-28')", 7);
    evalNull("IsoDayOfWeek(NULL)");
    evalFails("IsoDayOfWeek()");

  }
  
  @Test
  public void IsoWeek() throws Exception {
    evalEquals("IsoWeek(Date '2015-12-31')", 53);
    evalEquals("IsoWeek(Date '2016-01-01')", 53);
    evalEquals("IsoWeek(Date '2016-01-02')", 53);
    evalEquals("IsoWeek(Date '2016-01-03')", 53);
    evalEquals("IsoWeek(Date '2016-01-04')", 1);
    evalNull("IsoWeek(NULL)");
    evalFails("IsoWeek()");
  }

  @Test
  public void IsoYear() throws Exception {
    evalEquals("IsoYear(Date '2015-12-31')", 2015);
    evalEquals("IsoYear(Date '2016-01-01')", 2015);
    evalEquals("IsoYear(Date '2016-01-02')", 2015);
    evalEquals("IsoYear(Date '2016-01-04')", 2016);
    evalEquals("IsoYear(Date '2042-12-31')", 2043);
    evalNull("IsoYear(NULL)");
    evalFails("IsoYear('ERROR')");
    evalFails("IsoYear()");
  }

  @Test
  public void Add_Years() throws Exception {
    evalEquals("Add_Years(Date '2019-01-15',1)", LocalDate.of(2020, Month.JANUARY, 15));
    evalEquals("Add_Years(Date '2019-01-15',-2)", LocalDate.of(2017, Month.JANUARY, 15));
    evalEquals("Add_Years(Date '2019-11-15',3)", LocalDate.of(2022, Month.NOVEMBER, 15));
    // the resulting month has fewer days
    evalEquals("Add_Years(Date '2020-02-29',1)", LocalDate.of(2021, Month.FEBRUARY, 28));
    evalNull("Add_Years(Null,140)");
    evalNull("Add_Years(Date '2019-01-15',Null)");
    evalFails("Add_Years(Date '2019-01-15')");
    evalFails("Add_Years()");
  }

  @Test
  public void Add_Months() throws Exception {
    evalEquals("Add_Months(Date '2019-01-15',1)", LocalDate.of(2019, Month.FEBRUARY, 15));
    evalEquals("Add_Months(Date '2019-01-15',-2)", LocalDate.of(2018, Month.NOVEMBER, 15));
    evalEquals("Add_Months(Date '2019-11-15',3)", LocalDate.of(2020, Month.FEBRUARY, 15));
    // the resulting month has fewer days
    evalEquals("Add_Months(Date '2019-01-31',1)", LocalDate.of(2019, Month.FEBRUARY, 28));
    evalNull("Add_Months(Null,140)");
    evalNull("Add_Months(Date '2019-01-15',Null)");
    evalFails("Add_Months(Date '2019-01-15')");
  }
  
  @Test
  public void Add_Weeks() throws Exception {
    evalEquals("Add_Weeks(Date '2019-01-15',1)", LocalDate.of(2019, Month.JANUARY, 22));
    evalEquals("Add_Weeks(Date '2019-01-15',-3)", LocalDate.of(2018, Month.DECEMBER, 25));
    evalNull("Add_Weeks(Null,140)");
    evalNull("Add_Weeks(Date '2019-01-15',Null)");
    evalFails("Add_Weeks(Date '2019-01-15')");
  }

  @Test
  public void Add_Days() throws Exception {
    evalEquals("Add_Days(Date '2019-01-15',1)", LocalDate.of(2019, Month.JANUARY, 16));
    evalEquals("Add_Days(Date '2019-01-15',-20)", LocalDate.of(2018, Month.DECEMBER, 26));
    evalNull("Add_Days(Null,140)");
    evalNull("Add_Days(Date '2019-01-15',Null)");
    evalFails("Add_Days(Date '2019-01-15')");
  }

  @Test
  public void Add_Hours() throws Exception {
    evalEquals("Add_Hours(Date '2019-01-15',1)",
        LocalDateTime.of(2019, Month.JANUARY, 15, 1, 0, 0, 0));
    evalNull("Add_Hours(Null,140)");
    evalNull("Add_Hours(Date '2019-01-15',Null)");
    evalFails("Add_Hours(Date '2019-01-15')");
  }

  @Test
  public void Add_Minutes() throws Exception {
    evalEquals("Add_Minutes(Date '2019-01-15',20)",
        LocalDateTime.of(2019, Month.JANUARY, 15, 0, 20, 0, 0));
    evalNull("Add_Minutes(Null,140)");
    evalNull("Add_Minutes(Date '2019-01-15',Null)");
    evalFails("Add_Minutes(Date '2019-01-15')");
  }

  @Test
  public void Add_Seconds() throws Exception {
    evalEquals("Add_Seconds(Date '2019-01-15',20)",
        LocalDateTime.of(2019, Month.JANUARY, 15, 0, 0, 20, 0));
    evalEquals("Add_Seconds(Date '2019-01-15',140)",
        LocalDateTime.of(2019, Month.JANUARY, 15, 0, 2, 20, 0));
    evalNull("Add_Seconds(Null,140)");
    evalNull("Add_Seconds(Date '2019-01-15',Null)");
    evalFails("Add_Seconds(Date '2019-01-15')");
  }

  @Test
  public void Hour() throws Exception {
    evalEquals("Hour(Timestamp '2019-01-01 15:28:59')", 15);
    evalNull("Hour(Null)");
    evalFails("Hour()");
  }

  @Test
  public void Minute() throws Exception {
    evalEquals("Minute(Timestamp '2019-01-01 15:28:59')", 28);
    evalNull("Minute(Null)");
    evalFails("Minute()");
  }

  @Test
  public void Second() throws Exception {
    evalEquals("Second(Timestamp '2019-01-01 15:28:59')", 59);
    evalNull("Second(Null)");
    evalFails("Second()");
  }

  @Test
  public void Lower() throws Exception {
    evalEquals("Lower('TesT')", "test");
    evalNull("Lower(NULL)");
    evalFails("Lower()");
    evalFails("Lower('Test','Test')");

    // Alias
    // evalEquals("LCase('TesT')", "test");
    
    returnType("Lower('TesT')", DataTypeName.STRING);
  }

  @Test
  public void Substring() throws Exception {
    evalEquals("Substring('TEST FROM',6)", "FROM");
    evalEquals("Substring('TEST FROM',6,2)", "FR");
    evalEquals("Substring('TEST FROM',1,4)", "TEST");
    evalEquals("Substring('TEST FROM',-4)", "FROM");
    evalEquals("Substring('ABCDEFG',1,1)", "A");

    // Compatibility mode
    evalEquals("Substring('ABCDEFG',0,1)", "A");

    // Alias
    evalEquals("Substr('TEST',5)", "");
    
    returnType("Substring('ABCDEFG',0,1)", DataTypeName.STRING);
  }

  @Test
  public void Split_part() throws Exception {
    evalEquals("Split_Part('127.1.2.3','.',1)", "127");
    evalEquals("Split_Part('127.1.2.3','.',2)", "1");
    evalEquals("Split_Part('127.1.2.3','.',4)", "3");    
    evalEquals("Split_Part('127.1.2.3','.',-1)", "3");
    evalEquals("Split_Part('127.1.2.3','.',-2)", "2");
    evalEquals("SPLIT_PART('user@hop.apache.org', '.', -2)","apache");
    evalEquals("Split_Part('AAA-@-BBB-BBB-@-CCC','-@-',2)", "BBB-BBB");
    
    // No split if delimiter is empty
    evalEquals("Split_Part('127.1.2.3','',1)", "127.1.2.3");
    
    // If the part index is out of range, the returned value is an empty string.
    evalEquals("Split_Part('127.1.2.3','.',5)","");
    
    evalNull("Split_Part(NULL,'.',5)");
    evalNull("Split_Part('127.1.2.3',NULL,5)");
    evalNull("Split_Part('127.1.2.3','.',NULL)");
    
    evalFails("Split_Part('127.1.2.3','.')");
  }
  
  @Test
  public void Strtok() throws Exception {
    evalEquals("Strtok('127.1-2-3','.-:',1)", "127");
    evalEquals("Strtok('127.1-2-3','.-:',2)", "1");
    evalEquals("Strtok('127.1-2-3','.-:',4)", "3");    
    evalEquals("Strtok('127.1-2-3','.-:',-1)", "3");
    evalEquals("Strtok('127.1-2-3','.-:',-2)", "2");
    evalEquals("Strtok('AAA.BBB:CCC-DDD','.-:',2)", "BBB");
    
    // Optional arguments
    evalEquals("Strtok('AAA BBB CCC DDD')", "AAA");
    evalEquals("Strtok('AAA BBB CCC DDD',3)", "CCC");
    evalEquals("Strtok('AAA:BBB-CCC DDD','-: ')", "AAA");
    
    // If the string starts or is terminated with the delimiter
    evalEquals("Strtok(' AAA:BBB-CCC DDD', 1)", "");
    evalEquals("Strtok('AAA:BBB-CCC DDD ', 3)", "");
    
    // Adjacent delimiters are treated as delimiters for empty tokens.
    evalEquals("Strtok('AAA  BBB CCC', 3)", "BBB");
    
    // No split if delimiter is empty
    evalEquals("Strtok('127.1.2.3','',1)", "127.1.2.3");
    
    // If the part index is out of range, the returned value is null.
    evalNull("Strtok('127.1.2.3','.',5)");    
    evalNull("Strtok('','',1)");
    
    // If one operands is null
    evalNull("Strtok(NULL,'.',5)");
    evalNull("Strtok('127.1.2.3',NULL,5)");
    evalNull("Strtok('127.1.2.3','.',NULL)");
    
    evalFails("Strtok()");
    evalFails("Strtok('127.1.2.3','.',5,5)");
    
    returnType("Strtok('AAA BBB CCC DDD')", DataTypeName.STRING);
  }
  
  @Test
  public void Space() throws Exception {
    evalEquals("Space(4)", "    ");
    evalEquals("Space(0)", "");
    evalNull("Space(-3)");
    evalNull("Space(NULL)");
    evalFails("Space()");
    evalFails("Space('str')");
    returnType("Space(4)", DataTypeName.STRING);
  }

  @Test
  public void Abs() throws Exception {
    evalEquals("Abs(0)", 0);
    evalEquals("Abs(1)", 1);
    evalEquals("Abs(-1)", 1);
    evalEquals("Abs(Price)", 5.12);
    evalEquals("Abs(-1::INTEGER)", 1);
    evalEquals("Abs(-1.12345679)", 1.12345679);
    evalEquals("Abs(-1.1234567912345679123456791234567912345679)",
        1.123456791234567912345679123456791234567912345679);
    evalNull("Abs(NULL)");
    evalFails("Abs()");

    writeEquals("ABS(-AGE)");
  }

  @Test
  public void Acos() throws Exception {
    evalEquals("Acos(0)", 1.5707963267948966);
    evalEquals("Acos(1)", 0);
    evalNull("Acos(NULL)");
    evalFails("Acos(2)");
    evalFails("Acos(-2)");
    evalFails("Acos()");
    returnType("Acos(0)", DataTypeName.NUMBER);
  }

  @Test
  public void Acosh() throws Exception {
    evalEquals("Acosh(1)", 0);
    evalEquals("Acosh(3)", 1.762747174039086);
    evalNull("Acosh(NULL)");
    evalFails("Acosh()");
  }

  @Test
  public void Asin() throws Exception {
    evalEquals("Asin(0)", 0);
    evalEquals("Asin(sin(0.5))", 0.5);
    evalNull("Asin(NULL)");
    evalFails("Asin()");
  }

  @Test
  public void Asinh() throws Exception {
    evalEquals("Asinh(asin(0.5))", 0.5022189850346116D);
    evalNull("Asinh(NULL)");
    evalFails("Asinh()");
  }

  @Test
  public void Atan() throws Exception {
    evalEquals("Atan(0.5)", 0.4636476090008061D);
    evalEquals("Atan(Tan(0.5))", 0.5);
    evalNull("Atan(NULL)");
    evalFails("Atan()");
  }

  @Test
  public void Atan2() throws Exception {
    evalEquals("Atan2(0,3)", 0);
    evalEquals("Atan2(0,-3)", Math.PI);
    evalNull("Atan2(NULL,0)");
    evalNull("Atan2(1,NULL)");
    evalFails("Atan2()");
    evalFails("Atan2(1)");
  }

  @Test
  public void Atanh() throws Exception {
    evalEquals("Atanh(0.2)", 0.2027325540540821D);
    evalNull("Atanh(NULL)");
    evalFails("Atanh()");
  }

  @Test
  public void Avg() throws Exception {    
    returnType("AVG(AGE)", DataTypeName.NUMBER);
  }  
  
  @Test
  public void Max() throws Exception {    
    returnType("MAX(NAME)", DataTypeName.STRING);
    returnType("MAX(AGE)", DataTypeName.INTEGER);
    returnType("MAX(DN)", DataTypeName.DATE);
  }  
  
  @Test
  public void Min() throws Exception {    
    returnType("MIN(NAME)", DataTypeName.STRING);
    returnType("MIN(AGE)", DataTypeName.INTEGER);
    returnType("MIN(DN)", DataTypeName.DATE);
  }  
  
  @Test
  public void Cos() throws Exception {
    evalEquals("Cos(1)", 0.5403023058681398);
    evalEquals("Cos(Pi())", -1);
    evalEquals("Cos(0)", 1.0);
    evalNull("Cos(NULL)");
    evalFails("Cos()");
    evalFails("Cos(0,1)");
    
  }

  @Test
  public void Cosh() throws Exception {
    evalEquals("Cosh(1.234)", 1.8630338016984225);
    evalEquals("Cosh(0)", 1.0);
    evalNull("Cosh(NULL)");
    evalFails("Cosh()");
    evalFails("Cosh(0,1)");
  }

  @Test
  public void Sin() throws Exception {
    evalEquals("Sin(1)", 0.8414709848078965);
    evalEquals("Sin(84.4)", 0.4104993826174394);
    evalEquals("Sin(0)", 0);
    evalNull("Sin(NULL)");
    evalFails("Sin()");
    evalFails("Sin(0,1)");
  }

  @Test
  public void Sinh() throws Exception {
    evalEquals("Sinh(84.4)", 2.2564425307671042E36);
    evalEquals("Sinh(0)", 0);
    evalNull("Sinh(NULL)");
    evalFails("Sinh()");
    evalFails("Sinh('test')");
    evalFails("Sinh(0,1)");
  }

  @Test
  public void Cot() throws Exception {
    evalEquals("Cot(1)", 0.6420926159343306);
    // evalEquals("Cot(0)", Double.POSITIVE_INFINITY);
    evalNull("Cot(NULL)");
    evalFails("Cot(0)");
    evalFails("Cot()");
    evalFails("Cot('test')");
    evalFails("Cot(1,0)");
  }

  @Test
  public void Csc() throws Exception {
    evalEquals("Csc(Pi()/2)", 1);
    
    evalNull("Csc(NULL)");
    evalFails("Csc(0)");
    evalFails("Csc()");
    evalFails("Csc('test')");
    evalFails("Csc(1,0)");
  }
  
  @Test
  public void Tan() throws Exception {
    evalEquals("Tan(84.4)", -0.45017764606194366D);
    evalEquals("Tan(0)", 0D);
    evalNull("Tan(NULL)");
    evalFails("Tan()");
    evalFails("Tan(0,1)");
  }

  @Test
  public void Tanh() throws Exception {
    evalEquals("Tanh(1.234)", 0.8437356625893302D);
    evalEquals("Tanh(0)", 0D);
    evalNull("Tanh(NULL)");
    evalFails("Tanh()");
    evalFails("Tanh(0,1)");
  }

  @Test
  public void Exp() throws Exception {
    evalEquals("Exp(1)", Math.E);
    evalEquals("Exp(2)", Math.E * Math.E);
    evalNull("Exp(NULL)");
    evalFails("Exp()");
    evalFails("Exp(1,2)");
  }

  @Test
  public void Power() throws Exception {
    evalEquals("Power(3,2)", 9D);
    evalEquals("Power(100,0.5)", 10D);
    evalEquals("Power(-4,2)", 16D);
    evalEquals("Power(0,0)", 1D);
    evalEquals("Power(999,0)", 1D);
    evalEquals("Power(2,2.5)", 5.656854249492381D);
    evalEquals("Power(2,-3)", 0.125D);   
    evalNull("Power(NULL,2)");
    evalNull("Power(3,NULL)");
    evalFails("Power()");
    evalFails("Power(3)");
    evalFails("Power(1,2,3)");
  }
  
  @Test
  public void Sign() throws Exception {
    evalEquals("Sign(0.3)", 1L);
    evalEquals("Sign(0)", 0L);
    evalEquals("Sign(-5)", -1L);
    evalFails("Sign()");
    evalFails("Sign(1,2)");
    evalNull("Sign(NULL)");
  }

  @Test
  public void Cbrt() throws Exception {
    evalEquals("Cbrt(0)", 0);
    evalEquals("Cbrt(2)", 1.2599210498948732D);
    evalEquals("Cbrt(-343)", -7);
    evalNull("Cbrt(NULL)");
    evalFails("Cbrt()");
  }

  @Test
  public void Sqrt() throws Exception {
    evalEquals("Sqrt(9)", 3);
    evalFails("Sqrt(-5)");
    evalFails("Sqrt()");
    evalNull("Sqrt(NULL)");
  }

  @Test
  public void Square() throws Exception {
    evalEquals("Square(1)", 1);
    evalEquals("Square(-5)", 25);
    evalFails("Square()");
    evalNull("Square(NULL)");
  }

  @Test
  public void Trim() throws Exception {
    evalEquals("Trim('a')", "a");
    evalEquals("Trim(' a ')", "a");
    evalEquals("Trim('  a b  ')", "a b");
    evalEquals("Trim('01ABC10 ', '012')", "ABC10 ");
    evalEquals("Trim(' 01ABC10 ', ' 012')", "ABC");
    evalNull("Trim(NULL)");
    evalNull("Trim(' 01ABC012 ',NULL)");
    evalFails("Trim()");
    
    returnType("Trim(NAME)", DataTypeName.STRING);
  }

  @Test
  public void LTrim() throws Exception {
    evalEquals("LTrim('a')", "a");
    evalEquals("LTrim(' a ')", "a ");
    evalEquals("LTrim('01ABC012', '012')", "ABC012");
    evalNull("LTrim(NULL)");
    evalNull("LTrim('01ABC012',NULL)");
    evalFails("LTrim()");
    
    returnType("LTrim(NAME)", DataTypeName.STRING);
  }

  @Test
  public void RTrim() throws Exception {
    evalEquals("RTrim('a')", "a");
    evalEquals("RTrim(' a ')", " a");
    evalEquals("RTrim('012ABC10', '012')", "012ABC");
    evalNull("RTrim(NULL)");
    evalNull("RTrim('01ABC012',NULL)");
    evalFails("RTrim()");
    
    returnType("RTrim(NAME)", DataTypeName.STRING);
  }

  @Test
  public void Greatest() throws Exception {
    evalEquals("Greatest(5,2,null,9,4)", 9);
    evalEquals("Greatest('B','A','C')", "C");
    evalEquals("Greatest(0x12,0x1F,0x0A)", new byte[] {0x1F});

    evalEquals("Greatest(Date '2020-01-01',Date '2021-12-06',Date '1990-12-08')",
        LocalDate.of(2021, 12, 6));
    evalTrue("Greatest(false,true,false)");
    evalFalse("Greatest(false,false,false)");

    // Datatype mixte
    evalFails("Greatest(123,'str',123)");
        
    returnType("Greatest(NAME,'st','bf')", DataTypeName.STRING);
    returnType("Greatest(123,456,789)", DataTypeName.BIGNUMBER);
  }

  @Test
  public void Least() throws Exception {
    evalEquals("Least(5,2,null,9,4)", 2);
    evalEquals("Least('B','A','C')", "A");
    evalEquals("Least(0x12,0x1F,0x0A)", new byte[] {0x0A});
    evalEquals("Least(Date '2020-01-01',Date '2021-12-06',Date '1990-12-08')",
        LocalDate.of(1990, 12, 8));
    evalFalse("Least(false,true,false)");
    evalTrue("Least(true,true,true)");
    
    // Datatype mixte
    evalFails("Least(123,'str',123)");
  }

  @Test
  public void Length() throws Exception {
    // String
    evalEquals("Length('TEST')", 4);

    // Binary
    evalEquals("Length(0xF0FA)", 2);
    evalEquals("Length(0x0F0FA)", 3);

    evalNull("Length(null)");
    evalFails("Length()");
    
    returnType("Length(NAME)", DataTypeName.INTEGER);
  }

  @Test
  public void Left() throws Exception {
    evalEquals("Left('TEST FROM',4)", "TEST");
    evalEquals("Left('',1)", "");
    evalEquals("Left('TEST',10)", "TEST");
    evalEquals("Left('TEST',-1)", "");
    evalEquals("Left(0x1234567890, 2)", new byte[] {0x12, 0x34});
    evalNull("Left(NULL,4)");
    evalNull("Left('TEST',NULL)");
    evalFails("Left()");
  }

  @Test
  public void Insert() throws Exception {
    evalEquals("Insert('abcd', 2, 1, 'qw')", "aqwcd");
    evalEquals("Insert('abcdefg', 1, 9, 'zy')", "zy");
    evalEquals("Insert(0x1234, 2, 0, 0x56)", new byte[] {0x12, 0x56, 0x34});
    evalEquals("Insert(0x1234, 0, 0, 0x56)", new byte[] {0x56, 0x12, 0x34});
    evalNull("Insert(NULL, 2, 1, 'qw')");
    evalNull("Insert('abcd', NULL, 1, 'qw')");
    evalNull("Insert('abcd', 2, NULL, 'qw')");
    evalNull("Insert('abcd', 2, 1, NULL)");
    evalFails("Insert()");
  }

  @Test
  public void Right() throws Exception {
    evalEquals("Right('TEST FROM',4)", "FROM");
    evalEquals("Right('',1)", "");
    evalEquals("Right('TEST',10)", "TEST");
    evalEquals("Right('TEST',-1)", "");
    evalEquals("Right(0x1234567890, 2)", new byte[] {0x78, (byte) 0x90});
    evalNull("Right(NULL,4)");
    evalNull("Right('TEST',NULL)");
    evalFails("Right()");
  }

  @Test
  public void Repeat() throws Exception {
    evalEquals("Repeat('ABCD',3)", "ABCDABCDABCD");
    evalEquals("Repeat('ABCDEFCD',0)", "");
    evalEquals("Repeat(0x1234,3)", new byte[] {0x12, 0x34, 0x12, 0x34, 0x12, 0x34});
    evalNull("Repeat(NULL,2)");
    evalNull("Repeat('ABCD',NULL)");
    evalFails("Repeat()");
  }

  @Test
  public void Replace() throws Exception {
    evalEquals("Replace('ABCD','CD')", "AB");
    evalEquals("Replace('ABCDEFCD','CD','EF')", "ABEFEFEF");
    evalNull("Replace(NULL,'CD','EF')");
    evalNull("Replace('ABCD',NULL,'EF')");
    evalFails("Replace()");
  }

  @Test
  public void To_Boolean() throws Exception {
    evalTrue("To_Boolean('True')");
    evalTrue("To_Boolean('t')");
    evalTrue("To_Boolean('yes')");
    evalTrue("To_Boolean('on')");
    evalTrue("To_Boolean('1')");
    evalTrue("To_Boolean(5)");
    evalTrue("To_Boolean(2.3)");
    evalTrue("To_Boolean(-1)");

    evalFalse("To_Boolean('False')");
    evalFalse("To_Boolean('off')");
    evalFalse("To_Boolean('NO')");
    evalFalse("To_Boolean('F')");
    evalFalse("To_Boolean('n')");
    evalFalse("To_Boolean('0')");
    evalFalse("To_Boolean(0)");

    evalNull("To_Boolean(NULL)");

    evalFails("To_Boolean()");
    evalFails("To_Boolean('falsee')");
    evalFails("To_Boolean(1,2,3)");
  }

  @Test
  public void To_Number() throws Exception {

    // No precision/scale and no format
    evalEquals("TO_NUMBER('12.3456')", 12.3456);
    evalEquals("TO_NUMBER('98.76546')", 98.76546);
    evalEquals("TO_NUMBER('1.2E3')", 1200D);
    evalEquals("TO_NUMBER('1.2E-3')", 0.0012D);

    // Format with Decimals
    evalEquals("TO_NUMBER('5467.12', '999999.99')", 5467.12);
    evalEquals("TO_NUMBER('1234.5','09999.99')", 1234.5);
    Locale.setDefault(new Locale("en", "EN"));
    evalEquals("TO_NUMBER('5467.12', '999999D99')", 5467.12);
    Locale.setDefault(new Locale("fr", "BE"));
    evalEquals("TO_NUMBER('5467,12', '999999D99')", 5467.12);

    // Format No Decimals
    evalEquals("TO_NUMBER('4687841', '9999999')", 4687841D);

    // Trailing space
    evalEquals("TO_NUMBER('   5467.12', '999999.99')", 5467.12);

    // No sign
    evalEquals("TO_NUMBER('+0.1','99.99')", 0.1);
    evalEquals("TO_NUMBER('-0.2','99.99')", -0.2);
    evalEquals("TO_NUMBER(' -0.2','99.99')", -0.2);
    evalEquals("TO_NUMBER(' .2','99.99')", 0.2);  
    
    // Sign S_ and _S
    evalEquals("TO_NUMBER('-0.2','S99.99')", -0.2);
    evalEquals("TO_NUMBER('0.3-','99.99S')", -0.3);
    evalEquals("TO_NUMBER('0.3-','99.99s')", -0.3);

    // Sign MI_ and _MI
    evalEquals("TO_NUMBER('0.4-','99.99MI')", -0.4);
    evalEquals("TO_NUMBER('0.4-','99.99mi')", -0.4);
    evalEquals("TO_NUMBER('0.4 -','99.99mi')", -0.4);
    evalEquals("TO_NUMBER(' 0.4 -','99.99mi')", -0.4);
    evalEquals("TO_NUMBER('-   4','MI9999')", -4);
    evalEquals("TO_NUMBER('-4','MI9999')", -4);

    // Sign PR (format element can appear only in the last position of a number format model.)
    evalEquals("TO_NUMBER(' 0.5 ','99.99PR')", 0.5);
    evalEquals("TO_NUMBER('<0.5>','99.99PR')", -0.5);
    evalFails("TO_NUMBER('-5','PR9999')");

    // Format with Thousand Group Markers
    Locale.setDefault(new Locale("en", "US"));
    evalEquals("TO_NUMBER('12,345,678', '999,999,999')", 12_345_678);
    Locale.setDefault(new Locale("fr", "BE"));
    // Fail with sonar build
    // evalEquals("TO_NUMBER('12.345.678', '999G999G999')", 12_345_678);
    // evalEquals("TO_NUMBER('12.345.678,123', '999G999G999D000')", 12_345_678.123);

    // Format with Currency dollar
    Locale.setDefault(new Locale("en", "US"));
    evalEquals("TO_NUMBER('$65.169', '$99.999')", 65.169);
    Locale.setDefault(new Locale("fr", "BE"));
    evalEquals("TO_NUMBER('$65.169', '$99.999')", 65.169);

    // Format with Currency symbol
    Locale.setDefault(new Locale("en", "US"));
    evalEquals("TO_NUMBER('$65.169', 'L99.999')", 65.169);
    Locale.setDefault(new Locale("fr", "BE"));
    evalEquals("TO_NUMBER('€65.169', 'L99.999')", 65.169);
    evalEquals("TO_NUMBER('65.16€', '99.999L')", 65.16);

    // Format with Currency code
    Locale.setDefault(new Locale("en", "US"));
    evalEquals("TO_NUMBER('USD65.169', 'C99.999')", 65.169);
    Locale.setDefault(new Locale("fr", "BE"));
    evalEquals("TO_NUMBER('EUR65.169', 'C99.999')", 65.169);
    evalEquals("TO_NUMBER('65.16EUR', '99.999C')", 65.16);

    // Format Hex
    evalEquals("TO_NUMBER('ABCD','FMXXXX')", 43981);

    // Format Roman numeral
    evalEquals("TO_NUMBER('DXV','RN')", 515);
    evalEquals("TO_NUMBER('CDLXXXV','RN')", 485);

    evalEquals("TO_NUMBER('MCMXCIX','rn')", 1999);
    evalEquals("TO_NUMBER('MMMDCCXXIV','rn')", 3724);

    // Parse multi format
    evalEquals("TO_NUMBER('1234-','MI9999|9999MI')", -1234);

    evalNull("TO_NUMBER(NULL)");

    // You can specify only one decimal separator in a number format model.
    evalFails("TO_NUMBER('123.456','9D999D9')");
    evalFails("TO_NUMBER('123.456','9.999.9')");

    // A group separator cannot appear to the right of a decimal character or period in a number
    // format model.
    evalFails("TO_NUMBER('-0.2','999.999G99')");
    evalFails("TO_NUMBER('-0.2','999.999,99')");
  }

  @Test
  public void To_Char() throws Exception {
    // Text
    evalNull("TO_CHAR(NULL)");

    // Default format
    evalEquals("TO_CHAR(0.45)", ".45");
    evalEquals("TO_CHAR(12923)", "12923");

    // Format fixed length with decimal
    Locale.setDefault(new Locale("en", "EN"));
    evalEquals("TO_CHAR(0.1,'90.99')", "  0.1 ");
    evalEquals("TO_CHAR(-0.2,'90.90')", " -0.20");
    evalEquals("TO_CHAR(0,'90.99')", "  0.  ");
    evalEquals("TO_CHAR(0,'90D99')", "  0.  ");
    evalEquals("TO_CHAR(0,'90d99')", "  0.  ");
    evalEquals("TO_CHAR(0,'99.99')", "  0.  ");
    evalEquals("TO_CHAR(0,'9999')", "    0");
    evalEquals("TO_CHAR(0,'9999999')", "       0");
    evalEquals("TO_CHAR(0,'0999')", " 0000");
    evalEquals("TO_CHAR(-0.5, '90.99')", " -0.5 ");
    evalEquals("TO_CHAR(0.003,'0.999')", " 0.003");
    evalEquals("TO_CHAR(12,'99')", " 12");
    evalEquals("TO_CHAR(-7,'99')", " -7");
    evalEquals("TO_CHAR(12923,'99,999.00')", " 12,923.00");
    evalEquals("TO_CHAR(12,'9990999.9')", "    0012. ");
    evalEquals("TO_CHAR(0.3,'99.00000')", "   .30000");
    evalEquals("TO_CHAR(0.3,'00.00')", " 00.30");
    evalEquals("TO_CHAR(12923,'FM99999.99')", "12923.");
    evalEquals("TO_CHAR(12923,'FM9,9,9,9,9')", "1,2,9,2,3");
    evalEquals("TO_CHAR(0.3,'FM00.99')", "00.3");
    
    // Blanks for the integer part of a fixed-point number when the integer part is zero
    evalEquals("TO_CHAR(-0.2,'99.90')", "  -.20");
    evalEquals("TO_CHAR(-0.2,'99.99')", "  -.2 ");
    
    //evalEquals("TO_CHAR(485.8, '\"Pre:\"999\" Post:\" .999')", "Pre: 485 Post: .800");

    evalEquals("TO_CHAR(12345.567,'9,999')", "######");
    evalEquals("TO_CHAR(1234.94,'9999MI')", "1234 ");
    evalEquals("TO_CHAR(555.0, 'FM999.009')", "555.00");
    Locale.setDefault(new Locale("fr", "BE"));
    evalEquals("TO_CHAR(0,'90.99')", "  0.  ");
    evalEquals("TO_CHAR(0,'90D99')", "  0,  ");
    evalEquals("TO_CHAR(0,'90d00')", "  0,00");

    // Format fixed length with grouping
    Locale.setDefault(new Locale("en", "EN"));
    evalEquals("TO_CHAR(1485,'9,999')", " 1,485");
    Locale.setDefault(new Locale("fr", "FR"));
   // evalEquals("TO_CHAR(3148.5, '9G999D999')", " 3.148,5 ");
   // evalEquals("TO_CHAR(3148.5, '9g999d990')", " 3.148,500");

    // Sign
    evalEquals("TO_CHAR(12,'S99')", "+12");
    evalEquals("TO_CHAR(12,'99S')", "12+");
    evalEquals("TO_CHAR(-7,'S99')", " -7");
    evalEquals("TO_CHAR(-7,'99S')", " 7-");
    evalEquals("TO_CHAR(-7,'99s')", " 7-");

    evalEquals("TO_CHAR(12,'99MI')", "12 ");
    evalEquals("TO_CHAR(7,'99MI')", " 7 ");
    evalEquals("TO_CHAR(-7,'99MI')", " 7-");
    evalEquals("TO_CHAR(-7,'MI99')", "- 7");
    // FM affect the trailing blank added by the MI suffix.
    evalEquals("TO_CHAR(485,'FMMI999')", "485");
    evalEquals("TO_CHAR(485,'FM999MI')", "485");


    evalEquals("TO_CHAR(7,'9999pr')", "    7 ");
    evalEquals("TO_CHAR(-7,'9999PR')", "   <7>");
    evalEquals("TO_CHAR(7,'FM9999PR')", "7");
    evalFails("TO_CHAR(-7,'PR9999')");

    // Currency dollar
    evalEquals("TO_CHAR(12,'$99')", " $12");
    evalEquals("TO_CHAR(-7,'$99')", " -$7");
    evalEquals("TO_CHAR(-7,'99$')", " -$7");
    evalEquals("TO_CHAR(124,'$99')", "####");
    evalEquals("TO_CHAR(124,'FM$99')", "###");

    // Currency code ISO 4217
    Locale.setDefault(new Locale("en", "GB"));
    evalEquals("TO_CHAR(12,'C99')", " GBP12");
    Locale.setDefault(new Locale("en", "US"));
    evalEquals("TO_CHAR(-7,'C99')", " -USD7");
    Locale.setDefault(new Locale("fr", "BE"));
    evalEquals("TO_CHAR(-77,'99C')", "-77EUR");
    Locale.setDefault(new Locale("et", "EE"));
    evalEquals("TO_CHAR(-7,'99C')", " -7EUR");
    evalEquals("TO_CHAR(0,'FMC')", "EUR"); // Only currency ISO code

    // Currency symbol
    Locale.setDefault(new Locale("en", "GB"));
    evalEquals("TO_CHAR(12,'FML99')", "£12");
    Locale.setDefault(new Locale("fr", "BE"));
    evalEquals("TO_CHAR(-7,'L99')", " -€7");
    evalEquals("TO_CHAR(-7,'99L')", " -7€");
    evalEquals("TO_CHAR(123.45,'L999.99')", " €123.45");
    evalEquals("TO_CHAR(123.45,'FML999.99')", "€123.45");
    evalEquals("TO_CHAR(0,'FML')", "€"); // Only symbol

    // Text minimum
    evalEquals("TO_CHAR(123.456,'TM')", "123.456");
    evalEquals("TO_CHAR(123.456,'tm')", "123.456");
    evalEquals("TO_CHAR(123.456,'tm9')", "123.456");
    evalEquals("TO_CHAR(123.456,'TME')", "1.23456E+02");
    evalEquals("TO_CHAR(123.456,'tMe')", "1.23456e+02");
    evalEquals("TO_CHAR(123.456,'tMe')", "1.23456e+02");

    // Scientific
    evalEquals("TO_CHAR(123.456,'9.9EEEE')", "  1.2E+02");

    // Roman
    evalEquals("TO_CHAR(11,'FMRN')", "XI");
    evalEquals("TO_CHAR(11,'rn')", "             xi");
    evalEquals("TO_CHAR(5.2, 'FMRN')", "V");
    evalEquals("TO_CHAR(485, 'FMRN')", "CDLXXXV");
    evalEquals("TO_CHAR(515, 'RN')", "            DXV");
    evalFails("TO_CHAR(0, 'RN')"); // Must be > 0
    evalFails("TO_CHAR(4000, 'RN')"); // Must be < 4000
     
    // Hex
    evalEquals("TO_CHAR(123,'XX')", " 7B");
    evalEquals("TO_CHAR(123,'xx')", " 7b");
    evalEquals("TO_CHAR(123,'0XXX')", " 007B");
    evalEquals("TO_CHAR(123,'FM0XXX')", "007B");
    evalEquals("TO_CHAR(9234,'xx')", "###");

    // No space
    evalFails("TO_CHAR(485,'9 9 9')");

    // Date
    evalEquals("To_Char(Date '2019-07-23','AD')", "AD");
    evalEquals("To_Char(Date '2019-07-23','BC')", "AD");
    evalEquals("To_Char(Date '2019-07-23','Bc')", "Ad");
    evalEquals("To_Char(Date '2019-07-23','bc')", "ad");
    evalEquals("To_Char(Date '2019-07-23','A.D.')", "A.D.");
    evalEquals("To_Char(Date '2019-07-23','B.C.')", "A.D.");
    evalEquals("To_Char(Date '2019-07-23','B.c.')", "A.d.");
    evalEquals("To_Char(Date '2019-07-23','b.c.')", "a.d.");
    evalEquals("To_Char(Date(-10,07,23),'b.c.')", "b.c.");

    // Punctuation is reproduced in the result
    evalEquals("To_Char(Date '2019-07-23','dd/mm/yyyy')", "23/07/2019");
    evalEquals("To_Char(Date '2019-07-23','dd.mm.yyyy')", "23.07.2019");
    evalEquals("To_Char(Date '2019-07-23',':dd,mm;yyyy')", ":23,07;2019");

    // Quoted text is reproduced in the result
    evalEquals("To_Char(Date '2019-07-23','dd\"text\"mmyyyy')", "23text072019");

    evalEquals("To_Char(Date '2019-07-23','CC')", "21");
    evalEquals("To_Char(Date '2000-07-23','CC')", "20");
    evalEquals("To_Char(Date '2019-07-23','SCC')", " 21");
    evalEquals("To_Char(Date '2000-07-23','SCC')", " 20");
    evalEquals("To_Char(Date '2000-07-23','FMSCC')", "20");
    evalEquals("To_Char(To_Date('-0200','SYYYY'),'SCC')", "-02");
    evalEquals("To_Char(To_Date('-0200','SYYYY'),'FMSCC')", "-2");


    evalEquals("To_Char(Date '2122-01-01','YEAR')", "TWENTY-ONE TWENTY-TWO");
    evalEquals("To_Char(Date '2021-01-01','YEAR')", "TWENTY TWENTY-ONE");
    evalEquals("To_Char(Date '2020-01-01','YEAR')", "TWENTY TWENTY");
    evalEquals("To_Char(Date '1999-01-01','YEAR')", "NINETEEN NINETY-NINE");
    evalEquals("To_Char(Date '1900-01-01','YEAR')", "NINETEEN HUNDRED");
    evalEquals("To_Char(Date '1830-01-01','YEAR')", "EIGHTEEN THIRTY");
    evalEquals("To_Char(Date '2020-01-01','year')", "twenty twenty");
    evalEquals("To_Char(Date '2020-01-01','syear')", " twenty twenty");
    evalEquals("To_Char(Date '2020-01-01','FMsyear')", "twenty twenty");
    evalEquals("To_Char(To_Date('-1999','SYYYY'),'SYEAR')", "-NINETEEN NINETY-NINE");
    evalEquals("To_Char(To_Date('-0228','SYYYY'),'SYEAR')", "-TWO TWENTY-EIGHT");

    evalEquals("To_Char(Date '2019-01-01','YYYY')", "2019");
    evalEquals("To_Char(Date '0800-01-01','YYYY')", "0800");
    evalEquals("To_Char(Date '0800-01-01','FMYYYY')", "800"); // Year compact

    evalEquals("To_Char(Date '2019-01-01','YYY')", "019");
    evalEquals("To_Char(Date '2019-01-01','YY')", "19");
    evalEquals("To_Char(Date '2019-01-01','Y')", "9");
    evalEquals("To_Char(Date '2019-01-01','SYYYY')", " 2019");
    evalEquals("To_Char(To_Date('-2000','SYYYY'),'YYYY BC')", "2000 BC");
    evalEquals("To_Char(To_Date('-800','SYYYY'),'SYYYY')", "-0800"); // Negative signed year
    evalEquals("To_Char(To_Date('-800','SYYYY'),'YYYY BC')", "0800 BC");
    evalEquals("To_Char(Date '0800-07-23','FMSYYYY')", "800"); // Signed year compact
    evalEquals("To_Char(To_Date('-800','SYYYY'),'FMSYYYY BC')", "-800 BC"); // Negative signed year
                                                                            // compact

    evalEquals("To_Char(Date '2019-07-23','Q')", "3");

    evalEquals("To_Char(Date '2019-07-23','MM')", "07"); // Month number
    evalEquals("To_Char(Date '2019-07-23','FMMM')", "7"); // Month number compact
    evalEquals("To_Char(Date '2019-07-23','Month')", "July     ");
    evalEquals("To_Char(Date '2019-07-23','MONTH')", "JULY     ");
    evalEquals("To_Char(Date '2019-07-23','FMMONTH')", "JULY");
    evalEquals("To_Char(Date '2019-09-23','month')", "september");
    evalEquals("To_Char(Date '2019-09-23','MON')", "SEP");
    evalEquals("To_Char(Date '2019-09-23','Mon')", "Sep");
    evalEquals("To_Char(Date '2019-09-23','mon')", "sep");

    // Week of year and ISO Week of year (The first week of the ISO year is the week that contains
    // January 4.)
    evalEquals("To_Char(Date '2015-12-31','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2015-12-31 thu IYYY=2015 IW=53 WW=53");
    evalEquals("To_Char(Date '2016-01-01','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-01 fri IYYY=2015 IW=53 WW=01");
    evalEquals("To_Char(Date '2016-01-02','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-02 sat IYYY=2015 IW=53 WW=01");
    evalEquals("To_Char(Date '2016-01-03','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-03 sun IYYY=2015 IW=53 WW=01");
    evalEquals("To_Char(Date '2016-01-04','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-04 mon IYYY=2016 IW=01 WW=01");
    evalEquals("To_Char(Date '2016-01-05','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-05 tue IYYY=2016 IW=01 WW=01");
    evalEquals("To_Char(Date '2016-01-06','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-06 wed IYYY=2016 IW=01 WW=01");
    evalEquals("To_Char(Date '2016-01-07','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-07 thu IYYY=2016 IW=01 WW=01");
    evalEquals("To_Char(Date '2016-01-08','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-08 fri IYYY=2016 IW=01 WW=02");
    evalEquals("To_Char(Date '2042-12-31','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2042-12-31 wed IYYY=2043 IW=01 WW=53");
    evalEquals("To_Char(Date '2016-01-04','FM\"IW=\"IW \"WW=\"WW')", "IW=1 WW=1"); // Compact


    evalEquals("To_Char(Date '2019-07-21','D')", "1"); // Day of week
    evalEquals("To_Char(Date '2019-07-23','D')", "3"); // Day of week
    evalEquals("To_Char(Date '2019-07-08','DD')", "08"); // Day of month
    evalEquals("To_Char(Date '2019-07-08','FMDD')", "8"); // Day of month compact
    evalEquals("To_Char(Date '2019-07-23','DD')", "23"); // Day of month
    evalEquals("To_Char(Date '2019-02-23','DDD')", "054"); // Day of year
    evalEquals("To_Char(Date '2019-02-23','FMDDD')", "54"); // Day of year compact

    evalEquals("To_Char(Date '2019-07-23','DAY')", "TUESDAY  "); // Day name
    evalEquals("To_Char(Date '2019-07-23','Day')", "Tuesday  "); // Day name
    evalEquals("To_Char(Date '2019-07-23','day')", "tuesday  "); // Day name
    evalEquals("To_Char(Date '2019-07-23','fmDay')", "Tuesday"); // Day name compact
    evalEquals("To_Char(Date '2019-07-23','DY')", "TUE"); // Day name
    evalEquals("To_Char(Date '2019-07-23','Dy')", "Tue"); // Day name
    evalEquals("To_Char(Date '2019-07-23','dy')", "tue"); // Day name

    // evalEquals("To_Char(TIMESTAMP '2020-12-03 01:02:03.123456789','yyyy-mm-dd hh:mi:ss.FF')",
    // "2020-12-03 01:02:03.123456789");

    // Time Zone Region
    evalEquals("To_Char(Date '2019-07-23','TZR')", "UTC");
    // Time zone region abbreviated with Daylight Saving Time
    evalEquals("To_Char(Date '2019-07-23','TZD')", "UTC");

    // Time Zone Hour:Minute
    evalEquals("To_Char(Date '2019-07-23','TZH:TZM')", "+00:00");

    // Time
    evalEquals("To_Char(Timestamp '2019-02-13 15:34:56','HH:MI:SS')", "03:34:56");
    // Time 12 hours
    evalEquals("To_Char(Timestamp '2019-02-13 03:34:56','HH12:MI:SS AM')", "03:34:56 AM");
    evalEquals("To_Char(Timestamp '2019-02-13 15:34:56','HH12:MI:SS AM')", "03:34:56 PM");
    // Time 24 hours
    evalEquals("To_Char(Timestamp '2019-02-13 15:34:56.123','HH24:MI:SS')", "15:34:56");
    // Time fraction
    evalEquals("To_Char(Timestamp '2019-02-13 15:34:56.123','HH24:MI:SS.FF3')", "15:34:56.123");
    evalEquals("To_Char(Timestamp '2019-02-13 15:34:56.123456','HH24:MI:SS.FF6')",
        "15:34:56.123456");
    evalEquals("To_Char(Timestamp '2019-02-13 15:34:56.123456789','HH24:MI:SS.FF9')",
        "15:34:56.123456789");

    Locale.setDefault(new Locale("fr", "BE"));
    evalEquals("To_Char(Date '2019-07-23','DS')", "07/23/2019"); // Date short
    evalEquals("To_Char(Date '2019-07-23','DL')", "mardi 23 juillet 2019 à 0 h 00 min 00 s Temps universel coordonné"); // Date long
   // evalEquals("To_Char(Timestamp '2019-07-23 14:52','TS')", "14:52:00 PM"); // Time short

    evalEquals("To_Char(Date '2019-07-23','J')", "2458688");
    evalEquals("To_Char(Date '0001-01-01','J')", "1721426");

    // Special char
    evalEquals("To_Char(Date '2019-07-23',':;.,=-/(FMMONTH)')", ":;.,=-/(JULY)");

    // Special char
    evalFails("To_Char(Date '2019-07-23','*')");
    evalFails("To_Char(Date '2019-07-23','£')");
    evalFails("To_Char(Date '2019-07-23','{}[]')");
    
    evalFails("TO_CHAR('abc')");
  }

  @Test
  public void To_Date() throws Exception {
    evalEquals("To_Date('2019-02-13','YYYY-MM-DD')", LocalDate.of(2019, 2, 13));
    evalEquals("To_Date('2020:148','YYYY:DDD')", LocalDate.of(2020, 5, 27));
    evalEquals("To_Date('2020-08','YYYY-MM')", LocalDate.of(2020, 8, 1));
    evalEquals("To_Date('2020-MarCH','YYYY-MONTH')", LocalDate.of(2020, Month.MARCH, 1));
    evalEquals("To_Date('2020,feb,25','YYYY,MON,DD')", LocalDate.of(2020, Month.FEBRUARY, 25));
    evalEquals("To_Date('2019-02-13 15:34:56','YYYY-MM-DD HH:MI:SS')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 3, 34, 56));
    evalEquals("To_Date('2019-02-13 15:34:56','YYYY-MM-DD HH12:MI:SS')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 3, 34, 56));
    evalEquals("To_Date('2019-02-13 15:34:56','YYYY-MM-DD HH24:MI:SS')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 15, 34, 56));

    // Separator T
    evalEquals("To_Date('2019-02-13T15:34:56','YYYY-MM-DD HH24:MI:SS')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 15, 34, 56));
    evalEquals("To_Date('2019-02-13T15:34:56','YYYY-MM-DD\"T\"HH24:MI:SS')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 15, 34, 56));

    evalEquals("To_Date('01/02/2020','DD/MM/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));
    evalEquals("To_Date('01/II/2020','DD/RM/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));

    evalEquals("To_Date('01/02/-100','DD/MM/SYYYY')", LocalDate.of(-100, 2, 1));

    evalEquals("To_Date('01/02/10','DD/MM/YY')", LocalDate.of(2010, 2, 1));
    evalEquals("To_Date('01/02/50','DD/MM/YY')", LocalDate.of(2050, 2, 1));
    evalEquals("To_Date('01/02/80','DD/MM/YY')", LocalDate.of(1980, 2, 1));
    
    ExpressionContext context = createExpressionContext();
    context.setVariable(ExpressionContext.EXPRESSION_TWO_DIGIT_YEAR_START, "1970");
    evalEquals("To_Date('01/02/69','DD/MM/YY')", LocalDate.of(2069, 2, 1), context);
    context.setVariable(ExpressionContext.EXPRESSION_TWO_DIGIT_YEAR_START, "1970");
    evalEquals("To_Date('01/02/70','DD/MM/YY')", LocalDate.of(1970, 2, 1), context);
    context.setVariable(ExpressionContext.EXPRESSION_TWO_DIGIT_YEAR_START, "2000");
    evalEquals("To_Date('01/02/80','DD/MM/YY')", LocalDate.of(2080, 2, 1), context );       
    
    // TO VERIFY
    evalEquals("To_Date('01-jan-4710bc','dd-mon-yyyybc')", LocalDate.of(-4709, 1, 1));

    // Time zone offset
    evalEquals("To_Date('2019-02-13 15:34:56 +08:00','YYYY-MM-DD HH24:MI:SS TZH:TZM')",
        ZonedDateTime.of(2019, 2, 13, 15, 34, 56, 0, ZoneOffset.ofHours(8)));
    evalEquals("To_Date('2019-02-13 15:34:56 +8:00','YYYY-MM-DD HH24:MI:SS TZH:TZM')",
        ZonedDateTime.of(2019, 2, 13, 15, 34, 56, 0, ZoneOffset.ofHours(8)));
    evalEquals("To_Date('2019-02-13 15:34:56 -04:00','YYYY-MM-DD HH24:MI:SS TZH:TZM')",
        ZonedDateTime.of(2019, 2, 13, 15, 34, 56, 0, ZoneOffset.ofHours(-4)));

    // Time zone region
    evalEquals("To_Date('2019-02-13 15:34:56 US/Pacific','YYYY-MM-DD HH24:MI:SS TZR')",
        ZonedDateTime.of(2019, 2, 13, 15, 34, 56, 0, ZoneId.of("US/Pacific")));
    evalEquals("To_Date('Europe/Paris 2019-02-13 15:34:56','TZR YYYY-MM-DD HH24:MI:SS')",
        ZonedDateTime.of(2019, 2, 13, 15, 34, 56, 0, ZoneId.of("Europe/Paris")));

    // Trailing space
    evalEquals("To_Date('  2020-08','YYYY-MM')", LocalDate.of(2020, 8, 1));

    evalEquals("To_Date('01/2/0001','DD/MM/RRRR')", LocalDate.of(2001, Month.FEBRUARY, 1));
    evalEquals("To_Date('01/2/52','DD/MM/RRRR')", LocalDate.of(1952, Month.FEBRUARY, 1));
    evalEquals("To_Date('01/2/0923','DD/MM/RRRR')", LocalDate.of(923, Month.FEBRUARY, 1));

    // Month and day shorter than format
    evalEquals("To_Date('2020/2/1','YYYY/MM/DD')", LocalDate.of(2020, Month.FEBRUARY, 1));

    // If single elements are omitted, then their minimum values are assumed
    evalEquals("To_Date('2020-12','YYYY-MM')", LocalDate.of(2020, Month.DECEMBER, 1));
    evalEquals("To_Date('2020-02','YYYY-DD')", LocalDate.of(2020, Month.JANUARY, 2));
    evalEquals("To_Date('12-02','MM-DD')", LocalDate.of(1970, Month.DECEMBER, 2));
    evalEquals("To_Date('2019-02-13','YYYY-MM-DD HH24:MI:SS')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 0, 0, 0));

    // Fractional seconds. FF0 (seconds), FF3 (milliseconds), FF6 (microseconds), FF9 (nanoseconds).
    evalEquals("To_Date('2019-02-13 19:34:56.123456','YYYY-MM-DD HH24:MI:SS.FF')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 19, 34, 56, 123456000));
    evalEquals("To_Date('2019-02-13 19:34:56.123','YYYY-MM-DD HH24:MI:SS.FF3')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 19, 34, 56, 123000000));
    evalEquals("To_Date('2019-02-13 19:34:56.123456','YYYY-MM-DD HH24:MI:SS.FF6')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 19, 34, 56, 123456000));
    evalEquals("To_Date('2019-02-13 19:34:56.123456789','YYYY-MM-DD HH24:MI:SS.FF9')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 19, 34, 56, 123456789));


    // Rule to try alternate format MM -> MON and MONTH
    evalEquals("To_Date('01/Feb/2020','DD/MM/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));
    // Rule to try alternate format MM -> MON and MONTH
    evalEquals("To_Date('01/February/2020','DD/MM/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));
    // Rule to try alternate format MON -> MONTH
    evalEquals("To_Date('01/February/2020','DD/MON/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));
    // Rule to try alternate format MONTH -> MON
    evalEquals("To_Date('01/Feb/2020','DD/MONTH/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));

    // Julian date
    evalEquals("To_Date('2454803','J')", LocalDate.of(2008, 12, 2));
    evalEquals("To_Date('1721426','J')", LocalDate.of(1, 1, 1));
    evalEquals("To_Date('1001426','J')", LocalDate.of(-1971, 9, 16));


    // Is interpreted as 10 February 2003
    // evalEquals("To_Date('06-2003-MON','WW-YYYY-DY')", LocalDate.of(2003, 2, 10));

    // Is interpreted as 31 December 2003, 12:59:33
    evalEquals("To_Date('12:59:33 365-2003', 'HH24:MI:SS DDD-YYYY')",
        LocalDateTime.of(2003, 12, 31, 12, 59, 33));

    // Is interpreted as 24 December 2009, 23:00:00
    evalEquals("To_Date('2009-12-24 11:00:00 PM','YYYY-MM-DD HH:MI:SS AM')",
        LocalDateTime.of(2009, 12, 24, 23, 0, 0));
    evalEquals("To_Date('2009-12-24 11:00:00 PM','YYYY-MM-DD HH12:MI:SS AM')",
        LocalDateTime.of(2009, 12, 24, 23, 0, 0));


    // Is interpreted as 12 May 2003, 00:00:10.123
    // evalEquals("To_Date('2000_MAY_12 10.123','YYYY_MONTH_DD SS.FF3');

    evalEquals("To_Date('15:30:40','hh24:mi:ss')", LocalDateTime.of(1970, 1, 1, 15, 30, 40));

    evalNull("To_Date(NULL)");
  }

  @Test
  public void Json_Value() throws Exception {
    // Json string type
    evalEquals("Json_Value('{\"name\":\"Smith\", \"age\":29}','$.name')", "Smith");
    evalEquals("Json_Value('{\"name\":\"Smith\", \"age\":29}','$[''name'']')", "Smith");
    evalEquals("Json_Value('{\"name\":\"Smith\", \"age\":29,\"address\":{\"zip\":\"12345\",\"street\":\"Blvd des capusins\"}}','$.address.zip')", "12345");        
    evalEquals("Json_Value('{\"name\":\"Smith\", \"language\":[\"English\", \"French\"]}','$.language[1]')", "French");

    // Json numeric type
    evalEquals("Json_Value('{\"name\":\"Smith\", \"age\":29}','$.age')", 29L);
    evalEquals("Json_Value('[0, 1, 2, 3]', '$[1]')", 1L);
    evalEquals("Json_Value('{\"a\":[5, 10, 15, 20]}', '$.a[2]')", 15L);
    evalEquals("Json_Value('{\"a\":[5, 10, 15, 20]}', '$[''a''][2]')", 15L);
    evalEquals("Json_Value('[{\"a\":100}, {\"a\":200}, {\"a\":300}]', '$[1].a')", 200L);
    
    // Json boolean type
    evalFalse("Json_Value('{\"a\":[true, false, true, false]}', '$.a[1]')");
    evalTrue("Json_Value('{\"a\":[true, false, true, false]}', '$.a[2]')");

    // Json 'null' should return a NULL value
    evalNull("Json_Value('{\"name\":\"Smith\", \"age\":29, \"department\":null}','$.department')");
    
    // Json without field name quotes 
    evalEquals("Json_Value('{name:\"Smith\", age:29}','$.name')", "Smith");         
    
    evalNull("Json_Value('{\"name\":\"Smith\", \"age\":29}',Null)");
    evalNull("Json_Value(Null,'$.name')");
    
    evalFails("Json_Value('{\"name\":\"Smith\", \"age\":29}','$.notexist')");
  }
  
  @Test
  public void Json_Object() throws Exception {
    evalEquals("Json_Object(KEY 'name' VALUE 'Smith')", Converter.toJson("{\"name\":\"Smith\"}"));
    evalEquals("Json_Object(KEY 'name' VALUE 'Smith', KEY 'langue' VALUE 'english')", Converter.toJson("{\"name\":\"Smith\",\"langue\":\"english\"}"));
    evalEquals("Json_Object( 'name' VALUE 'Smith')", Converter.toJson("{\"name\":\"Smith\"}"));
    evalEquals("Json_Object(KEY 'name' VALUE 'Smith', KEY 'empty' VALUE null)", Converter.toJson("{\"name\":\"Smith\",\"empty\":null}"));
     
    
    evalFails("Json_Object(KEY 'name' VALUE )");
    evalFails("Json_Object(KEY VALUE 'Smith')");
    
    returnType("Json_Object( 'name' VALUE 'Smith')", DataTypeName.JSON);
  }
  
  @Test
  public void Reverse() throws Exception {
    evalEquals("Reverse('Hello, world!')", "!dlrow ,olleH");
    evalNull("Reverse(NULL)");
    evalFails("Reverse()");
  }

  @Test
  public void Soundex() throws Exception {
    evalEquals("Soundex('Wikipedia')", "W213");
    evalEquals("Soundex('I LOVE ROCKS.')", "I416");
    evalEquals("Soundex('I LOVE ROCK AND ROLL MUSIC.')", "I416");
    evalNull("Soundex(NULL)");
    evalFails("Soundex()");
  }

  @Test
  public void Difference() throws Exception {
    evalEquals("Difference('Juice', 'Jucy')", 4);
    evalNull("Difference(NULL,NULL)");
    evalNull("Difference(NULL,'Jucy')");
    evalNull("Difference('Juice',NULL)");
  }

  @Test
  public void Levenshtein() throws Exception {
    evalEquals("Levenshtein('Superman', 'Superman')", 0);
    evalEquals("Levenshtein('kitten', 'sitting')", 3);
    evalNull("Levenshtein(NULL,NULL)");
    evalNull("Levenshtein(NULL,'Superman')");
    evalNull("Levenshtein('Superman',NULL)");
  }

  @Test
  public void JaroWinkler() throws Exception {
    evalEquals("JaroWinkler('Superman', 'Superman')", 100);
    evalEquals("JaroWinkler('Peter Parker', 'Pete Parker')", 88);
    evalEquals("JaroWinkler('święta', 'swieta')", 78);
    evalEquals("JaroWinkler('Ich weiß nicht', 'Ich weiss nicht')", 93);
    
    evalNull("JaroWinkler(NULL,NULL)");
    evalNull("JaroWinkler(NULL,'Superman')");
    evalNull("JaroWinkler('Superman',NULL)");
  }
  
  @Test
  public void Translate() throws Exception {
    evalEquals("Translate('Hello, world!','eo','EO')", "HEllO, wOrld!");
    evalEquals("Translate('Hello, wOrld!','eol', 'E')", "HE, wOrd!");
    evalEquals("Translate('Hello, world!','oel,! ', '')", "Hwrd");
    evalNull("Translate(NULL,'eo','EO')");
    evalNull("Translate('Hello, world!',NULL,'EO')");
    evalNull("Translate('Hello, world!','EO',NULL)");
  }

  @Test
  public void Truncate() throws Exception {
    evalEquals("Truncate(-975.975)", -975D);
    evalEquals("Truncate(-975.975,-1)", -970D);
    evalEquals("Truncate(-975.975, 0)", -975D);
    evalEquals("Truncate(-975.975, 2)", -975.97);
    evalEquals("Truncate(-975.975, 3)", -975.975);
    evalEquals("Truncate(-975.975, 50)", -975.975);
    evalEquals("Truncate(123.456, -2)", 100D);
    evalNull("Truncate(-975.975, Null)");
    evalNull("Truncate(NULL, 2)");

    // Alias
    evalEquals("Trunc(123.456, -2)", 100D);
  }

  @Test
  public void Date_Trunc() throws Exception {
    evalEquals("Date_Trunc(MILLENNIUM, DATE '2020-05-08')", LocalDate.of(2000, Month.JANUARY, 1));
    evalEquals("Date_Trunc(CENTURY, DATE '2020-05-08')", LocalDate.of(2000, Month.JANUARY, 1));
    evalEquals("Date_Trunc(DECADE, DATE '2021-05-08')", LocalDate.of(2020, Month.JANUARY, 1));
    evalEquals("Date_Trunc(YEAR, DATE '2020-05-08')", LocalDate.of(2020, Month.JANUARY, 1));
    evalEquals("Date_Trunc(YY, DATE '2020-05-08')", LocalDate.of(2020, Month.JANUARY, 1));
    evalEquals("Date_Trunc(MONTH, DATE '2020-05-08')", LocalDate.of(2020, Month.MAY, 1));
    evalEquals("Date_Trunc(MM, DATE '2020-05-08')", LocalDate.of(2020, Month.MAY, 1));
    evalEquals("Date_Trunc(DAY, DATE '2020-05-25')", LocalDate.of(2020, Month.MAY, 25));
    evalEquals("Date_Trunc(DD, DATE '2020-05-25')", LocalDate.of(2020, Month.MAY, 25));
    evalEquals("Date_Trunc(QUARTER, DATE '2020-05-25')", LocalDate.of(2020, Month.APRIL, 1));
    evalEquals("Date_Trunc(Q, DATE '2020-05-25')", LocalDate.of(2020, Month.APRIL, 1));
    evalEquals("Date_Trunc(WEEK, DATE '2020-05-28')", LocalDate.of(2020, Month.MAY, 25));
    evalEquals("Date_Trunc(WW, DATE '2020-05-28')", LocalDate.of(2020, Month.MAY, 25));
    evalNull("Date_Trunc(DAY, NULL)");
    evalFails("Date_Trunc(NULL, DATE '2020-05-25')");

    // Truncate timestamp
    evalEquals("Date_Trunc(DAY, Timestamp '2020-05-25 23:59:59')",
        LocalDate.of(2020, Month.MAY, 25));
    evalEquals("Date_Trunc(HOUR, Timestamp '2020-05-25 23:59:59')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 0, 0, 0));
    evalEquals("Date_Trunc(HH, Timestamp '2020-05-25 23:59:59')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 0, 0, 0));
    evalEquals("Date_Trunc(MINUTE, Timestamp '2020-05-25 23:59:59')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 59, 0, 0));
    evalEquals("Date_Trunc(MI, Timestamp '2020-05-25 23:59:59')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 59, 0, 0));
    evalEquals("Date_Trunc(SECOND, Timestamp '2020-05-25 23:59:59')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 59, 59, 0));
    evalEquals("Date_Trunc(SS, Timestamp '2020-05-25 23:59:59')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 59, 59, 0));
    evalEquals("Date_Trunc(MILLISECOND, Timestamp '2020-05-25 23:59:59.123456789')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 59, 59, 123000000));
    evalEquals("Date_Trunc(MICROSECOND, Timestamp '2020-05-25 23:59:59.123456789')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 59, 59, 123456000));
    evalEquals("Date_Trunc(NANOSECOND, Timestamp '2020-05-25 23:59:59.123456789')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 59, 59, 123456789));  
  }

  @Test
  public void Contains() throws Exception {
    evalTrue("CONTAINS(NAME,'ES')");
    evalFalse("CONTAINS(NAME,'YZ')");
    evalNull("CONTAINS(NULL,'ES')");
    evalNull("CONTAINS(NAME,NULL)");    
    returnType("CONTAINS(NAME,'ES')", DataTypeName.BOOLEAN);
  }

  @Test
  public void StartsWith() throws Exception {
    // String
    evalTrue("StartsWith('TEST FROM','TES')");
    evalFalse("StartsWith('-TEST FROM','TES')");
    
    // Binary
    evalTrue("StartsWith(0xFAA12345,0xfA)");;
    evalFalse("StartsWith(0xFAA12345,0xEE)");
    evalFalse("StartsWith(0x12345,0x123456)");
    
    evalNull("StartsWith(NULL,'ROMA')");
    evalNull("StartsWith('TEST FROM',NULL)");
    evalFails("StartsWith()");    
    returnType("StartsWith(NAME,'ES')", DataTypeName.BOOLEAN);
  }

  @Test
  public void EndsWith() throws Exception {
    // String
    evalTrue("EndsWith('TEST FROM','ROM')");
    evalFalse("EndsWith('TEST FROM','ROMA')");
   
    // Binary
    evalTrue("EndsWith(0xFAA12345,0x2345)");
    evalFalse("EndsWith(0xFAA12345,0x88)");
    evalFalse("EndsWith(0x12345,0xFFFF12345)");
    
    evalNull("EndsWith(NULL,'ROMA')");
    evalNull("EndsWith('TEST FROM',NULL)");
    evalFails("EndsWith()");
    returnType("EndsWith(NAME,'ES')", DataTypeName.BOOLEAN);
  }

  @Test
  public void Regexp_Like() throws Exception {
    evalTrue("Regexp_Like('aaa','a{2,4}')");       
    evalTrue("Regexp_Like('Erdbeere','Erd[a[:SPACE:]b]eere')"); 
    evalTrue("Regexp_Like('12345TEST','123[:alnum:]*')");
    evalTrue("Regexp_Like('ABcdf987','[:xdigit:]*')");
    evalTrue("Regexp_Like('ABcdf987','[:xdigit:]*')");

    evalTrue("Regexp_Like('A','[a-z]','i')");
    evalFalse("Regexp_Like('A','[a-z]','c')");

    // An empty pattern '' matches nothing
    evalFalse("Regexp_Like('','')");
    evalFalse("Regexp_Like('ABC','')");

    evalNull("Regexp_Like(null,'A')");
    evalNull("Regexp_Like('A', null)");

    evalFails("Regexp_Like()");
    evalFails("Regexp_Like('A')");
    evalFails("Regexp_Like('A','[a-z]','z')");
  }

  @Test
  public void Regexp_Replace() throws Exception {
    evalEquals("Regexp_Replace('A1.2.3.4','[^0-9]')", "1234");
    evalEquals("Regexp_Replace('A1.2.3.4','[^0-9]', '', 1, 0)", "1234");
    evalEquals("Regexp_Replace('ABC, ABC, ABC','ABC', 'EFG', 1, 2)", "ABC, EFG, ABC");
   // evalEquals("Regexp_Replace('AAA BBB  CCC', '[:space:]+', '-')", "AAA BBB CCC");
    evalEquals("Regexp_Replace('expression', 's+','s')", "expresion");

    evalEquals(
        "Regexp_Replace('This line    contains    more      than one   spacing      between      words', '( ){2,}', ' ')",
        "This line contains more than one spacing between words");
    evalEquals("Regexp_Replace('ABCEFG', 'A..','WXYZ')", "WXYZEFG");
    evalEquals("Regexp_Replace('ABCEFG', '[A-Z]','',1,1)", "BCEFG");

    // An empty pattern matches nothing
    evalEquals("Regexp_Replace('ABCDEEEEEEFG', '','E')", "ABCDEEEEEEFG");

    // Back reference
    evalEquals("Regexp_Replace('FIRSTNAME MIDDLENAME LASTNAME','(.*) (.*) (.*)','\\3, \\1 \\2')",
        "LASTNAME, FIRSTNAME MIDDLENAME");

    evalNull("Regexp_Replace(null,'A')");
    evalNull("Regexp_Replace('A', null)");

    evalFails("Regexp_Replace()");
  }

  @Test
  public void Regexp_Instr() throws Exception {
    evalEquals("Regexp_Instr('email@apache.org', '@[^.]*')", 6);
    evalEquals("Regexp_Instr('hello to YOU', '(.o).', 1, 3, 1,'i')", 13);
    evalEquals(
        "Regexp_Instr('REGEXP_INSTR is an advanced extension of the INSTR function','[:a-z]{3,8}', 3, 2, 1)",
        37);

    // An empty pattern matches nothing
    evalEquals("Regexp_Instr('email@apache.org', '')", 0);
  }

  @Test
  public void Regexp_Substr() throws Exception {
    evalEquals("regexp_substr('email@apache.org', '@[^.]*')", "@apache");
    evalEquals("regexp_substr('This is a regexp_substr demo', '[a-zA-Z0-9_]+', 1, 4)",
        "regexp_substr");

    evalEquals(
        "regexp_substr('It was the best of times, it was the worst of times.', 'the\\W+\\w+', 1, 1)",
        "the best");
    evalEquals(
        "regexp_substr('It was the best of times, it was the worst of times.', 'the\\W+\\w+', 1, 2)",
        "the worst");

    // [[:alnum:]] >>> \p{Alnum}
    // evalEquals("regexp_substr('http://www.apache.org/products',
    // 'http://([a-zA-Z0-9]+\\.?){3,4}/?')", "http://www.apache.org/");

    // An empty pattern matches nothing
    evalNull("regexp_substr('email@apache.org', '')");

    evalNull("regexp_substr(null,  '@[^.]*')");
    evalNull("regexp_substr('email@apache.org', null)");

  }

  @Test
  public void EqualsNull() throws Exception {
    evalFalse("Equal_Null(1,null)");
    evalFalse("Equal_Null(null,true)");
    evalTrue("Equal_Null(null,null)");
    evalFails("Equal_Null(NOM)");
    evalTrue("Equal_Null(Date '2019-01-01',Date '2019-01-01')");
    evalFalse("Equal_Null(Date '2019-01-01',Date '2018-01-01')");
  }

  @Test
  public void Concat() throws Exception {
    // String
    evalEquals("'tes'||'t'", "test");
    evalEquals("Concat('tes','t')", "test");
    evalEquals("Concat('a','b','c')", "abc");
    evalEquals("Concat(NULL,'a')", "a");
    evalEquals("Concat('a',NULL)", "a");

    // Binary
    evalEquals("Concat(0x1F,0x2A3B)", new byte[]{0x1F, 0x2A, 0x3B});

    evalNull("Concat(NULL,NULL)");

    evalFails("Concat()");
    
    // Mix String and Binary
    evalFails("Concat(NOM,0x2A3B)");
    
    returnType("NOM||'t'", DataTypeName.STRING);
    returnType("Concat(0x1F,0x2A3B)", DataTypeName.BINARY);
  }

  @Test
  public void Chr() throws Exception {
    evalEquals("Chr(83)", "S");
    evalEquals("Chr(115)", "s");
    evalEquals("Chr(233)", "é");
    evalEquals("Chr(945)", "α");
    evalEquals("Chr(8364)", "€");
    evalEquals("Chr(33288)", "興");
    evalNull("Chr(NULL)");
    
    evalFails("Chr()");
    evalFails("Chr(-1)");
    evalFails("Chr(999999999999)");
    
    returnType("Chr(233)", DataTypeName.STRING);
  }

  @Test
  public void Ascii() throws Exception {
    evalEquals("Ascii('ABC')", 65);
    evalEquals("Ascii('é')", 233);
    evalEquals("Ascii('€')", 8364);
    evalEquals("Ascii('興')", 33288);
    evalEquals("Ascii('')", 0);
    evalNull("Ascii(NULL)");
    evalFails("Ascii()");
    returnType("Ascii('A')", DataTypeName.INTEGER);
  }

  @Test
  public void Unicode() throws Exception {
    evalEquals("Unicode('SSSS')", 83);
    evalEquals("Unicode('é')", 233);
    evalEquals("Unicode('€')", 8364);
    evalEquals("Unicode('')", 0);
    evalNull("Unicode(NULL)");
    evalFails("Unicode()");
  }

  @Test
  public void String_Encode() throws Exception {
    evalEquals("String_Encode('\t\r\n\f\b\"')", "\\t\\r\\n\\f\\b\\\"");
    // Encode 16 bit unicode
    evalEquals("String_Encode('€')", "\\u20AC");
    evalNull("String_Encode(NULL)");
  }

  @Test
  public void String_Decode() throws Exception {
    evalEquals("String_Decode('\\t\\r\\n\\f\\b\\\"')", "\t\r\n\f\b\"");
    // Decode 16 bits unicode
    evalEquals("String_Decode('\\u20AC')", "€");
    // Decode octal
    evalEquals("String_Decode('\366\344\374')", "öäü");
    evalNull("String_Decode(NULL)");
  }

  @Test
  public void Html_Encode() throws Exception {
    evalEquals("Html_Encode('18€ & <test> ™')", "18&euro; &amp; &lt;test&gt; &trade;");
    evalNull("Html_Encode(NULL)");
    evalFails("UrHtml_Encodel_Encode()");
    evalFails("Html_Encode('x','y')");
  }
  
  @Test
  public void Html_Decode() throws Exception {
    evalEquals("Html_Decode('18&euro; &amp; &lt;test&gt; &#8482;')", "18€ & <test> ™");
    evalNull("Html_Decode(NULL)");
    evalFails("Html_Decode()");
    evalFails("Html_Decode('x','y')");
  }
  
  @Test
  public void Url_Encode() throws Exception {
    evalEquals("Url_Encode('a b')", "a+b");
    evalEquals("Url_Encode('a+b')", "a%2Bb");
    evalEquals("Url_Encode('âéè')", "%C3%A2%C3%A9%C3%A8");
    evalNull("Url_Encode(NULL)");
    evalFails("Url_Encode()");
    evalFails("Url_Encode('x','y')");
  }

  @Test
  public void Url_Decode() throws Exception {
    evalEquals("Url_Decode('a+b')", "a b");
    evalEquals("Url_Decode('a%2Bb')", "a+b");
    evalEquals("Url_Decode('%C3%A2%C3%A9%C3%A8')", "âéè");
    evalNull("Url_Decode(NULL)");
    evalFails("Url_Decode('a%%2Bb')");
    evalFails("Url_Decode()");
    evalFails("Url_Decode('x','y')");
  }
  
  @Test
  public void Base64_Encode() throws Exception {
    evalEquals("Base64_Encode('Apache Hop')", "QXBhY2hlIEhvcA==");
    evalEquals("Base64_Encode('Apache Hop'::Binary)", "QXBhY2hlIEhvcA==");
    evalFails("Base64_Encode()");
    returnType("Base64_Encode('QXBhY2hlIEhvcA==')", DataTypeName.STRING);   
  }  
  
  @Test
  public void Base64_Decode() throws Exception {
    evalEquals("Base64_Decode('QXBhY2hlIEhvcA==')", "Apache Hop");
    evalEquals("Base64_Decode('QXBhY2hlIEhvcA=='::Binary)", "Apache Hop");
    evalFails("Base64_Decode()");
    
    returnType("Base64_Decode('QXBhY2hlIEhvcA==')", DataTypeName.STRING);   
  }
  
  @Test
  public void Ceil() throws Exception {
    evalEquals("Ceil(1)", 1);
    evalEquals("Ceil(125.9)", 126);
    evalEquals("Ceil(0.4873)", 1.0);
    evalEquals("Ceil(-0.65)", 0);
    evalEquals("Ceil(-42.8)", -42);
    evalNull("Ceil(null)");
    evalFails("Ceil()");
    evalFails("Ceil(1,2,3)");
    evalFails("Ceil('x')");

    // Alias
    evalEquals("Ceiling(1)", 1);
  }

  @Test
  public void Floor() throws Exception {
    evalEquals("Floor(1)", 1);
    evalEquals("Floor(125.9)", 125);
    evalEquals("Floor(0.4873)", 0.0);
    evalEquals("Floor(-0.65)", -1);
    evalEquals("Floor(Price)", -6);
    evalEquals("Floor(Amount)", 123456);
    evalNull("Floor(null)");
    evalFails("Floor()");
    evalFails("Floor(1,2,3)");
    evalFails("Floor('x')");
  }

  @Test
  public void Round() throws Exception {
    evalEquals("Round(1)", 1);
    evalEquals("Round(125.9)", 126);
    evalEquals("Round(0.4873)", 0.0);
    evalEquals("Round(-0.65)", -1);
    evalNull("Round(null)");
    evalFails("Round()");
    evalFails("Round(1,2,3)");
    evalFails("Round('x')");
  }

  @Test
  public void Ln() throws Exception {
    evalEquals("Ln(1)", 0);
    evalEquals("Exp(Ln(2.4))", 2.4);
    evalEquals("Ln(10)", 2.302585092994046);
    evalNull("Ln(null)");
    evalFails("Ln(0)");
    evalFails("Ln()");
  }

  @Test
  public void Log() throws Exception {
    evalEquals("Log(10,100)", 2);
    evalNull("Log(10,null)");
    evalNull("Log(null,1)");
    evalFails("Log(10,0)");
    evalFails("Log(-2)");
    evalFails("Log(1)");
    evalFails("Log(x,y)");
  }

  @Test
  public void Log10() throws Exception {
    evalEquals("Log10(10)", 1);
    evalNull("Log10(null)");
    evalFails("Log10(-1)");
    evalFails("Log10()");
    evalFails("Log10(1,2)");
  }

  @Test
  public void Degrees() throws Exception {
    evalEquals("Degrees(Pi())", 180);
    evalEquals("Degrees(Radians(50))", 50);
    evalNull("Degrees(null)");
    evalFails("Degrees()");
    evalFails("Degrees(1,2)");
    
  }

  @Test
  public void Radians() throws Exception {
    evalEquals("Radians(180)", Math.PI);
    evalNull("Radians(null)");
    evalFails("Radians()");
    evalFails("Radians(1,2)");
  }

  @Test
  public void Crc32() throws Exception {
    evalEquals("CRC32('Apache Hop')", "dbb81b5e");
    evalEquals("CRC32(0x123456789ABCDEF)", "2f720f20");
    evalNull("CRC32(null)");
    evalFails("CRC32()");
  }
  
  @Test
  public void MD5() throws Exception {
    evalEquals("MD5('Test')", "0cbc6611f5540bd0809a388dc95a615b");
    evalEquals("MD5(0x123456789ABCDEF123456789ABCDEF123456789ABCDEF123456789ABCDEF)",
        "99c415050a2cddbeb525670345ff0aee");
    evalNull("MD5(null)");
    evalFails("MD5()");
  }

  @Test
  public void Sha1() throws Exception {
    evalEquals("SHA1('Test')", "640ab2bae07bedc4c163f679a746f7ab7fb5d1fa");
    evalNull("SHA1(null)");
    evalFails("SHA1()");
  }

  @Test
  public void Sha224() throws Exception {
    evalEquals("SHA224('Test')",
        "c696f08d2858549cfe0929bb7b098cfa9b64d51bec94aa68471688e4");
    evalNull("SHA224(null)");
    evalFails("SHA224()");
  }
  
  @Test
  public void Sha256() throws Exception {
    evalEquals("SHA256('Test')",
        "532eaabd9574880dbf76b9b8cc00832c20a6ec113d682299550d7a6e0f345e25");
    evalNull("SHA256(null)");
    evalFails("SHA256()");
  }

  @Test
  public void Sha384() throws Exception {
    evalEquals("SHA384('Test')",
        "7b8f4654076b80eb963911f19cfad1aaf4285ed48e826f6cde1b01a79aa73fadb5446e667fc4f90417782c91270540f3");
    evalNull("SHA384(null)");
    evalFails("SHA384()");
  }

  @Test
  public void Sha512() throws Exception {
    evalEquals("SHA512('Test')",
        "c6ee9e33cf5c6715a1d148fd73f7318884b41adcb916021e2bc0e800a5c5dd97f5142178f6ae88c8fdd98e1afb0ce4c8d2c54b5f37b30b7da1997bb33b0b8a31");
    evalNull("SHA512(null)");
  }

  @Test
  public void Random() throws Exception {

    Random random = new SecureRandom();
    random.setSeed(180);
    // Hard to test, not the same on each JVM
    // evalEquals("Rand(180)", random.nextDouble());
    evalTrue("Random(0)>0");
    evalTrue("Random(180)>0");

    // Alias
    // evalTrue("Rand()>0");
  }

  @Test
  public void Compress() throws Exception {
    // evalEquals("Decompress(Compress('Test'::BINARY))::STRING", "Test");
  }

  @Test
  public void Decompress() throws Exception {
    // evalEquals("Decompress(Compress('Test'::BINARY))::STRING", "Test");
  }
  
  @Test
  public void BitGet() throws Exception {
    evalTrue("BitGet(3,1)");
    evalTrue("BitGet(3,2)");
    evalFalse("BitGet(3,4)");

    // Overflow is always false
    evalFalse("BitGet(3,255)");
    
    evalNull("BitGet(123,0)");
    evalNull("BitGet(123,-1)");
    evalNull("BitGet(null,3)");
    evalNull("BitGet(123, null)");
    
    evalFails("BitGet()");
    evalFails("BitGet(123)");
  }
  
  @Test
  public void BitSet() throws Exception {
    evalEquals("BitSet(16,1)",17);
    evalEquals("BitSet(1,4)",9);
    evalEquals("BitSet(16,4)",24);
    evalEquals("BitSet(32,4)",40);

    // Overflow has no impact on result
    evalEquals("BitSet(32,66)",32);

    evalNull("BitSet(123,0)");
    evalNull("BitSet(123,-1)");
    evalNull("BitSet(null,3)");
    evalNull("BitSet(123, null)");
    
    evalFails("BitSet()");    
    evalFails("BitSet(123)");
  }
  
  @Test
  public void BitClear() throws Exception {
    evalEquals("BitClear(3,1)",2);
    evalEquals("BitClear(3,4)",3);

    // Overflow has no impact on result
    evalEquals("BitClear(32,66)",32);

    evalNull("BitClear(123,0)");
    evalNull("BitClear(123,-1)");
    evalNull("BitClear(null,3)");
    evalNull("BitClear(123, null)");
    
    evalFails("BitClear()");    
    evalFails("BitClear(123)");
  }  
  
  @Test
  public void BitShift() throws Exception {
    evalEquals("BitShift(123,0)",123);
    evalEquals("BitShift(1,4)",16);
    evalEquals("BitShift(2,8)",512);
    evalEquals("BitShift(6,-2)", 1);
    evalEquals("BitShift(16,-4)", 1);    
    evalEquals("BitShift(10000,-3)", 1250);
    evalEquals("BitShift(1,63)", 0x8000000000000000L);
    evalEquals("BitShift(0x8000000000000000,-63)", 1);
    
    // Cast to integer
    evalEquals("BitShift(16.3,-4)", 1);
    evalEquals("BitShift(16.9,-4)", 1);
    
    // Overflow
    evalEquals("BitShift(1,64)", 0);
    evalEquals("BitShift(1,-64)", 0);
    // Underflow
    evalEquals("BitShift(1,-1)", 0);
    
    
    evalNull("BitShift(null,3)");
    evalNull("BitShift(123, null)");
    
    evalFails("BitShift('Bidon',3)");
    evalFails("BitShift()");
    evalFails("BitShift(123)");
  }
  
  @Test
  public void BitRotate() throws Exception {
    evalEquals("BitRotate(123,0)",123);
    evalEquals("BitRotate(1,4)",16);
    evalEquals("BitRotate(16,-4)", 1);
    evalEquals("BitRotate(-9223372036854775807,2)", 6L);
    evalEquals("BitRotate(6,-2)", -9223372036854775807L);
    evalEquals("BitRotate(10000,-3)", 1250);
    // Full rotate 64 bits
    evalEquals("BitRotate(123456,64)",123456);
    evalEquals("BitRotate(123456,128)",123456);
    evalEquals("BitRotate(123456,-64)",123456);
    evalEquals("BitRotate(123456,-128)",123456);

    
    evalNull("BitRotate(null,3)");
    evalNull("BitRotate(123, null)");
    
    evalFails("BitRotate()");
    evalFails("BitRotate(123)");
  } 

  @Test
  public void TypeOf() throws Exception {
    evalEquals("TypeOf('str')","STRING");
    evalEquals("TypeOf(BitRotate(1,4))","INTEGER");
    evalEquals("TypeOf(TRUE)","BOOLEAN");
  }
  
  @Test
  public void Count() throws Exception {
    //evalEquals("Exp(1)", Math.E);
    //evalEquals("Exp(2)", Math.E * Math.E);
    //evalNull("COUNT(NULL)");
    evalFails("Count()");
    evalFails("Count(DISTINCT )");
    evalFails("Count(1,2)");
    returnType("Count(*)", DataTypeName.INTEGER);
    writeEquals("COUNT(AGE)");
    writeEquals("COUNT(*)");
    writeEquals("COUNT(DISTINCT AGE)");
  }
  
  @Test
  public void CountIf() throws Exception {
    //evalEquals("Exp(1)", Math.E);
    //evalEquals("Exp(2)", Math.E * Math.E);
    //evalNull("COUNT(NULL)");
    evalFails("CountIf()");
    evalFails("CountIf(FIELD_DATE)");
    evalFails("CountIf(1,2)");
    returnType("CountIf(AGE>=10)", DataTypeName.INTEGER);
    writeEquals("COUNTIF(AGE>=10)");
  }
  
  @Test
  public void Extract() throws Exception {
    evalEquals("Extract(MILLENNIUM from Timestamp '2020-05-25 23:48:59')", 3);
    evalEquals("Extract(CENTURY from Timestamp '2000-12-25 23:48:59')", 20);
    evalEquals("Extract(CENTURY from Timestamp '2020-05-25 23:48:59')", 21);
    evalEquals("Extract(CENTURY from Date '0001-01-01')", 1);
    evalEquals("Extract(DECADE from Timestamp '1999-02-16 20:38:40')", 199);
    evalEquals("Extract(YEAR from Timestamp '2020-05-25 23:48:59')", 2020);
    evalEquals("Extract(ISOYEAR from Date '2017-01-01')", 2016);
    evalEquals("Extract(QUARTER from Timestamp '2020-05-25 23:48:59')", 2);
    evalEquals("Extract(MONTH from Timestamp '2020-05-25 23:48:59')", 5);
    evalEquals("Extract(WEEK from Timestamp '2020-05-25 23:48:59')", 21);
    evalEquals("Extract(WEEK from Timestamp '2020-01-01 23:48:59')", 1);
    evalEquals("Extract(ISOWEEK from Date '2016-01-03')", 53);
    evalEquals("Extract(ISOWEEK from Date '2016-01-04')", 1);
    evalEquals("Extract(WEEKOFMONTH from Date '2011-03-15')", 3);
    evalEquals("Extract(DAY from Timestamp '2020-05-25 23:48:59')", 25);
    evalEquals("Extract(DAYOFWEEK from Timestamp '2020-05-25 23:48:59')", 2);
    evalEquals("Extract(ISODAYOFWEEK from Date '2003-12-28')", 7);    
    evalEquals("Extract(EPOCH from Timestamp '1970-01-01 00:00:00')", 0);
    evalEquals("Extract(EPOCH from Timestamp '1970-01-02 00:00:00')", 86400);
    evalEquals("Extract(HOUR from Timestamp '2020-05-25 23:48:59')", 23);
    evalEquals("Extract(MINUTE from Timestamp '2020-05-25 23:48:59')", 48);
    evalEquals("Extract(SECOND from Timestamp '2020-05-25 23:48:59')", 59);
    evalEquals("Extract(MILLISECOND from Timestamp '2020-05-25 00:00:01.123456')", 123);
    evalEquals("Extract(MICROSECOND from Timestamp '2020-05-25 00:00:01.123456')", 123456);
    evalEquals("Extract(NANOSECOND from Timestamp '2020-05-25 00:00:01.123456')", 123456000);   
    evalEquals("Extract(TIMEZONE_ABBR from Timestamp '2021-01-01 15:28:59')", "UTC");
    evalEquals("Extract(TIMEZONE_REGION from Timestamp '2021-01-01 15:28:59')", "UTC");
    evalEquals("Extract(TIMEZONE_HOUR from Timestamp '2021-01-01 15:28:59 +02:00')", 2);
    evalEquals("Extract(TIMEZONE_HOUR from Timestamp '2021-01-01 15:28:59 -04:00')", -4);
    evalEquals("Extract(TIMEZONE_MINUTE from Timestamp '2021-01-01 15:28:59 +01:28')", 28);
    
    evalNull("Extract(SECOND from NULL)");

    evalFails("Extract(NULL from Date '2021-01-01')");
    evalFails("Extract(BIDON from NULL)");

    writeEquals("EXTRACT(CENTURY FROM FIELD_DATE)");
    
    // Alias
    evalEquals("Date_Part(HOUR,Timestamp '2020-05-25 23:48:59')", 23);    
    
    returnType("EXTRACT(CENTURY FROM FIELD_DATE)", DataTypeName.INTEGER);
  }
  
  @Test
  public void Position() throws Exception {   
    evalEquals("Position('abc' IN 'abcdefgh')", 1);
    evalEquals("Position('XYZ' IN 'abcdefgh')", 0);
    evalEquals("Position('def' IN 'abcdefgh')", 4);
    
    evalNull("Position(NULL IN 'abcdefgh')");
    evalNull("Position('abc' IN NULL)");
    evalFails("Position('abc' IN ");
    evalFails("Position( IN 'fsd'");
  }
}

