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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.apache.hop.expression.Attribute;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.BooleanType;
import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.IntervalType;
import org.apache.hop.expression.type.JsonType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.junit.Test;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Locale;
import java.util.TimeZone;
import ch.obermuhlner.math.big.BigDecimalMath;

public class ScalarFunctionTest extends ExpressionTest {

  @Test
  public void Abort() throws Exception {
    evalFails("ABORT('Custom error message')");
  }

  @Test
  public void Try_Cast() throws Exception {

    // String to Boolean
    evalTrue("TRY_CAST('Yes' as Boolean)").returnType(BooleanType.BOOLEAN);
    evalFalse("TRY_CAST('False' as Boolean)");
    evalNull("TRY_CAST('Fake' as Boolean)");

    // Number to Boolean
    evalTrue("TRY_CAST(1 as Boolean)").returnType(BooleanType.BOOLEAN);
    evalTrue("TRY_CAST(-12.1 as Boolean)");
    evalNull("TRY_CAST('test' as Boolean)");

    // Date to String
    evalEquals("TRY_CAST(DATE '2019-02-25' AS STRING FORMAT 'DD/MM/YYYY')", "25/02/2019").returnType(StringType.STRING);
    evalNull("TRY_CAST('2019-99-25' AS DATE)").returnType(DateType.DATE);
    evalNull("TRY_CAST('2019-99-25' AS DATE FORMAT 'YYYY-MM-DD')");
    evalNull("TRY_CAST(NULL_STRING AS DATE)");

    // Bad syntax
    evalFails("TRY_CAST('2020-01-021' AS NULL)");
    evalFails("TRY_CAST('2020-01-021' AS DATE FORMAT NULL_STRING)");
    evalFails("TRY_CAST('bad' AS)");
    evalFails("TRY_CAST(1234 AS STRING FORMAT )");
    evalFails("TRY_CAST(DATE '2019-02-25' AS String FORMAT )");

    // Bad data type
    evalFails("Try_Cast(123 as Nill)");

    // Bad format
    evalFails("TRY_CAST('2020-01-021' AS DATE FORMAT 'OOOO-MM-DD')");

    optimize("TRY_CAST(FIELD_STRING AS BINARY)");
    optimize("TRY_CAST(FIELD_INTEGER AS NUMBER)");
    optimize("TRY_CAST(FIELD_STRING AS DATE FORMAT 'YYYY-MM-DD')");
  }

  @Test
  public void Try_To_Binary() throws Exception {
    evalEquals("TRY_TO_BINARY(HEX_ENCODE('Apache Hop'),'HEX')", "Apache Hop".getBytes());
    evalEquals("TRY_TO_BINARY('41706163686520486f70','HEX')", "Apache Hop".getBytes());
    evalNull("TRY_TO_BINARY('Z4','HEX')");

    evalEquals("TRY_TO_BINARY(BASE64_ENCODE('Apache Hop'),'BASE64')", "Apache Hop".getBytes());
    evalEquals("TRY_TO_BINARY('QXBhY2hlIEhvcA==','BASE64')", "Apache Hop".getBytes());

    evalEquals("TRY_TO_BINARY('Apache Hop','UtF-8')",
        "Apache Hop".getBytes(StandardCharsets.UTF_8));

    evalNull("TRY_TO_BINARY(NULL_STRING)");

    // Failed if format is null
    evalFails("TRY_TO_BINARY('Apache Hop',NULL_STRING)");
    // Failed if format is bad
    evalFails("TRY_TO_BINARY('Apache Hop','ZZZ')");
    evalFails("TRY_TO_BINARY()");
  }

  @Test
  public void Try_To_Boolean() throws Exception {
    evalTrue("TRY_TO_BOOLEAN('True')").returnType(BooleanType.BOOLEAN);
    evalFalse("TRY_TO_BOOLEAN('falSE')");
    evalNull("TRY_TO_BOOLEAN('test')").returnType(BooleanType.BOOLEAN);
    evalNull("TRY_TO_BOOLEAN(NULL_STRING)");
    evalFails("TRY_TO_BOOLEAN()");
  }

  @Test
  public void Try_To_Number() throws Exception {
    evalEquals("TRY_TO_NUMBER('5467.12', '999999.99')", 5467.12D).returnType(NumberType.NUMBER);

    // Return NULL if parsing failed
    evalNull("TRY_TO_NUMBER('54Z67z12', '999999D99')");

    evalNull("TRY_TO_NUMBER(NULL_STRING)");

    // Failed if format is bad
    evalFails("TRY_TO_NUMBER('5467.12', 'ZZZ')");
    evalFails("TRY_TO_NUMBER()");
  }

  @Test
  public void Try_To_Date() throws Exception {
    evalEquals("TRY_TO_DATE('2019-02-13','YYYY-MM-DD')", LocalDate.of(2019, 2, 13));

    // Return NULL if parsing failed
    evalNull("TRY_TO_DATE('2019-01-42','YYYY-MM-DD')");
    evalNull("TRY_TO_DATE('2019-01-0x','YYYY-MM-DD')");
    evalNull("TRY_TO_DATE('2019-13-13','YYYY-MM-DD')");
    evalNull("TRY_TO_DATE('20x9-13-13','YYYY-MM-DD')");

    evalNull("TRY_TO_DATE(NULL_STRING,'FXDD/MM/YYYY')");

    // Failed if format is bad
    evalFails("TRY_TO_DATE('2019-12-01','OOOO-MM-DD')");
  }

  @Test
  public void Try_To_Json() throws Exception {

    evalEquals("Try_To_Json('{\"name\":\"Smith\", \"age\":29}')",
        JsonType.convertStringToJson("{\"name\":\"Smith\",\"age\":29}")).returnType(JsonType.JSON);
    evalEquals("Try_To_Json('true')", JsonType.convertStringToJson("true"));
    evalEquals("Try_To_Json('null')", JsonType.convertStringToJson("null"));

    evalNull("Try_To_Json(NULL_STRING)");
    evalNull("Try_To_Json('BAD JSON ;')");
    evalNull("Try_To_Json('{\"name\":\"Smith\"; \"age\":29}')");

    evalFails("Try_To_Json()");
    evalFails("Try_To_Json(BOOLEAN_FIELD)");
  }


  @Test
  public void Coalesce() throws Exception {
    // Coalesce string
    evalEquals("Coalesce(NULL_STRING,'TEST','BIDON')", "TEST").returnType(StringType.STRING);

    // Coalesce numeric
    evalEquals("Coalesce(1,2,3)", 1L).returnType( IntegerType.of(1));
    // TODO: max precision evalEquals("Coalesce(1,2,30)", 1L).returnType( IntegerType.of(2));
    evalEquals("Coalesce(NULL_NUMBER,NULL_INTEGER,1,2)", 1L).returnType(NumberType.NUMBER);
    evalNull("Coalesce(NULL_NUMBER,NULL_INTEGER,NULL_BIGNUMBER)").returnType(NumberType.NUMBER);
    evalFails("Coalesce()");

    optimize("COALESCE(NULL)", "NULL");
    optimize("COALESCE(FIELD_INTEGER)", "FIELD_INTEGER");
    optimize("COALESCE(NULL, FIELD_INTEGER, FIELD_NUMBER)", "COALESCE(FIELD_INTEGER,FIELD_NUMBER)");   
    
    // Duplicate coalesce
    optimize("COALESCE(FIELD_INTEGER,FIELD_INTEGER)", "FIELD_INTEGER");
    optimize("COALESCE(FIELD_INTEGER, FIELD_NUMBER, FIELD_NUMBER)",
        "COALESCE(FIELD_INTEGER,FIELD_NUMBER)");

    // Flatten
    optimize("COALESCE(FIELD_INTEGER,COALESCE(FIELD_INTEGER,COALESCE(FIELD_NUMBER,2)))",
        "COALESCE(FIELD_INTEGER,FIELD_NUMBER,2)");

    // First not null is literal
    optimize("COALESCE(1, FIELD_INTEGER,FIELD_NUMBER)", "1");
    optimize("COALESCE(null, 1, FIELD_INTEGER,FIELD_NUMBER)", "1");
  }

  @Test
  public void If() throws Exception {
    evalEquals("If(FIELD_BOOLEAN_TRUE,'True','False')", "True").returnType(StringType.STRING);
    evalEquals("If(FIELD_BOOLEAN_FALSE,'True','False')", "False");
    evalEquals("If(FIELD_BOOLEAN_TRUE,1,2)",1L).returnType(IntegerType.INTEGER);
    evalEquals("If(FIELD_BOOLEAN_TRUE,2,2.3)",2L).returnType(NumberType.NUMBER);
    evalEquals("If(FIELD_BOOLEAN_TRUE,Date '2023-01-01',Date '2023-02-01')", LocalDate.of(2023, 1, 1)).returnType(DateType.DATE);
    
    // If condition is NULL then return false value
    evalEquals("If(NULL_BOOLEAN,'A','B')", "B").returnType(StringType.STRING);

    // Syntax with only 2 operands 
    evalEquals("If(true,'Test')", "Test");
    evalNull("If(false,'Test')");
    evalNull("If(false,1)");
    evalNull("If(false,Date '2023-01-01')");


    evalFails("If()");
    evalFails("If(true)");
    evalFails("If(true,2,'2')");
    evalFails("If(Date '2023-01-01',1,2)");

    // Simplify IF(x IS NULL,y,x)→ IFNULL(x, y)
    optimize("IF(FIELD_INTEGER IS NULL,\"YEAR\",FIELD_INTEGER)", "IFNULL(FIELD_INTEGER,\"YEAR\")");
    // Simplify IF(x IS NULL,y,z) → NVL2(x, z, y)
    optimize("IF(FIELD_INTEGER IS NULL,\"YEAR\",NULL_INTEGER)",
        "NVL2(FIELD_INTEGER,NULL_INTEGER,\"YEAR\")");
    // Simplify IF(x IS NOT NULL,y,z) → NVL2(x, y, z)
    optimize("IF(FIELD_INTEGER IS NOT NULL,\"YEAR\",NULL_INTEGER)",
        "NVL2(FIELD_INTEGER,\"YEAR\",NULL_INTEGER)");
    // Simplify IF(x=y,NULL,x) → NULLIF(x, y)
    optimize("IF(FIELD_INTEGER=FIELD_NUMBER,NULL,FIELD_INTEGER)",
        "NULLIF(FIELD_INTEGER,FIELD_NUMBER)");
    // Simplify IF(x=y,NULL,y) → NULLIF(y, x)
    optimize("IF(FIELD_INTEGER=FIELD_NUMBER,NULL,FIELD_NUMBER)",
        "NULLIF(FIELD_NUMBER,FIELD_INTEGER)");
    // No simplify
    optimize("IF(FIELD_INTEGER=FIELD_NUMBER,NULL,FIELD_BOOLEAN_TRUE)",
        "IF(FIELD_INTEGER=FIELD_NUMBER,NULL,FIELD_BOOLEAN_TRUE)");
  }

  @Test
  public void Nvl2() throws Exception {
    evalEquals("Nvl2(FIELD_BOOLEAN_TRUE,FIELD_STRING,'ex2')", "TEST").returnType(StringType.of(1000));
    evalEquals("Nvl2('test','ex1','ex2')", "ex1");
    evalEquals("Nvl2(NULL_STRING,'ex1','ex2')", "ex2");

    evalFails("Nvl2()");
    evalFails("Nvl2(true)");
    evalFails("Nvl2(true,2)");
  }

  @Test
  public void IfNull() throws Exception {
    evalEquals("IfNull(1,FIELD_INTEGER)", 1L).returnType(IntegerType.INTEGER);
    evalEquals("IfNull(NULL_INTEGER, FIELD_NUMBER)", -5L).returnType(NumberType.NUMBER);
    
    evalEquals("IfNull(NULL_STRING,'B')", "B").returnType(StringType.STRING);
    
    evalEquals("IfNull('A','B')", "A").returnType(StringType.STRING);
    evalEquals("IfNull(NULL_STRING,'B')", "B").returnType(StringType.STRING);
    
    evalEquals("IfNull(NULL_DATE,DATE '2022-01-01')", LocalDate.of(2022, 1, 1)).returnType( DateType.DATE);
    
    evalFails("IfNull()");
    evalFails("IfNull(1)");
    evalFails("IfNull(1,2,3)");

    // Alias
    evalEquals("NVL(NULL_INTEGER,1)", 1L);
  }

  @Test
  public void NullIf() throws Exception {
    evalEquals("NullIf(1,NULL_INTEGER)", 1L);
    evalNull("NullIf(1,1)");
    evalNull("NULLIF(0.1,0.1)");
    evalNull("NullIf(NULL_INTEGER,1)");
    evalNull("NullIf('TEST','TEST')");
    evalNull("NullIf(DATE '2019-01-01',DATE '2019-01-01')");
    evalEquals("NullIf(1,2)", 1L);
    evalEquals("NullIf('TEST','XXX')", "TEST");
    evalEquals("NullIf(DATE '2019-01-01',DATE '2018-12-31')", LocalDate.of(2019, Month.JANUARY, 1));

    optimizeNull("NULLIF(NULL, NULL)");
    optimizeNull("NULLIF(true, true)");
    optimizeTrue("NULLIF(true, false)");
    optimizeNull("NULLIF(NULL, false)");
    optimizeTrue("NULLIF(true, NULL)");
    optimizeNull("NULLIF(NULL, FIELD_STRING)");
    optimizeNull("NULLIF('a', 'a')");
    optimize("NULLIF('a', 'b')", "'a'");
    optimizeNull("NULLIF(NULL, 'b')");
    optimize("NULLIF('a', NULL)", "'a'");
    optimizeNull("NULLIF(1, 1)");
    optimize("NULLIF(1, 2)", "1");
  }

  @Test
  public void ZeroIfNull() throws Exception {
    evalEquals("ZeroIfNull(1)", 1L);
    evalEquals("ZeroIfNull(NULL_INTEGER)", 0L);
    evalFails("ZeroIfNull()");
    evalFails("ZeroIfNull(1,2)");
  }

  @Test
  public void NullIfZero() throws Exception {
    evalEquals("NULLIFZERO(0.1)", new BigDecimal("0.1"));
    evalEquals("NULLIFZERO(0.1)", 0.1);
    evalEquals("NullIfZero(1)", 1L);
    evalNull("NullIfZero(0)");
    evalNull("NullIfZero(0.000)");
    evalNull("NullIfZero(-0.0)");

    evalFails("NullIfZero()");

    optimize("NULLIFZERO(0.1)", "0.1");
    optimizeNull("NULLIFZERO(0.0)");
    optimizeNull("NULLIFZERO(-0.000)");
  }

  @Test
  public void Decode() throws Exception {
    evalEquals("Decode(1,1,'one',2,'two',NULL_INTEGER,'<NULL>','other')", "one");
    evalEquals("Decode(2,1,'one',2,'two',NULL_INTEGER,'<NULL>','other')", "two");
    evalEquals("Decode(NULL_INTEGER,1,'one',2,'two',NULL_INTEGER,'<NULL>','other')", "<NULL>");
    evalEquals("Decode(9,1,'one',2,'two',NULL_INTEGER,'<NULL>','other')", "other");

    // Support ABORT as default
    evalEquals("Decode(FIELD_INTEGER,4,'Flag 1',40,'Flag 2',ABORT('Error generated with ABORT'))",
        "Flag 2");

    evalNull("Decode(9,1,'one',2,'two',NULL_INTEGER,'<NULL>')");
    evalNull("Decode(9,1,'one',9,NULL,NULL_INTEGER,'<NULL>')");
    evalNull("Decode(1,1,NULL,9,'9',NULL_INTEGER,'<NULL>')");

    evalFails("Decode()");
    evalFails("Decode(1)");
    evalFails("Decode(1,2)");
    // Mixed type
    evalFails("Decode(AGE,1,'baby',20,false,true)");

    // Simplify unreachable DECODE clauses
    optimize("DECODE(FIELD_NUMBER,FIELD_INTEGER,1,2,2,FIELD_INTEGER,3)",
        "DECODE(FIELD_NUMBER,FIELD_INTEGER,1,2,2)");
    optimize("DECODE(FIELD_NUMBER,FIELD_INTEGER,1,2,2,FIELD_INTEGER,3,4)",
        "DECODE(FIELD_NUMBER,FIELD_INTEGER,1,2,2,4)");
  }

  @Test
  public void Pi() throws Exception {
    evalEquals("Pi()", PI).returnType(NumberType.NUMBER);
    evalFails("Pi(123)");

    optimize("PI()", "3.1415926535897932384626433832795");
  }

  @Test
  public void Current_Date() throws Exception {
    ExpressionContext context = createExpressionContext();
    ZonedDateTime today = Attribute.CURRENT_DATE.get(context);
    evalEquals(context, "Today()", today).returnType(DateType.DATE);
    evalEquals(context, "Current_Date()", today);

    evalFails("Today(Null)");
  }

  @Test
  public void Current_Timestamp() throws Exception {
    ExpressionContext context = createExpressionContext();
    ZonedDateTime today = Attribute.CURRENT_TIMESTAMP.get(context);
    evalEquals(context, "Now()", today).returnType(DateType.DATE);
    evalEquals(context, "Current_Timestamp()", today);

    evalFails("Now(Null)");
  }

  @Test
  public void Current_TimeZone() throws Exception {
    TimeZone.setDefault(TimeZone.getTimeZone("Europe/Paris"));
    evalEquals("Current_Timezone()", "Europe/Paris").returnType(StringType.STRING);
    TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
    evalEquals("Current_Timezone()", "UTC").returnType(StringType.STRING);
    evalFails("Current_Timezone(Null)");
  }

  @Test
  public void ConvertTimeZone() throws Exception {
    evalEquals(
        "CONVERT_TIMEZONE('America/Los_Angeles', 'America/New_York', TIMESTAMP '2023-01-01 14:00:00')",
        LocalDateTime.of(2023, Month.JANUARY, 1, 17, 00, 00)).returnType(DateType.DATE);
    evalEquals("CONVERT_TIMEZONE('America/Los_Angeles', TIMESTAMP '2023-01-01 14:00:00 +02:00')",
        LocalDateTime.of(2023, Month.JANUARY, 1, 04, 00, 00));
    evalEquals("CONVERT_TIMEZONE('Asia/Tokyo', TIMESTAMP '2023-01-01 14:00:00')",
        LocalDateTime.of(2023, Month.JANUARY, 1, 23, 00, 00));
    evalEquals("CONVERT_TIMEZONE('+00:00','+10:00', TIMESTAMP '2023-01-01 12:00:00')",
        ZonedDateTime.of(2023, 1, 1, 22, 00, 00, 00000000, ZoneOffset.ofHoursMinutes(10, 0)));

    evalNull("CONVERT_TIMEZONE('Europe/Paris', NULL_TIMESTAMP)").returnType(DateType.DATE);
    evalNull("CONVERT_TIMEZONE('Europe/Paris', 'America/New_York', NULL_TIMESTAMP)");

    evalFails("CONVERT_TIMEZONE(Null, '2023-01-01 14:00:00')");
  }

  @Test
  public void Current_User() throws Exception {
    evalEquals("Current_User()", System.getProperty("user.name")).returnType(StringType.of(System.getProperty("user.name").length()));
  }

  @Test
  public void MakeDate() throws Exception {
    evalEquals("MAKE_DATE(2019,01,1)", LocalDate.of(2019, Month.JANUARY, 1)).returnType(DateType.DATE);
    evalEquals("MAKE_DATE(2020,02,27)", LocalDate.of(2020, Month.FEBRUARY, 27));
    evalEquals("MAKE_DATE(2020,19,1)", LocalDate.of(2021, Month.JULY, 1));
    evalEquals("MAKE_DATE(2020, 0, 1)", LocalDate.of(2019, Month.DECEMBER, 1));
    evalEquals("MAKE_DATE(2020,-1,1)", LocalDate.of(2019, Month.NOVEMBER, 1));
    evalEquals("MAKE_DATE(2020,-2, 1)", LocalDate.of(2019, Month.OCTOBER, 1));
    evalEquals("MAKE_DATE(2020,-6,1)", LocalDate.of(2019, Month.JUNE, 1));
    evalEquals("MAKE_DATE(2020, 6, 50)", LocalDate.of(2020, Month.JULY, 20));
    evalEquals("MAKE_DATE(2020, 2, 0)", LocalDate.of(2020, Month.JANUARY, 31));
    evalEquals("MAKE_DATE(2020, 2, -1)", LocalDate.of(2020, Month.JANUARY, 30));

    evalNull("MAKE_DATE(NULL_INTEGER,-1,1)").returnType(DateType.DATE);
    evalNull("MAKE_DATE(2020,NULL_INTEGER,1)");
    evalNull("MAKE_DATE(2020,-1,NULL_INTEGER)");

    evalFails("MAKE_DATE()");
    evalFails("MAKE_DATE(2020)");
    evalFails("MAKE_DATE(2020,15)");
    evalFails("MAKE_DATE(2020,1,1,1)");
  }

  @Test
  public void MakeTimestamp() throws Exception {
    evalEquals("MAKE_TIMESTAMP(2019,01,1,23,15,59)",
        LocalDateTime.of(2019, Month.JANUARY, 1, 23, 15, 59)).returnType(DateType.DATE);
    evalEquals("MAKE_TIMESTAMP(2020,-6,1,23,15,59)",
        LocalDateTime.of(2019, Month.JUNE, 1, 23, 15, 59));
    evalEquals("MAKE_TIMESTAMP(2020,0,1,23,15,59)",
        LocalDateTime.of(2019, Month.DECEMBER, 1, 23, 15, 59));
    evalEquals("MAKE_TIMESTAMP(2020,-1,1,23,15,59)",
        LocalDateTime.of(2019, Month.NOVEMBER, 1, 23, 15, 59));
    evalEquals("MAKE_TIMESTAMP(2020,6,50,23,15,59)",
        LocalDateTime.of(2020, Month.JULY, 20, 23, 15, 59));
    evalEquals("MAKE_TIMESTAMP(2020,6,50,23,15,59.123)",
        LocalDateTime.of(2020, Month.JULY, 20, 23, 15, 59, 123000000));
    evalEquals("MAKE_TIMESTAMP(2020,6,50,23,15,59.123456)",
        LocalDateTime.of(2020, Month.JULY, 20, 23, 15, 59, 123456000));
    evalEquals("MAKE_TIMESTAMP(2020,6,50,23,15,59.123456789)",
        LocalDateTime.of(2020, Month.JULY, 20, 23, 15, 59, 123456789));

    evalNull("MAKE_TIMESTAMP(NULL_INTEGER,-1,1,23,15,59)").returnType(DateType.DATE);
    evalNull("MAKE_TIMESTAMP(2020,NULL_INTEGER,1,23,15,59)");
    evalNull("MAKE_TIMESTAMP(2020,-1,NULL_INTEGER,23,15,59)");

    evalFails("MAKE_TIMESTAMP()");
    evalFails("MAKE_TIMESTAMP(2020)");
    evalFails("MAKE_TIMESTAMP(2020,15)");
    evalFails("MAKE_TIMESTAMP(2020,1,1,23,15,59.123456789,9999)");
  }

  @Test
  public void MakeInterval() throws Exception {
    evalEquals("MAKE_INTERVAL(20,1,1,23,15,59)", Interval.of(20, 1, 1, 23, 15, 59)).returnType(IntervalType.INTERVAL);
    evalEquals("MAKE_INTERVAL(20,1,1,23,15,59.123)", Interval.of(20, 1, 1, 23, 15, 59, 123000000));

    evalFails("MAKE_INTERVAL()");
    evalFails("MAKE_INTERVAL(20)");
    evalFails("MAKE_INTERVAL(20,15)");
    evalFails("MAKE_INTERVAL(20,1,1,23,15,59.123456789,9999)");
  }

  @Test
  public void First_Day() throws Exception {
    evalEquals("First_Day(DATE '2019-01-01')", LocalDate.of(2019, Month.JANUARY, 1)).returnType(DateType.DATE);
    evalEquals("First_Day(DATE '2020-02-27')", LocalDate.of(2020, Month.FEBRUARY, 1));
    evalEquals("First_Day(DATE '2020-02-27', YEAR)", LocalDate.of(2020, Month.JANUARY, 1));
    evalEquals("First_Day(DATE '2020-02-27', MONTH)", LocalDate.of(2020, Month.FEBRUARY, 1));
    evalEquals("First_Day(DATE '2020-02-27', QUARTER)", LocalDate.of(2020, Month.JANUARY, 1));
    evalEquals("First_Day(DATE '2020-05-27', QUARTER)", LocalDate.of(2020, Month.APRIL, 1));
    evalEquals("First_Day(DATE '2020-02-01', WEEK)", LocalDate.of(2020, Month.JANUARY, 27));
    evalEquals("First_Day(DATE '2020-02-27', WEEK)", LocalDate.of(2020, Month.FEBRUARY, 24));
    evalEquals("First_Day(DATE '2020-12-31', WEEK)", LocalDate.of(2020, Month.DECEMBER, 28));
    evalEquals("First_Day(FIELD_DATE, YEAR)", LocalDate.of(1981, Month.JANUARY, 1));
    evalEquals("First_Day(FIELD_TIMESTAMP, YEAR)", LocalDate.of(2023, Month.JANUARY, 1));

    // Remove time
    evalEquals("First_Day(TIMESTAMP '2020-02-27 23:59:12', YEAR)",
        LocalDate.of(2020, Month.JANUARY, 1));
    evalEquals("First_Day(TIMESTAMP '2020-02-27 23:59:12', MONTH)",
        LocalDate.of(2020, Month.FEBRUARY, 1));

    evalNull("First_Day(NULL_DATE)").returnType(DateType.DATE);
    evalNull("First_day(NULL_DATE, MONTH)");

    evalFails("First_Day()");
    evalFails("First_Day(FIELD_STRING)");
    evalFails("First_Day(FIELD_DATE, FIELD_INTEGER)");
    evalFails("First_Day(FIELD_DATE, NULL)");
    evalFails("First_Day(FIELD_DATE, HOUR)");
  }

  @Test
  public void Last_Day() throws Exception {
    evalEquals("Last_Day(DATE '2019-01-01')", LocalDate.of(2019, Month.JANUARY, 31)).returnType(DateType.DATE);
    evalEquals("Last_Day(DATE '2020-02-27')", LocalDate.of(2020, Month.FEBRUARY, 29));
    evalEquals("Last_Day(DATE '2020-02-27', YEAR)", LocalDate.of(2020, Month.DECEMBER, 31));
    evalEquals("Last_Day(DATE '2022-02-27', MONTH)", LocalDate.of(2022, Month.FEBRUARY, 28));
    evalEquals("Last_Day(DATE '2020-02-27', MONTH)", LocalDate.of(2020, Month.FEBRUARY, 29));
    evalEquals("Last_Day(DATE '2020-02-27', QUARTER)", LocalDate.of(2020, Month.MARCH, 31));
    evalEquals("Last_Day(DATE '2020-07-27', QUARTER)", LocalDate.of(2020, Month.SEPTEMBER, 30));
    evalEquals("Last_Day(DATE '2020-12-31', WEEK)", LocalDate.of(2021, Month.JANUARY, 3));
    evalEquals("Last_Day(FIELD_DATE, YEAR)", LocalDate.of(1981, Month.DECEMBER, 31));
    evalEquals("Last_Day(FIELD_TIMESTAMP, YEAR)", LocalDate.of(2023, Month.DECEMBER, 31));

    // Remove time
    evalEquals("Last_Day(TIMESTAMP '2020-02-27 23:59:12')", LocalDate.of(2020, Month.FEBRUARY, 29));
    evalEquals("Last_Day(TIMESTAMP '2020-02-27 23:59:12', YEAR)",
        LocalDate.of(2020, Month.DECEMBER, 31));

    evalNull("Last_Day(NULL_DATE)").returnType(DateType.DATE);
    evalNull("Last_Day(NULL_DATE, MONTH)");

    evalFails("Last_Day()");
    evalFails("Last_Day(FIELD_INTEGER)");
    evalFails("Last_Day(FIELD_STRING)");
    evalFails("Last_Day(FIELD_DATE, FIELD_INTEGER)");
    evalFails("Last_Day(FIELD_DATE, NULL)");
    evalFails("Last_Day(FIELD_DATE, HOUR)");
  }

  @Test
  public void Next_Day() throws Exception {
    evalEquals("Next_Day(DATE '2020-02-28','monday')", LocalDate.of(2020, Month.MARCH, 2)).returnType(DateType.DATE);
    evalEquals("Next_Day(FIELD_DATE,'monday')", LocalDate.of(1981, Month.JUNE, 29));
    evalEquals("Next_Day(FIELD_TIMESTAMP,'monday')", LocalDate.of(2023, Month.MARCH, 06));

    evalNull("Next_Day(NULL_DATE, 'monday')").returnType(DateType.DATE);
    evalNull("Next_Day(FIELD_DATE, NULL_STRING)");

    evalFails("Next_Day()");
    evalFails("Next_Day(FIELD_INTEGER, 'monday')");
    evalFails("Next_Day(FIELD_STRING, 'monday')");
    evalFails("Next_Day(FIELD_DATE)");
    evalFails("Next_Day(FIELD_DATE, HOUR)");
  }

  @Test
  public void Previous_Day() throws Exception {
    evalEquals("Previous_Day(DATE '2020-02-28','monday')", LocalDate.of(2020, Month.FEBRUARY, 24)).returnType(DateType.DATE);

    evalNull("Previous_Day(NULL_DATE, 'monday')").returnType(DateType.DATE);
    evalNull("Previous_Day(FIELD_DATE, NULL_STRING)");

    evalFails("Previous_Day()");
    evalFails("Previous_Day(FIELD_DATE)");
    evalFails("Previous_Day(FIELD_INTEGER, 'monday')");
  }

  @Test
  public void Normalize() throws Exception {
    evalEquals("Normalize('\u00ea')", "ê").returnType(StringType.STRING);
    evalEquals("Normalize('\u0065\u0302')", "ê");
    evalEquals("Normalize('Jane\u2004Doe', 'NFKC')", "Jane Doe");
    evalEquals("Normalize('Jane\u2006Doe', 'NFKC')", "Jane Doe");
    evalEquals("Normalize('¼', 'NFKC')", "1⁄4");
    evalEquals("Normalize('i⁹', 'NFKC')", "i9");
    evalNull("Normalize(NULL_STRING)");

    evalFails("Normalize()");
    evalFails("Normalize('\u00ea','BAD')");
  }

  @Test
  public void Unaccent() throws Exception {
    evalEquals("Unaccent('ÁÀÂÃÄÅĀĄàáâãäåāą')", "AAAAAAAAaaaaaaaa").returnType(StringType.STRING);
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

    evalNull("Unaccent(NULL_STRING)").returnType(StringType.STRING);
    
    evalFails("Unaccent()");

    // Function repetition
    optimize("Unaccent(Unaccent(FIELD_STRING))", "UNACCENT(FIELD_STRING)");
  }

  @Test
  public void Upper() throws Exception {
    evalEquals("Upper('test')", "TEST").returnType(StringType.STRING);
    evalNull("Upper(NULL_STRING)").returnType(StringType.STRING);

    evalFails("Upper()");

    // Function repetition
    optimize("UPPER(UPPER(FIELD_STRING))", "UPPER(FIELD_STRING)");
    optimize("UPPER(LOWER(FIELD_STRING))", "UPPER(FIELD_STRING)");
    optimize("UPPER(INITCAP(FIELD_STRING))", "UPPER(FIELD_STRING)");
  }

  @Test
  public void InitCap() throws Exception {
    evalEquals("InitCap('hello the wORLD')", "Hello The World").returnType(StringType.STRING);
    evalEquals("InitCap('tRy a littlE  ')", "Try A Little  ");
    evalEquals("InitCap('won''t it?no')", "Won'T It?No");
    evalEquals("InitCap('ÉéÀàè]çÂâ ÊêÎÔô ÛûËÏ ïÜŸÇç ŒœÆæ')", "Ééààè]Çââ Êêîôô Ûûëï Ïüÿçç Œœææ");
    evalNull("InitCap(NULL_STRING)").returnType(StringType.STRING);
    evalFails("InitCap()");

    // Function repetition
    optimize("INITCAP(LOWER(FIELD_STRING))", "INITCAP(FIELD_STRING)");
    optimize("INITCAP(UPPER(FIELD_STRING))", "INITCAP(FIELD_STRING)");
    optimize("INITCAP(INITCAP(FIELD_STRING))", "INITCAP(FIELD_STRING)");
  }

  @Test
  public void Instr() throws Exception {
    evalEquals("Instr('CORPORATE FLOOR','OR')", 2L).returnType(IntegerType.INTEGER);
    evalEquals("Instr('CORPORATE FLOOR','or')", 0L);
    evalEquals("Instr('CORPORATE FLOOR','ORA')", 5L);
    evalEquals("Instr('CORPORATE FLOOR','ORA',6)", 0L);
    evalEquals("Instr('CORPORATE FLOOR','OR',5)", 5L);
    evalEquals("Instr('CORPORATE FLOOR','OR',6)", 14L);
    evalEquals("Instr('CORPORATE FLOOR','OR',3)", 5L);
    evalEquals("Instr('CORPORATE FLOOR','OR',3, 1)", 5L);
    evalEquals("Instr('CORPORATE FLOOR','OR',3, 2)", 14L);
    evalEquals("Instr('CORPORATE FLOOR','OR',3, 3)", 0L);

    evalEquals("Instr('CORPORATE FLOOR','O',-1)", 14L);
    evalEquals("Instr('CORPORATE FLOOR','O',-2)", 13L);
    evalEquals("Instr('CORPORATE FLOOR','O',-3)", 5L);
    evalEquals("Instr('CORPORATE FLOOR','O',-4)", 5L);
    evalEquals("Instr('CORPORATE FLOOR','OR',-1)", 14L);
    evalEquals("Instr('CORPORATE FLOOR','OR',-3)", 5L);
    evalEquals("Instr('CORPORATE FLOOR','OR',-12)", 2L);
    evalEquals("Instr('CORPORATE FLOOR','OR',-3, 1)", 5L);
    evalEquals("Instr('CORPORATE FLOOR','OR',-3, 2)", 2L);
    evalEquals("Instr('CORPORATE FLOOR','OR',-3, 3)", 0L);

    evalNull("Instr(NULL_STRING,'test')");
    evalNull("Instr('test',NULL_STRING)");
    evalNull("Instr(NULL_STRING,NULL_STRING)");

    evalFails("Instr('CORPORATE FLOOR','OR',-3, 0)");
    evalFails("Instr('CORPORATE FLOOR','OR',0)");
    evalFails("Instr()");
  }

  @Test
  public void RPad() throws Exception {
    evalEquals("RPad('test',7)", "test   ").returnType(StringType.STRING);
    evalEquals("RPad('test',7,'*')", "test***");
    evalEquals("RPad('test',4,'*')", "test");
    evalEquals("RPad('test',3,'*')", "tes");
    evalEquals("RPad('test',4,'ABC')", "test");
    evalEquals("RPad('test',6,'ABC')", "testAB");
    evalEquals("RPad('test',7,'ABC')", "testABC");
    evalEquals("RPad('test',8,'ABC')", "testABCA");

    evalEquals("RPad(BINARY '1A2B3C',2,BINARY '4D5E6F')", new byte[] {0x1A, 0x2B}).returnType(BinaryType.BINARY);
    evalEquals("RPad(BINARY '1A2B3C',3,BINARY '4D5E6F')", new byte[] {0x1A, 0x2B, 0x3C});
    evalEquals("RPad(BINARY '1A2B3C',4,BINARY '4D5E6F')", new byte[] {0x1A, 0x2B, 0x3C, 0x4D});
    evalEquals("RPad(BINARY '1A2B3C',5,BINARY '4D5E6F')",
        new byte[] {0x1A, 0x2B, 0x3C, 0x4D, 0x5E});
    evalEquals("RPad(BINARY '1A2B3C',6,BINARY '4D5E6F')",
        new byte[] {0x1A, 0x2B, 0x3C, 0x4D, 0x5E, 0x6F});
    evalEquals("RPad(BINARY '1A2B3C',7,BINARY '4D5E6F')",
        new byte[] {0x1A, 0x2B, 0x3C, 0x4D, 0x5E, 0x6F, 0x4D});

    // Empty padding
    evalEquals("RPad('test',5,'')", "test");
    evalEquals("RPad(BINARY '2A3B4C',5,BINARY '')", new byte[] {0x2A, 0x3B, 0x4C});

    // If length is a negative number, the result of the function is an empty string or binary.
    evalEquals("RPad('test',-8)", "");
    evalEquals("RPad(FIELD_BINARY,-8)", new byte[0]);

    evalNull("RPad(NULL_STRING,2)").returnType(StringType.STRING);
    evalNull("RPad(NULL_BINARY,2)").returnType(BinaryType.BINARY);
    evalNull("RPad(NULL_STRING,-8)");
    evalNull("RPad(NULL_BINARY,-8)");

    // Missing length
    evalFails("RPad('test')");

    // Test PAD_LIMIT
    evalFails("RPad('test',10000)");
    evalFails("RPad(FIELD_BINARY,10000)");
  }

  @Test
  public void LPad() throws Exception {
    evalEquals("LPad('test',6)", "  test").returnType(StringType.STRING);
    evalEquals("LPad('test',7,'*')", "***test");
    evalEquals("LPad('test',3,'*')", "tes");
    evalEquals("LPad('test',4,'ABC')", "test");
    evalEquals("LPad('test',6,'ABC')", "ABtest");
    evalEquals("LPad('test',7,'ABC')", "ABCtest");
    evalEquals("LPad('test',8,'ABC')", "ABCAtest");

    evalEquals("LPad(BINARY '1A2B3C',2,BINARY '4D5E6F')", new byte[] {0x1A, 0x2B}).returnType(BinaryType.BINARY);
    evalEquals("LPad(BINARY '1A2B3C',3,BINARY '4D5E6F')", new byte[] {0x1A, 0x2B, 0x3C});
    evalEquals("LPad(BINARY '1A2B3C',4,BINARY '4D5E6F')", new byte[] {0x4D, 0x1A, 0x2B, 0x3C});
    evalEquals("LPad(BINARY '1A2B3C',5,BINARY '4D5E6F')",
        new byte[] {0x4D, 0x5E, 0x1A, 0x2B, 0x3C});
    evalEquals("LPad(BINARY '1A2B3C',6,BINARY '4D5E6F')",
        new byte[] {0x4D, 0x5E, 0x6F, 0x1A, 0x2B, 0x3C});
    evalEquals("LPad(BINARY '1A2B3C',7,BINARY '4D5E6F')",
        new byte[] {0x4D, 0x5E, 0x6F, 0x4D, 0x1A, 0x2B, 0x3C});

    // Empty padding
    evalEquals("LPad('test',5,'')", "test");
    evalEquals("LPad(BINARY '2A3B4C',5,BINARY '')", new byte[] {0x2A, 0x3B, 0x4C});

    // If length is a negative number, the result of the function is an empty string or binary.
    evalEquals("LPad('test',-8)", "");

    evalNull("LPad(NULL_STRING,2)").returnType(StringType.STRING);
    evalNull("LPad(NULL_STRING,-8)");
    evalNull("LPad(NULL_BINARY,2)").returnType(BinaryType.BINARY);
    evalNull("LPad(NULL_BINARY,-8)");

    evalFails("LPad('test')");

    // Test PAD_LIMIT
    evalFails("LPad('test',10000)");
  }

  @Test
  public void Year() throws Exception {
    evalEquals("Year(DATE '2019-01-01')", 2019L).returnType(IntegerType.INTEGER);
    evalNull("Year(NULL_DATE)").returnType(IntegerType.INTEGER);
    evalFails("Year()");
  }

  @Test
  public void MonthName() throws Exception {
    evalEquals("MonthName(DATE '2019-01-01')", "January").returnType(StringType.STRING);
    evalEquals("MonthName(DATE '2019-12-28')", "December");
    evalNull("MonthName(NULL_DATE)");
    evalFails("MonthName()");
  }

  @Test
  public void DayName() throws Exception {
    evalEquals("DayName(DATE '2019-01-01')", "Tuesday").returnType(StringType.STRING);
    evalEquals("DayName(DATE '2019-12-28')", "Saturday");
    evalNull("DayName(NULL_DATE)");
    evalFails("DayName()");
  }

  @Test
  public void Month() throws Exception {
    evalEquals("Month(DATE '2019-01-01')", 1L).returnType(IntegerType.INTEGER);
    evalEquals("Month(DATE '2020-02-23')", 2L);
    evalEquals("Month(DATE '2019-12-28')", 12L);
    evalNull("Month(NULL_DATE)").returnType(IntegerType.INTEGER);
    evalFails("Month()");
    evalFails("Month(FIELD_INTEGER)");
  }

  @Test
  public void Date_Diff() throws Exception {
    evalEquals("Date_Diff(MILLENNIUM, DATE '1001-01-01',DATE '3150-01-01')", 2L).returnType(IntegerType.INTEGER);
    evalEquals("Date_Diff(CENTURY, DATE '1001-01-01',DATE '2000-01-01')", 9L);
    evalEquals("Date_Diff(DECADE, DATE '1001-01-01',DATE '2000-01-01')", 99L);
    evalEquals("Date_Diff(YEAR, TIMESTAMP '2001-01-01 12:00:00',DATE '2000-01-01')", -1L);
    evalEquals("Date_Diff(YEAR, DATE '2022-12-31',DATE '2024-06-01')", 1L);
    evalEquals("Date_Diff(QUARTER, DATE '2022-12-31',DATE '2024-06-01')", 5L);
    evalEquals("Date_Diff(MONTH, TIMESTAMP '2001-01-01 12:00:00',DATE '2000-01-01')", -12L);
    evalEquals("Date_Diff(WEEK, TIMESTAMP '2001-01-01 12:00:00',DATE '2000-01-01')", -52L);
    evalEquals("Date_Diff(DAY, DATE '2021-11-09',DATE '2020-12-28')", -316L);
    evalEquals("Date_Diff(HOUR, TIMESTAMP '2019-01-01 15:00:59',TIMESTAMP '2019-01-02 15:00:59')",
        24L);
    evalEquals("Date_Diff(MINUTE, TIMESTAMP '2019-01-01 15:00:59',TIMESTAMP '2019-01-02 15:00:59')",
        1440L);
    evalEquals("Date_Diff(SECOND, TIMESTAMP '2019-01-01 15:00:59',TIMESTAMP '2019-01-02 15:00:59')",
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

    evalNull("Date_Diff(YEAR, NULL_DATE, DATE '2007-11-09')").returnType(IntegerType.INTEGER);
    evalNull("Date_Diff(YEAR, DATE '2007-11-09',NULL_DATE)");
    evalNull("Date_Diff(YEAR, NULL_DATE, NULL_DATE)");

    evalFails("Date_Diff(YEAR, DATE '2007-11-09')");
  }

  @Test
  public void Years_Between() throws Exception {
    evalEquals("Years_Between(TIMESTAMP '2001-01-01 12:00:00',TIMESTAMP '2000-01-01 00:00:00')",
        -1L);
    evalNull("Years_Between(NULL_DATE, DATE '2007-11-09')");
    evalNull("Years_Between(DATE '2007-11-09',NULL_DATE)");
    evalNull("Years_Between(NULL_DATE, NULL_DATE)");
    evalFails("Years_Between(DATE '2007-11-09')");
  }

  @Test
  public void Months_Between() throws Exception {
    evalEquals("Months_Between(DATE '2005-01-01',DATE '2005-02-02')", 1.032258064516129).returnType(NumberType.NUMBER);
    evalEquals("Months_Between(DATE '2007-11-09',DATE '2003-12-28')", -45.54838709677419);

    // The time difference is ignored because the day of the month is the same for both values.
    // evalEquals("Months_Between(TIMESTAMP '2007-12-13-09.40.30',TIMESTAMP '2007-11-13-08.40.30')",
    // 1.0);

    // evalEquals("Months_Between(DATE '2007-11-10',DATE '2007-12-09')", -0.967742);
    // TODO: If the months and days are identical, the result is an integer.
    evalEquals("Months_Between(DATE '2007-11-09',DATE '2007-12-09')", 0.967741935483871);

    evalNull("Months_Between(DATE '2007-11-09',NULL_DATE)");
    evalNull("Months_Between(NULL_DATE, DATE '2007-11-09')");
    evalNull("Months_Between(NULL_DATE, NULL_DATE)");
    evalFails("Months_Between(DATE '2007-11-09')");
  }

  @Test
  public void Days_Between() throws Exception {
    evalEquals("Days_Between(DATE '2021-01-01',DATE '2021-01-01')", 0L);
    evalEquals("Days_Between(DATE '2021-11-09',DATE '2020-12-28')", -316L);
    evalEquals("Days_Between(DATE '2007-11-09',DATE '2007-12-09')", 30L);

    evalNull("Days_Between(DATE '2007-11-09',NULL_DATE)");
    evalNull("Days_Between(NULL_DATE, Date '2007-11-09')");
    evalNull("Days_Between(NULL_DATE, NULL_DATE)");
    evalFails("Days_Between(DATE '2007-11-09')");
  }

  @Test
  public void Hours_Between() throws Exception {
    evalEquals("Hours_Between(TIMESTAMP '2019-01-01 15:00:59',TIMESTAMP '2019-01-01 15:28:59')",
        0L);
    evalEquals("Hours_Between(TIMESTAMP '2019-01-01 15:00:59',TIMESTAMP '2019-01-02 15:00:59')",
        24L);
    evalNull("Hours_Between(NULL_TIMESTAMP, TIMESTAMP '2019-01-01 15:00:59')");
    evalNull("Hours_Between(TIMESTAMP '2019-01-01 15:00:59', NULL_TIMESTAMP)");
    evalFails("Hours_Between(DATE '2007-11-09')");
  }

  @Test
  public void Minutes_Between() throws Exception {
    evalEquals("Minutes_Between(TIMESTAMP '2019-01-01 15:00:59',TIMESTAMP '2019-01-01 15:28:59')",
        28L);
    evalEquals("Minutes_Between(TIMESTAMP '2019-01-01 15:00:59',TIMESTAMP '2019-01-02 15:00:59')",
        1440L);
    evalNull("Minutes_Between(NULL_DATE, TIMESTAMP '2019-01-01 15:00:59')");
    evalNull("Minutes_Between(TIMESTAMP '2019-01-01 15:00:59', NULL_DATE)");
    evalFails("Minutes_Between(DATE '2007-11-09')");
  }

  @Test
  public void Seconds_Between() throws Exception {
    evalEquals("Seconds_Between(TIMESTAMP '2019-01-01 15:00:59',TIMESTAMP '2019-01-01 15:28:59')",
        28L * 60L);
    evalEquals("Seconds_Between(TIMESTAMP '2019-01-01 15:00:59',TIMESTAMP '2019-01-02 15:00:59')",
        86400L);
    evalNull("Seconds_Between(NULL_DATE, TIMESTAMP '2019-01-01 15:00:59')");
    evalNull("Seconds_Between(TIMESTAMP '2019-01-01 15:00:59', NULL_DATE)");
    evalFails("Seconds_Between(DATE '2007-11-09')");
  }

  @Test
  public void Quarter() throws Exception {
    evalEquals("Quarter(DATE '2019-01-01')", 1L).returnType(IntegerType.INTEGER);
    evalEquals("Quarter(DATE '2019-02-28')", 1L);
    evalEquals("Quarter(DATE '2019-04-28')", 2L);
    evalEquals("Quarter(DATE '2019-08-28')", 3L);
    evalEquals("Quarter(DATE '2019-12-28')", 4L);

    evalNull("Quarter(NULL_DATE)").returnType(IntegerType.INTEGER);

    evalFails("Quarter()");
    evalFails("Quarter(FIELD_STRING)");
    evalFails("Quarter(FIELD_INTEGER)");
    evalFails("Quarter(FIELD_NUMBER)");
  }

  @Test
  public void DayOfWeek() throws Exception {
    evalEquals("DayOfWeek(DATE '2019-01-01')", 3L).returnType(IntegerType.INTEGER);
    evalEquals("DayOfWeek(DATE '2019-07-27')", 7L);
    evalEquals("DayOfWeek(DATE '2019-07-28')", 1L);
    evalEquals("DayOfWeek(DATE '2019-12-31')", 3L);
    evalEquals("DayOfWeek(FIELD_DATE)", 3L);
    evalEquals("DayOfWeek(FIELD_TIMESTAMP)", 3L);

    evalNull("DayOfWeek(NULL_DATE)").returnType(IntegerType.INTEGER);

    evalFails("DayOfWeek()");
  }

  @Test
  public void Day() throws Exception {
    evalEquals("Day(DATE '2019-01-01')", 1L).returnType(IntegerType.INTEGER);
    evalEquals("Day(DATE '2019-02-28')", 28L);
    evalEquals("Day(DATE '2019-12-28')", 28L);
    evalEquals("Day(FIELD_DATE)", 23L);
    evalEquals("Day(FIELD_TIMESTAMP)", 28L);
    evalNull("Day(NULL_DATE)").returnType(IntegerType.INTEGER);

    evalFails("Day()");
    evalFails("Day(123)");
    evalFails("Day('text')");

    optimize("DAY(DATE '2019-02-15')", "15");
    optimize("DAY(MAKE_DATE(2019,2,15))", "15");
  }

  @Test
  public void DayOfYear() throws Exception {
    evalEquals("DayOfYear(DATE '2019-01-01')", 1L).returnType(IntegerType.INTEGER);
    evalEquals("DayOfYear(DATE '2019-12-31')", 365L);
    evalEquals("DayOfYear(FIELD_DATE)", 174L);
    evalEquals("DayOfYear(FIELD_TIMESTAMP)", 59L);
    evalNull("DayOfYear(NULL_DATE)").returnType(IntegerType.INTEGER);
    evalFails("DayOfYear()");
    evalFails("DayOfYear(123)");
  }

  @Test
  public void Julian_Day() throws Exception {
    evalEquals("Julian_Day(DATE '2021-06-23')", 2459389L).returnType(IntegerType.INTEGER);
    evalEquals("Julian_Day(TIMESTAMP  '2021-06-23 8:00:00' at time zone 'UTC+12')", 2459389L);
    evalNull("Julian_Day(NULL_DATE)").returnType(IntegerType.INTEGER);
    evalFails("Julian_Day()");
    evalFails("Julian_Day(123)");
  }

  @Test
  public void Week() throws Exception {
    evalEquals("Week(DATE '2015-12-31')", 53L).returnType(IntegerType.INTEGER);
    evalEquals("Week(DATE '2015-01-01')", 1L);
    evalEquals("Week(DATE '2015-01-02')", 1L);
    evalNull("Week(NULL_DATE)").returnType(IntegerType.INTEGER);

    evalFails("Week()");
    evalFails("Week(123)");
  }

  @Test
  public void IsoDayOfWeek() throws Exception {
    evalEquals("IsoDayOfWeek(DATE '2003-12-28')", 7L).returnType(IntegerType.INTEGER);
    evalNull("IsoDayOfWeek(NULL_DATE)").returnType(IntegerType.INTEGER);
    evalFails("IsoDayOfWeek()");
  }

  @Test
  public void IsoWeek() throws Exception {
    evalEquals("IsoWeek(DATE '2015-12-31')", 53L).returnType(IntegerType.INTEGER);
    evalEquals("IsoWeek(DATE '2016-01-01')", 53L);
    evalEquals("IsoWeek(DATE '2016-01-02')", 53L);
    evalEquals("IsoWeek(DATE '2016-01-03')", 53L);
    evalEquals("IsoWeek(DATE '2016-01-04')", 1L);
    evalNull("IsoWeek(NULL_DATE)").returnType(IntegerType.INTEGER);
    evalFails("IsoWeek()");
  }

  @Test
  public void IsoYear() throws Exception {
    evalEquals("IsoYear(DATE '2015-12-31')", 2015L).returnType(IntegerType.INTEGER);
    evalEquals("IsoYear(DATE '2016-01-01')", 2015L);
    evalEquals("IsoYear(DATE '2016-01-02')", 2015L);
    evalEquals("IsoYear(DATE '2016-01-04')", 2016L);
    evalEquals("IsoYear(DATE '2042-12-31')", 2043L);
    evalNull("IsoYear(NULL_DATE)").returnType(IntegerType.INTEGER);
    evalFails("IsoYear('ERROR')");
    evalFails("IsoYear()");
  }

  @Test
  public void Add_Years() throws Exception {
    evalEquals("Add_Years(DATE '2019-01-15',1)", LocalDate.of(2020, Month.JANUARY, 15));
    evalEquals("Add_Years(DATE '2019-01-15',-2)", LocalDate.of(2017, Month.JANUARY, 15));
    evalEquals("Add_Years(DATE '2019-11-15',3)", LocalDate.of(2022, Month.NOVEMBER, 15));
    // the resulting month has fewer days
    evalEquals("Add_Years(DATE '2020-02-29',1)", LocalDate.of(2021, Month.FEBRUARY, 28));

    evalNull("Add_Years(NULL_DATE,140)");
    evalNull("Add_Years(DATE '2019-01-15',NULL_INTEGER)");

    evalFails("Add_Years(DATE '2019-01-15')");
    evalFails("Add_Years()");
    optimize("Add_Years(FIELD_DATE,0)", "FIELD_DATE");
  }

  @Test
  public void Add_Months() throws Exception {
    evalEquals("Add_Months(DATE '2019-01-15',1)", LocalDate.of(2019, Month.FEBRUARY, 15));
    evalEquals("Add_Months(DATE '2019-01-15',-2)", LocalDate.of(2018, Month.NOVEMBER, 15));
    evalEquals("Add_Months(DATE '2019-11-15',3)", LocalDate.of(2020, Month.FEBRUARY, 15));

    // the resulting month has fewer days
    evalEquals("Add_Months(DATE '2019-01-31',1)", LocalDate.of(2019, Month.FEBRUARY, 28));
    evalNull("Add_Months(NULL_DATE,140)");
    evalNull("Add_Months(DATE '2019-01-15',NULL_INTEGER)");
    evalFails("Add_Months(DATE '2019-01-15')");
    optimize("Add_Months(FIELD_DATE,0)", "FIELD_DATE");
  }

  @Test
  public void Add_Weeks() throws Exception {
    evalEquals("Add_Weeks(DATE '2019-01-15',1)", LocalDate.of(2019, Month.JANUARY, 22));
    evalEquals("Add_Weeks(DATE '2019-01-15',-3)", LocalDate.of(2018, Month.DECEMBER, 25));

    evalNull("Add_Weeks(NULL_DATE,140)");
    evalNull("Add_Weeks(DATE '2019-01-15',NULL_INTEGER)");
    evalFails("Add_Weeks(DATE '2019-01-15')");
    optimize("Add_Weeks(FIELD_DATE,0)", "FIELD_DATE");
  }

  @Test
  public void Add_Days() throws Exception {
    evalEquals("Add_Days(DATE '2019-01-15',1)", LocalDate.of(2019, Month.JANUARY, 16));
    evalEquals("Add_Days(DATE '2019-01-15',-20)", LocalDate.of(2018, Month.DECEMBER, 26));
    evalEquals("Add_Days(TIMESTAMP '2021-01-01 15:28:59+0200',20)",
        ZonedDateTime.of(2021, 1, 21, 15, 28, 59, 0, ZoneOffset.ofHoursMinutes(2, 0)));

    evalNull("Add_Days(NULL_DATE,140)");
    evalNull("Add_Days(DATE '2019-01-15',NULL_INTEGER)");
    evalFails("Add_Days(DATE '2019-01-15')");

    optimize("Add_Days(FIELD_DATE,0)", "FIELD_DATE");
  }

  @Test
  public void Add_Hours() throws Exception {
    evalEquals("Add_Hours(DATE '2019-01-15',1)",
        LocalDateTime.of(2019, Month.JANUARY, 15, 1, 0, 0, 0));
    evalEquals("Add_Hours(TIMESTAMP '2021-01-01 15:28:59+0200',2)",
        ZonedDateTime.of(2021, 1, 1, 17, 28, 59, 0, ZoneOffset.ofHoursMinutes(2, 0)));
    evalNull("Add_Hours(NULL_DATE,140)");
    evalNull("Add_Hours(DATE '2019-01-15',NULL_INTEGER)");
    evalFails("Add_Hours(DATE '2019-01-15')");

    optimize("Add_Hours(FIELD_DATE,0)", "FIELD_DATE");
  }

  @Test
  public void Add_Minutes() throws Exception {
    evalEquals("Add_Minutes(DATE '2019-01-15',20)",
        LocalDateTime.of(2019, Month.JANUARY, 15, 0, 20, 0, 0));
    evalNull("Add_Minutes(NULL_DATE,140)");
    evalNull("Add_Minutes(DATE '2019-01-15',NULL_INTEGER)");
    evalFails("Add_Minutes(DATE '2019-01-15')");
    optimize("Add_Minutes(FIELD_DATE,0)", "FIELD_DATE");
  }

  @Test
  public void Add_Seconds() throws Exception {
    evalEquals("Add_Seconds(DATE '2019-01-15',20)",
        LocalDateTime.of(2019, Month.JANUARY, 15, 0, 0, 20));
    evalEquals("Add_Seconds(DATE '2019-01-15',140)",
        LocalDateTime.of(2019, Month.JANUARY, 15, 0, 2, 20));
    evalNull("Add_Seconds(NULL_DATE,140)");
    evalNull("Add_Seconds(DATE '2019-01-15',NULL_INTEGER)");
    evalFails("Add_Seconds(DATE '2019-01-15')");
  }

  @Test
  public void Add_Nanoseconds() throws Exception {
    evalEquals("Add_NanoSeconds(DATE '2019-01-15',20)",
        LocalDateTime.of(2019, Month.JANUARY, 15, 0, 0, 0, 20));
    evalEquals("Add_NanoSeconds(DATE '2019-01-15',140)",
        LocalDateTime.of(2019, Month.JANUARY, 15, 0, 0, 0, 140));
    evalNull("Add_NanoSeconds(NULL_DATE,140)");
    evalNull("Add_NanoSeconds(DATE '2019-01-15',NULL_INTEGER)");
    evalFails("Add_NanoSeconds(DATE '2019-01-15')");
    optimize("Add_NanoSeconds(FIELD_DATE,0)", "FIELD_DATE");
  }

  @Test
  public void Date_Add() throws Exception {
    evalEquals("DATE_ADD(YEAR,1,DATE '2020-02-29')", LocalDate.of(2021, Month.FEBRUARY, 28));
    evalEquals("DATE_ADD(QUARTER,1,DATE '2019-11-15')", LocalDate.of(2020, Month.FEBRUARY, 15));
    evalEquals("DATE_ADD(MONTH,3,DATE '2019-11-15')", LocalDate.of(2020, Month.FEBRUARY, 15));
    evalEquals("DATE_ADD(WEEK,-3,DATE '2019-01-15')", LocalDate.of(2018, Month.DECEMBER, 25));
    evalEquals("DATE_ADD(DAY,-20,DATE '2019-01-15')", LocalDate.of(2018, Month.DECEMBER, 26));
    evalEquals("DATE_ADD(HOUR,1,DATE '2019-01-15')",
        LocalDateTime.of(2019, Month.JANUARY, 15, 1, 0, 0, 0));
    evalEquals("DATE_ADD(MINUTE,20,DATE '2019-01-15')",
        LocalDateTime.of(2019, Month.JANUARY, 15, 0, 20, 0, 0));
    evalEquals("DATE_ADD(SECOND,140,DATE '2019-01-15')",
        LocalDateTime.of(2019, Month.JANUARY, 15, 0, 2, 20, 0));
    evalEquals("DATE_ADD(NANOSECOND,23,DATE '2019-01-15')",
        LocalDateTime.of(2019, Month.JANUARY, 15, 0, 0, 0, 23));

    optimize("DATE_ADD(YEAR,1,FIELD_DATE)", "ADD_YEARS(FIELD_DATE,1)");
    optimize("DATE_ADD(QUARTER,1,FIELD_DATE)", "ADD_QUARTERS(FIELD_DATE,1)");
    optimize("DATE_ADD(MONTH,1,FIELD_DATE)", "ADD_MONTHS(FIELD_DATE,1)");
    optimize("DATE_ADD(HOUR,1,FIELD_DATE)", "ADD_HOURS(FIELD_DATE,1)");
    optimize("DATE_ADD(MINUTE,1,FIELD_DATE)", "ADD_MINUTES(FIELD_DATE,1)");
    optimize("DATE_ADD(SECOND,1,FIELD_DATE)", "ADD_SECONDS(FIELD_DATE,1)");
  }

  @Test
  public void Hour() throws Exception {
    evalEquals("Hour(TIMESTAMP '2019-01-01 15:28:59')", 15L);
    evalNull("Hour(NULL_DATE)");
    evalFails("Hour()");
  }

  @Test
  public void Minute() throws Exception {
    evalEquals("Minute(TIMESTAMP '2019-01-01 15:28:59')", 28L);
    evalNull("Minute(NULL_DATE)");
    evalFails("Minute()");
  }

  @Test
  public void Second() throws Exception {
    evalEquals("Second(TIMESTAMP '2019-01-01 15:28:59')", 59L);
    evalNull("Second(NULL_DATE)");
    evalFails("Second()");
  }

  @Test
  public void Lower() throws Exception {
    evalEquals("Lower('TesT')", "test").returnType(StringType.STRING);
    evalNull("Lower(NULL_STRING)").returnType(StringType.STRING);
    evalFails("Lower()");
    evalFails("Lower('Test','Test')");

    // Function repetition
    optimize("LOWER(LOWER(FIELD_STRING))", "LOWER(FIELD_STRING)");
    optimize("LOWER(UPPER(FIELD_STRING))", "LOWER(FIELD_STRING)");
    optimize("LOWER(INITCAP(FIELD_STRING))", "LOWER(FIELD_STRING)");
  }

  @Test
  public void Squeeze() throws Exception {
    evalEquals("SQUEEZE('   Tes T      ')", "Tes T").returnType(StringType.STRING);
    evalEquals("SQUEEZE(' T  es T ')", "T es T");
    evalEquals("SQUEEZE('T\t es T ')", "T es T");
    evalEquals("SQUEEZE('T \t es T')", "T es T");
    evalEquals("SQUEEZE('T \t es T\n\r')", "T es T");
    evalNull("SQUEEZE(NULL_STRING)").returnType(StringType.STRING);
    evalFails("SQUEEZE()");
    evalFails("SQUEEZE('Test','Test')");

    // Function repetition
    optimize("SQUEEZE(Squeeze(FIELD_STRING))", "SQUEEZE(FIELD_STRING)");
  }

  @Test
  public void Substring() throws Exception {
    evalEquals("Substring('TEST FROM',6)", "FROM").returnType(StringType.STRING);
    evalEquals("Substring('TEST FROM',6,2)", "FR");
    evalEquals("Substring('TEST FROM',1,4)", "TEST");
    evalEquals("Substring('TEST FROM',-4)", "FROM");
    evalEquals("Substring('ABCDEFG',1,1)", "A");
    evalNull("Substring(NULL_STRING,1,1)").returnType(StringType.STRING);

    // Compatibility mode
    evalEquals("Substring('ABCDEFG',0,1)", "A");

    // Alias
    evalEquals("Substr('TEST',5)", "");
  }

  @Test
  public void Split_part() throws Exception {
    evalEquals("Split_Part('127.1.2.3','.',1)", "127").returnType(StringType.STRING);
    evalEquals("Split_Part('127.1.2.3','.',2)", "1");
    evalEquals("Split_Part('127.1.2.3','.',4)", "3");
    evalEquals("Split_Part('127.1.2.3','.',-1)", "3");
    evalEquals("Split_Part('127.1.2.3','.',-2)", "2");
    evalEquals("SPLIT_PART('user@hop.apache.org', '.', -2)", "apache");
    evalEquals("Split_Part('AAA-@-BBB-BBB-@-CCC','-@-',2)", "BBB-BBB");

    // No split if delimiter is empty
    evalEquals("Split_Part('127.1.2.3','',1)", "127.1.2.3");

    // If the part index is out of range, the returned value is an empty string.
    evalEquals("Split_Part('127.1.2.3','.',5)", "");

    evalNull("Split_Part(NULL_STRING,'.',5)");
    evalNull("Split_Part('127.1.2.3',NULL_STRING,5)");
    evalNull("Split_Part('127.1.2.3','.',NULL_INTEGER)");

    evalFails("Split_Part('127.1.2.3','.')");
  }

  @Test
  public void Strtok() throws Exception {
    evalEquals("Strtok('127.1-2-3','.-:',1)", "127").returnType(StringType.STRING);
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
    evalNull("Strtok('127.1.2.3','.',5)").returnType(StringType.STRING);
    evalNull("Strtok('','',1)");

    // If one operands is null
    evalNull("Strtok(NULL_STRING,'.',5)").returnType(StringType.STRING);
    evalNull("Strtok('127.1.2.3',NULL_STRING,5)").returnType(StringType.STRING);
    evalNull("Strtok('127.1.2.3','.',NULL_INTEGER)").returnType(StringType.STRING);

    evalFails("Strtok()");
    evalFails("Strtok('127.1.2.3','.',5,5)");
  }

  @Test
  public void Space() throws Exception {
    evalEquals("Space(4)", "    ").returnType(StringType.STRING);
    evalEquals("Space(0)", "");
    evalNull("Space(-3)");
    evalNull("Space(NULL_INTEGER)");
    evalFails("Space()");
    evalFails("Space('str')");
  }

  @Test
  public void Abs() throws Exception {
    evalEquals("Abs(0)", 0L).returnType(IntegerType.of(1));
    evalEquals("Abs(1)", 1L).returnType(IntegerType.of(1));
    evalEquals("Abs(-1)", 1L).returnType(IntegerType.of(1));
    evalEquals("Abs(FIELD_INTEGER)", 40L).returnType(IntegerType.INTEGER);
    evalEquals("Abs(FIELD_NUMBER)", 5.12D).returnType(NumberType.NUMBER);
    evalEquals("Abs(FIELD_STRING_NUMBER)", 12.56D).returnType(NumberType.NUMBER);
    evalEquals("Abs(-1::INTEGER)", 1L);
    evalEquals("Abs(-1.12345679)", 1.12345679D);
    evalEquals("Abs(-1.1234567912345679123456791234567912345)",
        new BigDecimal("1.1234567912345679123456791234567912345"));

    evalNull("Abs(NULL_INTEGER)");
    evalNull("Abs(NULL_NUMBER)");
    evalNull("Abs(NULL_BIGNUMBER)");

    // Interval
    evalEquals("Abs(INTERVAL 5 YEARS)", Interval.of(5)).returnType(IntervalType.INTERVAL);
    evalEquals("Abs(INTERVAL -5 YEARS)", Interval.of(5)).returnType(IntervalType.INTERVAL);
    evalNull("Abs(NULL_STRINg::INTERVAL)").returnType(IntervalType.INTERVAL);    
    
    evalFails("Abs()");
    evalFails("Abs(");
    evalFails("Abs(FIELD_STRING)");

    optimize("ABS(-FIELD_INTEGER)");

    // Function repetition
    optimize("ABS(ABS(FIELD_INTEGER))", "ABS(FIELD_INTEGER)");
  }

  @Test
  public void Acos() throws Exception {
    evalEquals("Acos(0)", 1.5707963267948966).returnType(NumberType.NUMBER);
    evalEquals("Acos(1)", 0L).returnType(NumberType.NUMBER);
    evalNull("Acos(NULL_INTEGER)");

    evalFails("Acos(2)");
    evalFails("Acos(-2)");
    evalFails("Acos()");
    evalFails("Acos(FIELD_STRING)");
  }

  @Test
  public void Acosh() throws Exception {
    evalEquals("Acosh(1)", 0L).returnType(NumberType.NUMBER);
    evalEquals("Acosh(3)", new BigDecimal("1.7627471740390860504652186499596")).returnType(NumberType.NUMBER);
    evalNull("Acosh(NULL_INTEGER)").returnType(NumberType.NUMBER);

    evalFails("Acosh()");
    evalFails("Acosh(FIELD_STRING)");
  }

  @Test
  public void Asin() throws Exception {
    evalEquals("Asin(0)", 0L).returnType(NumberType.NUMBER);
    evalEquals("Asin(sin(0.5))", 0.5D);
    evalNull("Asin(NULL_INTEGER)").returnType(NumberType.NUMBER);
    evalFails("Asin()");
    evalFails("Asin(FIELD_STRING)");
  }

  @Test
  public void Asinh() throws Exception {
    evalEquals("Asinh(asin(0.5))", new BigDecimal("0.50221898503461160828703900193479")).returnType(NumberType.NUMBER);
    evalNull("Asinh(NULL_INTEGER)").returnType(NumberType.NUMBER);
    evalFails("Asinh()");
    evalFails("Asinh(FIELD_STRING)");
  }

  @Test
  public void Atan() throws Exception {
    evalEquals("Atan(0.5)", new BigDecimal("0.46364760900080611621425623146121")).returnType(NumberType.NUMBER);
    evalEquals("Atan(Tan(0.5))", 0.5);
    evalNull("Atan(NULL_INTEGER)").returnType(NumberType.NUMBER);
    evalFails("Atan()");
  }

  @Test
  public void Atan2() throws Exception {
    evalEquals("Atan2(0,3)", 0L).returnType(NumberType.NUMBER);
    evalEquals("Atan2(0,-3)", PI);
    evalNull("Atan2(NULL_INTEGER,0)").returnType(NumberType.NUMBER);
    evalNull("Atan2(1,NULL_INTEGER)");

    evalFails("Atan2()");
    evalFails("Atan2(1)");
    evalFails("Atan2(FIELD_STRING)");
  }

  @Test
  public void Atanh() throws Exception {
    evalEquals("Atanh(0.2)", new BigDecimal("0.20273255405408219098900655773217")).returnType(NumberType.NUMBER);
    evalNull("Atanh(NULL_INTEGER)").returnType(NumberType.NUMBER);

    evalFails("Atanh()");
    evalFails("Atanh(FIELD_STRING)");
  }

  @Test
  public void Cos() throws Exception {
    evalEquals("Cos(0)", 1L).returnType(NumberType.NUMBER);
    evalEquals("Cos(1)", new BigDecimal("0.54030230586813971740093660744298")).returnType(NumberType.NUMBER);
    evalEquals("Cos(Pi())", -1L);
    evalNull("Cos(NULL_NUMBER)");
    evalFails("Cos()");
    evalFails("Cos(0,1)");
  }

  @Test
  public void Cosh() throws Exception {
    evalEquals("Cosh(1.234)", new BigDecimal("1.8630338016984225890736437502561")).returnType(NumberType.NUMBER);
    evalEquals("Cosh(0)", 1L);
    evalNull("Cosh(NULL_NUMBER)").returnType(NumberType.NUMBER);
    evalFails("Cosh()");
    evalFails("Cosh(0,1)");
  }

  @Test
  public void Sin() throws Exception {
    evalEquals("Sin(0)", 0L);
    evalEquals("Sin(1)", new BigDecimal("0.84147098480789650665250232163030")).returnType(NumberType.NUMBER);
    evalEquals("Sin(Pi()/2)", 1L);
    evalNull("Sin(NULL_NUMBER)").returnType(NumberType.NUMBER);
    evalFails("Sin()");
    evalFails("Sin(0,1)");
  }

  @Test
  public void Sinh() throws Exception {
    evalEquals("Sinh(84.4)", new BigDecimal("2.2564425307670914188455367832027E+36")).returnType(NumberType.NUMBER);
    evalEquals("Sinh(0)", 0L);
    evalNull("Sinh(NULL_NUMBER)").returnType(NumberType.NUMBER);
    evalFails("Sinh()");
    evalFails("Sinh(FIELD_STRING)");
    evalFails("Sinh(0,1)");
  }

  @Test
  public void Cot() throws Exception {
    evalEquals("Cot(1)", new BigDecimal("0.64209261593433070300641998659427")).returnType(NumberType.NUMBER);
    // evalEquals("Cot(0)", Double.POSITIVE_INFINITY);
    evalNull("Cot(NULL_NUMBER)").returnType(NumberType.NUMBER);

    evalFails("Cot(0)");
    evalFails("Cot()");
    evalFails("Cot(FIELD_STRING)");
    evalFails("Cot(1,0)");
  }

  @Test
  public void Csc() throws Exception {
    evalEquals("Csc(Pi()/2)", 1L).returnType(NumberType.NUMBER);
    evalNull("Csc(NULL_INTEGER)").returnType(NumberType.NUMBER);

    evalFails("Csc(0)");
    evalFails("Csc()");
    evalFails("Csc(FIELD_STRING)");
    evalFails("Csc(1,0)");
  }

  @Test
  public void Csch() throws Exception {
    evalEquals("Csch(1.5)", new BigDecimal("0.4696424405952245847295644318870206")).returnType(NumberType.NUMBER);
    evalEquals("Csch(Pi())", 0L).returnType(NumberType.NUMBER);
    evalNull("Csch(NULL_INTEGER)");

    evalFails("Csch(0)");
    evalFails("Csch()");
    evalFails("Csch(FIELD_STRING)");
    evalFails("Csch(1,0)");
  }

  @Test
  public void Sec() throws Exception {
    evalEquals("Sec(Pi())", -1L).returnType(NumberType.NUMBER);

    evalNull("Sec(NULL_INTEGER)").returnType(NumberType.NUMBER);
    evalFails("Sec(0)");
    evalFails("Sec()");
    evalFails("Sec(FIELD_STRING)");
    evalFails("Sec(1,0)");
  }


  @Test
  public void Sech() throws Exception {
    evalEquals("Sech(0)", 1L).returnType(NumberType.NUMBER);
    evalEquals("Sech(1)", 0L);
    evalNull("Sech(NULL_INTEGER)").returnType(NumberType.NUMBER);

    evalFails("Sech()");
    evalFails("Sech(FIELD_STRING)");
    evalFails("Sech(1,0)");
  }


  @Test
  public void Tan() throws Exception {
    evalEquals("Tan(84.4)", new BigDecimal("-0.45017764606195051960412881423455")).returnType(NumberType.NUMBER);
    evalEquals("Tan(0)", 0L).returnType(NumberType.NUMBER);
    evalNull("Tan(NULL_NUMBER)").returnType(NumberType.NUMBER);
    evalFails("Tan()");
    evalFails("Tan(0,1)");
  }

  @Test
  public void Tanh() throws Exception {
    evalEquals("Tanh(1.234)", new BigDecimal("0.84373566258933019391702000004355")).returnType(NumberType.NUMBER);
    evalEquals("Tanh(0)", 0L);
    evalNull("Tanh(NULL_INTEGER)").returnType(NumberType.NUMBER);
    evalFails("Tanh()");
    evalFails("Tanh(0,1)");
  }

  @Test
  public void Exp() throws Exception {
    evalEquals("Exp(1)", BigDecimalMath.exp(BigDecimal.ONE, Operator.MATH_CONTEXT)).returnType(NumberType.NUMBER);
    evalEquals("Exp(2)", new BigDecimal("7.3890560989306502272304274605750"));

    evalNull("Exp(NULL_INTEGER)").returnType(NumberType.NUMBER);

    evalFails("Exp()");
    evalFails("Exp(1,2)");

    optimize("EXP(1)", "2.7182818284590452353602874713527");
  }

  @Test
  public void Factorial() throws Exception {
    evalEquals("Factorial(0)", 1L);
    evalEquals("Factorial(1)", 1L);
    evalEquals("Factorial(5)", 120L);
    evalEquals("Factorial(10)", 3628800L);
    evalEquals("Factorial(33)", new BigDecimal("8683317618811886495518194401280000000"));
    evalEquals("Factorial(40)", new BigDecimal("815915283247897734345611269596115894272000000000"));

    evalNull("Factorial(NULL_INTEGER)");

    evalFails("Factorial()");
    evalFails("Factorial(-2)");
    evalFails("Factorial(1,2)");
  }

  @Test
  public void Power() throws Exception {
    evalEquals("Power(3,2)", 9L);
    evalEquals("Power(100,0.5)", 10L);
    evalEquals("Power(-4,2)", 16L);
    evalEquals("Power(FIELD_INTEGER,0)", 1L);
    evalEquals("Power(999,0)", 1L);
    evalEquals("Power(2,2.5)", new BigDecimal("5.6568542494923801952067548968388"));
    evalEquals("Power(2,-3)", 0.125D);

    evalNull("Power(NULL_INTEGER,2)");
    evalNull("Power(3,NULL_INTEGER)");

    evalFails("Power()");
    evalFails("Power(3)");
    evalFails("Power(1,2,3)");

    // Alias
    evalEquals("Pow(3,2)", 9L);

    optimize("POWER(FIELD_INTEGER,1)", "FIELD_INTEGER");
  }

  @Test
  public void Sign() throws Exception {
    evalEquals("Sign(0.3)", 1L).returnType(IntegerType.INTEGER);
    evalEquals("Sign(0)", 0L);
    evalEquals("Sign(-5)", -1L);
    evalFails("Sign()");
    evalFails("Sign(1,2)");

    evalNull("Sign(NULL_INTEGER)").returnType(IntegerType.INTEGER);

    // Function repetition
    optimize("SIGN(SIGN(FIELD_INTEGER))", "SIGN(FIELD_INTEGER)");
  }

  @Test
  public void Cbrt() throws Exception {
    evalEquals("Cbrt(0)", 0L);
    evalEquals("Cbrt(1)", 1L);
    evalEquals("Cbrt(64)", 4L);
    evalEquals("Cbrt(343)", 7L);
    evalNull("Cbrt(NULL_INTEGER)");
    evalFails("Cbrt()");
  }

  @Test
  public void Sqrt() throws Exception {
    evalEquals("Sqrt(9)", 3L);
    evalFails("Sqrt(-5)");
    evalFails("Sqrt()");
    evalNull("Sqrt(NULL_INTEGER)");
  }

  @Test
  public void Square() throws Exception {
    evalEquals("Square(1)", 1L);
    evalEquals("Square(-5)", 25L);
    evalEquals("Square(FIELD_STRING_INTEGER)", 25L * 25L);
    evalFails("Square()");
    evalNull("Square(NULL_INTEGER)");
  }

  @Test
  public void Trim() throws Exception {
    evalEquals("Trim('a')", "a").returnType(StringType.STRING);
    evalEquals("Trim(' a ')", "a");
    evalEquals("Trim('  a b  ')", "a b");
    evalEquals("Trim('01ABC10 ', '012')", "ABC10 ");
    evalEquals("Trim(' 01ABC10 ', ' 012')", "ABC");
    evalNull("Trim(NULL_STRING)").returnType(StringType.STRING);
    evalNull("Trim(' 01ABC012 ',NULL_STRING)");
    evalFails("Trim()");

    optimize("Trim(' test ')", "'test'");
    optimizeTrue("Trim(' test ')='test'");

    // Function repetition
    optimize("TRIM(TRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
    optimize("TRIM(LTRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
    optimize("TRIM(RTRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
  }

  @Test
  public void LTrim() throws Exception {
    evalEquals("LTrim('a')", "a").returnType(StringType.STRING);
    evalEquals("LTrim(' a ')", "a ");
    evalEquals("LTrim('01ABC012', '012')", "ABC012");
    evalNull("LTrim(NULL_STRING)").returnType(StringType.STRING);
    evalNull("LTrim('01ABC012',NULL_STRING)");
    evalFails("LTrim()");

    // Function repetition
    optimize("LTRIM(LTRIM(FIELD_STRING))", "LTRIM(FIELD_STRING)");
    optimize("LTRIM(TRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
  }

  @Test
  public void RTrim() throws Exception {
    evalEquals("RTrim('a')", "a").returnType(StringType.STRING);
    evalEquals("RTrim(' a ')", " a");
    evalEquals("RTrim('012ABC10', '012')", "012ABC");
    evalNull("RTrim(NULL_STRING)").returnType(StringType.STRING);
    evalNull("RTrim('01ABC012',NULL_STRING)");
    evalFails("RTrim()");

    // Function repetition
    optimize("RTRIM(RTRIM(FIELD_STRING))", "RTRIM(FIELD_STRING)");
    optimize("RTRIM(TRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
  }

  @Test
  public void Greatest() throws Exception {
    // Boolean
    evalTrue("Greatest(false, FIELD_BOOLEAN_TRUE)").returnType(BooleanType.BOOLEAN);
    evalTrue("Greatest(false,true,false)").returnType(BooleanType.BOOLEAN);
    evalFalse("Greatest(false,false,false)");

    // Numeric
    evalEquals("Greatest(5,2,9,4)", 9L).returnType(IntegerType.INTEGER);
    evalEquals("Greatest(-5,2.1,9,4)", 9L).returnType(NumberType.NUMBER);
    evalEquals("Greatest(123,FIELD_INTEGER,789)", 789L).returnType(IntegerType.INTEGER); 
    evalEquals("Greatest(FIELD_INTEGER,FIELD_BIGNUMBER,FIELD_NUMBER)", 123456L).returnType(NumberType.NUMBER);
    
    // String
    evalEquals("Greatest('B','A','C')", "C").returnType(StringType.STRING);
    evalEquals("Greatest(FIELD_STRING,'Ab','Bf')", "TEST").returnType(StringType.STRING);
    
    // Binary
    evalEquals("Greatest(BINARY '12', BINARY '1F',BINARY '0A')", new byte[] {0x1F}).returnType(BinaryType.BINARY);
    
    // Date
    evalEquals("Greatest(DATE '2020-01-01',DATE '2021-12-06',DATE '1990-12-08')", LocalDate.of(2021, 12, 6)).returnType(DateType.DATE);
    
    // 1 argument only
    evalEquals("Greatest(5)", 5L);

    // Ignore nulls
    evalEquals("Greatest(NULL_STRING, 'B','A','C')", "C");

    // Null only if all values are null
    evalNull("Greatest(NULL_INTEGER, NULL_NUMBER)");

    // Data type mixed
    evalFails("Greatest(123,'str',123)");
  }

  @Test
  public void Least() throws Exception {
    // Boolean
    evalTrue("Least(FIELD_BOOLEAN_TRUE, true)").returnType(BooleanType.BOOLEAN);
    evalFalse("Least(true,false,true,false)");
    evalTrue("Least(true,true,true)");
    
    // Numeric
    evalEquals("Least(5,2,9,4)", 2L).returnType(IntegerType.INTEGER);    
    //evalEquals("Least(-5,2.1,9,4)", -5L);
    evalEquals("Least(123,FIELD_INTEGER,789)", 40L).returnType(IntegerType.INTEGER); 
    evalEquals("Least(FIELD_INTEGER,FIELD_NUMBER,789)", -5L).returnType(NumberType.NUMBER); 
    evalEquals("Least(FIELD_INTEGER,FIELD_BIGNUMBER,FIELD_NUMBER)", -5L).returnType(NumberType.NUMBER);
    
    // String
    evalEquals("Least('B','A','C')", "A").returnType(StringType.STRING);
    evalEquals("Least(FIELD_STRING,'st','bf')", "TEST").returnType(StringType.STRING);
    
    // Binary
    evalEquals("Least(BINARY '12',BINARY '1F',BINARY '0A')", new byte[] {0x0A}).returnType(BinaryType.BINARY);

    // Date
    evalEquals("Least(DATE '2020-01-01',DATE '2021-12-06',DATE '1990-12-08')", LocalDate.of(1990, 12, 8)).returnType(DateType.DATE);

    // 1 argument only
    evalEquals("Least(5)", 5L);

    // Ignore nulls
    evalEquals("Least(NULL_STRING, 'B','A','C')", "A");

    // Null only if all values are null
    evalNull("Least(NULL_INTEGER, NULL_NUMBER)");

    // Type mixed
    evalFails("Least(123,'str',123)");
  }

  @Test
  public void Length() throws Exception {
    // String
    evalEquals("Length('TEST')", 4L).returnType(IntegerType.INTEGER);
    evalNull("Length(NULL_STRING)").returnType(IntegerType.INTEGER);

    // Binary
    evalEquals("Length(BINARY 'F0FA')", 2L).returnType(IntegerType.INTEGER);
    evalEquals("Length(BINARY '0F0FA')", 3L).returnType(IntegerType.INTEGER);
    evalNull("Length(NULL_BINARY)");

    // Implicit conversion from Boolean to String
    evalEquals("Length(true)", 4L);
    evalEquals("Length(FALSE)", 5L);

    // Implicit conversion from Integer or Number to String
    evalEquals("Length(5)", 1L);
    evalEquals("Length(-5.3)", 4L);

    // TODO: Implicit conversion from Date to String ?
    // evalEquals("Length(Date '2023-01-01')", 8L);

    // Bad data type
    evalFails("Length()");
    evalFails("Length(Date '2023-01-01')");
  }

  @Test
  public void Left() throws Exception {
    // String
    evalEquals("Left('TEST FROM',4)", "TEST").returnType(StringType.STRING);
    evalEquals("Left('',1)", "");
    evalEquals("Left('TEST',10)", "TEST");
    evalEquals("Left('TEST',-1)", "");
    evalNull("Left(NULL_STRING,4)");
    evalNull("Left(FIELD_STRING,NULL_INTEGER)");

    // Binary
    evalEquals("Left(BINARY '12345678', 4)", new byte[] {0x12, 0x34, 0x56, 0x78}).returnType(BinaryType.BINARY);
    evalEquals("Left(BINARY '12345678', 2)", new byte[] {0x12, 0x34});
    evalEquals("Left(BINARY '12345678', -2)", new byte[] {});
    evalNull("Left(NULL_BINARY,4)");
    evalNull("Left(FIELD_BINARY,NULL_INTEGER)");

    evalFails("Left()");
  }

  @Test
  public void Insert() throws Exception {
    // String
    evalEquals("Insert('abcd', 1, 0, 'QW')", "QWabcd").returnType(StringType.STRING);
    evalEquals("Insert('abcd', 2, 1, 'QW')", "aQWcd");
    evalEquals("Insert('abcd', 2, 2, 'QW')", "aQWd");
    evalEquals("Insert('abcd', 5, 0, 'QW')", "abcdQW");

    // evalEquals("Insert('abcdefg', 1, 9, 'zy')", "zy");

    evalNull("Insert(NULL_STRING, 2, 1, 'qw')");
    evalNull("Insert('abcd', NULL_INTEGER, 1, 'qw')");
    evalNull("Insert('abcd', 2, NULL_INTEGER, 'qw')");
    evalNull("Insert('abcd', 2, 1, NULL_STRING)");

    // Binary
    evalEquals("Insert(BINARY '1234', 1, 0, BINARY '56')", new byte[] {0x56, 0x12, 0x34}).returnType(BinaryType.BINARY);
    evalEquals("Insert(BINARY '1234', 2, 0, BINARY '56')", new byte[] {0x12, 0x56, 0x34});
    evalEquals("Insert(BINARY '1234', 3, 0, BINARY '56')", new byte[] {0x12, 0x34, 0x56});
    evalEquals("Insert(BINARY '1234', 1, 1, BINARY '56')", new byte[] {0x56, 0x34});
    evalNull("Insert(NULL_BINARY, 1, 0, BINARY '56')");

    evalFails("Insert()");
    evalFails("Insert(BINARY '1234', 0, 0, BINARY '56')");
    evalFails("Insert(BINARY '1234', 4, 0, BINARY '56')");
  }

  @Test
  public void Right() throws Exception {
    // String
    evalEquals("Right('TEST FROM',4)", "FROM").returnType(StringType.STRING);
    evalEquals("Right('',1)", "");
    evalEquals("Right('TEST',10)", "TEST");
    evalEquals("Right('TEST',-1)", "");
    evalNull("Right(NULL_STRING,4)");
    evalNull("Right('TEST',NULL_INTEGER)");

    // Binary
    evalEquals("Right(BINARY '12345678', 2)", new byte[] {0x56, 0x78}).returnType(BinaryType.BINARY);
    evalEquals("Right(BINARY '12345678', 4)", new byte[] {0x12, 0x34, 0x56, 0x78});
    evalEquals("Right(BINARY '12345678', -2)", new byte[] {});

    evalNull("Right(NULL_BINARY,4)");
    evalNull("Right(FIELD_BINARY,NULL_INTEGER)");
    evalNull("Right(FIELD_BINARY,NULL_INTEGER)");

    evalFails("Right()");
  }

  @Test
  public void Repeat() throws Exception {
    // String
    evalEquals("Repeat('ABCD',3)", "ABCDABCDABCD").returnType(StringType.STRING);
    evalEquals("Repeat('ABCDEFCD',0)", "");
    evalNull("Repeat(NULL_STRING,2)").returnType(StringType.STRING);
    evalNull("Repeat('ABCD',NULL_INTEGER)");

    // Binary
    evalEquals("Repeat(BINARY '1234',3)", new byte[] {0x12, 0x34, 0x12, 0x34, 0x12, 0x34}).returnType(BinaryType.BINARY);
    evalNull("Repeat(NULL_BINARY,2)");
    evalNull("Repeat(FIELD_BINARY,NULL_INTEGER)");

    evalFails("Repeat()");
  }

  @Test
  public void Replace() throws Exception {
    evalEquals("Replace('ABCD','CD')", "AB").returnType(StringType.STRING);
    evalEquals("Replace('ABCDEFCD','CD','EF')", "ABEFEFEF");
    evalNull("Replace(NULL_STRING,'CD','EF')");
    evalNull("Replace('ABCD',NULL_STRING,'EF')");

    evalFails("Replace()");
  }

  @Test
  public void Is_Number() throws Exception {
    // String
    evalTrue("IS_NUMBER(' 123   ')").returnType(BooleanType.BOOLEAN);
    evalTrue("IS_NUMBER('-123.45')");
    evalTrue("IS_NUMBER('-3.45e+32')");
    evalTrue("IS_NUMBER('+3.45E-32')");
    evalTrue("IS_NUMBER('.6804')");
    evalFalse("IS_NUMBER('   ')");
    evalFalse("IS_NUMBER('3.45E-')");
    evalFalse("IS_NUMBER('12word')");
    evalFalse("IS_NUMBER(NULL_STRING)");

    // Number or Integer
    evalTrue("IS_NUMBER(-123)").returnType(BooleanType.BOOLEAN);
    evalTrue("IS_NUMBER(123.45)");
    evalTrue("IS_NUMBER(PI())");
    evalTrue("IS_NUMBER(FIELD_INTEGER)");
    evalTrue("IS_NUMBER(FIELD_NUMBER)");
    evalFalse("IS_NUMBER(NULL_INTEGER)");
    evalFalse("IS_NUMBER(NULL_NUMBER)");
    optimize("IS_NUMBER(FIELD_INTEGER)", "FIELD_INTEGER IS NOT NULL");

    // Other data type
    evalFalse("IS_NUMBER(FIELD_BOOLEAN_TRUE)").returnType(BooleanType.BOOLEAN);
    evalFalse("IS_NUMBER(FIELD_DATE)");
    evalFalse("IS_NUMBER(FIELD_BINARY)");
    evalFalse("IS_NUMBER(FIELD_JSON)");

    evalFails("IS_NUMBER()");
  }

  @Test
  public void Is_Json() throws Exception {
    evalTrue("IS_JSON('{\"name\":\"Smith\", \"age\":29}')").returnType(BooleanType.BOOLEAN);
    evalTrue("IS_JSON('{name:\"Smith\", age:29}')");
    evalTrue("IS_JSON('{id:1,coordinates:[10,20]}')");
    evalTrue("IS_JSON('[1,2]')");
    evalFalse("IS_JSON('name:\"Smith\", age:29}')");
    evalFalse("IS_JSON(NULL_STRING)");

    // evalFalse("IS_JSON(TRUE)");

    optimize("IS_JSON(FIELD_JSON)", "FIELD_JSON IS NOT NULL");
    optimizeFalse("IS_JSON(FIELD_DATE)");

    evalFails("IS_JSON()");
  }

  @Test
  public void Is_Date() throws Exception {

    // String
    evalTrue("IS_DATE('2023','YYYY')").returnType(BooleanType.BOOLEAN);
    evalTrue("IS_DATE('2023-04-25','YYYY-MM-DD')");
    evalTrue("IS_DATE('01/05/2023','DD/MM/YYYY')");
    evalFalse("IS_DATE('2023-02-31','YYYY-MM-DD')");
    evalFalse("IS_DATE('2023-02-31','YYYY.MM.DD')");
    evalFalse("IS_DATE(NULL_STRING,'YYYY.MM.DD')");


    // Date or Timestamp
    evalTrue("IS_DATE(FIELD_DATE,'YYYY-MM-DD')").returnType(BooleanType.BOOLEAN);
    evalTrue("IS_DATE(FIELD_TIMESTAMP,'YYYY-MM-DD HH24:MI:SS')");
    evalFalse("IS_DATE(NULL_DATE,'YYYY-MM-DD')");
    evalFalse("IS_DATE(NULL_TIMESTAMP,'YYYY-MM-DD HH24:MI:SS')");
    optimize("IS_DATE(FIELD_DATE,'YYYY-MM-DD')", "FIELD_DATE IS NOT NULL");
    optimize("IS_DATE(NULL_TIMESTAMP,'YYYY-MM-DD HH24:MI:SS')", "NULL_TIMESTAMP IS NOT NULL");

    // Other data type
    evalFalse("IS_DATE(FIELD_BOOLEAN_TRUE,'YYYY-MM-DD')");
    evalFalse("IS_DATE(FIELD_NUMBER,'YYYY-MM-DD')");
    evalFalse("IS_DATE(FIELD_BINARY,'YYYY-MM-DD')");
    evalFalse("IS_DATE(FIELD_JSON,'YYYY-MM-DD')");
  }

  @Test
  public void To_Boolean() throws Exception {
    evalTrue("To_Boolean('True')").returnType(BooleanType.BOOLEAN);
    evalTrue("To_Boolean('t')");
    evalTrue("To_Boolean('yes')");
    evalTrue("To_Boolean('on')");
    evalTrue("To_Boolean('1')");
    evalTrue("To_Boolean(5)");
    evalTrue("To_Boolean(-1)");
    evalTrue("To_Boolean(2.3)");
    evalTrue("To_Boolean(-2.3)");
    evalFalse("To_Boolean('False')");
    evalFalse("To_Boolean('off')");
    evalFalse("To_Boolean('NO')");
    evalFalse("To_Boolean('F')");
    evalFalse("To_Boolean('n')");
    evalFalse("To_Boolean('0')");
    evalFalse("To_Boolean(0)");
    evalFalse("To_Boolean(0.0000)");

    evalNull("To_Boolean(NULL_STRING)").returnType(BooleanType.BOOLEAN);

    evalFails("To_Boolean()");
    evalFails("To_Boolean('falsee')");
    evalFails("To_Boolean(1,2,3)");
  }

  @Test
  public void HexEncode() throws Exception {
    evalEquals("HEX_ENCODE('Apache Hop')", "41706163686520486f70");

    evalNull("HEX_ENCODE(NULL_STRING)");

    evalFails("HEX_ENCODE()");
    evalFails("HEX_ENCODE(0x01,0x02)");
  }

  @Test
  public void HexDecode() throws Exception {
    evalEquals("HEX_DECODE('41706163686520486f70')", "Apache Hop");

    evalNull("HEX_DECODE(NULL_STRING)");

    evalFails("HEX_DECODE()");
    evalFails("HEX_DECODE('p1','p2')");
  }

  @Test
  public void To_Binary() throws Exception {
    evalEquals("TO_BINARY(HEX_ENCODE('Apache Hop'),'HEX')", "Apache Hop".getBytes());
    evalEquals("TO_BINARY('41706163686520486f70','HEX')", "Apache Hop".getBytes());

    evalEquals("TO_BINARY(BASE64_ENCODE('Apache Hop'),'BASE64')", "Apache Hop".getBytes());
    evalEquals("TO_BINARY('QXBhY2hlIEhvcA==','BASE64')", "Apache Hop".getBytes());

    evalEquals("TO_BINARY('Apache Hop','UtF-8')", "Apache Hop".getBytes(StandardCharsets.UTF_8));
    evalEquals("TO_BINARY('Apache Hop','UtF8')", "Apache Hop".getBytes(StandardCharsets.UTF_8));

    evalNull("TO_BINARY(NULL_STRING)");

    evalFails("TO_BINARY('Apache Hop',NULL_STRING)");
    evalFails("TO_BINARY()");
  }

  @Test
  public void To_Number() throws Exception {

    // No precision/scale and no format
    evalEquals("TO_NUMBER('12.3456')", 12.3456);
    evalEquals("TO_NUMBER('98.76546')", 98.76546);
    evalEquals("TO_NUMBER('1.2E3')", 1200L);
    evalEquals("TO_NUMBER('1.2E-3')", 0.0012D);

    // Format with Decimals
    evalEquals("TO_NUMBER('5467.12', '999999.99')", 5467.12);
    evalEquals("TO_NUMBER('1234.5','09999.99')", 1234.5);
    Locale.setDefault(new Locale("en", "EN"));
    evalEquals("TO_NUMBER('5467.12', '999999D99')", 5467.12);
    Locale.setDefault(new Locale("fr", "BE"));
    evalEquals("TO_NUMBER('5467,12', '999999D99')", 5467.12);

    // Format No Decimals
    evalEquals("TO_NUMBER('4687841', '9999999')", 4687841L);

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
    evalEquals("TO_NUMBER('-   4','MI9999')", -4L);
    evalEquals("TO_NUMBER('-4','MI9999')", -4L);

    // Sign PR (format element can appear only in the last position of a number format model.)
    evalEquals("TO_NUMBER(' 0.5 ','99.99PR')", 0.5);
    evalEquals("TO_NUMBER('<0.5>','99.99PR')", -0.5);
    evalFails("TO_NUMBER('-5','PR9999')");

    // Format with Thousand Group Markers
    Locale.setDefault(new Locale("en", "US"));
    evalEquals("TO_NUMBER('12,345,678', '999,999,999')", 12_345_678D);
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
    evalEquals("TO_NUMBER('ABCD','FMXXXX')", 43981L);

    // Format Roman numeral
    evalEquals("TO_NUMBER('DXV','RN')", 515L);
    evalEquals("TO_NUMBER('CDLXXXV','RN')", 485L);

    evalEquals("TO_NUMBER('MCMXCIX','rn')", 1999L);
    evalEquals("TO_NUMBER('MMMDCCXXIV','rn')", 3724L);

    // Parse multi format
    evalEquals("TO_NUMBER('1234-','MI9999|9999MI')", -1234L);

    evalNull("TO_NUMBER(NULL_STRING)");

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
    // Null
    evalNull("TO_CHAR(NULL_NUMBER)").returnType(StringType.STRING);
    evalNull("TO_CHAR(NULL_DATE)").returnType(StringType.STRING);
    evalNull("TO_CHAR(NULL_BINARY)").returnType(StringType.STRING);

    // Default format
    evalEquals("TO_CHAR(0.45)", "0.45").returnType(StringType.STRING);
    evalEquals("TO_CHAR(12923)", "12923").returnType(StringType.STRING);

    // Format fixed length with decimal
    Locale.setDefault(new Locale("en", "EN"));
    evalEquals("TO_CHAR(0.1,'90.99')", "  0.1 ").returnType(StringType.STRING);
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

    // evalEquals("TO_CHAR(485.8, '\"Pre:\"999\" Post:\" .999')", "Pre: 485 Post: .800");

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
    evalEquals("To_Char(DATE '2019-07-23','AD')", "AD").returnType(StringType.STRING);
    evalEquals("To_Char(DATE '2019-07-23','BC')", "AD");
    evalEquals("To_Char(DATE '2019-07-23','Bc')", "Ad");
    evalEquals("To_Char(DATE '2019-07-23','bc')", "ad");
    evalEquals("To_Char(DATE '2019-07-23','A.D.')", "A.D.");
    evalEquals("To_Char(DATE '2019-07-23','B.C.')", "A.D.");
    evalEquals("To_Char(DATE '2019-07-23','B.c.')", "A.d.");
    evalEquals("To_Char(DATE '2019-07-23','b.c.')", "a.d.");
    evalEquals("To_Char(MAKE_DATE(-10,07,23),'b.c.')", "b.c.");

    // Punctuation is reproduced in the result
    evalEquals("To_Char(DATE '2019-07-23','dd/mm/yyyy')", "23/07/2019");
    evalEquals("To_Char(DATE '2019-07-23','dd.mm.yyyy')", "23.07.2019");
    evalEquals("To_Char(DATE '2019-07-23',':dd,mm;yyyy')", ":23,07;2019");

    // Quoted text is reproduced in the result
    evalEquals("To_Char(DATE '2019-07-23','dd\"text\"mmyyyy')", "23text072019");

    // Century
    evalEquals("To_Char(DATE '2019-07-23','CC')", "21");
    evalEquals("To_Char(DATE '2000-07-23','CC')", "20");
    evalEquals("To_Char(DATE '2019-07-23','SCC')", " 21");
    evalEquals("To_Char(DATE '2000-07-23','SCC')", " 20");
    evalEquals("To_Char(DATE '2000-07-23','FMSCC')", "20");
    evalEquals("To_Char(To_Date('-0200','SYYYY'),'SCC')", "-02");
    evalEquals("To_Char(To_Date('-0200','SYYYY'),'FMSCC')", "-2");

    evalEquals("To_Char(DATE '2122-01-01','YEAR')", "TWENTY-ONE TWENTY-TWO");
    evalEquals("To_Char(DATE '2021-01-01','YEAR')", "TWENTY TWENTY-ONE");
    evalEquals("To_Char(DATE '2020-01-01','YEAR')", "TWENTY TWENTY");
    evalEquals("To_Char(DATE '1999-01-01','YEAR')", "NINETEEN NINETY-NINE");
    evalEquals("To_Char(DATE '1900-01-01','YEAR')", "NINETEEN HUNDRED");
    evalEquals("To_Char(DATE '1830-01-01','YEAR')", "EIGHTEEN THIRTY");
    evalEquals("To_Char(DATE '2020-01-01','year')", "twenty twenty");
    evalEquals("To_Char(DATE '2020-01-01','syear')", " twenty twenty");
    evalEquals("To_Char(DATE '2020-01-01','FMsyear')", "twenty twenty");
    evalEquals("To_Char(To_Date('-1999','SYYYY'),'SYEAR')", "-NINETEEN NINETY-NINE");
    evalEquals("To_Char(To_Date('-0228','SYYYY'),'SYEAR')", "-TWO TWENTY-EIGHT");

    // Year
    evalEquals("To_Char(DATE '2019-01-01','YYYY')", "2019");
    evalEquals("To_Char(DATE '0800-01-01','YYYY')", "0800");
    evalEquals("To_Char(DATE '0800-01-01','FMYYYY')", "800"); // Year compact
    evalEquals("To_Char(DATE '2019-01-01','SYYYY')", " 2019");
    evalEquals("To_Char(To_Date('-2000','SYYYY'),'YYYY BC')", "2000 BC");
    evalEquals("To_Char(To_Date('-800','SYYYY'),'SYYYY')", "-0800"); // Negative signed year
    evalEquals("To_Char(To_Date('-800','SYYYY'),'YYYY BC')", "0800 BC");
    evalEquals("To_Char(DATE '0800-07-23','FMSYYYY')", "800"); // Signed year compact
    evalEquals("To_Char(To_Date('-800','SYYYY'),'FMSYYYY BC')", "-800 BC");
    evalEquals("To_Char(DATE '2019-01-01','YYY')", "019");
    evalEquals("To_Char(DATE '2019-01-01','YY')", "19");
    evalEquals("To_Char(DATE '2019-01-01','Y')", "9");

    // ISO Year
    evalEquals("To_Char(DATE '2019-12-28','IYYY')", "2019");
    evalEquals("To_Char(DATE '2019-12-28','IYY')", "019");
    evalEquals("To_Char(DATE '2019-12-28','IY')", "19");
    evalEquals("To_Char(DATE '2019-12-28','I')", "9");
    evalEquals("To_Char(DATE '2019-12-31','IYYY')", "2020");
    evalEquals("To_Char(DATE '2019-12-31','IYY')", "020");
    evalEquals("To_Char(DATE '2019-12-31','IY')", "20");
    evalEquals("To_Char(DATE '2019-12-31','I')", "0");

    // Quarter
    evalEquals("To_Char(DATE '2019-07-23','Q')", "3");

    // Month number
    evalEquals("To_Char(DATE '2019-07-23','MM')", "07");
    evalEquals("To_Char(DATE '2019-07-23','FMMM')", "7");

    // Full month name
    evalEquals("To_Char(DATE '2019-07-23','Month')", "July     ");
    evalEquals("To_Char(DATE '2019-07-23','MONTH')", "JULY     ");
    evalEquals("To_Char(DATE '2019-07-23','FMMONTH')", "JULY");
    evalEquals("To_Char(DATE '2019-09-23','month')", "september");

    // Short month name
    evalEquals("To_Char(DATE '2019-09-23','MON')", "SEP");
    evalEquals("To_Char(DATE '2019-09-23','Mon')", "Sep");
    evalEquals("To_Char(DATE '2019-09-23','mon')", "sep");

    // Roman numeral month
    evalEquals("To_Char(DATE '2019-09-23','RM')", "IX");
    evalEquals("To_Char(DATE '2019-06-23','rm')", "vi");

    // Aligned week of month
    evalEquals("To_Char(DATE '2015-12-31','\"W=\"W')", "W=5");
    evalEquals("To_Char(DATE '2015-02-05','\"W=\"W')", "W=1");

    // Aligned week of year and ISO Week of year (The first week of the ISO year is the week that
    // contains January 4.)
    evalEquals("To_Char(DATE '2015-12-31','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2015-12-31 thu IYYY=2015 IW=53 WW=53");
    evalEquals("To_Char(DATE '2016-01-01','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-01 fri IYYY=2015 IW=53 WW=01");
    evalEquals("To_Char(DATE '2016-01-02','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-02 sat IYYY=2015 IW=53 WW=01");
    evalEquals("To_Char(DATE '2016-01-03','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-03 sun IYYY=2015 IW=53 WW=01");
    evalEquals("To_Char(DATE '2016-01-04','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-04 mon IYYY=2016 IW=01 WW=01");
    evalEquals("To_Char(DATE '2016-01-05','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-05 tue IYYY=2016 IW=01 WW=01");
    evalEquals("To_Char(DATE '2016-01-06','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-06 wed IYYY=2016 IW=01 WW=01");
    evalEquals("To_Char(DATE '2016-01-07','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-07 thu IYYY=2016 IW=01 WW=01");
    evalEquals("To_Char(DATE '2016-01-08','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-08 fri IYYY=2016 IW=01 WW=02");
    evalEquals("To_Char(DATE '2042-12-31','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2042-12-31 wed IYYY=2043 IW=01 WW=53");
    evalEquals("To_Char(DATE '2016-01-04','FM\"IW=\"IW \"WW=\"WW')", "IW=1 WW=1"); // Compact

    // Day of week
    evalEquals("To_Char(DATE '2019-07-21','D')", "1");
    evalEquals("To_Char(DATE '2019-07-23','D')", "3");

    // Day of month
    evalEquals("To_Char(DATE '2019-07-08','DD')", "08");
    evalEquals("To_Char(DATE '2019-07-08','FMDD')", "8");
    evalEquals("To_Char(DATE '2019-07-23','DD')", "23");

    // Day of year
    evalEquals("To_Char(DATE '2019-02-23','DDD')", "054");
    evalEquals("To_Char(DATE '2019-02-23','FMDDD')", "54");

    // Julian day
    evalEquals("To_Char(DATE '2019-07-23','J')", "2458688");
    evalEquals("To_Char(DATE '0001-01-01','J')", "1721426");

    // Long day name
    evalEquals("To_Char(DATE '2019-07-23','DAY')", "TUESDAY  ");
    evalEquals("To_Char(DATE '2019-07-23','Day')", "Tuesday  ");
    evalEquals("To_Char(DATE '2019-07-23','day')", "tuesday  ");
    evalEquals("To_Char(DATE '2019-07-23','fmDay')", "Tuesday");

    // Short day name
    evalEquals("To_Char(DATE '2019-07-23','DY')", "TUE");
    evalEquals("To_Char(DATE '2019-07-23','Dy')", "Tue");
    evalEquals("To_Char(DATE '2019-07-23','dy')", "tue");

    // Time Zone Region
    evalEquals("To_Char(DATE '2019-07-23','TZR')", "UTC");
    evalEquals(
        "To_Char(TIMESTAMP '2021-01-01 15:28:59.123456789' AT TIME ZONE 'Europe/Paris','TZR')",
        "Europe/Paris");

    // Time zone region abbreviated with Daylight Saving Time
    evalEquals("To_Char(DATE '2019-07-23','TZD')", "UTC");
    evalEquals(
        "To_Char(TIMESTAMP '2021-01-01 15:28:59.123456789' AT TIME ZONE 'Europe/Paris','TZD')",
        "CET");
    evalEquals(
        "To_Char(TIMESTAMP '2021-01-01 15:28:59.123456789' AT TIME ZONE 'America/New_York','TZD')",
        "EST");

    // Time Zone Hour:Minute
    evalEquals("To_Char(DATE '2019-07-23','TZH:TZM')", "+00:00");
    evalEquals(
        "To_Char(To_Date('2019-02-13 15:34:56 -06:00','YYYY-MM-DD HH24:MI:SS TZH:TZM'),'TZH:TZM')",
        "-06:00");
    evalEquals(
        "To_Char(To_Date('2019-02-13 15:34:56 +8:00','YYYY-MM-DD HH24:MI:SS TZH:TZM'),'TZH:TZM')",
        "+08:00");

    // Time
    evalEquals("To_Char(TIMESTAMP '2019-02-13 15:34:56','HH:MI:SS')", "03:34:56");

    // Time 12 hours
    evalEquals("To_Char(TIMESTAMP '2019-02-13 03:34:56','HH12:MI:SS AM')", "03:34:56 AM");
    evalEquals("To_Char(TIMESTAMP '2019-02-13 15:34:56','HH12:MI:SS AM')", "03:34:56 PM");

    // Time 24 hours
    evalEquals("To_Char(TIMESTAMP '2019-02-13 15:34:56.123','HH24:MI:SS')", "15:34:56");

    // Time fraction
    evalEquals("To_Char(TIMESTAMP '2019-02-13 15:34:56.123','HH24:MI:SS.FF3')", "15:34:56.123");
    evalEquals("To_Char(TIMESTAMP '2019-02-13 15:34:56.123456','HH24:MI:SS.FF6')",
        "15:34:56.123456");
    evalEquals("To_Char(TIMESTAMP '2019-02-13 15:34:56.123456789','HH24:MI:SS.FF9')",
        "15:34:56.123456789");

    // Seconds of day
    evalEquals("To_Char(TIMESTAMP '2019-02-13 03:34:56','SSSSS')", "12896");

    // AM PM
    evalEquals("To_Char(TIMESTAMP '2019-02-13 03:34:56','HH12:MI:SS Am')", "03:34:56 Am");
    evalEquals("To_Char(TIMESTAMP '2019-02-13 15:34:56','HH12:MI:SS Pm')", "03:34:56 Pm");
    evalEquals("To_Char(TIMESTAMP '2019-02-13 03:34:56','HH12:MI:SS A.M.')", "03:34:56 A.M.");
    evalEquals("To_Char(TIMESTAMP '2019-02-13 15:34:56','HH12:MI:SS p.m.')", "03:34:56 p.m.");

    // Short date and time
    Locale.setDefault(new Locale("en", "US"));
    evalEquals("To_Char(TIMESTAMP '2019-07-23 14:52:00','DS TS')", "Jul 23, 2019 2:52:00 PM");
    Locale.setDefault(new Locale("fr", "BE"));
    evalEquals("To_Char(TIMESTAMP '2019-07-23 14:52:00','DS TS')", "Jul 23, 2019 2:52:00 PM");

    // Long date
    Locale.setDefault(new Locale("fr", "BE"));
    evalEquals("To_Char(TIMESTAMP '2019-07-23 14:52:00','DL')",
        "mardi 23 juillet 2019 à 14 h 52 min 00 s Temps universel coordonné");

    // Local radix character
    Locale.setDefault(new Locale("en", "GB"));
    // evalEquals("To_Char(TIMESTAMP '2019-07-23 14:52:00','HH:MI:SSXFF')", "02:52:00,000000");
    Locale.setDefault(new Locale("fr", "BE"));
    // evalEquals("To_Char(TIMESTAMP '2019-07-23 14:52:00','HH:MI:SSXFF')", "02:52:00,000000");

    // Special char
    evalEquals("To_Char(DATE '2019-07-23',':;.,=-/(FMMONTH)')", ":;.,=-/(JULY)");
    evalFails("To_Char(DATE '2019-07-23','*')");
    evalFails("To_Char(DATE '2019-07-23','Â£')");
    evalFails("To_Char(DATE '2019-07-23','{}[]')");

    // Full case
    evalEquals("To_Char(TIMESTAMP '2020-12-03 01:02:03.123456','yyyy-mm-dd hh:mi:ss.FF')",
        "2020-12-03 01:02:03.123456");

    // Binary
    evalEquals("TO_CHAR(BINARY '41706163686520486f70','HEX')", "41706163686520486f70");
    evalEquals("TO_CHAR(BINARY '41706163686520486f70','BASE64')", "QXBhY2hlIEhvcA==");
    evalEquals("TO_CHAR(BINARY '41706163686520486f70','UTF8')", "Apache Hop");
    evalEquals("TO_CHAR(BINARY '41706163686520486f70','UTF-8')", "Apache Hop");

    // String
    evalEquals("TO_CHAR('Apache Hop')", "Apache Hop");    
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
    evalEquals("To_Date(' 01/02/2020','DD/MM/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));
    evalEquals("To_Date('01/02/2020 ','DD/MM/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));
    evalEquals("To_Date('01/ 02/2020 ','DD/MM/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));
    evalEquals("To_Date('01/ 2/2020 ','DD/MM/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));

    evalEquals("To_Date('01/II/2020','DD/RM/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));
    evalEquals("To_Date('01/VII/2020','DD/RM/YYYY')", LocalDate.of(2020, Month.JULY, 1));

    evalEquals("To_Date('01/02/-100','DD/MM/SYYYY')", LocalDate.of(-100, 2, 1));

    evalEquals("To_Date('01/02/10','DD/MM/YY')", LocalDate.of(2010, 2, 1));
    evalEquals("To_Date('01/02/50','DD/MM/YY')", LocalDate.of(2050, 2, 1));
    evalEquals("To_Date('01/02/80','DD/MM/YY')", LocalDate.of(1980, 2, 1));

    ExpressionContext context = createExpressionContext();
    context.setVariable(ExpressionContext.EXPRESSION_TWO_DIGIT_YEAR_START, "1970");
    evalEquals(context, "To_Date('01/02/69','DD/MM/YY')", LocalDate.of(2069, 2, 1));
    context.setVariable(ExpressionContext.EXPRESSION_TWO_DIGIT_YEAR_START, "1970");
    evalEquals(context, "To_Date('01/02/70','DD/MM/YY')", LocalDate.of(1970, 2, 1));
    context.setVariable(ExpressionContext.EXPRESSION_TWO_DIGIT_YEAR_START, "2000");
    evalEquals(context, "To_Date('01/02/80','DD/MM/YY')", LocalDate.of(2080, 2, 1));

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
    evalEquals("To_Date(' 08- 2020','MM-SYYYY')", LocalDate.of(2020, 8, 1));

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

    // Fractional seconds FF3 (milliseconds), FF or FF6 (microseconds), FF9 (nanoseconds).
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

    // FX
    evalFails("To_Date('15/ Feb /2020','FXDD/MM/YYYY')");
    evalFails("To_Date('1-02-2020','FXDD/MM/YYYY')");
    evalFails("To_Date('1/02/2020','FXDD/MM/YYYY')");
    // evalEquals("To_Date('1/02/2020','FXFMDD-MON-YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));

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
    evalEquals("To_Date('2000_MAY_12 10.123','YYYY_MONTH_DD SS.FF3')",
        LocalDateTime.of(2000, 5, 12, 0, 0, 10, 123000000));

    evalEquals("To_Date('15:30:40','hh24:mi:ss')", LocalDateTime.of(1970, 1, 1, 15, 30, 40));

    evalNull("To_Date(NULL_STRING,'FXDD/MM/YYYY')");
  }

  @Test
  public void To_Interval() throws Exception {
    evalEquals("TO_INTERVAL('0-0 45 22:30:58')", Interval.of(0, 0, 45, 22, 30, 58)).returnType(IntervalType.INTERVAL);
    evalEquals("TO_INTERVAL('+0-0 45 22:30:58')", Interval.of(0, 0, 45, 22, 30, 58));
    evalEquals("TO_INTERVAL('45 days 22 hours 30 minutes 58 seconds')",
        Interval.of(0, 0, 45, 22, 30, 58));
    evalEquals("TO_INTERVAL('-0-0 45 22:30:58')", Interval.of(0, 0, 45, 22, 30, 58).negate());
    evalNull("TO_INTERVAL(NULL_STRING)");

    optimize("TO_INTERVAL('-0-0 45 22:30:58')", "INTERVAL '-45 22:30:58' DAY TO SECOND");
  }

  @Test
  public void To_Json() throws Exception {

    evalEquals("To_Json('{\"name\":\"Smith\", \"age\":29}')",
        JsonType.convertStringToJson("{\"name\":\"Smith\",\"age\":29}")).returnType(JsonType.JSON);
    evalEquals("To_Json('true')", JsonType.convertStringToJson("true"));
    evalEquals("To_Json('null')", JsonType.convertStringToJson("null"));

    evalNull("To_Json(NULL_STRING)").returnType(JsonType.JSON);

    evalFails("To_Json()");
    evalFails("To_Json(BOOLEAN_FIELD)");
    evalFails("To_Json('{\"name\":\"Smith\"; \"age\":29}')");
  }

  @Test
  public void Json_Value() throws Exception {
    // Json string type
    evalEquals("Json_Value('{\"name\":\"Smith\", \"age\":29}','$.name')", "Smith");
    evalEquals(
        "Json_Value('{\"name\":\"Smith\", \"age\":29,\"address\":{\"zip\":\"12345\",\"street\":\"Blvd des capusins\"}}','$.address.zip')",
        "12345");
    evalEquals(
        "Json_Value('{\"name\":\"Smith\", \"language\":[\"English\", \"French\"]}','$.language[1]')",
        "French");

    // Json numeric type
    evalEquals("Json_Value('{\"name\":\"Smith\", \"age\":29}','$.age')", 29L);

    evalEquals("Json_Value('{\"a\":[5, 10, 15, 20]}', '$.a[2]')", 15L);


    // TODO: Syntax of the special characters $[01] or $[6F,FF,00,1F] in variable resolution not
    // compatible with JsonPath array
    // evalEquals("Json_Value('{\"a\":[5, 10, 15, 20]}', '$[''a''][2]')", 15L);
    // evalEquals("Json_Value('{\"name\":\"Smith\", \"age\":29}','$[''name'']')", "Smith");
    // evalEquals("Json_Value('[0, 1, 2, 3]', '$[1]')", 1L);
    // evalEquals("Json_Value('[{\"a\":100}, {\"a\":200}, {\"a\":300}]', '$[1].a')", 200L);

    // Json boolean type
    evalFalse("Json_Value('{\"a\":[true, false, true, false]}', '$.a[1]')");
    evalTrue("Json_Value('{\"a\":[true, false, true, false]}', '$.a[2]')");

    // Json 'null' should return a NULL value
    evalNull("Json_Value('{\"name\":\"Smith\", \"age\":29, \"department\":null}','$.department')");

    // Json without field name quotes
    evalEquals("Json_Value('{name:\"Smith\", age:29}','$.name')", "Smith");


    evalNull("Json_Value(NULL_STRING,'$.name')");
    evalNull("Json_Value(NULL_JSON,'$.name')");

    evalFails("Json_Value('{\"name\":\"Smith\", \"age\":29}',NULL_STRING)");
    evalFails("Json_Value('{\"name\":\"Smith\", \"age\":29}','$.notexist')");
  }

  @Test
  public void Json_Query() throws Exception {
    evalEquals("Json_Query('{name:\"Smith\",age:29}')",
        JsonType.convertStringToJson("{name:\"Smith\",age:29}")).returnType(JsonType.JSON);
    evalEquals(
        "Json_Query('{Suspect:{Name:\"Smith\",Hobbies:[\"Eating\",\"Sleeping\",\"Base Jumping\"]}}','$.Suspect.Hobbies')",
        JsonType.convertStringToJson("[\"Eating\", \"Sleeping\", \"Base Jumping\"]"));
    evalEquals("Json_Query('null','$')", JsonType.convertStringToJson("null"));

    evalNull("Json_Query(NULL_JSON,'$')").returnType(JsonType.JSON);

    evalFails("Json_Query('{\"name\":\"Smith\", \"age\":29}',NULL_STRING)");
    evalFails("Json_Query('{\"name\":\"Smith\", \"age\":29}','$.notexist')");
  }

  @Test
  public void Json_Object() throws Exception {
    evalEquals("Json_Object(KEY 'name' VALUE 'Smith')",
        JsonType.convertStringToJson("{\"name\":\"Smith\"}")).returnType(JsonType.JSON);
    evalEquals("Json_Object(KEY 'name' VALUE 'Smith', KEY 'langue' VALUE 'english')",
        JsonType.convertStringToJson("{\"name\":\"Smith\",\"langue\":\"english\"}"));

    // Support null
    evalEquals("Json_Object(KEY 'name' VALUE 'Smith', KEY 'empty' VALUE null)",
        JsonType.convertStringToJson("{\"name\":\"Smith\",\"empty\":null}"));

    // Support duplicate key
    evalEquals("Json_Object(KEY 'name' VALUE 'Smith', KEY 'name' VALUE 'John')",
        JsonType.convertStringToJson("{\"name\":\"Smith\", \"name\":\"John\"}"));

    // Accept missing KEY
    evalEquals("Json_Object('name' VALUE 'Smith')",
        JsonType.convertStringToJson("{\"name\":\"Smith\"}"));

    evalFails("Json_Object(KEY 'name' VALUE )");
    evalFails("Json_Object(KEY VALUE 'Smith')");
    evalFails("Json_Object(KEY 'name' VALUE 'Smith'");

    optimize("JSON_OBJECT(KEY 'name' VALUE 'Smith',KEY 'langue' VALUE 'english')");
  }

  @Test
  public void Reverse() throws Exception {
    evalEquals("Reverse('Hello, world!')", "!dlrow ,olleH").returnType(StringType.of(13));
    evalEquals("Reverse(BINARY '2A3B4C')", new byte[] {0x4C, 0x3B, 0x2A}).returnType(BinaryType.of(3));

    evalNull("Reverse(NULL_STRING)").returnType(StringType.STRING);
    evalNull("Reverse(NULL_BINARY)");

    evalFails("Reverse()");
    evalFails("Reverse(FIELD_DATE)");
  }

  @Test
  public void Soundex() throws Exception {
    evalEquals("Soundex('Wikipedia')", "W213").returnType(StringType.STRING);
    evalEquals("Soundex('I LOVE ROCKS.')", "I416");
    evalEquals("Soundex('I LOVE ROCK AND ROLL MUSIC.')", "I416");
    evalNull("Soundex(NULL_STRING)").returnType(StringType.STRING);
    
    evalFails("Soundex()");
    evalFails("Soundex(FIELD_DATE)");
  }

  @Test
  public void Difference() throws Exception {
    evalEquals("Difference('Juice', 'Jucy')", 4L);
    evalNull("Difference(NULL_STRING,NULL_STRING)");
    evalNull("Difference(NULL_STRING,'Jucy')");
    evalNull("Difference('Juice',NULL_STRING)");
  }

  @Test
  public void Levenshtein() throws Exception {
    evalEquals("Levenshtein('Superman', 'Superman')", 0L);
    evalEquals("Levenshtein('kitten', 'sitting')", 3L);
    evalNull("Levenshtein(NULL_STRING,NULL_STRING)");
    evalNull("Levenshtein(NULL_STRING,'Superman')");
    evalNull("Levenshtein('Superman',NULL_STRING)");
  }

  @Test
  public void JaroWinkler() throws Exception {
    evalEquals("JaroWinkler('Superman', 'Superman')", 100L);
    evalEquals("JaroWinkler('Peter Parker', 'Pete Parker')", 88L);
    evalEquals("JaroWinkler('elephant', 'hippo')", 44L);
    evalEquals("JaroWinkler('święta', 'swieta')", 78L);
    evalEquals("JaroWinkler('Ich weiß nicht', 'Ich weiss nicht')", 93L);

    evalNull("JaroWinkler(NULL_STRING,NULL_STRING)");
    evalNull("JaroWinkler(NULL_STRING,'Superman')");
    evalNull("JaroWinkler('Superman',NULL_STRING)");
  }

  @Test
  public void Translate() throws Exception {
    evalEquals("Translate('Hello, world!','eo','EO')", "HEllO, wOrld!");
    evalEquals("Translate('Hello, wOrld!','eol', 'E')", "HE, wOrd!");
    evalEquals("Translate('Hello, world!','oel,! ', '')", "Hwrd");
    evalEquals("Translate('Hello, world!','eo',NULL_STRING)", "Hll, wrld!");
    evalNull("Translate(NULL_STRING,'eo','EO')");
    evalNull("Translate('Hello, world!',NULL_STRING,'EO')");

  }

  @Test
  public void Truncate() throws Exception {
    evalEquals("Truncate(-975.975)", -975L);
    evalEquals("Truncate(-975.975,-1)", -970L);
    evalEquals("Truncate(-975.975, 0)", -975L);
    evalEquals("Truncate(-975.975, 2)", -975.97D);
    evalEquals("Truncate(-975.975, 3)", -975.975D);
    evalEquals("Truncate(-975.975, 50)", -975.975D);
    evalEquals("Truncate(123.456, -2)", 100L);
    evalEquals("truncate(123456789012345678999.999,-2)", new BigDecimal("123456789012345678900"));
    evalNull("Truncate(-975.975, NULL_INTEGER)");
    evalNull("Truncate(NULL_NUMBER, 2)");

    // Alias
    evalEquals("Trunc(123.456, -2)", 100L);

    optimize("TRUNC(TRUNCATE(FIELD_NUMBER))", "TRUNCATE(FIELD_NUMBER)");
  }

  @Test
  public void Date_Trunc() throws Exception {
    evalEquals("Date_Trunc(MILLENNIUM, DATE '2020-05-08')", LocalDate.of(2000, Month.JANUARY, 1));
    evalEquals("Date_Trunc(CENTURY, DATE '2020-05-08')", LocalDate.of(2000, Month.JANUARY, 1));
    evalEquals("Date_Trunc(DECADE, DATE '2021-05-08')", LocalDate.of(2020, Month.JANUARY, 1));
    evalEquals("Date_Trunc(YEAR, DATE '2020-05-08')", LocalDate.of(2020, Month.JANUARY, 1));
    evalEquals("Date_Trunc(MONTH, DATE '2020-05-08')", LocalDate.of(2020, Month.MAY, 1));
    evalEquals("Date_Trunc(DAY, DATE '2020-05-25')", LocalDate.of(2020, Month.MAY, 25));
    evalEquals("Date_Trunc(DAYOFMONTH, DATE '2020-05-25')", LocalDate.of(2020, Month.MAY, 25));
    evalEquals("Date_Trunc(QUARTER, DATE '2020-05-25')", LocalDate.of(2020, Month.APRIL, 1));
    evalEquals("Date_Trunc(WEEK, DATE '2020-05-28')", LocalDate.of(2020, Month.MAY, 25));
    evalEquals("Date_Trunc(WEEK, DATE '2021-01-01')", LocalDate.of(2020, Month.DECEMBER, 28));
    evalEquals("Date_Trunc(WEEKOFYEAR, DATE '2020-05-28')", LocalDate.of(2020, Month.MAY, 25));

    evalNull("Date_Trunc(DAY, NULL_TIMESTAMP)");
    evalNull("Date_Trunc(DAY, NULL_DATE)");

    evalFails("Date_Trunc(DAY)");
    evalFails("Date_Trunc(ISOWEEK, DATE '2020-05-25')");
    evalFails("Date_Trunc(DATE '2020-05-25')");
    evalFails("Date_Trunc(NULL_DATE, DATE '2020-05-25')");
    evalFails("Date_Trunc(123, DATE '2020-05-25')");
    evalFails("Date_Trunc('xxx', DATE '2020-05-25')");

    // Truncate timestamp
    evalEquals("Date_Trunc(DAY, TIMESTAMP '2020-05-25 23:59:59')",
        LocalDate.of(2020, Month.MAY, 25));
    evalEquals("Date_Trunc(HOUR, TIMESTAMP '2020-05-25 23:59:59')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 0, 0, 0));
    evalEquals("Date_Trunc(MINUTE, TIMESTAMP '2020-05-25 23:59:59')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 59, 0, 0));
    evalEquals("Date_Trunc(SECOND, TIMESTAMP '2020-05-25 23:59:59')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 59, 59, 0));
    evalEquals("Date_Trunc(MILLISECOND, TIMESTAMP '2020-05-25 23:59:59.123456789')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 59, 59, 123000000));
    evalEquals("Date_Trunc(MICROSECOND, TIMESTAMP '2020-05-25 23:59:59.123456789')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 59, 59, 123456000));
    evalEquals("Date_Trunc(NANOSECOND, TIMESTAMP '2020-05-25 23:59:59.123456789')",
        LocalDateTime.of(2020, Month.MAY, 25, 23, 59, 59, 123456789));
  }

  @Test
  public void Contains() throws Exception {
    // String
    evalTrue("CONTAINS(FIELD_STRING,'ES')").returnType(BooleanType.BOOLEAN);
    evalFalse("CONTAINS(FIELD_STRING,'YZ')");
    evalNull("CONTAINS(NULL_STRING,'ES')").returnType(BooleanType.BOOLEAN);
    evalNull("CONTAINS(FIELD_STRING,NULL_STRING)");

    // Binary
    evalTrue("CONTAINS(BINARY '1A2B3C4D5E6F',BINARY '1A2B')").returnType(BooleanType.BOOLEAN);
    evalTrue("CONTAINS(BINARY '1A2B3C4D5E6F',BINARY '2B3C')");
    evalTrue("CONTAINS(BINARY '1A2B3C4D5E6F',BINARY '5E6F')");
    evalFalse("CONTAINS(BINARY '1A2B3C4D5E6F',BINARY '0A2B')");
    evalFalse("CONTAINS(BINARY '1A2B3C4D5E6F',BINARY '6F6F')");
    evalFalse("CONTAINS(BINARY '1A2B3C4D5E6F',BINARY '')");
    evalNull("CONTAINS(NULL_BINARY,BINARY '1A2B3C')").returnType(BooleanType.BOOLEAN);
    evalNull("CONTAINS(BINARY '1A2B3C',NULL_BINARY)");

    evalFails("CONTAINS()");
    evalFails("CONTAINS(FIELD_STRING)");
    evalFails("CONTAINS(FIELD_BINARY)");
  }

  @Test
  public void StartsWith() throws Exception {
    // String
    evalTrue("StartsWith('TEST FROM','TES')").returnType(BooleanType.BOOLEAN);
    evalFalse("StartsWith('-TEST FROM','TES')");

    // Binary
    evalTrue("StartsWith(BINARY 'FAA12345',BINARY 'fA')").returnType(BooleanType.BOOLEAN);
    evalFalse("StartsWith(BINARY 'FAA12345',BINARY 'EE')");
    evalFalse("StartsWith(BINARY '12345',BINARY '123456')");

    evalNull("StartsWith(NULL_STRING,'ROMA')");
    evalNull("StartsWith('TEST FROM',NULL_STRING)");
    evalFails("StartsWith()");
  }

  @Test
  public void EndsWith() throws Exception {
    // String
    evalTrue("EndsWith('TEST FROM','ROM')").returnType(BooleanType.BOOLEAN);
    evalFalse("EndsWith('TEST FROM','ROMA')");
    evalNull("EndsWith(NULL_STRING,'ROMA')").returnType(BooleanType.BOOLEAN);
    evalNull("EndsWith('TEST FROM',NULL_STRING)").returnType(BooleanType.BOOLEAN);
    
    // Binary
    evalTrue("EndsWith(BINARY 'FAA12345',BINARY '2345')").returnType(BooleanType.BOOLEAN);
    evalFalse("EndsWith(BINARY 'FAA12345',BINARY '88')");
    evalFalse("EndsWith(BINARY '12345',BINARY 'FFFF12345')");
    
    evalFails("EndsWith()");
  }

  @Test
  public void Regexp_Like() throws Exception {
    evalTrue("Regexp_Like('aaa','a{2,4}')").returnType(BooleanType.BOOLEAN);
    evalTrue("Regexp_Like('Erdbeere','Erd[a[:SPACE:]b]eere')");
    evalTrue("Regexp_Like('12345TEST','123[:alnum:]*')");
    evalTrue("Regexp_Like('ABcdf987','[:xdigit:]*')");
    evalTrue("Regexp_Like('ABcdf987','[:xdigit:]*')");
    evalTrue("Regexp_Like('A','[a-z]','i')");
    evalFalse("Regexp_Like('A','[a-z]','c')");
    evalNull("Regexp_Like(NULL_STRING,'A')").returnType(BooleanType.BOOLEAN);
    evalNull("Regexp_Like('A', NULL_STRING)").returnType(BooleanType.BOOLEAN);

    // An empty pattern '' matches nothing
    evalFalse("Regexp_Like('','')");
    evalFalse("Regexp_Like('ABC','')");

    evalFails("Regexp_Like()");
    evalFails("Regexp_Like('A')");
    evalFails("Regexp_Like('A','[a-z]','z')");
  }

  @Test
  public void Regexp_Replace() throws Exception {
    evalEquals("Regexp_Replace('A1.2.3.4','[^0-9]')", "1234").returnType(StringType.STRING);
    evalEquals("Regexp_Replace('A1.2.3.4','[^0-9]', '', 1, 0)", "1234");
    evalEquals("Regexp_Replace('ABC, ABC, ABC','ABC', 'EFG', 1, 2)", "ABC, EFG, ABC");
    evalEquals("Regexp_Replace('AAA BBB CCC', '[:space:]+', '-')", "AAA BBB CCC");
    evalEquals("Regexp_Replace('expressssion', 's+','ss')", "expression");
    evalEquals(
        "Regexp_Replace('This line    contains    more      than one   spacing      between      words', '( ){2,}', ' ')",
        "This line contains more than one spacing between words");
    evalEquals("Regexp_Replace('ABCEFG', 'A..','WXYZ')", "WXYZEFG");
    evalEquals("Regexp_Replace('ABCEFG', '[A-Z]','',1,1)", "BCEFG");
    evalEquals("Regexp_Replace('abc', '(b|c)', 'X')", "aXX");
    evalEquals("Regexp_Replace('abc', '(.*)c', '\\1e')", "abe");
    evalEquals("Regexp_Replace('abc', '(a)(b)', '\\2\\1')", "bac");


    // An empty pattern matches nothing
    evalEquals("Regexp_Replace('ABCDEEEEEEFG', '','E')", "ABCDEEEEEEFG");

    // Back reference
    evalEquals("Regexp_Replace('FIRSTNAME MIDDLENAME LASTNAME','(.*) (.*) (.*)','\\3, \\1 \\2')",
        "LASTNAME, FIRSTNAME MIDDLENAME");

    evalNull("Regexp_Replace(NULL_STRING,'A')").returnType(StringType.STRING);
    evalNull("Regexp_Replace('A', NULL_STRING)");

    evalFails("Regexp_Replace()");
  }

  @Test
  public void Regexp_Count() throws Exception {
    evalEquals("Regexp_Count('An apple costs 50 cents, a banana costs 10 cents.', '\\d+')", 2L).returnType(IntegerType.INTEGER);
    evalEquals("Regexp_Count('An apple costs 50 cents, a banana costs 10 cents.', '\\d+', 20)", 1L);
    evalEquals("Regexp_Count('An apple costs 50 cents, a banana costs 10 cents.', 'CENTS', 1, 'i')",
        2L);
  }

  @Test
  public void Regexp_Instr() throws Exception {
    evalEquals("Regexp_Instr('email@apache.org', '@[^.]*')", 6L).returnType(IntegerType.INTEGER);
    evalEquals("Regexp_Instr('hello to YOU', '(.o).', 1, 3, 1,'i')", 13L);
    evalEquals(
        "Regexp_Instr('REGEXP_INSTR is an advanced extension of the INSTR function','[:a-z]{3,8}', 3, 2, 1)",
        37L);

    // An empty pattern matches nothing
    evalEquals("Regexp_Instr('email@apache.org', '')", 0L);
  }

  @Test
  public void Regexp_Substr() throws Exception {
    evalEquals("regexp_substr('email@apache.org', '@[^.]*')", "@apache").returnType(StringType.STRING);
    evalEquals("regexp_substr('This is a regexp_substr demo', '[a-zA-Z0-9_]+', 1, 4)",
        "regexp_substr");

    evalEquals(
        "regexp_substr('It was the best of times, it was the worst of times.', 'the\\W+\\w+', 1, 1)",
        "the best");
    evalEquals(
        "regexp_substr('It was the best of times, it was the worst of times.', 'the\\W+\\w+', 1, 2)",
        "the worst");

    evalNull("regexp_substr('abc', 'z')");
    evalEquals("regexp_substr('abc', '.b.', 0)", "abc");
    evalNull("regexp_substr('abc', '.b.', 1)");
    // evalEquals("regexp_substr('abc', '([a-z])(b)', 1)", "a");
    // evalEquals("regexp_substr('abc', '([a-z])(b)', 2)", "b");

    // An empty pattern matches nothing
    evalNull("regexp_substr('email@apache.org', '')");

    evalNull("regexp_substr(NULL_STRING,  '@[^.]*')");
    evalNull("regexp_substr('email@apache.org', NULL_STRING)");
  }

  @Test
  public void EqualsNull() throws Exception {
    evalFalse("Equal_Null(1,NULL_INTEGER)").returnType(BooleanType.BOOLEAN);
    evalTrue("Equal_Null(NULL_STRING,NULL_STRING)").returnType(BooleanType.BOOLEAN);
    evalTrue("Equal_Null(NULL_INTEGER,NULL_NUMBER)");
    evalFails("Equal_Null(NOM)");
    evalTrue("Equal_Null(DATE '2019-01-01',DATE '2019-01-01')");
    evalFalse("Equal_Null(DATE '2019-01-01',DATE '2018-01-01')").returnType(BooleanType.BOOLEAN);

    optimizeTrue("EQUAL_NULL(NULL_STRING, NULL_STRING)");
    optimizeTrue("EQUAL_NULL(FIELD_STRING, FIELD_STRING)");
    optimizeTrue("EQUAL_NULL(FIELD_INTEGER, FIELD_INTEGER)");
    optimize("EQUAL_NULL(FIELD_INTEGER,NULL)", "FIELD_INTEGER IS NULL");
    optimize("EQUAL_NULL(NULL,FIELD_INTEGER)", "FIELD_INTEGER IS NULL");
  }

  @Test
  public void Concat() throws Exception {
    // String
    evalEquals("CONCAT('TES','T')", "TEST").returnType(StringType.of(4));
    evalEquals("FIELD_STRING||'t'","TESTt").returnType(StringType.of(1001));
    evalTrue("FIELD_STRING='TES'||'T'");
    evalTrue("FIELD_STRING='TES'||NULL_STRING||'T'");
    evalEquals("Concat(NULL_STRING,'a')", "a").returnType(StringType.STRING);
    evalEquals("Concat('a',NULL_STRING)", "a").returnType(StringType.STRING);
    evalEquals("4 || 2", "42").returnType(StringType.of(2));
    evalEquals("4 || '2'", "42").returnType(StringType.of(2));   
    evalEquals("concat(cast('a' as string(2)), cast('b' as string(3)),cast('c' as string(2)))","abc").returnType(StringType.of(3));
    evalNull("NULL_STRING||NULL_STRING").returnType(StringType.STRING);
           
    // Binary
    evalEquals("Concat(BINARY '1F',BINARY '2A3B')", new byte[] {0x1F, 0x2A, 0x3B}).returnType(BinaryType.of(3));
    evalEquals("BINARY '1F' || NULL_BINARY || BINARY '2A3B'", new byte[]{0x1F, 0x2A, 0x3B}).returnType(BinaryType.BINARY);
    evalEquals("NULL_BINARY || BINARY '1F' || BINARY '2A3B'", new byte[]{0x1F, 0x2A, 0x3B}).returnType(BinaryType.BINARY);
    evalEquals("BINARY '1F' || BINARY '2A3B' || NULL_BINARY", new byte[]{0x1F, 0x2A, 0x3B}).returnType(BinaryType.BINARY);
    evalNull("Concat(NULL_BINARY,NULL_BINARY)").returnType(BinaryType.BINARY);

    evalFails("Concat()");

    // Mix String and Binary
    // evalFails("Concat(FIELD_STRING, FIELD_BINARY)");

    // Check operands type
    evalFails("Concat(FIELD_STRING, FIELD_DATE)");
    evalFails("Concat(FIELD_BINARY, FIELD_DATE)");
    evalFails("Concat(FIELD_DATE, FIELD_STRING)");
    evalFails("Concat(FIELD_DATE, FIELD_BINARY)");
    
    // Literal
    optimize("CONCAT('TES','T')", "'TEST'");
    optimize("'A'||'B'", "'AB'");

    // Combine and flatten concats (Same syntax but cost reduced)
    optimize("'A'||FIELD_STRING||FIELD_STRING||'C'", "'A'||FIELD_STRING||FIELD_STRING||'C'");
    optimize("'A'||FIELD_STRING||NULL_STRING||'C'", "'A'||FIELD_STRING||NULL_STRING||'C'");
    optimize("CONCAT('A',CONCAT(FIELD_STRING,CONCAT(FIELD_STRING,'C')||'D'))",
        "'A'||FIELD_STRING||FIELD_STRING||'C'||'D'");
  }

  @Test
  public void ConcatWs() throws Exception {

    // String
    evalEquals("CONCAT_WS(',','ONE','TWO','THREE')", "ONE,TWO,THREE");
    evalEquals("CONCAT_WS('---','b','c')", "b---c").returnType(StringType.of(5));
    evalEquals("CONCAT_WS('--','one')", "one");
    evalEquals("CONCAT_WS(',','a',NULL_STRING,'b')", "a,b");

    // Binary
    evalEquals("CONCAT_WS(BINARY '1F',BINARY '2A3B',BINARY '4D',BINARY '5E')",
        new byte[] {0x2A, 0x3B, 0x1F, 0x4D, 0x1F, 0x5E}).returnType(BinaryType.of(6));

    // Number
    evalEquals("CONCAT_WS(':',4,2)", "4:2").returnType(StringType.of(3));
    
    evalNull("CONCAT_WS(NULL_STRING,'FIRST')").returnType(StringType.of(5));
    evalNull("CONCAT_WS('a',NULL_STRING)");
    evalNull("CONCAT_WS(BINARY '1F',NULL_STRING,NULL_STRING)").returnType(BinaryType.BINARY);

    evalFails("CONCAT_WS()");
    evalFails("CONCAT_WS(',')");

    // Mix String and Binary
    evalFails("CONCAT_WS(NOM,0x2A3B)");
  }

  @Test
  public void Chr() throws Exception {
    evalEquals("Chr(83)", "S").returnType(StringType.STRING);
    evalEquals("Chr(115)", "s");
    evalEquals("Chr(233)", "é");
    evalEquals("Chr(945)", "α");
    evalEquals("Chr(8364)", "€");
    evalEquals("Chr(33288)", "興");
    evalNull("Chr(NULL_INTEGER)");

    evalFails("Chr()");
    evalFails("Chr(-1)");
    evalFails("Chr(999999999999)");
  }

  @Test
  public void Ascii() throws Exception {
    evalEquals("Ascii('ABC')", 65L).returnType(IntegerType.INTEGER);
    evalEquals("Ascii('é')", 233L);
    evalEquals("Ascii('€')", 8364L);
    evalEquals("Ascii('興')", 33288L);
    evalEquals("Ascii('')", 0L);
    evalNull("Ascii(NULL_STRING)");

    evalFails("Ascii()");
  }

  @Test
  public void Unicode() throws Exception {
    evalEquals("Unicode('SSSS')", 83L).returnType(IntegerType.INTEGER);;
    evalEquals("Unicode('é')", 233L);
    evalEquals("Unicode('€')", 8364L);
    evalEquals("Unicode('')", 0L);
    evalNull("Unicode(NULL_STRING)");
    evalFails("Unicode()");
  }

  @Test
  public void String_Encode() throws Exception {
    evalEquals("String_Encode('\t\r\n\f\b\"')", "\\t\\r\\n\\f\\b\\\"").returnType(StringType.STRING);
    // Encode 16 bit unicode
    evalEquals("String_Encode('€')", "\\u20AC");
    evalNull("String_Encode(NULL_STRING)").returnType(StringType.STRING);
  }

  @Test
  public void String_Decode() throws Exception {
    evalEquals("String_Decode('\\t\\r\\n\\f\\b\\\"')", "\t\r\n\f\b\"").returnType(StringType.STRING);
    // Decode 16 bits unicode
    evalEquals("String_Decode('\\u20AC')", "€");
    // Decode octal
    evalEquals("String_Decode('\366\344\374')", "öäü");
    evalNull("String_Decode(NULL_STRING)").returnType(StringType.STRING);
  }

  @Test
  public void Html_Encode() throws Exception {
    evalEquals("Html_Encode('18€ & <test> ™')", "18&euro; &amp; &lt;test&gt; &trade;").returnType(StringType.STRING);
    evalNull("Html_Encode(NULL_STRING)").returnType(StringType.STRING);
    evalFails("Html_Encode()");
    evalFails("Html_Encode('x','y')");
  }

  @Test
  public void Html_Decode() throws Exception {
    evalEquals("Html_Decode('18&euro; &amp; &lt;test&gt; &#8482;')", "18€ & <test> ™");
    evalNull("Html_Decode(NULL_STRING)");
    evalFails("Html_Decode()");
    evalFails("Html_Decode('x','y')");
  }

  @Test
  public void Url_Encode() throws Exception {
    evalEquals("Url_Encode('a b')", "a+b").returnType(StringType.STRING);
    evalEquals("Url_Encode('a+b')", "a%2Bb");
    evalEquals("Url_Encode('âéè')", "%C3%A2%C3%A9%C3%A8");
    evalNull("Url_Encode(NULL_STRING)").returnType(StringType.STRING);
    evalFails("Url_Encode()");
    evalFails("Url_Encode('x','y')");
  }

  @Test
  public void Url_Decode() throws Exception {
    evalEquals("Url_Decode('a+b')", "a b").returnType(StringType.STRING);
    evalEquals("Url_Decode('a%2Bb')", "a+b");
    evalEquals("Url_Decode('%C3%A2%C3%A9%C3%A8')", "âéè");
    evalNull("Url_Decode(NULL_STRING)");
    evalFails("Url_Decode('a%%2Bb')");
    evalFails("Url_Decode()");
    evalFails("Url_Decode('x','y')");
  }

  @Test
  public void Base64_Encode() throws Exception {
    evalEquals("Base64_Encode('Apache Hop')", "QXBhY2hlIEhvcA==").returnType(StringType.STRING);
    evalEquals("Base64_Encode('Apache Hop'::Binary)", "QXBhY2hlIEhvcA==").returnType(StringType.STRING);
    evalNull("Base64_Encode(NULL_STRING)").returnType(StringType.STRING);
    evalFails("Base64_Encode()");    
  }

  @Test
  public void Base64_Decode() throws Exception {
    evalEquals("Base64_Decode('QXBhY2hlIEhvcA==')", "Apache Hop").returnType(StringType.STRING);
    evalEquals("Base64_Decode('QXBhY2hlIEhvcA=='::Binary)", "Apache Hop").returnType(StringType.STRING);
    evalNull("Base64_Decode(NULL_STRING)").returnType(StringType.STRING);
    evalFails("Base64_Decode()");
  }

  @Test
  public void Base32_Encode() throws Exception {
    evalEquals("Base32_Encode('Apache Hop')", "IFYGCY3IMUQEQ33Q").returnType(StringType.STRING);
    evalEquals("Base32_Encode('Apache Hop'::Binary)", "IFYGCY3IMUQEQ33Q").returnType(StringType.STRING);
    evalNull("Base32_Encode(NULL_STRING)").returnType(StringType.STRING);
    evalFails("Base32_Encode()");
  }

  @Test
  public void Base32_Decode() throws Exception {
    evalEquals("Base32_Decode('IFYGCY3IMUQEQ33Q')", "Apache Hop").returnType(StringType.STRING);
    evalEquals("Base32_Decode('IFYGCY3IMUQEQ33Q'::Binary)", "Apache Hop").returnType(StringType.STRING);
    evalNull("Base32_Decode(NULL_STRING)").returnType(StringType.STRING);
    evalFails("Base32_Decode()");
  }

  @Test
  public void Ceil() throws Exception {
    evalEquals("Ceil(1)", 1L).returnType(NumberType.NUMBER);
    evalEquals("Ceil(125.9)", 126L);
    evalEquals("Ceil(0.4873)", 1L);
    evalEquals("Ceil(-0.65)", 0L);
    evalEquals("Ceil(-42.8)", -42L);
    evalEquals("Ceil(FIELD_INTEGER)", 40L);
    evalEquals("Ceil(FIELD_NUMBER)", -5L);
    evalEquals("Ceil(FIELD_BIGNUMBER)", new BigDecimal("123457"));

    evalNull("Ceil(NULL_INTEGER)");
    evalNull("Ceil(NULL_NUMBER)");
    evalNull("Ceil(NULL_BIGNUMBER)");

    evalFails("Ceil()");
    evalFails("Ceil(1,2,3)");
    evalFails("Ceil('x')");

    // Alias
    evalEquals("Ceiling(1)", 1L);

    // Function repetition
    optimize("CEIL(CEILING(FIELD_NUMBER))", "CEILING(FIELD_NUMBER)");
  }

  @Test
  public void Floor() throws Exception {
    evalEquals("Floor(1)", 1L).returnType(NumberType.NUMBER);
    evalEquals("Floor(125.9)", 125L);
    evalEquals("Floor(0.4873)", 0L);
    evalEquals("Floor(-0.65)", -1L);
    evalEquals("Floor(FIELD_INTEGER)", 40L);
    evalEquals("Floor(FIELD_NUMBER)", -6L);
    evalEquals("Floor(FIELD_BIGNUMBER)", new BigDecimal("123456"));

    evalNull("Floor(NULL_INTEGER)");
    evalNull("Floor(NULL_NUMBER)");
    evalNull("Floor(NULL_BIGNUMBER)");

    evalFails("Floor()");
    evalFails("Floor(1,2,3)");
    evalFails("Floor('x')");

    // Function repetition
    optimize("FLOOR(FLOOR(FIELD_NUMBER))", "FLOOR(FIELD_NUMBER)");
  }

  @Test
  public void Round() throws Exception {
    evalEquals("Round(1)", 1L).returnType(NumberType.NUMBER);
    evalEquals("Round(125.49)", 125L);
    evalEquals("Round(125.99)", 126L);
    evalEquals("Round(0.4873)", 0L);
    evalEquals("Round(-0.65)", -1L);

    evalNull("Round(NULL_INTEGER)");
    evalNull("Round(NULL_NUMBER)");
    evalNull("Round(NULL_BIGNUMBER)");

    evalFails("Round()");
    evalFails("Round(1,2,3)");
    evalFails("Round('x')");

    // Function repetition
    optimize("ROUND(ROUND(FIELD_NUMBER))", "ROUND(FIELD_NUMBER)");
  }

  @Test
  public void Ln() throws Exception {
    evalEquals("Ln(1)", 0L).returnType(NumberType.NUMBER);
    evalEquals("Ln(Exp(2.4))", 2.4D).returnType(NumberType.NUMBER);
    evalEquals("Ln(10)", new BigDecimal("2.3025850929940456840179914546844")).returnType(NumberType.NUMBER);

    evalNull("Ln(NULL_INTEGER)");
    evalNull("Ln(NULL_NUMBER)");

    evalFails("Ln(0)");
    evalFails("Ln()");
  }

  @Test
  public void Log() throws Exception {
    evalEquals("Log(10,100)", 2L).returnType(NumberType.NUMBER);
    evalEquals("Log(10,1000)", 3L);

    evalNull("Log(10,NULL_INTEGER)").returnType(NumberType.NUMBER);
    evalNull("Log(NULL_INTEGER,1)").returnType(NumberType.NUMBER);

    evalFails("Log(10,0)");
    evalFails("Log(-2)");
    evalFails("Log(1)");
    evalFails("Log(x,y)");
  }

  @Test
  public void Log10() throws Exception {
    evalEquals("Log10(10)", 1L).returnType(NumberType.NUMBER);
    evalEquals("Log10(1000)", 3L).returnType(NumberType.NUMBER);
    evalNull("Log10(NULL_INTEGER)").returnType(NumberType.NUMBER);
    evalFails("Log10(-1)");
    evalFails("Log10()");
    evalFails("Log10(1,2)");
  }

  @Test
  public void Degrees() throws Exception {
    evalEquals("Degrees(Pi())", 180L).returnType(NumberType.NUMBER);
    evalEquals("Degrees(Radians(50))", 50L).returnType(NumberType.NUMBER);
    evalNull("Degrees(NULL_INTEGER)").returnType(NumberType.NUMBER);
    evalFails("Degrees()");
    evalFails("Degrees(1,2)");
  }

  @Test
  public void Radians() throws Exception {
    evalEquals("Radians(180)", PI).returnType(NumberType.NUMBER);
    evalNull("Radians(NULL_INTEGER)").returnType(NumberType.NUMBER);
    evalFails("Radians()");
    evalFails("Radians(1,2)");
  }

  @Test
  public void Crc32() throws Exception {
    evalEquals("CRC32('Apache Hop')", "dbb81b5e").returnType(StringType.STRING);
    evalEquals("CRC32(BINARY '123456789ABCDEF')", "2f720f20").returnType(StringType.STRING);
    evalNull("CRC32(NULL_STRING)").returnType(StringType.STRING);
    evalFails("CRC32()");
  }

  @Test
  public void MD5() throws Exception {
    evalEquals("MD5('Test')", "0cbc6611f5540bd0809a388dc95a615b").returnType(StringType.STRING);
    evalEquals("MD5(BINARY '123456789ABCDEF123456789ABCDEF123456789ABCDEF123456789ABCDEF')",
        "99c415050a2cddbeb525670345ff0aee").returnType(StringType.STRING);
    evalNull("MD5(NULL_STRING)").returnType(StringType.STRING);
    evalFails("MD5()");
  }

  @Test
  public void Sha1() throws Exception {
    evalEquals("SHA1('Test')", "640ab2bae07bedc4c163f679a746f7ab7fb5d1fa").returnType(StringType.STRING);
    evalNull("SHA1(NULL_STRING)").returnType(StringType.STRING);
    evalFails("SHA1()");
  }

  @Test
  public void Sha224() throws Exception {
    evalEquals("SHA224('Test')", "c696f08d2858549cfe0929bb7b098cfa9b64d51bec94aa68471688e4").returnType(StringType.STRING);
    evalNull("SHA224(NULL_STRING)").returnType(StringType.STRING);
    evalFails("SHA224()");
  }

  @Test
  public void Sha256() throws Exception {
    evalEquals("SHA256('Test')",
        "532eaabd9574880dbf76b9b8cc00832c20a6ec113d682299550d7a6e0f345e25").returnType(StringType.STRING);
    evalNull("SHA256(NULL_STRING)").returnType(StringType.STRING);
    evalFails("SHA256()");
  }

  @Test
  public void Sha384() throws Exception {
    evalEquals("SHA384('Test')",
        "7b8f4654076b80eb963911f19cfad1aaf4285ed48e826f6cde1b01a79aa73fadb5446e667fc4f90417782c91270540f3").returnType(StringType.STRING);
    evalNull("SHA384(NULL_STRING)").returnType(StringType.STRING);
    evalFails("SHA384()");
  }

  @Test
  public void Sha512() throws Exception {
    evalEquals("SHA512('Test')",
        "c6ee9e33cf5c6715a1d148fd73f7318884b41adcb916021e2bc0e800a5c5dd97f5142178f6ae88c8fdd98e1afb0ce4c8d2c54b5f37b30b7da1997bb33b0b8a31").returnType(StringType.STRING);
    evalNull("SHA512(NULL_STRING)").returnType(StringType.STRING);
  }

  @Test
  public void Random() throws Exception {
    assertFalse(FunctionRegistry.getFunction("RANDOM").isDeterministic());

    evalTrue("Random() between 0 and 1");

    // Keep the same context
    // Warning Random implementation is not the same on each JVM
    Evaluator evaluator = new Evaluator(createExpressionContext(true), "Random()");
    evaluator.returnType(NumberType.NUMBER);
    
    // Evaluate should execute
    Object value = evaluator.eval(Object.class);
    assertTrue(value instanceof BigDecimal);
    double randomValue = ((BigDecimal) value).doubleValue();
    assertTrue(0 <= randomValue && randomValue < 1);

    evalFails("Random('test')");
    evalFails("Random(1,2)");

    // Alias
    evalTrue("Rand()>0");

    // Optimize should do nothing
    optimize("RANDOM()", "RANDOM()");
  }


  @Test
  public void Uuid() throws Exception {
    assertFalse(FunctionRegistry.getFunction("UUID").isDeterministic());

    evalEquals("Length(Uuid())", 36L);
    
    returnType("UUID()", StringType.STRING);
  }

  @Test
  public void Compress() throws Exception {
    evalEquals("Decompress(Compress('Test'::BINARY))::STRING", "Test");
    evalNull("Compress(NULL_BINARY)").returnType(BinaryType.BINARY);    
  }

  @Test
  public void Decompress() throws Exception {
    evalEquals("Decompress(Compress('Test'::BINARY))::STRING", "Test");
    evalNull("Decompress(NULL_BINARY)").returnType(BinaryType.BINARY);    
  }

  @Test
  public void BitGet() throws Exception {
    evalTrue("Bit_Get(3,1)").returnType(BooleanType.BOOLEAN);
    evalTrue("Bit_Get(3,2)");
    evalFalse("Bit_Get(3,4)");

    // Overflow is always false
    evalFalse("Bit_Get(3,255)");

    evalNull("Bit_Get(123,0)");
    evalNull("Bit_Get(123,-1)");
    evalNull("Bit_Get(NULL_INTEGER,3)");
    evalNull("Bit_Get(123, NULL_INTEGER)");

    evalFails("Bit_Get()");
    evalFails("Bit_Get(123)");
  }

  @Test
  public void BitCount() throws Exception {
    evalEquals("Bit_Count(FIELD_INTEGER)", 2L).returnType(IntegerType.INTEGER);
    evalEquals("Bit_Count(31)", 5L);
    evalEquals("Bit_Count(True)", 1L);

    evalNull("Bit_Count(NULL_INTEGER)").returnType(IntegerType.INTEGER);

    evalFails("Bit_Count()");
    evalFails("Bit_Count(1,2)");
  }

  @Test
  public void BitSet() throws Exception {
    evalEquals("Bit_Set(16,1)", 17L).returnType(IntegerType.INTEGER);
    evalEquals("Bit_Set(1,4)", 9L);
    evalEquals("Bit_Set(16,4)", 24L);
    evalEquals("Bit_Set(32,4)", 40L);

    // Overflow has no impact on result
    evalEquals("Bit_Set(32,66)", 32L);

    evalNull("Bit_Set(123,0)").returnType(IntegerType.INTEGER);
    evalNull("Bit_Set(123,-1)");
    evalNull("Bit_Set(NULL_INTEGER,3)");
    evalNull("Bit_Set(123, NULL_INTEGER)");

    evalFails("Bit_Set()");
    evalFails("Bit_Set(123)");
  }

  @Test
  public void BitClear() throws Exception {
    evalEquals("Bit_Clear(3,1)", 2L).returnType(IntegerType.INTEGER);
    evalEquals("Bit_Clear(3,4)", 3L);

    // Overflow has no impact on result
    evalEquals("Bit_Clear(32,66)", 32L);

    evalNull("Bit_Clear(123,0)").returnType(IntegerType.INTEGER);
    evalNull("Bit_Clear(123,-1)");
    evalNull("Bit_Clear(NULL_INTEGER,3)");
    evalNull("Bit_Clear(123, NULL_INTEGER)");

    evalFails("Bit_Clear()");
    evalFails("Bit_Clear(123)");
  }

  @Test
  public void BitShift() throws Exception {
    evalEquals("Bit_Shift(123,0)", 123L).returnType(IntegerType.INTEGER);
    evalEquals("Bit_Shift(1,4)", 16L).returnType(IntegerType.INTEGER);
    evalEquals("Bit_Shift(2,8)", 512L);
    evalEquals("Bit_Shift(6,-2)", 1L);
    evalEquals("Bit_Shift(16,-4)", 1L);
    evalEquals("Bit_Shift(10000,-3)", 1250L);
    evalEquals("Bit_Shift(1,63)", 0x8000000000000000L);
    // evalEquals("Bit_Shift(0x8000000000000000,-63)", 1);

    // Cast to integer
    evalEquals("Bit_Shift(16.3,-4)", 1L);
    evalEquals("Bit_Shift(16.9,-4)", 1L);

    // Overflow
    evalEquals("Bit_Shift(1,64)", 0L);
    evalEquals("Bit_Shift(1,-64)", 0L);
    // Underflow
    evalEquals("Bit_Shift(1,-1)", 0L);


    evalNull("Bit_Shift(NULL_INTEGER,3)").returnType(IntegerType.INTEGER);
    evalNull("Bit_Shift(123, NULL_INTEGER)");

    evalFails("Bit_Shift('Bidon',3)");
    evalFails("Bit_Shift()");
    evalFails("Bit_Shift(123)");
    evalFails("Bit_Shift(DATE '2022-11-25', 3)");
  }

  @Test
  public void BitRotate() throws Exception {
    evalEquals("Bit_Rotate(123,0)", 123L).returnType(IntegerType.INTEGER);
    evalEquals("Bit_Rotate(1,4)", 16L).returnType(IntegerType.INTEGER);
    evalEquals("Bit_Rotate(16,-4)", 1L);
    evalEquals("Bit_Rotate(-9223372036854775807,2)", 6L);
    evalEquals("Bit_Rotate(6,-2)", -9223372036854775807L);
    evalEquals("Bit_Rotate(10000,-3)", 1250L);
    // Full rotate 64 bits
    evalEquals("Bit_Rotate(123456,64)", 123456L);
    evalEquals("Bit_Rotate(123456,128)", 123456L);
    evalEquals("Bit_Rotate(123456,-64)", 123456L);
    evalEquals("Bit_Rotate(123456,-128)", 123456L);

    evalNull("Bit_Rotate(NULL_INTEGER,3)").returnType(IntegerType.INTEGER);
    evalNull("Bit_Rotate(123, NULL_INTEGER)");

    evalFails("Bit_Rotate()");
    evalFails("Bit_Rotate(123)");
  }

  @Test
  public void TypeOf() throws Exception {
    evalEquals("TypeOf(TRUE)", "BOOLEAN").returnType(StringType.STRING);
    evalEquals("TypeOf('str')", "STRING").returnType(StringType.STRING);
    evalEquals("TypeOf(25)", "INTEGER").returnType(StringType.STRING);
    evalEquals("TypeOf(FIELD_NUMBER)", "NUMBER").returnType(StringType.STRING);
    evalEquals("TypeOf(DATE '2023-01-01')", "DATE").returnType(StringType.STRING);
    evalEquals("TypeOf(INTERVAL '3 years')", "INTERVAL").returnType(StringType.STRING);
  }

  @Test
  public void Extract() throws Exception {
    // Extract part from temporal
    evalEquals("Extract(MILLENNIUM from TIMESTAMP '2020-05-25 23:48:59')", 3L).returnType(IntegerType.INTEGER);
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
    evalEquals(
        "Extract(TIMEZONE_MINUTE from TIMESTAMP '2021-01-01 15:28:59' AT TIME ZONE 'Asia/Tokyo')",
        0L);
    evalNull("Extract(SECOND from NULL_DATE)").returnType(IntegerType.INTEGER);

    // Alias
    evalEquals("Date_Part(HOUR,TIMESTAMP '2020-05-25 23:48:59')", 23L).returnType(IntegerType.INTEGER);

    // Extract part from interval
    evalEquals("Extract(MILLENNIUM from INTERVAL 1234 YEAR)", 1L).returnType(IntegerType.INTEGER);
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


    evalFails("Extract(");
    evalFails("Extract()");
    evalFails("Extract(MONTH)");
    evalFails("Extract(MONTH DATE '2023-12-01')");
    evalFails("Extract(MONTH FROM DATE '2023-12-01'");
    evalFails("Extract(123 from DATE '2021-01-01')");
    evalFails("Extract('TEST' from DATE '2021-01-01')");
    evalFails("Extract(NULL from DATE '2021-01-01')");
    evalFails("Extract(BIDON from NULL)");
    evalFails("Extract(BIDON from DATE '2021-01-01')");
    evalFails("Extract(DAY DATE '2021-01-01')");
    evalFails("Extract(EPOCH from INTERVAL -45 DAYS)");

    optimize("EXTRACT(CENTURY FROM FIELD_DATE)");
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
  public void Position() throws Exception {
    evalEquals("Position('abc' IN 'abcdefgh')", 1L).returnType(IntegerType.INTEGER);
    evalEquals("Position('XYZ' IN 'abcdefgh')", 0L).returnType(IntegerType.INTEGER);
    evalEquals("Position('def' IN 'abcdefgh')", 4L).returnType(IntegerType.INTEGER);
    evalNull("Position(NULL_STRING IN 'abcdefgh')").returnType(IntegerType.INTEGER);
    evalNull("Position('abc' IN NULL_STRING)").returnType(IntegerType.INTEGER);
    evalFails("Position('abc' IN ");
    evalFails("Position( IN 'fsd'");
  }
}
