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

import org.apache.hop.expression.type.DataTypeName;
import org.junit.Test;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;

public class OperatorsTest extends BaseExpressionTest {

  @Test
  public void EqualTo() throws Exception {
    evalTrue("NAME = 'TEST'");
    evalTrue("Age = 40");
    evalTrue("FLAg = true");
    evalTrue("2.000 = 2");
    evalTrue("2.000 = 2.00");
    evalTrue("-1.4e-10 = -1.4e-10");

    // Integer
    evalTrue("'0.0' = 0");
    evalTrue("0.0 = '0.000'");
    evalTrue("15.0 = '15'");
    evalTrue("'.01' = 0.01");

    // Binary
    evalTrue("0b11110000 = 0xF0");
    
    // Boolean
    evalTrue("true = true");
    evalTrue("false = false");
    evalFalse("true = false");
    evalFalse("false = true");

    // String
    evalTrue("'ABC' = 'ABC'");
    evalFalse("'ABC' = 'abc'");

    // Date
    evalTrue("Date '2019-01-01' = Date '2019-01-01'");
    evalFalse("Date '2019-01-01' = Date '2018-01-01'");
    
    // Timestamp
    evalTrue("Timestamp '2019-01-01 8:00:00' AT TIME ZONE 'America/New_York' = Timestamp '2019-01-01 14:00:00' AT TIME ZONE 'Europe/Berlin'");
    evalTrue("Timestamp '2019-01-01 08:00:00 -08:00' = Timestamp '2019-01-01 11:00:00 -05:00'");
    evalTrue("Timestamp '2019-01-01 8:00:00 -08:00' = Timestamp '2019-01-01 11:00:00 -05:00'");
    evalFalse("Timestamp '2019-01-01 08:00:00 -08:00' = Timestamp '2019-01-01 8:00:00 -05:00'");
    
    // Null    
    evalNull("VALUE_NULL = null");
    evalNull("1 = null");
    evalNull("null = true");
    evalNull("null = false");
    // NULL is not equal ( = ) to anything—not even to another NULL.
    evalNull("null = null");

    evalFails("NOM=");
    evalFails("NOM = ");

    writeEquals("AGE=40");
    
    returnType("AGE=40", DataTypeName.BOOLEAN);
  }

  @Test
  public void NotEqualTo() throws Exception {
    evalTrue("'bar' != 'foo'");
    evalTrue("NAME <> 'tEST'");
    evalFalse("Age != 40");
    evalFalse("Age <> 40");

    evalTrue("1 <> 2");
    //evalTrue("10 <> 0x10");
    evalFalse("1 <> '1'");

    evalTrue("true <> false");
    evalTrue("false <> true");
    evalFalse("true <> true");
    evalFalse("false <> false");

    evalFalse("2 <> 2.000");
    evalFalse("2.000 <> 2.00");
    evalFalse("true <> true");
    evalTrue("Date '2019-01-01' <> Date '2018-01-01'");
    evalFalse("Date '2019-01-01' <> Date '2019-01-01'");

    evalTrue("Timestamp '2019-01-01 8:00:00' AT TIME ZONE 'UTC' <> Timestamp '2019-01-01 8:00:00' AT TIME ZONE 'US/Pacific'");
    evalFalse("Timestamp '2019-01-01 08:00:00 -8:00' <> Timestamp '2019-01-01 11:00:00 -5:00'");
    
    evalNull("null <> 'bar'");
    evalNull("'bar' <> null");
    evalNull("null <> null");

    evalFails("NOM<>");
    evalFails("NOM <> ");
    evalFails("NOM!");
    evalFails("NOM ! ");
    
    writeEquals("10!=AGE");
    
    returnType("AGE<>40", DataTypeName.BOOLEAN);  
  }

  @Test
  public void GreaterThan() throws Exception {
    evalTrue("9>5");
    evalTrue("9.4>9.358");
    evalTrue("(4+2)>10-9");
    evalTrue("Age>10");
    evalFalse("5>5");
    evalFalse("0xF5>0xFE");
    
    evalFalse("false > true");
    evalFalse("false > false");
    evalFalse("true > true");
    evalTrue("true > false");

    evalFalse("'bar' > 'foo'");
    evalFalse("'foo' > 'foo'");
    evalTrue("'foo' > 'bar'");

    evalTrue("Date '2019-02-01' > Date '2019-01-01'");
    evalFalse("Date '2019-01-01' > Date '2019-01-01'");
    evalFalse("Date '2018-01-01' > Date '2019-01-01'");

    evalNull("VALUE_NULL > 0");
    evalNull("1 > VALUE_NULL");

    evalFails("NOM>");
    evalFails("NOM > ");
    evalFails("NOM>5");
    
    writeEquals("10>AGE");
    
    returnType("'bar' > 'foo'", DataTypeName.BOOLEAN);
  }

  @Test
  public void GreaterThanOrEqualTo() throws Exception {
    evalTrue("9 >= 5");
    evalTrue("9.4 >= 9.358");
    evalTrue("(4+2) >= 10-9");
    evalTrue("Age >= 10");
    evalTrue("5 >= 5");

    evalFalse("false >= true");
    evalTrue("false >= false");
    evalTrue("true >= true");
    evalTrue("true >= false");

    evalFalse("'bar' >= 'foo'");
    evalTrue("'foo' >= 'foo'");
    evalTrue("'foo' >= 'bar'");

    evalTrue("Date '2019-02-01' >= Date '2019-01-01'");
    evalTrue("Date '2019-01-01' >= Date '2019-01-01'");
    evalFalse("Date '2018-01-01' >= Date '2019-01-01'");

    evalNull("VALUE_NULL >= 0");
    evalNull("1 >= VALUE_NULL");

    evalFails("NOM>=");
    evalFails("NOM >=");
    evalFails("NOM>=5");
    
    writeEquals("AGE>=80");
    
    returnType("'bar' >= 'foo'", DataTypeName.BOOLEAN);
  }

  @Test
  public void LessThan() throws Exception {
    evalTrue("5 < 9");
    evalTrue("9.358 < 9.4");
    evalTrue("10-9 < (4+2)");
    evalTrue("Age < 100");
    evalFalse("5 < 5");

    evalFalse("true < false");
    evalTrue("false < true");
    evalFalse("false < false");
    evalFalse("true < true");

    evalTrue("'bar' < 'foo'");
    evalFalse("'foo' < 'foo'");
    evalFalse("'foo' < 'bar'");

    evalTrue("Date '2019-01-01' < Date '2019-02-01'");
    evalFalse("Date '2019-01-01' < Date '2019-01-01'");
    evalFalse("Date '2019-01-01' < Date '2018-01-01'");

    evalNull("null < 1");
    evalNull("0 < null");

    evalFails("NOM<");
    evalFails("NOM < ");
    evalFails("NOM < 5");
    
    writeEquals("AGE<80");
    
    returnType("'bar' < 'foo'", DataTypeName.BOOLEAN);
  }

  @Test
  public void LessThanOrEqualTo() throws Exception {
    evalTrue("5 <= 9");
    evalTrue("9.358 <= 9.4");
    evalTrue("10-9 <= (4+2)");
    evalTrue("Age <= 100");
    evalTrue("5 <= 5");

    evalTrue("false <= false");
    evalTrue("true <= true");
    evalTrue("false <= true");
    evalFalse("true <= false");

    evalTrue("'foo' <= 'foo'");
    evalTrue("'bar' <= 'foo'");
    evalFalse("'foo' <= 'bar'");

    evalTrue("Date '2019-01-01' <= Date '2019-02-01'");
    evalTrue("Date '2019-01-01' <= Date '2019-01-01'");
    evalFalse("Date '2019-01-01' <= Date '2018-01-01'");

    evalNull("null <= 1");
    evalNull("0 <= null");

    evalFails("NOM<=");
    evalFails("NOM <=");
    evalFails("NOM <=5");
    
    writeEquals("AGE<=5");
    
    returnType("'bar' <= 'foo'", DataTypeName.BOOLEAN);
  }

  @Test
  public void In() throws Exception {
    evalTrue("SEX in ('?','F','RM')");
    evalTrue("SEX not in ('?','-','!')");
    evalTrue("2 in (1,2,3)");
    
    evalTrue("2.5 IN (1,2.5,3)");
    evalTrue("'2' in (null,1,2,3)");
    evalTrue("Date '2019-01-01' in (Date '2019-04-01',Date '2019-01-01',Date '2019-03-06')");
    evalTrue("0x0123456789 in (0x9876,0x0123456789,0x3698)");
    evalFalse("2 in (1,2.5,3)");

    evalTrue("2 in (null,1,2,3)");
    evalFalse("2 in (null,null,null)");
    evalFalse("1 not in (null,1)");
    evalNull("NULL in (1,2,3)");
    evalNull("NULL in (1,2,3,null)");

    evalFails("2 in (1,2.5,)");
    evalFails("2 in ()");

    writeEquals("AGE IN (10,20,30,40)");
    
    returnType("AGE IN (10,20,30,40)", DataTypeName.BOOLEAN);
  }

  @Test
  public void Is() throws Exception {
    evalTrue("True IS True");
    evalTrue("True IS NOT False");
    evalTrue("FLAG is True");
    evalFalse("VALUE_NULL IS True");  
    evalFalse("True IS False");
    evalFalse("True IS Null");
    evalTrue("False IS False");
    evalTrue("False IS NOT TRUE");   
    evalFalse("VALUE_NULL IS False");
    
    evalFalse("False IS Null");
    evalFalse("VALUE_NULL IS NOT NULL");
    evalFalse("Null is True");
    evalFalse("Null IS False");
    evalTrue("Null IS NULL");
    evalTrue("VALUE_NULL IS NULL");

    writeEquals("FLAG IS TRUE");
    
    returnType("FLAG IS TRUE", DataTypeName.BOOLEAN);
  }

  @Test
  public void Add() throws Exception {
    evalEquals("10+(-0.5)", 9.5);
    evalEquals("0xF+0", 15);
    evalEquals("0b00011+0", 3);
    evalEquals("-24.7+0.5+24.7+0.5E-2", 0.505);
    evalEquals("PRICE+PRICE", -10.24);
    evalEquals("AMOUNT+1", 123456.789 + 1);

    
    
    
    //evalEquals("Date '2019-02-25'+1", LocalDate.of(2019, 2, 26));
    //evalEquals("Date '2019-02-25'+1.5", LocalDateTime.of(2019, 2, 26, 12, 0, 0));
    //evalEquals("Date '2019-02-25'+5/(60*24)", LocalDateTime.of(2019, 2, 25, 0, 5, 0));

    evalNull("5+NULL+5");
    evalNull("+NULL+5");
    evalFails("5+");
    evalFails("TRUE+FALSE");
    
    writeEquals("10+AGE");
  }

  @Test
  public void Subtract() throws Exception {
    evalEquals("10-0.5", 9.5);
    evalEquals("Age-0.5", 39.5);
    evalEquals("Age-10::INTEGER", 30L);
    
    //evalEquals("Date '2019-02-25'-1", LocalDate.of(2019, 2, 24));
    //evalEquals("Date '2019-02-25'-28", LocalDate.of(2019, 1, 28));
    //evalEquals("Date '2019-02-25'-0.5", LocalDateTime.of(2019, 2, 24, 12, 0, 0));
    //evalEquals("Date '2019-02-25'-5/(60*24)", LocalDateTime.of(2019, 2, 24, 23, 55, 0));

    //evalEquals("Date '2019-02-25'-Date '2019-02-23'", 2);
    //evalEquals("Date '2019-02-23'-Date '2019-02-25'", -2);
    //evalEquals("Date '2019-02-25'-to_Date('2019-02-23 12:00','YYYY-MM-DD HH24:MI')", 1.5);

    evalNull("5-NULL");
    evalNull("NULL-5");

    evalFails("5-");
    evalFails("TRUE-FALSE");
    
    writeEquals("10-AGE");
  }

  @Test
  public void Between() throws Exception {
    evalTrue("3 between 1 and 5");
    evalTrue("3 between 3 and 5");
    evalTrue("5 between 3 and 5");
    evalFalse("5 between 5 and 3");
    evalTrue("AGE between symmetric 50 and 30");
    evalTrue("-1 between -3+1 and 5");
    evalTrue("'the' between 'that' and 'then'");
    evalFalse("1 between 3 and 5");
    evalTrue("Age between 39.999 and 40.0001");
    evalTrue("Age not between 10 and 20");
    evalTrue("Age not between 10 and 20 and 'Test' is not null");

    evalTrue("Date '2019-02-28' between Date '2019-01-01' and Date '2019-12-31'");

    evalNull("NULL between -10 and 20");
    evalNull("NULL between symmetric -10 and 20");
    evalNull("1 between NULL and 20");    
    evalNull("1 between symmetric NULL and 20");
    evalNull("1 between -10 and NULL");
    evalNull("1 between symmetric -10 and NULL");

    evalFails("Age between 10 and");
    evalFails("Age between and 10");
    evalFails("Age between and ");

    writeEquals("AGE BETWEEN 10 AND 20");
    writeEquals("AGE BETWEEN SYMMETRIC 50 AND 20");
    writeEquals("NAME BETWEEN 'AZE' AND 'KLM'");
    
    
    returnType("5 between 3 and 5", DataTypeName.BOOLEAN);
  }

  @Test
  public void Cast() throws Exception {

    // String to Boolean
    evalTrue("'Yes'::Boolean");
    evalTrue("'Yes' :: Boolean");
    evalTrue("CAST('YES' as Boolean)");
    evalTrue("CAST('yes' as Boolean)");
    evalTrue("CAST('Yes' as Boolean)");
    evalFalse("CAST('No' as Boolean)");
    evalFalse("CAST('no' as Boolean)");
    evalFalse("CAST('nO' as Boolean)");
    evalTrue("CAST('Y' as Boolean)");
    evalFalse("CAST('n' as Boolean)");
    evalTrue("CAST('ON' as Boolean)");
    evalTrue("CAST('on' as Boolean)");
    evalTrue("CAST('On' as Boolean)");
    evalTrue("CAST('oN' as Boolean)");
    evalFalse("CAST('OFF' as Boolean)");
    evalFalse("CAST('off' as Boolean)");
    evalFalse("CAST('Off' as Boolean)");
    evalTrue("CAST('TRUE' as Boolean)");
    evalTrue("CAST('true' as Boolean)");
    evalTrue("CAST('True' as Boolean)");
    evalFalse("CAST('FALSE' as Boolean)");
    evalFalse("CAST('false' as Boolean)");
    evalFalse("CAST('False' as Boolean)");

    // Number to Boolean
    evalTrue("CAST(1 as Boolean)");
    evalTrue("1::Boolean");
    evalTrue("CAST(-12.1 as Boolean)");
    evalFalse("CAST(0 as Boolean)");

    // Number to Integer
    evalEquals("CAST(1.25 as Integer)", 1);
    // Oracle truncate to 1 and Snowflake round it to 2
    evalEquals("CAST(1.75 as Integer)", 1);

    // Boolean to String
    evalEquals("CAST(true as String)", "TRUE");
    evalEquals("true::String", "TRUE");

    // Date to String
    evalEquals("CAST(Date '2019-02-25' AS String)", "2019-02-25");
    evalEquals("CAST(Date '2019-02-25' AS String FORMAT 'DD/MM/YYYY')", "25/02/2019");

    // evalEquals("Cast(Time '23:48:59' as String)", "23:48:59");

    // evalEquals("Timestamp '1900-01-04 12:00'::Number", 3.5);

    // evalEquals("Time '23:48:59'+1.5", "23:48:59");

    // Integer to String
    evalEquals("CAST(12923 AS STRING)", "12923");
    evalEquals("CAST(-1234 AS STRING FORMAT '9999MI')", "1234-");

    // Number to String
    evalEquals("CAST(123.6 as String)", "123.6");
    evalEquals("123.6::String", "123.6");
    evalEquals("CAST(0.45 AS STRING)", ".45");
    evalEquals("CAST(0.45 AS STRING FORMAT 'FM000.00')", "000.45");
    evalEquals("CAST(1234.56 AS STRING FORMAT '9999MI')", "1234 ");

    // String to Integer
    evalEquals("CAST('1234' as Integer)", 1234L);
    evalEquals("'1234'::Integer+5", 1239L);
    evalEquals("CAST('1234.567' as Integer)", 1234L);

    // String to Number
    evalEquals("'1'::Number", 1d);
    evalEquals("'1234'::Number", 1234d);
    evalEquals("CAST('1234' as Number)", 1234d);
    evalEquals("CAST('1234.567' as Number)", 1234.567d);
    evalEquals("CAST('  -1e-37  ' as Number)", -1e-37d);

    // Number to BigNumber
    evalEquals("CAST(0 as BigNumber)", 0);
    evalEquals("CAST(1234.456 as BigNumber)", 1234.456);

    // String to BigNumber
    evalEquals("CAST('0' as BigNumber)", 0);
    evalEquals("CAST('1' As BigNumber)", 1);
    evalEquals("CAST('-1e-37' as BigNumber)", -1e-37d);
    evalEquals("CAST(' -1e-37 ' as BigNumber)", -1e-37d);
    
    // String to Date
    evalEquals("CAST('2020-march' as DATE FORMAT 'YYYY-MONTH')", LocalDate.of(2020, 3, 1));
    evalEquals("CAST('2020-01-19 11:23:44' as DATE FORMAT 'YYYY-MM-DD HH:MI:SS')", LocalDateTime.of(2020, 1, 19, 11,23,44));
    
    // Binary to Integer
    evalEquals("CAST(0x123 as Integer)", 291L);


    evalEquals("TO_NUMBER('123','000')::INTEGER+1", 124);

    evalEquals("CAST(12345678901234567890123456789012345678 as BigNumber)",
        new BigDecimal("12345678901234567890123456789012345678"));

    evalNull("CAST(Null as Binary)");
    evalNull("CAST(Null as Boolean)");
    evalNull("CAST(Null as String)");
    evalNull("CAST(Null as Integer)");
    evalNull("CAST(Null as Number)");
    evalNull("CAST(Null as BigNumber)");

    // Unsupported conversion
    evalFails("CAST(Date '2019-02-25' AS INTEGER)");
    evalFails("CAST(Date '2019-02-25' AS NUMBER)");
    evalFails("CAST(TRUE AS DATE)");
    evalFails("CAST(Date '2019-02-25' AS BOOLEAN )");
    evalFails("CAST(Date '2019-02-25' AS BOOLEAN)");

    // Bad syntax
    evalFails("'1234':");
    evalFails("'1234':NUMBER");
    evalFails("'1234'::");
    evalFails("CAST('bad' AS)");
    evalFails("CAST('bad' AS NULL)");
    evalFails("CAST('2020-01-01' AS NULL)");
    evalFails("CAST(1234 AS STRING FORMAT )");
    evalFails("CAST(Date '2019-02-25' AS String FORMAT )");
    evalFails("CAST(Date '2019-02-25' AS String FORMAT NULL)");

    // Bad data type
    evalFails("Cast(123 as Nill)");

    writeEquals("CAST(DATA AS BINARY)", "CAST(DATA AS BINARY)");
    writeEquals("CAST(AGE AS NUMBER)", "CAST(AGE AS NUMBER)");
    writeEquals("AGE::NUMBER", "CAST(AGE AS NUMBER)");
    
    returnType("CAST(3 as BOOLEAN)", DataTypeName.BOOLEAN);
    returnType("CAST('3' as INTEGER)", DataTypeName.INTEGER);  
    returnType("CAST(Date '2019-02-25' AS Date FORMAT 'YYY-MM-DD')", DataTypeName.DATE);
  }

  @Test
  public void AtTimeZone() throws Exception {
    evalEquals("Timestamp '2020-05-25 20:48:00' AT TIME ZONE 'Europe/Paris'", ZonedDateTime.of(2020, 5, 25, 20,48,00,0,ZoneId.of("Europe/Paris")));
    evalEquals("Timestamp '2020-05-25 20:48:00' AT TIME ZONE 'Singapore'", ZonedDateTime.of(2020, 5, 25, 20,48,00,0,ZoneId.of("Singapore")));
    evalEquals("Timestamp '2020-05-25 20:48:00' AT TIME ZONE 'GMT+0'", ZonedDateTime.of(2020, 5, 25, 20,48,00,0,ZoneId.of("GMT+0")));
    evalEquals("Timestamp '2020-05-25 20:48:00' AT TIME ZONE 'CET'", ZonedDateTime.of(2020, 5, 25, 20,48,00,0,ZoneId.of("CET")));
    evalEquals("Timestamp '2020-05-25 20:48:00' AT TIME ZONE 'EET'", ZonedDateTime.of(2020, 5, 25, 20,48,00,0,ZoneId.of("EET")));
    evalEquals("Add_Days(Timestamp '2020-05-25 10:48:00' AT TIME ZONE 'UTC',1) AT TIME ZONE 'Asia/Singapore'", ZonedDateTime.of(2020, 5, 26, 18,48,00,0,ZoneId.of("Asia/Singapore")));    
    evalFails("Timestamp '2020-05-25 20:48:00' AT TIME ZONE 'XYZ'");
 
    //writeEquals("Timestamp '2020-05-25 20:48:00' AT TIME ZONE 'Europe/Paris'");
  }
  

  
  @Test
  public void Positive() throws Exception {
    evalEquals("+(40)", 40);
    evalEquals("+(Age)", 40);
    evalEquals("+40", 40);
    evalEquals("1+ +2", 3);
    evalNull("+null");
    
    writeEquals("+AGE","AGE");
  }

  @Test
  public void Negative() throws Exception {
    evalEquals("-(1+2)", -3);
    evalEquals("-40", -40);
    evalEquals("-Age", -40);
    evalEquals("+40", 40);
    evalEquals("1+ -2", -1);
    evalNull("-null");
    writeEquals("-AGE","-AGE");
  }

  @Test
  public void Mod() throws Exception {
    evalEquals("15%4", 3);
    evalEquals("Mod(15,4)", 3);
    evalEquals("Mod(15.3,4)", 3.3);
    evalEquals("Mod(15.3::BIGNUMBER,4)", 3.3);
    evalNull("Mod(NULL,2)");
    evalNull("Mod(2,NULL)");
    evalFails("'TEST'%5");
    evalFails("Mod()");
    evalFails("Mod(9,0)");
    evalFails("Mod(3)");
    
    writeEquals("AGE%4");
  }
  
  @Test
  public void Multiplication() throws Exception {
    evalEquals("2.55*10", 25.5D);
    evalEquals("4*10", 40D);
    evalEquals("-4*-1", 4D);
    evalEquals("2*-2", -4D);
    evalEquals("100 * .5", 50D);    
    evalEquals("1.23456::BigNumber*-2.987654", -3.68843812224);
    evalNull("null*1");
    evalNull("1*null");
    writeEquals("AGE*4","4*AGE");
  }

  @Test
  public void Div() throws Exception {
    evalEquals("10/4", 2.5D);
    evalEquals("40/-10", -4D);
    evalEquals("-40/-10", 4D);
    evalEquals("5/2", 2.5D);
    evalEquals("10.1/2.1",  4.809523809523809D);
    evalEquals("0.1/0.0000000000001", 1000000000000.0000000D);
    evalNull("null/1");
    evalNull("null/0");
    evalNull("1/null");
    evalFails("40/0");
        
    writeEquals("AGE/4");
  }

  @Test
  public void Div0() throws Exception {
    evalEquals("Div0(10,4)", 2.5D);
    evalEquals("Div0(AGE,-10)", -4D);    
    evalEquals("Div0(AGE,0)", 0);
    evalNull("Div0(null,1)");
    evalNull("Div0(null,0)");
    evalNull("Div0(1,null)");
    evalFails("Div0(40)");
  }
  
  @Test
  public void BitNot() throws Exception {
    evalEquals("~1", -2);
    evalEquals("~ 1", -2);
    evalEquals("~0", -1);
    evalEquals("~4", -5);
    evalEquals("~65504", -65505);
    evalNull("~NULL");
    evalFails("~");
    evalFails("~ ");

    // Alias
    evalEquals("BITNOT(1)", -2);
    
    writeEquals("~AGE");
    
    returnType("~AGE", DataTypeName.INTEGER);
  }

  @Test
  public void BitAnd() throws Exception {
    evalEquals("BITAND(3,2)", 2);
    evalEquals("3 & 2", 2);
    evalEquals("100 & 2", 0);
    evalNull("100 & null");
    evalNull("NULL & 100");
    evalFails("100&");
    evalFails("100 & ");
    
    writeEquals("AGE&4");
    
    returnType("AGE&4", DataTypeName.INTEGER);
  }

  @Test
  public void BitOr() throws Exception {
    evalEquals("BITOR(100,2)", 102);
    evalEquals("100 | 2", 102);
    evalEquals("3 | 2", 3);
    evalNull("100 | null");
    evalNull("NULL | 100");
    evalFails("3|");
    evalFails("3 | ");
    
    writeEquals("AGE|4");
    
    returnType("AGE|4", DataTypeName.INTEGER);
  }

  @Test
  public void BitXor() throws Exception {
    evalEquals("BITXOR(2,2)", 0);
    evalEquals("2 ^ 1", 3);
    evalEquals("100 ^ 2", 102);
    evalNull("100 ^ null");
    evalNull("NULL ^ 100");
    evalFails("100^");
    evalFails("100 ^ ");
    
    writeEquals("AGE^4");
    
    returnType("AGE^4", DataTypeName.INTEGER);
  }

  @Test
  public void BoolNot() throws Exception {
    evalTrue("FLAG is not false");
    evalTrue("VALUE_NULL is null");
    evalTrue("NOT (VALUE_NULL is not null)");
    evalFalse("NOT 1");
    evalTrue("NOT 0");
    evalTrue("NOT NOT True");
    evalNull("NOT NULL");
    evalFails("FLAG is ");
    evalFails("NOT");
    
    writeEquals("NOT FLAG");
    
    returnType("NOT FLAG", DataTypeName.BOOLEAN);
  }

  @Test
  public void BoolOr() throws Exception {
    evalTrue("true OR true");
    evalTrue("true OR false");
    evalTrue("false OR true");
    evalFalse("false OR false");
    evalTrue("true OR VALUE_NULL");
    evalTrue("true OR FIELD");
    evalTrue("VALUE_NULL OR true");
    evalTrue("FLAG OR false");
    evalNull("false OR VALUE_NULL");
    evalNull("VALUE_NULL OR false");
    evalNull("VALUE_NULL OR VALUE_NULL");
    
    evalFails("false OR");
    evalFails("OR false");

    //evalFails("true OR NAME");
    
    writeEquals("FLAG OR VALUE_NULL");
    
    returnType("false OR FLAG", DataTypeName.BOOLEAN);
  }

  @Test
  public void BoolAnd() throws Exception {
    evalTrue("true AND true");
    evalFalse("true AND false");    
    evalFalse("false AND true");
    evalFalse("false AND false");
    evalFalse("false AND FIELD");
    evalNull("FLAG AND null");
    evalNull("VALUE_NULL AND FLAG");
    evalFalse("false AND VALUE_NULL");
    evalFalse("VALUE_NULL AND false");
    evalNull("true AND VALUE_NULL");
    evalNull("VALUE_NULL AND true");
    evalNull("VALUE_NULL AND VALUE_NULL");
    
    evalFails("false AND");
    evalFails("AND false");

    writeEquals("FLAG AND VALUE_NULL");
    
    returnType("false AND FLAG", DataTypeName.BOOLEAN);
  }

  @Test
  public void ILike() throws Exception {
    evalTrue("'test' ILIKE '%t%'");
    evalTrue("'test' ILIKE '%T%'");

    // Escape with other char
    evalTrue("'Result 100% value' ilike 'RESULT%100^%%' escape '^'");

    evalNull("'test' ILIKE NULL");
    evalNull("'test' ILIKE 'TEST' escape NULL");
    evalNull("NULL ILIKE '%T%'");
    
    returnType("'amigo' ILIKE 'a%o' ESCAPE '@'", DataTypeName.BOOLEAN);
  }

  @Test
  public void Like() throws Exception {
    evalTrue("NAME like 'TES%'");
    evalTrue("NAME not like 'X%'");
    evalFalse("NAME like 'X%'");
    evalTrue("'Tuesday' like '%es%'");
    evalTrue("'...Tuesday....' like '%es%'");

    // Test one char
    evalTrue("'A' like '_'");
    // evalFalse("'AA' like '_'");

    // Test empty
    evalTrue("'' like '%'");
    evalFalse("'' like '_'");

    // values that starts with "a" and ends with "o"
    evalTrue("'amigo' like 'a%o'");
    evalTrue("'ao' like 'a%o'");
    // values that starts with "a" and are at least 3 characters in length
    evalTrue("'ami' like 'a_%_%'");
    evalTrue("'amigo' like 'a_%_%'");
    evalTrue("'Friday' like '___day'");
    evalFalse("'am' like 'a_%_%'");
    evalTrue("'AA' like '__'");
    evalFalse("'AA' like '___'");


    // New line
    evalTrue("'AA\nA' like 'AA%'");
    evalTrue("'AA\nA' like 'AA_A'");
    evalTrue("'AA\nA' like 'AA%A'");
    evalFalse("'AA\nA' like 'AA_'");

    // Escape with other char
    evalTrue("'Result 100% value' like '%100^%%' escape '^'");

    // Double escape char
    evalFalse("'^100% milles' like '^^100^%%' escape '^'");

    // Escape with Regexp special char
    evalTrue("'give me 30% discount' like '%30!%%' escape '!'");
    evalTrue("'ADD_MONTHS' like '%ADD!_%' escape '!'");

    // Optimizable
    evalTrue("'ABCDEFG' like 'ABCDEFG'");
    evalTrue("'ABCDEFG' like 'ABCDE%'");
    evalTrue("'ABCDEFG' like '%DEFG'");
    evalTrue("'ABCDEFG' like '%CDE%'");


    evalNull("NULL like 'NULL'");
    evalNull("'test' LIKE NULL");
    evalNull("'test' LIKE 'TEST' escape NULL");

    // NULL does not match NULL
    evalNull("NULL like NULL");

    evalFails("'give me 30% discount' like '%30!%%' escape '!!'");
    
    writeEquals("NAME LIKE 'ADD%'","STARTSWITH(NAME,'ADD')");
    writeEquals("NAME LIKE '%ADD!_%' ESCAPE '!'");
    
    returnType("'amigo' like 'a%o'", DataTypeName.BOOLEAN);
  }

  @Test
  public void Concat() throws Exception {
    // String
    evalEquals("CONCAT('TES','T')", "TEST");
    evalTrue("NAME='TES'||'T'");
    evalTrue("NAME='TES'||VALUE_NULL||'T'");
    evalEquals("'TEST'||null", "TEST");
    evalEquals("null||'TEST'", "TEST");
    
    // Binary
    evalEquals("0x1F || null || 0x2A3B", new byte[]{0x1F, 0x2A, 0x3B});
    evalEquals("null || 0x1F || 0x2A3B", new byte[]{0x1F, 0x2A, 0x3B});
    evalEquals("0x1F || 0x2A3B || null", new byte[]{0x1F, 0x2A, 0x3B});
    
    // Integer
    evalEquals("4 || 2", "42");
    
    evalNull("null||null");

    writeEquals("FIELD1||'TEST'");
  }

  @Test
  public void CaseWhen() throws Exception {
    // implicit ELSE NULL case
    evalNull("case when Age=10 then 10 end");
    evalEquals("case when Age=40 then 10 end", 10L);

    // explicit ELSE case
    evalEquals("case when Age=40 then 10 else 50 end", 10L);
    evalEquals("case when Age>80 then 'A' else 'B' end", "B");
    evalNull("case when Age>80 then 'A' end");

    // Search CASE WHEN
    evalEquals("case when Age=10+20 then 1*5 when Age=20+20 then 2*5 else 50 end", 10L);
    evalEquals("case when VALUE_NULL=Age then null when Age=20+20 then 2*5 else 50 end", 10L);
    
    // Simple CASE
    evalEquals("case Age when 10 then 10 when 40 then 40 else 50 end", 40L);
    evalEquals("case Age when 10 then 10 when 20 then 20 else -1 end", -1L);
    evalNull("case Age when 10 then 10 when 20 then 20 end");

    // Missing 'END'
    evalFails("case when Age=40 then 10 else 50");

    // Implicit ELSE NULL
    writeEquals("CASE WHEN AGE=40 THEN 10 END", "CASE WHEN AGE=40 THEN 10 ELSE NULL END");

    writeEquals("CASE WHEN AGE=40 THEN TRUE ELSE FALSE END");
    writeEquals("CASE AGE WHEN 40 THEN 'A' WHEN 20 THEN 'B' ELSE 'C' END");
  }
}


