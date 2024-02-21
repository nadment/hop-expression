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

import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.Interval;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Types;
import org.junit.Test;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;

public class OperatorTest extends ExpressionTest {

  @Test
  public void ElementAt() throws Exception {
    evalEquals("ARRAY[1,3,5][1]",1L);
    evalEquals("ARRAY[1,3.5,5][3]",5L);
    evalEquals("ARRAY['A','B',FIELD_STRING][3]","TEST");
    
    // Negative index
    evalEquals("ARRAY[1,3,5][-1]",5L);
    evalEquals("ARRAY[1,3,5][-3]",1L);   
    
    evalFails("ARRAY[1,3,5][0]");
    evalFails("ARRAY[1,3,5][9]");
  }
  
  @Test
  public void EqualTo() throws Exception {
    // Integer
    evalTrue("0.0 = 0");
    evalTrue("0.0 = -0.000");
    evalTrue("15.0 = 15");
    evalTrue("'.01' = 0.01");
    evalTrue("FIELD_INTEGER = 40.0").returnType(Types.BOOLEAN);
    evalTrue("0b11110000 = 0xF0");

    // Number
    evalTrue("FIELD_NUMBER = -5.12").returnType(Types.BOOLEAN);
    evalTrue("2.000 = 2");
    evalTrue("2.000 = 2.00");
    evalTrue("-1.4e-10 = -1.4e-10");

    // Binary
    evalTrue("BINARY 'FF22C' = BINARY 'ff22c'");

    // Boolean
    evalTrue("true = true");
    evalTrue("false = false");
    evalFalse("true = false");
    evalFalse("false = true");
    evalTrue("FIELD_BOOLEAN_TRUE = true").returnType(Types.BOOLEAN);

    // String
    evalTrue("'ABC' = 'ABC'");
    evalFalse("'ABC' = 'abc'");
    evalTrue("FIELD_STRING = 'TEST'");

    // Date
    evalTrue("DATE '2019-01-01' = DATE '2019-01-01'");
    evalFalse("DATE '2019-01-01' = DATE '2018-01-01'");

    // Timestamp
    evalTrue("Timestamp '2019-01-01 08:00:00 -08:00' = Timestamp '2019-01-01 11:00:00 -05:00'");
    evalTrue("Timestamp '2019-01-01 8:00:00 -08:00' = Timestamp '2019-01-01 11:00:00 -05:00'");
    evalFalse("Timestamp '2019-01-01 08:00:00 -08:00' = Timestamp '2019-01-01 8:00:00 -05:00'");
    evalTrue(
        "Timestamp '2019-01-01 8:00:00' AT TIME ZONE 'America/New_York' = Timestamp '2019-01-01 14:00:00' AT TIME ZONE 'Europe/Berlin'");

    // Interval
    evalTrue("INTERVAL 1 YEARS = INTERVAL 12 MONTHS");
    evalFalse("INTERVAL 3 YEARS = INTERVAL 3 MONTHS");

    // NULL is not equal ( = ) to anything not even to another NULL.
    evalNull("1 = NULL_INTEGER").returnType(Types.BOOLEAN);
    evalNull("1 = NULL_NUMBER").returnType(Types.BOOLEAN);
    evalNull("NULL_BOOLEAN = true").returnType(Types.BOOLEAN);
    evalNull("NULL_BOOLEAN = false").returnType(Types.BOOLEAN);
    evalNull("NULL_BOOLEAN = NULL_BOOLEAN").returnType(Types.BOOLEAN);
    evalNull("NULL_STRING = NULL_STRING").returnType(Types.BOOLEAN);
    evalNull("NULL_STRING = FIELD_STRING").returnType(Types.BOOLEAN);
    evalNull("FIELD_STRING = NULL_STRING").returnType(Types.BOOLEAN);
    evalNull("NULL_INTEGER = NULL_INTEGER").returnType(Types.BOOLEAN);
    evalNull("NULL_INTEGER = 1").returnType(Types.BOOLEAN);
    evalNull("FIELD_INTEGER=NULL_INTEGER").returnType(Types.BOOLEAN);
    
    // Compare numeric with implicit coercion from BOOLEAN
    evalTrue("TRUE=1");
    evalTrue("1=TRUE");
    evalFalse("TRUE=1.1");
    evalFalse("TRUE=0");

    // Compare numeric with implicit coercion from STRING
    evalTrue("2 = '2'");
    evalTrue("'2'=2");
    evalFalse("2 = '2.1'");
    evalFalse("'2.1' = 2");

    // Compare date with implicit coercion from STRING
    evalFalse("Date '2023-12-01' = '2023-10-31'");
    evalTrue("Date '2023-12-01' = '2023-12-01'");
    evalFalse("Date '2023-10-01' = '2023-10-31'");

    // Comparable unordered type
    evalNull("NULL_JSON = FIELD_JSON");
    evalFalse("FIELD_JSON = FIELD_STRING_JSON::JSON");

    // Syntax error
    evalFails("FIELD_INTEGER=");
    evalFails(" = FIELD_INTEGER ");
    
    optimizeTrue("'a' = 'a'");
    optimizeFalse("'a' = 'b'");   
    optimizeFalse("10151082135029368 = 10151082135029369");
    optimize("FIELD_INTEGER=40", "40=FIELD_INTEGER");
    
    // Simplify arithmetic comparisons
    optimize("FIELD_INTEGER+1=3", "2=FIELD_INTEGER");

    // Simplify comparison with same term if not nullable
    optimize("FIELD_STRING=FIELD_STRING", "FIELD_STRING=FIELD_STRING");
    optimize("PI()=PI()", "TRUE");
  }

  @Test
  public void NotEqualTo() throws Exception {
    evalTrue("FIELD_STRING != 'foo'").returnType(Types.BOOLEAN);
    evalTrue("FIELD_STRING <> 'tEST'");
    evalFalse("FIELD_INTEGER != 40");
    evalFalse("FIELD_INTEGER <> 40");

    evalTrue("1 <> 2");
    evalTrue("10 <> 0x10");
    evalFalse("1 <> '1'");

    evalTrue("true <> false");
    evalTrue("false <> true");
    evalFalse("true <> true");
    evalFalse("false <> false");

    evalFalse("2 <> 2.000");
    evalFalse("2.000 <> 2.00");
    evalFalse("true <> true");
    evalTrue("DATE '2019-01-01' <> DATE '2018-01-01'");
    evalFalse("DATE '2019-01-01' <> DATE '2019-01-01'");

    evalTrue(
        "Timestamp '2019-01-01 8:00:00' AT TIME ZONE 'UTC' <> Timestamp '2019-01-01 8:00:00' AT TIME ZONE 'US/Pacific'");
    evalFalse("Timestamp '2019-01-01 08:00:00 -8:00' <> Timestamp '2019-01-01 11:00:00 -5:00'");

    // Interval
    evalFalse("INTERVAL 1 YEARS <> INTERVAL 12 MONTHS");
    evalTrue("INTERVAL 3 YEARS <> INTERVAL 3 MONTHS");

    // NULL is not equal ( = ) to anything not even to another NULL.
    evalNull("NULL_STRING <> 'bar'");
    evalNull("'bar' <> NULL_STRING");
    evalNull("NULL_STRING <> NULL_STRING");

    // Compare numeric with implicit coercion from BOOLEAN
    evalFalse("TRUE<>1");
    evalFalse("1<>TRUE");
    evalTrue("TRUE<>1.1");
    evalTrue("TRUE<>0");
    evalFalse("TRUE<>1");

    // Compare numeric with implicit coercion from STRING
    evalTrue("2 = '2'");
    evalTrue("'2'=2");
    evalFalse("2 = '2.1'");
    evalFalse("'2.1' = 2");

    // Compare date with implicit coercion from STRING
    evalTrue("Date '2023-12-01' <> '2023-10-31'");
    evalFalse("Date '2023-12-01' <> '2023-12-01'");
    evalTrue("Date '2023-10-01' <> '2023-10-31'");

    // Comparable unordered type
    evalTrue("FIELD_JSON <> FIELD_STRING_JSON::JSON");

    // Syntax error
    evalFails("FIELD_INTEGER<>");
    evalFails("FIELD_INTEGER <> ");
    evalFails("FIELD_INTEGER!");
    evalFails("FIELD_INTEGER ! ");
    evalFails("<>FIELD_INTEGER");

    optimize("FIELD_BOOLEAN_TRUE<>TRUE", "FIELD_BOOLEAN_TRUE IS NOT TRUE");
    optimize("TRUE<>FIELD_BOOLEAN_TRUE", "FIELD_BOOLEAN_TRUE IS NOT TRUE");
    optimize("FIELD_BOOLEAN_TRUE<>FALSE", "FIELD_BOOLEAN_TRUE IS NOT FALSE");
    optimize("FALSE<>FIELD_BOOLEAN_TRUE", "FIELD_BOOLEAN_TRUE IS NOT FALSE");
    optimize("10!=FIELD_INTEGER");
    optimize("FIELD_STRING!=FIELD_STRING", "NULL AND FIELD_STRING IS NULL");
    
    // Simplify arithmetic comparisons
    optimize("FIELD_INTEGER+1!=3", "2!=FIELD_INTEGER");
  }

  @Test
  public void GreaterThan() throws Exception {
    evalTrue("9>5").returnType(Types.BOOLEAN);
    evalTrue("9.4>9.358");
    evalTrue("(4+2)>10-9");
    evalTrue("FIELD_INTEGER>10.3");
    evalFalse("5>5");
    evalTrue("5.1>5");
    evalFalse("BINARY 'F5'>BINARY 'FE'");

    evalFalse("false > true");
    evalFalse("false > false");
    evalFalse("true > true");
    evalTrue("true > false");

    evalFalse("'bar' > 'foo'");
    evalFalse("'foo' > 'foo'");
    evalTrue("'foo' > 'bar'");
    evalTrue("'foo2' > 'foo12'");

    evalTrue("DATE '2019-02-01' > DATE '2019-01-01'");
    evalFalse("DATE '2019-01-01' > DATE '2019-01-01'");
    evalFalse("DATE '2018-01-01' > DATE '2019-01-01'");

    evalFalse("INTERVAL 3 DAYS > INTERVAL 3 MONTHS");
    evalTrue("INTERVAL 3 YEARS > INTERVAL 3 MONTHS");

    evalFalse("BINARY '4100' > BINARY '4100'");
    evalFalse("BINARY '4100' > BINARY '42'").returnType(Types.BOOLEAN);
    evalTrue("BINARY '410000' > BINARY '4100'");
    evalFalse("BINARY '4100' > BINARY '410000'");

    evalNull("NULL_BOOLEAN > 0");
    evalNull("NULL_INTEGER > 0");
    evalNull("NULL_NUMBER > NULL_INTEGER");
    evalNull("1 > NULL_BOOLEAN");
    evalNull("FIELD_STRING > NULL_STRING");
    evalNull("NULL_STRING > FIELD_STRING");
    
    // Compare numeric with implicit coercion from BOOLEAN
    evalFalse("TRUE>1");
    evalFalse("1>TRUE");
    evalFalse("TRUE>1.1");
    evalTrue("TRUE>0.5");

    // Compare numeric with implicit coercion from STRING
    evalTrue("2 > '1'");
    evalTrue("2.1 > '2'");
    evalFalse("1 > '2'");
    evalFalse("2 > '2.1'");
    evalFalse("2.1 > '2.1'");

    // Compare date with implicit coercion from STRING
    evalTrue("Date '2023-12-01' > '2023-10-31'");
    evalFalse("Date '2023-10-01' > '2023-10-31'");

    // Compare unordered type
    evalFails("FIELD_JSON > FIELD_STRING");

    // Syntax error
    evalFails("> FIELD_INTEGER");
    evalFails("FIELD_INTEGER >");
    evalFails("FIELD_INTEGER > ");
    evalFails("FIELD_STRING>5");

    optimize("10>FIELD_INTEGER");
    optimizeTrue("25>12");

    // Simplify arithmetic comparisons
    optimize("3>FIELD_INTEGER+1", "2>FIELD_INTEGER");
    optimize("FIELD_INTEGER+1>3", "2<FIELD_INTEGER");

    // Simplify comparison with same term
    optimize("FIELD_STRING>FIELD_STRING", "NULL AND FIELD_STRING IS NULL");
  }

  @Test
  public void GreaterThanOrEqualTo() throws Exception {
    evalTrue("9 >= 5").returnType(Types.BOOLEAN);
    evalTrue("9.4 >= 9.358");
    evalTrue("(4+2) >= 10-9");
    evalTrue("FIELD_INTEGER >= 10");
    evalTrue("5 >= 5");

    evalFalse("false >= true");
    evalTrue("false >= false");
    evalTrue("true >= true");
    evalTrue("true >= false");

    evalFalse("'bar' >= 'foo'");
    evalTrue("'foo' >= 'foo'");
    evalTrue("'foo' >= 'bar'");

    evalTrue("DATE '2019-02-01' >= DATE '2019-01-01'");
    evalTrue("DATE '2019-01-01' >= DATE '2019-01-01'");
    evalFalse("DATE '2018-01-01' >= DATE '2019-01-01'");

    evalFalse("INTERVAL 3 DAYS >= INTERVAL 3 MONTHS");
    evalTrue("INTERVAL 3 YEARS >= INTERVAL 3 MONTHS");

    evalTrue("BINARY '4100' >= BINARY '4100'");
    evalFalse("BINARY '4100' >= BINARY '42'").returnType(Types.BOOLEAN);
    evalTrue("BINARY '410000' >= BINARY '4100'");
    evalFalse("BINARY '4100' >= BINARY '410000'");

    evalNull("NULL_BOOLEAN >= 0");
    evalNull("1 >= NULL_BOOLEAN");
    evalNull("NULL_BOOLEAN >= NULL_INTEGER");
    evalNull("FIELD_STRING >= NULL_STRING");
    evalNull("NULL_STRING >= FIELD_STRING");
    
    // Compare numeric with implicit coercion from BOOLEAN
    evalTrue("TRUE>=1");
    evalTrue("1>=TRUE");
    evalFalse("TRUE>=1.1");
    evalTrue("TRUE>=0.5");

    // Compare numeric with implicit coercion from STRING
    evalTrue("2 >= '1'");
    evalFalse("2 >= '2.1'");
    evalTrue("2 >= '2'");
    evalFalse("1 >= '2'");
    evalTrue("1 >= '1'");

    // Compare DATE with implicit coercion from STRING
    evalTrue("Date '2023-12-01' >= '2023-10-31'");
    evalTrue("Date '2023-12-01' >= '2023-12-01'");
    evalFalse("Date '2023-10-01' >= '2023-10-31'");

    // Compare unordered type
    evalFails("FIELD_JSON >= FIELD_STRING");

    // Syntax error
    evalFails(">=FIELD_INTEGER");
    evalFails("FIELD_INTEGER >=");
    evalFails("FIELD_INTEGER >= ");
    evalFails("FIELD_STRING>=5");

    optimize("FIELD_INTEGER>=80", "80<=FIELD_INTEGER");
    optimizeTrue("25>=12");
    optimize("FIELD_BOOLEAN_TRUE >= TRUE", "TRUE<=FIELD_BOOLEAN_TRUE");
    optimize("TRUE >= FIELD_BOOLEAN_TRUE", "FIELD_BOOLEAN_TRUE IS NOT NULL");
    optimize("FIELD_BOOLEAN_TRUE >= FALSE", "FIELD_BOOLEAN_TRUE IS NOT NULL");
    optimize("FALSE >= FIELD_BOOLEAN_TRUE", "FALSE>=FIELD_BOOLEAN_TRUE");
    
    // Simplify arithmetic comparisons
    optimize("FIELD_INTEGER+1>=3", "2<=FIELD_INTEGER");
    optimize("3>=FIELD_INTEGER+1", "2>=FIELD_INTEGER");

    // Simplify comparison with same term
    optimize("FIELD_STRING>=FIELD_STRING", "FIELD_STRING>=FIELD_STRING");
    optimize("PI()>=PI()", "TRUE");
  }

  @Test
  public void LessThan() throws Exception {
    evalTrue("5 < 9").returnType(Types.BOOLEAN);
    evalTrue("9.358 < 9.4");
    evalTrue("10-9 < (4+2)");
    evalTrue("FIELD_INTEGER < 100");
    evalFalse("5 < 5");

    evalFalse("true < false").returnType(Types.BOOLEAN);
    evalTrue("false < true");
    evalFalse("false < false");
    evalFalse("true < true");

    evalTrue("'bar' < 'foo'").returnType(Types.BOOLEAN);
    evalFalse("'foo' < 'foo'");
    evalFalse("'foo' < 'bar'");

    evalTrue("BINARY '4100' < BINARY '42'").returnType(Types.BOOLEAN);
    evalTrue("BINARY '4100' < BINARY '410000'");
    evalFalse("BINARY '4100' < BINARY '4100'");
    evalFalse("BINARY '410000' < BINARY '4100'");

    evalTrue("DATE '2019-01-01' < DATE '2019-02-01'");
    evalFalse("DATE '2019-01-01' < DATE '2019-01-01'");
    evalFalse("DATE '2019-01-01' < DATE '2018-01-01'");

    evalTrue("INTERVAL 3 DAYS < INTERVAL 3 MONTHS");
    evalFalse("INTERVAL 3 YEARS < INTERVAL 3 MONTHS");

    evalNull("NULL_INTEGER < 1").returnType(Types.BOOLEAN);
    evalNull("NULL_NUMBER < NULL_INTEGER");
    evalNull("NULL_STRING < Upper(FIELD_STRING)");
    evalNull("FIELD_STRING < NULL_STRING");
    evalNull("NULL_STRING < FIELD_STRING");
    
    // Compare numeric with implicit coercion from BOOLEAN
    evalFalse("TRUE<1");
    evalFalse("1<TRUE");
    evalTrue("TRUE<1.1");
    evalFalse("TRUE<0.5");

    // Compare numeric with implicit coercion from STRING
    evalFalse("2 < '1'");
    evalFalse("2.1 < '2'");
    evalTrue("1 < '2'");
    evalTrue("2 < '2.1'");
    evalFalse("2.1 < '2.1'");

    // Compare date with implicit coercion from STRING
    evalFalse("Date '2023-12-01' < '2023-10-31'");
    evalFalse("Date '2023-12-01' < '2023-12-01'");
    evalTrue("Date '2023-10-01' < '2023-10-31'");

    // Compare unordered type
    evalFails("FIELD_JSON < FIELD_STRING");

    // Syntax error
    evalFails("< FIELD_INTEGER");
    evalFails("FIELD_INTEGER <");
    evalFails("FIELD_INTEGER < ");
    evalFails("FIELD_STRING < 5");

    optimize("FIELD_INTEGER<80", "80>FIELD_INTEGER");
    optimizeFalse("25<12");
    
    // Simplify arithmetic comparisons
    optimize("FIELD_INTEGER+1<3", "2>FIELD_INTEGER");
    optimize("3>FIELD_INTEGER+1", "2>FIELD_INTEGER");

    // Simplify comparison with same term
    optimize("FIELD_STRING<FIELD_STRING", "NULL AND FIELD_STRING IS NULL");
  }

  @Test
  public void LessThanOrEqualTo() throws Exception {
    evalTrue("5 <= 9");
    evalTrue("9.358 <= 9.4");
    evalTrue("10-9 <= (4+2)");
    evalTrue("FIELD_INTEGER <= 100");
    evalTrue("5 <= 5");

    evalTrue("false <= false");
    evalTrue("true <= true");
    evalTrue("false <= true");
    evalFalse("true <= false");

    evalTrue("'foo' <= 'foo'").returnType(Types.BOOLEAN);
    evalTrue("'bar' <= 'foo'");
    evalFalse("'foo' <= 'bar'");

    evalTrue("DATE '2019-01-01' <= DATE '2019-02-01'");
    evalTrue("DATE '2019-01-01' <= DATE '2019-01-01'");
    evalFalse("DATE '2019-01-01' <= DATE '2018-01-01'");

    evalTrue("INTERVAL 3 DAYS <= INTERVAL 3 MONTHS");
    evalFalse("INTERVAL 3 YEARS <= INTERVAL 3 MONTHS");

    evalTrue("BINARY '4100' <= BINARY '4100'");
    evalTrue("BINARY '4100' <= BINARY '42'").returnType(Types.BOOLEAN);
    evalTrue("BINARY '4100' <= BINARY '410000'");
    evalFalse("BINARY '410000' <= BINARY '4100'");

    evalNull("NULL_INTEGER <= FIELD_INTEGER");
    evalNull("FIELD_INTEGER <= NULL_INTEGER");
    evalNull("NULL_STRING <= Upper(FIELD_STRING)");
    evalNull("FIELD_STRING <= NULL_STRING");
    
    // Compare numeric with implicit coercion from BOOLEAN
    evalTrue("TRUE<=1");
    evalTrue("1<=TRUE");
    evalTrue("TRUE<=1.1");
    evalFalse("TRUE<=0.5");

    // Compare numeric with implicit coercion from STRING
    evalFalse("2 <= '1'");
    evalFalse("2.1 <= '2'");
    evalTrue("1 <= '2'");
    evalTrue("2 <= '2.1'");
    evalTrue("2.1 <= '2.1'");

    // Compare date with implicit coercion from STRING
    evalFalse("Date '2023-12-01' <= '2023-10-31'");
    evalTrue("Date '2023-12-01' <= '2023-12-01'");
    evalTrue("Date '2023-10-01' <= '2023-10-31'");

    // Compare unordered type
    evalFails("FIELD_JSON <= FIELD_STRING");

    // Syntax error
    evalFails("<= FIELD_INTEGER");
    evalFails("FIELD_INTEGER <=");
    evalFails("FIELD_INTEGER <= ");
    evalFails("FIELD_STRING <=5");

    optimize("FIELD_INTEGER<=5", "5>=FIELD_INTEGER");
    optimizeFalse("25<=12");
    optimize("FIELD_BOOLEAN_TRUE <= TRUE", "FIELD_BOOLEAN_TRUE IS NOT NULL");
    optimize("TRUE <= FIELD_BOOLEAN_TRUE", "TRUE<=FIELD_BOOLEAN_TRUE");
    optimize("FIELD_BOOLEAN_TRUE <= FALSE", "FALSE>=FIELD_BOOLEAN_TRUE");
    optimize("FALSE <= FIELD_BOOLEAN_TRUE", "FIELD_BOOLEAN_TRUE IS NOT NULL");
    optimize("FALSE <= FIELD_BOOLEAN_FALSE", "FIELD_BOOLEAN_FALSE IS NOT NULL");
    
    // Simplify arithmetic comparisons
    optimize("3<=FIELD_INTEGER+1", "2<=FIELD_INTEGER");
    optimize("FIELD_INTEGER+1>=3", "2<=FIELD_INTEGER");

    // Simplify comparison with same term
    optimize("FIELD_STRING<=FIELD_STRING", "NVL2(FIELD_STRING,TRUE,NULL)");
  }

  @Test
  public void In() throws Exception {
    evalTrue("FIELD_STRING in ('?','*','TEST')").returnType(Types.BOOLEAN);
    evalTrue("FIELD_STRING not in ('?','-','!')").returnType(Types.BOOLEAN);
    evalTrue("FIELD_INTEGER not in (1,2,3)").returnType(Types.BOOLEAN);

    evalTrue("2.5 IN (1,2.5,3)");
    evalTrue("2 in (NULL_INTEGER,1,2,FIELD_INTEGER)");
    evalTrue("TRUE IN (FALSE,NULL_BOOLEAN,TRUE)");
    evalTrue("DATE '2019-01-01' in (DATE '2019-04-01',DATE '2019-01-01',DATE '2019-03-06')");
    evalTrue(
        "DATE '2019-01-01' in (TIMESTAMP '2019-04-01 00:00:00',DATE '2019-01-01',DATE '2019-03-06')");
    evalTrue("BINARY '0123456789' in (BINARY '9876',BINARY '0123456789',BINARY '3698')");
    evalFalse("2 in (1,2.5,3)");


    evalFalse("2 in (NULL_INTEGER,NULL_NUMBER)");
    evalNull("1 not in (NULL_INTEGER,2)");
    evalTrue("1 not in (2,3)");
    evalFalse("FIELD_INTEGER not in (40,2,3)");

    // c1 IN (c2, c3, NULL) is syntactically equivalent to (c1=c2 or c1=c3 or c1=NULL)
    // As a result, when the value of c1 is NULL, the expression c1 IN (c2, c3, NULL) always
    // evaluates to FALSE.
    evalFalse("FIELD_STRING in ('A','B',NULL_STRING)").returnType(Types.BOOLEAN);


    // c1 NOT IN (c2, c3, NULL) evaluates to NULL
    // It is syntactically equivalent to (c1<>c2 AND c1<>c3 AND c1<>NULL)
    evalNull("FIELD_STRING not in ('A','B',NULL_STRING)").returnType(Types.BOOLEAN);

    evalNull("NULL_INTEGER in (1,2,3)");
    evalNull("NULL_INTEGER in (NULL_NUMBER,2,3,NULL_INTEGER)");
    evalNull("NULL_INTEGER in (NULL_NUMBER,2,3,NULL_INTEGER)");

    evalFails(" in (1,2)");
    evalFails("FIELD_INTEGER in 1,2)");
    evalFails("FIELD_INTEGER in (1,2.5,)");
    evalFails("FIELD_INTEGER in ()");
    evalFails("FIELD_INTEGER in ()    ");
    evalFails("FIELD_INTEGER in 40");
    evalFails("FIELD_INTEGER in (,2,3)");
    evalFails("FIELD_INTEGER in (1,2,3");
    evalFails("FIELD_INTEGER in (1,,3)");
    evalFails("FIELD_INTEGER in (1,2,)");
    evalFails("FIELD_INTEGER in (1,2,NULL)");
    evalFails("NULL in (1,2,3)");

    optimize("FIELD_INTEGER IN (10,20,30,40)");
    optimize("FIELD_INTEGER NOT IN (10,20,30,40)");
    optimizeTrue("'foo' IN ('bar', 'baz', 'foo', 'blah')");
    optimizeTrue("25 in (1,25,66)");
    optimizeTrue("1.15 IN (1.1, 1.2, 1.3, 1.15)");

    optimize("FIELD_INTEGER in (1,2,1,2,3,4.1)", "FIELD_INTEGER IN (1,2,3,4.1)");
    optimize("FIELD_STRING in ('1','2','1')", "FIELD_STRING IN ('1','2')");


    // optimize("2 in (1,2,3/0)", "2 in (1,2,3/0)");

    // Normalize IN list with single element to comparison
    optimize("FIELD_INTEGER in (1)", "1=FIELD_INTEGER");
    optimize("FIELD_STRING in ('AB','AB')", "'AB'=FIELD_STRING");

    optimize("0 / 0 in (2, 2)", "2=0/0");

    // Value in the expression list
    // optimize("FIELD_INTEGER in (1,2,FIELD_INTEGER)", "NULL OR FIELD_INTEGER IS NOT NULL");
    // optimize("FIELD_STRING in ('XX',FIELD_STRING,'ZZ')", "NULL OR FIELD_STRING IS NOT NULL");
  }

  @Test
  public void IsTrue() throws Exception {
    evalTrue("True IS True").returnType(Types.BOOLEAN);
    evalTrue("True IS NOT False").returnType(Types.BOOLEAN);
    evalTrue("FIELD_BOOLEAN_TRUE is True").returnType(Types.BOOLEAN);
    evalFalse("NULL_BOOLEAN IS True");
    evalFalse("FIELD_STRING='XX' IS TRUE").returnType(Types.BOOLEAN);
    evalTrue("FIELD_STRING='XX' IS NOT TRUE");
    evalFalse("NULL_BOOLEAN is True");
    evalTrue("NULL_BOOLEAN IS NOT True");

    evalFails("NOM IS ");
    evalFails("IS TRUE");
    evalFails("IS NOT TRUE");

    optimize("FIELD_BOOLEAN_TRUE IS TRUE");
    optimize("FIELD_BOOLEAN_TRUE IS NOT TRUE");
    optimizeTrue("true is true");
    optimizeFalse("true is not true");
    optimizeTrue("false is not true");
    optimizeFalse("false is true");
  }

  @Test
  public void IsFalse() throws Exception {
    evalFalse("FIELD_BOOLEAN_TRUE IS FALSE").returnType(Types.BOOLEAN);
    evalTrue("FIELD_BOOLEAN_TRUE IS NOT FALSE").returnType(Types.BOOLEAN);
    evalTrue("FIELD_STRING='XX' IS FALSE");
    evalFalse("FIELD_STRING='XX' IS NOT FALSE");

    evalFalse("NULL_BOOLEAN IS False");
    evalTrue("NULL_BOOLEAN IS NOT False");

    evalFails("IS FALSE");
    evalFails("IS NOT FALSE");

    optimize("FIELD_BOOLEAN_TRUE IS FALSE");
    optimize("FIELD_BOOLEAN_TRUE IS NOT FALSE");
    optimizeTrue("false is false");
    optimizeFalse("true is false");
    optimizeTrue("true is not false");
  }

  @Test
  public void IsNull() throws Exception {
    evalFalse("True IS Null");
    evalFalse("False IS Null");
    evalFalse("NULL_BOOLEAN IS NOT NULL");
    evalTrue("NULL_INTEGER IS NULL");
    evalTrue("NULL_STRING IS NULL");
    evalTrue("NULL_BOOLEAN IS NULL");
    evalFalse("FIELD_BOOLEAN_TRUE IS NULL").returnType(Types.BOOLEAN);
    evalTrue("FIELD_BOOLEAN_TRUE IS NOT NULL").returnType(Types.BOOLEAN);

    evalFails("IS NULL");
    evalFails("IS NOT NULL");

    optimize("FIELD_BOOLEAN_TRUE IS NULL");
    optimize("FIELD_BOOLEAN_TRUE IS NOT NULL");
    optimizeFalse("true is null");
    optimizeFalse("false is null");
    optimizeTrue("NULL IS NULL");
    optimizeTrue("NULL+1 IS NULL");
    optimizeFalse("NULL+1 IS NOT NULL");
    optimizeTrue("TRUE IS NOT NULL");
    optimizeFalse("TRUE IS NULL");
    optimizeFalse("1 IS NULL");
    optimizeTrue("1 IS NOT NULL");
    optimizeFalse("Random() IS NULL");
    optimizeTrue("Random() IS NOT NULL");

    // If operator return type is not nullable
    optimizeTrue("UUID() IS NOT NULL");

    // CAST(x AS type) IS NOT NULL → x IS NOT NULL
    optimize("CAST(FIELD_NUMBER AS STRING) IS NOT NULL", "FIELD_NUMBER IS NOT NULL");
  }

  @Test
  public void IsDistinctFrom() throws Exception {
    evalTrue("1 IS DISTINCT FROM 2");
    evalFalse("1 IS DISTINCT FROM 1");
    evalTrue("FIELD_INTEGER IS DISTINCT FROM 1").returnType(Types.BOOLEAN);
    evalTrue("1 IS NOT DISTINCT FROM 1");

    evalFalse("NULL_BOOLEAN IS NOT DISTINCT FROM true");
    evalTrue("NULL_BOOLEAN IS NOT DISTINCT FROM NULL_BOOLEAN");
    evalFalse("NULL_INTEGER IS DISTINCT FROM NULL");
    evalTrue("NULL_INTEGER IS NOT DISTINCT FROM NULL");

    evalFalse("DATE '2019-01-01' IS DISTINCT FROM DATE '2019-01-01'");
    evalTrue("DATE '2019-01-01' IS NOT DISTINCT FROM DATE '2019-01-01'");

    evalTrue("DATE '2019-01-01' IS DISTINCT FROM DATE '2018-01-01'");
    evalFalse("DATE '2019-01-01' IS NOT DISTINCT FROM DATE '2018-01-01'");

    evalFails("FIELD_STRING IS NOT DISTINCT FROM ");
    evalFails("FIELD_STRING IS DISTINCT 'TEST' ");

    optimize("FIELD_BOOLEAN_TRUE IS DISTINCT FROM TRUE");
    optimize("FIELD_BOOLEAN_TRUE IS NOT DISTINCT FROM TRUE");
    optimizeTrue("NULL IS NOT DISTINCT FROM NULL");
    optimizeFalse("NULL IS DISTINCT FROM NULL");
    optimizeFalse("NULL IS NOT DISTINCT FROM TRUE");
    optimizeTrue("NULL IS DISTINCT FROM TRUE");
    optimizeTrue("NULL IS DISTINCT FROM 3");
    optimizeTrue("10151082135029368  IS DISTINCT FROM 10151082135029369");
    optimizeTrue("FIELD_INTEGER IS NOT DISTINCT FROM FIELD_INTEGER");
    optimizeFalse("FIELD_INTEGER IS DISTINCT FROM FIELD_INTEGER");


    // The DISTINCT predicate is a verbose way of NULL safe comparisons
    optimize("NULL_INTEGER IS DISTINCT FROM NULL", "NULL_INTEGER IS NOT NULL");
    optimize("NULL_INTEGER IS NOT DISTINCT FROM NULL", "NULL_INTEGER IS NULL");
    optimize("FIELD_STRING IS DISTINCT FROM NULL", "FIELD_STRING IS NOT NULL");
    optimize("NULL IS DISTINCT FROM FIELD_STRING", "FIELD_STRING IS NOT NULL");
    optimize("FIELD_STRING IS NOT DISTINCT FROM NULL", "FIELD_STRING IS NULL");
    optimize("NULL IS NOT DISTINCT FROM FIELD_STRING", "FIELD_STRING IS NULL");
  }

  @Test
  public void SimilarTo() throws Exception {
    evalTrue("'abc' SIMILAR TO 'abc'");
    evalTrue("'abc' SIMILAR TO '_b_'");
    evalTrue("'aaa' SIMILAR TO 'a{2,4}'");
    evalFalse("'aa' SIMILAR TO 'a{3,4}'");
    evalFalse("'aaaaa' SIMILAR TO 'a{2,4}'");
    evalFalse("'abc' SIMILAR TO '_B_'");
    evalFalse("'abc' SIMILAR TO '_a_'");

    // evalTrue("'Erdbeere' SIMILAR TO 'Erd[a[:SPACE:]b]eere'");
    evalTrue("'12345TEST' SIMILAR TO '123[:ALNUM:]*'");

    evalFalse("'abc' SIMILAR TO 'a'");
    evalTrue("'abc' SIMILAR TO '.*(b|d).*'");
    evalFalse("'abc' SIMILAR TO '(b|c).*'");
    evalFalse("'abc' NOT SIMILAR TO 'abc'");
    evalTrue("'xyz' SIMILAR TO '%(y|a)%'");

    // An empty pattern '' matches nothing
    evalFalse("'' SIMILAR TO  ''");
    evalFalse("'ABC' SIMILAR TO ''");

    evalNull("NULL_STRING SIMILAR TO 'A'");
    evalNull("'A' SIMILAR TO NULL_STRING");

    evalFails("FIELD_STRING IS ");
    evalFails("FIELD_STRING IS SIMILAR");
    evalFails("FIELD_STRING IS SIMILAR 'A'");
    evalFails("FIELD_STRING IS SIMILAR TO ");
    evalFails("FIELD_STRING IS SIMILAR AND TO ");

    optimize("FIELD_STRING SIMILAR TO 'abc'");
    optimize("FIELD_STRING NOT SIMILAR TO 'abc'");
  }

  @Test
  public void AddNumeric() throws Exception {
    // Addition of numeric
    evalEquals("10+(-0.5)", 9.5).returnType(NumberType.of(4, 1));
    evalEquals("BINARY 'F'::INTEGER+1", 16L);
    evalEquals("0b00011::INTEGER+0", 3L);
    evalEquals("-24.7+0.5+24.7+0.5E-2", 0.505);
    evalEquals("FIELD_INTEGER+FIELD_NUMBER+FIELD_BIGNUMBER", 123491.669).returnType(Types.NUMBER);
    evalEquals("FIELD_BIGNUMBER+FIELD_NUMBER+FIELD_INTEGER", 123491.669);
    evalEquals("FIELD_BIGNUMBER+1", 123456.789 + 1).returnType(Types.NUMBER);
    evalEquals("1::NUMBER(38,10)+3::NUMBER(38,5)", 4L).returnType(NumberType.of(38, 10));
    evalEquals("1::NUMBER(14,2)+3::NUMBER(14,2)", 4L).returnType(NumberType.of(15, 2));

    // Implicit coercion from BOOLEAN
    evalEquals("1+FIELD_BOOLEAN_FALSE", 1L);
    evalEquals("FIELD_BOOLEAN_TRUE+1", 2L);

    // Implicit coercion from STRING
    evalEquals("'1'+2", 3L).returnType(Types.NUMBER);
    evalEquals("1+'2'", 3L).returnType(Types.NUMBER);
    evalEquals("1.3+'2.5'", 3.8);
    evalEquals("1+'-2.5'", -1.5);

    // Addition of NULL is always null
    evalNull("5+NULL_INTEGER+5");
    evalNull("+NULL_INTEGER+5");
    evalNull("NULL_INTEGER+NULL_NUMBER");

    evalFails("5+");

    optimize("10+FIELD_INTEGER");
    optimize("3+1+1+1+1+1+1+1+1+1+1+1", "14");
    optimize("3+1+2", "6");
    optimize("3+1*2", "5");
    optimize("(3+1)*2", "8");
    optimize("FIELD_INTEGER+0", "FIELD_INTEGER");
    optimize("4+FIELD_INTEGER+1", "5+FIELD_INTEGER");
    optimize("0+FIELD_INTEGER", "FIELD_INTEGER");
    optimize("1+FIELD_INTEGER+3+FIELD_INTEGER+5*2", "14+FIELD_INTEGER+FIELD_INTEGER");
    optimize("FIELD_INTEGER+3+1", "4+FIELD_INTEGER");
    optimize("FIELD_INTEGER+(-FIELD_NUMBER)", "FIELD_INTEGER-FIELD_NUMBER");
    optimize("FIELD_DATE+INTERVAL 0 YEAR", "FIELD_DATE");
  }

  @Test
  public void AddTemporal() throws Exception {

    // Addition of interval to a temporal
    evalEquals("DATE '2019-02-25'+INTERVAL 2 YEAR", LocalDateTime.of(2021, 2, 25, 0, 0, 0));
    evalEquals("INTERVAL 2 YEARS+DATE '2019-02-25'", LocalDateTime.of(2021, 2, 25, 0, 0, 0));
    evalEquals("DATE '2019-02-25'+INTERVAL '2-11' YEAR TO MONTH",
        LocalDateTime.of(2022, 1, 25, 0, 0, 0));
    evalEquals("DATE '2019-02-25'+INTERVAL 1 WEEK", LocalDateTime.of(2019, 3, 4, 0, 0, 0));
    evalEquals("DATE '2019-02-25'+INTERVAL 12 HOUR", LocalDateTime.of(2019, 2, 25, 12, 0, 0));
    evalEquals("DATE '2019-02-25'+INTERVAL -12 HOUR", LocalDateTime.of(2019, 2, 24, 12, 0, 0));
    evalEquals("DATE '2019-02-25'+INTERVAL '10 4' DAY TO HOUR",
        LocalDateTime.of(2019, 3, 7, 4, 0, 0));
    // evalEquals("DATE '2019-02-25'+TO_INTERVAL('10 4:0:0')", LocalDateTime.of(2019, 3, 7, 4, 0,
    // 0));
    evalNull("NULL_DATE+INTERVAL 12 DAYS");
    evalNull("INTERVAL 12 DAYS+NULL_DATE");

    evalFails("FIELD_DATE+TO_INTERVAL('z')");

    // Adjust day to the end of month and leap year.
    evalEquals("DATE '2019-01-31'+INTERVAL 1 MONTH", LocalDate.of(2019, 2, 28));
    evalEquals("DATE '2020-01-31'+INTERVAL 1 MONTH", LocalDate.of(2020, 2, 29));
    evalEquals("DATE '2020-03-31'+INTERVAL 1 MONTH", LocalDate.of(2020, 4, 30));
    evalEquals("DATE '2020-02-29'+INTERVAL 12 MONTHS", LocalDate.of(2021, 2, 28));

    evalEquals("DATE '0010-01-01'+INTERVAL 178956970 YEARS", LocalDate.of(178956980, 1, 1));


    // Add interval to interval
    // optimize("INTERVAL 1 YEAR+INTERVAL 13 MONTHS", "INTERVAL '+2-1 0 00:00:00.000000000'");

    // Addition of days to a temporal
    evalEquals("DATE '2019-02-25'+1", LocalDate.of(2019, 2, 26));
    evalEquals("DATE '2019-02-25'+2", LocalDate.of(2019, 2, 27));
    evalEquals("Timestamp '2019-02-25'+2", LocalDate.of(2019, 2, 27));

    // Only integer, round number
    evalEquals("DATE '2019-02-25'+1.8", LocalDateTime.of(2019, 2, 26, 0, 0, 0));
    evalEquals("DATE '2019-02-25'+5/(60*24)", LocalDateTime.of(2019, 2, 25, 0, 0, 0));
  }

  @Test
  public void Subtract() throws Exception {
    // Subtract of numeric
    evalEquals("10-0.5", 9.5D).returnType(NumberType.of(4, 1));
    evalEquals("FIELD_INTEGER-0.5", 39.5D);
    evalEquals("FIELD_INTEGER-10::INTEGER", 30L);
    evalEquals("FIELD_INTEGER-FIELD_NUMBER-FIELD_BIGNUMBER", -123411.669);
    evalEquals("FIELD_BIGNUMBER-FIELD_NUMBER-FIELD_INTEGER", 123421.909);

    // Subtraction interval to a temporal
    evalEquals("DATE '2019-02-25'-INTERVAL 12 HOUR", LocalDateTime.of(2019, 2, 24, 12, 0, 0));
    evalEquals("DATE '2019-02-25'-INTERVAL 2 WEEKS", LocalDateTime.of(2019, 2, 11, 0, 0, 0));

    // Subtraction of days to a temporal
    evalEquals("DATE '2019-02-25'-1", LocalDate.of(2019, 2, 24));
    evalEquals("DATE '2019-02-25'-28", LocalDate.of(2019, 1, 28));
    evalEquals("Timestamp '2019-02-25'-2", LocalDate.of(2019, 2, 23));
    // evalEquals("DATE '2019-02-25'-0.5", LocalDateTime.of(2019, 2, 24, 12, 0, 0));
    // evalEquals("DATE '2019-02-25'-5/(60*24)", LocalDateTime.of(2019, 2, 24, 23, 55, 0));

    // Adjust day to the end of month and leap year.
    evalEquals("DATE '2019-03-30'-INTERVAL 1 MONTH", LocalDate.of(2019, 2, 28));
    evalEquals("DATE '2020-03-30'-INTERVAL 1 MONTH", LocalDate.of(2020, 2, 29));
    evalEquals("DATE '2020-04-30'-INTERVAL 1 MONTH", LocalDate.of(2020, 3, 30));
    evalEquals("DATE '2020-02-29'-INTERVAL 12 MONTHS", LocalDate.of(2019, 2, 28));

    // Implicit coercion from BOOLEAN
    evalEquals("TRUE-FALSE", 1L).returnType(IntegerType.of(2));
    evalEquals("FALSE-TRUE", -1L);
    evalEquals("1-FIELD_BOOLEAN_FALSE", 1L).returnType(IntegerType.of(2));
    evalEquals("FIELD_BOOLEAN_TRUE-2", -1L);

    // Implicit coercion from STRING
    evalEquals("'1'-2", -1L).returnType(Types.NUMBER);
    evalEquals("1-'2'", -1L).returnType(Types.NUMBER);
    evalEquals("2.5-'1.3'", 1.2).returnType(Types.NUMBER);;
    evalEquals("'2.5'-1", 1.5).returnType(Types.NUMBER);;

    // evalEquals("ADD_MONTHS(DATE '2019-04-30',1)", LocalDate.of(2019, 3, 31));

    // TODO: Diff of two date
    // evalEquals("DATE '2019-02-25'-DATE '2019-02-23'", 2);
    // evalEquals("DATE '2019-02-23'-DATE '2019-02-25'", -2);
    // evalEquals("DATE '2019-02-25'-to_Date('2019-02-23 12:00','YYYY-MM-DD HH24:MI')", 1.5);

    evalNull("5-NULL_INTEGER"); // TODO: .returnType(IntegerType.of(19));
    evalNull("NULL_INTEGER-5");

    evalFails("5-");

    optimize("10-FIELD_INTEGER");
    optimize("FIELD_INTEGER-0", "FIELD_INTEGER");
    optimize("0-FIELD_INTEGER", "-FIELD_INTEGER");
    optimize("FIELD_INTEGER-(0-FIELD_INTEGER)", "FIELD_INTEGER+FIELD_INTEGER");
    optimize("FIELD_INTEGER-(-FIELD_NUMBER)", "FIELD_INTEGER+FIELD_NUMBER");
  }

  @Test

  public void Between() throws Exception {
    evalTrue("3 between 1 and 5").returnType(Types.BOOLEAN);
    evalTrue("3 between 3 and 5");
    evalTrue("5 between 3 and 5");
    evalFalse("1 between 3 and 5");
    evalFalse("5 between asymmetric 5 and 3");
    evalTrue("FIELD_INTEGER between symmetric 30 and 50");
    evalTrue("FIELD_INTEGER between symmetric 50 and 30");
    evalTrue("FIELD_INTEGER between -3+27 and 50");
    evalTrue("FIELD_INTEGER between 39.999 and 40.0001");

    // Not between
    evalTrue("FIELD_INTEGER not between 10 and 20");
    evalTrue("FIELD_INTEGER not between 10.5 and 20");
    evalTrue("FIELD_INTEGER not between 10 and 20");
    evalTrue("FIELD_INTEGER not between 10 and 20 and 'Test' is not null");

    // Integer with string coercion
    evalTrue("FIELD_INTEGER not between '10' and 20");
    evalTrue("FIELD_INTEGER not between 10.5 and '20'");

    // Date
    evalTrue("DATE '2019-02-28' between DATE '2019-01-01' and DATE '2019-12-31'");

    // Date with String coercion
    evalTrue("DATE '2019-02-28' between '2019-01-01' and DATE '2019-12-31'");
    evalTrue("DATE '2019-02-28' between DATE '2019-01-01' and '2019-12-31'");

    // String
    evalTrue("'the' between 'that' and 'then'");


    evalNull("NULL_INTEGER between -10 and 20").returnType(Types.BOOLEAN);
    evalNull("NULL_INTEGER between symmetric -10 and 20");
    evalNull("1 between NULL_INTEGER and 20");
    evalNull("1 between symmetric NULL_INTEGER and 20");
    evalNull("1 between -10 and NULL_INTEGER");
    evalNull("1 between symmetric -10 and NULL_INTEGER");

    evalFails("'the' between 1 and 2");
    evalFails("FIELD_INTEGER between 10 and");
    evalFails("FIELD_INTEGER between and 10");
    evalFails("FIELD_INTEGER between and ");
    evalFails("FIELD_INTEGER between FIELD_DATE and FIELD_STRING");
    evalFails("FIELD_INTEGER BETWEEN 4 AND");
    evalFails("FIELD_INTEGER BETWEEN  AND 7");
    evalFails("FIELD_INTEGER BETWEEN 4 OR 6");

    optimize("FIELD_INTEGER BETWEEN 10 AND 20");
    optimize("FIELD_NUMBER BETWEEN SYMMETRIC 20 AND 50", "FIELD_NUMBER BETWEEN 20 AND 50");
    optimize("FIELD_NUMBER BETWEEN SYMMETRIC 50 AND 20", "FIELD_NUMBER BETWEEN 20 AND 50");
    optimize("FIELD_STRING BETWEEN 'AZE' AND 'KLM'");
    optimize("FIELD_INTEGER between 3 and (5+1)", "FIELD_INTEGER BETWEEN 3 AND 6");
    optimizeFalse("2 between 3 and (5+1)");
    optimizeTrue("25.8 between 18 and 32");
    optimizeTrue("DATE '2019-02-28' between DATE '2019-01-01' and DATE '2019-12-31'");
  }

  @Test
  public void CastToBoolean() throws Exception {

    // Boolean to Boolean
    evalTrue("CAST(TRUE as Boolean)").returnType(Types.BOOLEAN);
    evalFalse("CAST(FALSE as Boolean)");

    // Cast String to Boolean
    evalTrue("'Yes'::Boolean").returnType(Types.BOOLEAN);
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
    evalTrue("CAST(Upper(FIELD_STRING_BOOLEAN_TRUE) as Boolean)");
    evalTrue("'1'::Boolean");
    evalTrue("'On'::Boolean");
    evalTrue("'Y'::Boolean");
    evalTrue("true = 'Y'::Boolean");
    evalTrue("'Yes'::Boolean");
    evalTrue("true = 'Yes'::Boolean");
    evalTrue("'T'::Boolean");
    evalTrue("'TRUE'::Boolean");
    evalTrue("true = 'True'::Boolean");
    evalFalse("'0'::Boolean");
    evalFalse("'N'::Boolean");
    evalFalse("'NO'::Boolean");
    evalFalse("'OFF'::Boolean");
    evalFalse("'F'::Boolean");
    evalFalse("'FALSE'::Boolean");
    
    // Cast Integer to Boolean
    evalTrue("CAST(1 as Boolean)").returnType(Types.BOOLEAN);
    evalTrue("CAST(-123 as Boolean)");
    evalTrue("1::Boolean");
    evalFalse("CAST(0 as Boolean)");
    evalFalse("CAST(-0 as Boolean)");

    // Cast Number to Boolean
    evalTrue("CAST(-12.1 as Boolean)").returnType(Types.BOOLEAN);
    evalFalse("CAST(0.000 as Boolean)");
    evalTrue("CAST(12345678901234567890123456789012345678 as Boolean)");

    // Null
    evalNull("CAST(NULL_INTEGER as Boolean)").returnType(Types.BOOLEAN);
    evalNull("CAST(NULL_NUMBER as Boolean)").returnType(Types.BOOLEAN);
    evalNull("CAST(NULL_STRING as Boolean)").returnType(Types.BOOLEAN);
    evalNull("CAST(NULL_BOOLEAN as Boolean)").returnType(Types.BOOLEAN);

    // Unsupported conversion
    evalFails("CAST(DATE '2019-02-25' AS BOOLEAN)");
    evalFails("CAST(FIELD_JSON AS BOOLEAN)");

    // Remove lossless cast
    optimize("CAST(FIELD_BOOLEAN_TRUE AS BOOLEAN)", "FIELD_BOOLEAN_TRUE");

    // optimize("CAST(FIELD_STRING AS BOOLEAN)", "TO_BOOLEAN(FIELD_STRING)");
    // optimize("FIELD_STRING::BOOLEAN", "TO_BOOLEAN(FIELD_STRING)");
  }


  @Test
  public void CastToInteger() throws Exception {
    // Integer
    evalEquals("CAST(0 as Integer)", 0L).returnType(Types.INTEGER);
    evalEquals("CAST(123 as Integer)", 123L);
    evalEquals("CAST(-123 as Integer(3))", -123L).returnType(IntegerType.of(3));

    // Number
    evalEquals("CAST(1.25 as Integer)", 1L).returnType(Types.INTEGER);
    evalEquals("CAST(-1.25 as Integer)", -1L);

    // Oracle truncate to 1 and Snowflake round it to 2
    evalEquals("CAST(1.75 as Integer)", 1L);

    // Boolean
    evalEquals("CAST(TRUE as Integer)", 1L).returnType(Types.INTEGER);
    evalEquals("CAST(FALSE as Integer)", 0L);

    // String
    evalEquals("CAST('1234' as Integer)", 1234L).returnType(Types.INTEGER);
    evalEquals("'1234'::Integer+5", 1239L);
    evalEquals("CAST('1234.567' as Integer)", 1234L);

    // Binary
    evalEquals("CAST(BINARY '123' as Integer)", 291L);

    // Date to Unix Epoch
    evalEquals("CAST(TIMESTAMP '1970-01-01 00:00:01' as Integer)", 1L);
    evalEquals("CAST(DATE '2019-02-25' AS INTEGER)", 1551052800L);
    evalEquals("CAST(DATE '1800-01-01' AS INTEGER)", -5364662400L);    
    
    // Null
    evalNull("CAST(NULL_NUMBER as Integer)").returnType(Types.INTEGER);
    evalNull("CAST(NULL_INTEGER as Integer)").returnType(Types.INTEGER);
    evalNull("CAST(NULL_BOOLEAN as Integer(4))").returnType(IntegerType.of(4));
    evalNull("Cast(NULL_STRING as INTEGER(5))").returnType(IntegerType.of(5));

    // Accept DataType quoted like a String
    evalEquals("Cast(123 as 'INTEGER')", 123L).returnType(Types.INTEGER);

    // Unsupported conversion
    evalFails("CAST(FIELD_JSON AS INTEGER)");

    // Remove unnecessary cast excepted with format
    optimize("CAST(FIELD_INTEGER AS INTEGER)", "FIELD_INTEGER");
    optimize("CAST(FIELD_DATE AS DATE)", "FIELD_DATE");

    // But don't remove with format
    optimize("CAST(FIELD_STRING AS DATE FORMAT 'YYYYMM')",
        "CAST(FIELD_STRING AS DATE FORMAT 'YYYYMM')");

    // Remove loss-less cast
    optimize("CAST(CAST(FIELD_INTEGER AS INTEGER(10)) AS INTEGER(5))",
        "CAST(FIELD_INTEGER AS INTEGER(5))");
    optimize("CAST(CAST(FIELD_INTEGER AS INTEGER(5)) AS INTEGER(5))",
        "CAST(FIELD_INTEGER AS INTEGER(5))");
    optimize("CAST(FIELD_NUMBER AS NUMBER(38,9))", "FIELD_NUMBER");
    optimize("CAST(CAST(FIELD_STRING AS STRING(100)) AS STRING(20))",
        "CAST(FIELD_STRING AS STRING(20))");
    optimize("CAST(CAST(FIELD_STRING AS STRING(20)) AS STRING(20))",
        "CAST(FIELD_STRING AS STRING(20))");
    optimize("CAST(FIELD_STRING AS STRING(1000))", "FIELD_STRING");

    // Don't remove not loss-less cast
    optimize("CAST(CAST(FIELD_INTEGER AS INTEGER(5)) AS INTEGER(10))",
        "CAST(CAST(FIELD_INTEGER AS INTEGER(5)) AS INTEGER(10))");
    optimize("CAST(FIELD_NUMBER AS NUMBER(12,5))", "CAST(FIELD_NUMBER AS NUMBER(12,5))");
    optimize("CAST(CAST(FIELD_STRING AS STRING(10)) AS STRING(100))",
        "CAST(CAST(FIELD_STRING AS STRING(10)) AS STRING(100))");
  }

  @Test
  public void CastToNumber() throws Exception {

    // Boolean
    evalEquals("CAST(TRUE as Number)", 1L).returnType(Types.NUMBER);
    evalEquals("CAST(FALSE as Number(1))", 0L).returnType(NumberType.of(1));

    // Integer
    evalEquals("CAST(0 as Number)", 0L).returnType(Types.NUMBER);
    evalEquals("CAST(123 as Number)", 123L).returnType(Types.NUMBER);
    evalEquals("CAST(-123 as Number(6,2))", -123L).returnType(NumberType.of(6,2));

    // Number
    evalEquals("CAST(1234.456 as Number(20,9))", 1234.456D).returnType(NumberType.of(20, 9));
    evalEquals("CAST(1234.456 as Number(10,1))", 1234.4D);
    evalEquals("1.23456::Number", 1L);
    evalEquals("1.23456::Number(10)", 1L).returnType(NumberType.of(10));
    evalEquals("1.23456::Number(10,0)", 1L).returnType(NumberType.of(10));
    evalEquals("1.23456::Number(10,2)", 1.23).returnType(NumberType.of(10, 2));
    evalEquals("1.23456::Number(10,6)", new BigDecimal("1.234560"))
        .returnType(NumberType.of(10, 6));
    evalEquals("1.23456::Number(38,9)", 1.23456).returnType(NumberType.of(38, 9));
    evalEquals("CAST(12345678901234567890123456789012345678 as Number(38,0))",
        new BigDecimal("12345678901234567890123456789012345678")).returnType(NumberType.of(38));

    // String
    evalEquals("CAST('0' as Number)", 0L).returnType(Types.NUMBER);
    evalEquals("CAST('1' As Number)", 1L);
    evalEquals("CAST('-1e-37' as Number(38,37))", -1e-37d);
    evalEquals("CAST(' -1e-37 ' as Number(38,37))", -1e-37d);
    evalEquals("CAST('1234' as Number)", 1234L);
    evalEquals("CAST('1234.567' as Number(10,5))", 1234.567d);
    evalEquals("CAST('  -1e-37  ' as Number(38,37))", -1e-37d);
    evalEquals("'1'::Number", 1L);
    evalEquals("'1234'::Number", 1234L);
    evalEquals("' -1e-3 '::Number(10,3)", new BigDecimal("-1e-3"));  
    
    // Date to Unix Epoch
    evalEquals("CAST(DATE '1970-01-01' as Number)", 0L).returnType(Types.NUMBER);    
    evalEquals("CAST(DATE '2019-02-25' AS Number)", 1551052800L).returnType(Types.NUMBER);
    evalEquals("CAST(DATE '1800-01-01' AS Number)", -5364662400L).returnType(Types.NUMBER);

    // Timestamp to Unix Epoch
    evalEquals("CAST(TIMESTAMP '1970-01-01 00:00:01' as Number)", 1L).returnType(Types.NUMBER);

    
    // Null
    evalNull("CAST(NULL_INTEGER as Number)").returnType(Types.NUMBER);
    evalNull("CAST(NULL_NUMBER as Number)").returnType(Types.NUMBER);
    evalNull("CAST(NULL_BIGNUMBER as Number(12,2))").returnType(NumberType.of(12, 2));

    // Unsupported conversion    
    evalFails("CAST(FIELD_JSON AS NUMBER)");

    optimize("CAST(FIELD_INTEGER AS NUMBER)", "CAST(FIELD_INTEGER AS NUMBER)");
    optimize("FIELD_INTEGER::NUMBER", "CAST(FIELD_INTEGER AS NUMBER)");

    // Chained cast
    optimize("CAST(CAST(CAST(123456 AS INTEGER) AS NUMBER) AS NUMBER)", "123456");
  }

  @Test
  public void CastToString() throws Exception {
    // Boolean
    evalEquals("CAST(true as String)", "TRUE").returnType(Types.STRING);
    evalEquals("CAST(false as String)", "FALSE");
    evalEquals("CAST(FIELD_BOOLEAN_TRUE as String)", "TRUE");
    evalEquals("true::String", "TRUE");
    evalEquals("true::String(1)", "T");

    // Integer
    evalEquals("CAST(12923 AS STRING)", "12923");
    evalEquals("CAST(-1234 AS STRING FORMAT '9999MI')", "1234-");
    evalEquals("CAST(FIELD_INTEGER AS STRING)", "40");

    // Number
    evalEquals("CAST(123.6 as String)", "123.6");
    evalEquals("123.6::String", "123.6");
    evalEquals("CAST(0.45 AS STRING)", "0.45");
    evalEquals("CAST(0.45 AS STRING FORMAT 'FM000.00')", "000.45");
    evalEquals("CAST(1234.56 AS STRING FORMAT '9999MI')", "1234 ");
    evalEquals("CAST(12345678901234567890.123 AS STRING)", "12345678901234567890.123");
    evalEquals("CAST(FIELD_NUMBER as String)", "-5.12");

    // Date
    evalEquals("CAST(DATE '2019-02-25' AS String)", "2019-02-25");
    evalEquals("CAST(DATE '2019-02-25' AS String FORMAT 'DD/MM/YYYY')", "25/02/2019");

    // String
    evalEquals("CAST('abcdefg' AS String)", "abcdefg");
    evalEquals("CAST('abcdefg' AS String(3))", "abc");
    evalEquals("CAST('abcdefg' AS String(20))", "abcdefg");

    // Null
    evalNull("CAST(NULL_BINARY as STRING)").returnType(Types.STRING);
    evalNull("CAST(NULL_STRING as String)").returnType(Types.STRING);
    evalNull("Cast(NULL_BOOLEAN as STRING(4))").returnType(StringType.of(4));
    evalNull("Cast(NULL_DATE as STRING(10) format 'YYYY-MM-DD')").returnType(StringType.of(10));

    optimize("CAST(FIELD_STRING AS STRING(1000))", "FIELD_STRING");
    optimize("CAST(TRUE AS STRING)", "'TRUE'");
    optimize("CAST(FALSE AS STRING(5))", "'FALSE'");
    optimize("CAST(FALSE AS STRING(1))", "'F'");
    optimize("CAST(123456 AS STRING(6))", "'123456'");
    optimize("CAST(123456.1 AS STRING(8))", "'123456.1'");
    optimize("CAST(123456 AS STRING(3))", "'123'");
    optimize("CAST(123456.1 AS STRING(3))", "'123'");
    optimize("CAST(DATE '2013-02-02' AS STRING(10))", "'2013-02-02'");
    optimize("CAST(DATE '2013-02-02' AS STRING(4))", "'2013'");
  }

  @Test
  public void CastToDate() throws Exception {
    // String
    evalEquals("CAST('2020-march' as DATE FORMAT 'YYYY-MONTH')", LocalDate.of(2020, 3, 1))
        .returnType(Types.DATE);
    evalEquals("CAST('2020-01-19 11:23:44' as DATE FORMAT 'YYYY-MM-DD HH:MI:SS')",
        LocalDateTime.of(2020, 1, 19, 11, 23, 44)).returnType(Types.DATE);

    // Integer Unix Epoch
    evalEquals("CAST(0 AS DATE)", LocalDateTime.of(1970, 1, 1, 0, 0, 0));
    evalEquals("CAST(1551052800 AS DATE)", LocalDate.of(2019, 2, 25));
    evalEquals("CAST(-5364662400 AS DATE)", LocalDate.of(1800, 1, 1));
    evalEquals("CAST(1284352323 AS DATE)", LocalDateTime.of(2010, 9, 13, 4, 32, 3));
    
    // Number Unix Epoch
    evalEquals("CAST(1551052800.000000000 AS DATE)", LocalDate.of(2019, 2, 25));
    evalEquals("CAST(-5364662400.000000000 AS DATE)", LocalDate.of(1800, 1, 1));    
    evalEquals("CAST(1284352323.1 AS DATE)", LocalDateTime.of(2010, 9, 13, 4, 32, 3,  100000000));
    evalEquals("CAST(1284352323.12 AS DATE)", LocalDateTime.of(2010, 9, 13, 4, 32, 3, 120000000));
    evalEquals("CAST(1284352323.123 AS DATE)", LocalDateTime.of(2010, 9, 13, 4, 32, 3,  123000000));            
    evalEquals("CAST(1284352323.123456789 AS DATE)", LocalDateTime.of(2010, 9, 13, 4, 32, 3, 123456789));
    
    // Null
    evalNull("CAST(NULL as Date)").returnType(Types.DATE);
    evalNull("CAST(NULL_DATE as Date)").returnType(Types.DATE);

    // Error parsing format
    evalFails("CAST('2023-01-01' AS DATE FORMAT 'YYYY-MM')");
    evalFails("CAST('2023-01' AS DATE FORMAT 'YYYY-MM-DD')");

    // Unsupported conversion
    evalFails("CAST(TRUE AS DATE)");

    // optimize("CAST(FIELD_STRING AS DATE)", "TO_DATE(FIELD_STRING)");
    // optimize("CAST(FIELD_STRING AS DATE FORMAT 'YYYY-MM-DD')",
    // "TO_DATE(FIELD_STRING,'YYYY-MM-DD')");
    optimize("CAST(TO_CHAR(FIELD_DATE,'YYYYMMDD') AS DATE FORMAT 'YYYYMMDD')",
        "CAST(TO_CHAR(FIELD_DATE,'YYYYMMDD') AS DATE FORMAT 'YYYYMMDD')");
    optimize("Cast('2021-02-08' as DATE)", "DATE '2021-02-08'");
    optimize("'2021-02-08'::DATE", "DATE '2021-02-08'");
  }

  @Test
  public void CastToBinary() throws Exception {
    // String
    evalEquals("CAST('AB' as BINARY)", "AB".getBytes()).returnType(Types.BINARY);

    // Null
    evalNull("CAST(NULL_STRING as Binary)").returnType(Types.BINARY);
    evalNull("CAST(NULL_BINARY as Binary(100))").returnType(BinaryType.of(100));

    optimize("CAST(FIELD_STRING AS BINARY)", "CAST(FIELD_STRING AS BINARY)");
  }

  @Test
  public void CastToJson() throws Exception {
    // String
    // evalEquals("CAST('A' as JSON)", "A".getBytes());

    // Null
    evalNull("CAST(NULL as Json)").returnType(Types.JSON);
    evalNull("CAST(NULL_JSON as Json)");

    optimize("CAST(FIELD_JSON AS JSON)", "FIELD_JSON");
    // optimize("CAST(FIELD_STRING AS JSON)", "TO_JSON(FIELD_STRING)");
    // optimize("FIELD_STRING::JSON", "TO_JSON(FIELD_STRING)");
  }

  @Test
  public void CastToInterval() throws Exception {
    // String
    evalEquals("CAST('5 years' as INTERVAL)", Interval.of(5)).returnType(Types.INTERVAL);
    evalEquals("'2 hour'::INTERVAL", Interval.of(0, 0, 0, 2)).returnType(Types.INTERVAL);

    // Null
    evalNull("CAST(NULL as INTERVAL)");

    evalFails("CAST(3 as INTERVAL");
  }

  @Test
  public void Cast() throws Exception {

    evalEquals("TO_NUMBER('123','000')::INTEGER+1", 124L).returnType(NumberType.of(20));

    // Accept data type quoted like a String
    evalEquals("Cast(' 123' as 'INTEGER')", 123L).returnType(Types.INTEGER);
    evalEquals("Cast('2022-01-01' as 'DATE')", LocalDate.of(2022, 1, 1));

    // Error syntax
    evalFails("'1234':");
    evalFails("'1234':NUMBER");
    evalFails("'1234'::");
    evalFails("CAST('bad' AS)");
    evalFails("CAST('2020-01-01' AS NULL)");
    evalFails("CAST(1234 AS STRING FORMAT )");
    evalFails("CAST(DATE '2019-02-25' AS String FORMAT )");
    evalFails("CAST(DATE '2019-02-25' AS String FORMAT NULL)");
    evalFails("CAST 3 as BOOLEAN)");
    evalFails("CAST(3 as BOOLEAN");
    evalFails("CAST(3 as NILL)");
    evalFails("CAST(3 as )");
    evalFails("CAST(3 as");
    evalFails("CAST(3 STRING");

    // Unknown data type
    evalFails("Cast(123 as Nill)");
    evalFails("Cast(123 as 1)");
    evalFails("Cast(123 as TRUE)");
    evalFails("CAST('bad' AS NULL)");
    evalFails("Cast(123 as 'Text')");
  }

  @Test
  public void AtTimeZone() throws Exception {
    evalEquals("TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE 'Europe/Paris'",
        ZonedDateTime.of(2023, 5, 25, 20, 48, 00, 0, ZoneId.of("Europe/Paris")));
    evalEquals("TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE 'Singapore'",
        ZonedDateTime.of(2023, 5, 25, 20, 48, 00, 0, ZoneId.of("Singapore")));
    evalEquals("TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE 'GMT+0'",
        ZonedDateTime.of(2023, 5, 25, 20, 48, 00, 0, ZoneId.of("GMT+0")));
    evalEquals("TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE 'CET'",
        ZonedDateTime.of(2023, 5, 25, 20, 48, 00, 0, ZoneId.of("CET")));
    evalEquals("TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE 'EET'",
        ZonedDateTime.of(2023, 5, 25, 20, 48, 00, 0, ZoneId.of("EET")));
    evalEquals("(TIMESTAMP '2023-05-25 10:48:00' AT TIME ZONE 'UTC') AT TIME ZONE 'Asia/Singapore'",
        ZonedDateTime.of(2023, 5, 25, 10, 48, 00, 0, ZoneId.of("Asia/Singapore")));

    evalFails("TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE 'XYZ'");
    evalFails("TIMESTAMP '2023-05-25 20:48:00' AT TIME 'Europe/Paris'");
    evalFails("TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE");
    evalFails("TIMESTAMP '2023-05-25 20:48:00' AT ZONE 'Europe/Paris'");

    optimize("TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE 'Europe/Paris'");
  }

  @Test
  public void ConvertTimeZone() throws Exception {
    evalEquals("CONVERT_TIMEZONE('Europe/Paris',TIMESTAMP '2020-05-25 20:48:00')",
        ZonedDateTime.of(2020, 5, 25, 22, 48, 00, 0, ZoneId.of("Europe/Paris")));
    // evalEquals("CONVERT_TIMEZONE('Asia/Singapore',TIMESTAMP '2020-05-25 20:48:00' AT TIME ZONE
    // 'UTC')", ZonedDateTime.of(2020, 5, 26, 18,48,00,0,ZoneId.of("Asia/Singapore")));

    optimize("CONVERT_TIMEZONE('Europe/Paris',FIELD_TIMESTAMP)");
    optimize("CONVERT_TIMEZONE('Europe/Paris',TIMESTAMP '2020-05-25 20:48:00')",
        "TIMESTAMP '2020-05-25 22:48:00' AT TIME ZONE 'Europe/Paris'");
  }

  @Test
  public void Positive() throws Exception {
    evalEquals("+(40)", 40L);
    evalEquals("+FIELD_INTEGER", 40L).returnType(IntegerType.of(12));
    evalEquals("+FIELD_NUMBER", -5L).returnType(Types.NUMBER);
    evalEquals("+FIELD_BIGNUMBER", 123456L).returnType(Types.NUMBER);
    evalEquals("+40", 40L);
    evalEquals("1+ +2", 3L);
    evalNull("+NULL_INTEGER").returnType(Types.INTEGER);

    optimize("+FIELD_INTEGER", "FIELD_INTEGER");
  }

  @Test
  public void Negative() throws Exception {
    evalEquals("-(1+2)", -3L);
    evalEquals("-FIELD_INTEGER", -40L).returnType(IntegerType.of(12));
    evalEquals("-FIELD_NUMBER", 5L).returnType(Types.NUMBER);
    evalEquals("-FIELD_BIGNUMBER", -123456L).returnType(Types.NUMBER);
    evalEquals("+40", 40L);
    evalEquals("1+ -2", -1L);
    evalNull("-NULL_INTEGER").returnType(Types.INTEGER);

    optimize("-FIELD_INTEGER", "-FIELD_INTEGER");
    optimize("-(10+2)", "-12");
    optimize("-(1.1+1.2)", "-2.3");
    optimize("-(0)", "0");

    optimize("+(0 / 0)", "0/0");
    // optimize("-(0 / 0)", "-(0/0)");
    optimize("-(-(0 / 0))", "0/0");

    optimize("-RANDOM()", "-RANDOM()");
    optimize("+RANDOM()", "RANDOM()");

    // Negative
    optimize("-(-FIELD_INTEGER)", "FIELD_INTEGER");
    optimize("-(FIELD_INTEGER-FIELD_NUMBER)", "FIELD_NUMBER-FIELD_INTEGER");
  }

  @Test
  public void Mod() throws Exception {
    evalEquals("11%4", 3L).returnType(Types.NUMBER);
    evalEquals("Mod(11,4)", 3L);
    evalEquals("Mod(11,-4)", 3L);
    evalEquals("Mod(-11,4)", -3L);
    evalEquals("Mod(-11,-4)", -3L);
       
    evalEquals("Mod(11.3,4)", 3.3D);    
    evalEquals("Mod(11.3::NUMBER(12,4),4)", 3.3);
    
    evalNull("Mod(NULL_INTEGER,2)");
    evalNull("Mod(2,NULL_INTEGER)");
    
    // Syntax error
    evalFails("'TEST'%5");
    evalFails("Mod()");
    evalFails("Mod(3)");
    // Division by 0
    evalFails("Mod(9,0)");
    
    optimize("FIELD_INTEGER%4");
  }

  @Test
  public void Multiplication() throws Exception {
    evalEquals("2.55*10", 25.50D).returnType(NumberType.of(5, 2));
    evalEquals("4*10", 40D).returnType(IntegerType.of(3));
    evalEquals("-4*-1", 4D).returnType(IntegerType.of(2));
    evalEquals("2*-2", -4D).returnType(IntegerType.of(2));
    evalEquals("100 * .5", 50D).returnType(NumberType.of(5,1));
    evalEquals("1.23456::Number(38,9)*-2.987654", -3.68843812224).returnType(NumberType.of(38,15));
    evalEquals("FIELD_NUMBER::NUMBER(4,1)*3::NUMBER(3,2)", -15L).returnType(NumberType.of(7, 3));
    evalEquals("FIELD_NUMBER::NUMBER(38,5)*FIELD_INTEGER::NUMBER(9,8)", -204L)
        .returnType(NumberType.of(38, 8));

    // Implicit coercion from STRING
    evalEquals("'2'*2", 4L).returnType(NumberType.of(38,18));
    evalEquals("2.5*'2'", 5L).returnType(NumberType.of(38,10));
    
    // Check no overflow Long.MAX_VALUE * 2
    evalEquals("9223372036854775807*2", new BigDecimal("18446744073709551614")).returnType(NumberType.of(20));
    // Check no underflow Long.MIN_VALUE * 2
    evalEquals("-9223372036854775808*2", new BigDecimal("-18446744073709551616"));

    evalNull("NULL_INTEGER*1*1");
    evalNull("1*NULL_INTEGER");
    evalNull("FIELD_STRING_INTEGER::INTEGER*NULL_INTEGER");

    optimize("FIELD_INTEGER*4", "4*FIELD_INTEGER");
    optimize("FIELD_INTEGER*3*2", "6*FIELD_INTEGER");
    optimize("3*(FIELD_INTEGER*1)*1*(2*5)", "30*FIELD_INTEGER");
    optimize("4*FIELD_INTEGER*0.5", "2*FIELD_INTEGER");
    
    // Simplify arithmetic 0*A → 0
    optimize("FIELD_INTEGER*0", "0");
    optimize("0*FIELD_INTEGER", "0");
    
    // Simplify arithmetic 1*A → A
    optimize("FIELD_INTEGER*1", "FIELD_INTEGER");
    optimize("1.0*FIELD_INTEGER", "FIELD_INTEGER");
        
    // Simplify arithmetic (-A)*(-B) → A*B
    optimize("-FIELD_INTEGER*(-FIELD_NUMBER)", "FIELD_INTEGER*FIELD_NUMBER");
    
    // Simplify arithmetic A*A → SQUARE(A)
    optimize("FIELD_INTEGER*FIELD_INTEGER", "SQUARE(FIELD_INTEGER)");
    
    // Simplify arithmetic 1/A*B → B/A
    optimize("1/FIELD_INTEGER*4", "4/FIELD_INTEGER");
  }

  @Test
  public void Div() throws Exception {
    evalEquals("FIELD_INTEGER/4", 10D).returnType(NumberType.of(18, 6));
    evalEquals("10/4", 2.5D).returnType(NumberType.of(8, 6));
    evalEquals("40/-10", -4L).returnType(NumberType.of(8, 6));
    evalEquals("-40/-10", 4L).returnType(NumberType.of(8, 6));
    evalEquals("5/2", 2.5D).returnType(NumberType.of(7, 6));
    evalEquals("10.1/2.1", new BigDecimal("4.8095238095238095238095238095238")).returnType(NumberType.of(9, 6));
    evalEquals("0.1/0.0000000000001", 1000000000000L).returnType(NumberType.of(30, 16));
    evalEquals("FIELD_NUMBER::NUMBER(4,1)/3::NUMBER(3,2)", -1L).returnType(NumberType.of(11, 6));

    evalNull("NULL_INTEGER/1");
    evalNull("NULL_INTEGER/0");
    evalNull("1/NULL_INTEGER");
        
    evalFails("40/0");

    // Implicit coercion from STRING
    evalEquals("'8'/2", 4L).returnType(NumberType.of(38,37));
    evalEquals("5/'2'", 2.5).returnType(NumberType.of(38,37));
    
    optimize("0/0", "0/0");
    optimize("FIELD_INTEGER/4");
    
    // Simplify arithmetic A/1 → A
    optimize("FIELD_INTEGER/1", "FIELD_INTEGER");
    optimize("FIELD_INTEGER/1.0", "FIELD_INTEGER");
    
    // Simplify arithmetic (-A)/(-B) → A/B
    optimize("-FIELD_NUMBER/-FIELD_INTEGER", "FIELD_NUMBER/FIELD_INTEGER");
  }

  @Test
  public void Div0() throws Exception {
    evalEquals("Div0(10,4)", 2.5D).returnType(NumberType.of(8, 6));
    evalEquals("Div0(FIELD_INTEGER,-100)", -0.4D);
    evalEquals("Div0(FIELD_INTEGER,0)", 0L);
    evalEquals("Div0(FIELD_INTEGER,2)", 20L).returnType(NumberType.of(18, 6));
    evalNull("Div0(NULL_INTEGER,1)");
    evalNull("Div0(NULL_INTEGER,0)");
    evalNull("Div0(1,NULL_INTEGER)");

    evalFails("Div0()");
    evalFails("Div0(40)");
    evalFails("Div0(40,1,2)");

    optimize("DIV0(FIELD_INTEGER,1)", "FIELD_INTEGER");
    optimize("DIV0(-FIELD_NUMBER,-FIELD_INTEGER)", "DIV0(FIELD_NUMBER,FIELD_INTEGER)");
  }

  @Test
  public void BitNot() throws Exception {
    evalEquals("~1", -2L).returnType(Types.INTEGER);
    evalEquals("~ 1", -2L).returnType(Types.INTEGER);
    evalEquals("~0", -1L);
    evalEquals("~4", -5L);
    evalEquals("~65504", -65505L);
    evalNull("~NULL_INTEGER").returnType(Types.INTEGER);
    evalFails("~");
    evalFails("~ ");

    // Alias function
    evalEquals("BIT_NOT(1)", -2L);

    optimize("~FIELD_INTEGER");
    optimize("~(~FIELD_INTEGER)", "FIELD_INTEGER");
  }

  @Test
  public void BitAnd() throws Exception {
    evalEquals("3 & 2", 2L);
    evalEquals("100 & 2", 0L);
    evalEquals("100 & 2 & 1", 0L);
    evalNull("100 & NULL_INTEGER").returnType(Types.INTEGER);
    evalNull("NULL_INTEGER & 100").returnType(Types.INTEGER);
    evalFails("100&");
    evalFails("100 & ");

    // Alias function
    evalEquals("BIT_AND(3,2)", 2L).returnType(Types.INTEGER);
    
    optimize("FIELD_INTEGER&4");
  }

  @Test
  public void BitOr() throws Exception {
    evalEquals("100 | 2", 102L);
    evalEquals("3 | 2", 3L);
    evalNull("100 | NULL_INTEGER").returnType(Types.INTEGER);
    evalNull("NULL_INTEGER | 100").returnType(Types.INTEGER);
    evalFails("3|");
    evalFails("3 | ");

    // Alias function
    evalEquals("BIT_OR(100,2)", 102L).returnType(Types.INTEGER);
    
    optimize("FIELD_INTEGER|4","4|FIELD_INTEGER");    
    optimize("1|FIELD_INTEGER|4","5|FIELD_INTEGER");
    optimize("FIELD_INTEGER|0","FIELD_INTEGER");
    optimize("0|FIELD_INTEGER","FIELD_INTEGER");
  }

  @Test
  public void BitXor() throws Exception {
    evalEquals("BIT_XOR(2,2)", 0L).returnType(Types.INTEGER);
    evalEquals("2 ^ 1", 3L).returnType(Types.INTEGER);
    evalEquals("100 ^ 2", 102L).returnType(Types.INTEGER);
    evalNull("100 ^ NULL_INTEGER").returnType(Types.INTEGER);
    evalNull("NULL_INTEGER ^ 100").returnType(Types.INTEGER);

    evalFails("100^");
    evalFails("100 ^ ");

    optimize("FIELD_INTEGER^4");
  }

  @Test
  public void BoolNot() throws Exception {
    evalTrue("FIELD_BOOLEAN_TRUE is not false").returnType(Types.BOOLEAN);
    evalTrue("NULL_BOOLEAN is null").returnType(Types.BOOLEAN);
    evalTrue("NOT (NULL_BOOLEAN is not null)").returnType(Types.BOOLEAN);
    evalFalse("NOT 1").returnType(Types.BOOLEAN);
    evalTrue("NOT 0").returnType(Types.BOOLEAN);
    evalFalse("NOT FIELD_INTEGER").returnType(Types.BOOLEAN);
    evalTrue("NOT FIELD_STRING_BOOLEAN_FALSE").returnType(Types.BOOLEAN);
    evalFalse("NOT FIELD_STRING_BOOLEAN_TRUE").returnType(Types.BOOLEAN);
    evalTrue("NOT NOT True").returnType(Types.BOOLEAN);
    evalNull("NOT NULL_BOOLEAN").returnType(Types.BOOLEAN);

    evalFails("FIELD_BOOLEAN_TRUE is ");
    evalFails("NOT");

    // Function syntax
    evalTrue("NOT(FALSE)").returnType(Types.BOOLEAN);

    optimize("NOT FIELD_BOOLEAN_TRUE");
    optimize("NOT NOT FIELD_BOOLEAN_TRUE", "FIELD_BOOLEAN_TRUE");
    optimize("NOT NOT NOT FIELD_BOOLEAN_TRUE", "NOT FIELD_BOOLEAN_TRUE");
    optimizeFalse("not true");
    optimizeTrue("not false");
    optimizeTrue("not not true");
    optimizeFalse("not not false");
    optimize("NOT (NOT(FIELD_BOOLEAN_TRUE))", "FIELD_BOOLEAN_TRUE");
    optimize("NOT (FIELD_INTEGER>5)", "5>=FIELD_INTEGER");
    optimize("NOT (FIELD_INTEGER>=5)", "5>FIELD_INTEGER");
    optimize("NOT (FIELD_INTEGER<5)", "5<=FIELD_INTEGER");
    optimize("NOT (FIELD_INTEGER<=5)", "5<FIELD_INTEGER");
    optimize("NOT (FIELD_INTEGER=5)", "5!=FIELD_INTEGER");
    optimize("NOT (FIELD_INTEGER<>5)", "5=FIELD_INTEGER");
    optimize("NOT (FIELD_BOOLEAN_TRUE IS TRUE)", "FIELD_BOOLEAN_TRUE IS NOT TRUE");
    optimize("NOT (FIELD_BOOLEAN_TRUE IS NOT TRUE)", "FIELD_BOOLEAN_TRUE IS TRUE");
    optimize("NOT (FIELD_BOOLEAN_TRUE IS FALSE)", "FIELD_BOOLEAN_TRUE IS NOT FALSE");
    optimize("NOT (FIELD_BOOLEAN_TRUE IS NOT FALSE)", "FIELD_BOOLEAN_TRUE IS FALSE");
    optimize("NOT (FIELD_BOOLEAN_TRUE IS NOT NULL)", "FIELD_BOOLEAN_TRUE IS NULL");
    optimize("NOT (FIELD_BOOLEAN_TRUE IS NULL)", "FIELD_BOOLEAN_TRUE IS NOT NULL");
    optimize("NOT (FIELD_BOOLEAN_TRUE IS DISTINCT FROM NULL_BOOLEAN)",
        "FIELD_BOOLEAN_TRUE IS NOT DISTINCT FROM NULL_BOOLEAN");
    optimize("NOT (FIELD_BOOLEAN_TRUE IS NOT DISTINCT FROM NULL_BOOLEAN)",
        "FIELD_BOOLEAN_TRUE IS DISTINCT FROM NULL_BOOLEAN");
    optimize("NOT (FIELD_STRING NOT SIMILAR TO '.*(b|d).*')","FIELD_STRING SIMILAR TO '.*(b|d).*'");
    optimize("NOT (FIELD_STRING SIMILAR TO '.*(b|d).*')","FIELD_STRING NOT SIMILAR TO '.*(b|d).*'");
    optimize("NOT (FIELD_STRING IN ('A','B'))","FIELD_STRING NOT IN ('A','B')");
    optimize("NOT (FIELD_STRING NOT IN ('A','B'))","FIELD_STRING IN ('A','B')");
    
    // optimize("(A IS NOT NULL OR B) AND FIELD_BOOLEAN_TRUE IS NOT NULL","FIELD_BOOLEAN_TRUE IS NOT
    // NULL");
  }

  @Test
  public void BoolOr() throws Exception {
    evalTrue("true OR true").returnType(Types.BOOLEAN);
    evalTrue("true OR false");
    evalTrue("false OR true");
    evalFalse("false OR false").returnType(Types.BOOLEAN);
    evalTrue("true OR NULL_BOOLEAN");
    evalTrue("true OR FIELD_BOOLEAN_TRUE");
    evalTrue("NULL_BOOLEAN OR true");
    evalTrue("FIELD_BOOLEAN_TRUE OR false");
    evalNull("false OR NULL_BOOLEAN").returnType(Types.BOOLEAN);
    evalNull("NULL_BOOLEAN OR false");
    evalNull("NULL_BOOLEAN OR NULL_BOOLEAN");

    evalFails("false OR");
    evalFails("OR false");

    optimize("FIELD_BOOLEAN_TRUE OR NULL_BOOLEAN");
    optimizeTrue("true or true");
    optimizeTrue("true or false");
    optimizeTrue("false or true");
    optimizeFalse("false or false");
    optimizeTrue("true or NULL_BOOLEAN");
    optimizeTrue("NULL_BOOLEAN or true");
    optimize("NULL_BOOLEAN or NULL_BOOLEAN", "NULL_BOOLEAN");

    optimizeTrue("FIELD_BOOLEAN_TRUE or true");
    optimizeTrue("true or FIELD_STRING");
    optimizeTrue("true or FIELD_BOOLEAN_TRUE");
    optimizeTrue("FIELD_BOOLEAN_TRUE or true");
    optimize("false or FIELD_BOOLEAN_TRUE", "FALSE OR FIELD_BOOLEAN_TRUE");
    optimize("FIELD_BOOLEAN_TRUE or false", "FALSE OR FIELD_BOOLEAN_TRUE");
    optimize("FIELD_BOOLEAN_TRUE or FIELD_BOOLEAN_TRUE", "FIELD_BOOLEAN_TRUE");
    optimize("FIELD_BOOLEAN_TRUE OR NULL_BOOLEAN OR (FIELD_INTEGER>0) OR FIELD_BOOLEAN_TRUE",
        "FIELD_BOOLEAN_TRUE OR NULL_BOOLEAN OR 0<FIELD_INTEGER");

    optimize("false and true or FIELD_BOOLEAN_TRUE", "FALSE OR FIELD_BOOLEAN_TRUE");

    // Duplicate predicate
    optimize("FIELD_BOOLEAN_TRUE OR FIELD_BOOLEAN_TRUE", "FIELD_BOOLEAN_TRUE");
    optimize("FIELD_INTEGER=2 OR 2=FIELD_INTEGER", "2=FIELD_INTEGER");


    // Check if simplify doesn't create infinity loop with same operator if order change 
    optimize("FIELD_STRING LIKE 'AB%' OR FIELD_STRING LIKE 'BC%' OR FIELD_STRING LIKE '%DE' ", "ENDSWITH(FIELD_STRING,'DE') OR STARTSWITH(FIELD_STRING,'AB') OR STARTSWITH(FIELD_STRING,'BC')");

    // Simplify x < a OR x = a → x <= a
    optimize("FIELD_INTEGER<1 OR FIELD_INTEGER=1", "1>=FIELD_INTEGER");
    // Simplify x < a OR x != a → x != a
    optimize("FIELD_INTEGER<1 OR FIELD_INTEGER!=1", "1!=FIELD_INTEGER");
    // Simplify x < a OR x > a → x != a"
    optimize("FIELD_INTEGER<1 OR FIELD_INTEGER>1", "1!=FIELD_INTEGER");
    // Simplify x > a OR x != a → x != a
    optimize("FIELD_INTEGER>1 OR FIELD_INTEGER!=1", "1!=FIELD_INTEGER");
    // Simplify x > a OR x = a → x >= a
    optimize("FIELD_INTEGER>1 OR FIELD_INTEGER=1", "1<=FIELD_INTEGER");
    // Simplify x OR x IS NOT NULL → x IS NOT NULL
    optimize("FIELD_INTEGER OR FIELD_INTEGER IS NOT NULL", "FIELD_INTEGER IS NOT NULL");
    // Simplify x<5 OR IS NOT NULL(x) → IS NOT NULL(x)
    optimize("FIELD_INTEGER>10 OR FIELD_INTEGER IS NOT NULL", "FIELD_INTEGER IS NOT NULL");
    optimize("FIELD_INTEGER>=10 OR FIELD_INTEGER IS NOT NULL", "FIELD_INTEGER IS NOT NULL");
    optimize("FIELD_INTEGER<10 OR FIELD_INTEGER IS NOT NULL", "FIELD_INTEGER IS NOT NULL");
    optimize("FIELD_INTEGER<=10 OR FIELD_INTEGER IS NOT NULL", "FIELD_INTEGER IS NOT NULL");
    optimize("FIELD_INTEGER=10 OR FIELD_INTEGER IS NOT NULL", "FIELD_INTEGER IS NOT NULL");
    optimize("FIELD_INTEGER<>10 OR FIELD_INTEGER IS NOT NULL", "FIELD_INTEGER IS NOT NULL");
    optimize("FIELD_INTEGER!=10 OR FIELD_INTEGER IS NOT NULL", "FIELD_INTEGER IS NOT NULL");
    // Simplify X=1 OR X=2 OR X=3 → X IN (1,2,3) order is not important
    optimize("FIELD_INTEGER=1 OR FIELD_INTEGER=2 OR FIELD_INTEGER=3", "FIELD_INTEGER IN (3,1,2)");
    optimize("FIELD_INTEGER=1 OR FIELD_INTEGER in (2,3)", "FIELD_INTEGER IN (1,2,3)");
    optimize("FIELD_INTEGER IN (1,2) OR FIELD_INTEGER IN (3,4)", "FIELD_INTEGER IN (1,2,3,4)");
    optimize("FIELD_STRING='1' OR NULL_INTEGER in (1,2)",
        "'1'=FIELD_STRING OR NULL_INTEGER IN (1,2)");
  }

  @Test
  public void BoolAnd() throws Exception {
    evalTrue("true AND true");
    evalTrue("true AND FIELD_STRING_BOOLEAN_TRUE");
    evalFalse("true AND false");
    evalFalse("false AND true");
    evalFalse("false AND false");
    evalFalse("false AND FIELD_BOOLEAN_TRUE");
    evalFalse("false AND NULL_BOOLEAN");
    evalFalse("NULL_BOOLEAN AND false");

    evalNull("FIELD_BOOLEAN_TRUE AND NULL_BOOLEAN");
    evalNull("NULL_BOOLEAN AND FIELD_BOOLEAN_TRUE");
    evalNull("true AND NULL_BOOLEAN");
    evalNull("NULL_BOOLEAN AND true");
    evalNull("NULL_BOOLEAN AND FIELD_BOOLEAN_TRUE");

    evalFails("false AND");
    evalFails("AND false");

    optimize("FIELD_BOOLEAN_TRUE AND NULL_BOOLEAN");
    optimizeTrue("25>=12 and 14<15");
    optimizeTrue("true and true");
    optimizeFalse("true and false");
    optimizeFalse("false and true");
    optimizeFalse("false and false");

    optimizeFalse("false and NULL");
    optimizeFalse("false and FIELD_BOOLEAN_TRUE");
    optimizeFalse("FIELD_BOOLEAN_TRUE and false");
    optimizeFalse("null AND null AND null AND false");
    optimizeNull("null AND null AND null AND true");

    // Duplicate predicate
    optimize("FIELD_BOOLEAN_TRUE and FIELD_BOOLEAN_TRUE", "FIELD_BOOLEAN_TRUE");
    optimize("FIELD_BOOLEAN_TRUE AND NULL_BOOLEAN AND (FIELD_INTEGER>0) AND FIELD_BOOLEAN_TRUE",
        "FIELD_BOOLEAN_TRUE AND NULL_BOOLEAN AND 0<FIELD_INTEGER");
    optimize("(FIELD_INTEGER*2>1) AND FIELD_BOOLEAN_TRUE AND (2*FIELD_INTEGER>1)",
        "FIELD_BOOLEAN_TRUE AND 1<2*FIELD_INTEGER");
    optimize("FIELD_INTEGER=1 AND FIELD_BOOLEAN_TRUE AND FIELD_INTEGER=1",
        "FIELD_BOOLEAN_TRUE AND 1=FIELD_INTEGER");
    optimize("(FIELD_INTEGER*2>1) AND FIELD_BOOLEAN_TRUE AND (2*FIELD_INTEGER>1)",
        "FIELD_BOOLEAN_TRUE AND 1<2*FIELD_INTEGER");

    // Check if simplify doesn't create infinity loop with same operator if order change 
    optimize("FIELD_STRING LIKE 'AB%' AND FIELD_STRING LIKE 'BC%' AND FIELD_STRING LIKE '%DE' ", "ENDSWITH(FIELD_STRING,'DE') AND STARTSWITH(FIELD_STRING,'AB') AND STARTSWITH(FIELD_STRING,'BC')");
    
    // Simplify IS NULL
    optimizeFalse("FIELD_INTEGER IS NULL AND FIELD_INTEGER>5");
    optimizeFalse("FIELD_INTEGER IS NULL AND FIELD_INTEGER=5");
    optimizeFalse("FIELD_INTEGER IS NULL AND FIELD_INTEGER<>5");

    // Simplify IS NOT NULL
    optimize("FIELD_INTEGER>5 AND FIELD_INTEGER IS NOT NULL AND FIELD_INTEGER>5",
        "5<FIELD_INTEGER");

    // Simplify X<>1 AND X<>2 → X NOT IN (1,2)
    optimize("FIELD_INTEGER<>1 AND FIELD_INTEGER<>2", "FIELD_INTEGER NOT IN (1,2)");

    // Simplify X<>1 AND X NOT IN (2,3) → X NOT IN (1,2,3)
    optimize("FIELD_INTEGER<>1 AND FIELD_INTEGER NOT IN (2,3)", "FIELD_INTEGER NOT IN (1,2,3)");

    // Not satisfiable equality constant
    optimizeFalse("FIELD_INTEGER=1 AND FIELD_BOOLEAN_TRUE AND FIELD_INTEGER=2");
    optimizeFalse("NULL_INTEGER=1 AND FIELD_BOOLEAN_TRUE AND NULL_INTEGER=2");
  }

  @Test
  public void ILike() throws Exception {
    evalTrue("'test' ILIKE '%t%'");
    evalTrue("'test' ILIKE '%T%'");

    // Escape with other char
    evalTrue("'Result 100% value' ilike 'RESULT%100^%%' escape '^'");

    evalNull("'test' ILIKE NULL_STRING");
    evalNull("'test' ILIKE 'TEST' escape NULL_STRING");
    evalNull("NULL_STRING ILIKE '%T%'");
  }

  @Test
  public void Like() throws Exception {
    evalTrue("FIELD_STRING like 'TES%'");
    evalTrue("FIELD_STRING not like 'X%'");
    evalFalse("FIELD_STRING like 'X%'");
    evalTrue("'Tuesday' like '%es%'");
    evalTrue("'...Tuesday....' like '%es%'");

    // Test one char
    evalTrue("'A' like '_'");
    evalFalse("'AA' like '_'");

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

    // Ensure regex chars are escaped
    evalTrue("'\' LIKE '\'");
    evalTrue("'.*' LIKE '.*'");
    evalTrue("'[' LIKE '['");
    evalTrue("']' LIKE ']'");
    evalTrue("'{' LIKE '{'");
    evalTrue("'}' LIKE '}'");
    evalTrue("'?' LIKE '?'");
    evalTrue("'+' LIKE '+'");
    evalTrue("'(' LIKE '('");
    evalTrue("')' LIKE ')'");
    evalTrue("'|' LIKE '|'");
    evalTrue("'^' LIKE '^'");
    evalTrue("'$' LIKE '$'");

    // Optimizable
    evalTrue("'ABCDEFG' like 'ABCDEFG'");
    evalTrue("'ABCDEFG' like 'ABCDE%'");
    evalTrue("'ABCDEFG' like '%DEFG'");
    evalTrue("'ABCDEFG' like '%CDE%'");

    // NULL does not match NULL
    evalNull("NULL_STRING like NULL_STRING").returnType(Types.BOOLEAN);
    evalNull("NULL_STRING like 'NULL'").returnType(Types.BOOLEAN);
    evalNull("NULL_STRING LIKE '%'").returnType(Types.BOOLEAN);
    evalNull("'test' LIKE NULL_STRING").returnType(Types.BOOLEAN);
    evalNull("'a' LIKE NULL").returnType(Types.BOOLEAN);

    evalFails("'give me 30% discount' like '%30!%%' escape '!!'");
    evalFails("'test' LIKE 'TEST' escape NULL");
    evalFails("'a' LIKE '%' ESCAPE NULL");

    optimize("FIELD_STRING LIKE 'AD%D'");
    optimize("FIELD_STRING LIKE '%ADD!_%' ESCAPE '!'");
    optimize("FIELD_STRING LIKE '%'", "NVL2(FIELD_STRING,TRUE,NULL)");
    optimize("FIELD_STRING LIKE 'Hello'", "'Hello'=FIELD_STRING");
    optimize("FIELD_STRING LIKE 'H%'", "STARTSWITH(FIELD_STRING,'H')");
    optimize("FIELD_STRING LIKE 'ADD%'", "STARTSWITH(FIELD_STRING,'ADD')");
    optimize("FIELD_STRING LIKE '%o'", "ENDSWITH(FIELD_STRING,'o')");
    optimize("FIELD_STRING LIKE '%Hello%'", "CONTAINS(FIELD_STRING,'Hello')");
  }

  @Test
  public void CaseSearch() throws Exception {

    evalEquals("case when TRUE then 1 else 2 end", 1L).returnType(IntegerType.of(1));
    evalEquals("case when FALSE then 1 else 2 end", 2L).returnType(IntegerType.of(1));

    // Implicit ELSE NULL case
    evalNull("case when FIELD_INTEGER=10 then 10 end").returnType(IntegerType.of(2));
    evalEquals("case when FIELD_INTEGER=40 then 10 end", 10L).returnType(IntegerType.of(2));

    // Explicit ELSE case
    evalEquals("case when FIELD_INTEGER=40 then 10 else 50 end", 10L);
    evalEquals("case when FIELD_INTEGER>80 then 'A' else 'B' end", "B");
    evalNull("case when FIELD_INTEGER>80 then 'A' end");

    // Search CASE WHEN
    evalEquals(
        "case when FIELD_INTEGER=10+20 then 1*5 when FIELD_INTEGER=20+20 then 2*5 else 50 end",
        10L);
    evalEquals(
        "case when FIELD_INTEGER IS NULL then null when FIELD_INTEGER=20+20 then 2*5 else 50 end",
        10L);

    evalNull("case when NULL_INTEGER is NULL then NULL else 1 end").returnType(IntegerType.of(1));

    // Missing 'END'
    evalFails("case when FIELD_INTEGER=40 then 10 else 50");

    // Incompatible return type
    evalFails("case when FIELD_INTEGER=40 then 10 else 'Error'");

    // Unknown return type
    evalFails("case when false then NULL end");

    // Literal
    optimizeFalse("(CASE WHEN FALSE THEN 1 ELSE 2 END) IS NULL");

    // Implicit ELSE NULL
    optimize("CASE WHEN FIELD_INTEGER>40 THEN 10 WHEN FIELD_INTEGER>20 THEN 5 ELSE NULL END",
        "CASE WHEN 40<FIELD_INTEGER THEN 10 WHEN 20<FIELD_INTEGER THEN 5 END");
    optimize("CASE WHEN 40=FIELD_INTEGER THEN TRUE ELSE FALSE END");

    // Flatten search case
    optimize("CASE WHEN FIELD_INTEGER=1 THEN 1 ELSE CASE WHEN FIELD_NUMBER=2 THEN 2 ELSE 3 END END",
        "CASE WHEN 1=FIELD_INTEGER THEN 1 WHEN 2=FIELD_NUMBER THEN 2 ELSE 3 END");

    // "CASE WHEN x IS NULL THEN y ELSE x END" to "IFNULL(x, y)"
    optimize("CASE WHEN FIELD_STRING IS NULL THEN 'TEST' ELSE FIELD_STRING END",
        "IFNULL(FIELD_STRING,'TEST')");

    // "CASE WHEN x = y THEN NULL ELSE x END" to "NULLIF(x, y)"
    optimize("CASE WHEN FIELD_INTEGER=10 THEN NULL ELSE FIELD_INTEGER END",
        "NULLIF(FIELD_INTEGER,10)");
    optimize("CASE WHEN 0.5=FIELD_NUMBER THEN NULL ELSE FIELD_NUMBER END",
        "NULLIF(FIELD_NUMBER,0.5)");

    // "CASE WHEN x IS NOT NULL THEN y ELSE z END" to "NVL2(x, y, z)"
    optimize("CASE WHEN FIELD_INTEGER IS NOT NULL THEN FIELD_STRING ELSE 'TEST' END",
        "NVL2(FIELD_INTEGER,FIELD_STRING,'TEST')");

    // "CASE WHEN x IS NULL THEN y ELSE z END" to "NVL2(x, z, y)"
    optimize("CASE WHEN FIELD_INTEGER IS NULL THEN FIELD_STRING ELSE 'TEST' END",
        "NVL2(FIELD_INTEGER,'TEST',FIELD_STRING)");

    // Search case to simple case: CASE WHEN a = b THEN 1 END to CASE a WHEN b THEN 1 END
    // optimize("CASE WHEN FIELD_INTEGER=1 THEN 2 END", "CASE 1 WHEN FIELD_INTEGER THEN 2 END");

    evalEquals(
        "CASE WHEN FIELD_INTEGER IS NULL THEN '-' WHEN FIELD_INTEGER<0 THEN '' ELSE FIELD_STRING END",
        "TEST").returnType(StringType.of(1000));
    evalEquals("CASE WHEN NULL_INTEGER IS NULL THEN 0 ELSE FIELD_INTEGER END", 0L)
        .returnType(IntegerType.of(12));
    evalEquals("CASE WHEN NULL_INTEGER IS NULL THEN 0 ELSE FIELD_NUMBER END", 0L)
        .returnType(Types.NUMBER);
    evalEquals("CASE WHEN NULL_INTEGER IS NULL THEN Date '2023-01-01' ELSE FIELD_DATE END",
        LocalDate.of(2023, 1, 1)).returnType(Types.DATE);
  }

  @Test
  public void CaseSimple() throws Exception {

    evalEquals("case FIELD_INTEGER when 10 then 10 when 40 then 40 else 50 end", 40L)
        .returnType(IntegerType.of(2));
    evalEquals("case FIELD_INTEGER when 10 then 10 when 20 then 20 else -1 end", -1L)
        .returnType(IntegerType.of(2));
    evalNull("case FIELD_INTEGER when 10 then 10 when 20 then 20 end")
        .returnType(IntegerType.of(2));

    // If the operand is null, the else clause applies
    evalFalse("CASE NULL_INTEGER WHEN 1 THEN TRUE ELSE FALSE END").returnType(Types.BOOLEAN);
    evalEquals("CASE NULL_NUMBER WHEN 0 THEN 0 ELSE 1 END", 1L).returnType(Types.INTEGER);
    evalEquals("CASE NULL_NUMBER WHEN 0 THEN 1.023 ELSE 1 END", 1L).returnType(Types.NUMBER);
    evalEquals("CASE NULL_STRING WHEN 'A' THEN 'A' ELSE 'B' END", "B").returnType(Types.STRING);

    // Check null data type returned
    evalNull("CASE NULL_STRING WHEN 'A' THEN 'A' ELSE NULL END").returnType(Types.STRING);
        
    // Ignore division by zero in THEN term, should not evaluate
    evalEquals("CASE NULL_NUMBER WHEN 0 THEN 0/0 ELSE 1 END", 1L);
    evalEquals("CASE NULL_INTEGER WHEN 0 THEN 0 / 0 ELSE 1 END", 1L);
    evalEquals("CASE 1 WHEN 2 THEN 0 / 0 ELSE 3 END", 3L);

    // Ignore division by zero in ELSE term,
    evalEquals("CASE 1 WHEN 1 THEN 2 WHEN 1 THEN 0 / 0 END", 2L);
    evalEquals("CASE 1 WHEN 1 THEN 2 ELSE 0 / 0 END", 2L);

    // Simple case form with multi-value
    evalEquals("CASE FIELD_INTEGER WHEN 10, 20 THEN 'A' WHEN 40, 50, 60, 70 THEN 'B' ELSE 'C' END",
        "B").returnType(StringType.of(1));
    evalEquals(
        "CASE FIELD_INTEGER WHEN 10, 20.23 THEN 'A' WHEN 30, 40, 50, 60, 70 THEN 'B' ELSE 'C' END",
        "B");
    evalEquals("CASE FIELD_INTEGER WHEN 10, 20 THEN 'A' WHEN 30, 40 THEN 'B' ELSE 'C' END", "B");

    // Coerce
    evalFails("case FIELD_INTEGER when 10 then 1 when 40.1 then 2.123 else 0.5");


    // Incompatible return type
    evalFails("case FIELD_INTEGER when 10 then 'X' when ' T' then 'Test' else 'Error' end");

    // Missing 'END'
    evalFails("case FIELD_INTEGER when 40 then 10 else 50");
    evalFails("case FIELD_INTEGER then 10 else 50");


    optimize("CASE FIELD_INTEGER WHEN 40 THEN 'A' WHEN 20 THEN 'B' ELSE 'C' END");
    // Multi values
    optimize("CASE FIELD_INTEGER WHEN 1,2,3 THEN 'A' WHEN 4,5,6,7 THEN 'B' ELSE 'C' END");
  }
}


