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
import org.apache.hop.expression.type.BooleanType;
import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.junit.Test;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;

public class OperatorsTest extends ExpressionTest {

  @Test
  public void EqualTo() throws Exception {
    // Integer
    evalTrue("'0.0' = 0");
    evalTrue("0.0 = '0.000'");
    evalTrue("15.0 = '15'");
    evalTrue("'.01' = 0.01");
    evalTrue("FIELD_INTEGER = 40.0");
    
    // Number
    evalTrue("FIELD_NUMBER = -5.12");
    evalTrue("2.000 = 2");
    evalTrue("2.000 = 2.00");
    evalTrue("-1.4e-10 = -1.4e-10");
    
    // Binary
    evalTrue("0b11110000 = 0xF0");
    evalTrue("BINARY 'FF22C' = BINARY 'ff22c'");
    
    // Boolean
    evalTrue("true = true");
    evalTrue("false = false");
    evalFalse("true = false");
    evalFalse("false = true");
    evalTrue("FIELD_BOOLEAN = true");
    
    // String
    evalTrue("'ABC' = 'ABC'");
    evalFalse("'ABC' = 'abc'");
    evalTrue("FIELD_STRING = 'TEST'");
    
    // Date
    evalTrue("DATE '2019-01-01' = DATE '2019-01-01'");
    evalFalse("DATE '2019-01-01' = DATE '2018-01-01'");
    
    // Timestamp
    evalTrue("Timestamp '2019-01-01 8:00:00' AT TIME ZONE 'America/New_York' = Timestamp '2019-01-01 14:00:00' AT TIME ZONE 'Europe/Berlin'");
    evalTrue("Timestamp '2019-01-01 08:00:00 -08:00' = Timestamp '2019-01-01 11:00:00 -05:00'");
    evalTrue("Timestamp '2019-01-01 8:00:00 -08:00' = Timestamp '2019-01-01 11:00:00 -05:00'");
    evalFalse("Timestamp '2019-01-01 08:00:00 -08:00' = Timestamp '2019-01-01 8:00:00 -05:00'");
    
    // NULL is not equal ( = ) to anything not even to another NULL.
    evalNull("1 = NULL_INTEGER");
    evalFalse("NULL_BOOLEAN = true");
    evalFalse("NULL_BOOLEAN = false");  
    
    
    evalNull("NULL_BOOLEAN = NULL_BOOLEAN");
    evalNull("NULL_STRING = NULL_STRING");
    evalNull("NULL_INTEGER = NULL_INTEGER");
    evalNull("NULL_INTEGER = 1");
    evalNull("FIELD_INTEGER=NULL");
    
    evalFails("FIELD_INTEGER=");
    
    evalFails(" = FIELD_INTEGER ");
    
    optimizeNull("NULL = NULL");
    optimizeTrue("'a' = 'a'");
    optimizeFalse("'a' = 'b'");
    optimizeNull("'a' = NULL");
    optimizeNull("NULL = 'a'");
    optimizeFalse("10151082135029368 = 10151082135029369");
    optimize("FIELD_INTEGER=40","40=FIELD_INTEGER");
    optimize("FIELD_STRING = NULL", "NULL");    
    optimize("FIELD_INTEGER+1=3", "2=FIELD_INTEGER");
    optimize("FIELD_BOOLEAN = TRUE", "FIELD_BOOLEAN IS TRUE");
    optimize("TRUE = FIELD_BOOLEAN", "FIELD_BOOLEAN IS TRUE");
    optimize("FIELD_BOOLEAN = FALSE", "FIELD_BOOLEAN IS FALSE");
    optimize("FALSE = FIELD_BOOLEAN", "FIELD_BOOLEAN IS FALSE");
    
    // Simplify comparison with same term
    optimize("FIELD_STRING=FIELD_STRING", "NULL OR FIELD_STRING IS NOT NULL");
    
    returnType("FIELD_INTEGER=40", BooleanType.BOOLEAN);
  }

  @Test
  public void NotEqualTo() throws Exception {
    evalTrue("FIELD_STRING != 'foo'");
    evalTrue("FIELD_STRING <> 'tEST'");
    evalFalse("FIELD_INTEGER != 40");
    evalFalse("FIELD_INTEGER <> 40");

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
    evalTrue("DATE '2019-01-01' <> DATE '2018-01-01'");
    evalFalse("DATE '2019-01-01' <> DATE '2019-01-01'");

    evalTrue("Timestamp '2019-01-01 8:00:00' AT TIME ZONE 'UTC' <> Timestamp '2019-01-01 8:00:00' AT TIME ZONE 'US/Pacific'");
    evalFalse("Timestamp '2019-01-01 08:00:00 -8:00' <> Timestamp '2019-01-01 11:00:00 -5:00'");
    
    evalNull("NULL_STRING <> 'bar'");
    evalNull("'bar' <> NULL_STRING");
    evalNull("NULL_STRING <> NULL_STRING");

    evalFails("NOM<>");
    evalFails("NOM <> ");
    evalFails("NOM!");
    evalFails("NOM ! ");
    
    optimize("10!=FIELD_INTEGER");
    optimize("FIELD_INTEGER+1!=3", "2!=FIELD_INTEGER");
    optimize("FIELD_STRING!=FIELD_STRING", "NULL AND FIELD_STRING IS NULL");
        
    returnType("FIELD_INTEGER<>40", BooleanType.BOOLEAN);  
  }

  @Test
  public void GreaterThan() throws Exception {
    evalTrue("9>5");
    evalTrue("9.4>9.358");
    evalTrue("(4+2)>10-9");
    evalTrue("FIELD_INTEGER>10");
    evalFalse("5>5");
    evalFalse("BINARY 'F5'>BINARY 'FE'");
    
    evalFalse("false > true");
    evalFalse("false > false");
    evalFalse("true > true");
    evalTrue("true > false");

    evalFalse("'bar' > 'foo'");
    evalFalse("'foo' > 'foo'");
    evalTrue("'foo' > 'bar'");

    evalTrue("DATE '2019-02-01' > DATE '2019-01-01'");
    evalFalse("DATE '2019-01-01' > DATE '2019-01-01'");
    evalFalse("DATE '2018-01-01' > DATE '2019-01-01'");

    evalNull("NULL_BOOLEAN > 0");
    evalNull("NULL_INTEGER > 0");
    evalNull("NULL_NUMBER > 0");
    evalNull("1 > NULL_BOOLEAN");
        
    evalFails("> FIELD_INTEGER");
    evalFails("FIELD_INTEGER > ");
    evalFails("FIELD_STRING>5");
    
    optimize("10>FIELD_INTEGER");
    optimizeTrue("25>12");
    optimize("FIELD_STRING > NULL", "NULL");
    optimize("3>FIELD_INTEGER+1", "2>FIELD_INTEGER");
    optimize("FIELD_INTEGER+1>3", "2<FIELD_INTEGER");
    
    // Simplify comparison with same term
    optimize("FIELD_STRING>FIELD_STRING", "NULL AND FIELD_STRING IS NULL");
    
    
    
    returnType("'bar' > 'foo'", BooleanType.BOOLEAN);
  }

  @Test
  public void GreaterThanOrEqualTo() throws Exception {
    evalTrue("9 >= 5");
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

    evalNull("NULL_BOOLEAN >= 0");
    evalNull("1 >= NULL_BOOLEAN");
    evalNull("NULL_BOOLEAN >= NULL_INTEGER");
    
    evalFails(">=FIELD_INTEGER");
    evalFails("FIELD_INTEGER >=");
    evalFails("FIELD_STRING>=5");
    
    optimize("FIELD_INTEGER>=80","80<=FIELD_INTEGER");
    optimizeTrue("25>=12");
    optimize("FIELD_BOOLEAN >= TRUE", "FIELD_BOOLEAN IS TRUE");
    optimize("TRUE >= FIELD_BOOLEAN", "FIELD_BOOLEAN IS TRUE");
    optimize("FIELD_BOOLEAN >= FALSE", "FIELD_BOOLEAN IS FALSE");
    optimize("FALSE >= FIELD_BOOLEAN", "FIELD_BOOLEAN IS FALSE");
    optimize("FIELD_STRING >= NULL", "NULL");
    optimize("FIELD_INTEGER+1>=3", "2<=FIELD_INTEGER");
    optimize("3>=FIELD_INTEGER+1", "2>=FIELD_INTEGER");
    
    // Simplify comparison with same term
    optimize("FIELD_STRING>=FIELD_STRING", "NULL OR FIELD_STRING IS NOT NULL");
    
    returnType("'bar' >= 'foo'", BooleanType.BOOLEAN);
  }

  @Test
  public void LessThan() throws Exception {
    evalTrue("5 < 9");
    evalTrue("9.358 < 9.4");
    evalTrue("10-9 < (4+2)");
    evalTrue("FIELD_INTEGER < 100");
    evalFalse("5 < 5");

    evalFalse("true < false");
    evalTrue("false < true");
    evalFalse("false < false");
    evalFalse("true < true");

    evalTrue("'bar' < 'foo'");
    evalFalse("'foo' < 'foo'");
    evalFalse("'foo' < 'bar'");

    evalTrue("DATE '2019-01-01' < DATE '2019-02-01'");
    evalFalse("DATE '2019-01-01' < DATE '2019-01-01'");
    evalFalse("DATE '2019-01-01' < DATE '2018-01-01'");

    evalNull("NULL_INTEGER < 1");
    evalNull("0 < NULL_INTEGER");

    evalFails("< FIELD_INTEGER");
    evalFails("FIELD_INTEGER < ");
    evalFails("FIELD_STRING < 5");
    
    optimize("FIELD_INTEGER<80","80>FIELD_INTEGER");
    optimizeFalse("25<12");
    optimize("FIELD_STRING < NULL", "NULL");
    optimize("FIELD_INTEGER+1<3", "2>FIELD_INTEGER");
    optimize("3>FIELD_INTEGER+1", "2>FIELD_INTEGER");
    
    // Simplify comparison with same term   
    optimize("FIELD_STRING<FIELD_STRING", "NULL AND FIELD_STRING IS NULL");
    
    returnType("'bar' < 'foo'", BooleanType.BOOLEAN);
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

    evalTrue("'foo' <= 'foo'");
    evalTrue("'bar' <= 'foo'");
    evalFalse("'foo' <= 'bar'");

    evalTrue("DATE '2019-01-01' <= DATE '2019-02-01'");
    evalTrue("DATE '2019-01-01' <= DATE '2019-01-01'");
    evalFalse("DATE '2019-01-01' <= DATE '2018-01-01'");

    evalNull("NULL_INTEGER <= FIELD_INTEGER");
    evalNull("FIELD_INTEGER <= NULL_INTEGER");

    evalFails("<= FIELD_INTEGER");
    evalFails("FIELD_INTEGER <=");
    evalFails("FIELD_STRING <=5");
    
    optimize("FIELD_INTEGER<=5","5>=FIELD_INTEGER");
    optimizeFalse("25<=12");
    optimize("FIELD_BOOLEAN <= TRUE", "FIELD_BOOLEAN IS TRUE");
    optimize("TRUE <= FIELD_BOOLEAN", "FIELD_BOOLEAN IS TRUE");
    optimize("FIELD_BOOLEAN <= FALSE", "FIELD_BOOLEAN IS FALSE");
    optimize("FALSE <= FIELD_BOOLEAN", "FIELD_BOOLEAN IS FALSE");
    optimize("FIELD_STRING <= NULL", "NULL");    
    optimize("3>=FIELD_INTEGER+1", "2>=FIELD_INTEGER");    
    optimize("FIELD_INTEGER+1>=3", "2<=FIELD_INTEGER");
    
    // Simplify comparison with same term
    optimize("FIELD_STRING<=FIELD_STRING", "NULL OR FIELD_STRING IS NOT NULL");
    
    returnType("'bar' <= 'foo'", BooleanType.BOOLEAN);
  }

  @Test
  public void In() throws Exception {
    evalTrue("FIELD_STRING in ('?','*','TEST')");
    evalTrue("FIELD_STRING not in ('?','-','!')");
    evalTrue("FIELD_INTEGER not in (1,2,3)");
    
    evalTrue("2.5 IN (1,2.5,3)");
    evalTrue("2 in (NULL_INTEGER,1,2,FIELD_INTEGER)");
    evalTrue("TRUE IN (FALSE,NULL_BOOLEAN,TRUE)");
    evalTrue("DATE '2019-01-01' in (DATE '2019-04-01',DATE '2019-01-01',DATE '2019-03-06')");
    evalTrue("DATE '2019-01-01' in (TIMESTAMP '2019-04-01 00:00:00',DATE '2019-01-01',DATE '2019-03-06')");
    evalTrue("BINARY '0123456789' in (BINARY '9876',BINARY '0123456789',BINARY '3698')");
    evalFalse("2 in (1,2.5,3)");

    evalTrue("2 in (null,1,2,3)");
    evalFalse("2 in (NULL_INTEGER,NULL_NUMBER)");
    evalFalse("1 not in (NULL_INTEGER,1)");
    evalNull("NULL_INTEGER in (1,2,3)");
    evalNull("NULL_INTEGER in (1,2,3,NULL_INTEGER)");

    evalFails("2 in (1,2.5,)");
    evalFails("2 in ()");
   
    // TODO: assertExpressionExceptionThrownBy(() -> eval("3 in (2, 4, 3, 5 / 0)")).hasErrorCode(DIVISION_BY_ZERO);
    
    optimize("FIELD_INTEGER IN (10,20,30,40)");
    optimizeTrue("'foo' IN ('bar', 'baz', 'foo', 'blah')");
    optimizeTrue("25 in (1,25,66)");
    optimizeTrue("1.15 IN (1.1, 1.2, 1.3, 1.15)");

    optimize("FIELD_INTEGER in (1,2,1,null,null,3,4)", "FIELD_INTEGER IN (1,2,3,4)");
    optimize("FIELD_STRING in ('1','2','1',NULL,null)", "FIELD_STRING IN ('1','2')");
    

   // optimize("2 in (1,2,3/0)", "2 in (1,2,3/0)");
    
    // IN with a NULL left side expression is always NULL
    optimize("NULL in ('1','2','1',NULL,null)", "NULL");

    // Normalize IN list with single element to comparison
    optimize("FIELD_STRING in ('1','1',NULL)", "'1'=FIELD_STRING");
    optimize("0 / 0 in (2, 2)", "2=0/0");
    
    // Value in the expression list
    optimize("FIELD_INTEGER in (1,2,FIELD_INTEGER)", "NULL OR FIELD_INTEGER IS NOT NULL");
    optimize("FIELD_STRING in ('XX',FIELD_STRING,'ZZ')", "NULL OR FIELD_STRING IS NOT NULL");
    
    returnType("FIELD_INTEGER IN (10,20,30,40)", BooleanType.BOOLEAN);
  }

  @Test
  public void IsTrue() throws Exception {
    evalTrue("True IS True");
    evalTrue("True IS NOT False");
    evalTrue("FIELD_BOOLEAN is True");
    evalFalse("NULL_BOOLEAN IS True");  
    evalTrue("False IS NOT TRUE");      
    evalFalse("NULL_BOOLEAN is True");
    evalTrue("NULL_BOOLEAN IS NOT True");
    
    evalFails("NOM IS ");
    evalFails("IS TRUE");
    evalFails("IS NOT TRUE");
    
    optimize("FIELD_BOOLEAN IS TRUE");
    optimize("FIELD_BOOLEAN IS NOT TRUE");
    optimizeTrue("true is true");
    optimizeTrue("false is not true");

    
    returnType("FIELD_BOOLEAN IS TRUE", BooleanType.BOOLEAN);
    returnType("FIELD_BOOLEAN IS NOT TRUE", BooleanType.BOOLEAN);
  }

  @Test
  public void IsFalse() throws Exception {
    evalTrue("True IS NOT False");
    evalFalse("True IS False");
    evalTrue("False IS False");
    evalFalse("NULL_BOOLEAN IS False");        
    evalTrue("NULL_BOOLEAN IS NOT False");

    evalFails("IS FALSE");
    evalFails("IS NOT FALSE");
    
    optimize("FIELD_BOOLEAN IS FALSE");
    optimize("FIELD_BOOLEAN IS NOT FALSE");
    optimizeTrue("false is false");
    optimizeFalse("true is false");
    optimizeTrue("true is not false");
    
    returnType("FIELD_BOOLEAN IS FALSE", BooleanType.BOOLEAN);
    returnType("FIELD_BOOLEAN IS NOT FALSE", BooleanType.BOOLEAN);
  }

  @Test
  public void IsNull() throws Exception {
    evalFalse("True IS Null");
    evalFalse("False IS Null");
    evalFalse("NULL_BOOLEAN IS NOT NULL");
    evalTrue("NULL_INTEGER IS NULL");
    evalTrue("NULL_STRING IS NULL");
    evalTrue("NULL_BOOLEAN IS NULL");

    evalFails("IS NULL");
    evalFails("IS NOT NULL");
    
    optimize("FIELD_BOOLEAN IS NULL");
    optimize("FIELD_BOOLEAN IS NOT NULL");
    optimizeFalse("true is null");
    optimizeFalse("false is null");
    optimizeTrue("NULL IS NULL");
    optimizeTrue("NULL+1 IS NULL");
    optimizeFalse("NULL+1 IS NOT NULL");
    optimizeTrue("TRUE IS NOT NULL");
    optimizeFalse("TRUE IS NULL");
    optimizeFalse("1 IS NULL");
    optimizeTrue("1 IS NOT NULL");
    optimizeFalse("'a' IS NULL");
    optimizeTrue("'a' IS NOT NULL");
        
    // 'CAST(x AS type) IS NOT NULL' to 'x IS NOT NULL'
    optimize("CAST(FIELD_NUMBER AS STRING) IS NOT NULL","FIELD_NUMBER IS NOT NULL");
    
    returnType("FIELD_BOOLEAN IS NULL", BooleanType.BOOLEAN);
    returnType("FIELD_BOOLEAN IS NOT NULL", BooleanType.BOOLEAN);
  }
  
  @Test
  public void IsDistinctFrom() throws Exception {
    evalTrue("1 IS DISTINCT FROM 2");
    evalFalse("1 IS DISTINCT FROM 1");
    evalTrue("FIELD_INTEGER IS DISTINCT FROM 1");
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

    optimize("FIELD_BOOLEAN IS DISTINCT FROM TRUE");
    optimize("FIELD_BOOLEAN IS NOT DISTINCT FROM TRUE");
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
    
    returnType("FIELD_BOOLEAN IS DISTINCT FROM TRUE", BooleanType.BOOLEAN);
    returnType("FIELD_BOOLEAN IS NOT DISTINCT FROM TRUE", BooleanType.BOOLEAN);
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
    
    //evalTrue("'Erdbeere' SIMILAR TO 'Erd[a[:SPACE:]b]eere'");
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
    
    optimize("FIELD_STRING SIMILAR TO 'a{2,4}'");
  }
  
  @Test
  public void Add() throws Exception {
    evalEquals("10+(-0.5)", 9.5);
    evalEquals("BINARY 'F'::INTEGER+1", 16L);
    evalEquals("0b00011::INTEGER+0", 3L);
    evalEquals("-24.7+0.5+24.7+0.5E-2", 0.505);
    evalEquals("FIELD_INTEGER+FIELD_NUMBER+FIELD_BIGNUMBER", 123491.669);
    evalEquals("FIELD_BIGNUMBER+1", 123456.789 + 1);

    evalEquals("TRUE+FALSE", 1L);
    
    // Addition of days to DATE or TIMESTAMP
    evalEquals("DATE '2019-02-25'+1", LocalDate.of(2019, 2, 26));    
    evalEquals("DATE '2019-02-25'+2", LocalDate.of(2019, 2, 27));
    evalEquals("Timestamp '2019-02-25'+2", LocalDate.of(2019, 2, 27));
    
    // Only integer, round number
    evalEquals("DATE '2019-02-25'+1.8", LocalDateTime.of(2019, 2, 26, 0, 0, 0));
    evalEquals("DATE '2019-02-25'+5/(60*24)", LocalDateTime.of(2019, 2, 25, 0, 0, 0));

    evalNull("5+NULL_INTEGER+5");
    evalNull("+NULL_INTEGER+5");
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
    
    returnType("FIELD_NUMBER+FIELD_INTEGER", new NumberType(20));
    returnType("1::NUMBER(4,1)+3::NUMBER(1,2)", new NumberType(6,2));
    returnType("1::NUMBER(38,10)+3::NUMBER(38,5)", new NumberType(38,10));
    returnType("1::NUMBER(14,2)+3::NUMBER(14,2)", new NumberType(15,2));
  }

  @Test
  public void Subtract() throws Exception {
    evalEquals("10-0.5", 9.5D);
    evalEquals("FIELD_INTEGER-0.5", 39.5D);
    evalEquals("FIELD_INTEGER-10::INTEGER", 30L);

    evalEquals("TRUE-FALSE", 1L);
    
    // Subtraction of days to DATE or TIMESTAMP
    evalEquals("DATE '2019-02-25'-1", LocalDate.of(2019, 2, 24));
    evalEquals("DATE '2019-02-25'-28", LocalDate.of(2019, 1, 28));
    evalEquals("Timestamp '2019-02-25'-2", LocalDate.of(2019, 2, 23));
    
    //evalEquals("DATE '2019-02-25'-0.5", LocalDateTime.of(2019, 2, 24, 12, 0, 0));
    //evalEquals("DATE '2019-02-25'-5/(60*24)", LocalDateTime.of(2019, 2, 24, 23, 55, 0));

    // TODO: Diff of two date
    //evalEquals("DATE '2019-02-25'-DATE '2019-02-23'", 2);
    //evalEquals("DATE '2019-02-23'-DATE '2019-02-25'", -2);
    //evalEquals("DATE '2019-02-25'-to_Date('2019-02-23 12:00','YYYY-MM-DD HH24:MI')", 1.5);

    evalNull("5-NULL_INTEGER");
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
    evalTrue("3 between 1 and 5");
    evalTrue("3 between 3 and 5");
    evalTrue("5 between 3 and 5");
    evalFalse("5 between 5 and 3");
    evalTrue("FIELD_INTEGER between symmetric 30 and 50");
    evalTrue("FIELD_INTEGER between symmetric 50 and 30");
    evalTrue("FIELD_INTEGER between -3+27 and 50");
    evalTrue("'the' between 'that' and 'then'");
    evalFalse("1 between 3 and 5");
    evalTrue("FIELD_INTEGER between 39.999 and 40.0001");
    evalTrue("FIELD_INTEGER not between 10 and 20");
    evalTrue("FIELD_INTEGER not between 10 and 20 and 'Test' is not null");

    evalTrue("DATE '2019-02-28' between DATE '2019-01-01' and DATE '2019-12-31'");

    evalNull("NULL_INTEGER between -10 and 20");
    evalNull("NULL_INTEGER between symmetric -10 and 20");
    evalNull("1 between NULL_INTEGER and 20");    
    evalNull("1 between symmetric NULL_INTEGER and 20");
    evalNull("1 between -10 and NULL_INTEGER");
    evalNull("1 between symmetric -10 and NULL_INTEGER");

    evalFails("FIELD_INTEGER between 10 and");
    evalFails("FIELD_INTEGER between and 10");
    evalFails("FIELD_INTEGER between and ");
    evalFails("FIELD_INTEGER between FIELD_DATE and FIELD_STRING");

    optimize("FIELD_INTEGER BETWEEN 10 AND 20");
    optimize("FIELD_NUMBER BETWEEN SYMMETRIC 50 AND 20");
    optimize("FIELD_STRING BETWEEN 'AZE' AND 'KLM'");
    optimize("FIELD_INTEGER between 3 and (5+1)", "FIELD_INTEGER BETWEEN 3 AND 6");    
    optimizeFalse("2 between 3 and (5+1)");
    optimizeTrue("25.8 between 18 and 32");
    optimizeTrue("DATE '2019-02-28' between DATE '2019-01-01' and DATE '2019-12-31'");
    
    returnType("5 between 3 and 5", BooleanType.BOOLEAN);
  }

  @Test
  public void CastToBoolean() throws Exception {

    // Boolean
    evalTrue("CAST(TRUE as Boolean)");
    evalFalse("CAST(FALSE as Boolean)");
    
    // String
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

    // Integer
    evalTrue("CAST(1 as Boolean)");
    evalTrue("CAST(-123 as Boolean)");
    evalTrue("1::Boolean");
    evalFalse("CAST(0 as Boolean)");
    evalFalse("CAST(-0 as Boolean)");
    
    // Number
    evalTrue("CAST(-12.1 as Boolean)");
    evalFalse("CAST(0.000 as Boolean)");
    evalTrue("CAST(12345678901234567890123456789012345678 as Boolean)");
   
    // Null
    evalNull("CAST(NULL as Boolean)");
    evalNull("CAST(NULL_BOOLEAN as Boolean)");

    // Unsupported conversion    
    evalFails("CAST(DATE '2019-02-25' AS BOOLEAN)");
    
    // Remove lossless cast
    optimize("CAST(FIELD_BOOLEAN AS BOOLEAN)", "FIELD_BOOLEAN");
        
    //optimize("CAST(FIELD_STRING AS BOOLEAN)", "TO_BOOLEAN(FIELD_STRING)");
    //optimize("FIELD_STRING::BOOLEAN", "TO_BOOLEAN(FIELD_STRING)");
    
    // Return type
    returnType("CAST(3 as BOOLEAN)", BooleanType.BOOLEAN);
  }

  
  @Test
  public void CastToInteger() throws Exception {
    // Integer
    evalEquals("CAST(0 as Integer)", 0L);
    evalEquals("CAST(123 as Integer)", 123L);
    evalEquals("CAST(-123 as Integer)", -123L);
    
    // Number
    evalEquals("CAST(1.25 as Integer)", 1L);
    evalEquals("CAST(-1.25 as Integer)", -1L);
    // Oracle truncate to 1 and Snowflake round it to 2
    evalEquals("CAST(1.75 as Integer)", 1L);
    
    // Boolean
    evalEquals("CAST(TRUE as Integer)", 1L);
    evalEquals("CAST(FALSE as Integer)", 0L);
    
    // String
    evalEquals("CAST('1234' as Integer)", 1234L);
    evalEquals("'1234'::Integer+5", 1239L);
    evalEquals("CAST('1234.567' as Integer)", 1234L);
    
    // Binary
    evalEquals("CAST(BINARY '123' as Integer)", 291L);
    
    // Null
    evalNull("CAST(NULL as Integer)");
    evalNull("CAST(NULL_INTEGER as Integer)");
    
    // Accept DataType quoted like a String
    evalEquals("Cast(123 as 'INTEGER')", 123L);
    
    // Unsupported conversion
    evalFails("CAST(DATE '2019-02-25' AS INTEGER)");
    
    optimize("CAST(FIELD_INTEGER AS INTEGER)", "FIELD_INTEGER");
  
    // Return type
    returnType("CAST('3' as INTEGER)", new IntegerType());
    returnType("CAST(FIELD_NUMBER as INTEGER)", new IntegerType());
  }

  @Test
  public void CastToNumber() throws Exception {
    
    // Boolean
    evalEquals("CAST(TRUE as Number)", 1L);
    evalEquals("CAST(FALSE as Number)", 0L);
    
    // Integer
    evalEquals("CAST(0 as Number)", 0L);
    evalEquals("CAST(123 as Number)", 123L);
    evalEquals("CAST(-123 as Number)", -123L);
    
    // Number
    evalEquals("CAST(1234.456 as Number)", 1234.456D);
    evalEquals("CAST(1234.456 as Number(10,1))", 1234.4D);    
    evalEquals("1.23456::Number", 1.23456);
    evalEquals("1.23456::Number(10)", 1.23456);
    evalEquals("1.23456::Number(10,2)", 1.23);
    evalEquals("1.23456::Number(10,0)", 1L);
    evalEquals("1.23456::Number(10,6)", new BigDecimal("1.234560"));
    evalEquals("CAST(12345678901234567890123456789012345678 as Number)",
        new BigDecimal("12345678901234567890123456789012345678"));
    
    // String
    evalEquals("CAST('0' as Number)", 0L);
    evalEquals("CAST('1' As Number)", 1L);
    evalEquals("CAST('-1e-37' as Number)", -1e-37d);
    evalEquals("CAST(' -1e-37 ' as Number)", -1e-37d);
    evalEquals("'1'::Number", 1L);
    evalEquals("'1234'::Number", 1234L);
    evalEquals("CAST('1234' as Number)", 1234L);
    evalEquals("CAST('1234.567' as Number)", 1234.567d);
    evalEquals("CAST('  -1e-37  ' as Number)", -1e-37d);
    
    // Null
    evalNull("CAST(NULL as Number)");
    evalNull("CAST(NULL_NUMBER as Number)");
    evalNull("CAST(NULL_BIGNUMBER as Number)");
    
    // Unsupported conversion
    evalFails("CAST(DATE '2019-02-25' AS NUMBER)");
    
    optimize("CAST(FIELD_INTEGER AS NUMBER)", "CAST(FIELD_INTEGER AS NUMBER)");
    optimize("FIELD_INTEGER::NUMBER", "CAST(FIELD_INTEGER AS NUMBER)");

    
    // TODO: Chained cast
    // optimize("CAST(CAST(CAST(123456 AS INTEGER) AS NUMBER) AS NUMBER)", "123456");

    
    // Return type
    returnType("CAST(FIELD_STRING as NUMBER(5))", new NumberType(5));
    returnType("CAST(FIELD_NUMBER as NUMBER(5,0))", new NumberType(5,0));
    returnType("CAST('3.123' as NUMBER(10,2))", new NumberType(10, 2));
  }

  @Test
  public void CastToString() throws Exception {
    // Boolean
    evalEquals("CAST(true as String)", "TRUE");
    evalEquals("CAST(false as String)", "FALSE");  
    evalEquals("true::String", "TRUE");
    
    // Integer
    evalEquals("CAST(12923 AS STRING)", "12923");
    evalEquals("CAST(-1234 AS STRING FORMAT '9999MI')", "1234-");

    // Number
    evalEquals("CAST(123.6 as String)", "123.6");
    evalEquals("123.6::String", "123.6");
    evalEquals("CAST(0.45 AS STRING)", "0.45");
    evalEquals("CAST(0.45 AS STRING FORMAT 'FM000.00')", "000.45");
    evalEquals("CAST(1234.56 AS STRING FORMAT '9999MI')", "1234 ");
    evalEquals("CAST(12345678901234567890.123 AS STRING)", "12345678901234567890.123");
    
    // Date
    evalEquals("CAST(DATE '2019-02-25' AS String)", "2019-02-25");
    evalEquals("CAST(DATE '2019-02-25' AS String FORMAT 'DD/MM/YYYY')", "25/02/2019");

    // String
    evalEquals("CAST('abcdefg' AS String)", "abcdefg");
    evalEquals("CAST('abcdefg' AS String(3))", "abc");
    evalEquals("CAST('abcdefg' AS String(20))", "abcdefg");
    
    // Null
    evalNull("CAST(NULL as STRING)");
    evalNull("CAST(NULL_STRING as String)");
            
    optimize("CAST(FIELD_STRING AS STRING)", "FIELD_STRING");
    optimize("CAST(TRUE AS STRING)", "'TRUE'");
    optimize("CAST(FALSE AS STRING(5))", "'FALSE'");
    optimize("CAST(123456 AS STRING(6))", "'123456'");
    optimize("CAST(123456.1 AS STRING(8))", "'123456.1'");
    optimize("CAST(DATE '2013-02-02' AS STRING(10))", "'2013-02-02'"); 
    
    // Cast from data type to string fails because the result value does not fit in the type.
    // So the expression is not modified    
    optimize("CAST(FALSE AS STRING(3))", "CAST(FALSE AS STRING(3))");
    optimize("CAST(123456 AS STRING(3))", "CAST(123456 AS STRING(3))");
    optimize("CAST(123456.1 AS STRING(3))", "CAST(123456.1 AS STRING(3))");
    optimize("CAST(DATE '2013-02-02' AS STRING(3))", "CAST(DATE '2013-02-02' AS STRING(3))");    
  }
  
  @Test
  public void CastToDate() throws Exception {
    // String
    evalEquals("CAST('2020-march' as DATE FORMAT 'YYYY-MONTH')", LocalDate.of(2020, 3, 1));
    evalEquals("CAST('2020-01-19 11:23:44' as DATE FORMAT 'YYYY-MM-DD HH:MI:SS')", LocalDateTime.of(2020, 1, 19, 11,23,44));
    
    // Null
    evalNull("CAST(NULL as Date)");
    evalNull("CAST(NULL_DATE as Date)");
    
    // Erreur parsing format
    evalFails("CAST('2023-01-01' AS DATE FORMAT 'YYYY-MM')");
    evalFails("CAST('2023-01' AS DATE FORMAT 'YYYY-MM-DD')");
    
    // Unsupported conversion    
    evalFails("CAST(TRUE AS DATE)");
    
    // optimize("CAST(FIELD_STRING AS DATE)", "TO_DATE(FIELD_STRING)");
    // optimize("CAST(FIELD_STRING AS DATE FORMAT 'YYYY-MM-DD')", "TO_DATE(FIELD_STRING,'YYYY-MM-DD')");   
    optimize("CAST(TO_CHAR(FIELD_DATE,'YYYYMMDD') AS DATE FORMAT 'YYYYMMDD')", "CAST(TO_CHAR(FIELD_DATE,'YYYYMMDD') AS DATE FORMAT 'YYYYMMDD')");
    optimize("Cast('2021-02-08' as DATE)", "DATE '2021-02-08'");
    optimize("'2021-02-08'::DATE", "DATE '2021-02-08'");
    

    // Return type
    returnType("CAST(DATE '2019-02-25' AS Date FORMAT 'YYY-MM-DD')", DateType.DATE);
  }

  @Test
  public void CastToBinary() throws Exception {    
    // String
    evalEquals("CAST('AB' as BINARY)", "AB".getBytes());
    
    // Null
    evalNull("CAST(NULL as Binary)");
    evalNull("CAST(NULL_BINARY as Binary)");
    
    optimize("CAST(FIELD_STRING AS BINARY)", "CAST(FIELD_STRING AS BINARY)");
  }

  @Test
  public void CastToJson() throws Exception {    
    // String
    //evalEquals("CAST('A' as JSON)", "A".getBytes());
    
    // Null
    evalNull("CAST(NULL as Json)");
    evalNull("CAST(NULL_JSON as Json)");
    
    optimize("CAST(FIELD_JSON AS JSON)", "FIELD_JSON");
    // optimize("CAST(FIELD_STRING AS JSON)", "TO_JSON(FIELD_STRING)");
    // optimize("FIELD_STRING::JSON", "TO_JSON(FIELD_STRING)");
  }

  
  @Test
  public void Cast() throws Exception {

    evalEquals("TO_NUMBER('123','000')::INTEGER+1", 124L);

    // Accept DataType quoted like a String
    evalEquals("Cast(123 as 'INTEGER')", 123L);
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
    
    // Unknown data type
    evalFails("Cast(123 as Nill)");
    evalFails("Cast(123 as 1)");
    evalFails("Cast(123 as TRUE)");
    evalFails("CAST('bad' AS NULL)");
    evalFails("Cast(123 as 'Text')");
  }

  @Test
  public void AtTimeZone() throws Exception {
    evalEquals("TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE 'Europe/Paris'", ZonedDateTime.of(2023, 5, 25, 20,48,00,0,ZoneId.of("Europe/Paris")));
    evalEquals("TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE 'Singapore'", ZonedDateTime.of(2023, 5, 25, 20,48,00,0,ZoneId.of("Singapore")));
    evalEquals("TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE 'GMT+0'", ZonedDateTime.of(2023, 5, 25, 20,48,00,0,ZoneId.of("GMT+0")));
    evalEquals("TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE 'CET'", ZonedDateTime.of(2023, 5, 25, 20,48,00,0,ZoneId.of("CET")));
    evalEquals("TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE 'EET'", ZonedDateTime.of(2023, 5, 25, 20,48,00,0,ZoneId.of("EET")));
    evalEquals("(TIMESTAMP '2023-05-25 10:48:00' AT TIME ZONE 'UTC') AT TIME ZONE 'Asia/Singapore'", ZonedDateTime.of(2023, 5, 25, 10,48,00,0,ZoneId.of("Asia/Singapore")));    
    evalFails("TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE 'XYZ'");
 
    optimize("TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE 'Europe/Paris'");
  }
  
  @Test
  public void ConvertTimeZone() throws Exception {
    evalEquals("CONVERT_TIMEZONE('Europe/Paris',TIMESTAMP '2020-05-25 20:48:00')", ZonedDateTime.of(2020, 5, 25, 22,48,00,0,ZoneId.of("Europe/Paris")));
    //evalEquals("CONVERT_TIMEZONE('Asia/Singapore',TIMESTAMP '2020-05-25 20:48:00' AT TIME ZONE 'UTC')", ZonedDateTime.of(2020, 5, 26, 18,48,00,0,ZoneId.of("Asia/Singapore")));
    
    optimize("CONVERT_TIMEZONE('Europe/Paris',FIELD_TIMESTAMP)");
    optimize("CONVERT_TIMEZONE('Europe/Paris',TIMESTAMP '2020-05-25 20:48:00')","TIMESTAMP '2020-05-25 22:48:00' AT TIME ZONE 'Europe/Paris'");
  }
  
  @Test
  public void Positive() throws Exception {
    evalEquals("+(40)", 40L);
    evalEquals("+(FIELD_INTEGER)", 40L);
    evalEquals("+40", 40L);
    evalEquals("1+ +2", 3L);
    evalNull("+null");
    
    optimize("+FIELD_INTEGER","FIELD_INTEGER");
    
    returnType("+FIELD_INTEGER", IntegerType.INTEGER);
    returnType("+FIELD_NUMBER", NumberType.NUMBER);
    returnType("+FIELD_BIGNUMBER", NumberType.NUMBER);
  }

  @Test
  public void Negative() throws Exception {
    evalEquals("-(1+2)", -3L);
    evalEquals("-FIELD_INTEGER", -40L);
    evalEquals("-FIELD_INTEGER", -40L);
    evalEquals("+40", 40L);
    evalEquals("1+ -2", -1L);
    evalNull("-NULL_INTEGER");
    
    optimize("-FIELD_INTEGER","-FIELD_INTEGER");
    optimize("-(10+2)", "-12");
    optimize("-(1.1+1.2)", "-2.3");
    optimize("-(0)", "0");

    optimize("+(0 / 0)", "0/0");
    //optimize("-(0 / 0)", "-(0/0)");
    optimize("-(-(0 / 0))", "0/0");
    
    optimize("-RANDOM()", "-RANDOM()");
    optimize("+RANDOM()", "RANDOM()");
    
    // Negative    
    optimize("-(-FIELD_INTEGER)", "FIELD_INTEGER");
    optimize("-(FIELD_INTEGER-FIELD_NUMBER)", "FIELD_NUMBER-FIELD_INTEGER");
    
    returnType("-FIELD_INTEGER", IntegerType.INTEGER);
    returnType("-FIELD_NUMBER", NumberType.NUMBER);
    returnType("-FIELD_BIGNUMBER", NumberType.NUMBER);
  }

  @Test
  public void Mod() throws Exception {
    evalEquals("15%4", 3L);
    evalEquals("Mod(15,4)", 3L);
    evalEquals("Mod(15.3,4)", 3.3D);
    evalEquals("Mod(15.3::NUMBER,4)", 3.3);
    evalNull("Mod(NULL_INTEGER,2)");
    evalNull("Mod(2,NULL_INTEGER)");
    evalFails("'TEST'%5");
    evalFails("Mod()");
    evalFails("Mod(9,0)");
    evalFails("Mod(3)");
    
    optimize("FIELD_INTEGER%4");
  }
  
  @Test
  public void Multiplication() throws Exception {
    evalEquals("2.55*10", 25.50D);
    evalEquals("4*10", 40D);
    evalEquals("-4*-1", 4D);
    evalEquals("2*-2", -4D);
    evalEquals("100 * .5", 50D);    
    evalEquals("1.23456::Number*-2.987654", -3.68843812224);
    
    // Check no overflow Long.MAX_VALUE * 2
    evalEquals("9223372036854775807*2", new BigDecimal("18446744073709551614"));
    // Check no underflow Long.MIN_VALUE * 2
    evalEquals("-9223372036854775808*2", new BigDecimal("-18446744073709551616"));
        
    evalNull("NULL_INTEGER*1");
    evalNull("1*NULL_INTEGER");

    optimize("FIELD_INTEGER*4","4*FIELD_INTEGER");
    optimize("FIELD_INTEGER*1", "FIELD_INTEGER");
    optimize("FIELD_INTEGER*3*2", "6*FIELD_INTEGER");
    optimize("3*(FIELD_INTEGER*1)*1*(2*5)", "30*FIELD_INTEGER");
    optimize("1.0*FIELD_INTEGER", "FIELD_INTEGER");
    optimize("4*FIELD_INTEGER*0.5", "2*FIELD_INTEGER");
    optimize("-FIELD_INTEGER*(-FIELD_NUMBER)", "FIELD_INTEGER*FIELD_NUMBER");
    optimize("FIELD_INTEGER*FIELD_INTEGER", "SQUARE(FIELD_INTEGER)");

    returnType("FIELD_NUMBER::NUMBER(4,1)*3::NUMBER(1,2)", new NumberType(5,3));
    returnType("FIELD_NUMBER::NUMBER(38,1)*3::NUMBER(1,2)", new NumberType(38,3));
  }

  @Test
  public void Div() throws Exception {
    evalEquals("10/4", 2.5D);
    evalEquals("40/-10", -4L);
    evalEquals("-40/-10", 4L);
    evalEquals("5/2", 2.5D);
    evalEquals("10.1/2.1",  new BigDecimal("4.809523809523809523809523809523810"));
    evalEquals("0.1/0.0000000000001", 1000000000000L);
    evalNull("NULL_INTEGER/1");
    evalNull("NULL_INTEGER/0");
    evalNull("1/NULL_INTEGER");
    evalFails("40/0");
        
    optimize("0/0", "0/0");
    optimize("FIELD_INTEGER/4");
    optimize("FIELD_INTEGER/1", "FIELD_INTEGER");    
    optimize("FIELD_INTEGER/1.0", "FIELD_INTEGER");
    optimize("-FIELD_NUMBER/-FIELD_INTEGER", "FIELD_NUMBER/FIELD_INTEGER");
    
    returnType("FIELD_INTEGER/4", NumberType.NUMBER);
    returnType("FIELD_NUMBER::NUMBER(4,1)/3::NUMBER(1,2)", new NumberType(6,2));
  }

  @Test
  public void Div0() throws Exception {
    evalEquals("Div0(10,4)", 2.5D);
    evalEquals("Div0(FIELD_INTEGER,-100)", -0.4D);    
    evalEquals("Div0(FIELD_INTEGER,0)", 0L);
    evalNull("Div0(NULL_INTEGER,1)");
    evalNull("Div0(NULL_INTEGER,0)");
    evalNull("Div0(1,NULL_INTEGER)");
    evalFails("Div0(40)");
        
    optimize("DIV0(FIELD_INTEGER,1)", "FIELD_INTEGER");
    optimize("DIV0(-FIELD_NUMBER,-FIELD_INTEGER)", "DIV0(FIELD_NUMBER,FIELD_INTEGER)");
    
    returnType("Div0(FIELD_INTEGER,2)", NumberType.NUMBER);
  }
  
  @Test
  public void BitNot() throws Exception {
    evalEquals("~1", -2L);
    evalEquals("~ 1", -2L);
    evalEquals("~0", -1L);
    evalEquals("~4", -5L);
    evalEquals("~65504", -65505L);
    evalNull("~NULL_INTEGER");
    evalFails("~");
    evalFails("~ ");

    // Alias
    evalEquals("BIT_NOT(1)", -2L);
    
    optimize("~FIELD_INTEGER");
    optimize("~(~FIELD_INTEGER)", "FIELD_INTEGER");
    
    returnType("~FIELD_INTEGER", IntegerType.INTEGER);
  }

  @Test
  public void BitAnd() throws Exception {
    evalEquals("BIT_AND(3,2)", 2L);
    evalEquals("3 & 2", 2L);
    evalEquals("100 & 2", 0L);
    evalNull("100 & NULL_INTEGER");
    evalNull("NULL_INTEGER & 100");
    evalFails("100&");
    evalFails("100 & ");
    
    optimize("FIELD_INTEGER&4");
    
    returnType("FIELD_INTEGER&4", IntegerType.INTEGER);
  }

  @Test
  public void BitOr() throws Exception {
    evalEquals("BIT_OR(100,2)", 102L);
    evalEquals("100 | 2", 102L);
    evalEquals("3 | 2", 3L);
    evalNull("100 | NULL_INTEGER");
    evalNull("NULL_INTEGER | 100");
    evalFails("3|");
    evalFails("3 | ");
    
    optimize("FIELD_INTEGER|4");
    
    returnType("FIELD_INTEGER|4", IntegerType.INTEGER);
  }

  @Test
  public void BitXor() throws Exception {
    evalEquals("BIT_XOR(2,2)", 0L);
    evalEquals("2 ^ 1", 3L);
    evalEquals("100 ^ 2", 102L);
    evalNull("100 ^ NULL_INTEGER");
    evalNull("NULL_INTEGER ^ 100");
    evalFails("100^");
    evalFails("100 ^ ");
    
    optimize("FIELD_INTEGER^4");
    
    returnType("FIELD_INTEGER^4", IntegerType.INTEGER);
  }

  @Test
  public void BoolNot() throws Exception {
    evalTrue("FIELD_BOOLEAN is not false");
    evalTrue("NULL_BOOLEAN is null");
    evalTrue("NOT (NULL_BOOLEAN is not null)");
    evalFalse("NOT 1");
    evalTrue("NOT 0");
    
    evalTrue("NOT NOT True");
    evalNull("NOT NULL_BOOLEAN");
    evalFails("FIELD_BOOLEAN is ");
    evalFails("NOT");
    
    // Function syntax
    evalTrue("NOT(FALSE)");
    
    optimize("NOT FIELD_BOOLEAN");
    optimize("NOT NOT FIELD_BOOLEAN","FIELD_BOOLEAN");
    optimize("NOT NOT NOT FIELD_BOOLEAN","NOT FIELD_BOOLEAN");
    optimizeFalse("not true");
    optimizeTrue("not false");
    optimizeTrue("not not true");
    optimizeFalse("not not false");
    optimize("NOT (NOT(FIELD_BOOLEAN))", "FIELD_BOOLEAN");
    optimize("NOT (FIELD_INTEGER>5)", "5>=FIELD_INTEGER");
    optimize("NOT (FIELD_INTEGER>=5)", "5>FIELD_INTEGER");
    optimize("NOT (FIELD_INTEGER<5)", "5<=FIELD_INTEGER");
    optimize("NOT (FIELD_INTEGER<=5)", "5<FIELD_INTEGER");
    optimize("NOT (FIELD_INTEGER=5)", "5!=FIELD_INTEGER");
    optimize("NOT (FIELD_INTEGER<>5)", "5=FIELD_INTEGER");
    optimize("NOT (FIELD_BOOLEAN IS TRUE)", "FIELD_BOOLEAN IS NOT TRUE");
    optimize("NOT (FIELD_BOOLEAN IS NOT TRUE)", "FIELD_BOOLEAN IS TRUE");
    optimize("NOT (FIELD_BOOLEAN IS FALSE)", "FIELD_BOOLEAN IS NOT FALSE");
    optimize("NOT (FIELD_BOOLEAN IS NOT FALSE)", "FIELD_BOOLEAN IS FALSE");
    optimize("NOT (FIELD_BOOLEAN IS NOT NULL)", "FIELD_BOOLEAN IS NULL");
    optimize("NOT (FIELD_BOOLEAN IS NULL)", "FIELD_BOOLEAN IS NOT NULL");
    optimize("NOT (FIELD_BOOLEAN IS DISTINCT FROM NULL_BOOLEAN)",
        "FIELD_BOOLEAN IS NOT DISTINCT FROM NULL_BOOLEAN");
    optimize("NOT (FIELD_BOOLEAN IS NOT DISTINCT FROM NULL_BOOLEAN)",
        "FIELD_BOOLEAN IS DISTINCT FROM NULL_BOOLEAN");
    // optimize("(A IS NOT NULL OR B) AND FIELD_BOOLEAN IS NOT NULL","FIELD_BOOLEAN IS NOT NULL");
    
    returnType("NOT FIELD_BOOLEAN", BooleanType.BOOLEAN);
  }

  @Test
  public void BoolOr() throws Exception {
    evalTrue("true OR true");
    evalTrue("true OR false");
    evalTrue("false OR true");
    evalFalse("false OR false");
    evalTrue("true OR NULL_BOOLEAN");
    evalTrue("true OR FIELD_BOOLEAN");
    evalTrue("NULL_BOOLEAN OR true");
    evalTrue("FIELD_BOOLEAN OR false");
    evalNull("false OR NULL_BOOLEAN");
    evalNull("NULL_BOOLEAN OR false");
    evalNull("NULL_BOOLEAN OR NULL_BOOLEAN");
    
    evalFails("false OR");
    evalFails("OR false");
    
    optimize("FIELD_BOOLEAN OR NULL_BOOLEAN");
    optimizeTrue("true or true");
    optimizeTrue("true or false");
    optimizeTrue("false or true");
    optimizeFalse("false or false");
    optimizeTrue("true or NULL_BOOLEAN");
    optimizeTrue("NULL_BOOLEAN or true");
    optimize("NULL_BOOLEAN or NULL_BOOLEAN", "NULL_BOOLEAN");

    optimizeTrue("FIELD_BOOLEAN or true");
    optimizeTrue("true or FIELD_STRING");
    optimizeTrue("true or FIELD_BOOLEAN");
    optimizeTrue("FIELD_BOOLEAN or true");
    optimize("false or FIELD_BOOLEAN", "FALSE OR FIELD_BOOLEAN");
    optimize("FIELD_BOOLEAN or false", "FALSE OR FIELD_BOOLEAN");
    optimize("FIELD_BOOLEAN or FIELD_BOOLEAN", "FIELD_BOOLEAN");
    optimize("FIELD_BOOLEAN OR NULL_BOOLEAN OR (FIELD_INTEGER>0) OR FIELD_BOOLEAN",
        "FIELD_BOOLEAN OR NULL_BOOLEAN OR 0<FIELD_INTEGER");

    optimize("false and true or FIELD_BOOLEAN", "FALSE OR FIELD_BOOLEAN");
    
    // Duplicate predicate
    optimize("FIELD_BOOLEAN OR FIELD_BOOLEAN", "FIELD_BOOLEAN");
    optimize("FIELD_INTEGER=2 OR FIELD_INTEGER=2", "2=FIELD_INTEGER");

    // "x < a OR x = a" to "x <= a"
    optimize("FIELD_INTEGER<1 OR FIELD_INTEGER=1", "1>=FIELD_INTEGER");
    // "x < a OR x != a" to "x != a"
    optimize("FIELD_INTEGER<1 OR FIELD_INTEGER!=1", "1!=FIELD_INTEGER");
    // "x < a OR x > a" to "x != a"
    optimize("FIELD_INTEGER<1 OR FIELD_INTEGER>1", "1!=FIELD_INTEGER");
    // "x > a OR x != a" to "x != a"
    optimize("FIELD_INTEGER>1 OR FIELD_INTEGER!=1", "1!=FIELD_INTEGER");
    // "x > a OR x = a" to "x >= a"
    optimize("FIELD_INTEGER>1 OR FIELD_INTEGER=1", "1<=FIELD_INTEGER");

    // Simplify "X=1 OR X=2 OR X=3" to X IN (1,2,3)
    optimize("FIELD_INTEGER=1 OR FIELD_INTEGER=2 OR FIELD_INTEGER=3", "FIELD_INTEGER IN (3,2,1)");
    optimize("FIELD_INTEGER=1 OR FIELD_INTEGER in (2,3)", "FIELD_INTEGER IN (2,3,1)");
    optimize("FIELD_INTEGER IN (1,2) OR FIELD_INTEGER IN (3,4)", "FIELD_INTEGER IN (3,4,1,2)");
    optimize("FIELD_STRING='1' OR NULL_INTEGER in (1,2)", "'1'=FIELD_STRING OR NULL_INTEGER IN (1,2)");
    
    returnType("false OR FIELD_BOOLEAN", BooleanType.BOOLEAN);
  }

  @Test
  public void BoolAnd() throws Exception {
    evalTrue("true AND true");
    evalTrue("true AND STRING_BOOLEAN");    
    evalFalse("true AND false");    
    evalFalse("false AND true");
    evalFalse("false AND false");
    evalFalse("false AND FIELD_BOOLEAN");
    evalFalse("false AND NULL_BOOLEAN");
    evalFalse("NULL_BOOLEAN AND false");
    
    evalNull("FIELD_BOOLEAN AND NULL_BOOLEAN");
    evalNull("NULL_BOOLEAN AND FIELD_BOOLEAN");
    evalNull("true AND NULL_BOOLEAN");
    evalNull("NULL_BOOLEAN AND true");
    evalNull("NULL_BOOLEAN AND FIELD_BOOLEAN");
    
    evalFails("false AND");
    evalFails("AND false");

    optimize("FIELD_BOOLEAN AND NULL_BOOLEAN");
    optimizeTrue("25>=12 and 14<15");
    optimizeTrue("true and true");
    optimizeFalse("true and false");
    optimizeFalse("false and true");
    optimizeFalse("false and false");
    optimizeFalse("NULL and false");
    optimizeFalse("false and NULL");
    optimizeFalse("false and FIELD_BOOLEAN");
    optimizeFalse("FIELD_BOOLEAN and false");    
    optimizeFalse("null AND null AND null AND false");
    optimizeNull("null AND null AND null AND true");    
    
    // Duplicate predicate
    optimize("FIELD_BOOLEAN and FIELD_BOOLEAN", "FIELD_BOOLEAN");
    optimize("FIELD_BOOLEAN AND NULL_BOOLEAN AND (FIELD_INTEGER>0) AND FIELD_BOOLEAN",
        "FIELD_BOOLEAN AND NULL_BOOLEAN AND 0<FIELD_INTEGER");
    optimize("(FIELD_INTEGER*2>1) AND FIELD_BOOLEAN AND (2*FIELD_INTEGER>1)",
        "FIELD_BOOLEAN AND 1<2*FIELD_INTEGER");
    optimize("FIELD_INTEGER=1 OR FIELD_BOOLEAN OR FIELD_INTEGER=1",
        "FIELD_BOOLEAN OR 1=FIELD_INTEGER");
    optimize("(FIELD_INTEGER*2>1) OR FIELD_BOOLEAN OR (2*FIELD_INTEGER>1)",
        "FIELD_BOOLEAN OR 1<2*FIELD_INTEGER");

    // Simplify IS NULL
    optimizeFalse("FIELD_BOOLEAN IS NULL AND FIELD_BOOLEAN>5");
    optimizeFalse("FIELD_BOOLEAN IS NULL AND FIELD_BOOLEAN=5");
    optimizeFalse("FIELD_BOOLEAN IS NULL AND FIELD_BOOLEAN<>5");

    // Simplify IS NOT NULL
    optimize("FIELD_BOOLEAN>5 AND FIELD_BOOLEAN IS NOT NULL AND FIELD_BOOLEAN>5",
        "5<FIELD_BOOLEAN");

    // Not satisfiable equality constant
    optimizeFalse("FIELD_INTEGER=1 AND FIELD_BOOLEAN AND FIELD_INTEGER=2");
    optimizeFalse("NULL_INTEGER=1 AND FIELD_BOOLEAN AND NULL_INTEGER=2");
    
    returnType("false AND FIELD_BOOLEAN", BooleanType.BOOLEAN);
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
    
    returnType("'amigo' ILIKE 'a%o' ESCAPE '@'", BooleanType.BOOLEAN);
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

    // Null
    evalNull("NULL_STRING like 'NULL'");
    evalNull("'test' LIKE NULL_STRING");
    evalNull("NULL LIKE '%'");
    evalNull("'a' LIKE NULL");
    
    // NULL does not match NULL
    evalNull("NULL_STRING like NULL_STRING");

    evalFails("'give me 30% discount' like '%30!%%' escape '!!'");
    evalFails("'test' LIKE 'TEST' escape NULL");
    evalFails("'a' LIKE '%' ESCAPE NULL");
       
    optimize("FIELD_STRING LIKE '%ADD!_%' ESCAPE '!'");
    optimize("FIELD_STRING LIKE '%'", "FIELD_STRING IS NOT NULL");
    optimize("FIELD_STRING LIKE 'Hello'", "'Hello'=FIELD_STRING");
    optimize("FIELD_STRING LIKE 'H%'", "STARTSWITH(FIELD_STRING,'H')");
    optimize("FIELD_STRING LIKE 'ADD%'","STARTSWITH(FIELD_STRING,'ADD')");
    optimize("FIELD_STRING LIKE '%o'", "ENDSWITH(FIELD_STRING,'o')");    
    optimize("FIELD_STRING LIKE '%Hello%'", "CONTAINS(FIELD_STRING,'Hello')");
    
    returnType("'amigo' like 'a%o'", BooleanType.BOOLEAN);
  }

  @Test
  public void Concat() throws Exception {
    // String
    evalEquals("CONCAT('TES','T')", "TEST");
    evalTrue("FIELD_STRING='TES'||'T'");
    evalTrue("FIELD_STRING='TES'||NULL_STRING||'T'");
    evalEquals("'TEST'||NULL_STRING", "TEST");
    evalEquals("NULL_STRING||'TEST'", "TEST");
    evalEquals("4 || 2", "42");
    evalEquals("4 || '2'", "42");
    evalNull("NULL_STRING||NULL_STRING");
    
    returnType("NULL_STRING||'TEST'", StringType.STRING);
    
    // Binary
    evalEquals("BINARY '1F' || NULL_BINARY || BINARY '2A3B'", new byte[]{0x1F, 0x2A, 0x3B});
    evalEquals("NULL_BINARY || BINARY '1F' || BINARY '2A3B'", new byte[]{0x1F, 0x2A, 0x3B});
    evalEquals("BINARY '1F' || BINARY '2A3B' || NULL_BINARY", new byte[]{0x1F, 0x2A, 0x3B});

    returnType("BINARY '2A3B' || NULL_BINARY", BinaryType.BINARY);
    
    optimize("FIELD_STRING||'TEST'");
  }

  @Test
  public void SearchCase() throws Exception {
    
    evalEquals("case when TRUE then 1 else 2 end", 1L);
    evalEquals("case when FALSE then 1 else 2 end", 2L);
    
    
    // implicit ELSE NULL case
    evalNull("case when FIELD_INTEGER=10 then 10 end");
    evalEquals("case when FIELD_INTEGER=40 then 10 end", 10L);

    // explicit ELSE case
    evalEquals("case when FIELD_INTEGER=40 then 10 else 50 end", 10L);
    evalEquals("case when FIELD_INTEGER>80 then 'A' else 'B' end", "B");
    evalNull("case when FIELD_INTEGER>80 then 'A' end");

    // Search CASE WHEN
    evalEquals("case when FIELD_INTEGER=10+20 then 1*5 when FIELD_INTEGER=20+20 then 2*5 else 50 end", 10L);
    evalEquals("case when FIELD_INTEGER IS NULL then null when FIELD_INTEGER=20+20 then 2*5 else 50 end", 10L);
   
    evalNull("case when NULL_INTEGER is NULL then NULL else 1 end");
        
    // Missing 'END'
    evalFails("case when FIELD_INTEGER=40 then 10 else 50");

    // Incompatible return type
    evalFails("case when FIELD_INTEGER=40 then 10 else 'Error'");
    
    // Literal
    optimizeFalse("(CASE WHEN FALSE THEN 1 ELSE 2 END) IS NULL");

    // Implicit ELSE NULL
    optimize("CASE WHEN FIELD_INTEGER>40 THEN 10 WHEN FIELD_INTEGER>20 THEN 5 ELSE NULL END", "CASE WHEN 40<FIELD_INTEGER THEN 10 WHEN 20<FIELD_INTEGER THEN 5 END");
    optimize("CASE WHEN 40=FIELD_INTEGER THEN TRUE ELSE FALSE END");
    
    // Flatten search case
    optimize("CASE WHEN FIELD_INTEGER=1 THEN 1 ELSE CASE WHEN FIELD_NUMBER=2 THEN 2 ELSE 3 END END",
        "CASE WHEN 1=FIELD_INTEGER THEN 1 WHEN 2=FIELD_NUMBER THEN 2 ELSE 3 END");

    // "CASE WHEN x IS NULL THEN y ELSE x END" to "IFNULL(x, y)"
    optimize("CASE WHEN FIELD_STRING IS NULL THEN 'TEST' ELSE FIELD_STRING END",
        "IFNULL(FIELD_STRING,'TEST')");

    // "CASE WHEN x = y THEN NULL ELSE x END" to "NULLIF(x, y)"
    optimize("CASE WHEN FIELD_INTEGER=FIELD_NUMBER THEN NULL ELSE FIELD_INTEGER END",
        "NULLIF(FIELD_INTEGER,FIELD_NUMBER)");
    optimize("CASE WHEN FIELD_INTEGER=FIELD_NUMBER THEN NULL ELSE FIELD_NUMBER END",
        "NULLIF(FIELD_NUMBER,FIELD_INTEGER)");

    // "CASE WHEN x IS NOT NULL THEN y ELSE z END" to "NVL2(x, y, z)"
    optimize("CASE WHEN FIELD_INTEGER IS NOT NULL THEN FIELD_STRING ELSE 'TEST' END",
        "NVL2(FIELD_INTEGER,FIELD_STRING,'TEST')");

    // "CASE WHEN x IS NULL THEN y ELSE z END" to "NVL2(x, z, y)"
    optimize("CASE WHEN FIELD_INTEGER IS NULL THEN FIELD_STRING ELSE 'TEST' END",
        "NVL2(FIELD_INTEGER,'TEST',FIELD_STRING)");

    // Search case to simple case: CASE WHEN a = b THEN 1 END to CASE a WHEN b THEN 1 END
    // optimize("CASE WHEN FIELD_INTEGER=1 THEN 2 END", "CASE 1 WHEN FIELD_INTEGER THEN 2");

    returnType("CASE WHEN FIELD_INTEGER IS NULL THEN '-' ELSE FIELD_STRING END", StringType.STRING);
    returnType("CASE WHEN NULL_INTEGER IS NULL THEN 0 ELSE FIELD_INTEGER END", IntegerType.INTEGER);
    returnType("CASE WHEN NULL_INTEGER IS NULL THEN 0 ELSE FIELD_NUMBER END", NumberType.NUMBER);
    returnType("CASE WHEN NULL_INTEGER IS NULL THEN 0 ELSE TO_NUMBER(TO_CHAR(FIELD_INTEGER,'YYYYMMDD')) END", NumberType.NUMBER);    
  }

  @Test
  public void SimpleCase() throws Exception {
    
    evalEquals("case FIELD_INTEGER when 10 then 10 when 40 then 40 else 50 end", 40L);
    evalEquals("case FIELD_INTEGER when 10 then 10 when 20 then 20 else -1 end", -1L);
    evalNull("case FIELD_INTEGER when 10 then 10 when 20 then 20 end");   
    
    // If the operand is null, the else clause applies.
    evalEquals("CASE NULL_NUMBER WHEN 0 THEN 0 ELSE 1 END", 1L);
    evalEquals("CASE NULL_INTEGER WHEN 1 THEN 2 ELSE 0 END", 0L);
    evalEquals("CASE NULL_NUMBER WHEN 0/0 THEN 0/0 ELSE 1 END", 1L);
    evalEquals("CASE NULL_INTEGER WHEN 0 / 0 THEN 0 / 0 ELSE 1 END", 1L);

    evalEquals("CASE 1 WHEN 2 THEN 0 / 0 ELSE 3 END", 3L);
    evalEquals("CASE 1 WHEN 1 THEN 2 WHEN 1 THEN 0 / 0 END", 2L);
    evalEquals("CASE 1 WHEN 1 THEN 2 ELSE 0 / 0 END", 2L);
    
    //Comma-separated form for multi value
    evalEquals("CASE FIELD_INTEGER WHEN 10,20 THEN 'A' WHEN 40,50,60,70 THEN 'B' ELSE 'C' END","B");
    evalEquals("CASE FIELD_INTEGER WHEN 10,20 THEN 'A' WHEN 30,40,50,60,70 THEN 'B' ELSE 'C' END","B");
    evalEquals("CASE FIELD_INTEGER WHEN 10,20 THEN 'A' WHEN 30,40 THEN 'B' ELSE 'C' END","B");
    
    // Incompatible return type
    evalFails("case FIELD_INTEGER when 10 then 'X' when ' T' then 'Test' else 'Error'");        

    // Missing 'END'
    evalFails("case FIELD_INTEGER when 40 then 10 else 50");

      
    optimize("CASE FIELD_INTEGER WHEN 40 THEN 'A' WHEN 20 THEN 'B' ELSE 'C' END");
    
    // Multi values
    optimize("CASE FIELD_INTEGER WHEN 1,2,3 THEN 'A' WHEN 4,5,6,7 THEN 'B' ELSE 'C' END");
  }
}



