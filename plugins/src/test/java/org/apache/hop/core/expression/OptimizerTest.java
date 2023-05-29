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

import org.junit.Test;

public class OptimizerTest extends ExpressionTest {

  @Test
  public void testCast() {
    // Boolean
    optimize("CAST(FIELD_BOOLEAN AS BOOLEAN)", "FIELD_BOOLEAN");
    optimize("CAST(FIELD_STRING AS BOOLEAN)", "TO_BOOLEAN(FIELD_STRING)");
    optimize("FIELD_STRING::BOOLEAN", "TO_BOOLEAN(FIELD_STRING)");

    // Json
    optimize("CAST(FIELD_JSON AS JSON)", "FIELD_JSON");
    // optimize("CAST(FIELD_STRING AS JSON)", "TO_JSON(FIELD_STRING)");
    // optimize("FIELD_STRING::JSON", "TO_JSON(FIELD_STRING)");

    // optimize("CAST(FIELD_STRING AS STRING)", "FIELD_STRING");
    // optimize("CAST(FIELD_INTEGER AS INTEGER)", "FIELD_INTEGER");

    // optimize("CAST(FIELD_STRING AS DATE)", "TO_DATE(FIELD_STRING)");
    // optimize("CAST(FIELD_STRING AS DATE FORMAT 'YYYY-MM-DD')",
    // "TO_DATE(FIELD_STRING,'YYYY-MM-DD')");
  }


  @Test
  public void testChainedCast() {
    // optimize("CAST(CAST(CAST(123456 AS INTEGER) AS NUMBER) AS BIGNUMBER)", "123456");
  }

  @Test
  public void testIsNull() {
    optimizeTrue("NULL IS NULL");
    optimizeTrue("NULL+1 IS NULL");
    optimizeFalse("NULL+1 IS NOT NULL");
    optimizeTrue("TRUE IS NOT NULL");
    optimizeFalse("TRUE IS NULL");
    optimizeFalse("1 IS NULL");
    optimizeTrue("1 IS NOT NULL");
    optimizeFalse("'a' IS NULL");
    optimizeTrue("'a' IS NOT NULL");
  }

  @Test
  public void testIsDistinctFrom() {
    optimizeTrue("NULL IS NOT DISTINCT FROM NULL");
    optimizeFalse("NULL IS DISTINCT FROM NULL");
    optimizeFalse("NULL IS NOT DISTINCT FROM TRUE");
    optimizeTrue("NULL IS DISTINCT FROM TRUE");
    optimizeTrue("NULL IS DISTINCT FROM 3");
    optimizeTrue("10151082135029368  IS DISTINCT FROM 10151082135029369");
    optimizeTrue("FIELD_INTEGER IS NOT DISTINCT FROM FIELD_INTEGER");
    optimizeFalse("FIELD_INTEGER IS DISTINCT FROM FIELD_INTEGER");
    optimize("NULL_INTEGER IS DISTINCT FROM NULL", "NULL_INTEGER IS NOT NULL");
    optimize("NULL_INTEGER IS NOT DISTINCT FROM NULL", "NULL_INTEGER IS NULL");
  }

  @Test
  public void testNullIf() {
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
  public void testNullIfZero() {
    optimize("NULLIFZERO(0.1)","0.1");
    optimizeNull("NULLIFZERO(0.0)");
    optimizeNull("NULLIFZERO(-0.000)");
  }
  
  @Test
  public void testEqualNull() {
    optimizeTrue("EQUAL_NULL(NULL_STRING, NULL_STRING)");
    optimizeTrue("EQUAL_NULL(FIELD_STRING, FIELD_STRING)");
    optimizeTrue("EQUAL_NULL(FIELD_INTEGER, FIELD_INTEGER)");
    optimize("EQUAL_NULL(FIELD_INTEGER,NULL)", "FIELD_INTEGER IS NULL");
    optimize("EQUAL_NULL(NULL,FIELD_INTEGER)", "FIELD_INTEGER IS NULL");
  }

  @Test
  public void testIn() {
    optimizeTrue("'foo' IN ('bar', 'baz', 'foo', 'blah')");
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
  }

  @Test
  public void testLike() {
    optimize("FIELD_STRING LIKE '%'", "FIELD_STRING IS NOT NULL");
    optimize("FIELD_STRING LIKE 'Hello'", "'Hello'=FIELD_STRING");
    optimize("FIELD_STRING LIKE 'H%'", "STARTSWITH(FIELD_STRING,'H')");
    optimize("FIELD_STRING LIKE '%o'", "ENDSWITH(FIELD_STRING,'o')");
    optimize("FIELD_STRING LIKE '%Hello%'", "CONTAINS(FIELD_STRING,'Hello')");
  }

  @Test
  public void testExtract() {
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
  public void testBoolNot() {
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
  }

  @Test
  public void testBoolAnd() {

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
  }

  @Test
  public void testBoolOr() {

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
    optimize("FIELD_INTEGER=1 OR FIELD_INTEGER=2 OR FIELD_INTEGER=3 ", "FIELD_INTEGER IN (3,1,2)");
    optimize("FIELD_INTEGER=1 OR FIELD_INTEGER in (2,3)", "FIELD_INTEGER IN (1,2,3)");
    optimize("FIELD_INTEGER IN (1,2) OR FIELD_INTEGER IN (3,4)", "FIELD_INTEGER IN (1,2,3,4)");
    optimize("FIELD_STRING='1' OR NULL_INTEGER in (1,2)",
        "'1'=FIELD_STRING OR NULL_INTEGER IN (1,2)");
  }

  @Test
  public void testComparison() {
    optimizeNull("NULL = NULL");
    optimizeTrue("'a' = 'a'");
    optimizeFalse("'a' = 'b'");
    optimizeNull("'a' = NULL");
    optimizeNull("NULL = 'a'");
    optimizeFalse("10151082135029368 = 10151082135029369");
    optimizeTrue("25>12");
    optimizeTrue("25>=12");
    optimizeFalse("25<12");
    optimizeFalse("25<=12");

    optimize("FIELD_BOOLEAN = TRUE", "FIELD_BOOLEAN IS TRUE");
    optimize("TRUE = FIELD_BOOLEAN", "FIELD_BOOLEAN IS TRUE");
    optimize("FIELD_BOOLEAN >= TRUE", "FIELD_BOOLEAN IS TRUE");
    optimize("TRUE >= FIELD_BOOLEAN", "FIELD_BOOLEAN IS TRUE");
    optimize("FIELD_BOOLEAN <= TRUE", "FIELD_BOOLEAN IS TRUE");
    optimize("TRUE <= FIELD_BOOLEAN", "FIELD_BOOLEAN IS TRUE");

    optimize("FIELD_BOOLEAN = FALSE", "FIELD_BOOLEAN IS FALSE");
    optimize("FALSE = FIELD_BOOLEAN", "FIELD_BOOLEAN IS FALSE");
    optimize("FIELD_BOOLEAN >= FALSE", "FIELD_BOOLEAN IS FALSE");
    optimize("FALSE >= FIELD_BOOLEAN", "FIELD_BOOLEAN IS FALSE");
    optimize("FIELD_BOOLEAN <= FALSE", "FIELD_BOOLEAN IS FALSE");
    optimize("FALSE <= FIELD_BOOLEAN", "FIELD_BOOLEAN IS FALSE");

    optimize("FIELD_STRING = NULL", "NULL");
    optimize("FIELD_STRING > NULL", "NULL");
    optimize("FIELD_STRING >= NULL", "NULL");
    optimize("FIELD_STRING < NULL", "NULL");
    optimize("FIELD_STRING <= NULL", "NULL");

    // Simplify comparison with same term
    optimize("FIELD_STRING=FIELD_STRING", "NULL OR FIELD_STRING IS NOT NULL");
    optimize("FIELD_STRING>=FIELD_STRING", "NULL OR FIELD_STRING IS NOT NULL");
    optimize("FIELD_STRING<=FIELD_STRING", "NULL OR FIELD_STRING IS NOT NULL");
    optimize("FIELD_STRING!=FIELD_STRING", "NULL AND FIELD_STRING IS NULL");
    optimize("FIELD_STRING>FIELD_STRING", "NULL AND FIELD_STRING IS NULL");
    optimize("FIELD_STRING<FIELD_STRING", "NULL AND FIELD_STRING IS NULL");

    // The DISTINCT predicate is a verbose way of NULL safe comparisons
    optimize("FIELD_STRING IS DISTINCT FROM NULL", "FIELD_STRING IS NOT NULL");
    optimize("NULL IS DISTINCT FROM FIELD_STRING", "FIELD_STRING IS NOT NULL");
    optimize("FIELD_STRING IS NOT DISTINCT FROM NULL", "FIELD_STRING IS NULL");
    optimize("NULL IS NOT DISTINCT FROM FIELD_STRING", "FIELD_STRING IS NULL");
  }

  @Test
  public void testConstantOperator() {
    optimize("PI()", "3.141592653589793");
  }

  @Test
  public void testDeterministicFunctionCall()
  {
    // optimize should do nothing
    // TODO: optimize("RANDOM()", "RANDOM()");
    
    optimize("CONCAT('TES','T')", "'TEST'");

    optimize("'A'||'B'", "'AB'");


    optimizeFalse("(CASE WHEN FALSE THEN 1 ELSE 2 END) IS NULL");

    optimize("-(-FIELD_INTEGER)", "FIELD_INTEGER");
    optimize("false and true or FIELD_BOOLEAN", "FALSE OR FIELD_BOOLEAN");
    optimizeFalse("false and FIELD_BOOLEAN");

    optimizeTrue("true is true");
    optimizeTrue("false is false");
    optimizeFalse("true is false");
    optimizeTrue("false is not true");
    optimizeTrue("true is not false");
    optimizeFalse("true is null");
    optimizeFalse("false is null");


    optimizeTrue("25>=12 and 14<15");

    optimize("FIELD_INTEGER between 3 and (5+1)", "FIELD_INTEGER BETWEEN 3 AND 6");
    optimizeFalse("2 between 3 and (5+1)");

    optimize("Cast('2021-02-08' as DATE)", "DATE '2021-02-08'");
    optimize("'2021-02-08'::DATE", "DATE '2021-02-08'");

    optimizeTrue("'25' in ('1','25','66')");
    optimizeTrue("25.8 between 18 and 32");
    optimizeTrue("Trim(' test ')='test'");
    optimize("DAY(DATE '2019-02-15')", "15");
    optimize("DAY(DATE_FROM_PARTS(2019,2,15))", "15");
  }

  @Test
  public void testCombineConcats() {
    // Same syntax but cost reduced
    optimize("'A'||FIELD_STRING||FIELD_STRING||'C'", "'A'||FIELD_STRING||FIELD_STRING||'C'");
    optimize("'A'||FIELD_STRING||NULL_STRING||'C'", "'A'||FIELD_STRING||NULL_STRING||'C'");
    optimize("CONCAT('A',CONCAT(FIELD_STRING,CONCAT(FIELD_STRING,'C')||'D'))",
        "'A'||FIELD_STRING||FIELD_STRING||'C'||'D'");
  }
  
  @Test
  public void testArithmeticUnary() {
    optimize("-(10+2)", "-12");
    optimize("-(0)", "0");

    // Negative
    optimize("-(-FIELD_INTEGER)", "FIELD_INTEGER");
    optimize("-(FIELD_INTEGER-FIELD_NUMBER)", "FIELD_NUMBER-FIELD_INTEGER");
  }
  

  @Test
  public void testArithmetic() {

    optimize("3+1+1+1+1+1+1+1+1+1+1+1", "14");
    optimize("3+1+2", "6");
    optimize("3+1*2", "5");
    optimize("(3+1)*2", "8");
    optimize("0/0", "0/0");

    // Additive
    optimize("FIELD_INTEGER+0", "FIELD_INTEGER");
    optimize("4+FIELD_INTEGER+1", "5+FIELD_INTEGER");
    optimize("0+FIELD_INTEGER", "FIELD_INTEGER");
    optimize("1+FIELD_INTEGER+3+FIELD_INTEGER+5*2", "14+FIELD_INTEGER+FIELD_INTEGER");
    optimize("FIELD_INTEGER+3+1", "4+FIELD_INTEGER");
    optimize("FIELD_INTEGER+(-FIELD_NUMBER)", "FIELD_INTEGER-FIELD_NUMBER");

    // Subtract
    optimize("FIELD_INTEGER-0", "FIELD_INTEGER");
    optimize("0-FIELD_INTEGER", "-FIELD_INTEGER");
    optimize("FIELD_INTEGER-(0-FIELD_INTEGER)", "FIELD_INTEGER+FIELD_INTEGER");
    optimize("FIELD_INTEGER-(-FIELD_NUMBER)", "FIELD_INTEGER+FIELD_NUMBER");

    // Multiply
    optimize("FIELD_INTEGER*1", "FIELD_INTEGER");
    optimize("FIELD_INTEGER*3*2", "6*FIELD_INTEGER");
    optimize("3*(FIELD_INTEGER*1)*1*(2*5)", "30*FIELD_INTEGER");
    optimize("1.0*FIELD_INTEGER", "FIELD_INTEGER");
    optimize("4*FIELD_INTEGER*0.5", "2.0*FIELD_INTEGER");
    optimize("-FIELD_INTEGER*(-FIELD_NUMBER)", "FIELD_INTEGER*FIELD_NUMBER");
    optimize("FIELD_INTEGER*FIELD_INTEGER", "SQUARE(FIELD_INTEGER)");

    // Divide
    optimize("FIELD_INTEGER/1", "FIELD_INTEGER");
    optimize("DIV0(FIELD_INTEGER,1)", "FIELD_INTEGER");
    optimize("FIELD_INTEGER/1.0", "FIELD_INTEGER");
    optimize("-FIELD_NUMBER/-FIELD_INTEGER", "FIELD_NUMBER/FIELD_INTEGER");
    optimize("DIV0(-FIELD_NUMBER,-FIELD_INTEGER)", "DIV0(FIELD_NUMBER,FIELD_INTEGER)");

    // Power
    optimize("POWER(FIELD_INTEGER,1)", "FIELD_INTEGER");
  }

  @Test
  public void testArithmeticComparisons() {
    optimize("FIELD_INTEGER+1=3", "2=FIELD_INTEGER");
    optimize("FIELD_INTEGER+1!=3", "2!=FIELD_INTEGER");
    optimize("3>FIELD_INTEGER+1", "2>FIELD_INTEGER");
    optimize("FIELD_INTEGER+1>3", "2<FIELD_INTEGER");
    optimize("FIELD_INTEGER+1>=3", "2<=FIELD_INTEGER");
    optimize("3>=FIELD_INTEGER+1", "2>=FIELD_INTEGER");
    optimize("FIELD_INTEGER+1<3", "2>FIELD_INTEGER");
    optimize("3>FIELD_INTEGER+1", "2>FIELD_INTEGER");
    optimize("FIELD_INTEGER+1<=3", "2>=FIELD_INTEGER");
    optimize("3>=FIELD_INTEGER+1", "2>=FIELD_INTEGER");
  }

  @Test
  public void testBitwise() { 
    optimize("~(~FIELD_INTEGER)", "FIELD_INTEGER");
  }

  @Test
  public void testCoalesce() {
    optimize("COALESCE(NULL)", "NULL");
    optimize("COALESCE(FIELD_INTEGER)", "FIELD_INTEGER");

    // Duplicate coalesce
    optimize("COALESCE(FIELD_INTEGER,FIELD_INTEGER)", "FIELD_INTEGER");
    optimize("COALESCE(FIELD_INTEGER, FIELD_NUMBER, FIELD_NUMBER)",
        "COALESCE(FIELD_INTEGER,FIELD_NUMBER)");

    // Flatten
    optimize("COALESCE(FIELD_INTEGER,COALESCE(FIELD_INTEGER,COALESCE(FIELD_NUMBER,2)))",
        "COALESCE(FIELD_INTEGER,FIELD_NUMBER,2)");
  }

  @Test
  public void testConstantFunction() {
    optimize("PI()", "3.141592653589793");
    optimize("EXP(1)", "2.718281828459045");
  }

  @Test
  public void testFunctionRepetition() {
    optimize("LTRIM(LTRIM(FIELD_STRING))", "LTRIM(FIELD_STRING)");
    optimize("RTRIM(RTRIM(FIELD_STRING))", "RTRIM(FIELD_STRING)");
    optimize("TRIM(TRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
    optimize("TRIM(LTRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
    optimize("TRIM(RTRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
    optimize("RTRIM(TRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
    optimize("LTRIM(TRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
    optimize("UPPER(UPPER(FIELD_STRING))", "UPPER(FIELD_STRING)");
    optimize("UPPER(LOWER(FIELD_STRING))", "UPPER(FIELD_STRING)");
    optimize("LOWER(LOWER(FIELD_STRING))", "LOWER(FIELD_STRING)");
    optimize("LOWER(UPPER(FIELD_STRING))", "LOWER(FIELD_STRING)");
    optimize("ABS(ABS(FIELD_INTEGER))", "ABS(FIELD_INTEGER)");
    optimize("SIGN(SIGN(FIELD_INTEGER))", "SIGN(FIELD_INTEGER)");
    optimize("CEILING(CEILING(FIELD_NUMBER))", "CEILING(FIELD_NUMBER)");
    optimize("FLOOR(FLOOR(FIELD_NUMBER))", "FLOOR(FIELD_NUMBER)");
    optimize("ROUND(ROUND(FIELD_NUMBER))", "ROUND(FIELD_NUMBER)");
    // optimize("TRUNC(TRUNC(FIELD_NUMBER))", "TRUNC(FIELD_NUMBER)");
  }

  @Test
  public void testSearchCase() {
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
  }

}
