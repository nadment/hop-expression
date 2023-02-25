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
  public void testSimplifyInRule() throws Exception {
    optimize("FIELD_INTEGER in (1,2,1,null,null,3,4)", "FIELD_INTEGER IN (1,2,3,4)");
    optimize("FIELD_STRING in ('1','2','1',NULL,null)", "FIELD_STRING IN ('1','2')"); 
    
    // Value in the expression list
    optimize("FIELD_INTEGER in (1,2,FIELD_INTEGER)","FIELD_INTEGER=FIELD_INTEGER");
    optimize("FIELD_STRING in ('XX',FIELD_STRING,'ZZ')","FIELD_STRING=FIELD_STRING");
  }

  @Test
  public void testSimplifyLikeRule() throws Exception {
    optimizeNull("FIELD_STRING LIKE NULL");
    optimizeNull("NULL LIKE FIELD_STRING");
    optimize("FIELD_STRING LIKE '%'", "FIELD_STRING=FIELD_STRING");
    optimize("FIELD_STRING LIKE 'Hello'", "FIELD_STRING='Hello'");
    optimize("FIELD_STRING LIKE 'H%'", "STARTSWITH(FIELD_STRING,'H')");
    optimize("FIELD_STRING LIKE '%o'", "ENDSWITH(FIELD_STRING,'o')");
    optimize("FIELD_STRING LIKE '%Hello%'", "CONTAINS(FIELD_STRING,'Hello')");
  }

  @Test
  public void testSimplifyExtractRule() throws Exception {
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
  public void testSimplifyBooleanRule() throws Exception {
    optimizeFalse("not true");
    optimizeTrue("not false");
    optimizeTrue("not not true");
    optimizeFalse("not not false");
    optimize("NOT(NOT(FIELD_BOOLEAN))", "FIELD_BOOLEAN");
    optimize("not(FIELD_INTEGER>5)", "FIELD_INTEGER<=5");
    optimize("not(FIELD_INTEGER>=5)", "FIELD_INTEGER<5");
    optimize("not(FIELD_INTEGER<5)", "FIELD_INTEGER>=5");
    optimize("not(FIELD_INTEGER<=5)", "FIELD_INTEGER>5");

    optimizeTrue("true or true");
    optimizeTrue("true or false");
    optimizeTrue("false or true");
    optimizeFalse("false or false");
    optimizeTrue("true or null");
    optimizeTrue("null or true");
    optimizeNull("null or null");
    optimizeTrue("FIELD_BOOLEAN or true");
    optimizeTrue("true or FIELD_STRING");
    optimizeTrue("true or FIELD_BOOLEAN");
    optimizeTrue("FIELD_BOOLEAN or true");
    optimize("null or FIELD_BOOLEAN", "FIELD_BOOLEAN");
    optimize("FIELD_BOOLEAN or null", "FIELD_BOOLEAN");
    optimize("false or FIELD_BOOLEAN", "FALSE OR FIELD_BOOLEAN");
    optimize("FIELD_BOOLEAN or false", "FALSE OR FIELD_BOOLEAN");
    optimize("FIELD_BOOLEAN or FIELD_BOOLEAN", "FIELD_BOOLEAN");

    optimizeTrue("true and true");
    optimizeFalse("true and false");
    optimizeFalse("false and true");
    optimizeFalse("false and false");
    optimizeNull("FIELD_BOOLEAN and null");
    optimizeNull("null and FIELD_BOOLEAN");
    optimizeFalse("false and FIELD_BOOLEAN");
    optimizeFalse("FIELD_BOOLEAN and false");
    optimize("FIELD_BOOLEAN and FIELD_BOOLEAN", "FIELD_BOOLEAN");    
    
    optimizeTrue("EQUAL_NULL(NULL_STRING, NULL_STRING)");
    optimizeTrue("EQUAL_NULL(FIELD_STRING, FIELD_STRING)");
    
    // TODO: optimize("(A IS NOT NULL OR B) AND A IS NOT NULL","A IS NOT NULL");
  }

  @Test
  public void testDeterministicRule() throws Exception {
    optimize("CONCAT('TES','T')", "'TEST'");

    optimize("'A'||'B'", "'AB'");

    optimize("3+1+1+1+1+1+1+1+1+1+1+1", "14");
    optimize("3+1+2", "6");
    optimize("3+1*2", "5");
    optimize("(3+1)*2", "8");
    optimize("-(10+2)", "-12");
    optimize("-(0)", "0");

    optimize("NOT (FIELD_BOOLEAN IS TRUE)", "FIELD_BOOLEAN IS FALSE");
    optimize("NOT (FIELD_BOOLEAN IS NOT TRUE)", "FIELD_BOOLEAN IS TRUE");
    optimize("NOT (FIELD_BOOLEAN IS FALSE)", "FIELD_BOOLEAN IS TRUE");
    optimize("NOT (FIELD_BOOLEAN IS NOT FALSE)", "FIELD_BOOLEAN IS FALSE");
    optimize("NOT (FIELD_BOOLEAN IS NOT NULL)", "FIELD_BOOLEAN IS NULL");
    optimize("NOT (FIELD_BOOLEAN IS NULL)", "FIELD_BOOLEAN IS NOT NULL");

    optimize("-(-FIELD_INTEGER)", "FIELD_INTEGER");
    optimize("false and true or FIELD_BOOLEAN", "FALSE OR FIELD_BOOLEAN");
    optimizeFalse("false and FIELD_BOOLEAN");

    optimizeTrue("null is null");
    optimizeTrue("true is true");
    optimizeTrue("false is false");
    optimizeFalse("true is false");
    optimizeTrue("false is not true");
    optimizeTrue("true is not false");
    optimizeFalse("true is null");
    optimizeFalse("false is null");
    optimizeFalse("null is true");
    optimizeFalse("null is false");

    optimizeTrue("25>=12");
    optimizeTrue("25>=12 and 14<15");

    optimize("FIELD_INTEGER between 3 and (5+1)","FIELD_INTEGER BETWEEN 3 AND 6");
    optimizeFalse("2 between 3 and (5+1)");

    optimize("Cast('2021-02-08' as DATE)", "DATE '2021-02-08'");

    optimizeNull("null=null");
    optimizeTrue("'25' in ('1','25','66')");
    optimizeTrue("25.8 between 18 and 32");
    optimizeTrue("Trim(' test ')='test'");
    optimize("Day(Date '2019-02-15')", "15");
    optimize("Day(Date(2019,2,15))", "15");
  }

  @Test
  public void testCombineConcatsRule() throws Exception {
    // Same syntax but cost reduced
    optimize("'A'||FIELD_STRING||FIELD_STRING||'C'", "'A'||FIELD_STRING||FIELD_STRING||'C'");
    optimize("'A'||FIELD_STRING||NULL||'C'", "'A'||FIELD_STRING||'C'");
    optimize("CONCAT('A',CONCAT(FIELD_STRING,CONCAT(FIELD_STRING,'C')||'D'))", "'A'||FIELD_STRING||FIELD_STRING||'C'||'D'");
    optimize("NULL||CONCAT(FIELD_STRING,NULL)","FIELD_STRING");
  }

  @Test
  public void testArithmeticRule() throws Exception {

    optimize("-(-FIELD_INTEGER)", "FIELD_INTEGER");
    optimize("FIELD_INTEGER+0", "FIELD_INTEGER");
    optimize("0+FIELD_INTEGER", "FIELD_INTEGER");
    optimize("FIELD_INTEGER-0", "FIELD_INTEGER");
    optimize("0-FIELD_INTEGER", "-FIELD_INTEGER");
    optimize("FIELD_INTEGER-(0-FIELD_INTEGER)", "FIELD_INTEGER+FIELD_INTEGER");
    optimize("FIELD_INTEGER*1", "FIELD_INTEGER");
    optimize("1.0*FIELD_INTEGER", "FIELD_INTEGER");

    optimize("FIELD_INTEGER*3*2", "6*FIELD_INTEGER");
    optimize("3*(FIELD_INTEGER*1)*1*(2*5)", "30*FIELD_INTEGER");
    optimize("1+FIELD_INTEGER+3+FIELD_INTEGER+5*2", "14+FIELD_INTEGER+FIELD_INTEGER");
    optimize("FIELD_INTEGER+3+1", "4+FIELD_INTEGER");
    optimize("4+FIELD_INTEGER+1", "5+FIELD_INTEGER");
    optimize("4*FIELD_INTEGER*0.5", "2.0*FIELD_INTEGER");
    optimize("FIELD_INTEGER/1", "FIELD_INTEGER");
    optimize("FIELD_INTEGER/1.0", "FIELD_INTEGER");
  }

  @Test
  public void testChainedCast() throws Exception {
    //optimize("CAST(CAST(CAST(123456 AS INTEGER) AS NUMBER) AS BIGNUMBER)", "123456");
  }
}
