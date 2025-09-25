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

import java.math.BigDecimal;
import java.net.InetAddress;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionTest;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.BooleanType;
import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.InetType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.IntervalType;
import org.apache.hop.expression.type.JsonType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.util.JsonConversion;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.parallel.Execution;

@TestInstance(PER_CLASS)
@Execution(CONCURRENT)
public class OperatorTest extends ExpressionTest {
  @Nested
  class NotEqualTo {
    @Test
    void testPredicate() throws Exception {
      evalTrue("FIELD_STRING != 'foo'").returnType(BooleanType.BOOLEAN);
      evalTrue("FIELD_STRING <> 'tEST'");
      evalFalse("FIELD_INTEGER != 40");
      evalFalse("FIELD_INTEGER <> 40");

      evalTrue("1 <> 2");
      evalTrue("10 <> 0x10");
      evalFalse("1.12 <> 1.12");

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

      // Compare numeric with implicit coercion from BOOLEAN
      evalFalse("TRUE<>1");
      evalFalse("1<>TRUE");
      evalTrue("TRUE<>1.1");
      evalTrue("TRUE<>0");
      evalFalse("TRUE<>1");

      // Compare numeric with implicit coercion from INTEGER and NUMBER
      evalTrue("2 <> 2.1");
      evalTrue("2.1 <> 2");
      evalFalse("2 <> 2.0");
      evalFalse("2.0 <> 2");

      // Comparable unordered type
      evalTrue("FIELD_JSON <> FIELD_STRING_JSON::JSON");
    }

    @Test
    void testNullHandling() throws Exception {
      // NULL is not equal to anything, not even to another NULL.
      // x<>NULL is always null
      evalNull("NULL_STRING <> 'bar'");
      evalNull("'bar' <> NULL_STRING");
      evalNull("NULL_STRING <> NULL_STRING");
      evalNull("NULL_INTEGER <> FIELD_INTEGER");
      evalNull("NULL_INTEGER <> FIELD_NUMBER");
      evalNull("NULL_INTEGER <> NULL_NUMBER");
    }

    @Test
    void testOptimize() throws Exception {
      // Normalize
      optimize("10!=FIELD_INTEGER", "FIELD_INTEGER!=10");
      optimize("FIELD_INTEGER=40");

      // Simplify comparison with literals
      optimizeFalse("'a' <> 'a'");
      optimizeTrue("'a' <> 'b'");
      optimizeFalse("10151082135029368 <> 10151082135029368");
      optimizeTrue("10151082135029368 <> 10151082135029369");
      optimizeNull("NULL::STRING<>FIELD_STRING");
      optimizeNull("FIELD_STRING<>NULL::STRING");

      // Simplify comparison with the same term
      optimize("FIELD_STRING!=FIELD_STRING", "NULL AND FIELD_STRING IS NULL");

      // Simplify comparison when operands is of boolean type
      optimize("FIELD_BOOLEAN_TRUE<>FALSE", "FIELD_BOOLEAN_TRUE");
      optimize("FALSE<>FIELD_BOOLEAN_TRUE", "FIELD_BOOLEAN_TRUE");
      optimize("FIELD_BOOLEAN_TRUE<>TRUE", "NOT FIELD_BOOLEAN_TRUE");
      optimize("TRUE<>FIELD_BOOLEAN_TRUE", "NOT FIELD_BOOLEAN_TRUE");

      // Simplify comparison with arithmetic
      optimize("FIELD_INTEGER+1!=3", "FIELD_INTEGER!=2");
      optimizeFalse("PI()!=PI()");
    }
  }

  @Nested
  class GreaterThan {
    @Test
    void testPredicate() throws Exception {
      evalTrue("9>5").returnType(BooleanType.BOOLEAN_NOT_NULL);
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
      evalFalse("BINARY '4100' > BINARY '42'").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("BINARY '410000' > BINARY '4100'");
      evalFalse("BINARY '4100' > BINARY '410000'");
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("NULL_BOOLEAN > 0");
      evalNull("NULL_INTEGER > 0");
      evalNull("NULL_NUMBER > NULL_INTEGER");
      evalNull("1 > NULL_BOOLEAN");
      evalNull("FIELD_STRING > NULL_STRING");
      evalNull("NULL_STRING > FIELD_STRING");
    }

    @Test
    void testCoercion() throws Exception {
      // Compare numeric with implicit coercion from BOOLEAN
      evalFalse("TRUE>1");
      evalFalse("1>TRUE");
      evalFalse("TRUE>1.1");
      evalTrue("TRUE>0.5");

      // Compare numeric with implicit coercion from INTEGER and NUMBER
      evalTrue("2 > 1.5");
      evalTrue("2.1 > 2");
      evalFalse("1 > 2");
      evalFalse("2 > 2.1");
      evalFalse("2.0 > 2");
    }

    @Test
    void testError() throws Exception {
      // Unsupported coercion
      evalFails("FIELD_DATE>5", ErrorCode.ILLEGAL_ARGUMENT);

      // Compare unordered type
      evalFails("FIELD_JSON > FIELD_STRING", ErrorCode.ILLEGAL_ARGUMENT);

      // Conversion error
      evalFails("FIELD_STRING>5", ErrorCode.CONVERSION_ERROR);

      // Syntax error
      evalFails("> FIELD_INTEGER", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_INTEGER >", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_INTEGER > ", ErrorCode.SYNTAX_ERROR);
    }

    @Test
    void testOptimize() throws Exception {
      // Normalize
      optimize("FIELD_INTEGER>10");
      optimize("10>FIELD_INTEGER", "FIELD_INTEGER<10");
      optimizeTrue("25>12");

      // Simplify arithmetic comparisons
      optimize("3>FIELD_INTEGER+1", "FIELD_INTEGER<2");
      optimize("FIELD_INTEGER+1>3", "FIELD_INTEGER>2");

      // Simplify comparison with the same term
      optimize("FIELD_STRING>FIELD_STRING", "NULL AND FIELD_STRING IS NULL");
    }
  }

  @Nested
  class GreaterThanOrEqualTo {
    @Test
    void testPredicate() throws Exception {
      evalTrue("9 >= 5").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("9.4 >= 9.358");
      evalTrue("(4+2) >= 10-9");
      evalTrue("FIELD_INTEGER >= 10");
      evalTrue("FIELD_INTEGER >= 10.3");
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
      evalFalse("BINARY '4100' >= BINARY '42'").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("BINARY '410000' >= BINARY '4100'");
      evalFalse("BINARY '4100' >= BINARY '410000'");
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("NULL_BOOLEAN >= 0");
      evalNull("1 >= NULL_BOOLEAN");
      evalNull("NULL_BOOLEAN >= NULL_INTEGER");
      evalNull("FIELD_STRING >= NULL_STRING");
      evalNull("NULL_STRING >= FIELD_STRING");
    }

    @Test
    void testCoercion() throws Exception {
      // Compare numeric with implicit coercion from BOOLEAN
      evalTrue("TRUE>=1");
      evalTrue("1>=TRUE");
      evalFalse("TRUE>=1.1");
      evalTrue("TRUE>=0.5");

      // Compare numeric with implicit coercion from INTEGER and NUMBER
      evalTrue("2 >= 1.5");
      evalFalse("2 >= 2.1");
      evalTrue("2 >= 2.0");
      evalFalse("1.5 >= 2");
      evalTrue("1 >= 1.0");
    }

    @Test
    void testError() throws Exception {
      // Compare unordered type
      evalFails("FIELD_JSON >= FIELD_STRING", ErrorCode.ILLEGAL_ARGUMENT);

      // Unsupported coercion
      evalFails("FIELD_DATE>=5", ErrorCode.ILLEGAL_ARGUMENT);

      // Conversion error
      evalFails("FIELD_STRING>=5", ErrorCode.CONVERSION_ERROR);

      // Syntax error
      evalFails(">=FIELD_INTEGER", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_INTEGER >=", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_INTEGER >= ", ErrorCode.SYNTAX_ERROR);
    }

    @Test
    void testOptimize() throws Exception {
      // Normalize
      optimize("FIELD_INTEGER>=80");
      optimize("80>=FIELD_INTEGER", "FIELD_INTEGER<=80");

      optimizeTrue("25>=12");
      optimize("FIELD_BOOLEAN_TRUE>=TRUE");
      optimize("FIELD_BOOLEAN_TRUE>=FALSE", "FIELD_BOOLEAN_TRUE IS NOT NULL");
      optimize("TRUE >= FIELD_BOOLEAN_TRUE", "FIELD_BOOLEAN_TRUE IS NOT NULL");
      optimize("FIELD_BOOLEAN_TRUE >= FALSE", "FIELD_BOOLEAN_TRUE IS NOT NULL");
      optimize("FALSE >= FIELD_BOOLEAN_TRUE", "FIELD_BOOLEAN_TRUE<=FALSE");

      // Simplify arithmetic comparisons
      optimize("FIELD_INTEGER+1>=3", "FIELD_INTEGER>=2");
      optimize("3>=FIELD_INTEGER+1", "FIELD_INTEGER<=2");

      // Simplify comparison with same term
      optimize("FIELD_STRING>=FIELD_STRING", "FIELD_STRING>=FIELD_STRING");
      optimize("PI()>=PI()", "TRUE");
    }
  }

  @Nested
  class LessThan {
    @Test
    void testPredicate() throws Exception {
      evalTrue("5 < 9").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("9.358 < 9.4");
      evalTrue("10-9 < (4+2)");
      evalTrue("FIELD_INTEGER < 100");
      evalTrue("FIELD_INTEGER < 100.12");
      evalFalse("5 < 5");

      evalFalse("true < false").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("false < true");
      evalFalse("false < false");
      evalFalse("true < true");

      evalTrue("'bar' < 'foo'").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalFalse("'foo' < 'foo'");
      evalFalse("'foo' < 'bar'");

      evalTrue("BINARY '4100' < BINARY '42'").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("BINARY '4100' < BINARY '410000'");
      evalFalse("BINARY '4100' < BINARY '4100'");
      evalFalse("BINARY '410000' < BINARY '4100'");

      evalTrue("DATE '2019-01-01' < DATE '2019-02-01'");
      evalFalse("DATE '2019-01-01' < DATE '2019-01-01'");
      evalFalse("DATE '2019-01-01' < DATE '2018-01-01'");

      evalTrue("INTERVAL 3 DAYS < INTERVAL 3 MONTHS");
      evalFalse("INTERVAL 3 YEARS < INTERVAL 3 MONTHS");
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("NULL_INTEGER < 1").returnType(BooleanType.BOOLEAN);
      evalNull("NULL_NUMBER < NULL_INTEGER");
      evalNull("NULL_STRING < Upper(FIELD_STRING)");
      evalNull("FIELD_STRING < NULL_STRING");
      evalNull("NULL_STRING < FIELD_STRING");
    }

    @Test
    void testCoercion() throws Exception {
      // Compare numeric with implicit coercion from BOOLEAN
      evalFalse("TRUE<1");
      evalFalse("1<TRUE");
      evalTrue("TRUE<1.1");
      evalFalse("TRUE<0.5");

      // Compare numeric with implicit coercion from INTEGER and NUMBER
      evalFalse("2 < 1.5");
      evalFalse("2.1 < 2");
      evalTrue("1.5 < 2");
      evalTrue("2 < 2.1");
      evalFalse("2.1 < 2.1");
    }

    @Test
    void testError() throws Exception {
      // Compare unordered type
      evalFails("FIELD_JSON < FIELD_STRING", ErrorCode.ILLEGAL_ARGUMENT);

      // Conversion error
      evalFails("FIELD_STRING < 5", ErrorCode.CONVERSION_ERROR);

      // Syntax error
      evalFails("< FIELD_INTEGER", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_INTEGER <", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_INTEGER < ", ErrorCode.SYNTAX_ERROR);
    }

    @Test
    void testOptimize() throws Exception {
      // Normalize
      optimize("FIELD_INTEGER<80");
      optimize("80>FIELD_INTEGER", "FIELD_INTEGER<80");

      optimizeFalse("25<12");

      // Simplify arithmetic comparisons
      optimize("FIELD_INTEGER+1<3", "FIELD_INTEGER<2");
      optimize("3>FIELD_INTEGER+1", "FIELD_INTEGER<2");

      // Simplify comparison with the same term
      optimize("FIELD_STRING<FIELD_STRING", "NULL AND FIELD_STRING IS NULL");
    }
  }

  @Nested
  class LessThanOrEqualTo {
    @Test
    void testPredicate() throws Exception {
      evalTrue("5 <= 9");
      evalTrue("9.358 <= 9.4");
      evalTrue("10-9 <= (4+2)");
      evalTrue("FIELD_INTEGER <= 100");
      evalTrue("FIELD_INTEGER <= 100.12");
      evalTrue("5 <= 5");

      evalTrue("false <= false");
      evalTrue("true <= true");
      evalTrue("false <= true");
      evalFalse("true <= false");

      evalTrue("'foo' <= 'foo'").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("'bar' <= 'foo'");
      evalFalse("'foo' <= 'bar'");

      evalTrue("DATE '2019-01-01' <= DATE '2019-02-01'");
      evalTrue("DATE '2019-01-01' <= DATE '2019-01-01'");
      evalFalse("DATE '2019-01-01' <= DATE '2018-01-01'");

      evalTrue("INTERVAL 3 DAYS <= INTERVAL 3 MONTHS");
      evalFalse("INTERVAL 3 YEARS <= INTERVAL 3 MONTHS");

      evalTrue("BINARY '4100' <= BINARY '4100'");
      evalTrue("BINARY '4100' <= BINARY '42'").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("BINARY '4100' <= BINARY '410000'");
      evalFalse("BINARY '410000' <= BINARY '4100'");
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("NULL_INTEGER <= FIELD_INTEGER");
      evalNull("FIELD_INTEGER <= NULL_INTEGER");
      evalNull("NULL_STRING <= Upper(FIELD_STRING)");
      evalNull("FIELD_STRING <= NULL_STRING");
    }

    @Test
    void testCoercion() throws Exception {
      // Compare numeric with implicit coercion from BOOLEAN
      evalTrue("TRUE<=1");
      evalTrue("1<=TRUE");
      evalTrue("TRUE<=1.1");
      evalFalse("TRUE<=0.5");

      // Compare numeric with implicit coercion from INTEGER and NUMBER
      evalFalse("2 <= 1.5");
      evalFalse("2.1 <= 2");
      evalTrue("1.5 <= 2");
      evalTrue("2 <= 2.1");
      evalTrue("2.1 <= 2.1");
    }

    @Test
    void testError() throws Exception {
      // Compare unordered type
      evalFails("FIELD_JSON <= FIELD_STRING", ErrorCode.ILLEGAL_ARGUMENT);

      // Conversion error
      evalFails("FIELD_STRING <=5", ErrorCode.CONVERSION_ERROR);

      // Syntax error
      evalFails("<= FIELD_INTEGER", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_INTEGER <=", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_INTEGER <= ", ErrorCode.SYNTAX_ERROR);
    }

    @Test
    void testOptimize() throws Exception {
      // Normalize
      optimize("FIELD_INTEGER<=5");
      optimize("5<=FIELD_INTEGER", "FIELD_INTEGER>=5");

      optimizeFalse("25<=12");
      optimize("FIELD_BOOLEAN_TRUE <= TRUE", "FIELD_BOOLEAN_TRUE IS NOT NULL");
      optimize("TRUE <= FIELD_BOOLEAN_TRUE", "FIELD_BOOLEAN_TRUE>=TRUE");
      optimize("FIELD_BOOLEAN_TRUE <= FALSE", "FIELD_BOOLEAN_TRUE<=FALSE");
      optimize("FALSE <= FIELD_BOOLEAN_TRUE", "FIELD_BOOLEAN_TRUE IS NOT NULL");
      optimize("FALSE <= FIELD_BOOLEAN_FALSE", "FIELD_BOOLEAN_FALSE IS NOT NULL");

      // Simplify arithmetic comparisons
      optimize("3<=FIELD_INTEGER+1", "FIELD_INTEGER>=2");
      optimize("FIELD_INTEGER+1>=3", "FIELD_INTEGER>=2");

      // Simplify comparison with the same term
      optimize("FIELD_STRING<=FIELD_STRING", "NVL2(FIELD_STRING,TRUE,NULL)");
    }
  }

  @Nested
  class InList {
    @Test
    void testPredicate() throws Exception {
      evalTrue("FIELD_STRING in ('?','*','TEST')").returnType(BooleanType.BOOLEAN);
      evalTrue("FIELD_STRING not in ('?','-','!')").returnType(BooleanType.BOOLEAN);
      evalTrue("FIELD_INTEGER not in (1,2,3)").returnType(BooleanType.BOOLEAN);

      evalTrue("2.5 IN (1,2.5,3)");
      evalTrue("2 in (NULL_INTEGER,1,2,FIELD_INTEGER)");
      evalTrue("TRUE IN (FALSE,NULL_BOOLEAN,TRUE)");
      evalTrue("DATE '2019-01-01' in (DATE '2019-04-01',DATE '2019-01-01',DATE '2019-03-06')");
      evalTrue(
          "DATE '2019-01-01' in (TIMESTAMP '2019-04-01 00:00:00',DATE '2019-01-01',DATE '2019-03-06')");
      evalTrue("BINARY '0123456789' in (BINARY '9876',BINARY '0123456789',BINARY '3698')");
      evalFalse("2 in (1,2.5,3)");

      evalFalse("2 in (NULL_INTEGER,NULL_NUMBER)");
      evalTrue("1 not in (2,3)");
      evalFalse("FIELD_INTEGER not in (40,2,3)");

      // c1 IN (c2, c3, NULL) is syntactically equivalent to (c1=c2 or c1=c3 or c1=NULL)
      // As a result, when the value of c1 is NULL, the expression c1 IN (c2, c3, NULL) always
      // evaluates to FALSE.
      evalFalse("FIELD_STRING in ('A','B',NULL_STRING)").returnType(BooleanType.BOOLEAN);
    }

    @Test
    void testNullHandling() throws Exception {
      // c1 NOT IN (c2, c3, NULL) evaluates to NULL
      // It is syntactically equivalent to (c1<>c2 AND c1<>c3 AND c1<>NULL)
      evalNull("FIELD_STRING not in ('A','B',NULL_STRING)").returnType(BooleanType.BOOLEAN);
      evalNull("1 not in (NULL_INTEGER,2)");
      evalNull("NULL_INTEGER in (1,2,3)");
      evalNull("NULL_INTEGER in (NULL_NUMBER,2,3,NULL_INTEGER)");
      evalNull("NULL_INTEGER in (NULL_NUMBER,2,3,NULL_INTEGER)");
    }

    @Test
    void testError() throws Exception {
      evalFails("FIELD_INTEGER in () ", ErrorCode.EMPTY_VALUE_LIST_FOR_IN_PREDICATE);
      evalFails("FIELD_INTEGER in (1,2,)", ErrorCode.MISSING_ELEMENT);
      evalFails("FIELD_INTEGER in (1,2.5,)", ErrorCode.MISSING_ELEMENT);

      evalFails(" in (1,2)", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_INTEGER in (,)    ", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_INTEGER in (,2,3)", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_INTEGER in (1,,3)", ErrorCode.SYNTAX_ERROR);

      evalFails("FIELD_INTEGER in 1,2)", ErrorCode.MISSING_LEFT_PARENTHESIS);
      evalFails("FIELD_INTEGER in 40", ErrorCode.MISSING_LEFT_PARENTHESIS);
      evalFails("FIELD_INTEGER in (1,2,3", ErrorCode.MISSING_RIGHT_PARENTHESIS);

      evalFails("FIELD_INTEGER in (1,2,FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_DATE in (1,2,3)", ErrorCode.ILLEGAL_ARGUMENT);
    }

    @Test
    void testOptimize() throws Exception {
      optimize("FIELD_INTEGER IN (10,20,30,40)");
      optimize("FIELD_INTEGER NOT IN (10,20,30,40)");
      optimizeTrue("'foo' IN ('bar', 'baz', 'foo', 'blah')");
      optimizeTrue("25 in (1,25,66)");
      optimizeTrue("1.15 IN (1.1, 1.2, 1.3, 1.15)");

      optimize(
          "FIELD_INTEGER in (1,2,1,2,3,4.1)", "CAST(FIELD_INTEGER AS NUMBER(2,1)) IN (1,2,3,4.1)");
      optimize("FIELD_NUMBER in (1,2,1,2,3,4.1)", "FIELD_NUMBER IN (1,2,3,4.1)");
      optimize("FIELD_STRING in ('1','2','1')", "FIELD_STRING IN ('1','2')");

      // optimize("2 in (1,2,3/0)", "2 in (1,2,3/0)");

      // Normalize IN list with a single element to comparison
      optimize("FIELD_INTEGER in (1)", "FIELD_INTEGER=1");
      optimize("FIELD_STRING in ('AB','AB')", "FIELD_STRING='AB'");

      optimize("(0 / 0) in (2, 2)", "2=0/0");

      // Value in the expression list
      // optimize("FIELD_INTEGER in (1,2,FIELD_INTEGER)", "NULL OR FIELD_INTEGER IS NOT NULL");
      // optimize("FIELD_STRING in ('XX',FIELD_STRING,'ZZ')", "NULL OR FIELD_STRING IS NOT NULL");
    }
  }

  @Nested
  class Positive {
    @Test
    void testArithmetic() throws Exception {
      evalEquals("+(40-1)", 39L);
      evalEquals("+FIELD_INTEGER", 40L).returnType(IntegerType.of(12));
      evalEquals("+FIELD_NUMBER", -5.12).returnType(NumberType.NUMBER);
      evalEquals("+FIELD_BIGNUMBER", 123456.789).returnType(NumberType.NUMBER);
      evalEquals("+40", 40L);
      evalEquals("1+ +2", 3L);
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("+NULL_INTEGER").returnType(IntegerType.INTEGER);
    }

    @Test
    void testOptimize() throws Exception {
      optimize("+FIELD_INTEGER", "FIELD_INTEGER");
      optimize("+(FIELD_INTEGER)", "FIELD_INTEGER");
    }
  }

  @Nested
  class Negative {
    @Test
    void testArithmetic() throws Exception {
      evalEquals("-(1+2)", -3L);
      evalEquals("-FIELD_INTEGER", -40L).returnType(IntegerType.of(12));
      evalEquals("-FIELD_NUMBER", 5.12).returnType(NumberType.NUMBER);
      evalEquals("-FIELD_BIGNUMBER", -123456.789).returnType(NumberType.NUMBER);
      evalEquals("1+ -2", -1L);
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("-NULL_INTEGER").returnType(IntegerType.INTEGER);
    }

    @Test
    void testError() throws Exception {
      // Arithmetic overflow
      evalFails("-CAST(-9223372036854775808 as INTEGER)", ErrorCode.ARITHMETIC_OVERFLOW);
    }

    @Test
    void testOptimize() throws Exception {
      optimize("-FIELD_INTEGER", "-FIELD_INTEGER");
      optimize("-(10+2)", "-12");
      optimize("-(1.1+1.2)", "-2.3");
      optimize("-(0)", "0");

      optimize("+(0 / 0)", "0/0");
      optimize("-(0 / 0)", "-(0/0)");
      optimize("-(-(0 / 0))", "0/0");

      optimize("-RANDOM()", "-RANDOM()");
      optimize("+RANDOM()", "RANDOM()");

      // Negative
      optimize("-(-FIELD_INTEGER)", "FIELD_INTEGER");
      optimize("-(FIELD_INTEGER-FIELD_NUMBER)", "FIELD_NUMBER-FIELD_INTEGER");
    }
  }

  @Nested
  class Div {
    @Test
    void testArithmetic() throws Exception {
      evalEquals("FIELD_INTEGER/4", 10D).returnType(NumberType.of(18, 6));
      evalEquals("10/4", 2.5D).returnType(NumberType.of(8, 6, false));
      evalEquals("40/-10", -4D).returnType(NumberType.of(8, 6, false));
      evalEquals("-40/-10", 4D).returnType(NumberType.of(8, 6, false));
      evalEquals("5/2", 2.5D).returnType(NumberType.of(7, 6, false));
      evalEquals("10.1/2.1", new BigDecimal("4.8095238095238095238095238095238"))
          .returnType(NumberType.of(9, 6, false));
      evalEquals("0.1/0.0000000000001", 1000000000000D).returnType(NumberType.of(30, 16, false));
      evalEquals("FIELD_NUMBER::NUMBER(4,1)/3::NUMBER(3,2)", -1.7D)
          .returnType(NumberType.of(11, 6));
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("NULL_INTEGER/1");
      evalNull("NULL_INTEGER/0");
      evalNull("1/NULL_INTEGER");
    }

    @Test
    void testError() throws Exception {
      // Division by zero
      evalFails("40/0", ErrorCode.DIVISION_BY_ZERO);
    }

    @Test
    void testCoercion() throws Exception {
      // Implicit coercion from BOOLEAN
      evalEquals("8/TRUE", 8D).returnType(NumberType.of(7, 6, false));
      evalEquals("TRUE/2", 0.5D).returnType(NumberType.of(7, 6, false));
    }

    @Test
    void testOptimize() throws Exception {
      // Normalize
      optimize("0/0");
      optimize("FIELD_INTEGER/4");

      // Simplify arithmetic with NULL
      optimizeNull("NULL::INTEGER/FIELD_INTEGER");
      optimizeNull("FIELD_INTEGER/NULL::INTEGER");

      // Simplify arithmetic A / 1 → A
      optimize("FIELD_INTEGER/1", "FIELD_INTEGER");
      optimize("FIELD_INTEGER/1.0", "FIELD_INTEGER");

      // Simplify arithmetic (-A) / (-B) → A / B
      optimize("-FIELD_NUMBER/-FIELD_INTEGER", "FIELD_NUMBER/FIELD_INTEGER");
      optimize("-FIELD_NUMBER/FIELD_INTEGER", "-FIELD_NUMBER/FIELD_INTEGER");
      optimize("FIELD_NUMBER/-FIELD_INTEGER", "FIELD_NUMBER/-FIELD_INTEGER");

      // Simplify arithmetic (A * B) / C → (A / C) * B (if A and C are constants)
      optimize("(4*FIELD_NUMBER)/2", "2*FIELD_NUMBER");
      // optimize("((FIELD_NUMBER*4)*FIELD_INTEGER)/2", "2*FIELD_NUMBER*FIELD_INTEGER");
      // optimize("(FIELD_NUMBER*(FIELD_INTEGER*4))/2", "2*FIELD_NUMBER*FIELD_INTEGER");

      // Simplify arithmetic (A / B) / C → A / (B * C) (if B and C are constants)
      optimize("(FIELD_NUMBER/4)/2", "FIELD_NUMBER/8");
      optimize("((FIELD_NUMBER/4)/FIELD_INTEGER)/2", "FIELD_NUMBER/FIELD_INTEGER/8");
    }
  }

  @Nested
  class BoolNot {
    @Test
    void testPredicate() throws Exception {
      evalTrue("FIELD_BOOLEAN_TRUE is not false").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("NULL_BOOLEAN is null").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("NOT (NULL_BOOLEAN is not null)").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalFalse("NOT 1").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("NOT 0").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalFalse("NOT FIELD_INTEGER").returnType(BooleanType.BOOLEAN);
      evalTrue("NOT FIELD_STRING_BOOLEAN_FALSE::BOOLEAN").returnType(BooleanType.BOOLEAN);
      evalFalse("NOT FIELD_STRING_BOOLEAN_TRUE::BOOLEAN").returnType(BooleanType.BOOLEAN);
      evalTrue("NOT NOT True").returnType(BooleanType.BOOLEAN_NOT_NULL);

      // Function syntax
      evalTrue("NOT(FALSE)").returnType(BooleanType.BOOLEAN_NOT_NULL);
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("NOT NULL_BOOLEAN").returnType(BooleanType.BOOLEAN);
    }

    @Test
    void testError() throws Exception {
      evalFails("FIELD_BOOLEAN_TRUE is ", ErrorCode.SYNTAX_ERROR);
      evalFails("NOT", ErrorCode.SYNTAX_ERROR);
    }

    @Test
    void testOptimize() throws Exception {
      optimize("NOT FIELD_BOOLEAN_TRUE");
      optimize("NOT NOT FIELD_BOOLEAN_TRUE", "FIELD_BOOLEAN_TRUE");
      optimize("NOT NOT NOT FIELD_BOOLEAN_TRUE", "NOT FIELD_BOOLEAN_TRUE");
      optimizeFalse("not true");
      optimizeTrue("not false");
      optimizeTrue("not not true");
      optimizeFalse("not not false");
      optimize("NOT (NOT(FIELD_BOOLEAN_TRUE))", "FIELD_BOOLEAN_TRUE");
      optimize("NOT (FIELD_INTEGER>5)", "FIELD_INTEGER<=5");
      optimize("NOT (FIELD_INTEGER>=5)", "FIELD_INTEGER<5");
      optimize("NOT (FIELD_INTEGER<5)", "FIELD_INTEGER>=5");
      optimize("NOT (FIELD_INTEGER<=5)", "FIELD_INTEGER>5");
      optimize("NOT (FIELD_INTEGER=5)", "FIELD_INTEGER!=5");
      optimize("NOT (FIELD_INTEGER<>5)", "FIELD_INTEGER=5");
      optimize("NOT (FIELD_BOOLEAN_TRUE IS TRUE)", "FIELD_BOOLEAN_TRUE IS NOT TRUE");
      optimize("NOT (FIELD_BOOLEAN_TRUE IS NOT TRUE)", "FIELD_BOOLEAN_TRUE IS TRUE");
      optimize("NOT (FIELD_BOOLEAN_TRUE IS FALSE)", "FIELD_BOOLEAN_TRUE IS NOT FALSE");
      optimize("NOT (FIELD_BOOLEAN_TRUE IS NOT FALSE)", "FIELD_BOOLEAN_TRUE IS FALSE");
      optimize("NOT (FIELD_BOOLEAN_TRUE IS NOT NULL)", "FIELD_BOOLEAN_TRUE IS NULL");
      optimize("NOT (FIELD_BOOLEAN_TRUE IS NULL)", "FIELD_BOOLEAN_TRUE IS NOT NULL");
      optimize(
          "NOT (FIELD_BOOLEAN_TRUE IS DISTINCT FROM NULL_BOOLEAN)",
          "FIELD_BOOLEAN_TRUE IS NOT DISTINCT FROM NULL_BOOLEAN");
      optimize(
          "NOT (FIELD_BOOLEAN_TRUE IS NOT DISTINCT FROM NULL_BOOLEAN)",
          "FIELD_BOOLEAN_TRUE IS DISTINCT FROM NULL_BOOLEAN");
      optimize(
          "NOT (FIELD_STRING NOT SIMILAR TO '.*(b|d).*')", "FIELD_STRING SIMILAR TO '.*(b|d).*'");
      optimize(
          "NOT (FIELD_STRING SIMILAR TO '.*(b|d).*')", "FIELD_STRING NOT SIMILAR TO '.*(b|d).*'");
      optimize("NOT (FIELD_STRING IN ('A','B'))", "FIELD_STRING NOT IN ('A','B')");
      optimize("NOT (FIELD_STRING NOT IN ('A','B'))", "FIELD_STRING IN ('A','B')");

      // optimize("(A IS NOT NULL OR B) AND FIELD_BOOLEAN_TRUE IS NOT NULL","FIELD_BOOLEAN_TRUE IS
      // NOT
      // NULL");
    }
  }

  @Nested
  class BoolXor {
    @Test
    void testPredicate() throws Exception {
      evalFalse("true XOR true").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalFalse("false XOR false").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("true XOR false").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("false XOR true").returnType(BooleanType.BOOLEAN_NOT_NULL);
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("NULL XOR true");
      evalNull("true XOR NULLIF(true,true)");
      evalNull("false XOR NULLIF(true,true)");
    }

    @Test
    void testError() throws Exception {
      evalFails(" XOR true", ErrorCode.SYNTAX_ERROR);
      evalFails("XOR false", ErrorCode.SYNTAX_ERROR);
      evalFails("true XOR Date '2024-01-01'", ErrorCode.ILLEGAL_ARGUMENT);
    }
  }

  @Nested
  class BoolOr {
    @Test
    void testPredicate() throws Exception {
      evalTrue("true OR true").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("true OR false");
      evalTrue("false OR true");
      evalFalse("false OR false").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("true OR NULL_BOOLEAN");
      evalTrue("true OR FIELD_BOOLEAN_TRUE");
      evalTrue("NULL_BOOLEAN OR true");
      evalTrue("FIELD_BOOLEAN_TRUE OR false");
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("false OR NULL_BOOLEAN").returnType(BooleanType.BOOLEAN);
      evalNull("NULL_BOOLEAN OR false");
      evalNull("NULL_BOOLEAN OR NULL_BOOLEAN");
    }

    @Test
    void testError() throws Exception {
      evalFails("false OR", ErrorCode.SYNTAX_ERROR);
      evalFails("OR false", ErrorCode.SYNTAX_ERROR);
      evalFails("true OR Date '2024-01-01'", ErrorCode.ILLEGAL_ARGUMENT);
    }

    @Test
    void testOptimize() throws Exception {
      // Simplify literal
      optimizeTrue("true or true");
      optimizeTrue("true or false");
      optimizeTrue("false or true");
      optimizeFalse("false or false");
      optimizeTrue("FIELD_BOOLEAN_TRUE or true");
      optimizeTrue("true or FIELD_BOOLEAN_FALSE");
      optimizeTrue("true or FIELD_BOOLEAN_TRUE");
      optimizeTrue("FIELD_BOOLEAN_TRUE or true");
      optimize("false or FIELD_BOOLEAN_TRUE", "FIELD_BOOLEAN_TRUE");
      optimize("FIELD_BOOLEAN_TRUE or false", "FIELD_BOOLEAN_TRUE");

      optimize("FIELD_BOOLEAN_TRUE OR NULL_BOOLEAN");
      optimize(
          "FIELD_BOOLEAN_TRUE OR NULL_BOOLEAN OR (FIELD_INTEGER>0) OR FIELD_BOOLEAN_TRUE",
          "FIELD_BOOLEAN_TRUE OR NULL_BOOLEAN OR FIELD_INTEGER>0");
      optimize("false and true or FIELD_BOOLEAN_TRUE", "FIELD_BOOLEAN_TRUE");

      // Simplify NULL
      optimizeTrue("NULL::Boolean OR TRUE");
      optimizeTrue("TRUE OR NULL::Boolean");
      optimizeTrue("true or NULL_BOOLEAN");
      optimizeTrue("NULL_BOOLEAN or true");

      optimizeNull("FALSE OR NULL::Boolean");
      optimizeNull("NULL::Boolean OR FALSE");

      // Simplify duplicate predicate
      optimize("FIELD_BOOLEAN_TRUE OR FIELD_BOOLEAN_TRUE", "FIELD_BOOLEAN_TRUE");
      optimize("FIELD_INTEGER=2 OR 2=FIELD_INTEGER", "FIELD_INTEGER=2");
      optimize("NULL_BOOLEAN or NULL_BOOLEAN", "NULL_BOOLEAN");

      // Check if simplify doesn't create infinity loop with the same operator if order change
      optimize(
          "FIELD_STRING LIKE 'AB%' OR FIELD_STRING LIKE 'BC%' OR FIELD_STRING LIKE '%DE' ",
          "ENDSWITH(FIELD_STRING,'DE') OR STARTSWITH(FIELD_STRING,'AB') OR STARTSWITH(FIELD_STRING,'BC')");

      // Simplify x < a OR x = a → x <= a
      optimize("FIELD_INTEGER<1 OR FIELD_INTEGER=1", "FIELD_INTEGER<=1");
      // Simplify x < a OR x != a → x != a
      optimize("FIELD_INTEGER<1 OR FIELD_INTEGER!=1", "FIELD_INTEGER!=1");
      // Simplify x < a OR x > a → x != a"
      optimize("FIELD_INTEGER<1 OR FIELD_INTEGER>1", "FIELD_INTEGER!=1");
      // Simplify x > a OR x != a → x != a
      optimize("FIELD_INTEGER>1 OR FIELD_INTEGER!=1", "FIELD_INTEGER!=1");
      // Simplify x > a OR x = a → x >= a
      optimize("FIELD_INTEGER>1 OR FIELD_INTEGER=1", "FIELD_INTEGER>=1");
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

      // Simplify union X=1 OR X=2 OR X=3 → X IN (1,2,3) order is not important
      optimize("FIELD_INTEGER=1 OR FIELD_INTEGER=2 OR FIELD_INTEGER=3", "FIELD_INTEGER IN (3,1,2)");
      optimize("FIELD_INTEGER=1 OR FIELD_INTEGER in (2,3)", "FIELD_INTEGER IN (1,2,3)");
      optimize("FIELD_INTEGER IN (1,2) OR FIELD_INTEGER IN (3,4)", "FIELD_INTEGER IN (1,2,3,4)");
      optimize(
          "FIELD_STRING='1' OR NULL_INTEGER in (1,2)", "FIELD_STRING='1' OR NULL_INTEGER IN (1,2)");

      // Simplify intersection != and NOT IN
      optimize("FIELD_INTEGER!=2 OR FIELD_INTEGER NOT IN (1,2,3,4)", "FIELD_INTEGER!=2");
      optimize(
          "FIELD_INTEGER NOT IN (1,2,3) OR FIELD_INTEGER NOT IN (2,3,4,5)",
          "FIELD_INTEGER NOT IN (3,2)");
    }
  }

  @Nested
  class BoolAnd {
    @Test
    void testPredicate() throws Exception {
      evalTrue("true AND true");
      evalTrue("true AND FIELD_BOOLEAN_TRUE");
      evalFalse("true AND false");
      evalFalse("false AND true");
      evalFalse("false AND false");
      evalFalse("false AND FIELD_BOOLEAN_TRUE");
      evalFalse("false AND NULL_BOOLEAN");
      evalFalse("NULL_BOOLEAN AND false");
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("FIELD_BOOLEAN_TRUE AND NULL_BOOLEAN");
      evalNull("NULL_BOOLEAN AND FIELD_BOOLEAN_TRUE");
      evalNull("true AND NULL_BOOLEAN");
      evalNull("NULL_BOOLEAN AND true");
      evalNull("NULL_BOOLEAN AND FIELD_BOOLEAN_TRUE");
    }

    @Test
    void testError() throws Exception {
      evalFails("false AND", ErrorCode.SYNTAX_ERROR);
      evalFails("AND false", ErrorCode.SYNTAX_ERROR);
      evalFails("true AND Date '2024-01-01'", ErrorCode.ILLEGAL_ARGUMENT);
    }

    @Test
    void testOptimize() throws Exception {
      optimize("FIELD_BOOLEAN_TRUE AND NULL_BOOLEAN");
      optimizeTrue("25>=12 and 14<15");
      optimizeTrue("true and true");
      optimizeFalse("true and false");
      optimizeFalse("false and true");
      optimizeFalse("false and false");

      optimizeFalse("false and NULLIF(true,true)");
      optimizeFalse("false and FIELD_BOOLEAN_TRUE");
      optimizeFalse("FIELD_BOOLEAN_TRUE and false");
      optimizeFalse("NULLIF(true,true) AND NULLIF(true,true) AND NULLIF(true,true) AND false");
      optimizeNull("null AND null AND null AND true");

      // Simplify NULL
      optimizeNull("NULL::Boolean AND TRUE");
      optimizeNull("TRUE AND NULL::Boolean");
      optimizeFalse("FALSE AND NULL::Boolean");
      optimizeFalse("NULL::Boolean AND FALSE");

      // Simplify duplicate predicate
      optimize("FIELD_BOOLEAN_TRUE and FIELD_BOOLEAN_TRUE", "FIELD_BOOLEAN_TRUE");
      optimize(
          "FIELD_BOOLEAN_TRUE AND NULL_BOOLEAN AND (FIELD_INTEGER>0) AND FIELD_BOOLEAN_TRUE",
          "FIELD_BOOLEAN_TRUE AND NULL_BOOLEAN AND FIELD_INTEGER>0");
      optimize(
          "(FIELD_INTEGER*2>1) AND FIELD_BOOLEAN_TRUE AND (2*FIELD_INTEGER>1)",
          "FIELD_BOOLEAN_TRUE AND 1<2*FIELD_INTEGER");
      optimize(
          "FIELD_INTEGER=1 AND FIELD_BOOLEAN_TRUE AND FIELD_INTEGER=1",
          "FIELD_BOOLEAN_TRUE AND FIELD_INTEGER=1");
      optimize(
          "(FIELD_INTEGER*2>1) AND FIELD_BOOLEAN_TRUE AND (2*FIELD_INTEGER>1)",
          "FIELD_BOOLEAN_TRUE AND 1<2*FIELD_INTEGER");

      // Check if simplify doesn't create infinity loop with the same operator if order change
      optimize(
          "FIELD_STRING LIKE 'AB%' AND FIELD_STRING LIKE 'BC%' AND FIELD_STRING LIKE '%DE' ",
          "ENDSWITH(FIELD_STRING,'DE') AND STARTSWITH(FIELD_STRING,'AB') AND STARTSWITH(FIELD_STRING,'BC')");

      // Simplify IS NULL
      optimizeFalse("FIELD_INTEGER IS NULL AND FIELD_INTEGER>5");
      optimizeFalse("FIELD_INTEGER IS NULL AND FIELD_INTEGER=5");
      optimizeFalse("FIELD_INTEGER IS NULL AND FIELD_INTEGER<>5");

      // Simplify IS NOT NULL
      optimize(
          "FIELD_INTEGER>5 AND FIELD_INTEGER IS NOT NULL AND FIELD_INTEGER>5", "FIELD_INTEGER>5");

      // Simplify union <> and NOT IN
      optimize(
          "FIELD_INTEGER<>1 AND FIELD_INTEGER<>2 AND FIELD_INTEGER<>3",
          "FIELD_INTEGER NOT IN (3,1,2)");
      optimize("FIELD_INTEGER<>1 AND FIELD_INTEGER NOT IN (2,3)", "FIELD_INTEGER NOT IN (1,2,3)");
      optimize(
          "FIELD_INTEGER NOT IN (1,2,3) AND FIELD_INTEGER NOT IN (4,5)",
          "FIELD_INTEGER NOT IN (4,5,1,2,3)");

      // Simplify intersection = and IN
      optimize("FIELD_INTEGER IN (1,2,3) AND FIELD_INTEGER IN (3,4,5)", "FIELD_INTEGER=3");
      optimize("FIELD_INTEGER IN (1,2,3) AND FIELD_INTEGER IN (4,5,3,1)", "FIELD_INTEGER IN (3,1)");
      optimize(
          "FIELD_INTEGER IN (1,2,3,5) AND FIELD_INTEGER IN (2,3,4,5)", "FIELD_INTEGER IN (3,2,5)");
      optimizeFalse("FIELD_INTEGER IN (1,2,3) AND FIELD_INTEGER IN (4,5)");
      optimizeFalse("FIELD_STRING IN ('A','B','C') AND FIELD_STRING='X'");
      optimizeFalse("FIELD_INTEGER=1 AND FIELD_BOOLEAN_TRUE AND FIELD_INTEGER=2");
      optimizeFalse("NULL_INTEGER=1 AND FIELD_BOOLEAN_TRUE AND NULL_INTEGER=2");

      // Simplify intersection with exclusions != and NOT IN
      optimize("FIELD_INTEGER IN (1,2,3) AND FIELD_INTEGER!=3", "FIELD_INTEGER IN (1,2)");
      optimize(
          "FIELD_INTEGER IN (1,2,3) AND FIELD_INTEGER NOT IN (3,4,5)", "FIELD_INTEGER IN (1,2)");
    }
  }

  @Nested
  class ILike {
    @Test
    void testPredicate() throws Exception {
      evalTrue("'test' ILIKE '%t%'");
      evalTrue("'test' ILIKE '%T%'");

      // Optimizable
      evalTrue("'ABCDEFG' ILIKE 'abcdefg'");
      evalTrue("'ABCDEFG' ILIKE 'abcde%'");
      evalTrue("'ABCDEFG' ILIKE '%defg'");
      evalTrue("'ABCDEFG' ILIKE '%cde%'");
    }

    @Test
    void testEscapeChar() throws Exception {

      // Escape with other char
      evalTrue("'Result 100% value' ilike 'RESULT%100^%%' escape '^'");
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("'test' ILIKE NULL_STRING");
      evalNull("NULL_STRING ILIKE '%T%'");
    }

    @Test
    void testError() throws Exception {
      evalFails("'a' ILIKE '%' ESCAPE NULL", ErrorCode.INVALID_ARGUMENT);
      evalFails("'a' ILIKE '%' ESCAPE NULL_STRING", ErrorCode.INVALID_ARGUMENT);
    }

    @Test
    void testOptimize() throws Exception {
      optimize("FIELD_STRING ILIKE 'AB%C'");
      optimize("FIELD_STRING ILIKE '%ADC!_%' ESCAPE '!'");
      optimize("FIELD_STRING ILIKE 'AB%%C'", "FIELD_STRING ILIKE 'AB%C'");
      optimize("FIELD_STRING ILIKE 'AB%%%%C'", "FIELD_STRING ILIKE 'AB%C'");
      optimize("FIELD_STRING ILIKE '%%%AB%%C%%%'", "FIELD_STRING ILIKE '%AB%C%'");
      optimize("FIELD_STRING ILIKE '\\%%%AB%%C%%\\%'", "FIELD_STRING ILIKE '\\%%AB%C%\\%'");
    }
  }

  @Nested
  class CaseSearch {
    @Test
    void testCondition() throws Exception {

      evalEquals("case when TRUE then 1 else 2 end", 1L).returnType(IntegerType.of(1, false));
      evalEquals("case when FALSE then 1 else 2 end", 2L).returnType(IntegerType.of(1, false));

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

      evalNull("case when NULL_INTEGER is NULL then NULL else 1 end")
          .returnType(IntegerType.of(1, false));
    }

    @Test
    void testError() throws Exception {
      // Missing 'END'
      evalFails(
          "case when FIELD_INTEGER=40 then 10 else 50", ErrorCode.SYNTAX_ERROR_CASE_STATEMENT);

      // Incompatible return type
      evalFails(
          "case when FIELD_INTEGER=40 then 10 else 'Error'", ErrorCode.SYNTAX_ERROR_CASE_STATEMENT);

      // Unknown return type
      evalFails("case when false then NULL end", ErrorCode.ILLEGAL_ARGUMENT);
    }

    @Test
    void testOptimize() throws Exception {
      // Literal
      optimizeFalse("(CASE WHEN FALSE THEN 1 ELSE 2 END) IS NULL");

      // Implicit ELSE NULL
      optimize(
          "CASE WHEN FIELD_INTEGER>40 THEN 10 WHEN FIELD_INTEGER>20 THEN 5 ELSE NULL END",
          "CASE WHEN FIELD_INTEGER>40 THEN 10 WHEN FIELD_INTEGER>20 THEN 5 END");
      optimize("CASE WHEN FIELD_INTEGER=40 THEN TRUE ELSE FALSE END");

      // Flatten search case
      optimize(
          "CASE WHEN FIELD_INTEGER=1 THEN 1 ELSE CASE WHEN FIELD_NUMBER=2 THEN 2 ELSE 3 END END",
          "CASE WHEN FIELD_INTEGER=1 THEN 1 WHEN FIELD_NUMBER=2 THEN 2 ELSE 3 END");

      // "CASE WHEN x IS NULL THEN y ELSE x END" to "IFNULL(x, y)"
      optimize(
          "CASE WHEN FIELD_STRING IS NULL THEN 'TEST' ELSE FIELD_STRING END",
          "IFNULL(FIELD_STRING,'TEST')");

      // "CASE WHEN x = y THEN NULL ELSE x END" to "NULLIF(x, y)"
      optimize(
          "CASE WHEN FIELD_INTEGER=10 THEN NULL ELSE FIELD_INTEGER END",
          "NULLIF(FIELD_INTEGER,10)");
      optimize(
          "CASE WHEN 0.5=FIELD_NUMBER THEN NULL ELSE FIELD_NUMBER END", "NULLIF(FIELD_NUMBER,0.5)");

      // "CASE WHEN x IS NOT NULL THEN y ELSE z END" to "NVL2(x, y, z)"
      optimize(
          "CASE WHEN FIELD_INTEGER IS NOT NULL THEN FIELD_STRING ELSE 'TEST' END",
          "NVL2(FIELD_INTEGER,FIELD_STRING,'TEST')");

      // "CASE WHEN x IS NULL THEN y ELSE z END" to "NVL2(x, z, y)"
      optimize(
          "CASE WHEN FIELD_INTEGER IS NULL THEN FIELD_STRING ELSE 'TEST' END",
          "NVL2(FIELD_INTEGER,'TEST',FIELD_STRING)");

      // Search case to simple case: CASE WHEN a = b THEN 1 END to CASE a WHEN b THEN 1 END
      // optimize("CASE WHEN FIELD_INTEGER=1 THEN 2 END", "CASE 1 WHEN FIELD_INTEGER THEN 2 END");

    }
  }

  @Nested
  class CaseSimple {
    @Test
    void testCondition() throws Exception {

      evalEquals("case FIELD_INTEGER when 10 then 10 when 40 then 40 else 50 end", 40L)
          .returnType(IntegerType.of(2, false));
      evalEquals("case FIELD_INTEGER when 10 then 10 when 20 then 20 else -1 end", -1L)
          .returnType(IntegerType.of(2, false));
      evalNull("case FIELD_INTEGER when 10 then 10 when 20 then 20 end")
          .returnType(IntegerType.of(2));

      // If the operand is null, the else clause applies
      evalFalse("CASE NULL_INTEGER WHEN 1 THEN TRUE ELSE FALSE END")
          .returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalEquals("CASE NULL_NUMBER WHEN 0 THEN 0 ELSE 1 END", 1L).returnType(IntegerType.INTEGER);
      evalEquals("CASE NULL_NUMBER WHEN 0 THEN 1.023 ELSE 1 END", BigDecimal.ONE)
          .returnType(NumberType.NUMBER);
      evalEquals("CASE NULL_STRING WHEN 'A' THEN 'A' ELSE 'B' END", "B")
          .returnType(StringType.STRING);

      // Check null data type returned
      evalNull("CASE NULL_STRING WHEN 'A' THEN 'A' ELSE NULL END").returnType(StringType.STRING);

      // Ignore division by zero in THEN term, should not evaluate
      evalEquals("CASE NULL_NUMBER WHEN 0 THEN 0/0 ELSE 1 END", 1D);
      evalEquals("CASE NULL_INTEGER WHEN 0 THEN 0 / 0 ELSE 1 END", 1D);
      evalEquals("CASE 1 WHEN 2 THEN 0 / 0 ELSE 3 END", 3D);

      // Ignore division by zero in ELSE term,
      evalEquals("CASE 1 WHEN 1 THEN 2 WHEN 1 THEN 0 / 0 END", 2D);
      evalEquals("CASE 1 WHEN 1 THEN 2 ELSE 0 / 0 END", 2D);

      // Simple case form with multi-value
      evalEquals(
              "CASE FIELD_INTEGER WHEN 10, 20 THEN 'A' WHEN 40, 50, 60, 70 THEN 'B' ELSE 'C' END",
              "B")
          .returnType(StringType.of(1).withNullability(false));
      evalEquals(
          "CASE FIELD_INTEGER WHEN 10, 20.23 THEN 'A' WHEN 30, 40, 50, 60, 70 THEN 'B' ELSE 'C' END",
          "B");
      evalEquals("CASE FIELD_INTEGER WHEN 10, 20 THEN 'A' WHEN 30, 40 THEN 'B' ELSE 'C' END", "B");

      evalEquals(
              "CASE WHEN FIELD_INTEGER IS NULL THEN '-' WHEN FIELD_INTEGER<0 THEN '' ELSE FIELD_STRING END",
              "TEST")
          .returnType(StringType.of(1000));
      evalEquals("CASE WHEN NULL_INTEGER IS NULL THEN 0 ELSE FIELD_INTEGER END", 0L)
          .returnType(IntegerType.of(12));
      evalEquals("CASE WHEN NULL_INTEGER IS NULL THEN 0 ELSE FIELD_NUMBER END", 0D)
          .returnType(NumberType.NUMBER);
      evalEquals(
              "CASE WHEN NULL_INTEGER IS NULL THEN Date '2023-01-01' ELSE FIELD_DATE END",
              LocalDate.of(2023, 1, 1))
          .returnType(DateType.DATE);
    }

    @Test
    void testError() throws Exception {
      // Coerce
      evalFails(
          "case FIELD_INTEGER when 10 then 1 when 40.1 then 2.123 else 0.5",
          ErrorCode.SYNTAX_ERROR_CASE_STATEMENT);

      // TODO: Incompatible return type
      evalFails(
          "case FIELD_INTEGER when 10 then 'X' when ' T' then 'Test' else 'Error' end",
          ErrorCode.CONVERSION_ERROR);

      // Missing 'END'
      evalFails(
          "case FIELD_INTEGER when 40 then 10 else 50", ErrorCode.SYNTAX_ERROR_CASE_STATEMENT);
      evalFails("case FIELD_INTEGER then 10 else 50", ErrorCode.SYNTAX_ERROR_CASE_STATEMENT);
    }

    @Test
    void testOptimize() throws Exception {
      optimize("CASE FIELD_INTEGER WHEN 40 THEN 'A' WHEN 20 THEN 'B' ELSE 'C' END");
      // Multi values
      optimize("CASE FIELD_INTEGER WHEN 1,2,3 THEN 'A' WHEN 4,5,6,7 THEN 'B' ELSE 'C' END");
    }
  }

  @Nested
  class IsTrue {
    @Test
    void testPredicate() throws Exception {
      evalTrue("True IS True").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalFalse("True IS False").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("FIELD_BOOLEAN_TRUE is True").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalFalse("FIELD_STRING='XX' IS TRUE").returnType(BooleanType.BOOLEAN_NOT_NULL);
    }

    @Test
    void testNullHandling() throws Exception {
      evalFalse("NULL_BOOLEAN is True");
    }

    @Test
    void testError() throws Exception {
      evalFails("FIELD_BOOLEAN_TRUE IS ", ErrorCode.SYNTAX_ERROR);
      evalFails("IS TRUE", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_STRING_BOOLEAN_TRUE IS TRUE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_DATE IS TRUE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_TIMESTAMP IS TRUE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_BINARY IS TRUE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_JSON IS TRUE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_INET IS TRUE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_JSON IS TRUE", ErrorCode.ILLEGAL_ARGUMENT);
    }

    @Test
    void testOptimize() throws Exception {
      optimize("FIELD_BOOLEAN_TRUE IS TRUE");
      optimizeTrue("true is true");
      optimizeFalse("false is true");
    }
  }

  @Nested
  class IsNotTrue {
    @Test
    void testPredicate() throws Exception {
      evalFalse("True IS NOT True").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("True IS NOT False").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalFalse("FIELD_BOOLEAN_TRUE is not True").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("FIELD_STRING='XX' IS NOT TRUE");
    }

    @Test
    void testNullHandling() throws Exception {
      evalTrue("NULL_BOOLEAN IS NOT True");
    }

    @Test
    void testError() throws Exception {
      evalFails("FIELD_BOOLEAN IS NOT", ErrorCode.SYNTAX_ERROR);
      evalFails("IS NOT TRUE", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_STRING_BOOLEAN_TRUE IS NOT TRUE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_DATE IS NOT TRUE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_TIMESTAMP IS NOT TRUE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_BINARY IS NOT TRUE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_JSON IS NOT TRUE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_INET IS NOT TRUE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_JSON IS NOT TRUE", ErrorCode.ILLEGAL_ARGUMENT);
    }

    @Test
    void testOptimize() throws Exception {
      optimize("FIELD_BOOLEAN_TRUE IS NOT TRUE");
      optimizeFalse("true is not true");
      optimizeTrue("false is not true");
    }
  }

  @Nested
  class IsFalse {
    @Test
    void testPredicate() throws Exception {
      evalFalse("FIELD_BOOLEAN_TRUE IS FALSE").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("FIELD_STRING='XX' IS FALSE");
    }

    @Test
    void testNullHandling() throws Exception {
      evalFalse("NULL_BOOLEAN IS False");
    }

    @Test
    void testError() throws Exception {
      evalFails("FIELD_BOOLEAN_TRUE FALSE", ErrorCode.UNEXPECTED_CHARACTER);
      evalFails("IS FALSE", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_STRING_BOOLEAN_TRUE IS FALSE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_DATE IS FALSE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_TIMESTAMP IS FALSE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_BINARY IS FALSE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_JSON IS FALSE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_INET IS FALSE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_JSON IS FALSE", ErrorCode.ILLEGAL_ARGUMENT);
    }

    @Test
    void testOptimize() throws Exception {
      optimize("FIELD_BOOLEAN_TRUE IS FALSE");
      optimizeTrue("false is false");
      optimizeFalse("true is false");
    }
  }

  @Nested
  class IsNotFalse {
    @Test
    void testPredicate() throws Exception {
      evalTrue("FIELD_BOOLEAN_TRUE IS NOT FALSE").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalFalse("FIELD_STRING='XX' IS NOT FALSE");
    }

    @Test
    void testNullHandling() throws Exception {
      evalTrue("NULL_BOOLEAN IS NOT False");
    }

    @Test
    void testError() throws Exception {
      evalFails("FIELD_BOOLEAN_TRUE NOT FALSE", ErrorCode.SYNTAX_ERROR);
      evalFails("IS NOT FALSE", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_STRING_BOOLEAN_TRUE IS NOT FALSE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_DATE IS NOT FALSE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_TIMESTAMP IS NOT FALSE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_BINARY IS NOT FALSE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_JSON IS NOT FALSE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_INET IS NOT FALSE", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_JSON IS NOT FALSE", ErrorCode.ILLEGAL_ARGUMENT);
    }

    @Test
    void testOptimize() throws Exception {
      optimize("FIELD_BOOLEAN_TRUE IS NOT FALSE");
      optimizeFalse("false is not false");
      optimizeTrue("true is not false");
    }
  }

  @Nested
  class IsNull {
    @Test
    void testPredicate() throws Exception {
      evalFalse("True IS Null");
      evalFalse("False IS Null");
      evalTrue("NULL_INTEGER IS NULL");
      evalTrue("NULL_STRING IS NULL");
      evalTrue("NULL_BOOLEAN IS NULL");
      evalFalse("FIELD_BOOLEAN_TRUE IS NULL").returnType(BooleanType.BOOLEAN_NOT_NULL);
    }

    @Test
    void testError() throws Exception {
      evalFails("IS NULL", ErrorCode.SYNTAX_ERROR);
    }

    @Test
    void testOptimize() throws Exception {
      optimize("FIELD_BOOLEAN_TRUE IS NULL");
      optimizeFalse("true is null");
      optimizeFalse("false is null");
      optimizeTrue("NULL IS NULL");
      optimizeTrue("NULL IS NULL");
      optimizeFalse("TRUE IS NULL");
      optimizeFalse("1 IS NULL");
      optimizeFalse("Random() IS NULL");

      // If the operator return type is not nullable
      optimizeFalse("CURRENT_DATE() IS NULL");
      optimizeFalse("UUID() IS NULL");

      // CAST(x AS type) IS NULL → x IS NULL
      optimize("CAST(FIELD_NUMBER AS STRING) IS NULL", "FIELD_NUMBER IS NULL");
    }
  }

  @Nested
  class IsNotNull {
    @Test
    void testPredicate() throws Exception {
      evalTrue("True IS NOT Null").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("False IS NOT Null");
      evalFalse("NULL_BOOLEAN IS NOT NULL");
      evalFalse("NULL_INTEGER IS NOT NULL");
      evalFalse("NULL_STRING IS NOT NULL");
      evalFalse("NULL_DATE IS NOT NULL");
      evalFalse("NULL_BOOLEAN IS NOT NULL");
      evalTrue("FIELD_BOOLEAN_TRUE IS NOT NULL").returnType(BooleanType.BOOLEAN_NOT_NULL);
    }

    @Test
    void testError() throws Exception {
      evalFails("IS NOT NULL", ErrorCode.SYNTAX_ERROR);
    }

    @Test
    void testOptimize() throws Exception {
      optimize("FIELD_BOOLEAN_TRUE IS NOT NULL");
      optimizeFalse("NULL+1 IS NOT NULL");
      optimizeTrue("TRUE IS NOT NULL");
      optimizeFalse("TRUE IS NULL");
      optimizeTrue("1 IS NOT NULL");
      optimizeTrue("Random() IS NOT NULL");

      // If the operator return type is not nullable
      optimizeTrue("CURRENT_DATE() IS NOT NULL");
      optimizeTrue("UUID() IS NOT NULL");

      // CAST(x AS type) IS NOT NULL → x IS NOT NULL
      optimize("CAST(FIELD_NUMBER AS STRING) IS NOT NULL", "FIELD_NUMBER IS NOT NULL");
    }
  }

  @Nested
  class IsDistinctFrom {

    @Test
    void testPredicate() throws Exception {
      evalTrue("1 IS DISTINCT FROM 2");
      evalFalse("1 IS DISTINCT FROM 1");
      evalTrue("FIELD_INTEGER IS DISTINCT FROM 1").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalFalse("DATE '2019-01-01' IS DISTINCT FROM DATE '2019-01-01'");
      evalTrue("DATE '2019-01-01' IS DISTINCT FROM DATE '2018-01-01'");
    }

    @Test
    void testNullHandling() throws Exception {
      evalFalse("NULL_INTEGER IS DISTINCT FROM NULL");
    }

    @Test
    void testError() throws Exception {
      evalFails("FIELD_STRING IS DISTINCT 'TEST' ", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_STRING DISTINCT FROM 'TEST' ", ErrorCode.UNEXPECTED_CHARACTER);
    }

    @Test
    void testOptimize() throws Exception {
      optimize("FIELD_BOOLEAN_TRUE IS DISTINCT FROM TRUE");
      optimizeFalse("NULL IS DISTINCT FROM NULL");
      optimizeTrue("NULL IS DISTINCT FROM TRUE");
      optimizeTrue("NULL IS DISTINCT FROM 3");
      optimizeTrue("10151082135029368 IS DISTINCT FROM 10151082135029369");
      optimizeFalse("FIELD_INTEGER IS DISTINCT FROM FIELD_INTEGER");

      // The DISTINCT predicate is a verbose way of NULL safe comparisons
      optimizeFalse("NULL IS DISTINCT FROM NULL");
      optimize("NULL_INTEGER IS DISTINCT FROM NULL", "NULL_INTEGER IS NOT NULL");
      optimize("FIELD_STRING IS DISTINCT FROM NULL", "FIELD_STRING IS NOT NULL");
      optimize("NULL IS DISTINCT FROM FIELD_STRING", "FIELD_STRING IS NOT NULL");
    }
  }

  @Nested
  class IsNotDistinctFrom {

    @Test
    void testPredicate() throws Exception {
      evalFalse("1 IS NOT DISTINCT FROM 2");
      evalTrue("1 IS NOT DISTINCT FROM 1");
      evalTrue("FIELD_INTEGER IS NOT DISTINCT FROM 40").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalFalse("FIELD_INTEGER IS NOT DISTINCT FROM 1").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("DATE '2019-01-01' IS NOT DISTINCT FROM DATE '2019-01-01'");
      evalFalse("DATE '2019-01-01' IS NOT DISTINCT FROM DATE '2018-01-01'");
    }

    @Test
    void testNullHandling() throws Exception {
      evalFalse("NULL_BOOLEAN IS NOT DISTINCT FROM true");
      evalTrue("NULL_BOOLEAN IS NOT DISTINCT FROM NULL_BOOLEAN");
      evalTrue("NULL_INTEGER IS NOT DISTINCT FROM NULL");
    }

    @Test
    void testError() throws Exception {
      evalFails("FIELD_STRING IS NOT DISTINCT FROM ", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_STRING IS NOT DISTINCT 'TEST' ", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_STRING DISTINCT FROM 'TEST' ", ErrorCode.UNEXPECTED_CHARACTER);
    }

    @Test
    void testOptimize() throws Exception {
      optimize("FIELD_BOOLEAN_TRUE IS NOT DISTINCT FROM TRUE");
      optimizeTrue("NULL IS NOT DISTINCT FROM NULL");
      optimizeFalse("NULL IS NOT DISTINCT FROM TRUE");
      optimizeFalse("10151082135029368 IS NOT DISTINCT FROM 10151082135029369");
      optimizeTrue("FIELD_INTEGER IS NOT DISTINCT FROM FIELD_INTEGER");

      // The DISTINCT predicate is a verbose way of NULL safe comparisons
      optimizeTrue("NULL IS NOT DISTINCT FROM NULL");
      optimize("NULL_INTEGER IS NOT DISTINCT FROM NULL", "NULL_INTEGER IS NULL");
      optimize("FIELD_STRING IS NOT DISTINCT FROM NULL", "FIELD_STRING IS NULL");
      optimize("NULL IS NOT DISTINCT FROM FIELD_STRING", "FIELD_STRING IS NULL");
    }
  }

  @Nested
  class Add {
    @Test
    void testAddNumericToNumeric() throws Exception {
      // Addition of numeric
      evalEquals("10+(-0.5)", 9.5).returnType(NumberType.of(4, 1));

      evalEquals("0b00011::INTEGER+0", 3L);
      evalEquals("-24.7+0.5+24.7+0.5E-2", 0.505D);
      evalEquals("FIELD_INTEGER+FIELD_NUMBER+FIELD_BIGNUMBER", 123491.669D)
          .returnType(NumberType.NUMBER);
      evalEquals("FIELD_BIGNUMBER+FIELD_NUMBER+FIELD_INTEGER", 123491.669D);
      evalEquals("FIELD_BIGNUMBER+1", 123457.789D).returnType(NumberType.NUMBER);
      evalEquals("1::NUMBER(38,10)+3::NUMBER(38,5)", 4D).returnType(NumberType.of(38, 10));
      evalEquals("1::NUMBER(14,2)+3::NUMBER(14,2)", 4D).returnType(NumberType.of(15, 2));

      // Implicit coercion from BOOLEAN
      evalEquals("1+FALSE", 1L);
      evalEquals("TRUE+1", 2L);

      // Implicit coercion from STRING
      // evalEquals("'1'+2", 3L).returnType(NumberType.NUMBER);
      // evalEquals("1+'2'", 3L).returnType(NumberType.NUMBER);
      // evalEquals("1.3+'2.5'", 3.8);
      // evalEquals("1+'-2.5'", -1.5);
      // evalEquals("BINARY 'F'::INTEGER+1", 16L);

      // Addition of NULL is always null
      evalNull("5+NULL_INTEGER+5");
      evalNull("+NULL_INTEGER+5");
      evalNull("NULL_INTEGER+NULL_NUMBER");

      // Arithmetic overflow Long.MAX_VALUE+1
      evalFails("9223372036854775807::INTEGER+1::INTEGER", ErrorCode.ARITHMETIC_OVERFLOW);

      // Arithmetic overflow
      evalFails("cast(5e18 as INTEGER)+cast(5e18 as INTEGER)", ErrorCode.ARITHMETIC_OVERFLOW);

      // Syntax error
      evalFails("5+", ErrorCode.SYNTAX_ERROR);

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
    void testAddIntervalToTemporal() throws Exception {

      // Addition of an interval to a temporal
      evalEquals("DATE '2019-02-25'+INTERVAL 2 YEAR", LocalDateTime.of(2021, 2, 25, 0, 0, 0))
          .returnType(DateType.DATE_NOT_NULL);
      evalEquals(
          "INTERVAL 2 YEARS+DATE '2019-02-25'+INTERVAL 1 YEARS",
          LocalDateTime.of(2022, 2, 25, 0, 0, 0));
      evalEquals(
          "DATE '2019-02-25'+INTERVAL '2-11' YEAR TO MONTH",
          LocalDateTime.of(2022, 1, 25, 0, 0, 0));
      evalEquals("DATE '2019-02-25'+INTERVAL 1 WEEK", LocalDateTime.of(2019, 3, 4, 0, 0, 0));
      evalEquals("DATE '2019-02-25'+INTERVAL 12 HOUR", LocalDateTime.of(2019, 2, 25, 12, 0, 0));
      evalEquals("DATE '2019-02-25'+INTERVAL -12 HOUR", LocalDateTime.of(2019, 2, 24, 12, 0, 0));
      evalEquals(
          "DATE '2019-02-25'+INTERVAL '10 4' DAY TO HOUR", LocalDateTime.of(2019, 3, 7, 4, 0, 0));
      // evalEquals("DATE '2019-02-25'+TO_INTERVAL('10 4:0:0')", LocalDateTime.of(2019, 3, 7, 4, 0,
      // 0));
      evalNull("NULL_DATE+INTERVAL 12 DAYS").returnType(DateType.DATE);
      evalNull("INTERVAL 12 DAYS+NULL_DATE").returnType(DateType.DATE);

      evalFails("FIELD_DATE+TO_INTERVAL('z')", ErrorCode.INVALID_INTERVAL);

      // Adjust day to the end of the month and leap year.
      evalEquals("DATE '2019-01-31'+INTERVAL 1 MONTH", LocalDate.of(2019, 2, 28));
      evalEquals("DATE '2020-01-31'+INTERVAL 1 MONTH", LocalDate.of(2020, 2, 29));
      evalEquals("DATE '2020-03-31'+INTERVAL 1 MONTH", LocalDate.of(2020, 4, 30));
      evalEquals("DATE '2020-02-29'+INTERVAL 12 MONTHS", LocalDate.of(2021, 2, 28));

      evalEquals("DATE '0010-01-01'+INTERVAL 178956970 YEARS", LocalDate.of(178956980, 1, 1));

      optimize(
          "INTERVAL 1 HOUR+FIELD_DATE+INTERVAL 4 HOUR+INTERVAL 1 HOUR",
          "FIELD_DATE+INTERVAL 6 HOUR");
    }

    @Test
    void testAddNumericToTemporal() throws Exception {
      // Addition of days to a temporal
      evalEquals("DATE '2019-02-25'+1", LocalDate.of(2019, 2, 26))
          .returnType(DateType.DATE_NOT_NULL);
      evalEquals("DATE '2019-02-25'+2", LocalDate.of(2019, 2, 27));
      evalEquals("Timestamp '2019-02-25'+2", LocalDate.of(2019, 2, 27));

      // Only integer, round number
      evalEquals("DATE '2019-02-25'+1.8", LocalDateTime.of(2019, 2, 26, 0, 0, 0));
      evalEquals("DATE '2019-02-25'+5/(60*24)", LocalDateTime.of(2019, 2, 25, 0, 0, 0));
    }

    @Test
    void testAddIntervalToInterval() throws Exception {
      // Add interval to interval
      optimize("INTERVAL 1 YEAR+INTERVAL 13 MONTHS", "INTERVAL '+2-1' YEAR TO MONTH");
      optimize("INTERVAL 4 HOUR+INTERVAL 1 HOUR", "INTERVAL 5 HOUR");
      optimize("INTERVAL 4 MONTHS+INTERVAL 0 MONTH", "INTERVAL 4 MONTH");
    }
  }

  @Nested
  class Subtract {

    @Test
    void testSubtractNumericFromNumeric() throws Exception {
      // Subtract of numeric
      evalEquals("10-0.5", 9.5D).returnType(NumberType.of(4, 1));
      evalEquals("FIELD_INTEGER-0.5", 39.5D);
      evalEquals("FIELD_INTEGER-10::INTEGER", 30L);
      evalEquals("FIELD_INTEGER-FIELD_NUMBER-FIELD_BIGNUMBER", -123411.669D);
      evalEquals("FIELD_BIGNUMBER-FIELD_NUMBER-FIELD_INTEGER", 123421.909D);

      // Implicit coercion from BOOLEAN
      evalEquals("TRUE-FALSE", 1L).returnType(IntegerType.of(2));
      evalEquals("FALSE-TRUE", -1L);
      evalEquals("1-FIELD_BOOLEAN_FALSE", 1L).returnType(IntegerType.of(2));
      evalEquals("FIELD_BOOLEAN_TRUE-2", -1L);

      evalNull("5-NULL_INTEGER").returnType(IntegerType.of(19));
      evalNull("NULL_INTEGER-5");

      // Arithmetic overflow Long.MIN_VALUE-2
      evalFails("-9223372036854775807::INTEGER-2::INTEGER", ErrorCode.ARITHMETIC_OVERFLOW);

      // Arithmetic overflow
      evalFails("cast(-5e18 as INTEGER)-cast(5e18 as INTEGER)", ErrorCode.ARITHMETIC_OVERFLOW);

      // Syntax error
      evalFails("5-", ErrorCode.SYNTAX_ERROR);

      optimize("10-FIELD_INTEGER");
      optimize("FIELD_INTEGER-0", "FIELD_INTEGER");
      optimize("0-FIELD_INTEGER", "-FIELD_INTEGER");
      optimize("FIELD_INTEGER-(0-FIELD_INTEGER)", "FIELD_INTEGER+FIELD_INTEGER");
      optimize("FIELD_INTEGER-(-FIELD_NUMBER)", "FIELD_INTEGER+FIELD_NUMBER");
    }

    @Test
    void testSubtractIntervalFromTemporal() throws Exception {
      // Subtraction interval to a temporal
      evalEquals("DATE '2019-02-25'-INTERVAL 12 HOUR", LocalDateTime.of(2019, 2, 24, 12, 0, 0));
      evalEquals("DATE '2019-02-25'-INTERVAL 2 WEEKS", LocalDateTime.of(2019, 2, 11, 0, 0, 0));

      // TODO: Diff of two date
      // evalEquals("DATE '2019-02-25'-DATE '2019-02-23'", 2);
      // evalEquals("DATE '2019-02-23'-DATE '2019-02-25'", -2);
      // evalEquals("DATE '2019-02-25'-to_Date('2019-02-23 12:00','YYYY-MM-DD HH24:MI')", 1.5);

      // Adjust day to the end of the month and leap year.
      evalEquals("DATE '2019-03-30'-INTERVAL 1 MONTH", LocalDate.of(2019, 2, 28));
      evalEquals("DATE '2020-03-30'-INTERVAL 1 MONTH", LocalDate.of(2020, 2, 29));
      evalEquals("DATE '2020-04-30'-INTERVAL 1 MONTH", LocalDate.of(2020, 3, 30));
      evalEquals("DATE '2020-02-29'-INTERVAL 12 MONTHS", LocalDate.of(2019, 2, 28));
    }

    @Test
    void testSubtractNumericFromTemporal() throws Exception {
      // Subtraction of days to a temporal
      evalEquals("DATE '2019-02-25'-1", LocalDate.of(2019, 2, 24));
      evalEquals("DATE '2019-02-25'-28", LocalDate.of(2019, 1, 28));
      evalEquals("Timestamp '2019-02-25'-2", LocalDate.of(2019, 2, 23));
      // evalEquals("DATE '2019-02-25'-0.5", LocalDateTime.of(2019, 2, 24, 12, 0, 0));
      // evalEquals("DATE '2019-02-25'-5/(60*24)", LocalDateTime.of(2019, 2, 24, 23, 55, 0));
      // evalEquals("ADD_MONTHS(DATE '2019-04-30',1)", LocalDate.of(2019, 3, 31));
    }

    // @Test
    void testSubtractIntervalFromInterval() throws Exception {
      // Subtraction of an interval to an interval
      evalEquals("INTERVAL 5 HOUR - INTERVAL 1 HOUR", Interval.of(0, 0, 0, 4));

      optimize("INTERVAL 5 HOUR - INTERVAL 1 HOUR", "INTERVAL 4 HOURS");
    }
  }

  @Nested
  class Mod {
    @Test
    void testArithmetic() throws Exception {
      evalEquals("11%4", 3D).returnType(NumberType.NUMBER_NOT_NULL);
      evalEquals("Mod(11,4)", 3D);
      evalEquals("Mod(11,-4)", 3D);
      evalEquals("Mod(-11,4)", -3D);
      evalEquals("Mod(-11,-4)", -3D);

      evalEquals("Mod(11.3,4)", 3.3D);
      evalEquals("Mod(11.3::NUMBER(12,4),4)", 3.3D);
    }

    @Test
    void testError() throws Exception {
      // Syntax error
      evalFails("Mod()", ErrorCode.NOT_ENOUGH_ARGUMENT);
      evalFails("Mod(3)", ErrorCode.NOT_ENOUGH_ARGUMENT);

      // Illegal argument
      evalFails("'TEST'%5", ErrorCode.ILLEGAL_ARGUMENT);

      // Division by 0
      evalFails("Mod(9,0)", ErrorCode.DIVISION_BY_ZERO);
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("Mod(NULL_INTEGER,2)");
      evalNull("Mod(2,NULL_INTEGER)");
    }

    @Test
    void testOptimize() throws Exception {
      // Normalize
      optimize("0%0");
      optimize("FIELD_INTEGER%4");

      // Simplify arithmetic with NULL
      optimizeNull("NULL::INTEGER%FIELD_INTEGER");
      optimizeNull("FIELD_INTEGER%NULL::INTEGER");

      // Simplify arithmetic A % 1 → 0 (if A is not nullable)
      optimize("FIELD_INTEGER%1", "FIELD_INTEGER%1");
      optimize("FIELD_INTEGER%1.0", "FIELD_INTEGER%1");
      optimize("Random()%1", "0");
      optimize("Random()%1.0", "0");
    }
  }

  @Nested
  class Multiply {
    @Test
    void testArithmetic() throws Exception {
      evalEquals("4*10", 40L).returnType(IntegerType.of(3, false));
      evalEquals("2*-2", -4L).returnType(IntegerType.of(2, false));

      evalEquals("2.55*10", 25.50D).returnType(NumberType.of(5, 2, false));
      evalEquals("4*2.5", 10D).returnType(NumberType.of(3, 1, false));
      evalEquals("2.5*4", 10D).returnType(NumberType.of(3, 1, false));
      evalEquals("-4*-1", 4L).returnType(IntegerType.of(2, false));
      evalEquals("100 * .5", 50D).returnType(NumberType.of(5, 1, false));
      evalEquals("1.23456::Number(38,9)*-2.987654", -3.68843812224)
          .returnType(NumberType.of(38, 15, false));
      evalEquals("FIELD_NUMBER::NUMBER(4,1)*3::NUMBER(3,2)", -15.3).returnType(NumberType.of(7, 3));
      evalEquals("FIELD_NUMBER::NUMBER(38,5)*FIELD_INTEGER::NUMBER(9,8)", -204.8)
          .returnType(NumberType.of(38, 8));

      // Check no arithmetic overflow Long.MAX_VALUE * 2
      evalEquals("9223372036854775807*2::NUMBER", new BigDecimal("18446744073709551614"))
          .returnType(NumberType.NUMBER_NOT_NULL);

      // Check no arithmetic underflow Long.MIN_VALUE * 2
      evalEquals("-9223372036854775808*2::NUMBER", new BigDecimal("-18446744073709551616"));
    }

    @Test
    void testCoercion() throws Exception {
      // Implicit coercion from INTEGER to NUMBER
      evalEquals("2*2.2", 4.4D).returnType(NumberType.of(3, 1, false));
      evalEquals("2.5*2::INTEGER", 5D).returnType(NumberType.of(21, 1, false));
    }

    @Test
    void testError() throws Exception {
      // Arithmetic overflow
      evalFails("cast(5e9 as INTEGER) * cast(2e9 as INTEGER)", ErrorCode.ARITHMETIC_OVERFLOW);
      // evalFails("cast(2e9 as NUMBER(19,0)) * cast(-5e9 as NUMBER(19,0))");
      // evalFails("cast(5e4 as NUMBER(19,10)) * cast(2e4 as NUMBER(19,10))");
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("NULL_INTEGER*1*1");
      evalNull("1*NULL_INTEGER");
      evalNull("FIELD_STRING_INTEGER::INTEGER*NULL_INTEGER");
      evalNull("NULL_NUMBER*3.12");
      evalNull("3.12*NULL_NUMBER");
    }

    @Test
    void testOptimize() throws Exception {
      optimize("FIELD_INTEGER*4", "4*FIELD_INTEGER");
      optimize("FIELD_INTEGER*3*2", "6*FIELD_INTEGER");
      optimize("3*(FIELD_INTEGER*1)*1*(2*5)", "30*FIELD_INTEGER");
      optimize("4*FIELD_INTEGER*0.5", "2*FIELD_INTEGER");

      // Normalize
      optimize("0*FIELD_INTEGER");
      optimize("10*FIELD_INTEGER");
      optimize("FIELD_INTEGER*0", "0*FIELD_INTEGER");

      // Simplify arithmetic with NULL
      optimizeNull("NULL::INTEGER*FIELD_INTEGER");
      optimizeNull("FIELD_INTEGER*NULL::INTEGER");

      // Simplify arithmetic 0 * A → 0
      optimize("PI()*0", "0");
      optimize("0*PI()", "0");

      // Simplify arithmetic 1 * A → A
      optimize("FIELD_INTEGER*1", "FIELD_INTEGER");
      optimize("1.0*FIELD_INTEGER", "FIELD_INTEGER");

      // Simplify arithmetic (-A) * (-B) → A*B
      optimize("-FIELD_INTEGER*(-FIELD_NUMBER)", "FIELD_INTEGER*FIELD_NUMBER");
      optimize("-FIELD_INTEGER*(-FIELD_NUMBER)", "FIELD_INTEGER*FIELD_NUMBER");
      optimize("-FIELD_INTEGER*FIELD_NUMBER", "FIELD_NUMBER*-FIELD_INTEGER");

      // Simplify arithmetic only if one negating const * (-A) → -const * A
      optimize("-FIELD_INTEGER*(-2)", "2*FIELD_INTEGER");
      optimize("-2*(-FIELD_INTEGER)", "2*FIELD_INTEGER");
      optimize("2*(-FIELD_INTEGER)", "-2*FIELD_INTEGER");
      optimize("2*(-FIELD_INTEGER)*(-4)", "8*FIELD_INTEGER");

      // Simplify arithmetic A*A → SQUARE(A)
      // optimize("FIELD_INTEGER*FIELD_INTEGER", "SQUARE(FIELD_INTEGER)");
      // optimize("(2*FIELD_INTEGER)*(FIELD_INTEGER*2)", "4*SQUARE(FIELD_INTEGER)");

      // Simplify arithmetic (1/A)*B → B/A
      optimize("1/FIELD_INTEGER*4", "4*FIELD_INTEGER");

      // optimize("-2*(FIELD_INTEGER-4)/2", "4+FIELD_INTEGER");
    }
  }

  @Nested
  class Like {
    @Test
    void testPredicate() throws Exception {
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
      // values that start with "a" and are at least 3 characters in length
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

      // Ensure regex chars are escaped
      evalTrue("'' LIKE ''");
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
    }

    @Test
    void testEscapeChar() throws Exception {
      // Escape with other char
      evalTrue("'Result 100% value' like '%100^%%' escape '^'");

      // Double escape char
      evalFalse("'^100% milles' like '^^100^%%' escape '^'");

      // Escape with Regexp special char
      evalTrue("'give me 30% discount' like '%30!%%' escape '!'");
      evalTrue("'ADD_MONTHS' like '%ADD!_%' escape '!'");
    }

    @Test
    void testNullHandling() throws Exception {
      // NULL does not match NULL
      evalNull("NULL_STRING like NULL_STRING").returnType(BooleanType.BOOLEAN);
      evalNull("NULL_STRING like 'NULL'").returnType(BooleanType.BOOLEAN);
      evalNull("NULL_STRING LIKE '%'").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalNull("'test' LIKE NULL_STRING").returnType(BooleanType.BOOLEAN);
    }

    @Test
    void testError() throws Exception {
      evalFails("'a' LIKE '%' ESCAPE", ErrorCode.SYNTAX_ERROR);
      evalFails("'a' LIKE '%' ESCAPE ''", ErrorCode.INVALID_ARGUMENT);
      evalFails("'a' LIKE '%' ESCAPE NULL", ErrorCode.INVALID_ARGUMENT);
      evalFails("'a' LIKE '%' ESCAPE NULL_STRING", ErrorCode.INVALID_ARGUMENT);
      evalFails("'give me 30% discount' like '%30!%%' escape '!!'", ErrorCode.INVALID_ARGUMENT);
      evalFails("'test' LIKE 'TEST' escape NULL", ErrorCode.INVALID_ARGUMENT);
    }

    @Test
    void testOptimize() throws Exception {
      optimize("FIELD_STRING LIKE 'AB%C'");
      optimize("FIELD_STRING LIKE '%ADC!_%' ESCAPE '!'");
      optimize("FIELD_STRING LIKE '%'", "NVL2(FIELD_STRING,TRUE,NULL)");
      optimize("FIELD_STRING LIKE 'ABC'", "FIELD_STRING='ABC'");
      optimize("FIELD_STRING LIKE 'ABC%'", "STARTSWITH(FIELD_STRING,'ABC')");
      optimize("FIELD_STRING LIKE 'ABC%%'", "STARTSWITH(FIELD_STRING,'ABC')");
      optimize("FIELD_STRING LIKE 'ABC%%%'", "STARTSWITH(FIELD_STRING,'ABC')");
      optimize("FIELD_STRING LIKE '%ABC'", "ENDSWITH(FIELD_STRING,'ABC')");
      optimize("FIELD_STRING LIKE '%%ABC'", "ENDSWITH(FIELD_STRING,'ABC')");
      optimize("FIELD_STRING LIKE '%%%ABC'", "ENDSWITH(FIELD_STRING,'ABC')");
      optimize("FIELD_STRING LIKE '%ABC%'", "CONTAINS(FIELD_STRING,'ABC')");
      optimize("FIELD_STRING LIKE '%%ABC%%'", "CONTAINS(FIELD_STRING,'ABC')");

      optimize("FIELD_STRING LIKE 'AB%%C'", "FIELD_STRING LIKE 'AB%C'");
      optimize("FIELD_STRING LIKE 'AB%%%%C'", "FIELD_STRING LIKE 'AB%C'");
      optimize("FIELD_STRING LIKE '%%%AB%%C%%%'", "FIELD_STRING LIKE '%AB%C%'");
      optimize("FIELD_STRING LIKE '\\%%%AB%%C%%\\%'", "FIELD_STRING LIKE '\\%%AB%C%\\%'");

      // optimize("FIELD_STRING LIKE '%%'", "FIELD_STRING=FIELD_STRING");
      // optimize("FIELD_STRING LIKE '%%%'", "FIELD_STRING=FIELD_STRING");
    }
  }

  @Nested
  class ElementAt {
    @Test
    void testElementAt() throws Exception {
      evalEquals("[1,3,5][1]", 1L).returnType(IntegerType.of(1, false));
      evalEquals("[1,3.5,5][3]", 5D).returnType(NumberType.of(2, 1, false));
      evalEquals("['A','B',FIELD_STRING][3]", "TEST").returnType(StringType.of(1000, true));

      // Negative index
      evalEquals("[1,3,5][-1]", 5L);
      evalEquals("[1,3,5][-3]", 1L);
    }

    @Test
    void testError() {
      evalFails("[1,3,5][0]", ErrorCode.INVALID_ARRAY_INDEX);
      evalFails("[1,3,5][9]", ErrorCode.INVALID_ARRAY_INDEX);
    }

    @Test
    void testOptimize() throws Exception {
      optimize("[FIELD_INTEGER,3,5][1]");
      optimize("[FIELD_STRING,'A','B'][1]");
    }
  }

  @Nested
  class EqualTo {
    @Test
    void testCoercion() throws Exception {
      // Compare numeric with implicit coercion from BOOLEAN
      evalTrue("TRUE=1");
      evalTrue("1=TRUE");
      evalFalse("TRUE=1.1");
      evalFalse("TRUE=0");

      // Compare numeric with implicit coercion from integer and number
      evalTrue("2 = 2.0");
      evalTrue("2.0=2");
      evalFalse("2 = 2.1");
      evalFalse("2.1 = 2");

      // Comparable unordered type
      evalNull("NULL_JSON = FIELD_JSON");
      evalFalse("FIELD_JSON = FIELD_STRING_JSON::JSON");
    }

    @Test
    void testNullHandling() throws Exception {
      // NULL is not equal to anything, not even to another NULL.
      // x=NULL is always null
      evalNull("1 = NULL_INTEGER").returnType(BooleanType.BOOLEAN);
      evalNull("1 = NULL_NUMBER").returnType(BooleanType.BOOLEAN);
      evalNull("NULL_BOOLEAN = true").returnType(BooleanType.BOOLEAN);
      evalNull("NULL_BOOLEAN = false").returnType(BooleanType.BOOLEAN);
      evalNull("NULL_BOOLEAN = NULL_BOOLEAN").returnType(BooleanType.BOOLEAN);
      evalNull("NULL_STRING = NULL_STRING").returnType(BooleanType.BOOLEAN);
      evalNull("NULL_STRING = FIELD_STRING").returnType(BooleanType.BOOLEAN);
      evalNull("FIELD_STRING = NULL_STRING").returnType(BooleanType.BOOLEAN);
      evalNull("NULL_INTEGER = NULL_INTEGER").returnType(BooleanType.BOOLEAN);
      evalNull("NULL_INTEGER = 1").returnType(BooleanType.BOOLEAN);
      evalNull("NULL_DATE = Date '2025-08-12'").returnType(BooleanType.BOOLEAN);
      evalNull("NULL_TIMESTAMP = TIMESTAMP '2019-01-01 00:00:00'").returnType(BooleanType.BOOLEAN);
      evalNull("FIELD_INTEGER=NULL_INTEGER").returnType(BooleanType.BOOLEAN);
    }

    @Test
    void testPredicateWithInteger() throws Exception {
      evalTrue("0.0 = 0");
      evalTrue("0.0 = -0.000");
      evalTrue("15.0 = 15");
      evalTrue("'.01' = 0.01");
      evalTrue("FIELD_INTEGER = 40.0").returnType(BooleanType.BOOLEAN);
      evalTrue("0b11110000 = 0xF0");
    }

    @Test
    void testPredicateWithNumber() throws Exception {
      evalTrue("FIELD_NUMBER = -5.12").returnType(BooleanType.BOOLEAN);
      evalTrue("2.000 = 2");
      evalTrue("2.000 = 2.00");
      evalTrue("-1.4e-10 = -1.4e-10");
    }

    @Test
    void testPredicateWithDate() throws Exception {
      evalTrue("DATE '2019-01-01' = DATE '2019-01-01'");
      evalFalse("DATE '2019-01-01' = DATE '2018-01-01'");
      evalTrue("DATE '2019-01-01' = TIMESTAMP '2019-01-01 00:00:00'");
      evalTrue("TIMESTAMP '2019-01-01 00:00:00' = DATE '2019-01-01'");
    }

    @Test
    void testPredicateWithTimestamp() throws Exception {
      evalTrue("Timestamp '2019-01-01 08:00:00 -08:00' = Timestamp '2019-01-01 11:00:00 -05:00'");
      evalTrue("Timestamp '2019-01-01 8:00:00 -08:00' = Timestamp '2019-01-01 11:00:00 -05:00'");
      evalFalse("Timestamp '2019-01-01 08:00:00 -08:00' = Timestamp '2019-01-01 8:00:00 -05:00'");
      evalTrue(
          "Timestamp '2019-01-01 8:00:00' AT TIME ZONE 'America/New_York' = Timestamp '2019-01-01 14:00:00' AT TIME ZONE 'Europe/Berlin'");
    }

    @Test
    void testPredicateWithInterval() throws Exception {
      evalTrue("INTERVAL 1 YEARS = INTERVAL 12 MONTHS");
      evalFalse("INTERVAL 3 YEARS = INTERVAL 3 MONTHS");
    }

    @Test
    void testPredicateWithString() throws Exception {
      evalTrue("'ABC' = 'ABC'");
      evalFalse("'ABC' = 'abc'");
      evalTrue("FIELD_STRING = 'TEST'");
    }

    @Test
    void testPredicateWithBinary() throws Exception {
      evalTrue("BINARY 'FF22C' = BINARY 'ff22c'");
    }

    @Test
    void testPredicateWithBoolean() throws Exception {
      evalTrue("true = true").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("false = false");
      evalFalse("true = false");
      evalFalse("false = true");
      evalTrue("FIELD_BOOLEAN_TRUE = true").returnType(BooleanType.BOOLEAN);
    }

    @Test
    void testOptimize() throws Exception {
      // Normalize
      optimize("10=FIELD_INTEGER", "FIELD_INTEGER=10");
      optimize("FIELD_INTEGER=40");

      // Simplify comparison with literals
      optimizeTrue("'a' = 'a'");
      optimizeFalse("'a' = 'b'");
      optimizeFalse("10151082135029368 = 10151082135029369");
      optimizeNull("NULL::STRING=FIELD_STRING");
      optimizeNull("FIELD_STRING=NULL::STRING");

      // Simplify comparison when operands are of a boolean type
      optimize("FIELD_BOOLEAN_TRUE=TRUE", "FIELD_BOOLEAN_TRUE");
      optimize("TRUE=FIELD_BOOLEAN_TRUE", "FIELD_BOOLEAN_TRUE");
      optimize("FIELD_BOOLEAN_TRUE=FALSE", "NOT FIELD_BOOLEAN_TRUE");
      optimize("FALSE=FIELD_BOOLEAN_TRUE", "NOT FIELD_BOOLEAN_TRUE");

      // Simplify comparison with arithmetic
      optimize("FIELD_INTEGER+1=3", "FIELD_INTEGER=2");
      optimize("3=FIELD_INTEGER+1", "FIELD_INTEGER=2");

      // Simplify comparison with the same term when it is not nullable
      optimize("FIELD_STRING=FIELD_STRING", "FIELD_STRING=FIELD_STRING");
      optimize("PI()=PI()", "TRUE");
    }
  }

  @Nested
  class SimilarTo {
    @Test
    void testPredicate() throws Exception {
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
    }

    @Test
    void testCoercion() throws Exception {
      // Coercion integer to string
      evalTrue("FIELD_INTEGER SIMILAR TO '4_'");
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("NULL_STRING SIMILAR TO 'A'");
      evalNull("'A' SIMILAR TO NULL_STRING");
    }

    @Test
    void testError() throws Exception {
      evalFails("FIELD_STRING SIMILAR", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_STRING SIMILAR 'A'", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_STRING SIMILAR TO ", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_STRING SIMILAR AND TO ", ErrorCode.SYNTAX_ERROR);
    }

    @Test
    void testOptimize() throws Exception {
      optimize("FIELD_STRING SIMILAR TO 'abc'");
      optimize("FIELD_STRING NOT SIMILAR TO 'abc'");
    }
  }

  @Nested
  class Between {
    @Test
    void testBetweenNumber() throws Exception {
      evalTrue("3 between 1 and 5").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("3 between 3 and 5");
      evalTrue("5 between 3 and 5");
      evalFalse("1 between 3 and 5");
      evalFalse("5 between asymmetric 5 and 3");
      evalTrue("FIELD_INTEGER between symmetric 30 and 50");
      evalTrue("FIELD_INTEGER between symmetric 50 and 30");
      evalTrue("FIELD_INTEGER between -3+27 and 50");
      evalTrue("FIELD_INTEGER between 39.999 and 40.0001");
    }

    @Test
    void testBetweenString() throws Exception {
      evalTrue("'the' between 'that' and 'then'");
      evalFalse("'ti' between 'to' and 'tu'");
    }

    @Test
    void testBetweenDate() throws Exception {
      evalTrue("DATE '2019-02-28' between DATE '2019-01-01' and DATE '2019-12-31'");
    }

    @Test
    void testNot() throws Exception {
      // Not between
      evalTrue("FIELD_INTEGER not between 10 and 20");
      evalTrue("FIELD_INTEGER not between 10.5 and 20");
      evalFalse("FIELD_INTEGER not between 10 and 50");
      evalTrue("FIELD_INTEGER not between 10 and 20 and 'Test' is not null");
    }

    @Test
    void testCoercion() throws Exception {
      // Integer with boolean coercion
      evalTrue("FIELD_BOOLEAN_TRUE between -1 and 1");
      evalFalse("FIELD_INTEGER between FALSE and TRUE");

      // Integer with number coercion
      evalTrue("FIELD_INTEGER not between 10.5 and 20");
      evalTrue("FIELD_INTEGER not between 10 and 20.8");

      // Date with Timestamp coercion
      evalTrue("DATE '2019-02-28' between DATE '2019-01-01' and TIMESTAMP '2019-12-31 06:59'");
      evalTrue("TIMESTAMP '2019-02-28 06:59' between DATE '2019-01-01' and DATE '2019-12-31'");
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("NULL_INTEGER between -10 and 20").returnType(BooleanType.BOOLEAN);
      evalNull("NULL_INTEGER between symmetric -10 and 20");
      evalNull("1 between NULL_INTEGER and 20");
      evalNull("1 between symmetric NULL_INTEGER and 20");
      evalNull("1 between -10 and NULL_INTEGER");
      evalNull("1 between symmetric -10 and NULL_INTEGER");
    }

    @Test
    void testError() {
      evalFails("'the' between 1 and 2", ErrorCode.CONVERSION_ERROR);
      evalFails("FIELD_INTEGER between 10 and", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_INTEGER between and 10", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_INTEGER between and ", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_INTEGER between FIELD_DATE and FIELD_STRING", ErrorCode.ILLEGAL_ARGUMENT);
      evalFails("FIELD_INTEGER BETWEEN 4 AND", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_INTEGER BETWEEN  AND 7", ErrorCode.SYNTAX_ERROR);
      evalFails("FIELD_INTEGER BETWEEN 4 OR 6", ErrorCode.SYNTAX_ERROR);
    }

    @Test
    void testOptimize() throws Exception {
      optimize("FIELD_INTEGER BETWEEN 10 AND 20");
      optimize("FIELD_NUMBER BETWEEN 10.5 AND 20.12");
      optimize("FIELD_STRING BETWEEN 'AZE' AND 'KLM'");
      optimize("FIELD_DATE BETWEEN DATE '2019-01-01' AND DATE '2019-12-31'");

      // By default, BETWEEN is ASYMMETRIC
      optimize("FIELD_NUMBER BETWEEN ASYMMETRIC 20 AND 50", "FIELD_NUMBER BETWEEN 20 AND 50");
      optimize("FIELD_NUMBER BETWEEN ASYMMETRIC 50 AND 20", "FIELD_NUMBER BETWEEN 50 AND 20");

      // Remove SYMMETRIC if operands are constant, and swap bound if necessary
      optimize("FIELD_NUMBER BETWEEN SYMMETRIC 20 AND 50", "FIELD_NUMBER BETWEEN 20 AND 50");
      optimize("FIELD_NUMBER BETWEEN SYMMETRIC 50 AND 20", "FIELD_NUMBER BETWEEN 20 AND 50");

      optimize("FIELD_INTEGER between 3 and (5+1)", "FIELD_INTEGER BETWEEN 3 AND 6");
      optimizeFalse("2 between 3 and (5+1)");
      optimizeTrue("25.8 between 18 and 32");
      optimizeTrue("DATE '2019-02-28' between DATE '2019-01-01' and DATE '2019-12-31'");
    }
  }

  @Nested
  class Cast {
    @Test
    void cast() throws Exception {
      evalEquals("TO_NUMBER('123','000')::INTEGER+1", 124L).returnType(IntegerType.INTEGER);

      // Accept data type quoted like a String
      evalEquals("Cast(' 123' as 'INTEGER')", 123L).returnType(IntegerType.INTEGER_NOT_NULL);
      evalEquals("Cast('2022-01-01' as 'DATE')", LocalDate.of(2022, 1, 1));
    }

    @Test
    void testError() {
      // Operator syntax
      evalFails("'1234':", ErrorCode.UNEXPECTED_CHARACTER);
      evalFails("'1234':NUMBER", ErrorCode.UNEXPECTED_CHARACTER);
      evalFails("'1234'::", ErrorCode.SYNTAX_ERROR);

      // Function syntax
      evalFails("CAST('bad' AS)", ErrorCode.SYNTAX_ERROR_FUNCTION);
      evalFails("CAST(1234 AS STRING FORMAT )", ErrorCode.SYNTAX_ERROR_FUNCTION);
      evalFails("CAST(DATE '2019-02-25' AS String FORMAT )", ErrorCode.SYNTAX_ERROR_FUNCTION);
      evalFails("CAST(DATE '2019-02-25' AS String FORMAT NULL)", ErrorCode.SYNTAX_ERROR_FUNCTION);
      evalFails("CAST 3 as BOOLEAN)", ErrorCode.UNEXPECTED_CHARACTER);
      evalFails("CAST(3 as )", ErrorCode.SYNTAX_ERROR_FUNCTION);
      evalFails("CAST(3 as", ErrorCode.SYNTAX_ERROR_FUNCTION);
      evalFails("CAST(3 STRING", ErrorCode.SYNTAX_ERROR_FUNCTION);
      evalFails("CAST(3 as BOOLEAN", ErrorCode.MISSING_RIGHT_PARENTHESIS);

      // Bad data type
      evalFails("Cast(123 as Nill)", ErrorCode.INVALID_TYPE);
      evalFails("Cast(123 as 1)", ErrorCode.INVALID_TYPE);
      evalFails("Cast(123 as TRUE)", ErrorCode.INVALID_TYPE);
      evalFails("CAST('2020-01-01' AS NULL)", ErrorCode.INVALID_TYPE);
      evalFails("Cast(123 as 'Text')", ErrorCode.INVALID_TYPE);
    }

    @Test
    void castToBoolean() throws Exception {

      // Boolean to Boolean
      evalTrue("CAST(TRUE as Boolean)").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalFalse("CAST(FALSE as Boolean)");

      // Cast String to Boolean
      evalTrue("'Yes'::Boolean").returnType(BooleanType.BOOLEAN_NOT_NULL);
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
      evalTrue("CAST(1 as Boolean)").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalTrue("CAST(-123 as Boolean)");
      evalTrue("1::Boolean");
      evalFalse("CAST(0 as Boolean)");
      evalFalse("CAST(-0 as Boolean)");

      // Cast Number to Boolean
      evalTrue("CAST(-12.1 as Boolean)").returnType(BooleanType.BOOLEAN_NOT_NULL);
      evalFalse("CAST(0.000 as Boolean)");
      evalTrue("CAST(12345678901234567890123456789012345678 as Boolean)");

      // Null
      evalNull("CAST(NULL_INTEGER as Boolean)").returnType(BooleanType.BOOLEAN);
      evalNull("CAST(NULL_NUMBER as Boolean)").returnType(BooleanType.BOOLEAN);
      evalNull("CAST(NULL_STRING as Boolean)").returnType(BooleanType.BOOLEAN);
      evalNull("CAST(NULL_BOOLEAN as Boolean)").returnType(BooleanType.BOOLEAN);

      evalFails("'YEP'::Boolean", ErrorCode.CONVERSION_ERROR);

      // Unsupported conversion
      evalFails("CAST(FIELD_DATE AS BOOLEAN)", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("CAST(FIELD_TIMESTAMP AS BOOLEAN)", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("CAST(FIELD_JSON AS BOOLEAN)", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("CAST(FIELD_INET AS BOOLEAN)", ErrorCode.UNSUPPORTED_CONVERSION);

      // Remove lossless cast
      optimize("CAST(FIELD_BOOLEAN_TRUE AS BOOLEAN)", "FIELD_BOOLEAN_TRUE");

      // optimize("CAST(FIELD_STRING AS BOOLEAN)", "TO_BOOLEAN(FIELD_STRING)");
      // optimize("FIELD_STRING::BOOLEAN", "TO_BOOLEAN(FIELD_STRING)");
    }

    @Test
    void castToBinary() throws Exception {
      // String
      evalEquals("CAST('AB' as BINARY)", "AB".getBytes()).returnType(BinaryType.BINARY_NOT_NULL);

      // Null
      evalNull("CAST(NULL_STRING as Binary)").returnType(BinaryType.BINARY);
      evalNull("CAST(NULL_BINARY as Binary(100))").returnType(BinaryType.of(100));

      // Unsupported conversion
      evalFails("CAST(FIELD_BOOLEAN_TRUE AS BINARY)", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("CAST(FIELD_DATE AS BINARY)", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("CAST(FIELD_TIMESTAMP AS BINARY)", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("CAST(FIELD_INTEGER AS BINARY)", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("CAST(FIELD_NUMBER AS BINARY)", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("CAST(FIELD_JSON AS BINARY)", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("CAST(FIELD_INET AS BINARY)", ErrorCode.UNSUPPORTED_CONVERSION);

      optimize("CAST(FIELD_STRING AS BINARY)", "CAST(FIELD_STRING AS BINARY)");

      // Remove lossless cast
      optimize("CAST(FIELD_BINARY AS BINARY)", "FIELD_BINARY");
    }

    @Test
    void castToInteger() throws Exception {

      // Integer
      evalEquals("CAST(0 as Integer)", 0L).returnType(IntegerType.INTEGER_NOT_NULL);
      evalEquals("CAST(123 as Integer)", 123L);
      evalEquals("CAST(-123 as Integer(3))", -123L).returnType(IntegerType.of(3, false));

      // Number (Casting operations that result in loss of precision are allowed).
      evalEquals("CAST(1.25 as Integer)", 1L).returnType(IntegerType.INTEGER_NOT_NULL);
      evalEquals("CAST(-1.25 as Integer)", -1L);
      evalEquals("CAST(1.1 as Integer)", 1L);
      evalEquals("CAST(1.5 as Integer)", 2L);
      evalEquals("CAST(1.75 as Integer)", 2L);
      evalEquals("CAST(-1.7 as Integer)", -2L);

      // Boolean
      evalEquals("CAST(TRUE as Integer)", 1L).returnType(IntegerType.INTEGER_NOT_NULL);
      evalEquals("CAST(FALSE as Integer)", 0L);

      // String
      evalEquals("CAST(FIELD_STRING_INTEGER AS INTEGER)", 25L).returnType(IntegerType.INTEGER);
      evalEquals("CAST('1234' as Integer)", 1234L).returnType(IntegerType.INTEGER_NOT_NULL);
      evalEquals("'1234'::Integer+5", 1239L);
      evalEquals("CAST('1234.567' as Integer)", 1235L);
      evalEquals("CAST('1_234' as Integer)", 1234L);

      // Date to Unix Epoch
      evalEquals("CAST(TIMESTAMP '1970-01-01 00:00:01' as Integer)", 1L);
      evalEquals("CAST(DATE '2019-02-25' AS INTEGER)", 1551052800L);
      evalEquals("CAST(DATE '1800-01-01' AS INTEGER)", -5364662400L);
      evalEquals("Date '2025-12-23'::INTEGER", 1766448000L).returnType(IntegerType.INTEGER_NOT_NULL);

      // Null
      evalNull("CAST(NULL_NUMBER as Integer)").returnType(IntegerType.INTEGER);
      evalNull("CAST(NULL_INTEGER as Integer)").returnType(IntegerType.INTEGER);
      evalNull("CAST(NULL_BOOLEAN as Integer(4))").returnType(IntegerType.of(4));
      evalNull("Cast(NULL_STRING as INTEGER(5))").returnType(IntegerType.of(5));

      // Accept DataType quoted like a String
      evalEquals("Cast(123 as 'INTEGER')", 123L).returnType(IntegerType.INTEGER_NOT_NULL);

      // Casting operations that would result in a value overflow throw an error
      evalFails("CAST(9223372036854775807888 as INTEGER)", ErrorCode.CONVERSION_OVERFLOW);

      // Unsupported conversion
      evalFails("CAST(FIELD_BINARY AS INTEGER)", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("CAST(FIELD_JSON AS INTEGER)", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("CAST(FIELD_INET AS INTEGER)", ErrorCode.UNSUPPORTED_CONVERSION);

      // Remove unnecessary cast excepted with format
      optimize("CAST(FIELD_INTEGER AS INTEGER)", "FIELD_INTEGER");
      optimize("CAST(FIELD_DATE AS DATE)", "FIELD_DATE");

      // But don't remove with format
      optimize(
          "CAST(FIELD_STRING AS DATE FORMAT 'YYYYMM')",
          "CAST(FIELD_STRING AS DATE FORMAT 'YYYYMM')");

      // Remove loss-less cast
      optimize(
          "CAST(CAST(FIELD_INTEGER AS INTEGER(10)) AS INTEGER(5))",
          "CAST(FIELD_INTEGER AS INTEGER(5))");
      optimize(
          "CAST(CAST(FIELD_INTEGER AS INTEGER(5)) AS INTEGER(5))",
          "CAST(FIELD_INTEGER AS INTEGER(5))");
      optimize("CAST(FIELD_NUMBER AS NUMBER(38,9))", "FIELD_NUMBER");
      optimize(
          "CAST(CAST(FIELD_STRING AS STRING(100)) AS STRING(20))",
          "CAST(FIELD_STRING AS STRING(20))");
      optimize(
          "CAST(CAST(FIELD_STRING AS STRING(20)) AS STRING(20))",
          "CAST(FIELD_STRING AS STRING(20))");
      optimize("CAST(FIELD_STRING AS STRING(1000))", "FIELD_STRING");

      // Don't remove loss-less cast
      optimize(
          "CAST(CAST(FIELD_INTEGER AS INTEGER(5)) AS INTEGER(10))",
          "CAST(CAST(FIELD_INTEGER AS INTEGER(5)) AS INTEGER(10))");
      optimize("CAST(FIELD_NUMBER AS NUMBER(12,5))", "CAST(FIELD_NUMBER AS NUMBER(12,5))");
      optimize(
          "CAST(CAST(FIELD_STRING AS STRING(10)) AS STRING(100))",
          "CAST(CAST(FIELD_STRING AS STRING(10)) AS STRING(100))");
    }

    @Test
    void castToNumber() throws Exception {
      // Boolean
      evalEquals("CAST(TRUE as Number)", 1D).returnType(NumberType.NUMBER_NOT_NULL);
      evalEquals("CAST(FALSE as Number(1))", 0D)
          .returnType(NumberType.of(1).withNullability(false));

      // Integer
      evalEquals("CAST(0 as Number)", 0D).returnType(NumberType.NUMBER_NOT_NULL);
      evalEquals("CAST(123 as Number)", 123D).returnType(NumberType.NUMBER_NOT_NULL);
      evalEquals("CAST(-123 as Number(6,2))", -123D).returnType(NumberType.of(6, 2, false));

      // Number
      evalEquals("CAST(1234.456 as Number(20,9))", 1234.456D)
          .returnType(NumberType.of(20, 9, false));
      evalEquals("CAST(1234.456 as Number(10,1))", 1234.4D);
      evalEquals("1.23456::Number", 1D);
      evalEquals("1.23456::Number(10)", 1D).returnType(NumberType.of(10).withNullability(false));
      evalEquals("1.23456::Number(10,0)", 1D).returnType(NumberType.of(10).withNullability(false));
      evalEquals("1.23456::Number(10,2)", 1.23D).returnType(NumberType.of(10, 2, false));
      evalEquals("1.23456::Number(10,6)", new BigDecimal("1.234560"))
          .returnType(NumberType.of(10, 6, false));
      evalEquals("1.23456::Number(38,9)", 1.23456).returnType(NumberType.of(38, 9, false));
      evalEquals(
              "CAST(12345678901234567890123456789012345678 as Number(38,0))",
              new BigDecimal("12345678901234567890123456789012345678"))
          .returnType(NumberType.of(38).withNullability(false));

      // String
      evalEquals("FIELD_STRING_NUMBER::NUMBER", new BigDecimal("-12.56"))
                .returnType(NumberType.NUMBER);
      evalEquals("CAST('0' as Number)", 0D).returnType(NumberType.NUMBER_NOT_NULL);
      evalEquals("CAST('1' As Number)", 1D);
      evalEquals("CAST('-1e-37' as Number(38,37))", -1e-37d);
      evalEquals("CAST(' -1e-37 ' as Number(38,37))", -1e-37d);
      evalEquals("CAST('1234' as Number)", 1234D);
      evalEquals("CAST('1234.567' as Number(10,5))", 1234.567d);
      evalEquals("CAST('1_234.567_000' as Number(10,5))", 1234.567d);
      evalEquals("CAST('  -1e-37  ' as Number(38,37))", -1e-37d);
      evalEquals("'1'::Number", 1D);
      evalEquals("'1234'::Number", 1234D);
      evalEquals("' -1e-3 '::Number(10,3)", new BigDecimal("-1e-3"));
      evalEquals("CAST('1_2.3_4E1_0' as NUMBER)", 12.34E10D);

      // Date to Unix Epoch
      evalEquals("FIELD_DATE::NUMBER", new BigDecimal("362180698.123")).returnType(NumberType.NUMBER);
      evalEquals("CAST(DATE '1970-01-01' as Number)", 0D).returnType(NumberType.NUMBER_NOT_NULL);
      evalEquals("CAST(DATE '2019-02-25' AS Number)", 1551052800D)
          .returnType(NumberType.NUMBER_NOT_NULL);
      evalEquals("CAST(DATE '1800-01-01' AS Number)", -5364662400D)
          .returnType(NumberType.NUMBER_NOT_NULL);

      // Timestamp to Unix Epoch
      evalEquals("CAST(TIMESTAMP '1970-01-01 00:00:01' as Number)", 1D)
          .returnType(NumberType.NUMBER_NOT_NULL);

      // Null
      evalNull("CAST(NULL_INTEGER as Number)").returnType(NumberType.NUMBER);
      evalNull("CAST(NULL_NUMBER as Number)").returnType(NumberType.NUMBER);
      evalNull("CAST(NULL_BIGNUMBER as Number(12,2))").returnType(NumberType.of(12, 2));

      // Conversion raises an error at runtime
      evalFails("CAST('NotANumber' AS NUMBER)", ErrorCode.CONVERSION_ERROR);

      // Unsupported conversion
      evalFails("CAST(FIELD_BINARY AS NUMBER)", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("CAST(FIELD_JSON AS NUMBER)", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("CAST(FIELD_INET AS NUMBER)", ErrorCode.UNSUPPORTED_CONVERSION);

      optimize("CAST(FIELD_INTEGER AS NUMBER)", "CAST(FIELD_INTEGER AS NUMBER)");
      optimize("FIELD_INTEGER::NUMBER", "CAST(FIELD_INTEGER AS NUMBER)");

      // Chained cast
      optimize("CAST(CAST(CAST(123456 AS INTEGER) AS NUMBER) AS NUMBER)", "123456");
    }

    @Test
    void castToString() throws Exception {
      // Boolean
      evalEquals("CAST(true as String)", "TRUE").returnType(StringType.STRING_NOT_NULL);
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

      // Inet
      evalEquals("CAST(FIELD_INET AS String)", "10.10.10.1");

      // Null
      evalNull("CAST(NULL_BINARY as STRING)").returnType(StringType.STRING);
      evalNull("CAST(NULL_STRING as String)").returnType(StringType.STRING);
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
    void castToDate() throws Exception {
      // String
      evalEquals("FIELD_STRING_DATE::DATE", LocalDate.of(2025, 11, 23)).returnType(DateType.DATE);

      evalEquals("CAST('2020-march' as DATE FORMAT 'YYYY-MONTH')", LocalDate.of(2020, 3, 1))
          .returnType(DateType.DATE_NOT_NULL);
      evalEquals(
              "CAST('2020-01-19 11:23:44' as DATE FORMAT 'YYYY-MM-DD HH:MI:SS')",
              LocalDateTime.of(2020, 1, 19, 11, 23, 44))
          .returnType(DateType.DATE_NOT_NULL);

      // Integer Unix Epoch
      evalEquals("CAST(0 AS DATE)", LocalDateTime.of(1970, 1, 1, 0, 0, 0))
          .returnType(DateType.DATE_NOT_NULL);
      evalEquals("CAST(1551052800 AS DATE)", LocalDate.of(2019, 2, 25));
      evalEquals("CAST(-5364662400 AS DATE)", LocalDate.of(1800, 1, 1));
      evalEquals("CAST(1284352323 AS DATE)", LocalDateTime.of(2010, 9, 13, 4, 32, 3));
      evalEquals("FIRST_DAY(1766448000::DATE)", LocalDate.of(2025, 12, 1))
          .returnType(DateType.DATE_NOT_NULL);

      // Number Unix Epoch
      evalEquals("CAST(1551052800.000000000 AS DATE)", LocalDate.of(2019, 2, 25))
          .returnType(DateType.DATE_NOT_NULL);
      evalEquals("CAST(-5364662400.000000000 AS DATE)", LocalDate.of(1800, 1, 1));
      evalEquals("CAST(1284352323.1 AS DATE)", LocalDateTime.of(2010, 9, 13, 4, 32, 3, 100000000));
      evalEquals("CAST(1284352323.12 AS DATE)", LocalDateTime.of(2010, 9, 13, 4, 32, 3, 120000000));
      evalEquals(
          "CAST(1284352323.123 AS DATE)", LocalDateTime.of(2010, 9, 13, 4, 32, 3, 123000000));
      evalEquals(
          "CAST(1284352323.123456789 AS DATE)", LocalDateTime.of(2010, 9, 13, 4, 32, 3, 123456789));

      // Null
      evalNull("CAST(NULL as Date)").returnType(DateType.DATE);
      evalNull("CAST(NULL_DATE as Date)").returnType(DateType.DATE);

      // Bad format
      evalFails("CAST('2020-01-021' AS DATE FORMAT 'OOOO-MM-DD')", ErrorCode.INVALID_DATE_FORMAT);

      // Error parsing with format
      evalFails(
          "CAST('2023-01-01' AS DATE FORMAT 'YYYY-MM')", ErrorCode.UNPARSABLE_DATE_WITH_FORMAT);
      evalFails(
          "CAST('2023-01' AS DATE FORMAT 'YYYY-MM-DD')", ErrorCode.UNPARSABLE_DATE_WITH_FORMAT);

      // Unsupported conversion
      evalFails("CAST(FIELD_BOOLEAN_TRUE AS DATE)", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("CAST(FIELD_JSON AS DATE)", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("CAST(FIELD_INET AS DATE)", ErrorCode.UNSUPPORTED_CONVERSION);

      // optimize("CAST(FIELD_STRING AS DATE)", "TO_DATE(FIELD_STRING)");
      // optimize("CAST(FIELD_STRING AS DATE FORMAT 'YYYY-MM-DD')",
      // "TO_DATE(FIELD_STRING,'YYYY-MM-DD')");
      optimize(
          "CAST(TO_CHAR(FIELD_DATE,'YYYYMMDD') AS DATE FORMAT 'YYYYMMDD')",
          "CAST(TO_CHAR(FIELD_DATE,'YYYYMMDD') AS DATE FORMAT 'YYYYMMDD')");

      // Remove lossless cast
      optimize("Cast(FIELD_DATE as DATE)", "FIELD_DATE");
      optimize("'2021-02-08'::DATE", "DATE '2021-02-08'");
    }

    @Test
    void castToJson() throws Exception {
      // String
      evalEquals(
              "FIELD_STRING_JSON::JSON", JsonConversion.convert("{id:\"01\",name:\"John\",age:29}"))
          .returnType(JsonType.JSON);

      // Null
      evalNull("CAST(NULL_STRING as Json)").returnType(JsonType.JSON);
      evalNull("CAST(NULL_JSON as Json)");

      // Unsupported conversion
      evalFails("FIELD_BINARY::JSON", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("FIELD_BOOLEAN_TRUE::JSON", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("FIELD_DATE::JSON", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("FIELD_TIMESTAMP::JSON", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("FIELD_INTEGER::JSON", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("FIELD_NUMBER::JSON", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("FIELD_INET::JSON", ErrorCode.UNSUPPORTED_CONVERSION);

      // Remove lossless cast
      optimize("CAST(FIELD_JSON AS JSON)", "FIELD_JSON");

      // optimize("CAST(FIELD_STRING AS JSON)", "TO_JSON(FIELD_STRING)");
      // optimize("FIELD_STRING::JSON", "TO_JSON(FIELD_STRING)");
    }

    @Test
    void castToInterval() throws Exception {
      // String
      evalEquals("CAST('5 years' as INTERVAL)", Interval.of(5))
          .returnType(IntervalType.INTERVAL_NOT_NULL);
      evalEquals("'2 hour'::INTERVAL", Interval.of(0, 0, 0, 2))
          .returnType(IntervalType.INTERVAL_NOT_NULL);

      // Null
      evalNull("CAST(NULL_STRING as INTERVAL)");

      evalFails("CAST('5 yea' as INTERVAL)", ErrorCode.INVALID_INTERVAL);
      evalFails("CAST(3 as INTERVAL)", ErrorCode.UNSUPPORTED_CONVERSION);
      evalFails("CAST(TRUE as INTERVAL)", ErrorCode.UNSUPPORTED_CONVERSION);
    }

    @Test
    void castToInet() throws Exception {
      // String
      evalEquals("FIELD_STRING_INET::INET", InetAddress.getByName("10.10.10.1"))
          .returnType(InetType.INET);
      evalEquals("CAST('10.10.10.1' as INET)", InetAddress.getByName("10.10.10.1"))
          .returnType(InetType.INET_NOT_NULL);
      evalEquals("'10.10.10.1'::INET", InetAddress.getByName("10.10.10.1"))
          .returnType(InetType.INET_NOT_NULL);

      // Null
      evalNull("CAST(NULL_STRING as INET)");

      evalFails("CAST('xyz' as INET)", ErrorCode.CONVERSION_ERROR);
      evalFails("CAST(TRUE as INET)", ErrorCode.UNSUPPORTED_CONVERSION);

      // Remove lossless cast
      optimize("CAST(FIELD_INET AS INET)", "FIELD_INET");
    }
  }

  @Nested
  class AtTimeZone {
    @Test
    void testZone() throws Exception {
      evalEquals(
          "TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE 'Europe/Paris'",
          ZonedDateTime.of(2023, 5, 25, 20, 48, 0, 0, ZoneId.of("Europe/Paris")));
      evalEquals(
          "TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE 'Singapore'",
          ZonedDateTime.of(2023, 5, 25, 20, 48, 0, 0, ZoneId.of("Singapore")));
      evalEquals(
          "TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE 'GMT+0'",
          ZonedDateTime.of(2023, 5, 25, 20, 48, 0, 0, ZoneId.of("GMT+0")));
      evalEquals(
          "TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE 'CET'",
          ZonedDateTime.of(2023, 5, 25, 20, 48, 0, 0, ZoneId.of("CET")));
      evalEquals(
          "TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE 'EET'",
          ZonedDateTime.of(2023, 5, 25, 20, 48, 0, 0, ZoneId.of("EET")));
      evalEquals(
          "(TIMESTAMP '2023-05-25 10:48:00' AT TIME ZONE 'UTC') AT TIME ZONE 'Asia/Singapore'",
          ZonedDateTime.of(2023, 5, 25, 10, 48, 0, 0, ZoneId.of("Asia/Singapore")));
    }

    @Test
    void testError() {
      evalFails("TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE 'XYZ'", ErrorCode.INVALID_TIMEZONE);
      evalFails("TIMESTAMP '2023-05-25 20:48:00' AT TIME 'Europe/Paris'", ErrorCode.SYNTAX_ERROR);
      evalFails("TIMESTAMP '2023-05-25 20:48:00' AT TIME ZONE", ErrorCode.SYNTAX_ERROR);
      evalFails("TIMESTAMP '2023-05-25 20:48:00' AT ZONE 'Europe/Paris'", ErrorCode.SYNTAX_ERROR);
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("NULL_DATE AT TIME ZONE 'CET'");
    }

    @Test
    void testOptimize() throws Exception {
      optimize("FIELD_DATE AT TIME ZONE 'Europe/Paris'");
    }
  }
}
