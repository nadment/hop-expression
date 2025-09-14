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
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionTest;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.BooleanType;
import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.parallel.Execution;

@TestInstance(PER_CLASS)
@Execution(CONCURRENT)
class ConditionalFunctionTest extends ExpressionTest {

  @Test
  void If() throws Exception {
    evalEquals("If(FIELD_BOOLEAN_TRUE,'True','False')", "True").returnType(StringType.STRING);
    evalEquals("If(FIELD_BOOLEAN_FALSE,'True','False')", "False");
    evalEquals("If(FIELD_BOOLEAN_TRUE,1,2)", 1L).returnType(IntegerType.INTEGER);
    evalEquals("If(FIELD_BOOLEAN_TRUE,2,2.3)", new BigDecimal("2")).returnType(NumberType.NUMBER);
    evalEquals(
            "If(FIELD_BOOLEAN_TRUE,Date '2023-01-01',Date '2023-02-01')", LocalDate.of(2023, 1, 1))
        .returnType(DateType.DATE_NOT_NULL);

    // If condition is NULL then return false value
    evalEquals("If(NULL_BOOLEAN,'A','B')", "B").returnType(StringType.STRING);

    // Syntax with only 2 operands
    evalEquals("If(true,'Test')", "Test");
    evalNull("If(false,'Test')");
    evalNull("If(false,1)");
    evalNull("If(false,Date '2023-01-01')");

    // Check operands
    evalFails("If()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("If(true)", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("If(true,2,'2')", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("If(Date '2023-01-01',1,2)", ErrorCode.ILLEGAL_ARGUMENT);

    // Simplify IF(x IS NULL,y,x)→ IFNULL(x, y)
    optimize("IF(FIELD_INTEGER IS NULL,\"YEAR\",FIELD_INTEGER)", "IFNULL(FIELD_INTEGER,\"YEAR\")");

    // Simplify IF(x IS NULL,y,z) → NVL2(x, z, y)
    optimize(
        "IF(FIELD_INTEGER IS NULL,\"YEAR\",NULL_INTEGER)",
        "NVL2(FIELD_INTEGER,NULL_INTEGER,\"YEAR\")");

    // Simplify IF(x IS NOT NULL,y,z) → NVL2(x, y, z)
    optimize(
        "IF(FIELD_INTEGER IS NOT NULL,\"YEAR\",NULL_INTEGER)",
        "NVL2(FIELD_INTEGER,\"YEAR\",NULL_INTEGER)");

    // Simplify IF(x=y,NULL,x) → NULLIF(x, y)
    optimize("IF(FIELD_NUMBER=0,NULL,FIELD_NUMBER)", "NULLIF(FIELD_NUMBER,0)");

    // Simplify IF(x=y,NULL,y) → NULLIF(y, x)
    optimize("IF(0=FIELD_INTEGER,NULL,FIELD_INTEGER)", "NULLIF(FIELD_INTEGER,0)");

    // No simplify
    optimize("IF(FIELD_INTEGER=10,NULL,FIELD_NUMBER)");
  }

  @Test
  void IfNull() throws Exception {
    evalEquals("IfNull(1,FIELD_INTEGER)", 1L).returnType(IntegerType.of(1, false));
    evalEquals("IfNull(NULL_INTEGER, FIELD_NUMBER)", -5.12D).returnType(NumberType.NUMBER);

    evalEquals("IfNull(NULL_STRING,'B')", "B").returnType(StringType.STRING_NOT_NULL);

    evalEquals("IfNull('A','B')", "A").returnType(StringType.of(1, false));
    evalEquals("IfNull(NULL_STRING,'B')", "B").returnType(StringType.STRING_NOT_NULL);

    evalEquals("IfNull(NULL_DATE,DATE '2022-01-01')", LocalDate.of(2022, 1, 1))
        .returnType(DateType.DATE_NOT_NULL);

    // Check operands
    evalFails("IfNull()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("IfNull(1)", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("IfNull(1,2,3)", ErrorCode.TOO_MANY_ARGUMENT);

    // Simplify IfNull(NULL,x) → x
    optimize("IFNULL(NULL::STRING,FIELD_STRING)", "FIELD_STRING");

    // Simplify if x is not nullable IfNull(x,y) → x
    optimize("IFNULL('A',FIELD_STRING)", "'A'");

    // Flatten chained IFNULL or COALESCE
    optimize(
        "IFNULL(IFNULL(NULL_STRING,FIELD_STRING),FIELD_STRING)",
        "IFNULL(NULL_STRING,FIELD_STRING)");
    optimize(
        "COALESCE(IFNULL(NULL_STRING, FIELD_STRING), 'X')",
        "COALESCE(NULL_STRING,FIELD_STRING,'X')");
    optimize(
        "IFNULL(IFNULL(NULL_STRING, FIELD_STRING), 'X')", "COALESCE(NULL_STRING,FIELD_STRING,'X')");
    optimize(
        "IFNULL(NULL_STRING,IFNULL(FIELD_STRING, 'X'))", "COALESCE(NULL_STRING,FIELD_STRING,'X')");

    // Alias
    evalEquals("NVL(NULL_INTEGER,1)", 1L);
  }

  @Test
  void NullIf() throws Exception {
    evalEquals("NullIf(1,NULL_INTEGER)", 1L).returnType(IntegerType.of(1, false));
    evalNull("NullIf(1,1)").returnType(IntegerType.of(1));
    evalNull("NULLIF(0.1,0.1)").returnType(NumberType.of(2, 1));
    evalNull("NullIf('TEST','TEST')").returnType(StringType.of(4));
    evalNull("NullIf(NULL_INTEGER,1)");
    evalNull("NullIf(DATE '2019-01-01',DATE '2019-01-01')").returnType(DateType.DATE);
    evalEquals("NullIf(1,2)", 1L);
    evalEquals("NullIf('TEST','XXX')", "TEST");
    evalEquals("NullIf(DATE '2019-01-01',DATE '2018-12-31')", LocalDate.of(2019, Month.JANUARY, 1));

    // Check operands
    evalFails("NullIf()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("NullIf(1,2,3)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("NullIf(1,true)", ErrorCode.ILLEGAL_ARGUMENT);

    optimizeNull("NULLIF(NULL, NULL)");
    // TODO: If the second operand is not nullable, return type must be not null
    optimizeNull("NULLIF(true, true)").returnType(BooleanType.BOOLEAN);
    optimizeTrue("NULLIF(true, false)");
    optimizeNull("NULLIF(NULL, false)");
    optimizeNull("NULLIF(true, NULL)");
    optimizeNull("NULLIF(NULL, FIELD_STRING)");
    optimizeNull("NULLIF('a', 'a')");
    optimizeNull("NULLIF(NULL, 'b')");
    optimizeNull("NULLIF('a', NULL)");
    optimizeNull("NULLIF(1, 1)");
    optimize("NULLIF('a', 'b')", "'a'");
    optimize("NULLIF(1, 2)", "1");
  }

  @Test
  void ZeroIfNull() throws Exception {
    evalEquals("ZeroIfNull(1)", 1L);
    evalEquals("ZeroIfNull(1.1)", 1.1D);
    evalEquals("ZeroIfNull(NULL_INTEGER)", 0L);

    // Check operands
    evalFails("ZeroIfNull()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("ZeroIfNull(1,2)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("ZeroIfNull('test')", ErrorCode.ILLEGAL_ARGUMENT);

    optimizeNull("NULLIFZERO(0)");
    optimizeNull("NULLIFZERO(0.00)");
  }

  @Test
  void NullIfZero() throws Exception {
    evalEquals("NULLIFZERO(0.1)", 0.1D).returnType(NumberType.of(2, 1, false));
    evalEquals("NullIfZero(1)", 1L).returnType(IntegerType.of(1, false));
    evalNull("NullIfZero(0)");
    evalNull("NullIfZero(0.000)");
    evalNull("NullIfZero(-0.0)");

    // Null handling
    evalNull("NullIfZero(NULL_INTEGER)");
    evalNull("NullIfZero(NULL_NUMBER)");

    // Check operands
    evalFails("NullIfZero()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("NullIfZero(1,2)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("NullIfZero('test')", ErrorCode.ILLEGAL_ARGUMENT);

    optimize("NULLIFZERO(0.1)", "0.1");
    optimizeNull("NULLIFZERO(0.0)");
    optimizeNull("NULLIFZERO(-0.000)");
  }

  @Test
  void Greatest() throws Exception {
    // Boolean
    evalTrue("Greatest(false, FIELD_BOOLEAN_TRUE)").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalTrue("Greatest(false,true,false)").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalFalse("Greatest(false,false,false)");

    // Numeric
    evalEquals("Greatest(5,2,9,4)", 9L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Greatest(123,FIELD_INTEGER,789)", 789L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Greatest(-5,2.1,9,4)", new BigDecimal("9")).returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Greatest(FIELD_INTEGER,FIELD_BIGNUMBER,FIELD_NUMBER)", 123456.789D)
        .returnType(NumberType.NUMBER);

    // TOTO: Numeric with String coercion
    // evalEquals("Greatest(123,FIELD_INTEGER,'789')", 789L).returnType(NumberType.NUMBER);

    // String
    evalEquals("Greatest('B','A','C')", "C").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Greatest(FIELD_STRING,'Ab','Bf')", "TEST").returnType(StringType.STRING_NOT_NULL);

    // Binary
    evalEquals("Greatest(BINARY '12', BINARY '1F',BINARY '0A')", new byte[] {0x1F})
        .returnType(BinaryType.BINARY_NOT_NULL);

    // Date
    evalEquals(
            "Greatest(DATE '2020-01-01',DATE '2021-12-06',DATE '1990-12-08')",
            LocalDate.of(2021, 12, 6))
        .returnType(DateType.DATE_NOT_NULL);

    // One argument only
    evalEquals("Greatest(5)", 5L);

    // Ignore nulls
    evalEquals("Greatest(NULL_STRING, 'B','A','C')", "C");

    // Null only if all values are null
    evalNull("Greatest(NULL_INTEGER, NULL_NUMBER)");

    // Check mixed operands type
    evalFails("Greatest(123,'str',123)", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("Greatest(123,DATE '2021-12-06',123)", ErrorCode.ILLEGAL_ARGUMENT);

    // Compare unordered type
    evalFails("Greatest(NULL_JSON, FIELD_JSON)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void Least() throws Exception {
    // Boolean
    evalTrue("Least(FIELD_BOOLEAN_TRUE, true)").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalFalse("Least(true,false,true,false)");
    evalTrue("Least(true,true,true)");

    // Numeric
    evalEquals("Least(5,2,9,4)", 2L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Least(123,FIELD_INTEGER,789)", 40L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Least(-5,2.1,9,4)", -5D).returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Least(FIELD_INTEGER,FIELD_NUMBER,789)", -5.12D)
        .returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Least(FIELD_INTEGER,FIELD_BIGNUMBER,FIELD_NUMBER)", -5.12D)
        .returnType(NumberType.NUMBER);

    // TODO: Numeric with coercion to String
    // evalEquals("Least('123',FIELD_INTEGER,789)", 40D).returnType(NumberType.NUMBER);

    // String
    evalEquals("Least('B','A','C')", "A").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Least(FIELD_STRING,'st','bf')", "TEST").returnType(StringType.STRING_NOT_NULL);

    // Binary
    evalEquals("Least(BINARY '12',BINARY '1F',BINARY '0A')", new byte[] {0x0A})
        .returnType(BinaryType.BINARY_NOT_NULL);

    // Date
    evalEquals(
            "Least(DATE '2020-01-01',DATE '2021-12-06',DATE '1990-12-08')",
            LocalDate.of(1990, 12, 8))
        .returnType(DateType.DATE_NOT_NULL);

    // 1 argument only
    evalEquals("Least(5)", 5L);

    // Ignore nulls
    evalEquals("Least(NULL_STRING, 'B','A','C')", "A");

    // Null only if all values are null
    evalNull("Least(NULL_INTEGER, NULL_NUMBER)");

    // Non coercible data type mixed
    evalFails("Least(123,'str',123)", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("Least(123,DATE '2021-12-06',123)", ErrorCode.ILLEGAL_ARGUMENT);

    // Compare unordered type
    evalFails("Least(FIELD_STRING, FIELD_JSON)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void Coalesce() throws Exception {
    // Coalesce string
    evalEquals("Coalesce(NULL_STRING,'TEST','BIDON')", "TEST")
        .returnType(StringType.STRING_NOT_NULL);
    evalEquals("Coalesce('TEST','BIDON')", "TEST").returnType(StringType.STRING_NOT_NULL);

    // Coalesce numeric
    evalEquals("Coalesce(1,2,3)", 1L).returnType(IntegerType.of(1, false));
    evalEquals("Coalesce(1,2,FIELD_INTEGER)", 1L).returnType(IntegerType.of(1, false));

    // Check NOT NULL return type
    evalEquals("Coalesce(FIELD_INTEGER,1,2)", 40L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Coalesce(NULL_NUMBER,NULL_INTEGER,1,2)", 1D).returnType(NumberType.NUMBER_NOT_NULL);

    evalNull("Coalesce(NULL_NUMBER,NULL_INTEGER,NULL_BIGNUMBER)").returnType(NumberType.NUMBER);

    // Coalesce date
    evalEquals(
            "Coalesce(NULL_DATE,FIELD_TIMESTAMP,Date '2024-01-01')",
            LocalDateTime.of(2023, 2, 28, 22, 11, 1))
        .returnType(DateType.DATE_NOT_NULL);

    // Check operands
    evalFails("Coalesce()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    optimize("COALESCE(NULL)", "NULL");
    optimize("COALESCE(NULL::INTEGER)", "NULL");
    optimize("COALESCE(FIELD_INTEGER)", "FIELD_INTEGER");

    // Coerce operands
    optimize(
        "COALESCE(NULL, FIELD_INTEGER, FIELD_NUMBER)",
        "IFNULL(CAST(FIELD_INTEGER AS NUMBER),FIELD_NUMBER)");

    // Duplicate coalesce
    optimize("COALESCE(FIELD_INTEGER,FIELD_INTEGER)", "FIELD_INTEGER");
    optimize(
        "COALESCE(FIELD_INTEGER, FIELD_NUMBER, FIELD_NUMBER)",
        "IFNULL(CAST(FIELD_INTEGER AS NUMBER),FIELD_NUMBER)");
    optimize(
        "COALESCE(FIELD_INTEGER, FIELD_NUMBER, 5, FIELD_NUMBER)",
        "COALESCE(CAST(FIELD_INTEGER AS NUMBER),FIELD_NUMBER,5)");

    // Flatten chained IFNULL or COALESCE
    optimize(
        "COALESCE(FIELD_INTEGER,COALESCE(FIELD_INTEGER,IFNULL(FIELD_NUMBER,2)))",
        "COALESCE(CAST(FIELD_INTEGER AS NUMBER),FIELD_NUMBER,2)");
    optimize(
        "COALESCE(FIELD_INTEGER,COALESCE(FIELD_INTEGER,COALESCE(FIELD_NUMBER,2)))",
        "COALESCE(CAST(FIELD_INTEGER AS NUMBER),FIELD_NUMBER,2)");

    // First not null is literal
    optimize("COALESCE(1, FIELD_INTEGER,FIELD_NUMBER)", "1");
    optimize("COALESCE(null, 1, CAST(FIELD_INTEGER AS NUMBER),FIELD_NUMBER)", "1");
  }

  @Test
  void Nvl2() throws Exception {
    evalEquals("Nvl2(FIELD_BOOLEAN_TRUE,FIELD_STRING,'ex2')", "TEST")
        .returnType(StringType.of(1000));
    evalEquals("Nvl2('test','ex1','ex2')", "ex1");
    evalEquals("Nvl2(NULL_STRING,'ex1','ex2')", "ex2");

    // Check operands
    evalFails("Nvl2()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Nvl2(true)", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Nvl2(true,2)", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Test
  void Decode() throws Exception {
    evalEquals("Decode(1,1,'one',2,'two',NULL_INTEGER,'<NULL>','other')", "one")
        .returnType(StringType.of(3, false));
    evalEquals("Decode(2,1,'one',2,'two',NULL_INTEGER,'<NULL>','other')", "two");
    evalEquals("Decode(NULL_INTEGER,1,'one',2,'two',NULL_INTEGER,'<NULL>','other')", "<NULL>");
    evalEquals("Decode(9,1,'one',2,'two',NULL_INTEGER,'<NULL>','other')", "other");

    evalEquals("Decode('A','B',2,'C',3,0)", 0L).returnType(IntegerType.of(1, false));

    // Support ERROR as default
    evalEquals(
        "Decode(FIELD_INTEGER,4,'Flag 1',40,'Flag 2',Error('Error generated with ABORT'))",
        "Flag 2");

    // Null handling
    evalNull("Decode(9,1,'one',2,'two',NULL_INTEGER,'<NULL>')");
    evalNull("Decode(9,1,'one',9,NULL,NULL_INTEGER,'<NULL>')");
    evalNull("Decode(1,1,NULL_STRING,9,'9',NULL_INTEGER,'<NULL>')");

    // Check operands
    evalFails("Decode()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Decode(1)", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Decode(1,2)", ErrorCode.NOT_ENOUGH_ARGUMENT);

    // Mixed type
    evalFails("Decode(FIELD_INTEGER,1,'baby',20,false,true)", ErrorCode.ILLEGAL_ARGUMENT);

    // Simplify unreachable DECODE clauses
    optimize(
        "DECODE(FIELD_NUMBER,FIELD_INTEGER,1,2,2,FIELD_INTEGER,3)",
        "DECODE(FIELD_NUMBER,FIELD_INTEGER,1,2,2)");
    optimize(
        "DECODE(FIELD_NUMBER,FIELD_INTEGER,1,2,2,FIELD_INTEGER,3,4)",
        "DECODE(FIELD_NUMBER,FIELD_INTEGER,1,2,2,4)");
  }
}
