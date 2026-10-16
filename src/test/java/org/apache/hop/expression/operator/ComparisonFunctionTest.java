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

import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionTest;
import org.apache.hop.expression.type.BooleanType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.parallel.Execution;

@TestInstance(PER_CLASS)
@Execution(CONCURRENT)
class ComparisonFunctionTest extends ExpressionTest {

  @Test
  void Is_Number() throws Exception {
    // String
    evalTrue("IS_NUMBER(' 123   ')").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalTrue("IS_NUMBER('-123.45')");
    evalTrue("IS_NUMBER('-3.45e+32')");
    evalTrue("IS_NUMBER('+3.45E-32')");
    evalTrue("IS_NUMBER('.6804')");
    evalFalse("IS_NUMBER('   ')");
    evalFalse("IS_NUMBER('3.45E-')");
    evalFalse("IS_NUMBER('12word')");
    evalFalse("IS_NUMBER(NULL_STRING)");

    // Number or Integer
    evalTrue("IS_NUMBER(-123)").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalTrue("IS_NUMBER(123.45)");
    evalTrue("IS_NUMBER(PI())");
    evalTrue("IS_NUMBER(FIELD_INTEGER)");
    evalTrue("IS_NUMBER(FIELD_NUMBER)");
    evalFalse("IS_NUMBER(NULL_INTEGER)");
    evalFalse("IS_NUMBER(NULL_NUMBER)");
    optimize("IS_NUMBER(FIELD_INTEGER)", "FIELD_INTEGER IS NOT NULL");

    // Other data type
    evalFalse("IS_NUMBER(FIELD_BOOLEAN_TRUE)").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalFalse("IS_NUMBER(FIELD_DATE)");
    evalFalse("IS_NUMBER(FIELD_BINARY)");
    evalFalse("IS_NUMBER(FIELD_JSON)");

    // Check operands
    evalFails("IS_NUMBER()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("IS_NUMBER(1,2)", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void Is_Json() throws Exception {
    evalTrue("IS_JSON('{\"name\":\"Smith\", \"age\":29}')")
        .returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalTrue("IS_JSON('{name:\"Smith\", age:29}')");
    evalTrue("IS_JSON('{id:1,coordinates:[10,20]}')");
    evalTrue("IS_JSON('[1,2]')");
    evalFalse("IS_JSON('name:\"Smith\", age:29}')");
    // Content after the JSON value is not valid
    evalFalse("IS_JSON('{a:1} x')");
    evalFalse("IS_JSON('{a:1}{b:2}')");
    // A scalar value is a valid JSON
    evalTrue("IS_JSON('123')");
    evalTrue("IS_JSON('\"a\"')");
    evalFalse("IS_JSON('')");
    evalFalse("IS_JSON('   ')");
    evalFalse("IS_JSON(NULL_STRING)");

    // Json
    evalTrue("IS_JSON(FIELD_JSON)").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalFalse("IS_JSON(NULL_JSON)");
    optimize("IS_JSON(FIELD_JSON)", "FIELD_JSON IS NOT NULL");

    // Other data type
    evalFalse("IS_JSON(TRUE)");
    evalFalse("IS_JSON(NULL)");
    optimizeFalse("IS_JSON(FIELD_DATE)");

    // Check operands
    evalFails("IS_JSON()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("IS_JSON(1,2)", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void Is_Date() throws Exception {

    // String
    evalTrue("IS_DATE('2023','YYYY')").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalTrue("IS_DATE('2023-04-25','YYYY-MM-DD')");
    evalTrue("IS_DATE('01/05/2023','DD/MM/YYYY')");
    evalFalse("IS_DATE('2023-02-31','YYYY-MM-DD')");
    evalFalse("IS_DATE('2023-02-31','YYYY.MM.DD')");
    evalFalse("IS_DATE(NULL_STRING,'YYYY.MM.DD')");
    evalFalse("IS_DATE(NULL,'YYYY')");
    // The format must be a string literal, like TO_DATE
    evalFails("IS_DATE('2023',NULL_STRING)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("IS_DATE('2023-04-25',NULL)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("IS_DATE('2023-04-25',FIELD_STRING)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("IS_DATE('2023','XYZ')", ErrorCode.INVALID_DATE_FORMAT);
    // The format is checked whatever the data type or the nullity of the value
    evalFails("IS_DATE(NULL_STRING,'XYZ')", ErrorCode.INVALID_DATE_FORMAT);
    evalFails("IS_DATE(FIELD_DATE,'XYZ')", ErrorCode.INVALID_DATE_FORMAT);
    evalFails("IS_DATE(FIELD_NUMBER,'XYZ')", ErrorCode.INVALID_DATE_FORMAT);

    // Date or Timestamp
    evalTrue("IS_DATE(FIELD_DATE,'YYYY-MM-DD')").returnType(BooleanType.BOOLEAN_NOT_NULL);
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

    // Check operands
    evalFails("IS_DATE()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("IS_DATE('2023')", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("IS_DATE('2023','YYYY',1)", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void Contains() throws Exception {
    // String
    evalTrue("CONTAINS(FIELD_STRING,'ES')").returnType(BooleanType.BOOLEAN);
    evalFalse("CONTAINS(FIELD_STRING,'YZ')");
    evalFalse("CONTAINS('TEST','es')");
    evalTrue("CONTAINS('TEST','')");

    // Binary
    evalTrue("CONTAINS(BINARY '1A2B3C4D5E6F',BINARY '1A2B')")
        .returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalTrue("CONTAINS(BINARY '1A2B3C4D5E6F',BINARY '2B3C')");
    evalTrue("CONTAINS(BINARY '1A2B3C4D5E6F',BINARY '5E6F')");
    evalFalse("CONTAINS(BINARY '1A2B3C4D5E6F',BINARY '0A2B')");
    evalFalse("CONTAINS(BINARY '1A2B3C4D5E6F',BINARY '6F6F')");
    evalTrue("CONTAINS(BINARY '1A2B3C4D5E6F',BINARY '')");
    evalTrue("CONTAINS(BINARY '',BINARY '')");

    // Mixed data type
    evalTrue("CONTAINS('abc',BINARY '62')");
    evalTrue("CONTAINS(BINARY '616263','b')");

    // Null handling
    evalNull("CONTAINS(NULL_STRING,'ES')").returnType(BooleanType.BOOLEAN);
    evalNull("CONTAINS(FIELD_STRING,NULL_STRING)");
    evalNull("CONTAINS(NULL_BINARY,BINARY '1A2B3C')").returnType(BooleanType.BOOLEAN);
    evalNull("CONTAINS(BINARY '1A2B3C',NULL_BINARY)");
    evalNull("CONTAINS(NULL,'ES')");

    // Check operands
    evalFails("CONTAINS()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("CONTAINS(FIELD_STRING)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("CONTAINS(FIELD_BINARY)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("CONTAINS('a','b','c')", ErrorCode.TOO_MANY_ARGUMENTS);
    // A binary value only accepts a binary or a string search
    evalFails("CONTAINS(BINARY '616263',1)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("CONTAINS(BINARY '616263',FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    // Only accepts string or binary values
    evalFails("CONTAINS(123,'2')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("CONTAINS('123',2)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("CONTAINS(FIELD_DATE,'2')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void StartsWith() throws Exception {
    // String
    evalTrue("StartsWith('TEST FROM','TES')").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalFalse("StartsWith('XXXTEST FROM','TES')");
    evalFalse("StartsWith('TEST','TESTXXX')");
    evalFalse("StartsWith('TEST','tes')");
    evalTrue("StartsWith('TEST','')");

    // Binary
    evalTrue("StartsWith(BINARY 'FAA12345',BINARY 'fA')").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalFalse("StartsWith(BINARY 'FAA12345',BINARY 'EE')");
    evalFalse("StartsWith(BINARY '1234',BINARY '123456')");
    evalTrue("StartsWith(BINARY 'FA',BINARY '')");

    // Mixed data type
    evalTrue("StartsWith('abc',BINARY '61')");
    evalTrue("StartsWith(BINARY '616263','a')");

    // Null handling
    evalNull("StartsWith(NULL_STRING,'TEST')").returnType(BooleanType.BOOLEAN);
    evalNull("StartsWith('TEST',NULL_STRING)");
    evalNull("StartsWith(NULL_BINARY,BINARY 'FA')").returnType(BooleanType.BOOLEAN);
    evalNull("StartsWith(BINARY 'FAA12345',NULL_BINARY)");

    // Check operands
    evalFails("StartsWith()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("StartsWith(FIELD_STRING)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("StartsWith(FIELD_BINARY)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("StartsWith('a','b','c')", ErrorCode.TOO_MANY_ARGUMENTS);
    // A binary value only accepts a binary or a string prefix
    evalFails("StartsWith(BINARY '616263',1)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("StartsWith(BINARY '616263',FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    // Only accepts string or binary values
    evalFails("StartsWith(123,'1')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("StartsWith('123',1)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("StartsWith(FIELD_DATE,'2')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void EndsWith() throws Exception {
    // String
    evalTrue("EndsWith('TEST FROM','ROM')").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalFalse("EndsWith('TEST FROM','ROMA')");
    evalFalse("EndsWith('TEST','TESTXX')");
    evalFalse("EndsWith('TEST','XXTEST')");
    evalFalse("EndsWith('TEST','est')");
    evalTrue("EndsWith('TEST','')");

    // Null handling
    evalNull("EndsWith(NULL_STRING,'TEST')").returnType(BooleanType.BOOLEAN);
    evalNull("EndsWith('TEST',NULL_STRING)");
    evalNull("EndsWith(NULL_BINARY,BINARY 'FA')").returnType(BooleanType.BOOLEAN);
    evalNull("EndsWith(BINARY 'FAA12345',NULL_BINARY)");

    // Binary
    evalTrue("EndsWith(BINARY 'FAA12345',BINARY '2345')").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalFalse("EndsWith(BINARY 'FAA12345',BINARY '88')");
    evalFalse("EndsWith(BINARY '1234',BINARY 'FFFF1234')");
    evalTrue("EndsWith(BINARY 'FA',BINARY '')");

    // Mixed data type
    evalTrue("EndsWith('abc',BINARY '63')");
    evalTrue("EndsWith(BINARY '616263','c')");

    // Check operands
    evalFails("EndsWith()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("EndsWith(FIELD_STRING)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("EndsWith(FIELD_BINARY)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("EndsWith('a','b','c')", ErrorCode.TOO_MANY_ARGUMENTS);
    // A binary value only accepts a binary or a string suffix
    evalFails("EndsWith(BINARY '616263',1)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("EndsWith(BINARY '616263',FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    // Only accepts string or binary values
    evalFails("EndsWith(123,'3')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("EndsWith('123',3)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("EndsWith(FIELD_DATE,'2')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }
}
