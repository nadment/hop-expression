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
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionTest;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.BooleanType;
import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.parallel.Execution;

@TestInstance(PER_CLASS)
@Execution(CONCURRENT)
class SpecialFunctionTest extends ExpressionTest {
  @Test
  void Error() {
    evalFails("ERROR('Custom error message')", ErrorCode.MESSAGE_ERROR);
  }

  @Test
  void TypeOf() throws Exception {
    evalEquals("TypeOf(TRUE)", "BOOLEAN").returnType(StringType.STRING_NOT_NULL);
    evalEquals("TypeOf('str')", "STRING").returnType(StringType.STRING_NOT_NULL);
    evalEquals("TypeOf(25)", "INTEGER").returnType(StringType.STRING_NOT_NULL);
    evalEquals("TypeOf(FIELD_NUMBER)", "NUMBER").returnType(StringType.STRING_NOT_NULL);
    evalEquals("TypeOf(DATE '2023-01-01')", "DATE").returnType(StringType.STRING_NOT_NULL);
    evalEquals("TypeOf(INTERVAL '3 years')", "INTERVAL").returnType(StringType.STRING_NOT_NULL);
  }

  @Test
  void Current_User() throws Exception {
    evalEquals("Current_User()", System.getProperty("user.name"))
        .returnType(StringType.of(System.getProperty("user.name").length(), false));
  }

  @Nested
  class Try {

    /** Exceptions that should be suppressed */
    @Test
    void exceptionSuppressed() throws Exception {
      // @code{ErrorCode.UNPARSABLE_NUMBER_WITH_FORMAT}
      evalEquals("TRY('123'::INTEGER)", 123L).returnType(IntegerType.INTEGER_NOT_NULL);
      evalNull("TRY('abc'::INTEGER)").returnType(IntegerType.INTEGER);
      optimize("TRY('abc'::INTEGER)", "NULL");

      // @code{ErrorCode.UNPARSABLE_DATE_WITH_FORMAT}
      evalEquals("TRY(CAST('0000-01-01' AS DATE))", LocalDate.of(0, 1, 1))
          .returnType(DateType.DATE_NOT_NULL);
      evalNull("TRY(CAST('0000-00-01' AS DATE))").returnType(DateType.DATE);
      optimize("TRY(CAST('0000-00-01' AS DATE))", "NULL");

      // @code{ErrorCode.UNPARSABLE_BINARY}
      evalNull("TRY(TO_BINARY('0Z','HEX'))").returnType(BinaryType.BINARY);
      optimize("TRY(TO_BINARY('0Z','HEX'))", "NULL");

      // @code{ErrorCode.ARITHMETIC_OVERFLOW}
      evalNull("TRY(9223372036854775807::INTEGER+1::INTEGER)").returnType(IntegerType.INTEGER);
      optimize("TRY(9223372036854775807::INTEGER+1::INTEGER)", "NULL");

      // @code{ErrorCode.CONVERSION_OVERFLOW}
      evalNull("TRY(CAST(9223372036854775808 as INTEGER))").returnType(IntegerType.INTEGER);
      optimize("TRY(CAST(9223372036854775808 as INTEGER))", "NULL");

      // @code{ErrorCode.CONVERSION_ERROR}
      evalNull("TRY('ABC'::Boolean)").returnType(BooleanType.BOOLEAN);
      optimize("TRY('ABC'::Boolean)", "NULL");

      // @code{ErrorCode.DIVISION_BY_ZERO}
      evalEquals("TRY(5.2/2)", 2.6D).returnType(NumberType.of(7, 6, false));
      evalNull("TRY(5.2/0)").returnType(NumberType.of(7, 6));
      optimize("TRY(5.2/0)", "NULL");

      // @code{ErrorCode.ARGUMENT_OUT_OF_RANGE}
      evalEquals("TRY(LN(1))", BigDecimal.ZERO).returnType(NumberType.NUMBER_NOT_NULL);
      evalNull("TRY(LN(0))").returnType(NumberType.NUMBER);
      optimize("TRY(LN(0))", "NULL");
    }

    /** Exceptions that should not be suppressed */
    @Test
    void exceptionNotSuppressed() throws Exception {
      evalFails("TRY(ERROR('test'))", ErrorCode.MESSAGE_ERROR);
      evalFails("TRY(CAST(TRUE as INET))", ErrorCode.UNSUPPORTED_CONVERSION);
    }
  }
}
