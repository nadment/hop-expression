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

package org.apache.hop.expression;

import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.junit.jupiter.api.Test;

class AggregateFunctionTest extends ExpressionTest {
  @Test
  void Avg() throws Exception {
    evalFails("AVG()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("AVG(2,4)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("AVG(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);

    optimize("AVG(FIELD_INTEGER)").returnType(NumberType.NUMBER);
    optimize("AVG(FIELD_NUMBER)").returnType(NumberType.NUMBER);
  }

  @Test
  void Count() throws Exception {
    evalFails("Count()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Count(DISTINCT )", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Count(1,2)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Count(", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    optimize("COUNT(*)").returnType(IntegerType.INTEGER_NOT_NULL);
    optimize("COUNT(FIELD_INTEGER)").returnType(IntegerType.INTEGER_NOT_NULL);
    optimize("COUNT(DISTINCT FIELD_STRING)").returnType(IntegerType.INTEGER_NOT_NULL);
  }

  @Test
  void CountIf() throws Exception {
    evalFails("CountIf()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("CountIf(1,2)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("CountIf(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);

    optimize("COUNTIF(FIELD_INTEGER>=10)").returnType(IntegerType.INTEGER);
    optimize("COUNTIF(FIELD_INTEGER>=10)", "COUNTIF(FIELD_INTEGER>=10)");
  }

  @Test
  void Sum() throws Exception {
    evalFails("SUM()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("SUM(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);

    optimize("SUM(FIELD_INTEGER)").returnType(NumberType.NUMBER);
  }

  @Test
  void Max() throws Exception {
    evalFails("MAX()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    optimize("MAX(FIELD_STRING)").returnType(StringType.of(1000));
    optimize("MAX(FIELD_INTEGER)").returnType(IntegerType.of(12));
    optimize("MAX(FIELD_NUMBER)").returnType(NumberType.NUMBER);
    optimize("MAX(FIELD_DATE)").returnType(DateType.DATE);
    optimize("MAX(FIELD_BINARY)").returnType(BinaryType.BINARY);
  }

  @Test
  void Min() throws Exception {
    evalFails("MIN()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    optimize("MIN(FIELD_STRING)").returnType(StringType.of(1000));
    optimize("MIN(FIELD_INTEGER)").returnType(IntegerType.of(12));
    optimize("MIN(FIELD_NUMBER)").returnType(NumberType.NUMBER);
    optimize("MIN(FIELD_DATE)").returnType(DateType.DATE);
    optimize("MIN(FIELD_BINARY)").returnType(BinaryType.BINARY);
  }

  @Test
  void Median() throws Exception {
    evalFails("MEDIAN()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("MEDIAN(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);

    optimize("MEDIAN(FIELD_INTEGER)").returnType(NumberType.NUMBER);
  }

  @Test
  void AnyValue() throws Exception {
    evalFails("ANY_VALUE()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    optimize("ANY_VALUE(FIELD_DATE)").returnType(DateType.DATE);
    optimize("ANY_VALUE(FIELD_INTEGER)").returnType(IntegerType.of(12));
    optimize("ANY_VALUE(FIELD_STRING)").returnType(StringType.of(1000));
  }

  @Test
  void FirstValue() throws Exception {
    optimize("FIRST_VALUE(FIELD_STRING)").returnType(StringType.of(1000));

    optimize("FIRST_VALUE(FIELD_DATE)").returnType(DateType.DATE);
    optimize("FIRST_VALUE(FIELD_DATE) RESPECT NULLS", "FIRST_VALUE(FIELD_DATE)");
    optimize("FIRST_VALUE(FIELD_DATE) IGNORE NULLS");

    evalFails("FIRST_VALUE()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("FIRST_VALUE(1,2)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("FIRST_VALUE(FIELD_DATE) IGNORE", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("FIRST_VALUE(FIELD_DATE) RESPECT", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("FIRST_VALUE(FIELD_DATE) NULLS", ErrorCode.UNEXPECTED_CHARACTER);
  }

  @Test
  void LastValue() throws Exception {
    evalFails("LAST_VALUE()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("LAST_VALUE(1,2)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("LAST_VALUE(FIELD_DATE) IGNORE", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("LAST_VALUE(FIELD_DATE) RESPECT", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("LAST_VALUE(FIELD_DATE) NULLS", ErrorCode.UNEXPECTED_CHARACTER);

    optimize("LAST_VALUE(FIELD_STRING)").returnType(StringType.of(1000));
    optimize("LAST_VALUE(FIELD_INTEGER)").returnType(IntegerType.of(12));
    optimize("LAST_VALUE(FIELD_DATE)").returnType(DateType.DATE);
    optimize("LAST_VALUE(FIELD_DATE) RESPECT NULLS", "LAST_VALUE(FIELD_DATE)");
    optimize("LAST_VALUE(FIELD_DATE) IGNORE NULLS").returnType(DateType.DATE);
  }

  @Test
  void NthValue() throws Exception {
    evalFails("NTH_VALUE()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("NTH_VALUE(FIELD_DATE)", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("NTH_VALUE(FIELD_DATE,1) IGNORE", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("NTH_VALUE(FIELD_DATE,1) RESPECT", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("NTH_VALUE(FIELD_DATE,1) NULLS", ErrorCode.UNEXPECTED_CHARACTER);

    optimize("NTH_VALUE(FIELD_STRING,1)").returnType(StringType.of(1000));
    optimize("NTH_VALUE(FIELD_DATE,3)").returnType(DateType.DATE);
    optimize("NTH_VALUE(FIELD_DATE,2) RESPECT NULLS", "NTH_VALUE(FIELD_DATE,2)");
    optimize("NTH_VALUE(FIELD_DATE,2) IGNORE NULLS");
  }

  @Test
  void ListAgg() throws Exception {
    evalFails("LISTAGG()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    optimize("LISTAGG(FIELD_STRING,',')").returnType(StringType.STRING);
  }

  @Test
  void Percentile() throws Exception {
    evalFails("PERCENTILE()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    optimize("PERCENTILE(FIELD_INTEGER,0.75)").returnType(NumberType.NUMBER);
  }

  @Test
  void VarPop() throws Exception {
    evalFails("VARIANCE_POP()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    optimize("VARIANCE_POP(FIELD_INTEGER)").returnType(NumberType.NUMBER);
  }

  @Test
  void VarSamp() throws Exception {
    evalFails("ANY_VALUE()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    optimize("VARIANCE_SAMP(FIELD_INTEGER)").returnType(NumberType.NUMBER);
  }

  @Test
  void StdDevPop() throws Exception {
    evalFails("STDDEV_POP()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    optimize("STDDEV_POP(FIELD_INTEGER)").returnType(NumberType.NUMBER);
  }

  @Test
  void StdDevSamp() throws Exception {
    evalFails("STDDEV_SAMP()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    optimize("STDDEV_SAMP(FIELD_INTEGER)").returnType(NumberType.NUMBER);
  }
}
