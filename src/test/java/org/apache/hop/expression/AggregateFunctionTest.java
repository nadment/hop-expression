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

import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Types;
import org.junit.jupiter.api.Test;

class AggregateFunctionTest extends ExpressionTest {
  @Test
  void Avg() throws Exception {
    optimize("AVG(FIELD_NUMBER)").returnType(Types.NUMBER);
  }

  @Test
  void Count() throws Exception {
    evalFails("Count()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Count(DISTINCT )", ErrorCode.SYNTAX_ERROR);
    evalFails("Count(1,2)", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    optimize("COUNT(*)").returnType(Types.INTEGER_NOT_NULL);
    optimize("COUNT(FIELD_INTEGER)").returnType(Types.INTEGER_NOT_NULL);
    optimize("COUNT(DISTINCT FIELD_STRING)");
  }

  @Test
  void CountIf() throws Exception {
    evalFails("CountIf()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("CountIf(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("CountIf(1,2)", ErrorCode.TOO_MANY_ARGUMENT);

    optimize("COUNTIF(FIELD_INTEGER>=10)").returnType(Types.INTEGER);
    optimize("COUNTIF(FIELD_INTEGER>=10)", "COUNTIF(FIELD_INTEGER>=10)");
  }

  @Test
  void Sum() throws Exception {
    optimize("SUM(FIELD_INTEGER)").returnType(Types.NUMBER);
  }

  @Test
  void Max() throws Exception {
    optimize("MAX(FIELD_STRING)").returnType(StringType.of(1000));
    optimize("MAX(FIELD_INTEGER)").returnType(IntegerType.of(12));
    optimize("MAX(FIELD_NUMBER)").returnType(Types.NUMBER);
    optimize("MAX(FIELD_DATE)").returnType(Types.DATE);
  }

  @Test
  void Min() throws Exception {
    optimize("MIN(FIELD_STRING)").returnType(StringType.of(1000));
    optimize("MIN(FIELD_INTEGER)").returnType(IntegerType.of(12));
    optimize("MIN(FIELD_NUMBER)").returnType(Types.NUMBER);
    optimize("MIN(FIELD_DATE)").returnType(Types.DATE);
  }

  @Test
  void Median() throws Exception {
    optimize("MEDIAN(FIELD_INTEGER)").returnType(Types.NUMBER);
  }

  @Test
  void AnyValue() throws Exception {
    optimize("ANY_VALUE(FIELD_DATE)").returnType(Types.DATE);
  }

  @Test
  void FirstValue() throws Exception {
    optimize("FIRST_VALUE(FIELD_DATE)").returnType(Types.DATE);
    optimize("FIRST_VALUE(FIELD_DATE) RESPECT NULLS", "FIRST_VALUE(FIELD_DATE)");
    optimize("FIRST_VALUE(FIELD_DATE) IGNORE NULLS");

    evalFails("FIRST_VALUE(FIELD_DATE) IGNORE", ErrorCode.CALL_FUNCTION_ERROR);
    evalFails("FIRST_VALUE(FIELD_DATE) NULLS", ErrorCode.UNEXPECTED_CHARACTER);
  }

  @Test
  void LastValue() throws Exception {
    optimize("LAST_VALUE(FIELD_DATE)").returnType(Types.DATE);
    optimize("LAST_VALUE(FIELD_DATE) RESPECT NULLS", "LAST_VALUE(FIELD_DATE)");
    optimize("LAST_VALUE(FIELD_DATE) IGNORE NULLS").returnType(Types.DATE);

    evalFails("LAST_VALUE(FIELD_DATE) IGNORE", ErrorCode.CALL_FUNCTION_ERROR);
    evalFails("LAST_VALUE(FIELD_DATE) NULLS", ErrorCode.UNEXPECTED_CHARACTER);
  }

  @Test
  void NthValue() throws Exception {
    optimize("NTH_VALUE(FIELD_DATE,3)").returnType(Types.DATE);
    optimize("NTH_VALUE(FIELD_DATE,2) RESPECT NULLS", "NTH_VALUE(FIELD_DATE,2)");
    optimize("NTH_VALUE(FIELD_DATE,2) IGNORE NULLS");

    evalFails("NTH_VALUE(FIELD_DATE)", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("NTH_VALUE(FIELD_DATE) IGNORE", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("NTH_VALUE(FIELD_DATE) NULLS", ErrorCode.SYNTAX_ERROR_FUNCTION);
  }

  @Test
  void ListAgg() throws Exception {
    optimize("LISTAGG(FIELD_STRING,',')").returnType(Types.STRING);
  }

  @Test
  void Percentile() throws Exception {
    optimize("PERCENTILE(FIELD_INTEGER,0.75)").returnType(Types.NUMBER);
  }

  @Test
  void VarPop() throws Exception {
    optimize("VARIANCE_POP(FIELD_INTEGER)").returnType(Types.NUMBER);
  }

  void VarSamp() throws Exception {
    optimize("VARIANCE_SAMP(FIELD_INTEGER)").returnType(Types.NUMBER);
  }

  @Test
  void StdDevPop() throws Exception {
    optimize("STDDEV_POP(FIELD_INTEGER)").returnType(Types.NUMBER);
  }

  @Test
  void StdDevSamp() throws Exception {
    optimize("STDDEV_SAMP(FIELD_INTEGER)").returnType(Types.NUMBER);
  }
}
