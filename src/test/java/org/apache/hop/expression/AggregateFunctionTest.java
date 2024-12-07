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
    returnType("AVG(FIELD_NUMBER)", Types.NUMBER);
  }

  @Test
  void Count() throws Exception {
    evalFails("Count()", ErrorCode.CALL_FUNCTION_ERROR);
    evalFails("Count(DISTINCT )", ErrorCode.SYNTAX_ERROR);
    evalFails("Count(1,2)", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    returnType("Count(*)", Types.INTEGER);

    optimize("COUNT(FIELD_INTEGER)");
    optimize("COUNT(*)");
    optimize("COUNT(DISTINCT FIELD_STRING)");
  }

  @Test
  void CountIf() throws Exception {
    evalFails("CountIf()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("CountIf(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("CountIf(1,2)", ErrorCode.TOO_MANY_ARGUMENT);
    returnType("CountIf(FIELD_INTEGER>=10)", Types.INTEGER);
    optimize("COUNTIF(FIELD_INTEGER>=10)", "COUNTIF(FIELD_INTEGER>=10)");
  }

  @Test
  void Sum() throws Exception {
    returnType("SUM(FIELD_INTEGER)", Types.NUMBER);
  }

  @Test
  void Max() throws Exception {
    returnType("MAX(FIELD_STRING)", StringType.of(1000));
    returnType("MAX(FIELD_INTEGER)", IntegerType.of(12));
    returnType("MAX(FIELD_NUMBER)", Types.NUMBER);
    returnType("MAX(FIELD_DATE)", Types.DATE);
  }

  @Test
  void Min() throws Exception {
    returnType("MIN(FIELD_STRING)", StringType.of(1000));
    returnType("MIN(FIELD_INTEGER)", IntegerType.of(12));
    returnType("MIN(FIELD_NUMBER)", Types.NUMBER);
    returnType("MIN(FIELD_DATE)", Types.DATE);
  }

  @Test
  void Median() throws Exception {
    returnType("MEDIAN(FIELD_INTEGER)", Types.NUMBER);
  }

  @Test
  void AnyValue() throws Exception {
    returnType("Any_Value(FIELD_DATE)", Types.DATE);
  }

  @Test
  void FirstValue() throws Exception {
    evalFails("FIRST_VALUE(FIELD_DATE) IGNORE", ErrorCode.CALL_FUNCTION_ERROR);
    evalFails("FIRST_VALUE(FIELD_DATE) NULLS", ErrorCode.UNEXPECTED_CHARACTER);

    optimize("FIRST_VALUE(FIELD_DATE) RESPECT NULLS", "FIRST_VALUE(FIELD_DATE)");
    optimize("FIRST_VALUE(FIELD_DATE) IGNORE NULLS");

    returnType("FIRST_VALUE(FIELD_DATE)", Types.DATE);
  }

  @Test
  void LastValue() throws Exception {
    evalFails("LAST_VALUE(FIELD_DATE) IGNORE", ErrorCode.CALL_FUNCTION_ERROR);
    evalFails("LAST_VALUE(FIELD_DATE) NULLS", ErrorCode.UNEXPECTED_CHARACTER);

    optimize("LAST_VALUE(FIELD_DATE) RESPECT NULLS", "LAST_VALUE(FIELD_DATE)");
    optimize("LAST_VALUE(FIELD_DATE) IGNORE NULLS");

    returnType("LAST_VALUE(FIELD_DATE)", Types.DATE);
  }

  @Test
  void NthValue() throws Exception {
    evalFails("NTH_VALUE(FIELD_DATE)", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("NTH_VALUE(FIELD_DATE) IGNORE", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("NTH_VALUE(FIELD_DATE) NULLS", ErrorCode.SYNTAX_ERROR_FUNCTION);

    optimize("NTH_VALUE(FIELD_DATE,2) RESPECT NULLS", "NTH_VALUE(FIELD_DATE,2)");
    optimize("NTH_VALUE(FIELD_DATE,2) IGNORE NULLS");

    returnType("NTH_VALUE(FIELD_DATE,3)", Types.DATE);
  }

  @Test
  void ListAgg() throws Exception {
    returnType("ListAGG(FIELD_STRING,',')", Types.STRING);
  }

  @Test
  void Percentile() throws Exception {
    returnType("Percentile(FIELD_INTEGER,0.75)", Types.NUMBER);
  }

  @Test
  void VarPop() throws Exception {
    returnType("Variance_Pop(FIELD_INTEGER)", Types.NUMBER);
  }

  void VarSamp() throws Exception {
    returnType("Variance_Samp(FIELD_INTEGER)", Types.NUMBER);
  }

  @Test
  void StdDevPop() throws Exception {
    returnType("StdDev_Pop(FIELD_INTEGER)", Types.NUMBER);
  }

  @Test
  void StdDevSamp() throws Exception {
    returnType("StdDev_Samp(FIELD_INTEGER)", Types.NUMBER);
  }
}
