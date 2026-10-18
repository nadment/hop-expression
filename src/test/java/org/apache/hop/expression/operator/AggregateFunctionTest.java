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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.math.BigDecimal;
import org.apache.hop.expression.AggregateFunction;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionTest;
import org.apache.hop.expression.IAggregateProcessor;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IRowExpressionContext;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.junit.jupiter.api.Test;

class AggregateFunctionTest extends ExpressionTest {

  /**
   * Evaluate an aggregate expression over a group of rows, as the Aggregate transform does.
   *
   * @param source the aggregate expression
   * @param values the values of FIELD_INTEGER, one per row
   * @return the aggregated value
   */
  private Object evalAggregate(String source, Long... values) throws Exception {
    IRowExpressionContext context = createExpressionContext();
    Call call = (Call) IExpression.of(context, source);
    AggregateFunction function = (AggregateFunction) call.getOperator();
    IAggregateProcessor processor = function.createProcessor(context, call.getOperands());

    Object[] row = context.getRow().clone();
    for (Long value : values) {
      row[context.getRowMeta().indexOfValue("FIELD_INTEGER")] = value;
      context.setRow(row);
      processor.process(call.getOperands());
    }
    return processor.getValue();
  }

  @Test
  void Avg() throws Exception {
    evalFails("AVG()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("AVG(2,4)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("AVG(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("AVG(", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    optimize("AVG(FIELD_INTEGER)").returnType(NumberType.NUMBER);
    optimize("AVG(FIELD_NUMBER)").returnType(NumberType.NUMBER);

    // The average of integers is not rounded
    assertEquals(new BigDecimal("12.5"), evalAggregate("AVG(FIELD_INTEGER)", 10L, 15L));
    assertEquals(
        new BigDecimal("1.6666666666666666666666666666667"),
        evalAggregate("AVG(FIELD_INTEGER)", 1L, 2L, 2L));

    // Null values are ignored
    assertEquals(new BigDecimal("15"), evalAggregate("AVG(FIELD_INTEGER)", 10L, null, 20L));

    // The average of an empty group is NULL
    assertNull(evalAggregate("AVG(FIELD_INTEGER)"));
    assertNull(evalAggregate("AVG(FIELD_INTEGER)", (Long) null));
  }

  @Test
  void Count() throws Exception {
    evalFails("Count()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Count(ALL )", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Count(DISTINCT )", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Count(1,2)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("Count(", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    optimize("COUNT(*)").returnType(IntegerType.INTEGER_NOT_NULL);
    optimize("COUNT(FIELD_INTEGER)", "COUNT(ALL FIELD_INTEGER)")
        .returnType(IntegerType.INTEGER_NOT_NULL);
    optimize("COUNT(ALL FIELD_STRING)").returnType(IntegerType.INTEGER_NOT_NULL);
    optimize("COUNT(DISTINCT FIELD_STRING)").returnType(IntegerType.INTEGER_NOT_NULL);

    // Count rows, non-null values and distinct non-null values
    assertEquals(4L, evalAggregate("COUNT(*)", 1L, null, 2L, 1L));
    assertEquals(3L, evalAggregate("COUNT(FIELD_INTEGER)", 1L, null, 2L, 1L));
    assertEquals(2L, evalAggregate("COUNT(DISTINCT FIELD_INTEGER)", 1L, null, 2L, 1L));

    // The count of an empty group is zero
    assertEquals(0L, evalAggregate("COUNT(*)"));
    assertEquals(0L, evalAggregate("COUNT(FIELD_INTEGER)"));
    assertEquals(0L, evalAggregate("COUNT(DISTINCT FIELD_INTEGER)"));
  }

  @Test
  void CountIf() throws Exception {
    evalFails("CountIf()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("CountIf(1,2)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("CountIf(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("CountIf(", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    optimize("COUNTIF(FIELD_INTEGER>=10)").returnType(IntegerType.INTEGER);
    optimize("COUNTIF(FIELD_INTEGER>=10)", "COUNTIF(FIELD_INTEGER>=10)");

    // A NULL predicate is not counted
    assertEquals(2L, evalAggregate("COUNTIF(FIELD_INTEGER>1)", 1L, null, 2L, 3L));
    assertEquals(0L, evalAggregate("COUNTIF(FIELD_INTEGER>1)"));
  }

  @Test
  void Sum() throws Exception {
    evalFails("SUM()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("SUM(", ErrorCode.MISSING_RIGHT_PARENTHESIS);
    evalFails("SUM(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);

    optimize("SUM(FIELD_INTEGER)").returnType(NumberType.NUMBER);

    // Null values are ignored
    assertEquals(new BigDecimal("3"), evalAggregate("SUM(FIELD_INTEGER)", 1L, null, 2L));

    // The sum of an empty group is NULL
    assertNull(evalAggregate("SUM(FIELD_INTEGER)"));
    assertNull(evalAggregate("SUM(FIELD_INTEGER)", (Long) null));
  }

  @Test
  void Max() throws Exception {
    evalFails("MAX()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("MAX(", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    optimize("MAX(FIELD_STRING)").returnType(StringType.of(1000));
    optimize("MAX(FIELD_INTEGER)").returnType(IntegerType.of(12));
    optimize("MAX(FIELD_NUMBER)").returnType(NumberType.NUMBER);
    optimize("MAX(FIELD_DATE)").returnType(DateType.DATE);
    optimize("MAX(FIELD_BINARY)").returnType(BinaryType.BINARY);

    // Null values are ignored
    assertEquals(3L, evalAggregate("MAX(FIELD_INTEGER)", 1L, null, 3L, 2L));
    assertNull(evalAggregate("MAX(FIELD_INTEGER)"));
  }

  @Test
  void Min() throws Exception {
    evalFails("MIN()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("MIN(", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    optimize("MIN(FIELD_STRING)").returnType(StringType.of(1000));
    optimize("MIN(FIELD_INTEGER)").returnType(IntegerType.of(12));
    optimize("MIN(FIELD_NUMBER)").returnType(NumberType.NUMBER);
    optimize("MIN(FIELD_DATE)").returnType(DateType.DATE);
    optimize("MIN(FIELD_BINARY)").returnType(BinaryType.BINARY);

    // Null values are ignored
    assertEquals(3L, evalAggregate("MIN(FIELD_INTEGER)", 4L, null, 3L, 5L));
    assertNull(evalAggregate("MIN(FIELD_INTEGER)"));
  }

  @Test
  void Median() throws Exception {
    evalFails("MEDIAN()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("MEDIAN(", ErrorCode.MISSING_RIGHT_PARENTHESIS);
    evalFails("MEDIAN(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);

    optimize("MEDIAN(FIELD_INTEGER)").returnType(NumberType.NUMBER);

    assertEquals(BigDecimal.valueOf(2.0), evalAggregate("MEDIAN(FIELD_INTEGER)", 1L, 3L, 2L));
    assertEquals(
        BigDecimal.valueOf(2.5), evalAggregate("MEDIAN(FIELD_INTEGER)", 1L, 3L, null, 2L, 10L));

    // The median of an empty group is NULL
    assertNull(evalAggregate("MEDIAN(FIELD_INTEGER)"));
    assertNull(evalAggregate("MEDIAN(FIELD_INTEGER)", (Long) null));
  }

  @Test
  void AnyValue() throws Exception {
    evalFails("ANY_VALUE()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("ANY_VALUE(", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    optimize("ANY_VALUE(FIELD_DATE)").returnType(DateType.DATE);
    optimize("ANY_VALUE(FIELD_INTEGER)").returnType(IntegerType.of(12));
    optimize("ANY_VALUE(FIELD_STRING)").returnType(StringType.of(1000));

    // Returns a non-null value if any
    assertEquals(5L, evalAggregate("ANY_VALUE(FIELD_INTEGER)", null, 5L, 6L));
    assertNull(evalAggregate("ANY_VALUE(FIELD_INTEGER)"));
  }

  @Test
  void FirstValue() throws Exception {
    evalFails("FIRST_VALUE()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("FIRST_VALUE(1,2)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("FIRST_VALUE(", ErrorCode.MISSING_RIGHT_PARENTHESIS);
    evalFails("FIRST_VALUE(FIELD_DATE) IGNORE", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("FIRST_VALUE(FIELD_DATE) RESPECT", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("FIRST_VALUE(FIELD_DATE) NULLS", ErrorCode.UNEXPECTED_CHARACTER);

    optimize("FIRST_VALUE(FIELD_STRING)").returnType(StringType.of(1000));
    optimize("FIRST_VALUE(FIELD_DATE)").returnType(DateType.DATE);
    optimize("FIRST_VALUE(FIELD_DATE) RESPECT NULLS", "FIRST_VALUE(FIELD_DATE)");
    optimize("FIRST_VALUE(FIELD_DATE) IGNORE NULLS");

    assertEquals(5L, evalAggregate("FIRST_VALUE(FIELD_INTEGER)", 5L, null, 6L));
    assertNull(evalAggregate("FIRST_VALUE(FIELD_INTEGER)"));

    // Null values are respected unless IGNORE NULLS is specified
    assertNull(evalAggregate("FIRST_VALUE(FIELD_INTEGER)", null, 5L, 6L));
    assertNull(evalAggregate("FIRST_VALUE(FIELD_INTEGER) RESPECT NULLS", null, 5L, 6L));
    assertEquals(5L, evalAggregate("FIRST_VALUE(FIELD_INTEGER) IGNORE NULLS", null, 5L, 6L));
  }

  @Test
  void LastValue() throws Exception {
    evalFails("LAST_VALUE()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("LAST_VALUE(1,2)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("LAST_VALUE(", ErrorCode.MISSING_RIGHT_PARENTHESIS);
    evalFails("LAST_VALUE(FIELD_DATE) IGNORE", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("LAST_VALUE(FIELD_DATE) RESPECT", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("LAST_VALUE(FIELD_DATE) NULLS", ErrorCode.UNEXPECTED_CHARACTER);

    optimize("LAST_VALUE(FIELD_STRING)").returnType(StringType.of(1000));
    optimize("LAST_VALUE(FIELD_INTEGER)").returnType(IntegerType.of(12));
    optimize("LAST_VALUE(FIELD_DATE)").returnType(DateType.DATE);
    optimize("LAST_VALUE(FIELD_DATE) RESPECT NULLS", "LAST_VALUE(FIELD_DATE)");
    optimize("LAST_VALUE(FIELD_DATE) IGNORE NULLS").returnType(DateType.DATE);

    assertEquals(6L, evalAggregate("LAST_VALUE(FIELD_INTEGER)", 5L, null, 6L));
    assertNull(evalAggregate("LAST_VALUE(FIELD_INTEGER)"));

    // Null values are respected unless IGNORE NULLS is specified
    assertNull(evalAggregate("LAST_VALUE(FIELD_INTEGER)", 5L, 6L, null));
    assertNull(evalAggregate("LAST_VALUE(FIELD_INTEGER) RESPECT NULLS", 5L, 6L, null));
    assertEquals(6L, evalAggregate("LAST_VALUE(FIELD_INTEGER) IGNORE NULLS", 5L, 6L, null));
  }

  @Test
  void NthValue() throws Exception {
    evalFails("NTH_VALUE()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("NTH_VALUE(FIELD_DATE)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("NTH_VALUE(", ErrorCode.MISSING_RIGHT_PARENTHESIS);
    evalFails("NTH_VALUE(FIELD_DATE,1) IGNORE", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("NTH_VALUE(FIELD_DATE,1) RESPECT", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("NTH_VALUE(FIELD_DATE,1) NULLS", ErrorCode.UNEXPECTED_CHARACTER);

    optimize("NTH_VALUE(FIELD_STRING,1)").returnType(StringType.of(1000));
    optimize("NTH_VALUE(FIELD_DATE,3)").returnType(DateType.DATE);
    optimize("NTH_VALUE(FIELD_DATE,2) RESPECT NULLS", "NTH_VALUE(FIELD_DATE,2)");
    optimize("NTH_VALUE(FIELD_DATE,2) IGNORE NULLS");

    assertEquals(10L, evalAggregate("NTH_VALUE(FIELD_INTEGER,1)", 10L, 20L, 30L));
    assertEquals(30L, evalAggregate("NTH_VALUE(FIELD_INTEGER,3)", 10L, 20L, 30L));

    // The offset is beyond the number of rows
    assertNull(evalAggregate("NTH_VALUE(FIELD_INTEGER,4)", 10L, 20L, 30L));

    // Null values are counted unless IGNORE NULLS is specified
    assertNull(evalAggregate("NTH_VALUE(FIELD_INTEGER,2)", 10L, null, 20L, 30L));
    assertNull(evalAggregate("NTH_VALUE(FIELD_INTEGER,2) RESPECT NULLS", 10L, null, 20L, 30L));
    assertEquals(
        20L, evalAggregate("NTH_VALUE(FIELD_INTEGER,2) IGNORE NULLS", 10L, null, 20L, 30L));

    // The offset is mandatory
    ExpressionException exception =
        assertThrows(
            ExpressionException.class,
            () -> evalAggregate("NTH_VALUE(FIELD_INTEGER,NULL_INTEGER)", 10L));
    assertEquals(ErrorCode.ARGUMENT_OUT_OF_RANGE, exception.getErrorCode());

    // The offset starts at 1
    exception =
        assertThrows(
            ExpressionException.class, () -> evalAggregate("NTH_VALUE(FIELD_INTEGER,0)", 10L));
    assertEquals(ErrorCode.ARGUMENT_OUT_OF_RANGE, exception.getErrorCode());
    exception =
        assertThrows(
            ExpressionException.class, () -> evalAggregate("NTH_VALUE(FIELD_INTEGER,-1)", 10L));
    assertEquals(ErrorCode.ARGUMENT_OUT_OF_RANGE, exception.getErrorCode());
  }

  @Test
  void ListAgg() throws Exception {
    evalFails("LISTAGG()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("LISTAGG(", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    optimize("LISTAGG(FIELD_STRING,',')", "LISTAGG(ALL FIELD_STRING,',')")
        .returnType(StringType.STRING);
    optimize("LISTAGG(ALL FIELD_STRING,',')").returnType(StringType.STRING);
    optimize("LISTAGG(DISTINCT FIELD_STRING,',')").returnType(StringType.STRING);

    // Null values are ignored and the default delimiter is a comma
    assertEquals("1;2;1", evalAggregate("LISTAGG(FIELD_INTEGER::STRING,';')", 1L, null, 2L, 1L));
    assertEquals(
        "1;2", evalAggregate("LISTAGG(DISTINCT FIELD_INTEGER::STRING,';')", 1L, null, 2L, 1L));
    assertEquals("1,2", evalAggregate("LISTAGG(FIELD_INTEGER::STRING)", 1L, 2L));

    // The list of an empty group is NULL
    assertNull(evalAggregate("LISTAGG(FIELD_INTEGER::STRING,';')"));
    assertNull(evalAggregate("LISTAGG(FIELD_INTEGER::STRING,';')", (Long) null));
  }

  @Test
  void Percentile() throws Exception {
    evalFails("PERCENTILE()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("PERCENTILE(", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    optimize("PERCENTILE(FIELD_INTEGER,0.75)").returnType(NumberType.NUMBER);

    assertEquals(
        BigDecimal.valueOf(12.5),
        evalAggregate("PERCENTILE(FIELD_INTEGER,0.25)", 10L, 20L, 30L, 40L));
    assertEquals(
        BigDecimal.valueOf(25.0),
        evalAggregate("PERCENTILE(FIELD_INTEGER,0.5)", 10L, 20L, 30L, 40L));
    assertEquals(
        BigDecimal.valueOf(37.5),
        evalAggregate("PERCENTILE(FIELD_INTEGER,0.75)", 10L, 20L, 30L, 40L));

    // Null values are ignored
    assertEquals(
        BigDecimal.valueOf(25.0),
        evalAggregate("PERCENTILE(FIELD_INTEGER,0.5)", 10L, null, 20L, 30L, 40L));

    // The percentages 0 and 1 are the minimum and maximum values
    assertEquals(
        BigDecimal.valueOf(10.0), evalAggregate("PERCENTILE(FIELD_INTEGER,0)", 10L, 20L, 30L, 40L));
    assertEquals(
        BigDecimal.valueOf(40.0), evalAggregate("PERCENTILE(FIELD_INTEGER,1)", 10L, 20L, 30L, 40L));

    // The percentile of an empty group is NULL
    assertNull(evalAggregate("PERCENTILE(FIELD_INTEGER,0.5)"));
    assertNull(evalAggregate("PERCENTILE(FIELD_INTEGER,0.5)", (Long) null));

    // The percentage is mandatory
    ExpressionException exception =
        assertThrows(
            ExpressionException.class,
            () -> evalAggregate("PERCENTILE(FIELD_INTEGER,NULL_NUMBER)", 10L));
    assertEquals(ErrorCode.ARGUMENT_OUT_OF_RANGE, exception.getErrorCode());

    // The percentage is between 0 and 1
    exception =
        assertThrows(
            ExpressionException.class, () -> evalAggregate("PERCENTILE(FIELD_INTEGER,1.5)", 10L));
    assertEquals(ErrorCode.ARGUMENT_OUT_OF_RANGE, exception.getErrorCode());
    exception =
        assertThrows(
            ExpressionException.class, () -> evalAggregate("PERCENTILE(FIELD_INTEGER,-0.1)", 10L));
    assertEquals(ErrorCode.ARGUMENT_OUT_OF_RANGE, exception.getErrorCode());
  }

  @Test
  void VarPop() throws Exception {
    evalFails("VARIANCE_POP()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("VARIANCE_POP(", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    optimize("VARIANCE_POP(FIELD_INTEGER)").returnType(NumberType.NUMBER);

    assertEquals(
        new BigDecimal("4"),
        evalAggregate("VARIANCE_POP(FIELD_INTEGER)", 2L, 4L, 4L, null, 4L, 5L, 5L, 7L, 9L));

    // The variance of an empty group is NULL
    assertNull(evalAggregate("VARIANCE_POP(FIELD_INTEGER)"));
    assertNull(evalAggregate("VARIANCE_POP(FIELD_INTEGER)", (Long) null));
  }

  @Test
  void VarSamp() throws Exception {
    evalFails("VARIANCE_SAMP()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("VARIANCE_SAMP(", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    optimize("VARIANCE_SAMP(FIELD_INTEGER)").returnType(NumberType.NUMBER);

    assertEquals(
        new BigDecimal("4.5714285714285714285714285714286"),
        evalAggregate("VARIANCE_SAMP(FIELD_INTEGER)", 2L, 4L, 4L, null, 4L, 5L, 5L, 7L, 9L));

    // The variance of an empty group is NULL
    assertNull(evalAggregate("VARIANCE_SAMP(FIELD_INTEGER)"));
    assertNull(evalAggregate("VARIANCE_SAMP(FIELD_INTEGER)", (Long) null));
  }

  @Test
  void StdDevPop() throws Exception {
    evalFails("STDDEV_POP()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("STDDEV_POP(", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    optimize("STDDEV_POP(FIELD_INTEGER)").returnType(NumberType.NUMBER);

    assertEquals(
        new BigDecimal("2"),
        evalAggregate("STDDEV_POP(FIELD_INTEGER)", 2L, 4L, 4L, null, 4L, 5L, 5L, 7L, 9L));

    // The standard deviation of an empty group is NULL
    assertNull(evalAggregate("STDDEV_POP(FIELD_INTEGER)"));
    assertNull(evalAggregate("STDDEV_POP(FIELD_INTEGER)", (Long) null));
  }

  @Test
  void StdDevSamp() throws Exception {
    evalFails("STDDEV_SAMP()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("STDDEV_SAMP(", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    optimize("STDDEV_SAMP(FIELD_INTEGER)").returnType(NumberType.NUMBER);

    assertEquals(
        new BigDecimal("2.138089935299395077476427847038"),
        evalAggregate("STDDEV_SAMP(FIELD_INTEGER)", 2L, 4L, 4L, null, 4L, 5L, 5L, 7L, 9L));

    // The standard deviation of an empty group is NULL
    assertNull(evalAggregate("STDDEV_SAMP(FIELD_INTEGER)"));
    assertNull(evalAggregate("STDDEV_SAMP(FIELD_INTEGER)", (Long) null));
  }
}
