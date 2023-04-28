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

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaBigNumber;
import org.apache.hop.core.row.value.ValueMetaBinary;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaJson;
import org.apache.hop.core.row.value.ValueMetaNumber;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.core.row.value.ValueMetaTimestamp;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.ExpressionBuilder;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.DataType;
import org.apache.hop.junit.rules.RestoreHopEnvironment;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.ExternalResource;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.Temporal;
import java.util.Calendar;
import java.util.Locale;
import com.fasterxml.jackson.databind.JsonNode;

public class ExpressionTest {

  protected static final String ANSI_RESET = "\u001B[0m";
  protected static final String ANSI_BLACK = "\u001B[30m";
  protected static final String ANSI_RED = "\u001B[31m";
  protected static final String ANSI_GREEN = "\u001B[32m";
  protected static final String ANSI_YELLOW = "\u001B[33m";
  protected static final String ANSI_BLUE = "\u001B[34m";
  protected static final String ANSI_PURPLE = "\u001B[35m";
  protected static final String ANSI_CYAN = "\u001B[36m";
  protected static final String ANSI_WHITE = "\u001B[37m";

  @ClassRule
  public static RestoreHopEnvironment env = new RestoreHopEnvironment();

  @ClassRule
  public static ExternalResource getResource() {
    return new ExternalResource() {
      @Override
      protected void before() throws Throwable {
        FunctionRegistry.registerBuilInFunctions();
      }
    };
  }

  protected ExpressionContext createExpressionContext() throws Exception {
    return this.createExpressionContext(true);
  }

  protected ExpressionContext createExpressionContext(boolean withData) throws Exception {
    IVariables variables = new Variables();
    variables.setVariable("TEST", "12345");

    IRowMeta rowMeta = new RowMeta();
    rowMeta.addValueMeta(new ValueMetaString("FIELD_STRING"));
    rowMeta.addValueMeta(new ValueMetaString("SEX2"));
    rowMeta.addValueMeta(new ValueMetaDate("BIRTHDATE2"));
    rowMeta.addValueMeta(new ValueMetaInteger("FIELD_INTEGER"));
    rowMeta.addValueMeta(new ValueMetaNumber("FIELD_NUMBER"));
    rowMeta.addValueMeta(new ValueMetaBigNumber("FIELD_BIGNUMBER"));
    rowMeta.addValueMeta(new ValueMetaDate("FIELD_DATE"));
    rowMeta.addValueMeta(new ValueMetaTimestamp("FIELD_TIMESTAMP"));
    rowMeta.addValueMeta(new ValueMetaBoolean("FIELD_BOOLEAN"));
    rowMeta.addValueMeta(new ValueMetaBinary("FIELD_BINARY"));

    // Null values
    rowMeta.addValueMeta(new ValueMetaString("NULL_STRING"));
    rowMeta.addValueMeta(new ValueMetaBoolean("NULL_BOOLEAN"));
    rowMeta.addValueMeta(new ValueMetaInteger("NULL_INTEGER"));
    rowMeta.addValueMeta(new ValueMetaNumber("NULL_NUMBER"));
    rowMeta.addValueMeta(new ValueMetaBigNumber("NULL_BIGNUMBER"));
    rowMeta.addValueMeta(new ValueMetaDate("NULL_DATE"));
    rowMeta.addValueMeta(new ValueMetaTimestamp("NULL_TIMESTAMP"));
    rowMeta.addValueMeta(new ValueMetaJson("NULL_BINARY"));
    rowMeta.addValueMeta(new ValueMetaJson("NULL_JSON"));

    // For implicit cast
    rowMeta.addValueMeta(new ValueMetaString("STRING_BOOLEAN"));
    rowMeta.addValueMeta(new ValueMetaString("STRING_INTEGER"));
    rowMeta.addValueMeta(new ValueMetaString("STRING_NUMBER"));

    // Reserved words
    rowMeta.addValueMeta(new ValueMetaInteger("YEAR"));
    rowMeta.addValueMeta(new ValueMetaString("STRING"));
    rowMeta.addValueMeta(new ValueMetaBoolean("CASE"));
    rowMeta.addValueMeta(new ValueMetaInteger("CENTURY"));

    // Special identifier
    rowMeta.addValueMeta(new ValueMetaString("IDENTIFIER SPACE"));
    rowMeta.addValueMeta(new ValueMetaString("IDENTIFIER_UNDERSCORE"));
    rowMeta.addValueMeta(new ValueMetaString("IDENTIFIER lower"));

    ExpressionContext context = new ExpressionContext(variables, rowMeta);

    if (withData) {
      Calendar calendar = Calendar.getInstance();
      calendar.set(1981, 5, 23);

      Object[] row = new Object[30];
      row[0] = "TEST";
      row[1] = "F";
      row[2] = calendar.getTime();
      row[3] = 40L;
      row[4] = -5.12D;
      row[5] = BigDecimal.valueOf(123456.789);
      row[6] = calendar.getTime();
      row[7] = Timestamp.valueOf("2023-02-28 22:11:01");
      row[8] = true;
      row[9] = "TEST".getBytes();

      row[10] = null;
      row[11] = null;
      row[12] = null;
      row[13] = null;
      row[14] = null;
      row[15] = null;
      row[16] = null;
      row[17] = null;
      row[18] = null;

      row[19] = "True";
      row[20] = "25";
      row[21] = "-12.56";


      row[22] = 2020L;
      row[23] = "Paris";
      row[24] = true;
      row[25] = 2;
      row[26] = "SPACE";
      row[27] = "UNDERSCORE";
      row[28] = "lower";
      context.setRow(row);
    }


    return context;
  }

  protected void returnType(String source, DataType expected) throws Exception {
    ExpressionContext context = createExpressionContext(false);
    IExpression expression = ExpressionBuilder.build(context, source);
    assertEquals(expected, expression.getType());
  }

  protected Object eval(IExpressionContext context, String source) throws Exception {

    // Create default context
    if (context == null) {
      context = createExpressionContext(true);
    }

    try {
      IExpression expression = ExpressionBuilder.build(context, source);

      return expression.getValue(context);
    } catch (Exception ex) {
      System.err.println(ANSI_WHITE + source + "  " + ANSI_RED + ex.getMessage() + ANSI_RESET);
      throw ex;
    }
  }

  protected <T> T eval(IExpressionContext context, String source, final Class<T> clazz)
      throws Exception {

    // Create default context
    if (context == null) {
      context = createExpressionContext(true);
    }

    try {
      IExpression expression = ExpressionBuilder.build(context, source);

      return expression.getValue(context, clazz);
    } catch (Exception ex) {
      System.err.println(ANSI_WHITE + source + "  " + ANSI_RED + ex.getMessage() + ANSI_RESET);
      throw ex;
    }
  }

  protected void evalNull(String source) throws Exception {
    assertNull(eval(createExpressionContext(true), source));
  }

  protected void evalTrue(String source) throws Exception {
    assertEquals(Boolean.TRUE, eval(createExpressionContext(true), source, Boolean.class));
  }

  protected void evalFalse(String source) throws Exception {
    assertEquals(Boolean.FALSE, eval(createExpressionContext(true), source, Boolean.class));
  }

  protected void evalEquals(String source, byte[] expected) throws Exception {
    assertArrayEquals(expected, eval(createExpressionContext(true), source, byte[].class));
  }

  protected void evalEquals(String source, JsonNode expected) throws Exception {
    assertEquals(expected, (JsonNode) eval(createExpressionContext(true), source, JsonNode.class));
  }

  protected void evalEquals(String source, String expected) throws Exception {
    assertEquals(expected, eval(createExpressionContext(true), source, String.class));
  }

  protected void evalEquals(String source, Long expected) throws Exception {
    assertEquals(expected, eval(createExpressionContext(true), source, Long.class));
  }

  protected void evalEquals(ExpressionContext context, String source, Long expected)
      throws Exception {
    assertEquals(expected, eval(context, source, Long.class));
  }

  protected void evalEquals(ExpressionContext context, String source, Double expected)
      throws Exception {
    assertEquals(expected, eval(context, source, Double.class));
  }

  protected void evalEquals(String source, Double expected) throws Exception {
    assertEquals(expected, eval(createExpressionContext(true), source, Double.class),
        0.000000000000001);
  }

  protected void evalEquals(String source, BigDecimal expected) throws Exception {
    assertEquals(expected, eval(createExpressionContext(true), source, BigDecimal.class));
  }

  protected void evalEquals(String source, Temporal expected) throws Exception {
    evalEquals(createExpressionContext(true), source, expected);
  }

  protected void evalEquals(ExpressionContext context, String source, Temporal expected)
      throws Exception {

    Object result = eval(context, source);

    if (result instanceof LocalDateTime) {
      LocalDateTime value = (LocalDateTime) result;

      if (expected instanceof LocalDate) {
        result = value.toLocalDate();
      } else if (expected instanceof ZonedDateTime) {
        result = value.atZone(ZoneId.systemDefault());
      }
    }

    if (result instanceof ZonedDateTime) {
      ZonedDateTime value = (ZonedDateTime) result;

      if (expected instanceof LocalDate) {
        result = value.toLocalDate();
      } else if (expected instanceof LocalDateTime) {
        result = value.toLocalDateTime();
      }
    }

    assertEquals(expected, result);
  }

  protected void evalFails(final String source) {
    assertThrows(ExpressionException.class, () -> eval(createExpressionContext(true), source));
  }

  protected void writeEquals(String source) throws Exception {
    writeEquals(source, source);
  }

  protected void writeEquals(String source, String result) throws Exception {
    IExpressionContext context = createExpressionContext(false);
    IExpression expression = ExpressionBuilder.build(context, source);

    StringWriter writer = new StringWriter();
    expression.unparse(writer);
    assertEquals(result, writer.toString());
  }

  protected IExpression optimize(String e) throws Exception {
    IExpressionContext context = createExpressionContext(false);
    IExpression expression = ExpressionBuilder.build(context, e);

    String color = ANSI_YELLOW;
    if (expression.getType() == DataType.UNKNOWN) {
      color = ANSI_RED;
    }

    System.out.println(
        e + ANSI_PURPLE + " cost=" + expression.getCost() + "  " + color + expression + ANSI_RESET);

    return expression;
  }

  protected void optimize(String e, String expected) throws Exception {
    assertEquals(expected, optimize(e).toString());
  }

  protected void optimizeTrue(String e) throws Exception {
    assertEquals("TRUE", optimize(e).toString());
  }

  protected void optimizeFalse(String e) throws Exception {
    assertEquals("FALSE", optimize(e).toString());
  }

  protected void optimizeNull(String e) throws Exception {
    assertEquals("NULL", optimize(e).toString());
  }

  @Test
  public void test() throws Exception {
    // ExpressionContext context = createExpressionContext();
    // context.setVariable(ExpressionContext.EXPRESSION_TWO_DIGIT_YEAR_START, "1970");
    // evalEquals("To_Date('01/02/80','DD/MM/YY')", LocalDate.of(1980, 2, 1), context);
    // context.setVariable(ExpressionContext.EXPRESSION_TWO_DIGIT_YEAR_START, "2000");
    Locale.setDefault(new Locale("fr", "BE"));
    //evalFails("DATE '21-02-25'");
    //evalFails("CAST('2023-01-01' AS DATE FORMAT 'YYYY-MM')");    
  }
}

