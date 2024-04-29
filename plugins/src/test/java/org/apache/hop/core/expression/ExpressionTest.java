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

import com.fasterxml.jackson.databind.JsonNode;
import com.google.common.base.Charsets;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.InetAddress;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.temporal.Temporal;
import java.util.Calendar;
import java.util.Locale;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaBigNumber;
import org.apache.hop.core.row.value.ValueMetaBinary;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaInternetAddress;
import org.apache.hop.core.row.value.ValueMetaJson;
import org.apache.hop.core.row.value.ValueMetaNumber;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.core.row.value.ValueMetaTimestamp;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.expression.type.Interval;
import org.apache.hop.expression.type.JsonType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;
import org.apache.hop.junit.rules.RestoreHopEnvironment;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.ExternalResource;

public class ExpressionTest {
  protected static final BigDecimal PI = new BigDecimal("3.1415926535897932384626433832795");

  protected static final String ANSI_RESET = "\u001B[0m";
  protected static final String ANSI_BLACK = "\u001B[30m";
  protected static final String ANSI_RED = "\u001B[31m";
  protected static final String ANSI_GREEN = "\u001B[32m";
  protected static final String ANSI_YELLOW = "\u001B[33m";
  protected static final String ANSI_BLUE = "\u001B[34m";
  protected static final String ANSI_PURPLE = "\u001B[35m";
  protected static final String ANSI_CYAN = "\u001B[36m";
  protected static final String ANSI_WHITE = "\u001B[37m";

  @ClassRule public static RestoreHopEnvironment env = new RestoreHopEnvironment();

  @ClassRule
  public static ExternalResource getResource() {
    return new ExternalResource() {
      @Override
      protected void before() throws Throwable {
        FunctionRegistry.registerPluginFunctions();
      }
    };
  }

  public static class Evaluator {
    private final IExpression expression;
    private final String source;

    public Evaluator(IExpressionContext context, String source) {
      this.expression = context.createExpression(source);
      this.source = source;
    }

    public <T> T eval(Class<T> clazz) throws Exception {
      try {
        return expression.getValue(clazz);
      } catch (Exception ex) {
        System.err.println(ANSI_WHITE + source + "  " + ANSI_RED + ex.getMessage() + ANSI_RESET);
        throw ex;
      }
    }

    public void returnType(Type expectedType) {
      // assertTrue(expectedType.equalsIgnoreNullability(expression.getType()));
      assertEquals(expectedType.withNullability(true), expression.getType().withNullability(true));
    }
  }

  protected IExpressionContext createExpressionContext() throws Exception {
    return this.createExpressionContext(true);
  }

  protected IExpressionContext createExpressionContext(boolean withData) throws Exception {
    IVariables variables = new Variables();
    variables.setVariable("TEST", "12345");

    IRowMeta rowMeta = new RowMeta();
    rowMeta.addValueMeta(new ValueMetaString("FIELD_STRING", 1000, -1));
    rowMeta.addValueMeta(new ValueMetaDate("BIRTHDATE2"));
    rowMeta.addValueMeta(new ValueMetaInteger("FIELD_INTEGER", 12, -1));
    rowMeta.addValueMeta(new ValueMetaNumber("FIELD_NUMBER"));
    rowMeta.addValueMeta(new ValueMetaBigNumber("FIELD_BIGNUMBER"));
    rowMeta.addValueMeta(new ValueMetaDate("FIELD_DATE"));
    rowMeta.addValueMeta(new ValueMetaTimestamp("FIELD_TIMESTAMP"));
    rowMeta.addValueMeta(new ValueMetaBoolean("FIELD_BOOLEAN_TRUE"));
    rowMeta.addValueMeta(new ValueMetaBoolean("FIELD_BOOLEAN_FALSE"));
    rowMeta.addValueMeta(new ValueMetaBinary("FIELD_BINARY"));
    rowMeta.addValueMeta(new ValueMetaInternetAddress("FIELD_INET"));
    rowMeta.addValueMeta(new ValueMetaJson("FIELD_JSON"));

    // Null values
    rowMeta.addValueMeta(new ValueMetaString("NULL_STRING"));
    rowMeta.addValueMeta(new ValueMetaBoolean("NULL_BOOLEAN"));
    rowMeta.addValueMeta(new ValueMetaInteger("NULL_INTEGER"));
    rowMeta.addValueMeta(new ValueMetaNumber("NULL_NUMBER"));
    rowMeta.addValueMeta(new ValueMetaBigNumber("NULL_BIGNUMBER"));
    rowMeta.addValueMeta(new ValueMetaDate("NULL_DATE"));
    rowMeta.addValueMeta(new ValueMetaTimestamp("NULL_TIMESTAMP"));
    rowMeta.addValueMeta(new ValueMetaBinary("NULL_BINARY"));
    rowMeta.addValueMeta(new ValueMetaJson("NULL_JSON"));

    // Zero values
    rowMeta.addValueMeta(new ValueMetaInteger("FIELD_INTEGER_ZERO"));
    rowMeta.addValueMeta(new ValueMetaNumber("FIELD_NUMBER_ZERO"));
    rowMeta.addValueMeta(new ValueMetaBigNumber("FIELD_BIGNUMBER_ZERO"));

    // For implicit cast
    rowMeta.addValueMeta(new ValueMetaString("FIELD_STRING_BOOLEAN_TRUE"));
    rowMeta.addValueMeta(new ValueMetaString("FIELD_STRING_BOOLEAN_FALSE"));
    rowMeta.addValueMeta(new ValueMetaString("FIELD_STRING_INTEGER"));
    rowMeta.addValueMeta(new ValueMetaString("FIELD_STRING_NUMBER"));
    rowMeta.addValueMeta(new ValueMetaString("FIELD_STRING_JSON"));

    // Reserved words
    rowMeta.addValueMeta(new ValueMetaInteger("YEAR"));
    rowMeta.addValueMeta(new ValueMetaString("STRING"));
    rowMeta.addValueMeta(new ValueMetaBoolean("CASE"));
    rowMeta.addValueMeta(new ValueMetaBoolean("ASCII"));
    rowMeta.addValueMeta(new ValueMetaInteger("CENTURY"));

    // Special identifier
    rowMeta.addValueMeta(new ValueMetaString("IDENTIFIER SPACE"));
    rowMeta.addValueMeta(new ValueMetaString("IDENTIFIER_UNDERSCORE"));
    rowMeta.addValueMeta(new ValueMetaString("IDENTIFIER lower"));

    RowExpressionContext context = new RowExpressionContext(variables, rowMeta);

    if (withData) {
      Calendar calendar = Calendar.getInstance();
      calendar.set(1981, 5, 23);

      Object[] row = new Object[37];
      row[0] = "TEST";
      row[1] = calendar.getTime();
      row[2] = 40L;
      row[3] = -5.12D;
      row[4] = BigDecimal.valueOf(123456.789);
      row[5] = calendar.getTime();
      row[6] = Timestamp.valueOf("2023-02-28 22:11:01");
      row[7] = true;
      row[8] = false;
      row[9] = "TEST".getBytes(Charsets.UTF_8);
      row[10] = InetAddress.getByName("10.10.10.1");
      // row[11] = JsonType.convertToJson("{\"sstudent\": [{\"id\":\"01\",name:\"Tom\",\"lastname\":
      // \"Price\"},{\"id\":\"02\",\"name\": \"Nick\",\"lastname\": \"Thameson\"}]}");

      row[11] =
          JsonType.convertToJson(
              "{ \"store\":{ \"book\": [{ \"category\": \"reference\", \"author\": \"Nigel Rees\", \"title\": \"Sayings of the Century\", \"price\": 8.95 }, { \"category\": \"fiction\", \"author\": \"Evelyn Waugh\", \"title\": \"Sword of Honour\", \"price\": 12.99 }, {\"category\": \"fiction\", \"author\": \"Herman Melville\", \"title\": \"Moby Dick\", \"isbn\": \"0-553-21311-3\", \"price\": 8.99 }, {\"category\": \"fiction\", \"author\": \"J. R. R. Tolkien\", \"title\": \"The Lord of the Rings\", \"isbn\": \"0-395-19395-8\",\"price\": 22.99 }],  \"bicycle\": { \"color\": \"red\", \"price\": 19.95 } } }");

      // Null values
      row[12] = null;
      row[13] = null;
      row[14] = null;
      row[16] = null;
      row[16] = null;
      row[17] = null;
      row[18] = null;
      row[19] = null;
      row[20] = null;

      // Zero values
      row[21] = 0L;
      row[22] = 0D;
      row[23] = BigDecimal.ZERO;

      // String
      row[24] = "True";
      row[25] = "False";
      row[26] = "25";
      row[27] = "-12.56";
      row[28] = "{id:\"01\",name:\"John\",age:29}";

      // Reserved words
      row[29] = 2020L;
      row[30] = "Paris";
      row[31] = true;
      row[32] = "A";
      row[33] = 2;

      // Identifier
      row[34] = "SPACE";
      row[35] = "UNDERSCORE";
      row[36] = "lower";

      context.setRow(row);
    }

    return context;
  }

  protected void returnType(String source, Type expected) throws Exception {
    IExpressionContext context = createExpressionContext(false);
    IExpression expression = context.createExpression(source);
    assertEquals(expected.withNullability(true), expression.getType().withNullability(true));
  }

  protected Object eval(String source) throws Exception {
    Evaluator evaluator = new Evaluator(createExpressionContext(true), source);
    return evaluator.eval(Object.class);
  }

  protected Evaluator evalNull(String source) throws Exception {
    Evaluator evaluator = new Evaluator(createExpressionContext(true), source);
    assertNull(evaluator.eval(Object.class));
    return evaluator;
  }

  protected Evaluator evalTrue(String source) throws Exception {
    Evaluator evaluator = new Evaluator(createExpressionContext(true), source);
    assertEquals(Boolean.TRUE, evaluator.eval(Object.class));
    return evaluator;
  }

  protected Evaluator evalFalse(String source) throws Exception {
    Evaluator evaluator = new Evaluator(createExpressionContext(true), source);
    assertEquals(Boolean.FALSE, evaluator.eval(Object.class));
    return evaluator;
  }

  protected Evaluator evalEquals(String source, byte[] expected) throws Exception {
    Evaluator evaluator = new Evaluator(createExpressionContext(true), source);
    assertArrayEquals(expected, evaluator.eval(byte[].class));
    return evaluator;
  }

  protected Evaluator evalEquals(String source, JsonNode expected) throws Exception {
    Evaluator evaluator = new Evaluator(createExpressionContext(true), source);
    assertEquals(expected, evaluator.eval(JsonNode.class));
    return evaluator;
  }

  protected Evaluator evalEquals(String source, Interval expected) throws Exception {
    Evaluator evaluator = new Evaluator(createExpressionContext(true), source);
    assertEquals(expected, evaluator.eval(Interval.class));
    return evaluator;
  }

  protected Evaluator evalEquals(String source, String expected) throws Exception {
    Evaluator evaluator = new Evaluator(createExpressionContext(true), source);
    assertEquals(expected, evaluator.eval(String.class));
    return evaluator;
  }

  protected Evaluator evalEquals(String source, Long expected) throws Exception {
    return evalEquals(createExpressionContext(true), source, expected);
  }

  protected Evaluator evalEquals(IExpressionContext context, String source, Long expected)
      throws Exception {
    Evaluator evaluator = new Evaluator(context, source);
    assertEquals(expected, evaluator.eval(Long.class));
    return evaluator;
  }

  protected Evaluator evalEquals(IExpressionContext context, String source, Interval expected)
      throws Exception {
    Evaluator evaluator = new Evaluator(createExpressionContext(true), source);
    assertEquals(expected, evaluator.eval(Interval.class));
    return evaluator;
  }

  protected Evaluator evalEquals(IExpressionContext context, String source, InetAddress expected)
      throws Exception {
    Evaluator evaluator = new Evaluator(createExpressionContext(true), source);
    assertEquals(expected, evaluator.eval(InetAddress.class));
    return evaluator;
  }

  protected Evaluator evalEquals(IExpressionContext context, String source, Double expected)
      throws Exception {
    Evaluator evaluator = new Evaluator(createExpressionContext(true), source);
    BigDecimal result = evaluator.eval(BigDecimal.class);
    assertEquals(BigDecimal.valueOf(expected).stripTrailingZeros(), result.stripTrailingZeros());
    return evaluator;
  }

  protected Evaluator evalEquals(String source, BigInteger expected) throws Exception {
    Evaluator evaluator = new Evaluator(createExpressionContext(true), source);
    assertEquals(new BigDecimal(expected), evaluator.eval(BigDecimal.class));
    return evaluator;
  }

  protected Evaluator evalEquals(String source, Double expected) throws Exception {
    return evalEquals(createExpressionContext(true), source, expected);
  }

  protected Evaluator evalEquals(String source, BigDecimal expected) throws Exception {
    Evaluator evaluator = new Evaluator(createExpressionContext(true), source);
    assertEquals(expected, evaluator.eval(BigDecimal.class));
    return evaluator;
  }

  protected Evaluator evalEquals(String source, Temporal expected) throws Exception {
    return evalEquals(createExpressionContext(true), source, expected);
  }

  protected Evaluator evalEquals(String source, InetAddress expected) throws Exception {
    return evalEquals(createExpressionContext(true), source, expected);
  }

  protected Evaluator evalEquals(IExpressionContext context, String source, Temporal expected)
      throws Exception {

    Evaluator evaluator = new Evaluator(context, source);
    Object result = evaluator.eval(Object.class);

    if (result instanceof LocalDateTime) {
      LocalDateTime value = (LocalDateTime) result;

      if (expected instanceof LocalDate) {
        result = value.toLocalDate();
      } else if (expected instanceof ZonedDateTime) {
        result = value.atZone(ZoneOffset.UTC);
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
    return evaluator;
  }

  protected void evalFails(final String source) throws Exception {
    assertThrows(
        ExpressionException.class,
        () -> {
          Evaluator evaluator = new Evaluator(createExpressionContext(true), source);
          evaluator.eval(Object.class);
        });
  }

  protected IExpression compile(String source) throws Exception {

    IExpressionContext context = createExpressionContext(false);
    IExpression expression = context.createExpression(source);

    String color = ANSI_YELLOW;
    if (expression.getType() == Types.UNKNOWN) {
      color = ANSI_RED;
    }
    System.out.println(
        source
            + ANSI_PURPLE
            + " cost="
            + expression.getCost()
            + "  "
            + color
            + expression
            + ANSI_RESET);

    return expression;
  }

  protected void optimize(String source) throws Exception {
    assertEquals(source, compile(source).toString());
  }

  protected void optimize(String source, String expected) throws Exception {
    assertEquals(expected, compile(source).toString());
  }

  protected void optimizeTrue(String source) throws Exception {
    assertEquals("TRUE", compile(source).toString());
  }

  protected void optimizeFalse(String source) throws Exception {
    assertEquals("FALSE", compile(source).toString());
  }

  protected void optimizeNull(String source) throws Exception {
    assertEquals("NULL", compile(source).toString());
  }

  @Test
  public void test() throws Exception {
    Locale.setDefault(new Locale("fr", "BE"));
    // ExpressionContext context = createExpressionContext();
    // context.setVariable(ExpressionContext.EXPRESSION_TWO_DIGIT_YEAR_START, "1970");
    // evalEquals("To_Date('01/02/80','DD/MM/YY')", LocalDate.of(1980, 2, 1), context);
    // context.setVariable(ExpressionContext.EXPRESSION_TWO_DIGIT_YEAR_START, "2000");

    // evalEquals("'10.10.10.1'::INET", InetAddress.getByName("10.10.10.1")).returnType(Types.INET);
    evalEquals("CAST(FIELD_INET AS String)", "10.10.10.1");

    // String jsonPath = "$[0]['gender']";
    // Variables variables = new Variables();
    // String result = variables.resolve("$[0]['name']");
    // System.out.print(result);
  }
}
