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

import static org.junit.jupiter.api.AssertionFailureBuilder.assertionFailure;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.fasterxml.jackson.databind.JsonNode;
import com.google.common.base.Charsets;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.InetAddress;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.temporal.Temporal;
import java.util.Calendar;
import java.util.Locale;
import java.util.TimeZone;
import org.apache.hop.core.HopClientEnvironment;
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
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;
import org.apache.hop.expression.util.JsonConversion;
import org.apache.hop.junit.rules.RestoreHopEnvironment;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.platform.commons.util.UnrecoverableExceptions;

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

  public static RestoreHopEnvironment env;

  @BeforeAll
  static void setup() throws Throwable {
    // Can't use RestoreHopEnvironment because it is for Junit 4
    // RestoreHopEnvironment env = new RestoreHopEnvironment();
    Locale.setDefault(Locale.ENGLISH);
    HopClientEnvironment.init();
    FunctionRegistry.registerFunctions();
  }

  @AfterAll
  static void clean() {
    FunctionRegistry.unregisterFunctions();
    HopClientEnvironment.reset();
  }

  public static class Evaluator {
    private final IExpression expression;
    private final String source;

    public Evaluator(IExpressionContext context, String source) {

      try {
        this.expression = ExpressionFactory.create(context, source);
      } catch (Exception ex) {
        System.err.println(ANSI_WHITE + source + "  " + ANSI_RED + ex.getMessage() + ANSI_RESET);
        throw ex;
      }
      this.source = source;
    }

    public Object eval() throws Exception {
      try {
        return expression.getValue();
      } catch (Exception ex) {
        System.err.println(ANSI_WHITE + source + "  " + ANSI_RED + ex.getMessage() + ANSI_RESET);
        throw ex;
      }
    }

    public <T> T eval(Class<T> clazz) throws Exception {
      try {
        return expression.getValue(clazz);
      } catch (Exception ex) {
        System.err.println(ANSI_WHITE + source + "  " + ANSI_RED + ex.getMessage() + ANSI_RESET);
        throw ex;
      }
    }

    public IExpression getExpression() {
      return expression;
    }

    public void returnType(Type expectedType) {
      assertEquals(expectedType, expression.getType());
    }
  }

  protected IRowExpressionContext createExpressionContext() throws Exception {
    return this.createExpressionContext(new Variables());
  }

  protected IRowExpressionContext createExpressionContext(IVariables variables) throws Exception {

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

    Calendar calendar = Calendar.getInstance();
    calendar.set(1981, Calendar.JUNE, 23);
    calendar.setTimeZone(TimeZone.getTimeZone(ZoneOffset.UTC));

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
        JsonConversion.convert(
            "{ \"store\":{ \"book\": [{ \"category\": \"reference\", \"author\": \"Nigel Rees\", \"title\": \"Sayings of the Century\", \"price\": 8.95 }, { \"category\": \"fiction\", \"author\": \"Evelyn Waugh\", \"title\": \"Sword of Honour\", \"price\": 12.99 }, {\"category\": \"fiction\", \"author\": \"Herman Melville\", \"title\": \"Moby Dick\", \"isbn\": \"0-553-21311-3\", \"price\": 8.99 }, {\"category\": \"fiction\", \"author\": \"J. R. R. Tolkien\", \"title\": \"The Lord of the Rings\", \"isbn\": \"0-395-19395-8\",\"price\": 22.99 }],  \"bicycle\": { \"color\": \"red\", \"price\": 19.95 } } }");

    // Null values
    row[12] = null;
    row[13] = null;
    row[14] = null;
    row[15] = null;
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

    return context;
  }

  protected void returnType(String source, Type expected) throws Exception {
    IExpression expression = ExpressionFactory.create(createExpressionContext(), source);
    // assertEquals(expected.withNullability(true), expression.getType().withNullability(true));
    assertEquals(expected, expression.getType());
  }

  protected Evaluator evalNull(String source) throws Exception {
    Evaluator evaluator = new Evaluator(createExpressionContext(), source);
    assertNull(evaluator.eval());
    return evaluator;
  }

  protected Evaluator evalTrue(String source) throws Exception {
    Evaluator evaluator = new Evaluator(createExpressionContext(), source);
    assertEquals(Boolean.TRUE, evaluator.eval());
    return evaluator;
  }

  protected Evaluator evalFalse(String source) throws Exception {
    Evaluator evaluator = new Evaluator(createExpressionContext(), source);
    assertEquals(Boolean.FALSE, evaluator.eval());
    return evaluator;
  }

  protected Evaluator evalEquals(String source, byte[] expected) throws Exception {
    Evaluator evaluator = new Evaluator(createExpressionContext(), source);
    assertArrayEquals(expected, evaluator.eval(byte[].class));
    return evaluator;
  }

  protected Evaluator evalEquals(IExpressionContext context, String source, String expected)
      throws Exception {
    Evaluator evaluator = new Evaluator(context, source);
    assertEquals(expected, evaluator.eval());
    return evaluator;
  }

  protected Evaluator evalEquals(IExpressionContext context, String source, Long expected)
      throws Exception {
    Evaluator evaluator = new Evaluator(context, source);
    Object result = evaluator.eval();
    assertEquals(expected, result);
    return evaluator;
  }

  protected Evaluator evalEquals(IExpressionContext context, String source, Interval expected)
      throws Exception {
    Evaluator evaluator = new Evaluator(context, source);
    assertEquals(expected, evaluator.eval());
    return evaluator;
  }

  protected Evaluator evalEquals(IExpressionContext context, String source, InetAddress expected)
      throws Exception {
    Evaluator evaluator = new Evaluator(context, source);
    assertEquals(expected, evaluator.eval());
    return evaluator;
  }

  protected Evaluator evalEquals(IExpressionContext context, String source, JsonNode expected)
      throws Exception {
    Evaluator evaluator = new Evaluator(context, source);
    assertEquals(expected, evaluator.eval());
    return evaluator;
  }

  protected Evaluator evalEquals(IExpressionContext context, String source, Double expected)
      throws Exception {
    Evaluator evaluator = new Evaluator(context, source);
    BigDecimal result = evaluator.eval(BigDecimal.class);
    assertEquals(BigDecimal.valueOf(expected).stripTrailingZeros(), result.stripTrailingZeros());
    return evaluator;
  }

  protected Evaluator evalEquals(IExpressionContext context, String source, Temporal expected)
      throws Exception {

    Evaluator evaluator = new Evaluator(context, source);
    Object result = evaluator.eval(Object.class);

    if (result instanceof LocalDateTime value) {
      if (expected instanceof LocalDate) {
        result = value.toLocalDate();
      } else if (expected instanceof ZonedDateTime) {
        result = value.atZone(ZoneOffset.UTC);
      }
    }

    if (result instanceof ZonedDateTime value) {
      if (expected instanceof LocalDate) {
        result = value.toLocalDate();
      } else if (expected instanceof LocalDateTime) {
        result = value.toLocalDateTime();
      } else if (expected instanceof OffsetDateTime) {
        result = value.toOffsetDateTime();
      }
    }

    assertEquals(expected, result);
    return evaluator;
  }

  protected Evaluator evalEquals(String source, JsonNode expected) throws Exception {
    return evalEquals(createExpressionContext(), source, expected);
  }

  protected Evaluator evalEquals(String source, Interval expected) throws Exception {
    return evalEquals(createExpressionContext(), source, expected);
  }

  protected Evaluator evalEquals(String source, String expected) throws Exception {
    return evalEquals(createExpressionContext(), source, expected);
  }

  protected Evaluator evalEquals(String source, Long expected) throws Exception {
    return evalEquals(createExpressionContext(), source, expected);
  }

  protected Evaluator evalEquals(String source, BigInteger expected) throws Exception {
    Evaluator evaluator = new Evaluator(createExpressionContext(), source);
    assertEquals(new BigDecimal(expected), evaluator.eval(BigDecimal.class));
    return evaluator;
  }

  protected Evaluator evalEquals(String source, Double expected) throws Exception {
    return evalEquals(createExpressionContext(), source, expected);
  }

  protected Evaluator evalEquals(String source, BigDecimal expected) throws Exception {
    Evaluator evaluator = new Evaluator(createExpressionContext(), source);
    BigDecimal result = evaluator.eval(BigDecimal.class);
    assertEquals(expected.stripTrailingZeros(), result.stripTrailingZeros());
    return evaluator;
  }

  protected Evaluator evalEquals(String source, Temporal expected) throws Exception {
    return evalEquals(createExpressionContext(), source, expected);
  }

  protected Evaluator evalEquals(String source, InetAddress expected) throws Exception {
    return evalEquals(createExpressionContext(), source, expected);
  }

  /** Check if expression fails with the supplied {@code ErrorCode}. */
  protected void evalFails(final String source, ErrorCode error) {
    try {
      Evaluator evaluator = new Evaluator(createExpressionContext(), source);
      evaluator.eval(Object.class);
    } catch (Throwable exception) {
      if (exception instanceof ExpressionException ee) {
        if (error != ee.getErrorCode()) {
          throw assertionFailure()
              .reason(
                  "Expected %s to be thrown, but was %s."
                      .formatted(error.name(), ee.getErrorCode().name()))
              .build();
        }

        return;
      } else {
        UnrecoverableExceptions.rethrowIfUnrecoverable(exception);
        throw assertionFailure()
            .expected(ExpressionException.class)
            .actual(exception.getClass())
            .reason("Unexpected exception type thrown")
            .cause(exception)
            .build();
      }
    }
    throw assertionFailure()
        .reason("Expected ExpressionException to be thrown, but nothing was thrown.")
        .build();
  }

  protected IExpression compile(String source) throws Exception {

    IExpression expression = ExpressionFactory.create(createExpressionContext(), source);

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
    // Locale.setDefault(new Locale("fr", "BE"));
    // evalEquals("Json_Value('{\"name\":\"Smith\", \"createTime\":\"2024-06-14
    // 13:07:21\"}','$.createTime' RETURNING DATE)", LocalDateTime.of(2024,6,14,13,7,21));
    // evalEquals("Json_Value('[{a:100}, {b:200}, {c:300}]', '$[*].c')", "300");
    // evalFails("BOOLEAN ", ErrorCode.SYNTAX_ERROR);

  }
}
