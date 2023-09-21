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
import org.apache.hop.core.row.value.ValueMetaInternetAddress;
import org.apache.hop.core.row.value.ValueMetaJson;
import org.apache.hop.core.row.value.ValueMetaNumber;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.core.row.value.ValueMetaTimestamp;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.Expressions;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.JsonType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.UnknownType;
import org.apache.hop.junit.rules.RestoreHopEnvironment;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.ExternalResource;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.InetAddress;
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

  @ClassRule
  public static RestoreHopEnvironment env = new RestoreHopEnvironment();

  @ClassRule
  public static ExternalResource getResource() {
    return new ExternalResource() {
      @Override
      protected void before() throws Throwable {
        FunctionRegistry.registerPluginFunctions();
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
    rowMeta.addValueMeta(new ValueMetaDate("BIRTHDATE2"));
    rowMeta.addValueMeta(new ValueMetaInteger("FIELD_INTEGER"));
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
      row[9] = "TEST".getBytes();
      row[10] = InetAddress.getLocalHost();
      row[11] = JsonType.convert(
          "{\"student\": [{\"id\":\"01\",name:\"Tom\",\"lastname\": \"Price\"},{\"id\":\"02\",\"name\": \"Nick\",\"lastname\": \"Thameson\"}]}");

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
    ExpressionContext context = createExpressionContext(false);
    IExpression expression = Expressions.build(context, source);
    assertEquals(expected, expression.getType());
  }
  
  protected Object eval(String source) throws Exception {
    return eval(this.createExpressionContext(), source);
  }


  protected Object eval(IExpressionContext context, String source) throws Exception {

    // Create default context
    if (context == null) {
      context = createExpressionContext(true);
    }

    try {
      IExpression expression = Expressions.build(context, source);
      return expression.getValue();
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
      IExpression expression = Expressions.build(context, source);
      return expression.getValue(clazz);
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

  protected void evalEquals(ExpressionContext context, String source, Interval expected)
      throws Exception {
    assertEquals(expected, eval(context, source, Interval.class));
  }

  protected void evalEquals(ExpressionContext context, String source, Double expected)
      throws Exception {
    BigDecimal result = eval(context, source, BigDecimal.class);
    assertEquals(BigDecimal.valueOf(expected).stripTrailingZeros(), result.stripTrailingZeros());
  }

  protected void evalEquals(String source, BigInteger expected) throws Exception {
    assertEquals(new BigDecimal(expected),
        eval(createExpressionContext(true), source, BigDecimal.class));
  }

  protected void evalEquals(String source, Interval expected) throws Exception {
    evalEquals(createExpressionContext(true), source, expected);
  }
  
  protected void evalEquals(String source, Double expected) throws Exception {
    evalEquals(createExpressionContext(true), source, expected);
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
  
  protected void evalFails(final String source) throws Exception {
    IExpressionContext context = createExpressionContext(true);
    assertThrows(ExpressionException.class, () -> eval(context, source));
  }

  protected IExpression optimize(String source) throws Exception {

    IExpressionContext context = createExpressionContext(false);
    IExpression expression = Expressions.build(context, source);

    String color = ANSI_YELLOW;
    if (expression.getType() == UnknownType.UNKNOWN) {
      color = ANSI_RED;
    }
    System.out.println(source + ANSI_PURPLE + " cost=" + expression.getCost() + "  " + color
        + expression + ANSI_RESET);

    return expression;

  }

  protected void optimize(String source, String expected) throws Exception {
    assertEquals(expected, optimize(source).toString());
  }

  protected void optimizeTrue(String source) throws Exception {
    assertEquals("TRUE", optimize(source).toString());
  }

  protected void optimizeFalse(String source) throws Exception {
    assertEquals("FALSE", optimize(source).toString());
  }

  protected void optimizeNull(String source) throws Exception {
    assertEquals("NULL", optimize(source).toString());
  }

  @Test
  public void test() throws Exception {
    // ExpressionContext context = createExpressionContext();
    // context.setVariable(ExpressionContext.EXPRESSION_TWO_DIGIT_YEAR_START, "1970");
    // evalEquals("To_Date('01/02/80','DD/MM/YY')", LocalDate.of(1980, 2, 1), context);
    // context.setVariable(ExpressionContext.EXPRESSION_TWO_DIGIT_YEAR_START, "2000");
    Locale.setDefault(new Locale("fr", "BE"));
    //evalEquals("Json_Value('{\"name\":\"Smith\", \"age\":29}','$[''name'']')", "Smith");
    //evalNull("Json_Value(NULL_JSON,'$.name')");           
    //optimize("TO_YMINTERVAL('2-11')", "INTERVAL '2-11' YEAR TO MONTH");
    //String jsonPath = "$[0]['gender']";
    //Variables variables = new Variables();    
    //String result = variables.resolve("$[0]['name']");
    //System.out.print(result);;
  }
}

