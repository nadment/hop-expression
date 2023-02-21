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
import org.apache.hop.expression.type.Converter;
import org.apache.hop.expression.type.DataTypeName;
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
import java.util.Calendar;
import java.util.Locale;
import java.util.function.Consumer;
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
    rowMeta.addValueMeta(new ValueMetaTimestamp("NULL_TIMESAMP"));
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

    if ( withData ) {
      Calendar calendar = Calendar.getInstance();
      calendar.set(1981, 5, 23);

      Object[] row = new Object[29];
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
      
      row[18] = "True";
      row[19] = "25";
      row[20] = "-12.56";


      row[21] = 2020L;
      row[22] = "Paris";
      row[23] = true;
      row[24] = 2;
      row[25] = "SPACE";
      row[26] = "UNDERSCORE";
      row[27] = "lower";
      context.setRow(row);
    }


    return context;
  }

  
  protected void returnType(String source, DataTypeName expected) throws Exception {
    ExpressionContext context = createExpressionContext(false);
    IExpression expression = ExpressionBuilder.build(context, source);
    assertEquals(expected, expression.getType());
  }

  protected Object eval(String source) throws Exception {
    return eval(source, createExpressionContext(true), null);
  }

  protected Object eval(String source, ExpressionContext context) throws Exception {
    return eval(source, context, null);
  }

  protected Object eval(String source, ExpressionContext context,
      Consumer<ExpressionContext> consumer) throws Exception {

    // Create default context
    if (context == null) {
      context = createExpressionContext(true);
    }

    // Apply context customization
    if (consumer != null)
      consumer.accept(context);

    
    try {
      IExpression expression = ExpressionBuilder.build(context, source);

      return expression.getValue(context);
    } catch (Exception ex) {
      System.err.println(ANSI_WHITE + source + "  " + ANSI_RED + ex.getMessage() + ANSI_RESET);
      throw ex;
    }
  }

  protected void evalNull(String source) throws Exception {
    assertNull(eval(source));
  }

  protected void evalTrue(String source) throws Exception {
    assertEquals(Boolean.TRUE, eval(source));
  }

  protected void evalFalse(String source) throws Exception {
    assertEquals(Boolean.FALSE, eval(source));
  }

  protected void evalEquals(String source, byte[] expected) throws Exception {
    assertArrayEquals(expected, (byte[]) eval(source));
  }

  protected void evalEquals(String source, JsonNode expected) throws Exception {
    assertEquals(expected, (JsonNode) eval(source));
  }

  protected void evalEquals(String source, String expected) throws Exception {
    assertEquals(expected, (String) eval(source));
  }

  protected void evalEquals(String source, String expected, ExpressionContext context)
      throws Exception {
    assertEquals(expected, (String) eval(source, context, null));
  }

  protected void evalEquals(String source, String expected, Consumer<ExpressionContext> consumer)
      throws Exception {
    assertEquals(expected, (String) eval(source, null, consumer));
  }

  protected void evalEquals(String source, Long expected) throws Exception {
    assertEquals(expected, (Long) eval(source));
  }

  protected void evalEquals(String source, double expected) throws Exception {
    Object value = eval(source);
    assertEquals(expected, Converter.coerceToNumber(value), 0.000000000000001);
  }

  protected void evalEquals(String source, BigDecimal expected) throws Exception {
    assertEquals(expected, (BigDecimal) eval(source));
  }

  protected void evalEquals(String source, LocalDate expected) throws Exception {
    assertEquals(expected.atStartOfDay().atZone(ZoneId.systemDefault()), eval(source));
  }

  protected void evalEquals(String source, LocalDate expected, ExpressionContext context)
      throws Exception {
    assertEquals(expected.atStartOfDay().atZone(ZoneId.systemDefault()),
        eval(source, context, null));
  }

  protected void evalEquals(String source, LocalDateTime expected) throws Exception {
    assertEquals(expected.atZone(ZoneId.systemDefault()), eval(source));
  }

  protected void evalEquals(String source, ZonedDateTime expected) throws Exception {
    assertEquals(expected, eval(source));
  }

  protected void evalEquals(String source, ZonedDateTime expected, ExpressionContext context)
      throws Exception {
    assertEquals(expected, eval(source, context, null));
  }

  protected void evalEquals(String source, ZonedDateTime expected,
      Consumer<ExpressionContext> consumer) throws Exception {
    assertEquals(expected, eval(source, null, consumer));
  }

  protected void evalFails(final String source) {
    assertThrows(ExpressionException.class, () -> eval(source));
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
    if ( expression.getType()==DataTypeName.UNKNOWN) {
      color = ANSI_RED;
    }

    System.out.println(e + ANSI_PURPLE+" cost=" + expression.getCost() + "  "  +color + expression + ANSI_RESET);

    return expression;
  }

  protected void optimize(String e, String expected) throws Exception {
    assertEquals(expected, (String) optimize(e).toString());
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
    // evalEquals("TO_CHAR(TO_BINARY('Apache Hop','HEX'),'HEX')", "Apache Hop");
    //evalEquals("TO_BINARY('QXBhY2hlIEhvcA==','BASE64')", "Apache Hop".getBytes());
    // evalEquals("TO_CHAR(TO_BINARY('Apache Hop','BASE64'),'BASE64')", "Apache Hop");    
    //evalFails("Week(NULL_BOOLEAN)");
    //writeEquals("-FIELD_INTEGER","-FIELD_INTEGER");
    //returnType("FIELD_STRING||'t'", DataTypeName.STRING);
    evalEquals("Date_Part(HOUR,Timestamp '2020-05-25 23:48:59')", 23);
    //evalEquals("Extract(MILLENNIUM from Timestamp '2020-05-25 23:48:59')", 3);
  }
}

