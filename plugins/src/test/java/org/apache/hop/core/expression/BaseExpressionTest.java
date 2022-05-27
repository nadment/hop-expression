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
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaNumber;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.ExpressionBuilder;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.util.Coerse;
import org.apache.hop.expression.util.Converter;
import org.apache.hop.junit.rules.RestoreHopEnvironment;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.ExternalResource;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Calendar;
import java.util.Date;
import java.util.function.Consumer;
import com.fasterxml.jackson.databind.JsonNode;

public class BaseExpressionTest {

  @ClassRule
  public static RestoreHopEnvironment env = new RestoreHopEnvironment();

  @ClassRule
  public static ExternalResource getResource() {
      return new ExternalResource() {
          @Override
          protected void before() throws Throwable {
            FunctionRegistry.init();
          }
      };
  }
  
  protected ExpressionContext createExpressionContext() throws Exception {
    IVariables variables = new Variables();
    variables.setVariable("TEST", "12345");

    IRowMeta rowMeta = new RowMeta();
    rowMeta.addValueMeta(new ValueMetaString("NAME"));
    rowMeta.addValueMeta(new ValueMetaString("SEX"));
    rowMeta.addValueMeta(new ValueMetaDate("BIRTHDATE"));
    rowMeta.addValueMeta(new ValueMetaInteger("AGE"));
    rowMeta.addValueMeta(new ValueMetaDate("DN"));
    rowMeta.addValueMeta(new ValueMetaBoolean("FLAG"));
    rowMeta.addValueMeta(new ValueMetaBoolean("VALUE_NULL"));
    rowMeta.addValueMeta(new ValueMetaInteger("YEAR"));
    rowMeta.addValueMeta(new ValueMetaString("FROM"));
    rowMeta.addValueMeta(new ValueMetaNumber("PRICE"));
    rowMeta.addValueMeta(new ValueMetaBigNumber("AMOUNT"));
    rowMeta.addValueMeta(new ValueMetaString("IDENTIFIER SPACE"));
    rowMeta.addValueMeta(new ValueMetaString("IDENTIFIER_UNDERSCORE"));
    rowMeta.addValueMeta(new ValueMetaString("IDENTIFIER lower"));

    Calendar calendar = Calendar.getInstance();
    calendar.set(1981, 5, 23);

    Object[] row = new Object[14];
    row[0] = "TEST";
    row[1] = "F";
    row[2] = calendar.getTime();
    row[3] = 40L;
    row[4] = new Date();
    row[5] = true;
    row[6] = null;
    row[7] = 2020L;
    row[8] = "Paris";
    row[9] = -5.12D;
    row[10] = BigDecimal.valueOf(123456.789);
    row[11] = "SPACE";
    row[12] = "UNDERSCORE";
    row[13] = "lower";

    ExpressionContext context = new ExpressionContext(variables, rowMeta);
    context.setRow(row);

    return context;
  }

  protected Object eval(String source) throws Exception {
    return eval(source, createExpressionContext(), null);
  }

  protected Object eval(String source, ExpressionContext context) throws Exception {
    return eval(source, context, null);
  }

  protected Object eval(String source, ExpressionContext context,
      Consumer<ExpressionContext> consumer) throws Exception {

    // Create default context
    if (context == null) {
      context = createExpressionContext();
    }
    
    // Apply context customization
    if (consumer != null)
      consumer.accept(context);
    
    IExpression expression = ExpressionBuilder.compile(context, source);

    try {
      return expression.eval(context);
    } catch (ExpressionException ex) {
      System.out.println(source + " > " + ex.getMessage());
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
    assertEquals(expected, Coerse.toNumber(value), 0.000000000000001);
  }

  protected void evalEquals(String source, BigDecimal expected) throws Exception {
    assertEquals(expected, (BigDecimal) eval(source));
  }

  protected void evalEquals(String source, LocalDate expected) throws Exception {
    assertEquals(expected.atStartOfDay().atZone(ZoneId.systemDefault()), eval(source));
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
    try {
      eval(source);
    } catch (Exception e) {
      System.out.println(source+" >>> "+e.getMessage());
    }
    assertThrows(ExpressionException.class, () -> eval(source));
  }

  protected void writeEquals(String source) throws Exception {
    writeEquals(source, source);
  }

  protected void writeEquals(String source, String result) throws Exception {
    IExpressionContext context = createExpressionContext();
    IExpression expression = ExpressionBuilder.compile(context, source);

    StringWriter writer = new StringWriter();
    expression.unparse(writer);
    assertEquals(result, writer.toString());
  }

  @Test
  public void test() throws Exception {
    // ExpressionContext context = createExpressionContext();
    // context.setAttribute("TEST","");     
    //evalFails("Year)");

  }
}
