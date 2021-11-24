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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
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
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionParser;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.optimizer.Optimizer;
import org.apache.hop.junit.rules.RestoreHopEnvironment;
import org.junit.Assert;
import org.junit.ClassRule;
import org.junit.Test;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Date;
import java.util.function.Consumer;
import javax.script.ScriptContext;

public class BaseExpressionTest {
    
  @ClassRule
  public static RestoreHopEnvironment env = new RestoreHopEnvironment();

  protected ExpressionContext createExpressionContext() throws Exception {

    IVariables variables = new Variables();
    variables.setVariable("TEST", "12345");

    IRowMeta rowMeta = new RowMeta();
    rowMeta.addValueMeta(new ValueMetaString("NAME"));
    rowMeta.addValueMeta(new ValueMetaString("SEX"));
    rowMeta.addValueMeta(new ValueMetaInteger("AGE"));
    rowMeta.addValueMeta(new ValueMetaDate("DN"));
    rowMeta.addValueMeta(new ValueMetaBoolean("FLAG"));
    rowMeta.addValueMeta(new ValueMetaBoolean("NULLIS"));
    rowMeta.addValueMeta(new ValueMetaInteger("YEAR"));
    rowMeta.addValueMeta(new ValueMetaString("FROM"));
    rowMeta.addValueMeta(new ValueMetaNumber("PRICE"));
    rowMeta.addValueMeta(new ValueMetaBigNumber("AMOUNT"));
    rowMeta.addValueMeta(new ValueMetaString("IDENTIFIER SPACE"));
    rowMeta.addValueMeta(new ValueMetaString("IDENTIFIER_UNDERSCORE"));
    rowMeta.addValueMeta(new ValueMetaString("IDENTIFIER lower"));

    Object[] row = new Object[13];
    row[0] = "TEST";
    row[1] = "F";
    row[2] = 40L;
    row[3] = new Date();
    row[4] = true;
    row[5] = null;
    row[6] = 2020L;
    row[7] = "Paris";
    row[8] = -5.12D;
    row[9] = BigDecimal.valueOf(123456.789);
    row[10] = "SPACE";
    row[11] = "UNDERSCORE";
    row[12] = "lower";

    ExpressionContext context = new ExpressionContext(variables, rowMeta);
    context.setRow(row);
   
    return context;
  }
  protected Object eval(String source)  throws Exception {
    return eval(source, createExpressionContext(), null);
  }

  protected Object eval(String source, ExpressionContext context)  throws Exception {
    return eval(source, context, null);
  }
  
  protected Object eval(String source, ExpressionContext context, Consumer<ExpressionContext> consumer) throws Exception {

    IExpression expression = ExpressionParser.parse(source);
    Optimizer optimizer = new Optimizer();

    // Create default context
    if ( context==null) context = createExpressionContext();
    
    // Optimize in context
    expression = optimizer.optimize(context, expression);

    // Apply context customization
    if ( consumer!=null) consumer.accept(context);
    
    return expression.eval(context);
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

  protected void evalEquals(String source, String expected) throws Exception {
    assertEquals(expected, (String) eval(source));
  }

  protected void evalEquals(String source, String expected, ExpressionContext context) throws Exception {
    assertEquals(expected, (String) eval(source, context, null));
  }
  
  protected void evalEquals(String source, String expected, Consumer<ExpressionContext> consumer) throws Exception {
    assertEquals(expected, (String) eval(source, null, consumer));
  }
  
  protected void evalEquals(String source, Long expected) throws Exception {    
    assertEquals(expected, (Long)  eval(source));
  }

  protected void evalEquals(String source, double expected) throws Exception {
    Object value = eval(source);
    assertEquals(expected, Operator.coerceToNumber(value), 0.000001);
  }

  protected void evalEquals(String source, BigDecimal expected) throws Exception {
    assertEquals(expected, (BigDecimal) eval(source));
  }

  protected void evalEquals(String source, LocalDate expected) throws Exception {
    assertEquals(expected.atStartOfDay().atOffset(ZoneOffset.ofHours(0)).toZonedDateTime(),eval(source));
  }

  protected void evalEquals(String source, LocalDateTime expected) throws Exception {    
    assertEquals(expected.atOffset(ZoneOffset.ofHours(0)).toZonedDateTime(), eval(source));
  }
  
  protected void evalEquals(String source, OffsetDateTime expected) throws Exception {
    assertEquals(expected.toZonedDateTime(), eval(source));
  }
  
  protected void evalEquals(String source, ZonedDateTime expected) throws Exception {
    assertEquals(expected, eval(source));
  }
  protected void evalEquals(String source, ZonedDateTime expected, ExpressionContext context) throws Exception {
    assertEquals(expected, eval(source, context, null));
  }

  protected void evalEquals(String source, ZonedDateTime expected, Consumer<ExpressionContext> consumer) throws Exception {
    assertEquals(expected, eval(source, null, consumer));
  }

  
  protected void evalFails(final String source) {
    try {
      eval(source);
      Assert.fail(source + " Syntax or result should be invalid\n");
    } catch (ExpressionException ex) {
      // Assert.assertT.assertThrows(s+" Syntax or result should be invalid: "+ex.getMessage(), ex);
      // System.out.println(s+" > "+ex.toString());
    } catch (Exception ex) {
      Assert.fail(source + " Uncatched exception " + ex.getClass());
    }
  }

  protected void writeEquals(String source) throws Exception {
    writeEquals(source, source);
  }

  protected void writeEquals(String source, String result) throws Exception {
    IExpression expression = ExpressionParser.parse(source);

    StringWriter writer = new StringWriter();
    expression.write(writer);
    assertEquals(result, writer.toString());
  }

  @Test
  public void parser() throws Exception {
   ExpressionContext context = createExpressionContext();
   context.setAttribute("TEST","", ScriptContext.ENGINE_SCOPE);
   evalEquals("To_Char(Date '2019-07-23','AD')", "AD", context);
  }
}
