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
import org.junit.Before;
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

public class BaseExpressionTest {
  @ClassRule
  public static RestoreHopEnvironment env = new RestoreHopEnvironment();

  private ExpressionContext context;

  public ExpressionContext getContext() {
    return context;
  }

  @Before
  public void setupOnce() throws Exception {

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

    context = new ExpressionContext(variables, rowMeta);
    context.setRow(row);
  }

  protected Object eval(String s) throws Exception {

    IExpression expression = ExpressionParser.parse(s);
    Optimizer optimizer = new Optimizer();
    expression = optimizer.optimize(context, expression);

    return expression.eval(context);
  }

  protected void evalNull(String s) throws Exception {
    assertNull(eval(s));
  }

  protected void evalTrue(String s) throws Exception {
    assertEquals(Boolean.TRUE, eval(s));
  }

  protected void evalFalse(String s) throws Exception {
    assertEquals(Boolean.FALSE, eval(s));
  }

  protected void evalEquals(String s, String expected) throws Exception {
    assertEquals(expected, (String) eval(s));
  }

  protected void evalEquals(String s, Long expected) throws Exception {    
    assertEquals(expected, (Long)  eval(s));
  }

  protected void evalEquals(String s, double expected) throws Exception {
    Object value = eval(s);
    assertEquals(expected, Operator.coerceToNumber(value), 0.000001);
  }

  protected void evalEquals(String s, BigDecimal expected) throws Exception {
    assertEquals(expected, (BigDecimal) eval(s));
  }

  protected void evalEquals(String s, LocalDate expected) throws Exception {
    assertEquals(expected.atStartOfDay().atOffset(ZoneOffset.ofHours(0)).toZonedDateTime(),eval(s));
  }

  protected void evalEquals(String s, LocalDateTime expected) throws Exception {    
    
    assertEquals(expected.atOffset(ZoneOffset.ofHours(0)).toZonedDateTime(), eval(s));
  }
  
  protected void evalEquals(String s, OffsetDateTime expected) throws Exception {
    assertEquals(expected.toZonedDateTime(), eval(s));
  }
  
  protected void evalEquals(String s, ZonedDateTime expected) throws Exception {
    assertEquals(expected, eval(s));
  }
  
  protected void evalFails(final String s) {
    // Assert.assertThrows(ExpressionException.class, () -> {
    // eval(s);
    // });

    try {
      eval(s);
      Assert.fail(s + " Syntax or result should be invalid\n");
    } catch (ExpressionException ex) {
      // Assert.assertT.assertThrows(s+" Syntax or result should be invalid: "+ex.getMessage(), ex);
      // System.out.println(s+" > "+ex.toString());
    } catch (Exception ex) {
      Assert.fail(s + " Uncatched exception " + ex.getClass());
    }
  }

  protected void writeEquals(String original) throws Exception {
    writeEquals(original, original);
  }

  protected void writeEquals(String original, String result) throws Exception {

    IExpression expression = ExpressionParser.parse(original);

    StringWriter writer = new StringWriter();
    expression.write(writer);
    assertEquals(result, writer.toString());
  }


  @Test
  public void parser() throws Exception {
    evalEquals("To_Char(Date '2019-07-23','AD')", "AD");
    // BigDecimal v0 = BigDecimal.valueOf(0);
    // BigDecimal v1 = BigDecimal.valueOf(0.1);
    // BigDecimal v2 = BigDecimal.valueOf(123.11);
    // evalEquals("Abs(10)", 10);
    // evalEquals("Add(10,-0.5)", 9.5);
    // Time zone offset

    // evalEquals("To_Date('2019-02-13T15:34:56 +8:00','YYYY-MM-DD\"T\"HH24:MI:SS TZH:TZM')",
    // LocalDateTime.of(2019, Month.FEBRUARY, 13, 7, 34, 56));


  }


}
