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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionParser;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.expression.Value;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import java.math.BigDecimal;
import java.text.ParseException;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.Date;
import java.util.Locale;

public class BaseExpressionTest {

  private RowExpressionContext context;

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

    Object[] row = new Object[8];
    row[0] = "TEST";
    row[1] = "F";
    row[2] = 40L;
    row[3] = new Date();
    row[4] = true;
    row[5] = null;
    row[6] = 2020L;
    row[7] = "Paris";

    context = new RowExpressionContext(rowMeta);
    context.setRow(row);
    context.setLocale(new Locale("fr", "BE"));
  }

  protected void setLocale(Locale locale) {
    context.setLocale(locale);
  }
  
  protected Value eval(String s) throws Exception {
    IExpression expression = ExpressionParser.parse(s);
    return expression.eval(context);
  }

  protected void evalNull(String s) throws Exception {
    assertTrue(eval(s).isNull());
  }

  protected void evalTrue(String s) throws Exception {
    assertTrue(eval(s).toBoolean());
  }

  protected void evalFalse(String s) throws Exception {
    assertFalse(eval(s).toBoolean());
  }

  protected void evalEquals(String s, String expected) throws Exception {
    assertEquals(expected, eval(s).toString());
  }

  protected void evalEquals(String s, Long expected) throws Exception {
    Value value = eval(s);
    if (value.isNull())
      Assert.fail("Return null value");
    assertEquals(expected.longValue(), value.toInteger());
  }

  protected void evalEquals(String s, double expected) throws Exception {
    Value value = eval(s);
    if (value.isNull())
      Assert.fail("Return null value");
    assertEquals(expected, value.toNumber(), 0.000001);
  }

  protected void evalEquals(String s, BigDecimal expected) throws Exception {
    Value value = eval(s);
    if (value.isNull())
      Assert.fail("Return null value");
    assertEquals(expected, value.toBigNumber());
  }

  protected void evalEquals(String s, Instant expected) throws Exception {
    assertEquals(expected, eval(s).toDate());
  }

  protected void evalEquals(String s, LocalTime expected) throws Exception {
    assertEquals(Instant.from(expected), eval(s).toDate());
  }

  protected void evalEquals(String s, LocalDate expected) throws Exception {
    assertEquals(expected.atStartOfDay(ZoneId.of("UTC")).toInstant(), eval(s).toDate());
  }

  protected void evalEquals(String s, LocalDateTime expected) throws Exception {
    assertEquals(expected.atZone(ZoneId.of("UTC")).toInstant(), eval(s).toDate());
  }

  protected void evalFails(String s) {
    try {
      System.out.print(s);
      eval(s);
      Assert.fail("Syntax or result should be invalid");
      System.out.print('\n');
      System.out.flush();
    } catch (ParseException | ExpressionException | IllegalArgumentException ex) {
      System.err.println(' '+ex.getMessage());
      System.err.flush();
    } catch (Exception ex) {
      Assert.fail("Uncatched exception " + ex.getClass());
    }
  }

  @Test
  public void parser() throws Exception {

    evalEquals("CAST(1.75 as Integer)",2);
    
    //evalEquals("-.2", new BigDecimal("-0.2"));
    
    // evalEquals("TO_NUMBER('0.3-','#0.##-')", -0.3);
    // evalEquals("TO_NUMBER('65.169', '#########.0000')", 65.169);
    // evalEquals("TO_CHAR(-65.169, '$-###,000.000')", "$-065.169");
    //
    // setLocale(new Locale("fr", "BE"));
    // evalEquals("TO_NUMBER('5467,12', '999999D99')", 5467.12);
    // evalEquals("TO_CHAR(0.1,'99.99')", " 0.1 ");
    // evalEquals("TO_CHAR(0.1,'99.99')", " 0.1 ");
    // evalEquals("TO_CHAR(0.3,'FM00.99')", "00.3");
    // evalEquals("TO_CHAR(0.003,'0.999')", " 0.003");
    // evalEquals("TO_CHAR(12923,'FM99999.99')", "12923.");
    // evalEquals("TO_CHAR(0.1,'99.99')", " 0.1 ");
    //evalEquals("TO_NUMBER('1234-','MI9999|9999MI')", -1234);
    
    // evalEquals("TO_CHAR(0,'90.99')", " 0. ");
    // evalEquals("TO_CHAR(-0.2,'99.90')", " -.20");
    // evalEquals("TO_CHAR(555.0, 'FM999.909')","555.00");


    // setLocale(new Locale("en", "US"));
    // evalEquals("TO_NUMBER('$65.169', 'L99.999')", 65.169);

    // evalEquals("TO_NUMBER('5467.12', '999999.99')", 5467.12);
    // evalEquals("TO_NUMBER('12,345,678', '999G999G999')", 12_345_678);
    // evalEquals("TO_NUMBER('1234.5','09999.99')", 1234.5);
    // evalEquals("TO_NUMBER('12,345,678', '999G999G999')", 12_345_678);
  }


}