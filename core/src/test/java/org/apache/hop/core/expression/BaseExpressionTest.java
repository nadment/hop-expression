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
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
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
    Object value = eval(s);    
    assertEquals(expected, (String) eval(s));
  }

  protected void evalEquals(String s, Long expected) throws Exception {
    Object value = eval(s);
    assertEquals(expected, (Long) value);
  }

  protected void evalEquals(String s, double expected) throws Exception {
    Object value = eval(s);
    assertEquals(expected, Operator.coerceToNumber(value), 0.000001);
  }

  protected void evalEquals(String s, BigDecimal expected) throws Exception {
    Object value = eval(s);
    assertEquals(expected, (BigDecimal) value);
  }

  protected void evalEquals(String s, Instant expected) throws Exception {
    assertEquals(expected, eval(s));
  }

  protected void evalEquals(String s, LocalTime expected) throws Exception {
    assertEquals(Instant.from(expected), eval(s));
  }

  protected void evalEquals(String s, LocalDate expected) throws Exception {
    assertEquals(expected.atStartOfDay(ZoneId.of("UTC")).toInstant(), eval(s));
  }

  protected void evalEquals(String s, LocalDateTime expected) throws Exception {
    assertEquals(expected.atZone(ZoneId.of("UTC")).toInstant(), eval(s));
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
    expression.write(writer, 0, 0);
    assertEquals(result, writer.toString());
  }


  @Test
  public void parser() throws Exception {
   
//BigDecimal v0 = BigDecimal.valueOf(0);
//BigDecimal v1 = BigDecimal.valueOf(0.1);
//BigDecimal v2 = BigDecimal.valueOf(123.11);

evalTrue("'amigo' like 'a%o'");
    //evalEquals("5/(60*24)", 5);
   // evalEquals("CAST('1234.567' as Integer)", 1235L);
    
    // evalFails("Extract(NULL from Date '2021-01-01')");
    // evalFails("TRY_CAST('2020-01-021' AS DATE FORMAT NULL)");
    // evalEquals("CAST(1.75 as Integer)",2);
    // evalEquals("[IDENTIFIER SPACE]", "SPACE");
    // evalEquals("Extract(MILLENNIUM from Timestamp '2020-05-25 23:48:59')", 3);

    // writeEquals("Extract(MILLENNIUM from Timestamp '2020-05-25 23:48:59')");
    // evalEquals("-.2", new BigDecimal("-0.2"));

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
    // evalEquals("TO_NUMBER('1234-','MI9999|9999MI')", -1234);

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
