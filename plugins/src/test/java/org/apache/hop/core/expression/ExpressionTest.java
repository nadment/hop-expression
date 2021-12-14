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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.hop.expression.DataType;
import org.apache.hop.expression.DatePart;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorRegistry;
import org.junit.Test;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;

public class ExpressionTest extends BaseExpressionTest {
    
  @Test
  public void Comment() throws Exception {
    evalTrue(" // Test line comment \n  true ");
    evalTrue(" /* Test block comment */  true ");
    evalTrue(" true /* Test block comment */");
    evalTrue("/*\n * Comment on multi line\n *\n */ True");
    evalTrue("/*\n * Comment on multi line \n  with nesting: /* nested block comment */ *\n */   True");

    // Single line comment
    evalTrue("// Single line comment\nTrue");
    evalTrue("-- Single line comment\nTrue");
    
    // Multi line comment
    evalTrue("/* Line 1\n * Line 2 */ True");

    // Empty
    evalFails("-- Single line comment\n");    
    evalFails(" /");
    evalFails("/*   True");
    evalFails("/   True");
    evalFails("/*   True*");
    evalFails("/* /* nested block comment */    True");
  }
  
  @Test
  public void DataType() throws Exception {
    assertEquals(DataType.NONE, DataType.fromJava(null) );
    assertEquals(DataType.BOOLEAN, DataType.fromJava(true));
    assertEquals(DataType.STRING, DataType.fromJava("") );
    assertEquals(DataType.INTEGER, DataType.fromJava(1L));
    assertEquals(DataType.NUMBER, DataType.fromJava(1D));
    assertEquals(DataType.BIGNUMBER, DataType.fromJava(BigDecimal.ONE) );
    assertEquals(DataType.BINARY, DataType.fromJava(new byte[] {0x78}));
    assertEquals(DataType.DATE,
        DataType.fromJava(ZonedDateTime.of(LocalDateTime.now(), ZoneId.of("Asia/Ho_Chi_Minh")))
        );
    
    assertThrows(IllegalArgumentException.class, () -> DataType.fromJava(Float.class) );

    assertEquals(Boolean.class, DataType.BOOLEAN.javaClass() );
    assertEquals(Long.class, DataType.INTEGER.javaClass() );
    assertEquals(Double.class, DataType.NUMBER.javaClass() );
    assertEquals(BigDecimal.class, DataType.BIGNUMBER.javaClass() );
    assertEquals(String.class, DataType.STRING.javaClass() );
    assertEquals(ZonedDateTime.class, DataType.DATE.javaClass() );
  }

  @Test
  public void DatePart() throws Exception {
    assertFalse(DatePart.of("HOUR").equals(null));
    assertTrue(DatePart.exist("MONTH"));
    assertEquals(DatePart.QUARTER, DatePart.of("quarter"));
    assertEquals(DatePart.DAY, DatePart.of("d"));
    assertEquals(DatePart.DAY, DatePart.of("dd"));
    assertEquals(DatePart.DAY, DatePart.of("dayofmonth"));
    assertEquals(DatePart.HOUR, DatePart.of("HOUR"));
    assertEquals(DatePart.HOUR, DatePart.of("HH"));

  }
  
  @Test
  public void CoerceToBoolean() throws Exception {
    assertNull(Operator.coerceToBoolean(null));
    assertTrue(Operator.coerceToBoolean(true));
    assertTrue(Operator.coerceToBoolean(3L));
    assertTrue(Operator.coerceToBoolean(1L));
    assertFalse(Operator.coerceToBoolean(0L));
    assertFalse(Operator.coerceToBoolean(false));
    assertThrows(ExpressionException.class, () -> Operator.coerceToBoolean(ZonedDateTime.now()));
  }
   
  @Test
  public void ConverToBoolean() throws Exception {
    assertNull(Operator.convertTo(null, DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Operator.convertTo(3L, DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Operator.convertTo(1L, DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Operator.convertTo("1", DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Operator.convertTo("T", DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Operator.convertTo("True", DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Operator.convertTo("Y", DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Operator.convertTo("Yes", DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Operator.convertTo("ON", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Operator.convertTo(0L, DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Operator.convertTo("0", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Operator.convertTo("F", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Operator.convertTo("False", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Operator.convertTo("N", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Operator.convertTo("No", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Operator.convertTo("Off", DataType.BOOLEAN));
    assertThrows(ExpressionException.class, () -> Operator.convertTo("3", DataType.BOOLEAN) );
    assertThrows(ExpressionException.class, () -> Operator.convertTo("MO", DataType.BOOLEAN) );
    assertThrows(ExpressionException.class, () -> Operator.convertTo("BAD", DataType.BOOLEAN) );
    assertThrows(ExpressionException.class, () -> Operator.convertTo("TRUL", DataType.BOOLEAN) );    
    assertThrows(ExpressionException.class, () -> Operator.convertTo("FILSE", DataType.BOOLEAN) );
  }
  
  @Test
  public void CoerceToBinary() throws Exception {
    assertNull(Operator.coerceToBinary(null));    
    assertThrows(ExpressionException.class, () -> Operator.coerceToBinary(true) );
    assertThrows(ExpressionException.class, () -> Operator.coerceToBinary(3L) );
    assertThrows(ExpressionException.class, () -> Operator.coerceToBinary(3D) );
  }
  
  @Test
  public void ConverToBinary() throws Exception {
    assertNull(Operator.convertTo(null, DataType.BINARY));
  }
  
  @Test
  public void CoerceToDate() throws Exception {
    assertNull(Operator.coerceToDate(null));
    assertThrows(ExpressionException.class, () -> Operator.coerceToDate(true));
  }
  
  @Test
  public void CoerceToString() throws Exception {
    assertNull(Operator.coerceToString(null));
    assertEquals("-1.0", Operator.coerceToString(-1.0D));
    assertEquals("ABCD", Operator.coerceToString(Operator.coerceToBinary("ABCD")));
    assertEquals("-1.2", Operator.coerceToString(-1.2));    
    assertEquals("0.1", Operator.coerceToString(0.1D));
    assertEquals("-0.1", Operator.coerceToString(-0.1D));
  }
   
  @Test
  public void ConverToString() throws Exception {
    assertNull(Operator.convertTo(null, DataType.STRING));
    assertEquals("TRUE", Operator.convertTo(true, DataType.STRING));
    assertEquals("FALSE", Operator.convertTo(false, DataType.STRING));
  }
  
  @Test
  public void CoerceToInteger() throws Exception {
    assertNull(Operator.coerceToInteger(null));
    assertEquals(Long.valueOf(1), Operator.coerceToInteger(1));
    assertEquals(Long.valueOf(1), Operator.coerceToInteger(1L));
    assertEquals(Long.valueOf(1), Operator.coerceToInteger(1.2D));
    assertEquals(Long.valueOf(1), Operator.coerceToInteger("1.2"));
    assertEquals(Long.valueOf(-2), Operator.coerceToInteger("-1.6"));
    assertThrows(ExpressionException.class, () -> Operator.coerceToInteger(ZonedDateTime.now()));
  }

  @Test
  public void ConverToInteger() throws Exception {
    assertNull(Operator.convertTo(null, DataType.INTEGER));
    assertEquals(1L, Operator.convertTo(true, DataType.INTEGER));
    assertEquals(0L, Operator.convertTo(false, DataType.INTEGER));
    assertEquals(0L, Operator.convertTo(0L, DataType.INTEGER));
    assertEquals(3L, Operator.convertTo(3L, DataType.INTEGER));
    assertEquals(0L, Operator.convertTo(0.0D, DataType.INTEGER));
    assertEquals(3L, Operator.convertTo(3.3D, DataType.INTEGER));
  }
  
  @Test
  public void CoerceToNumber() throws Exception {
    assertNull(Operator.coerceToNumber(null));
    assertEquals(Double.valueOf(1D), Operator.coerceToNumber("1"));
    assertEquals(Double.valueOf(1.2D), Operator.coerceToNumber("1.2"));
    assertEquals(Double.valueOf(0.1D), Operator.coerceToNumber(".1"));
    assertEquals(Double.valueOf(-2.3E+2D), Operator.coerceToNumber("-2.3E+2"));
    assertEquals(Double.valueOf(-2.3E-2D), Operator.coerceToNumber("-2.3E-2"));
    assertEquals(Double.valueOf(-2.3E-2D), Operator.coerceToNumber("-2.3e-2"));
    assertThrows(ExpressionException.class, () -> Operator.coerceToNumber(ZonedDateTime.now()));
  }
  
  @Test
  public void ConverToNumber() throws Exception {
    assertNull(Operator.convertTo(null, DataType.NUMBER));
    assertEquals(Double.valueOf(1D), Operator.convertTo(true, DataType.NUMBER));
    assertEquals(Double.valueOf(0D), Operator.convertTo(false, DataType.NUMBER));
    assertEquals(Double.valueOf(0D), Operator.convertTo(0L, DataType.NUMBER));
    assertEquals(Double.valueOf(3D), Operator.convertTo(3L, DataType.NUMBER));
    assertEquals(Double.valueOf(-2.3E+2D), Operator.convertTo(-2.3E+2D, DataType.NUMBER));    
  }

  @Test
  public void CoerceToBigNumber() throws Exception {
    assertNull(Operator.coerceToBigNumber(null));
    assertEquals(BigDecimal.ZERO, Operator.coerceToBigNumber(0D));
    assertEquals(BigDecimal.ZERO, Operator.coerceToBigNumber(BigDecimal.ZERO));
    assertEquals(BigDecimal.ONE, Operator.coerceToBigNumber(1L));
    assertEquals(BigDecimal.ONE, Operator.coerceToBigNumber(1D));
    assertEquals(BigDecimal.ONE, Operator.coerceToBigNumber(BigDecimal.ONE) );
    assertSame(BigDecimal.valueOf(3), Operator.coerceToBigNumber(3L));
    assertThrows(ExpressionException.class, () -> Operator.coerceToBigNumber(ZonedDateTime.now()));
  }
  
  @Test
  public void ConverToBigNumber() throws Exception {
    assertNull(Operator.convertTo(null, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ONE, Operator.convertTo(true, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, Operator.convertTo(false, DataType.BIGNUMBER));
  }
  
  @Test
  public void Operator() throws Exception {
    assertEquals("Mathematical",OperatorRegistry.MULTIPLY.getCategory());
    assertTrue(OperatorRegistry.CONCAT.equals(OperatorRegistry.CONCAT));
    assertTrue(OperatorRegistry.CONCAT.isSame(OperatorRegistry.getFunction("CONCAT")));
    assertFalse(OperatorRegistry.CONCAT.isSame(null));
    assertNotNull(OperatorRegistry.CONCAT.getDescription());
    assertNotNull(OperatorRegistry.CONCAT.getDocumentationUrl());
    assertTrue(OperatorRegistry.getFunction("TRUNCATE").isSame(OperatorRegistry.getFunction("TRUNC")));
  }
  
  @Test
  public void precedenceAndAssociativity() throws Exception {

    assertEquals(51, OperatorRegistry.MULTIPLY.getLeftPrecedence());
    assertEquals(50, OperatorRegistry.MULTIPLY.getRightPrecedence());
    
    // Arithmetic
    evalEquals("3*5/2", ((3 * 5) / 2d));
    evalEquals("9/3*3", (9 / 3) * 3);
    evalEquals("1 + 2 * 3 * 4 + 5", ((1 + ((2 * 3) * 4)) + 5));
    evalEquals("1-2+3*4/5/6-7", 1 - 2 + 3 * 4d / 5d / 6d - 7);
    evalEquals("10*2+1", 21);
    evalEquals("1+10*2", 21);
    evalEquals("10*(2+1)", 30);
    evalEquals("30/(5+5)", 3);
    evalEquals("42%(3+2)", 2);
    evalEquals("1-2+3*4/5/6-7", (((1d - 2d) + (((3d * 4d) / 5d) / 6d)) - 7d));
    evalEquals("Age-(10+3*10+50-2*25)", 0);
    evalEquals("2*'1.23'", 2.46);

    // NOT has higher precedence than AND, which has higher precedence than OR
    evalTrue("NOT false AND NOT false");
    evalTrue("NOT 5 = 5 OR NOT 'Test' = 'X' AND NOT 5 = 4");

    // Equals (=) has higher precedence than NOT "NOT (1=1)"
    evalTrue("NOT 2 = 1");

    // IS NULL has higher precedence than NOT
    evalFalse("NOT \"NULLIS\" IS NULL");

    // IS NULL has lower precedence than comparison (1 = 1) IS NULL
    evalFalse("1 = 1 is null");
    evalTrue(" 3 > 5 IS FALSE");

    // BETWEEN, IN, LIKE have higher precedence than comparison
    // evalTrue("5 between 4=4 and 6=6");
  }
  
  @Test
  public void SyntaxError() throws Exception {
    evalFails("'T'||'T");
    evalFails("\"T\"||\"T");
    evalFails("9!7");
    evalFails("9+(");
    evalFails("9+*(");
    evalFails("Year(");
    evalFails("Year)");
    evalFails("Year()");
    evalFails("Year(()");
    evalFails("Year())");
    evalFails("Year(1,2)");
    evalFails("TRUE AND");
    evalFails("5 BETWEEN 4 AND");
    evalFails("5 BETWEEN 4 OR");
    evalFails("case when 1=1 then 1 else 0");
    evalFails("case when 1=1 then 1 else  end ");
    evalFails("case 1 when 1  else 0 end");
    evalFails("Cast(3 as NILL)");
    evalFails("Cast(3 as )");
    evalFails("Cast(3 as");
    evalFails("1 in ()    ");
    evalFails("1 in (,2,3)");
    evalFails("1 in (1,2,3");
    evalFails("1 in (1,,3)");
    evalFails("1 in (1,2,)");
    evalFails("0xABCDEFg");
    evalFails("Date '2020-20-28'");
  }
  
  @Test
  public void CoercionImplicit() throws Exception {
    // Coercion Number to Boolean
    evalTrue("true = 1");
    evalTrue("false = 0");
  }
  
  @Test
  public void CoercionExplicit() throws Exception {
    // Coercion String to Boolean
    evalTrue("'1'::Boolean=true");
    evalTrue("'On'::Boolean=true");
    evalTrue("'Y'::Boolean=true");
    evalTrue("true = 'Y'::Boolean");
    evalTrue("'Yes'::Boolean=true");
    evalTrue("true = 'Yes'::Boolean");
    evalTrue("'T'::Boolean=true");
    evalTrue("'TRUE'::Boolean=true");
    evalTrue("true = 'True'::Boolean");

    evalTrue("'0'::Boolean=false");
    evalTrue("'N'::Boolean=false");
    evalTrue("'NO'::Boolean=false");
    evalTrue("'OFF'::Boolean=false");
    evalTrue("'F'::Boolean=false");
    evalTrue("'FALSE'::Boolean=false");
  }
}

