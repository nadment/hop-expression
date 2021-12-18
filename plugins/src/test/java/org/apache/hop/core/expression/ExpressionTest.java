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
    assertEquals(DataType.NONE, DataType.fromData(null) );
    assertEquals(DataType.BOOLEAN, DataType.fromData(true));
    assertEquals(DataType.STRING, DataType.fromData("") );
    assertEquals(DataType.INTEGER, DataType.fromData(1L));
    assertEquals(DataType.NUMBER, DataType.fromData(1D));
    assertEquals(DataType.BIGNUMBER, DataType.fromData(BigDecimal.ONE) );
    assertEquals(DataType.BINARY, DataType.fromData(new byte[] {0x78}));
    assertEquals(DataType.DATE,
        DataType.fromData(ZonedDateTime.of(LocalDateTime.now(), ZoneId.of("Asia/Ho_Chi_Minh")))
        );
    
    assertThrows(IllegalArgumentException.class, () -> DataType.fromData(Float.class) );

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
    assertNull(DataType.toBoolean(null));
    assertTrue(DataType.toBoolean(true));
    assertTrue(DataType.toBoolean(3L));
    assertTrue(DataType.toBoolean(1L));
    assertFalse(DataType.toBoolean(0L));
    assertFalse(DataType.toBoolean(false));
    assertThrows(ExpressionException.class, () -> DataType.toBoolean(ZonedDateTime.now()));
  }
   
  @Test
  public void ConverToBoolean() throws Exception {
    assertNull(DataType.convertTo(null, DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, DataType.convertTo(3L, DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, DataType.convertTo(1L, DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, DataType.convertTo("1", DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, DataType.convertTo("T", DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, DataType.convertTo("True", DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, DataType.convertTo("Y", DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, DataType.convertTo("Yes", DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, DataType.convertTo("ON", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, DataType.convertTo(0L, DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, DataType.convertTo("0", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, DataType.convertTo("F", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, DataType.convertTo("False", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, DataType.convertTo("N", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, DataType.convertTo("No", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, DataType.convertTo("Off", DataType.BOOLEAN));
    assertThrows(ExpressionException.class, () -> DataType.convertTo("3", DataType.BOOLEAN) );
    assertThrows(ExpressionException.class, () -> DataType.convertTo("MO", DataType.BOOLEAN) );
    assertThrows(ExpressionException.class, () -> DataType.convertTo("BAD", DataType.BOOLEAN) );
    assertThrows(ExpressionException.class, () -> DataType.convertTo("TRUL", DataType.BOOLEAN) );    
    assertThrows(ExpressionException.class, () -> DataType.convertTo("FILSE", DataType.BOOLEAN) );
  }
  
  @Test
  public void CoerceToBinary() throws Exception {
    assertNull(DataType.toBinary(null));    
    assertThrows(ExpressionException.class, () -> DataType.toBinary(true) );
    assertThrows(ExpressionException.class, () -> DataType.toBinary(3L) );
    assertThrows(ExpressionException.class, () -> DataType.toBinary(3D) );
  }
  
  @Test
  public void ConverToBinary() throws Exception {
    assertNull(DataType.convertTo(null, DataType.BINARY));
  }
  
  @Test
  public void CoerceToDate() throws Exception {
    assertNull(DataType.toDate(null));
    assertThrows(ExpressionException.class, () -> DataType.toDate(true));
  }
  
  @Test
  public void CoerceToString() throws Exception {
    assertNull(DataType.toString(null));
    assertEquals("-1.0", DataType.toString(-1.0D));
    assertEquals("ABCD", DataType.toString(DataType.toBinary("ABCD")));
    assertEquals("-1.2", DataType.toString(-1.2));    
    assertEquals("0.1", DataType.toString(0.1D));
    assertEquals("-0.1", DataType.toString(-0.1D));
  }
   
  @Test
  public void ConverToString() throws Exception {
    assertNull(DataType.convertTo(null, DataType.STRING));
    assertEquals("TRUE", DataType.convertTo(true, DataType.STRING));
    assertEquals("FALSE", DataType.convertTo(false, DataType.STRING));
  }
  
  @Test
  public void CoerceToInteger() throws Exception {
    assertNull(DataType.toInteger(null));
    assertEquals(Long.valueOf(1), DataType.toInteger(1));
    assertEquals(Long.valueOf(1), DataType.toInteger(1L));
    assertEquals(Long.valueOf(1), DataType.toInteger(1.2D));
    assertEquals(Long.valueOf(1), DataType.toInteger("1.2"));
    assertEquals(Long.valueOf(-2), DataType.toInteger("-1.6"));
    assertThrows(ExpressionException.class, () -> DataType.toInteger(ZonedDateTime.now()));
  }

  @Test
  public void ConverToInteger() throws Exception {
    assertNull(DataType.convertTo(null, DataType.INTEGER));
    assertEquals(1L, DataType.convertTo(true, DataType.INTEGER));
    assertEquals(0L, DataType.convertTo(false, DataType.INTEGER));
    assertEquals(0L, DataType.convertTo(0L, DataType.INTEGER));
    assertEquals(3L, DataType.convertTo(3L, DataType.INTEGER));
    assertEquals(0L, DataType.convertTo(0.0D, DataType.INTEGER));
    assertEquals(3L, DataType.convertTo(3.3D, DataType.INTEGER));
  }
  
  @Test
  public void CoerceToNumber() throws Exception {
    assertNull(DataType.toNumber(null));
    assertEquals(Double.valueOf(1D), DataType.toNumber("1"));
    assertEquals(Double.valueOf(1.2D), DataType.toNumber("1.2"));
    assertEquals(Double.valueOf(0.1D), DataType.toNumber(".1"));
    assertEquals(Double.valueOf(-2.3E+2D), DataType.toNumber("-2.3E+2"));
    assertEquals(Double.valueOf(-2.3E-2D), DataType.toNumber("-2.3E-2"));
    assertEquals(Double.valueOf(-2.3E-2D), DataType.toNumber("-2.3e-2"));
    assertThrows(ExpressionException.class, () -> DataType.toNumber(ZonedDateTime.now()));
  }
  
  @Test
  public void ConverToNumber() throws Exception {
    assertNull(DataType.convertTo(null, DataType.NUMBER));
    assertEquals(Double.valueOf(1D), DataType.convertTo(true, DataType.NUMBER));
    assertEquals(Double.valueOf(0D), DataType.convertTo(false, DataType.NUMBER));
    assertEquals(Double.valueOf(0D), DataType.convertTo(0L, DataType.NUMBER));
    assertEquals(Double.valueOf(3D), DataType.convertTo(3L, DataType.NUMBER));
    assertEquals(Double.valueOf(-2.3E+2D), DataType.convertTo(-2.3E+2D, DataType.NUMBER));    
  }

  @Test
  public void CoerceToBigNumber() throws Exception {
    assertNull(DataType.toBigNumber(null));
    assertEquals(BigDecimal.ZERO, DataType.toBigNumber(0D));
    assertEquals(BigDecimal.ZERO, DataType.toBigNumber(BigDecimal.ZERO));
    assertEquals(BigDecimal.ONE, DataType.toBigNumber(1L));
    assertEquals(BigDecimal.ONE, DataType.toBigNumber(1D));
    assertEquals(BigDecimal.ONE, DataType.toBigNumber(BigDecimal.ONE) );
    assertSame(BigDecimal.valueOf(3), DataType.toBigNumber(3L));
    assertThrows(ExpressionException.class, () -> DataType.toBigNumber(ZonedDateTime.now()));
  }
  
  @Test
  public void ConverToBigNumber() throws Exception {
    assertNull(DataType.convertTo(null, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ONE, DataType.convertTo(true, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, DataType.convertTo(false, DataType.BIGNUMBER));
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

