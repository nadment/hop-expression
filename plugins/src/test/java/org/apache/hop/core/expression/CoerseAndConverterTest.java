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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.hop.expression.DataType;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.util.Coerse;
import org.apache.hop.expression.util.Converter;
import org.junit.Test;
import java.math.BigDecimal;
import java.time.ZonedDateTime;

public class CoerseAndConverterTest extends BaseExpressionTest {
  
  @Test
  public void coerceToBoolean() throws Exception {
    assertNull(Coerse.toBoolean(null));
    assertTrue(Coerse.toBoolean(true));
    assertTrue(Coerse.toBoolean(3L));
    assertTrue(Coerse.toBoolean(1L));
    assertFalse(Coerse.toBoolean(0L));
    assertFalse(Coerse.toBoolean(false));
    assertThrows(ExpressionException.class, () -> Coerse.toBoolean("True"));
    assertThrows(ExpressionException.class, () -> Coerse.toBoolean(ZonedDateTime.now()));
  }
   
  @Test
  public void convertToBoolean() throws Exception {
    assertNull(Converter.to(null, DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.to(3L, DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.to(1L, DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.to("1", DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.to("T", DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.to("True", DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.to("Y", DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.to("Yes", DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.to("ON", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.to(0L, DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.to("0", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.to("F", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.to("False", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.to("N", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.to("No", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.to("Off", DataType.BOOLEAN));
    assertThrows(ExpressionException.class, () -> Converter.to("3", DataType.BOOLEAN) );
    assertThrows(ExpressionException.class, () -> Converter.to("MO", DataType.BOOLEAN) );
    assertThrows(ExpressionException.class, () -> Converter.to("BAD", DataType.BOOLEAN) );
    assertThrows(ExpressionException.class, () -> Converter.to("TRUL", DataType.BOOLEAN) );    
    assertThrows(ExpressionException.class, () -> Converter.to("FILSE", DataType.BOOLEAN) );
  }
  
  @Test
  public void coerceToBinary() throws Exception {
    assertNull(Coerse.toBinary(null));    
    assertThrows(ExpressionException.class, () -> Coerse.toBinary(true) );
    assertThrows(ExpressionException.class, () -> Coerse.toBinary(3L) );
    assertThrows(ExpressionException.class, () -> Coerse.toBinary(3D) );
  }
  
  @Test
  public void convertToBinary() throws Exception {
    assertNull(Converter.to(null, DataType.BINARY));
  }
  
  @Test
  public void coerceToDate() throws Exception {
    assertNull(Coerse.toDate(null));
    assertThrows(ExpressionException.class, () -> Coerse.toDate(true));
  }
  
  @Test
  public void coerceToString() throws Exception {
    assertNull(Coerse.toString(null));
    assertEquals("-1.0", Coerse.toString(-1.0D));
    assertEquals("ABCD", Coerse.toString(Coerse.toBinary("ABCD")));
    assertEquals("-1.2", Coerse.toString(-1.2));    
    assertEquals("0.1", Coerse.toString(0.1D));
    assertEquals("-0.1", Coerse.toString(-0.1D));
    assertEquals("0", Coerse.toString(BigDecimal.ZERO));
    assertEquals("1", Coerse.toString(BigDecimal.ONE));
  }
   
  @Test
  public void convertToString() throws Exception {
    assertNull(Converter.to(null, DataType.STRING));
    assertEquals("TRUE", Converter.to(true, DataType.STRING));
    assertEquals("FALSE", Converter.to(false, DataType.STRING));
  }
  
  @Test
  public void coerceToInteger() throws Exception {
    assertNull(Coerse.toInteger(null));
    assertEquals(Long.valueOf(1L), Coerse.toInteger(1));
    assertEquals(Long.valueOf(1L), Coerse.toInteger(1L));
    assertEquals(Long.valueOf(1L), Coerse.toInteger(1.2D));
    assertEquals(Long.valueOf(1L), Coerse.toInteger("1.2"));
    assertEquals(Long.valueOf(-1L), Coerse.toInteger("-1.6"));
    assertThrows(ExpressionException.class, () -> Coerse.toInteger(ZonedDateTime.now()));
  }

  @Test
  public void convertToInteger() throws Exception {
    assertNull(Converter.to(null, DataType.INTEGER));
    assertEquals(Long.valueOf(1L), Converter.to(true, DataType.INTEGER));
    assertEquals(Long.valueOf(0L), Converter.to(false, DataType.INTEGER));
    assertEquals(Long.valueOf(0L), Converter.to(0L, DataType.INTEGER));
    assertEquals(Long.valueOf(3L), Converter.to(3L, DataType.INTEGER));
    assertEquals(Long.valueOf(0L), Converter.to(0.0D, DataType.INTEGER));
    assertEquals(Long.valueOf(3L), Converter.to(3.3D, DataType.INTEGER));
  }
  
  @Test
  public void coerceToNumber() throws Exception {
    assertNull(Coerse.toNumber(null));
    assertEquals(Double.valueOf(1D), Coerse.toNumber("1"));
    assertEquals(Double.valueOf(1.2D), Coerse.toNumber("1.2"));
    assertEquals(Double.valueOf(0.1D), Coerse.toNumber(".1"));
    assertEquals(Double.valueOf(-2.3E+2D), Coerse.toNumber("-2.3E+2"));
    assertEquals(Double.valueOf(-2.3E-2D), Coerse.toNumber("-2.3E-2"));
    assertEquals(Double.valueOf(-2.3E-2D), Coerse.toNumber("-2.3e-2"));
    assertThrows(ExpressionException.class, () -> Coerse.toNumber(ZonedDateTime.now()));
  }
  
  @Test
  public void convertToNumber() throws Exception {
    assertNull(Converter.to(null, DataType.NUMBER));
    assertEquals(Double.valueOf(1D), Converter.to(true, DataType.NUMBER));
    assertEquals(Double.valueOf(0D), Converter.to(false, DataType.NUMBER));
    assertEquals(Double.valueOf(0D), Converter.to(0L, DataType.NUMBER));
    assertEquals(Double.valueOf(3D), Converter.to(3L, DataType.NUMBER));
    assertEquals(Double.valueOf(-2.3E+2D), Converter.to(-2.3E+2D, DataType.NUMBER));    
  }

  @Test
  public void coerceToBigNumber() throws Exception {
    assertNull(Coerse.toBigNumber(null));    
    assertEquals(BigDecimal.ZERO, Coerse.toBigNumber(0L));
    assertEquals(BigDecimal.ZERO, Coerse.toBigNumber(0D));
    assertEquals(BigDecimal.ZERO, Coerse.toBigNumber(BigDecimal.ZERO));    
    assertEquals(BigDecimal.ONE, Coerse.toBigNumber(1L));
    assertEquals(BigDecimal.ONE, Coerse.toBigNumber(1D));
    assertEquals(BigDecimal.ONE, Coerse.toBigNumber(BigDecimal.ONE) );
    assertSame(BigDecimal.valueOf(3), Coerse.toBigNumber(3L));
    assertThrows(ExpressionException.class, () -> Coerse.toBigNumber(ZonedDateTime.now()));
  }
  
  @Test
  public void convertToBigNumber() throws Exception {
    assertNull(Converter.to(null, DataType.BIGNUMBER));    
    assertEquals(BigDecimal.ZERO, Converter.to(false, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, Converter.to(0L, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, Converter.to(0D, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, Converter.to("0", DataType.BIGNUMBER));
    //assertEquals(BigDecimal.ZERO, Convert.convertTo("0.000", DataType.BIGNUMBER));
    assertEquals(BigDecimal.ONE, Converter.to(true, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ONE, Converter.to(1L, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ONE, Converter.to(1D, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ONE, Converter.to("1", DataType.BIGNUMBER));
    //assertEquals(BigDecimal.ONE, DataType.convertTo("1.000", DataType.BIGNUMBER));
    assertEquals(BigDecimal.valueOf(-356L), Converter.to(-356L, DataType.BIGNUMBER));
    assertEquals(BigDecimal.valueOf(-3.56E2D), Converter.to(-3.56E+2D, DataType.BIGNUMBER));
    assertEquals(new BigDecimal("0.000"), Converter.to("0.000", DataType.BIGNUMBER));
    assertEquals(new BigDecimal("-3.56E2"), Converter.to("-3.56E+2", DataType.BIGNUMBER));
  }
  
  @Test
  public void convertToNone() throws Exception {
    assertNull(Converter.to(null, DataType.UNKNOWN));
    assertNull(Converter.to(true, DataType.UNKNOWN));
    assertNull(Converter.to(BigDecimal.ZERO, DataType.UNKNOWN));
  }
}

