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
import org.junit.Test;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;

public class DataTypeTest extends BaseExpressionTest {
      
  @Test
  public void from() throws Exception {
    assertEquals(DataType.NONE, DataType.from(null) );
    assertEquals(DataType.BOOLEAN, DataType.from(true));
    assertEquals(DataType.STRING, DataType.from("") );
    assertEquals(DataType.INTEGER, DataType.from(1L));
    assertEquals(DataType.NUMBER, DataType.from(1D));
    assertEquals(DataType.BIGNUMBER, DataType.from(BigDecimal.ONE) );
    assertEquals(DataType.BINARY, DataType.from(new byte[] {0x78}));
    assertEquals(DataType.DATE,
        DataType.from(ZonedDateTime.of(LocalDateTime.now(), ZoneId.of("Asia/Ho_Chi_Minh")))
        );
    
    assertThrows(IllegalArgumentException.class, () -> DataType.from(Float.class) );
    assertThrows(IllegalArgumentException.class, () -> DataType.from(Date.class) );
  }
  
  @Test
  public void javaClass() throws Exception {
    assertEquals(Boolean.class, DataType.BOOLEAN.javaClass() );
    assertEquals(Long.class, DataType.INTEGER.javaClass() );
    assertEquals(Double.class, DataType.NUMBER.javaClass() );
    assertEquals(BigDecimal.class, DataType.BIGNUMBER.javaClass() );
    assertEquals(String.class, DataType.STRING.javaClass() );
    assertEquals(ZonedDateTime.class, DataType.DATE.javaClass() );
    assertEquals(byte[].class, DataType.BINARY.javaClass() );
  }
  
  @Test
  public void coerceToBoolean() throws Exception {
    assertNull(DataType.toBoolean(null));
    assertTrue(DataType.toBoolean(true));
    assertTrue(DataType.toBoolean(3L));
    assertTrue(DataType.toBoolean(1L));
    assertFalse(DataType.toBoolean(0L));
    assertFalse(DataType.toBoolean(false));
    assertThrows(ExpressionException.class, () -> DataType.toBoolean(ZonedDateTime.now()));
  }
   
  @Test
  public void converToBoolean() throws Exception {
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
  public void coerceToBinary() throws Exception {
    assertNull(DataType.toBinary(null));    
    assertThrows(ExpressionException.class, () -> DataType.toBinary(true) );
    assertThrows(ExpressionException.class, () -> DataType.toBinary(3L) );
    assertThrows(ExpressionException.class, () -> DataType.toBinary(3D) );
  }
  
  @Test
  public void converToBinary() throws Exception {
    assertNull(DataType.convertTo(null, DataType.BINARY));
  }
  
  @Test
  public void coerceToDate() throws Exception {
    assertNull(DataType.toDate(null));
    assertThrows(ExpressionException.class, () -> DataType.toDate(true));
  }
  
  @Test
  public void coerceToString() throws Exception {
    assertNull(DataType.toString(null));
    assertEquals("-1.0", DataType.toString(-1.0D));
    assertEquals("ABCD", DataType.toString(DataType.toBinary("ABCD")));
    assertEquals("-1.2", DataType.toString(-1.2));    
    assertEquals("0.1", DataType.toString(0.1D));
    assertEquals("-0.1", DataType.toString(-0.1D));
    assertEquals("0", DataType.toString(BigDecimal.ZERO));
    assertEquals("1", DataType.toString(BigDecimal.ONE));
  }
   
  @Test
  public void converToString() throws Exception {
    assertNull(DataType.convertTo(null, DataType.STRING));
    assertEquals("TRUE", DataType.convertTo(true, DataType.STRING));
    assertEquals("FALSE", DataType.convertTo(false, DataType.STRING));
  }
  
  @Test
  public void coerceToInteger() throws Exception {
    assertNull(DataType.toInteger(null));
    assertEquals(Long.valueOf(1), DataType.toInteger(1));
    assertEquals(Long.valueOf(1), DataType.toInteger(1L));
    assertEquals(Long.valueOf(1), DataType.toInteger(1.2D));
    assertEquals(Long.valueOf(1), DataType.toInteger("1.2"));
    assertEquals(Long.valueOf(-2), DataType.toInteger("-1.6"));
    assertThrows(ExpressionException.class, () -> DataType.toInteger(ZonedDateTime.now()));
  }

  @Test
  public void converToInteger() throws Exception {
    assertNull(DataType.convertTo(null, DataType.INTEGER));
    assertEquals(1L, DataType.convertTo(true, DataType.INTEGER));
    assertEquals(0L, DataType.convertTo(false, DataType.INTEGER));
    assertEquals(0L, DataType.convertTo(0L, DataType.INTEGER));
    assertEquals(3L, DataType.convertTo(3L, DataType.INTEGER));
    assertEquals(0L, DataType.convertTo(0.0D, DataType.INTEGER));
    assertEquals(3L, DataType.convertTo(3.3D, DataType.INTEGER));
  }
  
  @Test
  public void coerceToNumber() throws Exception {
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
  public void converToNumber() throws Exception {
    assertNull(DataType.convertTo(null, DataType.NUMBER));
    assertEquals(Double.valueOf(1D), DataType.convertTo(true, DataType.NUMBER));
    assertEquals(Double.valueOf(0D), DataType.convertTo(false, DataType.NUMBER));
    assertEquals(Double.valueOf(0D), DataType.convertTo(0L, DataType.NUMBER));
    assertEquals(Double.valueOf(3D), DataType.convertTo(3L, DataType.NUMBER));
    assertEquals(Double.valueOf(-2.3E+2D), DataType.convertTo(-2.3E+2D, DataType.NUMBER));    
  }

  @Test
  public void coerceToBigNumber() throws Exception {
    assertNull(DataType.toBigNumber(null));    
    assertEquals(BigDecimal.ZERO, DataType.toBigNumber(0L));
    assertEquals(BigDecimal.ZERO, DataType.toBigNumber(0D));
    assertEquals(BigDecimal.ZERO, DataType.toBigNumber(BigDecimal.ZERO));    
    assertEquals(BigDecimal.ONE, DataType.toBigNumber(1L));
    assertEquals(BigDecimal.ONE, DataType.toBigNumber(1D));
    assertEquals(BigDecimal.ONE, DataType.toBigNumber(BigDecimal.ONE) );
    assertSame(BigDecimal.valueOf(3), DataType.toBigNumber(3L));
    assertThrows(ExpressionException.class, () -> DataType.toBigNumber(ZonedDateTime.now()));
  }
  
  @Test
  public void converToBigNumber() throws Exception {
    assertNull(DataType.convertTo(null, DataType.BIGNUMBER));    
    assertEquals(BigDecimal.ZERO, DataType.convertTo(false, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, DataType.convertTo(0L, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, DataType.convertTo(0D, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, DataType.convertTo("0", DataType.BIGNUMBER));
    //assertEquals(BigDecimal.ZERO, DataType.convertTo("0.000", DataType.BIGNUMBER));
    assertEquals(BigDecimal.ONE, DataType.convertTo(true, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ONE, DataType.convertTo(1L, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ONE, DataType.convertTo(1D, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ONE, DataType.convertTo("1", DataType.BIGNUMBER));
    //assertEquals(BigDecimal.ONE, DataType.convertTo("1.000", DataType.BIGNUMBER));
    assertEquals(BigDecimal.valueOf(-356L), DataType.convertTo(-356L, DataType.BIGNUMBER));
    assertEquals(BigDecimal.valueOf(-3.56E2D), DataType.convertTo(-3.56E+2D, DataType.BIGNUMBER));
    assertEquals(new BigDecimal("0.000"), DataType.convertTo("0.000", DataType.BIGNUMBER));
    assertEquals(new BigDecimal("-3.56E2"), DataType.convertTo("-3.56E+2", DataType.BIGNUMBER));
  }
  
  @Test
  public void converToNone() throws Exception {
    assertNull(DataType.convertTo(null, DataType.NONE));
    assertNull(DataType.convertTo(true, DataType.NONE));
    assertNull(DataType.convertTo(BigDecimal.ZERO, DataType.NONE));
  }
}

