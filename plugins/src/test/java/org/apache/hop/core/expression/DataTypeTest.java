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
import org.apache.hop.expression.DataTypeFamily;
import org.apache.hop.expression.DataTypeName;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.util.Coerse;
import org.apache.hop.expression.util.Converter;
import org.junit.Test;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;

public class DataTypeTest extends BaseExpressionTest {
    
  @Test
  public void of() throws Exception {
    //assertEquals(DataTypeName.UNKNOWN, DataTypeName.of("UNKNOWN") );
    assertEquals(DataTypeName.BOOLEAN, DataTypeName.of("BOOLEAN"));
    assertEquals(DataTypeName.BOOLEAN, DataTypeName.of("Boolean"));
    assertEquals(DataTypeName.STRING, DataTypeName.of("STRING") );
    assertEquals(DataTypeName.STRING, DataTypeName.of("String") );
    assertEquals(DataTypeName.DATE, DataTypeName.of("DATE") );
    assertEquals(DataTypeName.NUMBER, DataTypeName.of("NUMBER") );
    assertEquals(DataTypeName.BIGNUMBER, DataTypeName.of("BIGNUMBER") );
    assertEquals(DataTypeName.BINARY, DataTypeName.of("BINARY") );
    assertEquals(DataTypeName.JSON, DataTypeName.of("Json") );
    assertEquals(DataTypeName.INTEGER, DataTypeName.of("INTEGER") );
    assertThrows(IllegalArgumentException.class, () -> DataTypeName.of("NOP") );
  }
  
  @Test
  public void from() throws Exception {
    //assertEquals(DataTypeName.UNKNOWN, DataTypeName.from(null) );
    assertEquals(DataTypeName.BOOLEAN, DataTypeName.from(true));
    assertEquals(DataTypeName.STRING, DataTypeName.from("") );
    assertEquals(DataTypeName.INTEGER, DataTypeName.from(1L));
    assertEquals(DataTypeName.NUMBER, DataTypeName.from(1D));
    assertEquals(DataTypeName.BIGNUMBER, DataTypeName.from(BigDecimal.ONE) );
    assertEquals(DataTypeName.BINARY, DataTypeName.from(new byte[] {0x78}));
    assertEquals(DataTypeName.DATE, DataTypeName.from(ZonedDateTime.of(LocalDateTime.now(), ZoneId.of("Asia/Ho_Chi_Minh"))));
    assertEquals(DataTypeName.JSON, DataTypeName.from(Converter.toJson("{\"name\":\"Smith\"}")));
    
    assertThrows(IllegalArgumentException.class, () -> DataTypeName.from(Float.class) );
    assertThrows(IllegalArgumentException.class, () -> DataTypeName.from(Date.class) );
  }
  
  @Test
  public void family() throws Exception {
    assertEquals(DataTypeFamily.BINARY, DataTypeName.BINARY.getFamily());
    assertEquals(DataTypeFamily.BOOLEAN, DataTypeName.BOOLEAN.getFamily());
    assertEquals(DataTypeFamily.DATETIME, DataTypeName.DATE.getFamily());
    assertEquals(DataTypeFamily.JSON, DataTypeName.JSON.getFamily());
    assertEquals(DataTypeFamily.NUMERIC, DataTypeName.INTEGER.getFamily());
    assertEquals(DataTypeFamily.NUMERIC, DataTypeName.NUMBER.getFamily());
    assertEquals(DataTypeFamily.NUMERIC, DataTypeName.BIGNUMBER.getFamily());
    assertEquals(DataTypeFamily.STRING, DataTypeName.STRING.getFamily());
  }
  
  @Test
  public void javaClass() throws Exception {
    assertEquals(byte[].class, DataTypeName.BINARY.getJavaClass() );
    assertEquals(Boolean.class, DataTypeName.BOOLEAN.getJavaClass() );
    assertEquals(Long.class, DataTypeName.INTEGER.getJavaClass() );
    assertEquals(Double.class, DataTypeName.NUMBER.getJavaClass() );
    assertEquals(BigDecimal.class, DataTypeName.BIGNUMBER.getJavaClass() );
    assertEquals(String.class, DataTypeName.STRING.getJavaClass() );
    assertEquals(ZonedDateTime.class, DataTypeName.DATE.getJavaClass() );
   // assertEquals(Void.class, DataTypeName.UNKNOWN.getJavaClass() );
    //assertEquals(Object.class, DataTypeName.ANY.getJavaClass() );
  }
  
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
    assertNull(Converter.to(null, DataTypeName.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.to(3L, DataTypeName.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.to(1L, DataTypeName.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.to("1", DataTypeName.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.to("T", DataTypeName.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.to("True", DataTypeName.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.to("Y", DataTypeName.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.to("Yes", DataTypeName.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.to("ON", DataTypeName.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.to(0L, DataTypeName.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.to("0", DataTypeName.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.to("F", DataTypeName.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.to("False", DataTypeName.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.to("N", DataTypeName.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.to("No", DataTypeName.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.to("Off", DataTypeName.BOOLEAN));
    assertThrows(ExpressionException.class, () -> Converter.to("3", DataTypeName.BOOLEAN) );
    assertThrows(ExpressionException.class, () -> Converter.to("MO", DataTypeName.BOOLEAN) );
    assertThrows(ExpressionException.class, () -> Converter.to("BAD", DataTypeName.BOOLEAN) );
    assertThrows(ExpressionException.class, () -> Converter.to("TRUL", DataTypeName.BOOLEAN) );    
    assertThrows(ExpressionException.class, () -> Converter.to("FILSE", DataTypeName.BOOLEAN) );
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
    assertNull(Converter.to(null, DataTypeName.BINARY));
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
    assertNull(Converter.to(null, DataTypeName.STRING));
    assertEquals("TRUE", Converter.to(true, DataTypeName.STRING));
    assertEquals("FALSE", Converter.to(false, DataTypeName.STRING));
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
    assertNull(Converter.to(null, DataTypeName.INTEGER));
    assertEquals(Long.valueOf(1L), Converter.to(true, DataTypeName.INTEGER));
    assertEquals(Long.valueOf(0L), Converter.to(false, DataTypeName.INTEGER));
    assertEquals(Long.valueOf(0L), Converter.to(0L, DataTypeName.INTEGER));
    assertEquals(Long.valueOf(3L), Converter.to(3L, DataTypeName.INTEGER));
    assertEquals(Long.valueOf(0L), Converter.to(0.0D, DataTypeName.INTEGER));
    assertEquals(Long.valueOf(3L), Converter.to(3.3D, DataTypeName.INTEGER));
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
    assertNull(Converter.to(null, DataTypeName.NUMBER));
    assertEquals(Double.valueOf(1D), Converter.to(true, DataTypeName.NUMBER));
    assertEquals(Double.valueOf(0D), Converter.to(false, DataTypeName.NUMBER));
    assertEquals(Double.valueOf(0D), Converter.to(0L, DataTypeName.NUMBER));
    assertEquals(Double.valueOf(3D), Converter.to(3L, DataTypeName.NUMBER));
    assertEquals(Double.valueOf(-2.3E+2D), Converter.to(-2.3E+2D, DataTypeName.NUMBER));    
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
    assertNull(Converter.to(null, DataTypeName.BIGNUMBER));    
    assertEquals(BigDecimal.ZERO, Converter.to(false, DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, Converter.to(0L, DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, Converter.to(0D, DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, Converter.to("0", DataTypeName.BIGNUMBER));
    //assertEquals(BigDecimal.ZERO, Converter.to("0.000", DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.ONE, Converter.to(true, DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.ONE, Converter.to(1L, DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.ONE, Converter.to(1D, DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.ONE, Converter.to("1", DataTypeName.BIGNUMBER));
    //assertEquals(BigDecimal.ONE, DataType.convertTo("1.000", DataType.BIGNUMBER));
    assertEquals(BigDecimal.valueOf(-356L), Converter.to(-356L, DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.valueOf(-3.56E2D), Converter.to(-3.56E+2D, DataTypeName.BIGNUMBER));
    assertEquals(new BigDecimal("0.000"), Converter.to("0.000", DataTypeName.BIGNUMBER));
    assertEquals(new BigDecimal("-3.56E2"), Converter.to("-3.56E+2", DataTypeName.BIGNUMBER));
  }
  
  @Test
  public void convertToUnknown() throws Exception {
   // assertThrows(ExpressionException.class, () -> Converter.to(null, DataTypeName.UNKNOWN));
   // assertThrows(ExpressionException.class, () -> Converter.to(true, DataTypeName.UNKNOWN));
   // assertThrows(ExpressionException.class, () -> Converter.to("Test", DataTypeName.UNKNOWN));
   // assertThrows(ExpressionException.class, () -> Converter.to(BigDecimal.ZERO, DataTypeName.UNKNOWN));
  }

}

