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
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.type.Converter;
import org.apache.hop.expression.type.DataTypeFamily;
import org.apache.hop.expression.type.DataTypeName;
import org.junit.Test;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;
import com.fasterxml.jackson.databind.JsonNode;

public class DataTypeTest extends BaseExpressionTest {

  @Test
  public void of() throws Exception {
    assertEquals(DataTypeName.UNKNOWN, DataTypeName.of("UNKNOWN"));
    assertEquals(DataTypeName.BOOLEAN, DataTypeName.of("BOOLEAN"));
    assertEquals(DataTypeName.BOOLEAN, DataTypeName.of("Boolean"));
    assertEquals(DataTypeName.STRING, DataTypeName.of("STRING"));
    assertEquals(DataTypeName.STRING, DataTypeName.of("String"));
    assertEquals(DataTypeName.DATE, DataTypeName.of("DATE"));
    assertEquals(DataTypeName.NUMBER, DataTypeName.of("NUMBER"));
    assertEquals(DataTypeName.BIGNUMBER, DataTypeName.of("BIGNUMBER"));
    assertEquals(DataTypeName.BINARY, DataTypeName.of("BINARY"));
    assertEquals(DataTypeName.JSON, DataTypeName.of("Json"));
    assertEquals(DataTypeName.INTEGER, DataTypeName.of("INTEGER"));
    assertThrows(IllegalArgumentException.class, () -> DataTypeName.of("NOP"));
  }

  @Test
  public void from() throws Exception {
    assertEquals(DataTypeName.UNKNOWN, DataTypeName.from(null));
    assertEquals(DataTypeName.BOOLEAN, DataTypeName.from(true));
    assertEquals(DataTypeName.STRING, DataTypeName.from("test"));
    assertEquals(DataTypeName.INTEGER, DataTypeName.from(1L));
    assertEquals(DataTypeName.NUMBER, DataTypeName.from(1D));
    assertEquals(DataTypeName.BIGNUMBER, DataTypeName.from(BigDecimal.ONE));
    assertEquals(DataTypeName.BINARY, DataTypeName.from(new byte[] {0x78}));
    assertEquals(DataTypeName.DATE,
        DataTypeName.from(ZonedDateTime.of(LocalDateTime.now(), ZoneId.of("Asia/Ho_Chi_Minh"))));
    assertEquals(DataTypeName.JSON, DataTypeName.from(Converter.parseJson("{\"name\":\"Smith\"}")));

    assertThrows(IllegalArgumentException.class, () -> DataTypeName.from(Float.class));
    assertThrows(IllegalArgumentException.class, () -> DataTypeName.from(Date.class));
  }

  @Test
  public void family() throws Exception {
    assertTrue(DataTypeFamily.ANY.isSameFamily(DataTypeFamily.BINARY));
    assertTrue(DataTypeFamily.BINARY.isSameFamily(DataTypeFamily.ANY));
    assertTrue(DataTypeFamily.BINARY.isSameFamily(DataTypeFamily.BINARY));
    assertFalse(DataTypeFamily.BINARY.isSameFamily(DataTypeFamily.NUMERIC));
    
    assertEquals(DataTypeFamily.BINARY, DataTypeName.BINARY.getFamily());
    assertEquals(DataTypeFamily.BOOLEAN, DataTypeName.BOOLEAN.getFamily());
    assertEquals(DataTypeFamily.DATE, DataTypeName.DATE.getFamily());
    assertEquals(DataTypeFamily.JSON, DataTypeName.JSON.getFamily());
    assertEquals(DataTypeFamily.NUMERIC, DataTypeName.INTEGER.getFamily());
    assertEquals(DataTypeFamily.NUMERIC, DataTypeName.NUMBER.getFamily());
    assertEquals(DataTypeFamily.NUMERIC, DataTypeName.BIGNUMBER.getFamily());
    assertEquals(DataTypeFamily.STRING, DataTypeName.STRING.getFamily());
  }

  @Test
  public void javaClass() throws Exception {
    assertEquals(byte[].class, DataTypeName.BINARY.getJavaClass());
    assertEquals(Boolean.class, DataTypeName.BOOLEAN.getJavaClass());
    assertEquals(Long.class, DataTypeName.INTEGER.getJavaClass());
    assertEquals(Double.class, DataTypeName.NUMBER.getJavaClass());
    assertEquals(BigDecimal.class, DataTypeName.BIGNUMBER.getJavaClass());
    assertEquals(String.class, DataTypeName.STRING.getJavaClass());
    assertEquals(ZonedDateTime.class, DataTypeName.DATE.getJavaClass());
    assertEquals(JsonNode.class, DataTypeName.JSON.getJavaClass());
    assertEquals(Void.class, DataTypeName.UNKNOWN.getJavaClass());
  }

  @Test
  public void coerceToBoolean() throws Exception {
    assertNull(Converter.coerceToBoolean(null));
    assertTrue(Converter.coerceToBoolean(true));
    assertTrue(Converter.coerceToBoolean(3L));
    assertTrue(Converter.coerceToBoolean(1D));
    assertFalse(Converter.coerceToBoolean(0L));
    assertFalse(Converter.coerceToBoolean(false));
    assertThrows(ExpressionException.class, () -> Converter.coerceToBoolean("True"));
    assertThrows(ExpressionException.class, () -> Converter.coerceToBoolean(ZonedDateTime.now()));
  }

  @Test
  public void castToBoolean() throws Exception {
    assertNull(Converter.cast(null, DataTypeName.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast(3L, DataTypeName.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast(1L, DataTypeName.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast(1D, DataTypeName.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast(true, DataTypeName.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast(BigDecimal.ONE, DataTypeName.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast("1", DataTypeName.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast("T", DataTypeName.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast("True", DataTypeName.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast("Y", DataTypeName.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast("Yes", DataTypeName.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast("ON", DataTypeName.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.cast(0L, DataTypeName.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.cast(0D, DataTypeName.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.cast(false, DataTypeName.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.cast(BigDecimal.ZERO, DataTypeName.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.cast("0", DataTypeName.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.cast("F", DataTypeName.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.cast("False", DataTypeName.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.cast("N", DataTypeName.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.cast("No", DataTypeName.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.cast("Off", DataTypeName.BOOLEAN));
    assertThrows(ExpressionException.class, () -> Converter.cast("3", DataTypeName.BOOLEAN));
    assertThrows(ExpressionException.class, () -> Converter.cast("MO", DataTypeName.BOOLEAN));
    assertThrows(ExpressionException.class, () -> Converter.cast("BAD", DataTypeName.BOOLEAN));
    assertThrows(ExpressionException.class, () -> Converter.cast("TRUL", DataTypeName.BOOLEAN));
    assertThrows(ExpressionException.class, () -> Converter.cast("FILSE", DataTypeName.BOOLEAN));
  }

  @Test
  public void coerceToBinary() throws Exception {
    assertNull(Converter.coerceToBinary(null));
    assertThrows(ExpressionException.class, () -> Converter.coerceToBinary(true));
    assertThrows(ExpressionException.class, () -> Converter.coerceToBinary(3L));
    assertThrows(ExpressionException.class, () -> Converter.coerceToBinary(3D));
  }

  @Test
  public void castToBinary() throws Exception {
    assertNull(Converter.cast(null, DataTypeName.BINARY));
    //assertEquals(new byte[] {0xF, 0xC}, Converter.cast(new byte[] {0xF, 0xC}, DataTypeName.BINARY));

    assertThrows(ExpressionException.class, () ->  Converter.cast(true, DataTypeName.BINARY));
    assertThrows(ExpressionException.class, () ->  Converter.cast(1L, DataTypeName.BINARY));
    assertThrows(ExpressionException.class, () ->  Converter.cast(1D, DataTypeName.BINARY));
    assertThrows(ExpressionException.class, () ->  Converter.cast(BigDecimal.ONE, DataTypeName.BINARY));
    assertThrows(ExpressionException.class, () ->  Converter.cast(ZonedDateTime.now(), DataTypeName.BINARY));
  }

  @Test
  public void coerceToDate() throws Exception {
    ZonedDateTime date = LocalDate.of(2022, Month.DECEMBER, 28).atStartOfDay().atZone(ZoneId.systemDefault());
    
    assertNull(Converter.coerceToDateTime(null));
    //assertEquals(date, Converter.coerceToDate(date));
    
    assertThrows(ExpressionException.class, () -> Converter.coerceToDateTime(true));
    assertThrows(ExpressionException.class, () -> Converter.coerceToDateTime("2022"));
  }
  
  @Test
  public void castToDate() throws Exception {
    ZonedDateTime date = LocalDate.of(2022, Month.DECEMBER, 28).atStartOfDay().atZone(ZoneId.systemDefault());
    
    assertNull(Converter.cast(null, DataTypeName.DATE));
    assertEquals(date, Converter.cast("2022-12-28", DataTypeName.DATE));
    assertEquals(date, Converter.cast("2022-12-28", DataTypeName.DATE, "YYYY-MM-DD"));
  }

  @Test
  public void coerceToString() throws Exception {
    assertNull(Converter.coerceToString(null));
    assertEquals("TRUE", Converter.coerceToString(true));
    assertEquals("FALSE", Converter.coerceToString(false));
    assertEquals("-1.0", Converter.coerceToString(-1.0D));
    assertEquals("-1.2", Converter.coerceToString(-1.2));
    assertEquals("0.1", Converter.coerceToString(0.1D));
    assertEquals("-0.1", Converter.coerceToString(-0.1D));
    assertEquals("1", Converter.coerceToString(BigDecimal.ONE));
  }

  @Test
  public void castToString() throws Exception {
    assertNull(Converter.cast(null, DataTypeName.STRING));
    assertEquals("TRUE", Converter.cast(true, DataTypeName.STRING));
    assertEquals("FALSE", Converter.cast(false, DataTypeName.STRING));
    assertEquals("0", Converter.cast(0L, DataTypeName.STRING));
    assertEquals("1", Converter.cast(1L, DataTypeName.STRING));
    assertEquals("0", Converter.cast(0D, DataTypeName.STRING));
    assertEquals("1.2", Converter.cast(1.2D, DataTypeName.STRING));
    assertEquals("0", Converter.cast(BigDecimal.ZERO, DataTypeName.STRING));
    assertEquals("1", Converter.cast(BigDecimal.ONE, DataTypeName.STRING));
    assertEquals("ABCDéç", Converter.cast("ABCDéç".getBytes(StandardCharsets.UTF_8), DataTypeName.STRING));
  }

  @Test
  public void coerceToInteger() throws Exception {
    assertNull(Converter.coerceToInteger(null));
    assertEquals(Long.valueOf(1L), Converter.coerceToInteger(1));
    assertEquals(Long.valueOf(1L), Converter.coerceToInteger(1L));
    assertEquals(Long.valueOf(1L), Converter.coerceToInteger(1.2D));
    assertEquals(Long.valueOf(1L), Converter.coerceToInteger("1.2"));
    assertEquals(Long.valueOf(-1L), Converter.coerceToInteger("-1.6"));
    assertEquals(Long.valueOf(1L), Converter.coerceToInteger(BigDecimal.ONE));
    assertEquals(Long.valueOf(0L), Converter.coerceToInteger(BigDecimal.ZERO));

    assertThrows(ExpressionException.class, () -> Converter.coerceToInteger(true));
    assertThrows(ExpressionException.class, () -> Converter.coerceToInteger(new byte[] {0xF}));
    assertThrows(ExpressionException.class, () -> Converter.coerceToInteger(ZonedDateTime.now()));
    assertThrows(ExpressionException.class, () -> Converter.coerceToInteger("FALSE"));
  }

  @Test
  public void castToInteger() throws Exception {
    assertNull(Converter.cast(null, DataTypeName.INTEGER));
    assertEquals(1L, Converter.cast(true, DataTypeName.INTEGER));
    assertEquals(0L, Converter.cast(false, DataTypeName.INTEGER));
    assertEquals(0L, Converter.cast(0L, DataTypeName.INTEGER));
    assertEquals(3L, Converter.cast(3L, DataTypeName.INTEGER));
    assertEquals(0L, Converter.cast(0.0D, DataTypeName.INTEGER));
    assertEquals(3L, Converter.cast(3.3D, DataTypeName.INTEGER));
    assertEquals(0L, Converter.cast(BigDecimal.ZERO, DataTypeName.INTEGER));
    assertEquals(1L, Converter.cast(BigDecimal.ONE, DataTypeName.INTEGER));
    assertEquals(3L, Converter.cast(BigDecimal.valueOf(3.125), DataTypeName.INTEGER));
    assertEquals(5L, Converter.cast("5.9", DataTypeName.INTEGER));
    assertEquals(-5L, Converter.cast("-5.2", DataTypeName.INTEGER));
    assertEquals(15L, Converter.cast(new byte[] {0xF}, DataTypeName.INTEGER));
  }

  @Test
  public void coerceToNumber() throws Exception {
    assertNull(Converter.coerceToNumber(null));
    assertEquals(Double.valueOf(1D), Converter.coerceToNumber("1"));
    assertEquals(Double.valueOf(1.2D), Converter.coerceToNumber("1.2"));
    assertEquals(Double.valueOf(0.1D), Converter.coerceToNumber(".1"));
    assertEquals(Double.valueOf(-2.3E+2D), Converter.coerceToNumber("-2.3E+2"));
    assertEquals(Double.valueOf(-2.3E-2D), Converter.coerceToNumber("-2.3E-2"));
    assertEquals(Double.valueOf(-2.3E-2D), Converter.coerceToNumber("-2.3e-2"));
    assertEquals(Double.valueOf(1D), Converter.coerceToNumber(BigDecimal.ONE));
    assertEquals(Double.valueOf(0D), Converter.coerceToNumber(BigDecimal.ZERO));

    assertThrows(ExpressionException.class, () -> Converter.coerceToNumber(true));
    assertThrows(ExpressionException.class, () -> Converter.coerceToNumber(new byte[] {0xF}));
    assertThrows(ExpressionException.class, () -> Converter.coerceToNumber(ZonedDateTime.now()));
    assertThrows(ExpressionException.class, () -> Converter.coerceToNumber("FALSE"));
  }

  @Test
  public void castToNumber() throws Exception {
    assertNull(Converter.cast(null, DataTypeName.NUMBER));
    assertEquals(Double.valueOf(1D), Converter.cast(true, DataTypeName.NUMBER));
    assertEquals(Double.valueOf(0D), Converter.cast(false, DataTypeName.NUMBER));
    assertEquals(Double.valueOf(0D), Converter.cast(0L, DataTypeName.NUMBER));
    assertEquals(Double.valueOf(3D), Converter.cast(3L, DataTypeName.NUMBER));
    assertEquals(Double.valueOf(1D), Converter.cast(BigDecimal.ONE, DataTypeName.NUMBER));
    assertEquals(Double.valueOf(0D), Converter.cast(BigDecimal.ZERO, DataTypeName.NUMBER));
    assertEquals(Double.valueOf(0.5D), Converter.cast("0.5", DataTypeName.NUMBER));
    assertEquals(Double.valueOf(0.5D), Converter.cast(".5", DataTypeName.NUMBER));
    assertEquals(Double.valueOf(-2.3E+2D), Converter.cast(-2.3E+2D, DataTypeName.NUMBER));
    assertEquals(15D, Converter.cast(new byte[] {0xF}, DataTypeName.NUMBER));

    assertThrows(ExpressionException.class,
        () -> Converter.cast(ZonedDateTime.now(), DataTypeName.NUMBER));
  }

  @Test
  public void coerceToBigNumber() throws Exception {
    assertNull(Converter.coerceToBigNumber(null));

    assertEquals(BigDecimal.ZERO, Converter.coerceToBigNumber(0L));
    assertEquals(BigDecimal.ZERO, Converter.coerceToBigNumber(0D));
    assertEquals(BigDecimal.ZERO, Converter.coerceToBigNumber(BigDecimal.ZERO));
    assertEquals(BigDecimal.ZERO, Converter.coerceToBigNumber("0"));
    assertEquals(BigDecimal.ONE, Converter.coerceToBigNumber(1L));
    assertEquals(BigDecimal.ONE, Converter.coerceToBigNumber(1D));
    assertEquals(BigDecimal.ONE, Converter.coerceToBigNumber(BigDecimal.ONE));
    assertEquals(BigDecimal.ONE, Converter.coerceToBigNumber("1"));
    assertEquals(BigDecimal.valueOf(3.123), Converter.coerceToBigNumber(3.123D));

    assertThrows(ExpressionException.class, () -> Converter.coerceToBigNumber(true));
    assertThrows(ExpressionException.class, () -> Converter.coerceToBigNumber(new byte[] {0xF}));
    assertThrows(ExpressionException.class, () -> Converter.coerceToBigNumber(ZonedDateTime.now()));
    assertThrows(ExpressionException.class, () -> Converter.coerceToBigNumber("FALSE"));
  }

  @Test
  public void castToBigNumber() throws Exception {
    assertNull(Converter.cast(null, DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, Converter.cast(false, DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, Converter.cast(0L, DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, Converter.cast(0D, DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, Converter.cast("0", DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, Converter.cast(new byte[] {0x00}, DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.ONE, Converter.cast(true, DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.ONE, Converter.cast(1L, DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.ONE, Converter.cast(1D, DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.ONE, Converter.cast("1", DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.ONE, Converter.cast(new byte[] {0x01}, DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.valueOf(-356L), Converter.cast(-356L, DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.valueOf(-3.56E2D), Converter.cast(-3.56E+2D, DataTypeName.BIGNUMBER));
    assertEquals(new BigDecimal("0.000"), Converter.cast("0.000", DataTypeName.BIGNUMBER));
    assertEquals(new BigDecimal("-3.56E2"), Converter.cast("-3.56E+2", DataTypeName.BIGNUMBER));
    assertEquals(BigDecimal.valueOf(15), Converter.cast(new byte[] {0xF}, DataTypeName.BIGNUMBER));

    assertThrows(ExpressionException.class,
        () -> Converter.cast(ZonedDateTime.now(), DataTypeName.BIGNUMBER));
  }

  @Test
  public void castToUnknown() throws Exception {
    // assertThrows(ExpressionException.class, () -> Converter.cast(null, DataTypeName.UNKNOWN));
    // assertThrows(ExpressionException.class, () -> Converter.to(true, DataTypeName.UNKNOWN));
    // assertThrows(ExpressionException.class, () -> Converter.to("Test", DataTypeName.UNKNOWN));
    // assertThrows(ExpressionException.class, () -> Converter.to(BigDecimal.ZERO,
    // DataTypeName.UNKNOWN));
  }

}

