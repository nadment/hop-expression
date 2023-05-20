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
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.type.Converter;
import org.apache.hop.expression.type.DataFamily;
import org.apache.hop.expression.type.DataName;
import org.apache.hop.expression.type.DataType;
import org.junit.Test;
import java.math.BigDecimal;
import java.math.MathContext;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.time.Month;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;
import com.fasterxml.jackson.databind.JsonNode;

public class DataTypeTest extends ExpressionTest {

  @Test
  public void dataName() throws Exception {
    assertEquals(DataName.UNKNOWN, DataName.of("UNKNOWN"));    
    assertEquals(DataName.UNKNOWN, DataName.from(null));
    assertEquals(DataName.UNKNOWN, DataName.from(new Date()));    
    assertEquals(DataName.UNKNOWN, DataName.from(Void.class));
    assertEquals(DataName.UNKNOWN, DataName.from(Float.class));
    
    assertEquals(DataName.BOOLEAN, DataName.of("BOOLEAN"));    
    assertEquals(DataName.BOOLEAN, DataName.of("Boolean"));
    assertEquals(DataName.BOOLEAN, DataName.from(Boolean.class));
    assertEquals(DataName.BOOLEAN, DataName.from(true));
    
    assertEquals(DataName.STRING, DataName.of("STRING"));
    assertEquals(DataName.STRING, DataName.of("String"));
    assertEquals(DataName.STRING, DataName.from(String.class));
    assertEquals(DataName.STRING, DataName.from("test"));
    
    assertEquals(DataName.DATE, DataName.of("DATE"));
    assertEquals(DataName.DATE, DataName.from(ZonedDateTime.class));
    assertEquals(DataName.DATE, DataName.from(ZonedDateTime.now()));  
    
    assertEquals(DataName.NUMBER, DataName.of("NUMBER"));
    assertEquals(DataName.NUMBER, DataName.from(Double.class));
    assertEquals(DataName.NUMBER, DataName.from(1D));
    
    assertEquals(DataName.BIGNUMBER, DataName.of("BIGNUMBER"));
    assertEquals(DataName.BIGNUMBER, DataName.from(BigDecimal.class));
    assertEquals(DataName.BIGNUMBER, DataName.from(BigDecimal.ONE));
    
    assertEquals(DataName.BINARY, DataName.of("BINARY"));
    assertEquals(DataName.BINARY, DataName.from(byte[].class));
    assertEquals(DataName.BINARY, DataName.from(new byte[] {0x78}));
    
    assertEquals(DataName.JSON, DataName.of("Json"));
    assertEquals(DataName.JSON, DataName.from(JsonNode.class));
    assertEquals(DataName.JSON, DataName.from(Converter.parseJson("{\"name\":\"Smith\"}")));
    
    assertEquals(DataName.INTEGER, DataName.of("INTEGER"));
    assertEquals(DataName.INTEGER, DataName.from(Long.class));
    assertEquals(DataName.INTEGER, DataName.from(1L));
    
    assertEquals(DataName.TIMEUNIT, DataName.of("TIMEUNIT"));
    assertEquals(DataName.TIMEUNIT, DataName.from(TimeUnit.class));
    assertEquals(DataName.TIMEUNIT, DataName.from(TimeUnit.CENTURY));
    
    assertNull(DataName.of("NOP"));
  }

  @Test
  public void dataTypeOf() throws Exception {
    assertEquals(DataType.UNKNOWN, DataType.of(null));
    assertEquals(DataType.UNKNOWN, DataType.of(DataFamily.BOOLEAN));
    assertEquals(DataType.BOOLEAN, DataType.of(true));
    assertEquals(DataType.STRING, DataType.of("test"));
    assertEquals(DataType.NUMBER, DataType.of(123.456D));
    assertEquals(new DataType(DataName.NUMBER,38,0), DataType.of(123.456D));
    assertEquals(new DataType(DataName.BIGNUMBER,9,3), DataType.of(BigDecimal.valueOf(123456123,3)));
    assertEquals(DataType.DATE, DataType.of(ZonedDateTime.now()));
    assertEquals(DataType.TIMEUNIT, DataType.of(TimeUnit.CENTURY));
  }


  @Test
  public void family() throws Exception {
    assertTrue(DataFamily.ANY.isSameFamily(DataFamily.BINARY));
    assertTrue(DataType.BINARY.isSameFamily(DataFamily.ANY));
    
    assertTrue(DataType.BINARY.isSameFamily(DataFamily.BINARY));
    assertFalse(DataType.BINARY.isSameFamily(DataFamily.NUMERIC));
    
    assertTrue(DataType.BINARY.isSameFamily(DataFamily.ANY));
    assertFalse(DataType.BINARY.isSameFamily(DataFamily.NONE));
    
    assertTrue(DataType.BINARY.isSameFamily(DataFamily.BINARY));
    assertTrue(DataType.BOOLEAN.isSameFamily(DataFamily.BOOLEAN));
    assertTrue(DataType.DATE.isSameFamily(DataFamily.TEMPORAL));
    assertTrue(DataType.INTEGER.isSameFamily(DataFamily.NUMERIC));
    assertTrue(DataType.NUMBER.isSameFamily(DataFamily.NUMERIC));
    assertTrue(DataType.BIGNUMBER.isSameFamily(DataFamily.NUMERIC));
    assertTrue(DataType.STRING.isSameFamily(DataFamily.STRING));
    
    assertEquals(DataFamily.BINARY, DataType.BINARY.getFamily());
    assertEquals(DataFamily.BOOLEAN, DataType.BOOLEAN.getFamily());
    assertEquals(DataFamily.TEMPORAL, DataType.DATE.getFamily());
    assertEquals(DataFamily.JSON, DataType.JSON.getFamily());
    assertEquals(DataFamily.NUMERIC, DataType.INTEGER.getFamily());
    assertEquals(DataFamily.NUMERIC, DataType.NUMBER.getFamily());
    assertEquals(DataFamily.NUMERIC, DataType.BIGNUMBER.getFamily());
    assertEquals(DataFamily.STRING, DataType.STRING.getFamily());
  }

  @Test
  public void javaClass() throws Exception {
    assertEquals(byte[].class, DataName.BINARY.getJavaClass());
    assertEquals(Boolean.class, DataName.BOOLEAN.getJavaClass());
    assertEquals(Long.class, DataName.INTEGER.getJavaClass());
    assertEquals(Double.class, DataName.NUMBER.getJavaClass());
    assertEquals(BigDecimal.class, DataName.BIGNUMBER.getJavaClass());
    assertEquals(String.class, DataName.STRING.getJavaClass());
    assertEquals(ZonedDateTime.class, DataName.DATE.getJavaClass());
    assertEquals(JsonNode.class, DataName.JSON.getJavaClass());
    assertEquals(Void.class, DataName.UNKNOWN.getJavaClass());
  }

  @Test
  public void coerceToBoolean() throws Exception {
    assertNull(Converter.coerceToBoolean(null));
    assertTrue(Converter.coerceToBoolean(true));
    assertTrue(Converter.coerceToBoolean(3L));
    assertTrue(Converter.coerceToBoolean(1D));
    assertFalse(Converter.coerceToBoolean(0L));
    assertFalse(Converter.coerceToBoolean(false));
    assertThrows(IllegalArgumentException.class, () -> Converter.coerceToBoolean("True"));
    assertThrows(IllegalArgumentException.class, () -> Converter.coerceToBoolean(ZonedDateTime.now()));
  }

  @Test
  public void castToBoolean() throws Exception {
    assertNull(Converter.cast(null, DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast(3L, DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast(1L, DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast(1D, DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast(true, DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast(BigDecimal.ONE, DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast("1", DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast("T", DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast("True", DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast("Y", DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast("Yes", DataType.BOOLEAN));
    assertEquals(Boolean.TRUE, Converter.cast("ON", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.cast(0L, DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.cast(0D, DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.cast(false, DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.cast(BigDecimal.ZERO, DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.cast("0", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.cast("F", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.cast("False", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.cast("N", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.cast("No", DataType.BOOLEAN));
    assertEquals(Boolean.FALSE, Converter.cast("Off", DataType.BOOLEAN));
    assertThrows(IllegalArgumentException.class, () -> Converter.cast("3", DataType.BOOLEAN));
    assertThrows(IllegalArgumentException.class, () -> Converter.cast("MO", DataType.BOOLEAN));
    assertThrows(IllegalArgumentException.class, () -> Converter.cast("BAD", DataType.BOOLEAN));
    assertThrows(IllegalArgumentException.class, () -> Converter.cast("TRUL", DataType.BOOLEAN));
    assertThrows(IllegalArgumentException.class, () -> Converter.cast("FILSE", DataType.BOOLEAN));
  }

  @Test
  public void coerceToBinary() throws Exception {
    assertNull(Converter.coerceToBinary(null));
    assertThrows(IllegalArgumentException.class, () -> Converter.coerceToBinary(true));
    assertThrows(IllegalArgumentException.class, () -> Converter.coerceToBinary(3L));
    assertThrows(IllegalArgumentException.class, () -> Converter.coerceToBinary(3D));
  }

  @Test
  public void castToBinary() throws Exception {
    assertNull(Converter.cast(null, DataType.BINARY));
    //assertEquals(new byte[] {0xF, 0xC}, Converter.cast(new byte[] {0xF, 0xC}, DataType.BINARY));

    assertThrows(IllegalArgumentException.class, () ->  Converter.cast(true, DataType.BINARY));
    assertThrows(IllegalArgumentException.class, () ->  Converter.cast(1L, DataType.BINARY));
    assertThrows(IllegalArgumentException.class, () ->  Converter.cast(1D, DataType.BINARY));
    assertThrows(IllegalArgumentException.class, () ->  Converter.cast(BigDecimal.ONE, DataType.BINARY));
    assertThrows(IllegalArgumentException.class, () ->  Converter.cast(ZonedDateTime.now(), DataType.BINARY));
  }

  @Test
  public void coerceToZonedDateTime() throws Exception {
    ZonedDateTime date = LocalDate.of(2022, Month.DECEMBER, 28).atStartOfDay().atZone(ZoneId.systemDefault());
   
    assertNull(Converter.coerceToDate(null));

    
    assertThrows(IllegalArgumentException.class, () -> Converter.coerceToDate(true));
    assertThrows(IllegalArgumentException.class, () -> Converter.coerceToDate("2022"));
  }
  
  @Test
  public void castToDate() throws Exception {
    ZonedDateTime date = LocalDate.of(2022, Month.DECEMBER, 28).atStartOfDay().atZone(ZoneId.systemDefault());
    
    assertNull(Converter.cast(null, DataType.DATE));
    assertEquals(date, Converter.cast("2022-12-28", DataType.DATE));
    assertEquals(date, Converter.cast("2022-12-28", DataType.DATE, "YYYY-MM-DD"));
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
    assertNull(Converter.cast(null, DataType.STRING));
    assertEquals("TRUE", Converter.cast(true, DataType.STRING));
    assertEquals("FALSE", Converter.cast(false, DataType.STRING));
    assertEquals("0", Converter.cast(0L, DataType.STRING));
    assertEquals("1", Converter.cast(1L, DataType.STRING));
    assertEquals("0", Converter.cast(0D, DataType.STRING));
    assertEquals("1.2", Converter.cast(1.2D, DataType.STRING));
    assertEquals("0", Converter.cast(BigDecimal.ZERO, DataType.STRING));
    assertEquals("1", Converter.cast(BigDecimal.ONE, DataType.STRING));
    assertEquals("ABCD��", Converter.cast("ABCD��".getBytes(StandardCharsets.UTF_8), DataType.STRING));
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

    assertThrows(IllegalArgumentException.class, () -> Converter.coerceToInteger(true));
    assertThrows(IllegalArgumentException.class, () -> Converter.coerceToInteger(new byte[] {0xF}));
    assertThrows(IllegalArgumentException.class, () -> Converter.coerceToInteger(ZonedDateTime.now()));
    assertThrows(IllegalArgumentException.class, () -> Converter.coerceToInteger("FALSE"));
  }

  @Test
  public void castToInteger() throws Exception {
    assertNull(Converter.cast(null, DataType.INTEGER));
    assertEquals(1L, Converter.cast(true, DataType.INTEGER));
    assertEquals(0L, Converter.cast(false, DataType.INTEGER));
    assertEquals(0L, Converter.cast(0L, DataType.INTEGER));
    assertEquals(3L, Converter.cast(3L, DataType.INTEGER));
    assertEquals(0L, Converter.cast(0.0D, DataType.INTEGER));
    assertEquals(3L, Converter.cast(3.3D, DataType.INTEGER));
    assertEquals(0L, Converter.cast(BigDecimal.ZERO, DataType.INTEGER));
    assertEquals(1L, Converter.cast(BigDecimal.ONE, DataType.INTEGER));
    assertEquals(3L, Converter.cast(BigDecimal.valueOf(3.125), DataType.INTEGER));
    assertEquals(5L, Converter.cast("5.9", DataType.INTEGER));
    assertEquals(-5L, Converter.cast("-5.2", DataType.INTEGER));
    assertEquals(15L, Converter.cast(new byte[] {0xF}, DataType.INTEGER));
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

    assertThrows(IllegalArgumentException.class, () -> Converter.coerceToNumber(true));
    assertThrows(IllegalArgumentException.class, () -> Converter.coerceToNumber(new byte[] {0xF}));
    assertThrows(IllegalArgumentException.class, () -> Converter.coerceToNumber(ZonedDateTime.now()));
    assertThrows(IllegalArgumentException.class, () -> Converter.coerceToNumber("FALSE"));
  }

  @Test
  public void castToNumber() throws Exception {
    assertNull(Converter.cast(null, DataType.NUMBER));
    assertEquals(Double.valueOf(1D), Converter.cast(true, DataType.NUMBER));
    assertEquals(Double.valueOf(0D), Converter.cast(false, DataType.NUMBER));
    assertEquals(Double.valueOf(0D), Converter.cast(0L, DataType.NUMBER));
    assertEquals(Double.valueOf(3D), Converter.cast(3L, DataType.NUMBER));
    assertEquals(Double.valueOf(1D), Converter.cast(BigDecimal.ONE, DataType.NUMBER));
    assertEquals(Double.valueOf(0D), Converter.cast(BigDecimal.ZERO, DataType.NUMBER));
    assertEquals(Double.valueOf(0.5D), Converter.cast("0.5", DataType.NUMBER));
    assertEquals(Double.valueOf(0.5D), Converter.cast(".5", DataType.NUMBER));
    assertEquals(Double.valueOf(-2.3E+2D), Converter.cast(-2.3E+2D, DataType.NUMBER));
    assertEquals(15D, Converter.cast(new byte[] {0xF}, DataType.NUMBER));

    assertThrows(IllegalArgumentException.class,
        () -> Converter.cast(ZonedDateTime.now(), DataType.NUMBER));
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

    assertThrows(IllegalArgumentException.class, () -> Converter.coerceToBigNumber(true));
    assertThrows(IllegalArgumentException.class, () -> Converter.coerceToBigNumber(new byte[] {0xF}));
    assertThrows(IllegalArgumentException.class, () -> Converter.coerceToBigNumber(ZonedDateTime.now()));
    assertThrows(IllegalArgumentException.class, () -> Converter.coerceToBigNumber("FALSE"));
  }

  @Test
  public void castToBigNumber() throws Exception {
    assertNull(Converter.cast(null, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, Converter.cast(false, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, Converter.cast(0L, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, Converter.cast(0D, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, Converter.cast("0", DataType.BIGNUMBER));
    assertEquals(BigDecimal.ZERO, Converter.cast(new byte[] {0x00}, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ONE, Converter.cast(true, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ONE, Converter.cast(1L, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ONE, Converter.cast(1D, DataType.BIGNUMBER));
    assertEquals(BigDecimal.ONE, Converter.cast("1", DataType.BIGNUMBER));
    assertEquals(BigDecimal.ONE, Converter.cast(new byte[] {0x01}, DataType.BIGNUMBER));
    assertEquals(BigDecimal.valueOf(-356L), Converter.cast(-356L, DataType.BIGNUMBER));
    assertEquals(BigDecimal.valueOf(-3.56E2D), Converter.cast(-3.56E+2D, DataType.BIGNUMBER));
    assertEquals(new BigDecimal("0.000"), Converter.cast("0.000", DataType.BIGNUMBER));
    assertEquals(new BigDecimal("-3.56E2"), Converter.cast("-3.56E+2", DataType.BIGNUMBER));
    assertEquals(BigDecimal.valueOf(15), Converter.cast(new byte[] {0xF}, DataType.BIGNUMBER));

    assertThrows(IllegalArgumentException.class,
        () -> Converter.cast(ZonedDateTime.now(), DataType.BIGNUMBER));
  }

  @Test
  public void castToUnknown() throws Exception {
    // assertThrows(IllegalArgumentException.class, () -> Converter.cast(null, DataType.UNKNOWN));
    // assertThrows(IllegalArgumentException.class, () -> Converter.to(true, DataType.UNKNOWN));
    // assertThrows(IllegalArgumentException.class, () -> Converter.to("Test", DataType.UNKNOWN));
    // assertThrows(IllegalArgumentException.class, () -> Converter.to(BigDecimal.ZERO,
    // DataTypeName.UNKNOWN));
  }

  @Test
  public void coercionImplicit() throws Exception {
    // Coercion Number
    evalTrue("1::BIGNUMBER = 1::INTEGER");    
    evalTrue("0::BIGNUMBER = 0::NUMBER");
    evalTrue("1::NUMBER = 1::INTEGER");
            
    // String to Number
    evalTrue("'1.25' = 1.25::NUMBER");
    evalEquals("2.0*'1.23'", 2.46D);
    evalEquals("2+'2'", 4L);
    evalEquals("'2'+2", 4L);
    evalEquals("2 + 2 || 2", "42");
    evalEquals(" 4 + 4 || '2' ", "82");    
    evalEquals(" '8' || 1 + 1", 82L);
    
    // Integer to BigNumber
    evalEquals("'-1e-3'::BigNumber * 2", new BigDecimal("-2e-3", MathContext.DECIMAL128));    
    // Number to BigNumber
    evalEquals("'-1e-3'::BigNumber * 0.5", new BigDecimal("-5e-4", MathContext.DECIMAL128));
  }

  @Test
  public void coercionExplicit() throws Exception {
    // Cast String to Boolean
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
    
    // String to BigNumber
    evalEquals("' -1e-3 '::BigNumber", new BigDecimal("-1e-3", MathContext.DECIMAL128));    
  }
}

