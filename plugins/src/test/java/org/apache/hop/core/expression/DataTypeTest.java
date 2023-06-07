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
import org.apache.hop.expression.type.BinaryDataType;
import org.apache.hop.expression.type.BooleanDataType;
import org.apache.hop.expression.type.DataFamily;
import org.apache.hop.expression.type.DataName;
import org.apache.hop.expression.type.DataType;
import org.apache.hop.expression.type.DateDataType;
import org.apache.hop.expression.type.IntegerDataType;
import org.apache.hop.expression.type.JsonDataType;
import org.apache.hop.expression.type.NumberDataType;
import org.apache.hop.expression.type.StringDataType;
import org.apache.hop.expression.type.UnknownDataType;
import org.junit.Test;
import java.math.BigDecimal;
import java.math.MathContext;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.time.Month;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;
import java.util.Random;
import com.fasterxml.jackson.databind.JsonNode;

public class DataTypeTest extends ExpressionTest {

  @Test
  public void dataName() throws Exception {
    assertEquals(DataName.ANY, DataName.of("Any"));
    
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
    assertEquals(DataName.NUMBER, DataName.from(BigDecimal.class));
    assertEquals(DataName.NUMBER, DataName.from(BigDecimal.ONE));
    
    assertEquals(DataName.BINARY, DataName.of("BINARY"));
    assertEquals(DataName.BINARY, DataName.from(byte[].class));
    assertEquals(DataName.BINARY, DataName.from(new byte[] {0x78}));
    
    assertEquals(DataName.JSON, DataName.of("Json"));
    assertEquals(DataName.JSON, DataName.from(JsonNode.class));
    assertEquals(DataName.JSON, DataName.from(JsonDataType.convert("{\"name\":\"Smith\"}")));
    
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
    assertEquals(UnknownDataType.UNKNOWN, DataType.of(null));
    assertEquals(UnknownDataType.UNKNOWN, DataType.of(new Random()));    
    assertEquals(BooleanDataType.BOOLEAN, DataType.of(true));
    assertEquals(StringDataType.STRING, DataType.of("test"));
    assertEquals(NumberDataType.NUMBER, DataType.of(123.456D));
    //assertEquals(new DataType(DataName.NUMBER,38,0), DataType.of(123.456D));
    assertEquals(new NumberDataType(9,3), DataType.of(BigDecimal.valueOf(123456123,3)));
    assertEquals(DateDataType.DATE, DataType.of(ZonedDateTime.now()));
    assertEquals(UnknownDataType.UNKNOWN, DataType.of(TimeUnit.CENTURY));
  }


  @Test
  public void family() throws Exception {
    assertTrue(DataFamily.ANY.isSameFamily(DataFamily.ANY));
    assertTrue(DataFamily.ANY.isSameFamily(DataFamily.BINARY));
    assertTrue(DataFamily.ANY.isSameFamily(DataFamily.BOOLEAN));
    assertTrue(DataFamily.ANY.isSameFamily(DataFamily.TEMPORAL));
    
    assertTrue(UnknownDataType.ANY.isSameFamily(DataFamily.BINARY));
    assertTrue(UnknownDataType.ANY.isSameFamily(DataFamily.BOOLEAN));
    assertTrue(UnknownDataType.ANY.isSameFamily(DataFamily.TEMPORAL));
    assertTrue(UnknownDataType.ANY.isSameFamily(DataFamily.ANY));
    
    assertTrue(BinaryDataType.BINARY.isSameFamily(DataFamily.ANY));
    assertTrue(BinaryDataType.BINARY.isSameFamily(DataFamily.BINARY));
    assertFalse(BinaryDataType.BINARY.isSameFamily(DataFamily.NUMERIC));    
    assertTrue(BinaryDataType.BINARY.isSameFamily(DataFamily.ANY));
    assertFalse(BinaryDataType.BINARY.isSameFamily(DataFamily.NONE));
    assertTrue(BinaryDataType.BINARY.isSameFamily(DataFamily.BINARY));
        
    assertTrue(BooleanDataType.BOOLEAN.isSameFamily(DataFamily.BOOLEAN));
    assertTrue(DateDataType.DATE.isSameFamily(DataFamily.TEMPORAL));
    assertTrue(IntegerDataType.INTEGER.isSameFamily(DataFamily.NUMERIC));
    assertTrue(NumberDataType.NUMBER.isSameFamily(DataFamily.NUMERIC));
    assertTrue(StringDataType.STRING.isSameFamily(DataFamily.STRING));
        
    assertEquals(DataFamily.BINARY, BinaryDataType.BINARY.getFamily());
    assertEquals(DataFamily.BOOLEAN, BooleanDataType.BOOLEAN.getFamily());
    assertEquals(DataFamily.TEMPORAL, DateDataType.DATE.getFamily());
    assertEquals(DataFamily.JSON, JsonDataType.JSON.getFamily());
    assertEquals(DataFamily.NUMERIC, IntegerDataType.INTEGER.getFamily());
    assertEquals(DataFamily.NUMERIC, NumberDataType.NUMBER.getFamily());
    assertEquals(DataFamily.STRING, StringDataType.STRING.getFamily());
  }

  @Test
  public void javaClass() throws Exception {
    assertEquals(byte[].class, DataName.BINARY.getJavaClass());
    assertEquals(Boolean.class, DataName.BOOLEAN.getJavaClass());
    assertEquals(Long.class, DataName.INTEGER.getJavaClass());
    //assertEquals(Double.class, DataName.NUMBER.getJavaClass());
    assertEquals(BigDecimal.class, DataName.NUMBER.getJavaClass());
    assertEquals(String.class, DataName.STRING.getJavaClass());
    assertEquals(ZonedDateTime.class, DataName.DATE.getJavaClass());
    assertEquals(JsonNode.class, DataName.JSON.getJavaClass());
    assertEquals(Void.class, DataName.UNKNOWN.getJavaClass());
  }

  @Test
  public void signature() throws Exception {
    assertEquals("BOOLEAN", String.valueOf(BooleanDataType.BOOLEAN));
    assertEquals("DATE", String.valueOf(DateDataType.DATE));
    assertEquals("JSON", String.valueOf(JsonDataType.JSON));
    assertEquals("INTEGER", String.valueOf(IntegerDataType.INTEGER));    
    assertEquals("NUMBER", String.valueOf(NumberDataType.NUMBER));
    assertEquals("NUMBER(10)", String.valueOf(new NumberDataType(10)));
    assertEquals("NUMBER(10,2)", String.valueOf(new NumberDataType(10,2)));
    assertEquals("STRING", String.valueOf(StringDataType.STRING));
    assertEquals("STRING(10)", String.valueOf(new StringDataType(10)));
    assertEquals("BINARY", String.valueOf(new BinaryDataType()));
    assertEquals("BINARY(10)", String.valueOf(new BinaryDataType(10)));
  }
  
  @Test
  public void coerceToBoolean() throws Exception {
    assertNull(BooleanDataType.coerce(null));
    assertTrue(BooleanDataType.coerce(true));
    assertTrue(BooleanDataType.coerce(3L));
    assertTrue(BooleanDataType.coerce(1D));
    assertFalse(BooleanDataType.coerce(0L));
    assertFalse(BooleanDataType.coerce(false));
    assertThrows(IllegalArgumentException.class, () -> BooleanDataType.coerce("True"));
    assertThrows(IllegalArgumentException.class, () -> BooleanDataType.coerce(ZonedDateTime.now()));
  }

  @Test
  public void castToBoolean() throws Exception {
    BooleanDataType type = BooleanDataType.BOOLEAN;
    assertNull(type.cast(null));
    assertEquals(Boolean.TRUE, type.cast(3L));
    assertEquals(Boolean.TRUE, type.cast(1L));
    assertEquals(Boolean.TRUE, type.cast(1D));
    assertEquals(Boolean.TRUE, type.cast(true));
    assertEquals(Boolean.TRUE, type.cast(BigDecimal.ONE));
    assertEquals(Boolean.TRUE, type.cast("1"));
    assertEquals(Boolean.TRUE, type.cast("T"));
    assertEquals(Boolean.TRUE, type.cast("True"));
    assertEquals(Boolean.TRUE, type.cast("Y"));
    assertEquals(Boolean.TRUE, type.cast("Yes"));
    assertEquals(Boolean.TRUE, type.cast("ON"));
    assertEquals(Boolean.FALSE, type.cast(0L));
    assertEquals(Boolean.FALSE, type.cast(0D));
    assertEquals(Boolean.FALSE, type.cast(false));
    assertEquals(Boolean.FALSE, type.cast(BigDecimal.ZERO));
    assertEquals(Boolean.FALSE, type.cast("0"));
    assertEquals(Boolean.FALSE, type.cast("F"));
    assertEquals(Boolean.FALSE, type.cast("False"));
    assertEquals(Boolean.FALSE, type.cast("N"));
    assertEquals(Boolean.FALSE, type.cast("No"));
    assertEquals(Boolean.FALSE, type.cast("Off"));
    assertThrows(IllegalArgumentException.class, () -> type.cast("3"));
    assertThrows(IllegalArgumentException.class, () -> type.cast("MO"));
    assertThrows(IllegalArgumentException.class, () -> type.cast("BAD"));
    assertThrows(IllegalArgumentException.class, () -> type.cast("TRUL"));
    assertThrows(IllegalArgumentException.class, () -> type.cast("FILSE"));
  }

  @Test
  public void coerceToBinary() throws Exception {
    assertNull(BinaryDataType.coerce(null));
    assertThrows(IllegalArgumentException.class, () -> BinaryDataType.coerce(true));
    assertThrows(IllegalArgumentException.class, () -> BinaryDataType.coerce(3L));
    assertThrows(IllegalArgumentException.class, () -> BinaryDataType.coerce(3D));
  }

  @Test
  public void castToBinary() throws Exception {
    BinaryDataType type = BinaryDataType.BINARY;
    assertNull(type.cast(null));
    //assertEquals(new byte[] {0xF, 0xC}, type.cast(new byte[] {0xF, 0xC}));
    assertThrows(IllegalArgumentException.class, () ->  type.cast(true));
    assertThrows(IllegalArgumentException.class, () ->  type.cast(1L));
    assertThrows(IllegalArgumentException.class, () ->  type.cast(1D));
    assertThrows(IllegalArgumentException.class, () ->  type.cast(BigDecimal.ONE));
    assertThrows(IllegalArgumentException.class, () ->  type.cast(ZonedDateTime.now()));
  }

  @Test
  public void coerceToZonedDateTime() throws Exception {
    ZonedDateTime date = LocalDate.of(2022, Month.DECEMBER, 28).atStartOfDay().atZone(ZoneId.systemDefault());
   
    assertNull(DateDataType.coerce(null));

    
    assertThrows(IllegalArgumentException.class, () -> DateDataType.coerce(true));
    assertThrows(IllegalArgumentException.class, () -> DateDataType.coerce("2022"));
  }
  
  @Test
  public void castToDate() throws Exception {
    DateDataType type = DateDataType.DATE;
    ZonedDateTime date = LocalDate.of(2022, Month.DECEMBER, 28).atStartOfDay().atZone(ZoneId.systemDefault());
    
    assertNull(type.cast(null));
    assertEquals(date, type.cast("2022-12-28"));
    assertEquals(date, type.cast("2022-12-28", "YYYY-MM-DD"));
  }
  
  @Test
  public void coerceToString() throws Exception {
    assertNull(StringDataType.coerce(null));
    assertEquals("TRUE", StringDataType.coerce(true));
    assertEquals("FALSE", StringDataType.coerce(false));
    assertEquals("-1.0", StringDataType.coerce(-1.0D));
    assertEquals("-1.2", StringDataType.coerce(-1.2));
    assertEquals("0.1", StringDataType.coerce(0.1D));
    assertEquals("-0.1", StringDataType.coerce(-0.1D));
    assertEquals("1", StringDataType.coerce(BigDecimal.ONE));
  }

  @Test
  public void castToString() throws Exception {
    StringDataType type = StringDataType.STRING;
    assertNull(type.cast(null));
    assertEquals("TRUE", type.cast(true));
    assertEquals("FALSE", type.cast(false));
    assertEquals("0", type.cast(0L));
    assertEquals("1", type.cast(1L));
    assertEquals("0", type.cast(0D));
    assertEquals("1.2", type.cast(1.2D));
    assertEquals("0", type.cast(BigDecimal.ZERO));
    assertEquals("1", type.cast(BigDecimal.ONE));
    assertEquals("ABCD��", type.cast("ABCD��".getBytes(StandardCharsets.UTF_8)));
  }

  @Test
  public void coerceToInteger() throws Exception {
    assertNull(IntegerDataType.coerce(null));
    assertEquals(Long.valueOf(1L), IntegerDataType.coerce(1));
    assertEquals(Long.valueOf(1L), IntegerDataType.coerce(1L));
    assertEquals(Long.valueOf(1L), IntegerDataType.coerce(1.2D));
    assertEquals(Long.valueOf(1L), IntegerDataType.coerce("1.2"));
    assertEquals(Long.valueOf(-1L), IntegerDataType.coerce("-1.6"));
    assertEquals(Long.valueOf(1L), IntegerDataType.coerce(BigDecimal.ONE));
    assertEquals(Long.valueOf(0L), IntegerDataType.coerce(BigDecimal.ZERO));

    assertThrows(IllegalArgumentException.class, () -> IntegerDataType.coerce(true));
    assertThrows(IllegalArgumentException.class, () -> IntegerDataType.coerce(new byte[] {0xF}));
    assertThrows(IllegalArgumentException.class, () -> IntegerDataType.coerce(ZonedDateTime.now()));
    assertThrows(IllegalArgumentException.class, () -> IntegerDataType.coerce("FALSE"));
  }

  @Test
  public void castToInteger() throws Exception {
    IntegerDataType type = IntegerDataType.INTEGER;
    assertNull(type.cast(null));
    assertEquals(Long.valueOf(1L), type.cast(true));
    assertEquals(Long.valueOf(0L), type.cast(false));
    assertEquals(Long.valueOf(0L), type.cast(0L));
    assertEquals(Long.valueOf(3L), type.cast(3L));
    assertEquals(Long.valueOf(0L), type.cast(BigDecimal.ZERO));
    assertEquals(Long.valueOf(1L), type.cast(BigDecimal.ONE));
    assertEquals(Long.valueOf(3L), type.cast(BigDecimal.valueOf(3.125)));
    assertEquals(Long.valueOf(5L), type.cast("5.9"));
    assertEquals(Long.valueOf(-5L), type.cast("-5.2"));
    assertEquals(Long.valueOf(15L), type.cast(new byte[] {0xF}));
  }

  @Test
  public void coerceToBigNumber() throws Exception {
    assertNull(NumberDataType.coerce(null));

    assertEquals(BigDecimal.ZERO, NumberDataType.coerce(0L));
    assertEquals(BigDecimal.ZERO, NumberDataType.coerce(0D));
    assertEquals(BigDecimal.ZERO, NumberDataType.coerce(BigDecimal.ZERO));
    assertEquals(BigDecimal.ZERO, NumberDataType.coerce("0"));
    assertEquals(BigDecimal.ONE, NumberDataType.coerce(1L));
    assertEquals(BigDecimal.ONE, NumberDataType.coerce(1D));
    assertEquals(BigDecimal.ONE, NumberDataType.coerce(BigDecimal.ONE));
    assertEquals(BigDecimal.ONE, NumberDataType.coerce("1"));
    assertEquals(new BigDecimal("1.2"), NumberDataType.coerce("1.2"));
    assertEquals(new BigDecimal("0.1"), NumberDataType.coerce(".1"));
    assertEquals(new BigDecimal("-2.3E+2"), NumberDataType.coerce("-2.3E+2"));
    assertEquals(new BigDecimal("-2.3E-2"), NumberDataType.coerce("-2.3E-2"));
    assertEquals(new BigDecimal("-2.3E-2"), NumberDataType.coerce("-2.3e-2"));
    assertEquals(new BigDecimal("3.123"), NumberDataType.coerce(3.123D));

    assertThrows(IllegalArgumentException.class, () -> NumberDataType.coerce(true));
    assertThrows(IllegalArgumentException.class, () -> NumberDataType.coerce(new byte[] {0xF}));
    assertThrows(IllegalArgumentException.class, () -> NumberDataType.coerce(ZonedDateTime.now()));
    assertThrows(IllegalArgumentException.class, () -> NumberDataType.coerce("FALSE"));
  }

  @Test
  public void castToNumber() throws Exception {
    NumberDataType type = NumberDataType.NUMBER;
    assertNull(type.cast(null));
    assertEquals(BigDecimal.ZERO, type.cast(false));
    assertEquals(BigDecimal.ZERO, type.cast(0L));
    assertEquals(BigDecimal.ZERO, type.cast(0D));
    assertEquals(BigDecimal.ZERO, type.cast("0"));
    assertEquals(BigDecimal.ZERO, type.cast(new byte[] {0x00}));
    assertEquals(BigDecimal.ONE, type.cast(true));
    assertEquals(BigDecimal.ONE, type.cast(1L));
    assertEquals(BigDecimal.ONE, type.cast(1D));
    assertEquals(BigDecimal.ONE, type.cast("1"));
    assertEquals(BigDecimal.ONE, type.cast(new byte[] {0x01}));
    assertEquals(BigDecimal.valueOf(-356L), type.cast(-356L));
    assertEquals(BigDecimal.valueOf(-3.56E2D), type.cast(-3.56E+2D));
    assertEquals(new BigDecimal("0.000"), type.cast("0.000"));
    assertEquals(new BigDecimal("-3.56E2"), type.cast("-3.56E+2"));
    assertEquals(BigDecimal.valueOf(15), type.cast(new byte[] {0xF}));

    assertThrows(IllegalArgumentException.class, () -> type.cast(ZonedDateTime.now()));
  }

  @Test
  public void castToUnknown() throws Exception {
  //  UnknownDataType type = UnknownDataType.UNKNOWN;
  //  assertThrows(IllegalArgumentException.class, () -> type.cast(null));
//    assertThrows(IllegalArgumentException.class, () -> type.cast(true));
  //  assertThrows(IllegalArgumentException.class, () -> type.cast("Test"));
  }

  @Test
  public void coercionImplicit() throws Exception {
    // Coercion Number
    evalTrue("1::NUMBER = 1::INTEGER");    
    evalTrue("0::NUMBER = 0::NUMBER");
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
    evalEquals("'-1e-3'::Number * 2", new BigDecimal("-2e-3", MathContext.DECIMAL32));    
    // Number to BigNumber
    evalEquals("'-1e-3'::Number * 0.5", new BigDecimal("-5e-4", MathContext.DECIMAL32));
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
    evalEquals("' -1e-3 '::Number", new BigDecimal("-1e-3", MathContext.DECIMAL32));    
  }
}

