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
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.exception.ConversionException;
import org.apache.hop.expression.type.AnyType;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.BooleanType;
import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.IntervalType;
import org.apache.hop.expression.type.JsonType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeFamily;
import org.apache.hop.expression.type.TypeId;
import org.apache.hop.expression.type.UnknownType;
import org.junit.Test;
import java.math.BigDecimal;
import java.math.MathContext;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.time.Month;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Random;
import com.fasterxml.jackson.databind.JsonNode;

public class TypeTest extends ExpressionTest {

  @Test
  public void typeFamily() throws Exception {
    assertTrue(TypeFamily.ANY.isFamily(TypeFamily.BINARY));
    assertTrue(TypeFamily.ANY.isFamily(TypeFamily.BOOLEAN));
    assertTrue(TypeFamily.ANY.isFamily(TypeFamily.NUMERIC));
    assertTrue(TypeFamily.ANY.isFamily(TypeFamily.TEMPORAL));
    assertTrue(TypeFamily.ANY.isFamily(TypeFamily.STRING));
    assertTrue(TypeFamily.ANY.isFamily(TypeFamily.INTERVAL));
        
    assertTrue(TypeFamily.NUMERIC.isFamily(TypeFamily.NUMERIC));
    assertTrue(TypeFamily.BOOLEAN.isFamily(TypeFamily.BOOLEAN));
    assertTrue(TypeFamily.STRING.isFamily(TypeFamily.STRING));
    assertTrue(TypeFamily.TEMPORAL.isFamily(TypeFamily.TEMPORAL));
    
    assertFalse(TypeFamily.NUMERIC.isCompatibleWithCoercion(null));
    assertTrue(TypeFamily.BOOLEAN.isCompatibleWithCoercion(TypeFamily.STRING));
    assertTrue(TypeFamily.NUMERIC.isCompatibleWithCoercion(TypeFamily.STRING));
    
    assertTrue(TypeFamily.NUMERIC.getDataTypeNames().contains(TypeId.INTEGER));
    assertTrue(TypeFamily.NUMERIC.getDataTypeNames().contains(TypeId.NUMBER));
    assertTrue(TypeFamily.TEMPORAL.getDataTypeNames().contains(TypeId.DATE));
  }
    
  @Test
  public void typeNameOf() throws Exception {
    assertEquals(TypeId.ANY, TypeId.of("Any"));    
    assertEquals(TypeId.UNKNOWN, TypeId.of("UNKNOWN"));    
    assertEquals(TypeId.BOOLEAN, TypeId.of("BOOLEAN"));    
    assertEquals(TypeId.BOOLEAN, TypeId.of("Boolean"));   
    assertEquals(TypeId.STRING, TypeId.of("STRING"));
    assertEquals(TypeId.STRING, TypeId.of("String"));
    assertEquals(TypeId.DATE, TypeId.of("DATE"));
    assertEquals(TypeId.NUMBER, TypeId.of("NUMBER"));
    assertEquals(TypeId.BINARY, TypeId.of("BINARY"));
    assertEquals(TypeId.JSON, TypeId.of("Json"));
    assertEquals(TypeId.INTEGER, TypeId.of("INTEGER"));
    assertNull(TypeId.of("NOP"));
  }
  
  @Test
  public void findTypeId() throws Exception {
    assertEquals(TypeId.UNKNOWN, TypeId.fromJavaClass(null)); 
    assertEquals(TypeId.UNKNOWN, TypeId.fromJavaClass(Void.class));
    assertEquals(TypeId.UNKNOWN, TypeId.fromJavaClass(Float.class));
    assertEquals(TypeId.UNKNOWN, TypeId.fromJavaClass(Type.class));
    assertEquals(TypeId.UNKNOWN, TypeId.fromJavaClass(TimeUnit.class));
    assertEquals(TypeId.BOOLEAN, TypeId.fromJavaClass(Boolean.class));
    assertEquals(TypeId.STRING, TypeId.fromJavaClass(String.class));
    assertEquals(TypeId.DATE, TypeId.fromJavaClass(ZonedDateTime.class));
    assertEquals(TypeId.NUMBER, TypeId.fromJavaClass(BigDecimal.class));
    assertEquals(TypeId.BINARY, TypeId.fromJavaClass(byte[].class));
    assertEquals(TypeId.JSON, TypeId.fromJavaClass(JsonNode.class));
    assertEquals(TypeId.INTEGER, TypeId.fromJavaClass(Long.class));
  }

  @Test
  public void valueOf() throws Exception {
    assertEquals(UnknownType.UNKNOWN, Type.valueOf(null));
    assertEquals(UnknownType.UNKNOWN, Type.valueOf(new Random()));    
    assertEquals(BooleanType.BOOLEAN, Type.valueOf(true));
    assertEquals(UnknownType.UNKNOWN, Type.valueOf(TimeUnit.CENTURY));
    assertEquals(new StringType(4), Type.valueOf("test"));
    assertEquals(new BinaryType(1, false), Type.valueOf(new byte[] {0xF}));
    assertEquals(new IntegerType(3), Type.valueOf(123));
    assertEquals(new IntegerType(3), Type.valueOf(123L));
    assertEquals(new NumberType(6,3), Type.valueOf(123.456D));
    assertEquals(new NumberType(18,0), Type.valueOf(new BigDecimal("123456789123456789")));
    assertEquals(new NumberType(9,3), Type.valueOf(BigDecimal.valueOf(123456789,3)));
    assertEquals(DateType.DATE, Type.valueOf(ZonedDateTime.now()));
    assertEquals(IntervalType.INTERVAL, Type.valueOf(Interval.of(5)));
    assertEquals(JsonType.JSON, Type.valueOf(JsonType.convertStringToJson("{\"name\":\"Smith\"}")));

  }

  @Test
  public void isFamily() throws Exception {
    assertTrue(TypeFamily.ANY.isFamily(TypeFamily.ANY));
    assertTrue(TypeFamily.ANY.isFamily(TypeFamily.BINARY));
    assertTrue(TypeFamily.ANY.isFamily(TypeFamily.BOOLEAN));
    assertTrue(TypeFamily.ANY.isFamily(TypeFamily.TEMPORAL));
    
    assertTrue(AnyType.ANY.isFamily(TypeFamily.BINARY));
    assertTrue(AnyType.ANY.isFamily(TypeFamily.BOOLEAN));
    assertTrue(AnyType.ANY.isFamily(TypeFamily.NUMERIC));
    assertTrue(AnyType.ANY.isFamily(TypeFamily.TEMPORAL));
    assertTrue(AnyType.ANY.isFamily(TypeFamily.STRING));
    assertTrue(AnyType.ANY.isFamily(TypeFamily.JSON));
    assertTrue(AnyType.ANY.isFamily(TypeFamily.ANY));
    
    assertTrue(BinaryType.BINARY.isFamily(TypeFamily.ANY));
    assertTrue(BinaryType.BINARY.isFamily(TypeFamily.BINARY));
    assertFalse(BinaryType.BINARY.isFamily(TypeFamily.NUMERIC));    
    assertTrue(BinaryType.BINARY.isFamily(TypeFamily.ANY));
    assertFalse(BinaryType.BINARY.isFamily(TypeFamily.NONE));
    assertTrue(BinaryType.BINARY.isFamily(TypeFamily.BINARY));
        
    assertTrue(BooleanType.BOOLEAN.isFamily(TypeFamily.BOOLEAN));
    assertTrue(DateType.DATE.isFamily(TypeFamily.TEMPORAL));
    assertTrue(IntegerType.INTEGER.isFamily(TypeFamily.NUMERIC));
    assertTrue(NumberType.NUMBER.isFamily(TypeFamily.NUMERIC));
    assertTrue(StringType.STRING.isFamily(TypeFamily.STRING));
        
    assertEquals(TypeFamily.BINARY, BinaryType.BINARY.getFamily());
    assertEquals(TypeFamily.BOOLEAN, BooleanType.BOOLEAN.getFamily());
    assertEquals(TypeFamily.TEMPORAL, DateType.DATE.getFamily());
    assertEquals(TypeFamily.JSON, JsonType.JSON.getFamily());
    assertEquals(TypeFamily.NUMERIC, IntegerType.INTEGER.getFamily());
    assertEquals(TypeFamily.NUMERIC, NumberType.NUMBER.getFamily());
    assertEquals(TypeFamily.STRING, StringType.STRING.getFamily());
  }

  @Test
  public void javaClass() throws Exception {
    assertEquals(byte[].class, TypeId.BINARY.getJavaClass());
    assertEquals(Boolean.class, TypeId.BOOLEAN.getJavaClass());
    assertEquals(Long.class, TypeId.INTEGER.getJavaClass());
    assertEquals(BigDecimal.class, TypeId.NUMBER.getJavaClass());
    assertEquals(String.class, TypeId.STRING.getJavaClass());
    assertEquals(ZonedDateTime.class, TypeId.DATE.getJavaClass());
    assertEquals(JsonNode.class, TypeId.JSON.getJavaClass());
    assertEquals(Void.class, TypeId.UNKNOWN.getJavaClass());
  }

  @Test
  public void signature() throws Exception {
    assertEquals("BOOLEAN", String.valueOf(BooleanType.BOOLEAN));
    assertEquals("DATE", String.valueOf(DateType.DATE));
    assertEquals("JSON", String.valueOf(JsonType.JSON));
    assertEquals("INTEGER", String.valueOf(IntegerType.INTEGER));  
    assertEquals("INTEGER", String.valueOf(new IntegerType()));
    assertEquals("INTEGER(6)", String.valueOf(new IntegerType(6)));
    assertEquals("NUMBER", String.valueOf(NumberType.NUMBER));
    assertEquals("NUMBER(10)", String.valueOf(new NumberType(10)));
    assertEquals("NUMBER(10)", String.valueOf(new NumberType(10,0)));
    assertEquals("NUMBER(10,2)", String.valueOf(new NumberType(10,2)));
    assertEquals("STRING", String.valueOf(StringType.STRING));
    assertEquals("STRING", String.valueOf(new StringType()));
    assertEquals("STRING(10)", String.valueOf(new StringType(10)));
    assertEquals("BINARY", String.valueOf(BinaryType.BINARY));
    assertEquals("BINARY", String.valueOf(new BinaryType()));
    assertEquals("BINARY(10)", String.valueOf(new BinaryType(10)));
  }
  
  @Test
  public void exceptionPrecisionOutOfRange() throws Exception {
    assertThrows(IllegalArgumentException.class, () ->  new IntegerType(100));
  }
    
  @Test
  public void coerceToBoolean() throws Exception {
    assertNull(BooleanType.coerce(null));
    assertTrue(BooleanType.coerce(true));
    assertTrue(BooleanType.coerce(3L));
    assertTrue(BooleanType.coerce(1D));
    assertFalse(BooleanType.coerce(0L));
    assertFalse(BooleanType.coerce(false));
    assertThrows(ConversionException.class, () -> BooleanType.coerce("True"));
    assertThrows(ConversionException.class, () -> BooleanType.coerce(ZonedDateTime.now()));
  }

  @Test
  public void castToBoolean() throws Exception {
    BooleanType type = BooleanType.BOOLEAN;
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
    assertThrows(ConversionException.class, () -> type.cast("3"));
    assertThrows(ConversionException.class, () -> type.cast("MO"));
    assertThrows(ConversionException.class, () -> type.cast("BAD"));
    assertThrows(ConversionException.class, () -> type.cast("TRUL"));
    assertThrows(ConversionException.class, () -> type.cast("FILSE"));
  }

  @Test
  public void convertToBoolean() throws Exception {
    BooleanType type = BooleanType.BOOLEAN;
    assertNull(type.convert(null, Boolean.class));
    assertNull(type.convert(null, Long.class));
    assertNull(type.convert(null, String.class));

    assertEquals(Boolean.TRUE, type.convert(true, Boolean.class));
    assertEquals("TRUE", type.convert(true, String.class));
    assertEquals(Long.valueOf(1), type.convert(true, Long.class));
    assertEquals(BigDecimal.ONE, type.convert(true, BigDecimal.class));
    assertEquals(BigDecimal.ZERO, type.convert(false, BigDecimal.class));
    assertThrows(ConversionException.class, () -> type.convert(true, ZonedDateTime.class));
  }
  
  @Test
  public void coerceToBinary() throws Exception {
    assertNull(BinaryType.coerce(null));
    assertThrows(ConversionException.class, () -> BinaryType.coerce(true));
    assertThrows(ConversionException.class, () -> BinaryType.coerce(3L));
    assertThrows(ConversionException.class, () -> BinaryType.coerce(3D));
  }

  @Test
  public void castToBinary() throws Exception {
    BinaryType type = BinaryType.BINARY;
    assertNull(type.cast(null));
    //assertEquals(new byte[] {0xF, 0xC}, type.cast(new byte[] {0xF, 0xC}));
    assertThrows(ConversionException.class, () ->  type.cast(true));
    assertThrows(ConversionException.class, () ->  type.cast(1L));
    assertThrows(ConversionException.class, () ->  type.cast(1D));
    assertThrows(ConversionException.class, () ->  type.cast(BigDecimal.ONE));
    assertThrows(ConversionException.class, () ->  type.cast(ZonedDateTime.now()));
  }

  @Test
  public void coerceToDate() throws Exception {
    ZonedDateTime date = LocalDate.of(2022, Month.DECEMBER, 28).atStartOfDay().atZone(ZoneId.systemDefault());
   
    assertNull(DateType.coerce(null));
    
    assertThrows(ConversionException.class, () -> DateType.coerce(true));
    assertThrows(ConversionException.class, () -> DateType.coerce("2022"));
  }
  
  @Test
  public void castToDate() throws Exception {
    DateType type = DateType.DATE;
    ZonedDateTime date = LocalDate.of(2022, Month.DECEMBER, 28).atStartOfDay().atZone(ZoneId.systemDefault());
    
    assertNull(type.cast(null));
    assertEquals(date, type.cast(date));
    assertEquals(date, type.cast("2022-12-28"));
    assertEquals(date, type.cast("2022-12-28", "YYYY-MM-DD"));
  }
  

  @Test
  public void convertToDate() throws Exception {
    DateType type = DateType.DATE;
    assertNull(type.convert(null, ZonedDateTime.class));
  }
  
  @Test
  public void coerceToString() throws Exception {
    assertNull(StringType.coerce(null));
    assertEquals("TRUE", StringType.coerce(true));
    assertEquals("FALSE", StringType.coerce(false));
    assertEquals("-1.0", StringType.coerce(-1.0D));
    assertEquals("-1.2", StringType.coerce(-1.2));
    assertEquals("0.1", StringType.coerce(0.1D));
    assertEquals("-0.1", StringType.coerce(-0.1D));
    assertEquals("1", StringType.coerce(BigDecimal.ONE));
  }

  @Test
  public void castToString() throws Exception {
    StringType type = StringType.STRING;
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
  public void convertToString() throws Exception {
    StringType type = StringType.STRING;
    assertNull(type.convert(null, Boolean.class));
    assertNull(type.convert(null, Long.class));
    assertNull(type.convert(null, String.class));

    assertEquals(Boolean.TRUE, type.convert("True", Boolean.class));
    assertEquals("Test", type.convert("Test", String.class));
    assertEquals(Long.valueOf(123), type.convert("123", Long.class));
    assertEquals(BigDecimal.ONE, type.convert("1", BigDecimal.class));
    assertThrows(ConversionException.class, () -> type.convert(true, ZonedDateTime.class));
  }
  
  @Test
  public void coerceToInteger() throws Exception {
    assertNull(IntegerType.coerce(null));
    assertEquals(Long.valueOf(1L), IntegerType.coerce(1));
    assertEquals(Long.valueOf(1L), IntegerType.coerce(1L));
    assertEquals(Long.valueOf(1L), IntegerType.coerce(1.2D));
    assertEquals(Long.valueOf(1L), IntegerType.coerce("1.2"));
    assertEquals(Long.valueOf(-1L), IntegerType.coerce("-1.6"));
    assertEquals(Long.valueOf(1L), IntegerType.coerce(BigDecimal.ONE));
    assertEquals(Long.valueOf(0L), IntegerType.coerce(BigDecimal.ZERO));

    assertThrows(ConversionException.class, () -> IntegerType.coerce(true));
    assertThrows(ConversionException.class, () -> IntegerType.coerce(new byte[] {0xF}));
    assertThrows(ConversionException.class, () -> IntegerType.coerce(ZonedDateTime.now()));
    assertThrows(ConversionException.class, () -> IntegerType.coerce("FALSE"));
  }

  @Test
  public void castToInteger() throws Exception {
    IntegerType type = IntegerType.INTEGER;
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
  public void convertToInteger() throws Exception {
    IntegerType type = IntegerType.INTEGER;
    assertNull(type.convert(null, Long.class));
    assertNull(type.convert(null, BigDecimal.class));
    assertNull(type.convert(null, String.class));

    assertEquals(Boolean.FALSE, type.convert(0L, Boolean.class));
    assertEquals(Boolean.TRUE, type.convert(1L, Boolean.class));
    assertEquals("123", type.convert(123L, String.class));
    assertEquals(Long.valueOf(123), type.convert(123L, Long.class));
    assertEquals(new BigDecimal("123"), type.convert(123L, BigDecimal.class));
    assertThrows(ConversionException.class, () -> type.convert(123L, ZonedDateTime.class));
  }
  
  @Test
  public void coerceToNumber() throws Exception {
    assertNull(NumberType.coerce(null));
    assertEquals(BigDecimal.ZERO, NumberType.coerce(0L));
    assertEquals(BigDecimal.ZERO, NumberType.coerce(0D));
    assertEquals(BigDecimal.ZERO, NumberType.coerce(BigDecimal.ZERO));
    assertEquals(BigDecimal.ZERO, NumberType.coerce("0"));
    assertEquals(BigDecimal.ONE, NumberType.coerce(1L));
    assertEquals(BigDecimal.ONE, NumberType.coerce(1D));
    assertEquals(BigDecimal.ONE, NumberType.coerce(BigDecimal.ONE));
    assertEquals(BigDecimal.ONE, NumberType.coerce("1"));
    assertEquals(new BigDecimal("1.2"), NumberType.coerce("1.2"));
    assertEquals(new BigDecimal("0.1"), NumberType.coerce(".1"));
    assertEquals(new BigDecimal("-2.3E+2"), NumberType.coerce("-2.3E+2"));
    assertEquals(new BigDecimal("-2.3E-2"), NumberType.coerce("-2.3E-2"));
    assertEquals(new BigDecimal("-2.3E-2"), NumberType.coerce("-2.3e-2"));
    assertEquals(new BigDecimal("3.123"), NumberType.coerce(3.123D));

    assertThrows(ConversionException.class, () -> NumberType.coerce(true));
    assertThrows(ConversionException.class, () -> NumberType.coerce(new byte[] {0xF}));
    assertThrows(ConversionException.class, () -> NumberType.coerce(ZonedDateTime.now()));
    assertThrows(ConversionException.class, () -> NumberType.coerce("FALSE"));
  }

  @Test
  public void castToNumber() throws Exception {
    NumberType type = NumberType.NUMBER;
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
    assertEquals(new BigDecimal("0.1"), type.cast("0.1"));
    assertEquals(new BigDecimal("0.1"), type.cast(".1"));
    assertEquals(BigDecimal.ONE, type.cast(new byte[] {0x01}));
    assertEquals(BigDecimal.valueOf(-356L), type.cast(-356L));
    assertEquals(BigDecimal.valueOf(-3.56E2D), type.cast(-3.56E+2D));
    assertEquals(new BigDecimal("0.000"), type.cast("0.000"));
    assertEquals(new BigDecimal("-3.56E2"), type.cast("-3.56E+2"));
    assertEquals(BigDecimal.valueOf(15), type.cast(new byte[] {0xF}));

    assertThrows(ConversionException.class, () -> type.cast(ZonedDateTime.now()));
    assertThrows(ConversionException.class, () -> type.cast("TRUE"));
  }

  @Test
  public void convertToNumber() throws Exception {
    NumberType type = NumberType.NUMBER;
    assertNull(type.convert(null, Long.class));
    assertNull(type.convert(null, BigDecimal.class));
    assertNull(type.convert(null, String.class));

    assertEquals(Boolean.FALSE, type.convert(BigDecimal.ZERO, Boolean.class));
    assertEquals(Boolean.TRUE, type.convert(BigDecimal.ONE, Boolean.class));
    assertEquals("-123.045", type.convert(new BigDecimal("-123.045"), String.class));
    assertEquals(Long.valueOf(-123), type.convert(new BigDecimal("-123.045"), Long.class));
    assertEquals(new BigDecimal("-123.045"), type.convert(new BigDecimal("-123.045"), BigDecimal.class));
    assertThrows(ConversionException.class, () -> type.convert(new BigDecimal("-123.045"), ZonedDateTime.class));
  }
  
  @Test
  public void castToUnknown() throws Exception {
    UnknownType type = UnknownType.UNKNOWN;
    assertThrows(ConversionException.class, () -> type.cast(null));
    assertThrows(ConversionException.class, () -> type.cast(true));
    assertThrows(ConversionException.class, () -> type.cast("Test","MM"));
  }

  @Test
  public void coercionImplicit() throws Exception {
    // Coercion Number
    evalTrue("1::NUMBER = 1::INTEGER");    
    evalTrue("0x1F::NUMBER = 0x1F::INTEGER");
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
    
    // Integer to Number
    evalEquals("'-2e-3'::Number * 2", new BigDecimal("-4e-3", MathContext.DECIMAL32));    
    evalEquals("'-4e-4'::Number * 0.5", new BigDecimal("-0.00020", MathContext.DECIMAL32));
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

