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

package org.apache.hop.expression;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.fasterxml.jackson.databind.JsonNode;
import java.math.BigDecimal;
import java.net.InetAddress;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.time.Month;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Random;
import org.apache.hop.expression.type.ArrayType;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.BooleanType;
import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.Interval;
import org.apache.hop.expression.type.JsonType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeFamily;
import org.apache.hop.expression.type.TypeId;
import org.apache.hop.expression.type.Types;
import org.apache.hop.expression.type.UnknownType;
import org.apache.hop.expression.util.DateTimeParseException;
import org.junit.jupiter.api.Test;

public class TypeTest extends ExpressionTest {

  @Test
  void isCoercible() throws Exception {
    assertFalse(TypeFamily.NUMERIC.isCoercible(null));
    assertTrue(TypeFamily.BOOLEAN.isCoercible(TypeFamily.STRING));
    assertTrue(TypeFamily.NUMERIC.isCoercible(TypeFamily.STRING));
    assertFalse(TypeFamily.ARRAY.isCoercible(TypeFamily.NUMERIC));
  }

  @Test
  void isCastable() throws Exception {
    assertTrue(TypeFamily.BINARY.isCastable(TypeFamily.STRING));
    assertFalse(TypeFamily.BINARY.isCastable(null));
    assertFalse(TypeFamily.BINARY.isCastable(TypeFamily.TEMPORAL));

    assertTrue(TypeFamily.BOOLEAN.isCastable(TypeFamily.NUMERIC));
    assertTrue(TypeFamily.BOOLEAN.isCastable(TypeFamily.STRING));
    assertFalse(TypeFamily.BOOLEAN.isCastable(null));
    assertFalse(TypeFamily.BOOLEAN.isCastable(TypeFamily.TEMPORAL));

    assertTrue(TypeFamily.NUMERIC.isCastable(TypeFamily.BOOLEAN));
    assertTrue(TypeFamily.NUMERIC.isCastable(TypeFamily.STRING));
    assertTrue(TypeFamily.NUMERIC.isCastable(TypeFamily.TEMPORAL));
    assertFalse(TypeFamily.NUMERIC.isCastable(null));
    assertFalse(TypeFamily.NUMERIC.isCastable(TypeFamily.JSON));

    assertTrue(TypeFamily.STRING.isCastable(TypeFamily.BOOLEAN));
    assertTrue(TypeFamily.STRING.isCastable(TypeFamily.NUMERIC));
    assertTrue(TypeFamily.STRING.isCastable(TypeFamily.TEMPORAL));
    assertTrue(TypeFamily.STRING.isCastable(TypeFamily.BINARY));
    assertFalse(TypeFamily.STRING.isCastable(null));

    assertTrue(TypeFamily.TEMPORAL.isCastable(TypeFamily.STRING));
    assertTrue(TypeFamily.TEMPORAL.isCastable(TypeFamily.NUMERIC));
    assertFalse(TypeFamily.TEMPORAL.isCastable(null));
    assertFalse(TypeFamily.TEMPORAL.isCastable(TypeFamily.BOOLEAN));

    assertTrue(TypeFamily.INTERVAL.isCastable(TypeFamily.INTERVAL));
    assertFalse(TypeFamily.INTERVAL.isCastable(null));
    assertFalse(TypeFamily.INTERVAL.isCastable(TypeFamily.TEMPORAL));

    assertTrue(TypeFamily.JSON.isCastable(TypeFamily.STRING));
    assertFalse(TypeFamily.JSON.isCastable(null));
    assertFalse(TypeFamily.JSON.isCastable(TypeFamily.TEMPORAL));
  }

  @Test
  void typeIdOf() throws Exception {
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
    assertEquals(TypeId.INET, TypeId.of("INET"));
    assertNull(TypeId.of("NOP"));
  }

  @Test
  void typeIdFromJavaClass() throws Exception {
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
    assertEquals(TypeId.INET, TypeId.fromJavaClass(InetAddress.class));
    assertEquals(TypeId.INTEGER, TypeId.fromJavaClass(Long.class));
    assertEquals(TypeId.ARRAY, TypeId.fromJavaClass(Array.class));
  }

  @Test
  void typeIdFromValue() throws Exception {
    assertEquals(TypeId.UNKNOWN, TypeId.fromValue(null));
    assertEquals(TypeId.UNKNOWN, TypeId.fromValue(new Random()));
    assertEquals(TypeId.BOOLEAN, TypeId.fromValue(true));
    assertEquals(TypeId.UNKNOWN, TypeId.fromValue(TimeUnit.CENTURY));
    assertEquals(TypeId.STRING, TypeId.fromValue("test"));
    assertEquals(TypeId.BINARY, TypeId.fromValue(new byte[] {0xF}));
    assertEquals(TypeId.INTEGER, TypeId.fromValue(123));
    assertEquals(TypeId.INTEGER, TypeId.fromValue(123L));
    assertEquals(TypeId.NUMBER, TypeId.fromValue(123.456D));
    assertEquals(TypeId.NUMBER, TypeId.fromValue(new BigDecimal("123456789123456789")));
    assertEquals(TypeId.NUMBER, TypeId.fromValue(BigDecimal.valueOf(123456789, 3)));
    assertEquals(TypeId.DATE, TypeId.fromValue(ZonedDateTime.now()));
    assertEquals(TypeId.INTERVAL, TypeId.fromValue(Interval.of(5)));
    assertEquals(TypeId.JSON, TypeId.fromValue(JsonType.convertToJson("{\"name\":\"Smith\"}")));
  }

  @Test
  void isFamily() throws Exception {
    assertTrue(TypeFamily.ANY.isFamily(TypeFamily.ANY));
    assertTrue(TypeFamily.ANY.isFamily(TypeFamily.BINARY));
    assertTrue(TypeFamily.ANY.isFamily(TypeFamily.BOOLEAN));
    assertTrue(TypeFamily.ANY.isFamily(TypeFamily.NUMERIC));
    assertTrue(TypeFamily.ANY.isFamily(TypeFamily.TEMPORAL));
    assertTrue(TypeFamily.ANY.isFamily(TypeFamily.STRING));
    assertTrue(TypeFamily.ANY.isFamily(TypeFamily.INTERVAL));
    assertTrue(TypeFamily.ANY.isFamily(TypeFamily.JSON));
    assertTrue(TypeFamily.ANY.isFamily(TypeFamily.INET));
    assertTrue(TypeFamily.ANY.isFamily(TypeFamily.ARRAY));

    assertTrue(TypeFamily.NUMERIC.isFamily(TypeFamily.NUMERIC));
    assertTrue(TypeFamily.BOOLEAN.isFamily(TypeFamily.BOOLEAN));
    assertTrue(TypeFamily.STRING.isFamily(TypeFamily.STRING));
    assertTrue(TypeFamily.TEMPORAL.isFamily(TypeFamily.TEMPORAL));
    assertTrue(TypeFamily.INTERVAL.isFamily(TypeFamily.INTERVAL));
    assertTrue(TypeFamily.BINARY.isFamily(TypeFamily.BINARY));
    assertTrue(TypeFamily.INET.isFamily(TypeFamily.INET));

    assertTrue(Types.ANY.isFamily(TypeFamily.BINARY));
    assertTrue(Types.ANY.isFamily(TypeFamily.BOOLEAN));
    assertTrue(Types.ANY.isFamily(TypeFamily.NUMERIC));
    assertTrue(Types.ANY.isFamily(TypeFamily.TEMPORAL));
    assertTrue(Types.ANY.isFamily(TypeFamily.STRING));
    assertTrue(Types.ANY.isFamily(TypeFamily.JSON));
    assertTrue(Types.ANY.isFamily(TypeFamily.ANY));

    assertTrue(Types.BINARY.isFamily(TypeFamily.ANY));
    assertTrue(Types.BINARY.isFamily(TypeFamily.BINARY));
    assertFalse(Types.BINARY.isFamily(TypeFamily.NUMERIC));
    assertTrue(Types.BINARY.isFamily(TypeFamily.ANY));
    assertFalse(Types.BINARY.isFamily(TypeFamily.NONE));
    assertTrue(Types.BINARY.isFamily(TypeFamily.BINARY));

    assertTrue(Types.BOOLEAN.isFamily(TypeFamily.BOOLEAN));
    assertTrue(Types.DATE.isFamily(TypeFamily.TEMPORAL));
    assertTrue(Types.INTEGER.isFamily(TypeFamily.NUMERIC));
    assertTrue(Types.NUMBER.isFamily(TypeFamily.NUMERIC));
    assertTrue(Types.STRING.isFamily(TypeFamily.STRING));

    assertEquals(TypeFamily.BINARY, Types.BINARY.getFamily());
    assertEquals(TypeFamily.BOOLEAN, Types.BOOLEAN.getFamily());
    assertEquals(TypeFamily.TEMPORAL, Types.DATE.getFamily());
    assertEquals(TypeFamily.JSON, Types.JSON.getFamily());
    assertEquals(TypeFamily.NUMERIC, Types.INTEGER.getFamily());
    assertEquals(TypeFamily.NUMERIC, Types.NUMBER.getFamily());
    assertEquals(TypeFamily.STRING, Types.STRING.getFamily());
    assertEquals(TypeFamily.INTERVAL, Types.INTERVAL.getFamily());
  }

  @Test
  void javaClass() throws Exception {
    assertEquals(byte[].class, TypeId.BINARY.getJavaClass());
    assertEquals(Boolean.class, TypeId.BOOLEAN.getJavaClass());
    assertEquals(Long.class, TypeId.INTEGER.getJavaClass());
    assertEquals(BigDecimal.class, TypeId.NUMBER.getJavaClass());
    assertEquals(String.class, TypeId.STRING.getJavaClass());
    assertEquals(ZonedDateTime.class, TypeId.DATE.getJavaClass());
    assertEquals(JsonNode.class, TypeId.JSON.getJavaClass());
    assertEquals(InetAddress.class, TypeId.INET.getJavaClass());
    assertEquals(Void.class, TypeId.UNKNOWN.getJavaClass());
  }

  @Test
  void signature() throws Exception {
    assertEquals("BOOLEAN", String.valueOf(Types.BOOLEAN));
    assertEquals("DATE", String.valueOf(Types.DATE));
    assertEquals("JSON", String.valueOf(Types.JSON));
    assertEquals("INET", String.valueOf(Types.INET));
    assertEquals("INTEGER", String.valueOf(Types.INTEGER));
    assertEquals("INTEGER", String.valueOf(IntegerType.of(-1)));
    assertEquals("INTEGER(6)", String.valueOf(IntegerType.of(6)));
    assertEquals("NUMBER", String.valueOf(Types.NUMBER));
    assertEquals("NUMBER(10)", String.valueOf(NumberType.of(10)));
    assertEquals("NUMBER(10)", String.valueOf(NumberType.of(10, 0)));
    assertEquals("NUMBER(10,2)", String.valueOf(NumberType.of(10, 2)));
    assertEquals("STRING", String.valueOf(Types.STRING));
    assertEquals("STRING", String.valueOf(StringType.of(-1)));
    assertEquals("STRING(10)", String.valueOf(StringType.of(10)));
    assertEquals("BINARY", String.valueOf(Types.BINARY));
    assertEquals("BINARY", String.valueOf(BinaryType.of(-1)));
    assertEquals("BINARY(10)", String.valueOf(BinaryType.of(10)));
    assertEquals("INTEGER(10)[]", String.valueOf(ArrayType.of(IntegerType.of(10))));
  }

  @Test
  void checkPrecisionScale() throws Exception {
    // Check maximum precision
    assertThrows(
        IllegalArgumentException.class, () -> IntegerType.of(TypeId.INTEGER.getMaxPrecision() + 1));
    assertThrows(
        IllegalArgumentException.class, () -> NumberType.of(TypeId.NUMBER.getMaxPrecision() + 1));
    assertThrows(
        IllegalArgumentException.class, () -> StringType.of(TypeId.STRING.getMaxPrecision() + 1));
    assertThrows(
        IllegalArgumentException.class, () -> BinaryType.of(TypeId.BINARY.getMaxPrecision() + 1));

    // Check minimum precision
    assertThrows(IllegalArgumentException.class, () -> IntegerType.of(0));
    assertThrows(IllegalArgumentException.class, () -> NumberType.of(0));
    assertThrows(IllegalArgumentException.class, () -> StringType.of(0));
    assertThrows(IllegalArgumentException.class, () -> BinaryType.of(0));

    // Check scale > precision
    assertThrows(IllegalArgumentException.class, () -> NumberType.of(10, 20));
  }

  @Test
  void coerceToBoolean() throws Exception {
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
  void castToBoolean() throws Exception {
    BooleanType type = Types.BOOLEAN;
    assertNull(type.cast(null));
    assertEquals(Boolean.TRUE, type.cast(3L));
    assertEquals(Boolean.TRUE, type.cast(1L));
    assertEquals(Boolean.TRUE, type.cast(true));
    assertEquals(Boolean.TRUE, type.cast(BigDecimal.ONE));
    assertEquals(Boolean.TRUE, type.cast(new BigDecimal("0.5")));
    assertEquals(Boolean.TRUE, type.cast("1"));
    assertEquals(Boolean.TRUE, type.cast("T"));
    assertEquals(Boolean.TRUE, type.cast("t"));
    assertEquals(Boolean.TRUE, type.cast("True"));
    assertEquals(Boolean.TRUE, type.cast("Y"));
    assertEquals(Boolean.TRUE, type.cast("Yes"));
    assertEquals(Boolean.TRUE, type.cast("ON"));
    assertEquals(Boolean.FALSE, type.cast(0L));
    assertEquals(Boolean.FALSE, type.cast(false));
    assertEquals(Boolean.FALSE, type.cast(BigDecimal.ZERO));
    assertEquals(Boolean.FALSE, type.cast("0"));
    assertEquals(Boolean.FALSE, type.cast("F"));
    assertEquals(Boolean.FALSE, type.cast("f"));
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
  void coerceToBinary() throws Exception {
    assertNull(BinaryType.coerce(null));

    assertThrows(ConversionException.class, () -> BinaryType.coerce(true));
    assertThrows(ConversionException.class, () -> BinaryType.coerce(3L));
    assertThrows(ConversionException.class, () -> BinaryType.coerce(3D));
  }

  @Test
  void castToBinary() throws Exception {
    BinaryType type = Types.BINARY;
    assertNull(type.cast(null));
    // assertEquals(new byte[] {0xF, 0xC}, type.cast(new byte[] {0xF, 0xC}));

    assertThrows(ConversionException.class, () -> type.cast(true));
    assertThrows(ConversionException.class, () -> type.cast(1L));
    assertThrows(ConversionException.class, () -> type.cast(1D));
    assertThrows(ConversionException.class, () -> type.cast(BigDecimal.ONE));
    assertThrows(ConversionException.class, () -> type.cast(ZonedDateTime.now()));
  }

  @Test
  void coerceToDate() throws Exception {
    ZonedDateTime date =
        LocalDate.of(2022, Month.DECEMBER, 28).atStartOfDay().atZone(ZoneOffset.UTC);

    assertNull(DateType.coerce(null));
    assertEquals(date, DateType.coerce(date));

    assertThrows(ConversionException.class, () -> DateType.coerce(true));
    assertThrows(ConversionException.class, () -> DateType.coerce("2022"));
  }

  @Test
  void castToDate() throws Exception {
    DateType type = Types.DATE;
    ZonedDateTime date =
        LocalDate.of(2022, Month.DECEMBER, 28).atStartOfDay().atZone(ZoneOffset.UTC);
    assertNull(type.cast(null));
    assertEquals(date, type.cast(date));
    assertEquals(date, type.cast(1672185600L));
    assertEquals(date, type.cast(BigDecimal.valueOf(1672185600L)));

    date = LocalDate.of(2022, Month.DECEMBER, 28).atStartOfDay().atZone(ZoneOffset.UTC);
    assertEquals(date, type.cast("2022-12-28"));
    assertEquals(date, type.cast("2022-12-28", "YYYY-MM-DD"));

    ZonedDateTime timestamp = ZonedDateTime.of(2022, 12, 28, 13, 32, 55, 123456789, ZoneOffset.UTC);
    assertEquals(
        timestamp, type.cast("2022-12-28 13:32:55.123456789", "YYYY-MM-DD HH24:MI:SS.FF9"));

    assertThrows(ConversionException.class, () -> type.cast(true));
  }

  @Test
  void coerceToString() throws Exception {
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
  void castToString() throws Exception {
    StringType type = Types.STRING;
    assertNull(type.cast(null));
    assertEquals("TRUE", type.cast(true));
    assertEquals("FALSE", type.cast(false));
    assertEquals("0", type.cast(0L));
    assertEquals("1", type.cast(1L));
    assertEquals("0", type.cast(BigDecimal.ZERO));
    assertEquals("1", type.cast(BigDecimal.ONE));
    assertEquals("3.123", type.cast(new BigDecimal("3.123")));
    assertEquals("-3.123", type.cast(new BigDecimal("-3.123")));
    assertEquals("ABCD��", type.cast("ABCD��".getBytes(StandardCharsets.UTF_8)));
    assertEquals("{\"name\":\"Smith\"}", type.cast(JsonType.convertToJson("{\"name\":\"Smith\"}")));
  }

  @Test
  void coerceToInteger() throws Exception {
    assertNull(IntegerType.coerce(null));
    assertEquals(Long.valueOf(1L), IntegerType.coerce(1));
    assertEquals(Long.valueOf(1L), IntegerType.coerce(1L));
    assertEquals(Long.valueOf(1L), IntegerType.coerce(1.2D));
    assertEquals(Long.valueOf(0L), IntegerType.coerce(".2"));
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
  void castToInteger() throws Exception {
    IntegerType type = Types.INTEGER;
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
    assertEquals(
        Long.valueOf(1672185600L),
        type.cast(LocalDate.of(2022, Month.DECEMBER, 28).atStartOfDay().atZone(ZoneOffset.UTC)));
  }

  @Test
  void coerceToNumber() throws Exception {
    assertNull(NumberType.coerce(null));
    assertEquals(BigDecimal.ZERO, NumberType.coerce(0L));
    assertEquals(BigDecimal.ZERO, NumberType.coerce(BigDecimal.ZERO));
    assertEquals(BigDecimal.ZERO, NumberType.coerce("0"));
    assertEquals(BigDecimal.ONE, NumberType.coerce(1L));
    assertEquals(BigDecimal.ONE, NumberType.coerce(BigDecimal.ONE));
    assertEquals(BigDecimal.ONE, NumberType.coerce("1"));
    assertEquals(new BigDecimal("1.2"), NumberType.coerce("1.2"));
    assertEquals(new BigDecimal("0.1"), NumberType.coerce(".1"));
    assertEquals(new BigDecimal("-2.3E+2"), NumberType.coerce("-2.3E+2"));
    assertEquals(new BigDecimal("-2.3E-2"), NumberType.coerce("-2.3E-2"));
    assertEquals(new BigDecimal("-2.3E-2"), NumberType.coerce("-2.3e-2"));
    assertEquals(new BigDecimal("3.123"), NumberType.coerce(new BigDecimal("3.123")));

    assertThrows(ConversionException.class, () -> NumberType.coerce(true));
    assertThrows(ConversionException.class, () -> NumberType.coerce(new byte[] {0xF}));
    assertThrows(ConversionException.class, () -> NumberType.coerce(ZonedDateTime.now()));
    assertThrows(ConversionException.class, () -> NumberType.coerce("FALSE"));
  }

  @Test
  void castToNumber() throws Exception {
    NumberType type = Types.NUMBER;
    assertNull(type.cast(null));
    assertEquals(BigDecimal.ZERO, type.cast(false));
    assertEquals(BigDecimal.ZERO, type.cast(0L));
    assertEquals(BigDecimal.ZERO, NumberType.of(10, 0).cast(BigDecimal.ZERO));
    assertEquals(new BigDecimal("0.00"), NumberType.of(10, 2).cast(BigDecimal.ZERO));
    assertEquals(BigDecimal.ZERO, type.cast("0"));
    assertEquals(BigDecimal.ZERO, type.cast(new byte[] {0x00}));
    assertEquals(BigDecimal.ONE, type.cast(true));
    assertEquals(BigDecimal.ONE, type.cast(1L));
    assertEquals(BigDecimal.ONE, NumberType.of(10, 0).cast(BigDecimal.ONE));
    assertEquals(BigDecimal.ONE, type.cast("1"));
    assertEquals(new BigDecimal("0.1"), type.cast("0.1"));
    assertEquals(new BigDecimal("0.1"), type.cast(".1"));
    assertEquals(BigDecimal.ONE, type.cast(new byte[] {0x01}));
    assertEquals(BigDecimal.valueOf(-356L), type.cast(-356L));
    // assertEquals(BigDecimal.valueOf(-3.56E2D), type.cast(-3.56E+2D));
    assertEquals(new BigDecimal("0.000"), type.cast("0.000"));
    assertEquals(new BigDecimal("-3.56E2"), type.cast("-3.56E+2"));
    assertEquals(BigDecimal.valueOf(15), type.cast(new byte[] {0xF}));

    assertThrows(ConversionException.class, () -> type.cast("TRUE"));
  }

  @Test
  void castToUnknown() throws Exception {
    UnknownType type = Types.UNKNOWN;
    assertThrows(ConversionException.class, () -> type.cast(null));
    assertThrows(ConversionException.class, () -> type.cast(true));
    assertThrows(ConversionException.class, () -> type.cast("Test", "MM"));
  }

  @Test
  void coercion() throws Exception {
    // Coercion Number
    evalTrue("1::NUMBER = 1::INTEGER");
    evalTrue("0x1F::NUMBER = 0x1F::INTEGER");
    evalTrue("0::NUMBER = 0::NUMBER");
    evalTrue("1::NUMBER = 1::INTEGER");

    // Coerce String to Number
    evalTrue("'1.25' = 1.25::NUMBER(10,4)");
    evalEquals("2.0*'1.23'", 2.46D);
    evalEquals("2*'1.23'", 2.46D);
    evalEquals("2+'2'", 4L);
    evalEquals("'2'+2", 4L);

    // Coerce Integer to String
    evalEquals("2 + 2 || 2", "42");
    evalEquals(" 4 + 4 || '2' ", "82");
    evalEquals(" '8' || 1 + 1", 82L);

    // Coerce Integer to Number
    evalEquals("'-2e-3' * 2", new BigDecimal("-4e-3"));
    evalEquals("'-4e-4'::Number(12,4) * 0.5", new BigDecimal("-0.00020"));
  }

  @Test
  void convertToBoolean() throws Exception {
    BooleanType type = Types.BOOLEAN;
    assertNull(type.convert(null, Boolean.class));
    assertNull(type.convert(null, Long.class));
    assertNull(type.convert(null, String.class));

    assertEquals(Boolean.TRUE, type.convert(true, Boolean.class));
    assertEquals("TRUE", type.convert(true, String.class));
    assertEquals("FALSE", type.convert(false, String.class));
    assertEquals(Long.valueOf(0), type.convert(false, Long.class));
    assertEquals(Long.valueOf(1), type.convert(true, Long.class));
    assertEquals(BigDecimal.ONE, type.convert(true, BigDecimal.class));
    assertEquals(BigDecimal.ZERO, type.convert(false, BigDecimal.class));

    assertThrows(ConversionException.class, () -> type.convert(true, ZonedDateTime.class));
  }

  @Test
  void convertToInteger() throws Exception {
    IntegerType type = Types.INTEGER;
    assertNull(type.convert(null, Long.class));
    assertNull(type.convert(null, BigDecimal.class));
    assertNull(type.convert(null, String.class));

    assertEquals(Boolean.FALSE, type.convert(0L, Boolean.class));
    assertEquals(Boolean.TRUE, type.convert(1L, Boolean.class));
    assertEquals(Boolean.TRUE, type.convert(-5L, Boolean.class));
    assertEquals("123", type.convert(123L, String.class));
    assertEquals("-123", type.convert(-123L, String.class));
    assertEquals(Long.valueOf(123), type.convert(123L, Long.class));
    assertEquals(BigDecimal.valueOf(123), type.convert(123L, BigDecimal.class));

    assertThrows(ConversionException.class, () -> type.convert(1672185600L, ZonedDateTime.class));
  }

  @Test
  void convertToNumber() throws Exception {
    NumberType type = Types.NUMBER;
    assertNull(type.convert(null, Long.class));
    assertNull(type.convert(null, BigDecimal.class));
    assertNull(type.convert(null, String.class));

    assertEquals(Boolean.FALSE, type.convert(BigDecimal.ZERO, Boolean.class));
    assertEquals(Boolean.TRUE, type.convert(BigDecimal.ONE, Boolean.class));
    assertEquals("-123.045", type.convert(new BigDecimal("-123.045"), String.class));
    assertEquals(Long.valueOf(-123), type.convert(new BigDecimal("-123.045"), Long.class));
    assertEquals(
        BigDecimal.valueOf(-123.045), type.convert(new BigDecimal("-123.045"), BigDecimal.class));

    assertThrows(
        ConversionException.class,
        () -> type.convert(new BigDecimal("-123.045"), ZonedDateTime.class));
  }

  @Test
  void convertToString() throws Exception {
    StringType type = Types.STRING;
    assertNull(type.convert(null, Boolean.class));
    assertNull(type.convert(null, Long.class));
    assertNull(type.convert(null, String.class));

    assertEquals(Boolean.TRUE, type.convert("True", Boolean.class));
    assertEquals(Boolean.FALSE, type.convert("Off", Boolean.class));
    assertEquals("Test", type.convert("Test", String.class));
    assertEquals(Long.valueOf(123), type.convert("123", Long.class));
    assertEquals(Long.valueOf(-123), type.convert("-123", Long.class));
    assertEquals(BigDecimal.ONE, type.convert("1", BigDecimal.class));

    assertThrows(DateTimeParseException.class, () -> type.convert("ABC", ZonedDateTime.class));
  }

  @Test
  void convertToDate() throws Exception {
    DateType type = Types.DATE;

    ZonedDateTime date =
        LocalDate.of(2022, Month.DECEMBER, 28).atStartOfDay().atZone(ZoneOffset.UTC);

    assertNull(type.convert(null, Long.class));
    assertNull(type.convert(null, String.class));
    assertNull(type.convert(null, ZonedDateTime.class));

    assertEquals(date, type.convert(date, ZonedDateTime.class));
    assertEquals(Long.valueOf(1672185600L), type.convert(date, Long.class));
    assertEquals(BigDecimal.valueOf(1672185600L), type.convert(date, BigDecimal.class));
    assertEquals("2022-12-28", type.convert(date, String.class));

    assertThrows(ConversionException.class, () -> type.convert(date, Boolean.class));
  }
}
