/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.hop.expression.type;

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
import org.apache.hop.expression.Array;
import org.apache.hop.expression.ConversionException;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionTest;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.IntervalQualifier;
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.util.FormatParseException;
import org.apache.hop.expression.util.JsonConversion;
import org.junit.jupiter.api.Test;

public class TypeTest extends ExpressionTest {

  @Test
  void isCoercible() {
    assertFalse(TypeName.NUMBER.isCoercible(null));
    assertTrue(TypeName.BOOLEAN.isCoercible(TypeName.INTEGER));
    assertTrue(TypeName.BOOLEAN.isCoercible(TypeName.NUMBER));
    assertTrue(TypeName.INTEGER.isCoercible(TypeName.NUMBER));

    assertTrue(TypeName.NUMBER.isCoercible(TypeName.INTEGER));
    assertTrue(TypeName.NUMBER.isCoercible(TypeName.STRING));
    assertFalse(TypeName.ARRAY.isCoercible(TypeName.NUMBER));
  }

  @Test
  void isCastable() {
    assertTrue(TypeName.BINARY.isCastable(TypeName.STRING));
    assertFalse(TypeName.BINARY.isCastable(null));
    assertFalse(TypeName.BINARY.isCastable(TypeName.DATE));
    assertFalse(TypeName.BINARY.isCastable(TypeName.INET));

    assertTrue(TypeName.BOOLEAN.isCastable(TypeName.INTEGER));
    assertTrue(TypeName.BOOLEAN.isCastable(TypeName.NUMBER));
    assertTrue(TypeName.BOOLEAN.isCastable(TypeName.STRING));
    assertFalse(TypeName.BOOLEAN.isCastable(null));
    assertFalse(TypeName.BOOLEAN.isCastable(TypeName.DATE));
    assertFalse(TypeName.BOOLEAN.isCastable(TypeName.INET));

    assertTrue(TypeName.INTEGER.isCastable(TypeName.BOOLEAN));
    assertTrue(TypeName.INTEGER.isCastable(TypeName.STRING));
    assertTrue(TypeName.INTEGER.isCastable(TypeName.DATE));
    assertFalse(TypeName.INTEGER.isCastable(null));
    assertFalse(TypeName.INTEGER.isCastable(TypeName.JSON));

    assertTrue(TypeName.NUMBER.isCastable(TypeName.BOOLEAN));
    assertTrue(TypeName.NUMBER.isCastable(TypeName.STRING));
    assertTrue(TypeName.NUMBER.isCastable(TypeName.DATE));
    assertFalse(TypeName.NUMBER.isCastable(null));
    assertFalse(TypeName.NUMBER.isCastable(TypeName.JSON));

    assertTrue(TypeName.STRING.isCastable(TypeName.BOOLEAN));
    assertTrue(TypeName.STRING.isCastable(TypeName.INTEGER));
    assertTrue(TypeName.STRING.isCastable(TypeName.NUMBER));
    assertTrue(TypeName.STRING.isCastable(TypeName.DATE));
    assertTrue(TypeName.STRING.isCastable(TypeName.BINARY));
    assertFalse(TypeName.STRING.isCastable(null));

    assertTrue(TypeName.DATE.isCastable(TypeName.STRING));
    assertTrue(TypeName.DATE.isCastable(TypeName.INTEGER));
    assertTrue(TypeName.DATE.isCastable(TypeName.NUMBER));
    assertFalse(TypeName.DATE.isCastable(null));
    assertFalse(TypeName.DATE.isCastable(TypeName.BOOLEAN));

    assertTrue(TypeName.INTERVAL.isCastable(TypeName.INTERVAL));
    assertFalse(TypeName.INTERVAL.isCastable(null));
    assertFalse(TypeName.INTERVAL.isCastable(TypeName.DATE));

    assertTrue(TypeName.JSON.isCastable(TypeName.STRING));
    assertFalse(TypeName.JSON.isCastable(null));
    assertFalse(TypeName.JSON.isCastable(TypeName.DATE));
  }

  @Test
  void typeOf() {
    assertThrows(ExpressionException.class, () -> IntegerType.of(20));
    assertThrows(ExpressionException.class, () -> NumberType.of(39));
    assertThrows(ExpressionException.class, () -> StringType.of(16_777_217));
    assertThrows(ExpressionException.class, () -> BinaryType.of(16_777_217));
    assertThrows(ExpressionException.class, () -> IntegerType.of(0));
    assertThrows(ExpressionException.class, () -> NumberType.of(0));
    assertThrows(ExpressionException.class, () -> StringType.of(0));
    assertThrows(ExpressionException.class, () -> BinaryType.of(0));
    assertThrows(ExpressionException.class, () -> NumberType.of(10, 20));
  }

  @Test
  void typeNameOf() {
    assertEquals(TypeName.ANY, TypeName.of("Any"));
    assertEquals(TypeName.UNKNOWN, TypeName.of("UNKNOWN"));
    assertEquals(TypeName.BOOLEAN, TypeName.of("BOOLEAN"));
    assertEquals(TypeName.BOOLEAN, TypeName.of("Boolean"));
    assertEquals(TypeName.STRING, TypeName.of("STRING"));
    assertEquals(TypeName.STRING, TypeName.of("String"));
    assertEquals(TypeName.DATE, TypeName.of("DATE"));
    assertEquals(TypeName.NUMBER, TypeName.of("NUMBER"));
    assertEquals(TypeName.BINARY, TypeName.of("BINARY"));
    assertEquals(TypeName.JSON, TypeName.of("Json"));
    assertEquals(TypeName.INTEGER, TypeName.of("INTEGER"));
    assertEquals(TypeName.INET, TypeName.of("INET"));
    assertEquals(TypeName.ENUM, TypeName.of("ENUM"));
    assertNull(TypeName.of("NOP"));
  }

  @Test
  void typeNameFromClass() {
    assertEquals(TypeName.UNKNOWN, TypeName.fromClass(null));
    assertEquals(TypeName.UNKNOWN, TypeName.fromClass(Void.class));
    assertEquals(TypeName.UNKNOWN, TypeName.fromClass(Float.class));
    assertEquals(TypeName.UNKNOWN, TypeName.fromClass(Type.class));
    assertEquals(TypeName.ENUM, TypeName.fromClass(TimeUnit.class));
    assertEquals(TypeName.ENUM, TypeName.fromClass(IntervalQualifier.class));
    assertEquals(TypeName.BOOLEAN, TypeName.fromClass(Boolean.class));
    assertEquals(TypeName.STRING, TypeName.fromClass(String.class));
    assertEquals(TypeName.DATE, TypeName.fromClass(ZonedDateTime.class));
    assertEquals(TypeName.NUMBER, TypeName.fromClass(BigDecimal.class));
    assertEquals(TypeName.BINARY, TypeName.fromClass(byte[].class));
    assertEquals(TypeName.JSON, TypeName.fromClass(JsonNode.class));
    assertEquals(TypeName.INET, TypeName.fromClass(InetAddress.class));
    assertEquals(TypeName.INTEGER, TypeName.fromClass(Long.class));
    assertEquals(TypeName.ARRAY, TypeName.fromClass(Array.class));
  }

  @Test
  void typeNameFromValue() throws Exception {
    assertEquals(TypeName.UNKNOWN, TypeName.fromValue(null));
    assertEquals(TypeName.UNKNOWN, TypeName.fromValue(new Random()));
    assertEquals(TypeName.BOOLEAN, TypeName.fromValue(true));
    assertEquals(TypeName.ENUM, TypeName.fromValue(TimeUnit.CENTURY));
    assertEquals(TypeName.ENUM, TypeName.fromValue(IntervalQualifier.YEAR_TO_MONTH));
    assertEquals(TypeName.STRING, TypeName.fromValue("test"));
    assertEquals(TypeName.BINARY, TypeName.fromValue(new byte[] {0xF}));
    assertEquals(TypeName.INTEGER, TypeName.fromValue(123));
    assertEquals(TypeName.INTEGER, TypeName.fromValue(123L));
    assertEquals(TypeName.NUMBER, TypeName.fromValue(123.456D));
    assertEquals(TypeName.NUMBER, TypeName.fromValue(new BigDecimal("123456789123456789")));
    assertEquals(TypeName.NUMBER, TypeName.fromValue(BigDecimal.valueOf(123456789, 3)));
    assertEquals(TypeName.DATE, TypeName.fromValue(ZonedDateTime.now()));
    assertEquals(TypeName.INTERVAL, TypeName.fromValue(Interval.of(5)));
    assertEquals(TypeName.INET, TypeName.fromValue(InetAddress.getByName("127.0.0.1")));
    assertEquals(TypeName.JSON, TypeName.fromValue(JsonConversion.convert("{\"name\":\"Smith\"}")));
  }

  @Test
  void isFamily() {
    assertTrue(TypeName.ANY.isFamily(TypeFamily.ANY));
    assertTrue(TypeName.ANY.isFamily(TypeFamily.BINARY));
    assertTrue(TypeName.ANY.isFamily(TypeFamily.BOOLEAN));
    assertTrue(TypeName.ANY.isFamily(TypeFamily.NUMERIC));
    assertTrue(TypeName.ANY.isFamily(TypeFamily.TEMPORAL));
    assertTrue(TypeName.ANY.isFamily(TypeFamily.STRING));
    assertTrue(TypeName.ANY.isFamily(TypeFamily.INTERVAL));
    assertTrue(TypeName.ANY.isFamily(TypeFamily.JSON));
    assertTrue(TypeName.ANY.isFamily(TypeFamily.NETWORK));
    assertTrue(TypeName.ANY.isFamily(TypeFamily.ARRAY));
    assertTrue(TypeName.INTEGER.isFamily(TypeFamily.NUMERIC));
    assertTrue(TypeName.NUMBER.isFamily(TypeFamily.NUMERIC));
    assertTrue(TypeName.BOOLEAN.isFamily(TypeFamily.BOOLEAN));
    assertTrue(TypeName.STRING.isFamily(TypeFamily.STRING));
    assertTrue(TypeName.DATE.isFamily(TypeFamily.TEMPORAL));
    assertTrue(TypeName.INTERVAL.isFamily(TypeFamily.INTERVAL));
    assertTrue(TypeName.BINARY.isFamily(TypeFamily.BINARY));
    assertTrue(TypeName.INET.isFamily(TypeFamily.NETWORK));
    assertTrue(TypeName.ENUM.isFamily(TypeFamily.ENUM));
  }

  @Test
  void javaClass() {
    assertEquals(byte[].class, TypeName.BINARY.getJavaClass());
    assertEquals(Boolean.class, TypeName.BOOLEAN.getJavaClass());
    assertEquals(Long.class, TypeName.INTEGER.getJavaClass());
    assertEquals(BigDecimal.class, TypeName.NUMBER.getJavaClass());
    assertEquals(String.class, TypeName.STRING.getJavaClass());
    assertEquals(ZonedDateTime.class, TypeName.DATE.getJavaClass());
    assertEquals(JsonNode.class, TypeName.JSON.getJavaClass());
    assertEquals(InetAddress.class, TypeName.INET.getJavaClass());
    assertEquals(Void.class, TypeName.UNKNOWN.getJavaClass());
    assertEquals(Enum.class, TypeName.ENUM.getJavaClass());
  }

  @Test
  void signature() {
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
    assertEquals("ARRAY<INTEGER(10)>", String.valueOf(ArrayType.of(IntegerType.of(10))));
  }

  @Test
  void checkPrecisionScale() {
    // Check maximum precision
    evalFails("CAST(1 as INTEGER(20))", ErrorCode.PRECISION_OUT_OF_RANGE);
    evalFails("CAST(1 as NUMBER(39))", ErrorCode.PRECISION_OUT_OF_RANGE);
    evalFails("CAST('x' as STRING(16_777_217))", ErrorCode.PRECISION_OUT_OF_RANGE);
    evalFails("CAST(BINARY '00' as BINARY(16_777_217))", ErrorCode.PRECISION_OUT_OF_RANGE);

    // Check minimum precision
    evalFails("CAST(1 as INTEGER(0))", ErrorCode.PRECISION_OUT_OF_RANGE);
    evalFails("CAST(1 as NUMBER(0))", ErrorCode.PRECISION_OUT_OF_RANGE);
    evalFails("CAST('x' as STRING(0))", ErrorCode.PRECISION_OUT_OF_RANGE);
    evalFails("CAST(BINARY '00' as BINARY(0))", ErrorCode.PRECISION_OUT_OF_RANGE);

    // Check scale > precision
    evalFails("CAST(1 as NUMBER(10,20))", ErrorCode.SCALE_GREATER_THAN_PRECISION);
  }

  @Test
  void castToBoolean() {
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
  void castToBinary() {
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
  void castToDate() {
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
    assertThrows(FormatParseException.class, () -> type.cast("2022"));
  }

  @Test
  void castToString() {
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
    assertEquals("{\"name\":\"Smith\"}", type.cast(JsonConversion.convert("{\"name\":\"Smith\"}")));
  }

  @Test
  void castToInteger() {
    IntegerType type = Types.INTEGER;
    assertNull(type.cast(null));
    assertEquals(Long.valueOf(1L), type.cast(true));
    assertEquals(Long.valueOf(0L), type.cast(false));
    assertEquals(Long.valueOf(0L), type.cast(0L));
    assertEquals(Long.valueOf(3L), type.cast(3L));
    assertEquals(Long.valueOf(0L), type.cast(BigDecimal.ZERO));
    assertEquals(Long.valueOf(1L), type.cast(BigDecimal.ONE));
    assertEquals(Long.valueOf(3L), type.cast(BigDecimal.valueOf(3.125)));
    assertEquals(Long.valueOf(0L), type.cast("0.9"));
    assertEquals(Long.valueOf(5L), type.cast("5.9"));
    assertEquals(Long.valueOf(-5L), type.cast("-5.2"));
    assertEquals(Long.valueOf(15L), type.cast(new byte[] {0xF}));
    assertEquals(
        Long.valueOf(1672185600L),
        type.cast(LocalDate.of(2022, Month.DECEMBER, 28).atStartOfDay().atZone(ZoneOffset.UTC)));
  }

  @Test
  void castToNumber() {
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
    assertEquals(new BigDecimal("-2.3E-2"), type.cast("-2.3e-2"));
    assertEquals(BigDecimal.valueOf(15), type.cast(new byte[] {0xF}));
    assertEquals(
        new BigDecimal("1672234375.123456789"),
        type.cast(ZonedDateTime.of(2022, 12, 28, 13, 32, 55, 123456789, ZoneOffset.UTC)));

    assertThrows(ConversionException.class, () -> type.cast("TRUE"));
  }

  @Test
  void castToUnknown() {
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
    // evalTrue("'1.25' = 1.25::NUMBER(10,4)");
    // evalEquals("2.0*'1.23'", 2.46D);
    // evalEquals("2*'1.23'", 2.46D);
    // evalEquals("2+'2'", 4L);
    // evalEquals("'2'+2", 4L);

    // Coerce Integer to String
    evalEquals("2 + 2 || 2", "42");
    evalEquals(" 4 + 4 || '2' ", "82");
    //  evalEquals(" '8' || 1 + 1", 82L);

    // Coerce Integer to Number
    evalEquals("-2e-3 * 2", new BigDecimal("-4e-3"));
    evalEquals("'-4e-4'::Number(12,4) * 0.5", new BigDecimal("-0.00020"));
  }

  @Test
  void convertToBoolean() {
    BooleanType type = Types.BOOLEAN;
    assertNull(type.convert(null, Boolean.class));
    assertNull(type.convert(null, Long.class));
    assertNull(type.convert(null, BigDecimal.class));
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
  void convertToInteger() {
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
  void convertToNumber() {
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
  void convertToString() {
    StringType type = Types.STRING;
    assertNull(type.convert(null, Boolean.class));
    assertNull(type.convert(null, Long.class));
    assertNull(type.convert(null, String.class));
    assertNull(type.convert(null, ZonedDateTime.class));

    assertEquals(Boolean.TRUE, type.convert("True", Boolean.class));
    assertEquals(Boolean.FALSE, type.convert("Off", Boolean.class));
    assertEquals("Test", type.convert("Test", String.class));
    assertEquals(Long.valueOf(123), type.convert("123", Long.class));
    assertEquals(Long.valueOf(-123), type.convert("-123", Long.class));
    assertEquals(BigDecimal.ONE, type.convert("1", BigDecimal.class));

    assertThrows(FormatParseException.class, () -> type.convert("ABC", ZonedDateTime.class));
  }

  @Test
  void convertToDate() {
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
