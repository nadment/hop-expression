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

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.LocalDate;
import java.time.Month;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionTest;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.util.IntegerConversion;
import org.apache.hop.expression.util.JsonConversion;
import org.apache.hop.expression.util.NumberConversion;
import org.junit.jupiter.api.Test;

public class TypeTest extends ExpressionTest {

  @Test
  void leastRestrictiveOfIntegerAndNumber() {
    // In the numeric family, NUMBER is more general than INTEGER whatever the precision
    Type integer = IntegerType.of(3);
    Type number = NumberType.of(2, 1);
    assertEquals(NumberType.of(4, 1), Types.getLeastRestrictive(integer, number));
    assertEquals(NumberType.of(4, 1), Types.getLeastRestrictive(number, integer));

    // The NUMBER type is kept when it already holds the values of the INTEGER type
    assertEquals(number, Types.getLeastRestrictive(IntegerType.of(1), number));
    assertEquals(NumberType.NUMBER, Types.getLeastRestrictive(NumberType.NUMBER, integer));
  }

  @Test
  void integerTypeFrom() {
    // Regression: from(0) must not throw PRECISION_OUT_OF_RANGE
    assertEquals(1, IntegerType.from(0L).getPrecision());
    assertEquals(1, IntegerType.from(0).getPrecision());
    assertEquals(1, IntegerType.from(1L).getPrecision());
    assertEquals(1, IntegerType.from(-1L).getPrecision());
    assertEquals(3, IntegerType.from(123L).getPrecision());
    assertEquals(3, IntegerType.from(-123L).getPrecision());
  }

  @Test
  void typeOfFailsWhenPrecisionOrScaleOutOfRange() {
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
  void typeSignature() {
    assertEquals("BOOLEAN", String.valueOf(BooleanType.BOOLEAN));
    assertEquals("DATE", String.valueOf(DateType.DATE));
    assertEquals("JSON", String.valueOf(JsonType.JSON));
    assertEquals("INET", String.valueOf(InetType.INET));
    assertEquals("INTEGER", String.valueOf(IntegerType.INTEGER));
    assertEquals("INTEGER", String.valueOf(IntegerType.of(-1)));
    assertEquals("INTEGER(6)", String.valueOf(IntegerType.of(6)));
    assertEquals("NUMBER", String.valueOf(NumberType.NUMBER));
    assertEquals("NUMBER(10)", String.valueOf(NumberType.of(10)));
    assertEquals("NUMBER(10)", String.valueOf(NumberType.of(10, 0)));
    assertEquals("NUMBER(10,2)", String.valueOf(NumberType.of(10, 2)));
    // NUMBER has no fixed scale, a fixed scale is always written to parse the same type again
    assertEquals(NumberType.NUMBER, NumberType.of(-1, -1));
    assertEquals("NUMBER(38)", String.valueOf(NumberType.of(38)));
    assertEquals("NUMBER(38,9)", String.valueOf(NumberType.of(38, 9)));
    assertNotEquals(NumberType.NUMBER, NumberType.of(38, 0));
    assertEquals("STRING", String.valueOf(StringType.STRING));
    assertEquals("STRING", String.valueOf(StringType.of(-1)));
    assertEquals("STRING(10)", String.valueOf(StringType.of(10)));
    assertEquals("BINARY", String.valueOf(BinaryType.BINARY));
    assertEquals("BINARY", String.valueOf(BinaryType.of(-1)));
    // A precision not specified is the maximum precision, whatever the nullability
    assertEquals(BinaryType.BINARY_NOT_NULL, BinaryType.of(-1, false));
    assertEquals("BINARY NOT NULL", String.valueOf(BinaryType.of(-1, false)));
    assertEquals("BINARY(10)", String.valueOf(BinaryType.of(10)));
    assertEquals("ARRAY<INTEGER(10)>", String.valueOf(ArrayType.of(IntegerType.of(10))));
  }

  @Test
  void castFailsWhenPrecisionOrScaleOutOfRange() {
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
    BooleanType type = BooleanType.BOOLEAN;
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

    assertThrows(ExpressionException.class, () -> type.cast("3"));
    assertThrows(ExpressionException.class, () -> type.cast("MO"));
    assertThrows(ExpressionException.class, () -> type.cast("BAD"));
    assertThrows(ExpressionException.class, () -> type.cast("TRUL"));
    assertThrows(ExpressionException.class, () -> type.cast("FILSE"));
  }

  @Test
  void castToBinary() throws Exception {
    BinaryType type = BinaryType.BINARY;
    assertNull(type.cast(null));
    // assertEquals(new byte[] {0xF, 0xC}, type.cast(new byte[] {0xF, 0xC}));
    assertArrayEquals(new byte[] {0x41}, type.cast("A"));

    // The result is truncated to the precision without error
    BinaryType binary2 = BinaryType.of(2);
    assertNull(binary2.cast(null));
    assertArrayEquals(new byte[] {0x41, 0x42}, binary2.cast("ABC"));
    assertArrayEquals(new byte[] {0x41, 0x42}, binary2.cast("AB"));
    assertArrayEquals(new byte[] {0x1F, 0x2E}, binary2.cast(new byte[] {0x1F, 0x2E, 0x3D}));
    evalEquals("CAST('ABC' AS BINARY(2))", new byte[] {0x41, 0x42});
    evalEquals("BINARY '1F2E3D'::BINARY(1)", new byte[] {0x1F});

    assertThrows(ExpressionException.class, () -> type.cast(true));
    assertThrows(ExpressionException.class, () -> type.cast(1L));
    assertThrows(ExpressionException.class, () -> type.cast(1D));
    assertThrows(ExpressionException.class, () -> type.cast(BigDecimal.ONE));
    assertThrows(ExpressionException.class, () -> type.cast(ZonedDateTime.now()));
  }

  @Test
  void castToDate() {
    DateType type = DateType.DATE;
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

    assertThrows(ExpressionException.class, () -> type.cast(true));
    assertThrows(ExpressionException.class, () -> type.cast("2022"));
  }

  @Test
  void castToString() {
    StringType type = StringType.STRING;
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
    assertEquals("+5-0 0 00:00:00.000000000", type.cast(Interval.of(5)));
    assertEquals("-0-0 0 02:30:00.000000000", type.cast(Interval.of(0, 0, 0, 2, 30).negate()));

    // A value of an unsupported Java type raises an expression error, not a ClassCastException
    ExpressionException exception =
        assertThrows(ExpressionException.class, () -> type.cast(Integer.valueOf(1)));
    assertEquals(ErrorCode.UNSUPPORTED_CONVERSION, exception.getErrorCode());
    exception = assertThrows(ExpressionException.class, () -> type.cast(new Object()));
    assertEquals(ErrorCode.UNSUPPORTED_CONVERSION, exception.getErrorCode());
  }

  @Test
  void castToInterval() {
    IntervalType type = IntervalType.INTERVAL;
    Interval interval = Interval.of(0, 0, 0, 2);
    assertNull(type.cast(null));
    assertEquals(interval, type.cast(interval));
    assertEquals(interval, type.cast("2 hours"));

    ExpressionException exception = assertThrows(ExpressionException.class, () -> type.cast(1L));
    assertEquals(ErrorCode.UNSUPPORTED_CONVERSION, exception.getErrorCode());
  }

  @Test
  void castToInteger() throws Exception {
    IntegerType type = IntegerType.INTEGER;
    assertNull(type.cast(null));
    assertEquals(Long.valueOf(1L), type.cast(true));
    assertEquals(Long.valueOf(0L), type.cast(false));
    assertEquals(Long.valueOf(0L), type.cast(0L));
    assertEquals(Long.valueOf(3L), type.cast(3L));
    assertEquals(Long.valueOf(0L), type.cast(BigDecimal.ZERO));
    assertEquals(Long.valueOf(1L), type.cast(BigDecimal.ONE));

    // Casting operations that result in loss of precision are allowed
    assertEquals(Long.valueOf(3L), type.cast(BigDecimal.valueOf(3.125)));
    assertEquals(Long.valueOf(1L), type.cast(BigDecimal.valueOf(1.1)));
    assertEquals(Long.valueOf(2L), type.cast(BigDecimal.valueOf(1.5)));
    assertEquals(Long.valueOf(2L), type.cast(BigDecimal.valueOf(1.75)));
    assertEquals(Long.valueOf(-2L), type.cast(BigDecimal.valueOf(-1.7)));

    assertEquals(Long.valueOf(2L), type.cast("1.9"));
    assertEquals(Long.valueOf(6L), type.cast("5.9"));
    assertEquals(Long.valueOf(-5L), type.cast("-5.2"));

    assertEquals(
        Long.valueOf(1672185600L),
        type.cast(LocalDate.of(2022, Month.DECEMBER, 28).atStartOfDay().atZone(ZoneOffset.UTC)));

    // The fractions of seconds of a date are rounded like the same number of seconds
    for (long seconds = -3; seconds <= 3; seconds++) {
      for (int nanos : new int[] {0, 1, 499_999_999, 500_000_000, 500_000_001, 999_999_999}) {
        ZonedDateTime datetime =
            ZonedDateTime.ofInstant(Instant.ofEpochSecond(seconds, nanos), ZoneOffset.UTC);
        assertEquals(
            IntegerConversion.convert(NumberConversion.convert(datetime)),
            type.cast(datetime),
            datetime::toString);
      }
    }

    // The number of digits of the result is checked against the precision
    IntegerType integer3 = IntegerType.of(3);
    assertNull(integer3.cast(null));
    assertEquals(Long.valueOf(999L), integer3.cast(999L));
    assertEquals(Long.valueOf(-999L), integer3.cast(-999L));
    assertEquals(Long.valueOf(999L), integer3.cast(new BigDecimal("999.4")));
    assertEquals(Long.MIN_VALUE, IntegerType.INTEGER.cast(Long.MIN_VALUE));
    for (Object value : new Object[] {1000L, -1000L, "1234", new BigDecimal("999.5")}) {
      ExpressionException exception =
          assertThrows(ExpressionException.class, () -> integer3.cast(value));
      assertEquals(ErrorCode.NUMBER_CONVERSION_OVERFLOW, exception.getErrorCode());
    }
    evalFails("CAST(12345 AS INTEGER(3))", ErrorCode.NUMBER_CONVERSION_OVERFLOW);
    evalFails("CAST('12345' AS INTEGER(3))", ErrorCode.NUMBER_CONVERSION_OVERFLOW);
    evalNull("TRY_CAST(12345 AS INTEGER(3))");
  }

  @Test
  void castToNumber() {
    NumberType type = NumberType.NUMBER;
    assertNull(type.cast(null));
    assertEquals(BigDecimal.ZERO, type.cast(false));
    assertEquals(BigDecimal.ZERO, type.cast(0L));
    assertEquals(BigDecimal.ZERO, NumberType.of(10, 0).cast(BigDecimal.ZERO));
    assertEquals(new BigDecimal("0.00"), NumberType.of(10, 2).cast(BigDecimal.ZERO));
    assertEquals(BigDecimal.ZERO, type.cast("0"));
    assertEquals(BigDecimal.ONE, type.cast(true));
    assertEquals(BigDecimal.ONE, type.cast(1L));
    assertEquals(BigDecimal.ONE, NumberType.of(10, 0).cast(BigDecimal.ONE));
    assertEquals(BigDecimal.ONE, type.cast("1"));
    assertEquals(new BigDecimal("0.1"), type.cast("0.1"));
    assertEquals(new BigDecimal("0.1"), type.cast(".1"));
    assertEquals(BigDecimal.valueOf(-356L), type.cast(-356L));
    assertEquals(new BigDecimal("0.000"), type.cast("0.000"));
    assertEquals(new BigDecimal("-3.56E2"), type.cast("-3.56E+2"));
    assertEquals(new BigDecimal("-2.3E-2"), type.cast("-2.3e-2"));
    assertEquals(
        new BigDecimal("1672234375.123456789"),
        type.cast(ZonedDateTime.of(2022, 12, 28, 13, 32, 55, 123456789, ZoneOffset.UTC)));

    assertThrows(ExpressionException.class, () -> type.cast("TRUE"));
  }

  @Test
  void castToUnknown() {
    UnknownType type = UnknownType.UNKNOWN;
    assertThrows(ExpressionException.class, () -> type.cast(null));
    assertThrows(ExpressionException.class, () -> type.cast(true));
    assertThrows(ExpressionException.class, () -> type.cast("Test", "MM"));

    // The types without casting raise a conversion error, not an internal error
    ExpressionException exception =
        assertThrows(ExpressionException.class, () -> AnyType.ANY.cast(1L));
    assertEquals(ErrorCode.UNSUPPORTED_CONVERSION, exception.getErrorCode());
  }

  @Test
  void implicitCoercion() throws Exception {
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
  void convertFromBoolean() {
    BooleanType type = BooleanType.BOOLEAN;
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

    assertThrows(ExpressionException.class, () -> type.convert(true, ZonedDateTime.class));
  }

  @Test
  void convertFromInteger() {
    IntegerType type = IntegerType.INTEGER;
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

    assertThrows(ExpressionException.class, () -> type.convert(1672185600L, ZonedDateTime.class));
  }

  @Test
  void convertFromNumber() {
    NumberType type = NumberType.NUMBER;
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
        ExpressionException.class,
        () -> type.convert(new BigDecimal("-123.045"), ZonedDateTime.class));
  }

  @Test
  void convertFromString() {
    StringType type = StringType.STRING;
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

    assertThrows(ExpressionException.class, () -> type.convert("ABC", ZonedDateTime.class));
  }

  @Test
  void convertFromDate() {
    DateType type = DateType.DATE;

    ZonedDateTime date =
        LocalDate.of(2022, Month.DECEMBER, 28).atStartOfDay().atZone(ZoneOffset.UTC);

    assertNull(type.convert(null, Long.class));
    assertNull(type.convert(null, String.class));
    assertNull(type.convert(null, ZonedDateTime.class));

    assertEquals(date, type.convert(date, ZonedDateTime.class));
    assertEquals(Long.valueOf(1672185600L), type.convert(date, Long.class));
    assertEquals(BigDecimal.valueOf(1672185600L), type.convert(date, BigDecimal.class));
    assertEquals("2022-12-28", type.convert(date, String.class));

    assertThrows(ExpressionException.class, () -> type.convert(date, Boolean.class));
  }
}
