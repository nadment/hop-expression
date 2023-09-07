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
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.type.BooleanType;
import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.JsonType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.UnknownType;
import org.junit.Test;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Objects;
import com.fasterxml.jackson.databind.json.JsonMapper;

public class LiteralTest extends ExpressionTest {
  @Test
  public void testEquals() throws Exception {
    assertEquals(Literal.ZERO, Literal.of(0L));
    assertEquals(Literal.ONE, Literal.of(1L));
    assertEquals(Literal.of(new BigDecimal("5")), Literal.of(5L));
    assertEquals(Literal.of("test"), Literal.of("test"));
  }

  @Test
  public void Unknown() throws Exception {
    assertEquals(Kind.LITERAL, Literal.UNKNOWN.getKind());
    assertEquals(Literal.UNKNOWN, Literal.of(null));
    assertEquals(Objects.hash(null, UnknownType.UNKNOWN), Literal.UNKNOWN.hashCode());
    assertFalse(Literal.UNKNOWN.is((Kind) null));
    assertFalse(Literal.UNKNOWN.is((Operator) null));
    assertTrue(Literal.UNKNOWN.isConstant());
    assertNotEquals(Literal.UNKNOWN, null);
    assertNotEquals(Literal.UNKNOWN, Literal.ZERO);
    assertEquals(Literal.UNKNOWN.getType(), UnknownType.UNKNOWN);
    assertNull(Literal.UNKNOWN.getValue());
  }

  @Test
  public void Null() throws Exception {
    assertEquals(Kind.LITERAL, Literal.NULL.getKind());
    assertEquals(Objects.hash(null, BooleanType.BOOLEAN), Literal.NULL.hashCode());
    assertFalse(Literal.NULL.is((Kind) null));
    assertFalse(Literal.NULL.is((Operator) null));
    assertTrue(Literal.NULL.isConstant());
    assertNotEquals(Literal.NULL, null);
    assertNotEquals(Literal.NULL, Literal.ZERO);
    assertEquals(Literal.NULL.getType(), BooleanType.BOOLEAN);
    assertNull(Literal.NULL.getValue());
  }
  
  @Test
  public void TimeUnit() throws Exception {
    assertEquals(TimeUnit.HOUR, Literal.of(TimeUnit.HOUR).getValue());
  }

  public void Type() throws Exception {
    assertEquals(NumberType.NUMBER, Literal.of(NumberType.NUMBER).getValue());

    evalEquals("Cast(123 as InTeGeR)", 123L);
  }

  @Test
  public void String() throws Exception {
    assertEquals("Test", Literal.of("Test").getValue());
    assertEquals(Literal.of("Test"), Literal.of("Test"));

    // Single quote
    evalTrue("'test'='test'");

    // Single quote with two adjacent single quotes
    evalEquals("'te''st'", "te'st");
    evalEquals("'te''''st'", "te''st");

    // Empty string
    evalEquals("''", "");

    optimize("'Test ''Bla'' string'");

    returnType("FIELD_STRING", StringType.STRING);
  }


  @Test
  public void Json() throws Exception {
    JsonMapper mapper = JsonMapper.builder().build();

    assertEquals(Literal.of(mapper.readTree("{\"name\": \"John\", \"age\": 30}")),
        Literal.of(mapper.readTree("{\"name\": \"John\", \"age\": 30}")));

    evalEquals("JSON '{\"name\":\"John\",\"age\":5}'",
        mapper.readTree("{\"name\": \"John\", \"age\": 5}"));

    // Ignores the order of attributes
    evalTrue(
        "JSON '{\"name\":\"John\",\"age\":5,\"gender\":null}'=JSON '{\"name\":\"John\",\"gender\":null,\"age\":5}'");

    // Considers numeric values 5.0 and 5 as equals.
    evalTrue("JSON '{\"name\":\"John\",\"age\":5}'=JSON '{\"name\":\"John\",\"age\":5.0}'");

    optimize("JSON '{\"name\":\"John\",\"age\":5.0}'", "JSON '{\"name\":\"John\",\"age\":5.0}'");

    returnType("JSON '{\"name\":\"John\",\"age\":5}'", JsonType.JSON);
  }

  @Test
  public void Boolean() throws Exception {
    assertEquals(Boolean.TRUE, Literal.TRUE.getValue());
    assertEquals(Boolean.FALSE, Literal.FALSE.getValue());
    assertEquals(Literal.TRUE, Literal.of(true));
    assertEquals(Literal.FALSE, Literal.of(false));
    assertEquals(Literal.FALSE.hashCode(), Literal.of(false).hashCode());

    assertEquals("TRUE", String.valueOf(Literal.TRUE));
    assertEquals("FALSE", String.valueOf(Literal.FALSE));

    evalTrue("True");
    evalTrue("True");
    evalTrue("'On'::Boolean");
    evalTrue("'Yes'::Boolean");
    evalFalse("'Off'::Boolean");
    evalFalse("'No'::Boolean");
    evalNull("NULL");

    evalFails("'YEP'::Boolean");

    optimize("TRUE", "TRUE");

    returnType("TRUE and TRUE", BooleanType.BOOLEAN);
  }

  @Test
  public void Binary() throws Exception {

    evalEquals("BINARY ''", new byte[] {});
    evalEquals("BINARY '1F'", new byte[] {0x1F});
    evalEquals("BINARY '1234567812345678'", new byte[] {0x12, 0x34, 0x56, 0x78, 0x12, 0x34, 0x56, 0x78});

    evalFails("BINARY '0Z'");


    optimize("BINARY '12AF'", "BINARY '12AF'");
  }

  @Test
  public void Integer() throws Exception {
    assertEquals(Literal.ZERO, Literal.of(0L));
    assertEquals(Literal.ONE, Literal.of(1L));

    // For internal use int.class are supported
    assertEquals(Literal.ZERO, Literal.of(0L));
    assertEquals(Literal.ONE, Literal.of(1L));

    assertEquals("-123456", Literal.of(-123456L).toString());

    // Decimal
    evalEquals("-9223372036854775808", Long.MIN_VALUE);
    evalEquals("922_3372_0368_5477_5807", Long.MAX_VALUE);
    evalEquals("1_234", 1234L);
    evalEquals("1_2_3_4", 1234L);
    evalFails("_123");
    evalFails("123_");
    evalFails("-_123");
    evalFails("+_123");
    evalFails("1__23");
    
    // Hexadecimal
    evalEquals("0x1eee_FFFF", 0x1eee_FFFFL);
    evalEquals("0x123_4567_890ab_cDEF", 0x1234567890abcDEFL);
    evalEquals("0x4585_5892_1485_2587_2555_2569_123_4567_890ab_cDEF", new BigInteger("4585589214852587255525691234567890abcDEF",16));
    evalEquals("0xFFFF_EEEE_0000_AAA0", 0xFFFF_EEEE_0000_AAA0L);
    evalEquals("0X1F", 0x1FL);
    evalEquals("0x0F", 0xFL);
    evalEquals("0x0_F", 0xFL);
    evalEquals("0x_0F", 0xFL);    
    evalEquals("0xF", 0xFL);
    evalFails("0x");
    evalFails("0xG");    
    evalFails("0xF2_");
    evalFails("0xF2__FF");

    // Octal
    evalEquals("0o0757", 495L);
    evalEquals("0o12345671234567", 718046312823L);
    evalEquals("0O12345", 5349L);
    evalEquals("0O1_2_3_4_5", 5349L);
    evalEquals("0O_12345", 5349L);
    evalFails("0o");
    evalFails("0O99");
    evalFails("0o72_");
    evalFails("0O12__345");

    // Binary
    evalEquals("0b10", 0b10L);
    evalEquals("0b00000010", 0b10L);
    evalEquals("0b011", 0b11L);
    evalEquals("0b000000011111111", 0b000000011111111L);
    evalEquals("0b1010000101000101101000010100010110100001010001011010000101000101", 0b1010000101000101101000010100010110100001010001011010000101000101L);
    evalEquals("0B010101", 0b010101L);
    evalEquals("0B0_1_0101", 0b010101L);
    evalEquals("0B_0001_0101", 0b010101L);    
    evalFails("0b");
    evalFails("0b2");    
    evalFails("0b1001_");
    evalFails("0b10__01");
    
    optimize("123456", "123456");
    optimize("0X1F", "31");

    returnType("FIELD_INTEGER", IntegerType.INTEGER);
    returnType("0x4585_5892_1485_2587_2555_2569_123_4567_890ab_cDEF", NumberType.NUMBER);
  }

  @Test
  public void Number() throws Exception {
    assertEquals(Literal.ZERO, Literal.of(0D));
    assertEquals(Literal.ZERO, Literal.of(BigDecimal.ZERO));
    assertEquals(Literal.ONE, Literal.of(1D));
    assertEquals(Literal.ONE, Literal.of(BigDecimal.ONE));
    assertEquals(BigDecimal.valueOf(2.2), Literal.of(2.2D).getValue());
    assertEquals("-123456.789", Literal.of(-123456.789D).toString());
    assertEquals("-123456.789", Literal.of(BigDecimal.valueOf(-123456.789)).toString());

    // Number decimal
    evalEquals("+.1", 0.1D);
    evalEquals("-.2", -0.2D);
    evalEquals("-0.2", -0.2D);
    evalEquals(".000_005", 0.000005D);
    evalEquals("15167890123456789012345678901234567890",
        new BigDecimal("15167890123456789012345678901234567890"));

    // Number exponent
    evalEquals("2.3E2", 230L);
    evalEquals("2.3E+2", 230L);
    evalEquals("2_0.3_1E+2", 2031L);
    evalEquals("-2.3E-2", -2.3E-2D);
    evalEquals("-2.3e-2", -2.3E-2D);
    evalEquals("1_000.5e0_1", 10005D);
      
    // Underscore
    evalFails("1__2");
    evalFails("0.0__1");
    evalFails("2E2E2");
    evalFails("2E-2.2");
    evalFails("-2.3EE-2");
    evalFails("-2.3E");
    evalFails("-2.3E--2");
    evalFails("-_2.3E-2");
    evalFails("-2_.3E-2");
    evalFails("-2__0.3E-2");
    evalFails("-2._3E-2");
    evalFails("-2.3_E-2");
    evalFails("-2.3E_2");
    evalFails("-2.3E2_");
    evalFails("-2.3E1__2");
    
    evalFails("-1.");
    evalFails("..1");
    evalFails(".0.1");

    optimize("-2.3E-2", "-0.023");
  }

  @Test
  public void Date() throws Exception {
    ZonedDateTime datetime = ZonedDateTime.of(LocalDate.of(2021, 2, 25), LocalTime.of(2, 59, 00),
        ZoneId.systemDefault());

    assertEquals(datetime, Literal.of(datetime).getValue());
    assertEquals(Literal.of(datetime), Literal.of(datetime));

    evalEquals("DaTe '2021-02-25'", LocalDate.of(2021, 2, 25));
    evalEquals("'2021-02-25'::DATE", LocalDate.of(2021, 2, 25));

    // Fails because literal use exact mode
    evalFails("DATE '2021-Feb-25'");
    evalFails("DATE '2021-2-25'");
    evalFails("DATE '2021-02-2'");
    evalFails("DATE ' 2021-02-02'");
    evalFails("DATE '2021-02-02 '");
    evalFails("DATE '2021 -02-02'");
    evalFails("DATE '2021- 02-02'");
    evalFails("DATE '2021-02-32'");
    evalFails("DATE '21-02-25'");
    evalFails("DATE '201-02-25'");

    optimize("DATE '2021-02-25'");

    returnType("DATE '2021-02-25'", DateType.DATE);
  }

  @Test
  public void Timestamp() throws Exception {

    // ISO Timestamp Formats
    evalEquals("TimeSTAMP '2021-02-25 3'",
        ZonedDateTime.of(2021, 2, 25, 3, 0, 0, 0, ZoneOffset.systemDefault()));
    evalEquals("TimeSTAMP '2021-02-25 03'",
        ZonedDateTime.of(2021, 2, 25, 3, 0, 0, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-02-25 23'",
        ZonedDateTime.of(2021, 2, 25, 23, 0, 0, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-02-25T23'",
        ZonedDateTime.of(2021, 2, 25, 23, 0, 0, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-02-25 3:59'",
        ZonedDateTime.of(2021, 2, 25, 3, 59, 0, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-02-25 03:59'",
        ZonedDateTime.of(2021, 2, 25, 3, 59, 0, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-02-25 23:59'",
        ZonedDateTime.of(2021, 2, 25, 23, 59, 0, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-02-25T23:59'",
        ZonedDateTime.of(2021, 2, 25, 23, 59, 0, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-02-25 3:59:59'",
        ZonedDateTime.of(2021, 2, 25, 3, 59, 59, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-02-25 03:59:59'",
        ZonedDateTime.of(2021, 2, 25, 3, 59, 59, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-02-25 23:59:59'",
        ZonedDateTime.of(2021, 2, 25, 23, 59, 59, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-01-01T15:28:59'",
        ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-01-01T5:28:59'",
        ZonedDateTime.of(2021, 1, 1, 5, 28, 59, 0, ZoneOffset.systemDefault()));

    // ISO Timestamp Formats with fraction seconds
    evalEquals("TIMESTAMP '2021-12-01 5:01:01.123456789'",
        ZonedDateTime.of(2021, 12, 1, 5, 01, 01, 123456789, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-12-01 12:01:01.123456789'",
        ZonedDateTime.of(2021, 12, 1, 12, 01, 01, 123456789, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-12-01 12:01:01.123456'",
        ZonedDateTime.of(2021, 12, 1, 12, 01, 01, 123456000, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-12-01 12:01:01.123'",
        ZonedDateTime.of(2021, 12, 1, 12, 01, 01, 123000000, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-12-01 12:01:01'",
        ZonedDateTime.of(2021, 12, 1, 12, 01, 01, 000000000, ZoneOffset.systemDefault()));

    // ISO Timestamp Formats with time zone offset
    evalEquals("TIMESTAMP '2021-01-01 5:28+02'",
        ZonedDateTime.of(2021, 1, 1, 5, 28, 0, 0, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28+02'",
        ZonedDateTime.of(2021, 1, 1, 15, 28, 0, 0, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28-02'",
        ZonedDateTime.of(2021, 1, 1, 15, 28, 0, 0, ZoneOffset.ofHoursMinutes(-2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 5:28+02:00'",
        ZonedDateTime.of(2021, 1, 1, 5, 28, 0, 0, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28+02:00'",
        ZonedDateTime.of(2021, 1, 1, 15, 28, 0, 0, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28 +02:00'",
        ZonedDateTime.of(2021, 1, 1, 15, 28, 0, 0, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 5:28 -02:00'",
        ZonedDateTime.of(2021, 1, 1, 5, 28, 0, 0, ZoneOffset.ofHoursMinutes(-2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28 -02:00'",
        ZonedDateTime.of(2021, 1, 1, 15, 28, 0, 0, ZoneOffset.ofHoursMinutes(-2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 5:28:59+02:00'",
        ZonedDateTime.of(2021, 1, 1, 5, 28, 59, 0, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59+02:00'",
        ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 0, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59+0200'",
        ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 0, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59 +02:00'",
        ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 0, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59 -02:00'",
        ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 0, ZoneOffset.ofHoursMinutes(-2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 5:28:59.123456789+0200'",
        ZonedDateTime.of(2021, 1, 1, 5, 28, 59, 123456789, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59.123456789+0200'",
        ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 123456789, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59.123456789+02:00'",
        ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 123456789, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 5:28:59.123456789 +02:00'",
        ZonedDateTime.of(2021, 1, 1, 5, 28, 59, 123456789, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59.123456789 +02:00'",
        ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 123456789, ZoneOffset.ofHoursMinutes(2, 0)));

    // With time zone region
    evalEquals("TIMESTAMP '2021-01-01 15:28'",
        ZonedDateTime.of(2021, 1, 1, 15, 28, 0, 0, ZoneId.of("UTC")));
    evalEquals("TIMESTAMP '2021-01-01 15:28' AT TIME ZONE 'UTC'",
        ZonedDateTime.of(2021, 1, 1, 15, 28, 0, 0, ZoneId.of("UTC")));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59' AT TIME ZONE 'America/New_York'",
        ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 0, ZoneId.of("America/New_York")));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59.123' AT TIME ZONE 'US/Pacific'",
        ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 123000000, ZoneId.of("US/Pacific")));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59.123456' AT TIME ZONE 'US/Pacific'",
        ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 123456000, ZoneId.of("US/Pacific")));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59.123456789' AT TIME ZONE 'Europe/Paris'",
        ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 123456789, ZoneId.of("Europe/Paris")));

    // The range of valid timestamp values
    evalEquals("TIMESTAMP '0001-01-1 00:00:00'",
        ZonedDateTime.of(1, 1, 1, 0, 0, 0, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '9999-12-31 23:59:59.999999999'",
        ZonedDateTime.of(9999, 12, 31, 23, 59, 59, 999999999, ZoneOffset.systemDefault()));

    optimize("TIMESTAMP '9999-12-31 23:59:59'");
    optimize("TIMESTAMP '9999-12-31 23:59:59.999'");
    optimize("TIMESTAMP '9999-12-31 23:59:59.999999'");
    optimize("TIMESTAMP '9999-12-31 23:59:59.999999999'");
    optimize("TIMESTAMP '2021-12-01 12:01:01 +02:00'");
    optimize("TIMESTAMP '2021-12-01 12:01:01.999 +02:00'");
    optimize("TIMESTAMP '2021-12-01 12:01:01.999999 +02:00'");
    optimize("TIMESTAMP '2021-12-01 12:01:01.999999999 +02:00'");
    optimize("TIMESTAMP '2021-12-01 12:01:01' AT TIME ZONE 'Europe/Paris'");

    evalFails("TIMESTAMP '21-02-25 23:59:59.999999999'");
    evalFails("TIMESTAMP '2021-01-01 15:28:59.123456789' AT TIME ZONE 'test'");

    // TODO: evalFails("TIMESTAMP '21-Feb-25 23:59:59.999999999'");

    returnType("TIMESTAMP '2021-12-01 12:01:01'", DateType.DATE);
    returnType("TIMESTAMP '2021-12-01 12:01:01 +02:00'", DateType.DATE);
    returnType("TIMESTAMP '2021-12-01 12:01:01' AT TIME ZONE 'America/New_York'",
        DateType.DATE);
  }
}

