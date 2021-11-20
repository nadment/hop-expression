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
import static org.junit.Assert.assertNull;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.junit.Test;
import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;

public class LiteralTest extends BaseExpressionTest {

  @Test
  public void Null() throws Exception {
    assertEquals(Kind.LITERAL, Literal.NULL.getKind());
    assertEquals(Literal.NULL, Literal.of(null));
    assertEquals(Literal.NULL, new Literal(null));
    assertNull(Literal.NULL.eval(null));
  }

  @Test
  public void String() throws Exception {
    assertEquals("Test", Literal.of("Test").eval(null));
    assertEquals(Literal.of("Test"), Literal.of("Test"));

    // Single quote
    evalTrue("'test'='test'");

    // Single quote with two adjacent single quotes
    evalEquals("'te''st'", "te'st");
    evalEquals("'te''''st'", "te''st");

    // Empty string
    evalEquals("''", "");

    writeEquals("'Test ''Bla'' string'");
  }


  @Test
  public void Boolean() throws Exception {
    assertEquals(Boolean.TRUE, Literal.TRUE.eval(null));
    assertEquals(Boolean.FALSE, Literal.FALSE.eval(null));
    assertEquals(Literal.TRUE, Literal.of(true));
    assertEquals(Literal.FALSE, Literal.of(false));

    assertEquals("TRUE", Operator.coerceToString(Literal.TRUE));
    assertEquals("FALSE", Operator.coerceToString(Literal.FALSE));

    evalTrue("True");
    evalTrue("True");
    evalTrue("'On'::Boolean");
    evalTrue("'Yes'::Boolean");
    evalFalse("'Off'::Boolean");
    evalFalse("'No'::Boolean");
    evalNull("NULL");
  }

  @Test
  public void Binary() throws Exception {

    // Hexadecimal
    evalEquals("0xff", 255L);
    evalEquals("0xfE", 254L);
    evalEquals("0x0F", 15L);
    evalFails("0X0F");
    evalFails("0X0FG");

    // Binary
    evalEquals("0b10", 2L);
    evalEquals("0b00000010", 2L);
    evalEquals("0b011", 3L);
    evalEquals("0b000000011111111", 255L);
    evalEquals("0b00001010101010101010101010101101010101010101010101010101010101010101",
        6.1489146933895936E18);
    evalFails("0B010101");
    evalFails("0B010501");
  }



  @Test
  public void Integer() throws Exception {
    assertEquals(Literal.ZERO, Literal.of(0L));
    assertEquals(Literal.ONE, Literal.of(1L));
    assertEquals("-123456", Literal.of(-123456L).toString());

    // Integer
    evalEquals("-9223372036854775818", Long.MIN_VALUE);
    evalEquals("9223372036854775807", Long.MAX_VALUE);
  }

  @Test
  public void Number() throws Exception {
    assertEquals(Literal.ZERO, Literal.of(0D));
    assertEquals(Literal.ONE, Literal.of(1D));
    assertEquals(Math.PI, Literal.PI.eval(null));
    assertEquals("-123456.789", Literal.of(-123456.789D).toString());

    // Number
    evalEquals("+.1", 0.1);
    evalEquals("-.2", -0.2);
    evalEquals("-0.2", -0.2);
    evalEquals("-1.", -1);
    evalEquals("2.3E2", 2.3E2);
    evalEquals("2.3E+2", 2.3E2);
    evalEquals("-2.3E-2", -2.3E-2);
    evalEquals("-2.3e-2", -2.3E-2);

    evalFails("..1");
    evalFails(".0.1");
    evalFails("2E2E2");
    evalFails("2E-2.2");
    evalFails("-2.3EE-2");
    evalFails("-2.3E");
    evalFails("-2.3E--2");

    writeEquals("-2.3E-2", "-.023");
  }

  @Test
  public void BigNumber() throws Exception {
    assertEquals(Literal.ZERO, Literal.of(BigDecimal.ZERO));
    assertEquals(Literal.ONE, Literal.of(BigDecimal.ONE));
    assertEquals("-123456.789", Literal.of(BigDecimal.valueOf(-123456.789)).toString());

    // Big number
    evalEquals("15167890123456789012345678901234567890",
        new BigDecimal("15167890123456789012345678901234567890"));
  }

  @Test
  public void Date() throws Exception {
    Instant instant = Instant.ofEpochMilli(0);
    assertEquals(instant, Literal.of(instant).eval(null));
    assertEquals(Literal.of(instant), Literal.of(instant));

    evalEquals("Date '2021-02-25'", LocalDate.of(2021, 2, 25));
    evalEquals("Date '21-02-25'", LocalDate.of(21, 2, 25));
    evalEquals("Date '2021-Feb-25'", LocalDate.of(2021, 2, 25));

    writeEquals("DATE '2021-02-25'");
  }


  @Test
  public void Time() throws Exception {
    // evalEquals("Time '23:48:59'", LocalDateTime.of(1970, 1, 1, 23, 48, 59));
    // evalEquals("Time '01:05'", LocalDateTime.of(1970, 1, 1, 1, 5, 0));
    // evalEquals("Time '10:30 am'", LocalDateTime.of(1900, 1, 1, 10, 30,0));
    // evalEquals("Time '06:25:15 PM'", LocalDateTime.of(1900, 1, 1, 23, 48, 59));
  }

  @Test
  public void Timestamp() throws Exception {
    evalEquals("TimeSTAMP '2021-02-25 2:59'", LocalDateTime.of(2021, 2, 25, 2, 59, 00));
    evalEquals("Timestamp '2021-02-25 23:59'", LocalDateTime.of(2021, 2, 25, 23, 59, 00));
    evalEquals("Timestamp '2021-02-25 23:59:59'", LocalDateTime.of(2021, 2, 25, 23, 59, 59));
    evalEquals("Timestamp '2021-01-01 15:28:59'", LocalDateTime.of(2021, 1, 1, 15, 28, 59));

    // Time zone offset
    evalEquals("Timestamp '2021-01-01 15:28:59 +2:00'", LocalDateTime.of(2021, 1, 1, 13, 28, 59));
    evalEquals("Timestamp '2021-01-01 15:28:59 +02:00'", LocalDateTime.of(2021, 1, 1, 13, 28, 59));
    evalEquals("Timestamp '2021-01-01 15:28:59 -02:00'", LocalDateTime.of(2021, 1, 1, 17, 28, 59));

    // TIMESTAMP '2004-02-19 8:00:00 US/Pacific');

    // Fraction seconds
    evalEquals("Timestamp '2021-12-01 12:01:01.123456789'",
        LocalDateTime.of(2021, 12, 1, 12, 01, 01, 123456789));
    evalEquals("Timestamp '2021-12-01 12:01:01.123456'",
        LocalDateTime.of(2021, 12, 1, 12, 01, 01, 123456000));
    evalEquals("Timestamp '2021-12-01 12:01:01.123'",
        LocalDateTime.of(2021, 12, 1, 12, 01, 01, 123000000));
    evalEquals("Timestamp '2021-12-01 12:01:01'",
        LocalDateTime.of(2021, 12, 1, 12, 01, 01, 000000000));


    // The range of valid timestamp values
    evalEquals("Timestamp '0001-01-1 00:00:00'", LocalDateTime.of(1, 1, 1, 0, 0, 0, 0));
    evalEquals("Timestamp '9999-12-31 23:59:59.999999999'",
        LocalDateTime.of(9999, 12, 31, 23, 59, 59, 999999999));

    writeEquals("TIMESTAMP '9999-12-31 23:59:59.999999999'");
  }
}

