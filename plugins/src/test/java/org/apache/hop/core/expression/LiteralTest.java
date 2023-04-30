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
import org.apache.hop.expression.type.Converter;
import org.apache.hop.expression.type.DataType;
import org.junit.Test;
import java.math.BigDecimal;
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
    assertEquals(Literal.of(5), Literal.of(5));
    assertEquals(Literal.of("test"), Literal.of("test"));
  }
  
  @Test
  public void Null() throws Exception {
    assertEquals(Kind.LITERAL, Literal.NULL.getKind());
    assertEquals(Literal.NULL, Literal.of(null));
    assertEquals(Objects.hash(null, DataType.UNKNOWN), Literal.NULL.hashCode());
    assertFalse(Literal.NULL.is((Kind)null));    
    assertFalse(Literal.NULL.is((Operator)null));
    assertTrue(Literal.NULL.isConstant());
    assertNotEquals(Literal.NULL,null);
    assertNotEquals(Literal.NULL,Literal.ZERO);
    assertNull(Literal.NULL.getValue(createExpressionContext()));
    // assertThrows(IllegalArgumentException.class, () -> Literal.of(Literal.NULL));
  }

  @Test
  public void TimeUnit() throws Exception {
    assertEquals(TimeUnit.HOUR, Literal.of(TimeUnit.HOUR).getValue(null));
  }

  public void DataType() throws Exception {
    assertEquals(DataType.BIGNUMBER, Literal.of(DataType.BIGNUMBER).getValue(null));
    
    evalEquals("Cast(123 as InTeGeR)", 123L);
    
    // Accept DataType quoted like a String
    evalEquals("Cast(123 as 'INTEGER')", 123L);
    evalEquals("Cast('2022-01-01' as 'DATE')", LocalDate.of(2022, 1, 1));
  }

  @Test
  public void String() throws Exception {
    assertEquals("Test", Literal.of("Test").getValue(null));
    assertEquals(Literal.of("Test"), Literal.of("Test"));

    // Single quote
    evalTrue("'test'='test'");

    // Single quote with two adjacent single quotes
    evalEquals("'te''st'", "te'st");
    evalEquals("'te''''st'", "te''st");

    // Empty string
    evalEquals("''", "");

    writeEquals("'Test ''Bla'' string'");
    
    returnType("FIELD_STRING", DataType.STRING);
  }
  

  @Test
  public void Json() throws Exception {
    JsonMapper mapper = JsonMapper.builder().build();
    
    assertEquals(Literal.of(mapper.readTree("{\"name\": \"John\", \"age\": 30}")), Literal.of(mapper.readTree("{\"name\": \"John\", \"age\": 30}")));
    
    evalEquals("JSON '{\"name\":\"John\",\"age\":5}'", mapper.readTree("{\"name\": \"John\", \"age\": 5}"));
    
    // Ignores the order of attributes
    evalTrue("JSON '{\"name\":\"John\",\"age\":5,\"gender\":null}'=JSON '{\"name\":\"John\",\"gender\":null,\"age\":5}'");

    // Considers numeric values 5.0 and 5 as equals.
    evalTrue("JSON '{\"name\":\"John\",\"age\":5}'=JSON '{\"name\":\"John\",\"age\":5.0}'");
    
    writeEquals("JSON '{\"name\":\"John\",\"age\":5.0}'","JSON '{\"name\":\"John\",\"age\":5.0}'");
    
    returnType("JSON '{\"name\":\"John\",\"age\":5}'", DataType.JSON);
  }

  @Test
  public void Boolean() throws Exception {
    assertEquals(Boolean.TRUE, Literal.TRUE.getValue(null));
    assertEquals(Boolean.FALSE, Literal.FALSE.getValue(null));
    assertEquals(Literal.TRUE, Literal.of(true));
    assertEquals(Literal.FALSE, Literal.of(false));
    assertEquals(Literal.FALSE.hashCode(), Literal.of(false).hashCode());

    assertEquals("TRUE", Converter.coerceToString(Literal.TRUE));
    assertEquals("FALSE", Converter.coerceToString(Literal.FALSE));

    evalTrue("True");
    evalTrue("True");
    evalTrue("'On'::Boolean");
    evalTrue("'Yes'::Boolean");
    evalFalse("'Off'::Boolean");
    evalFalse("'No'::Boolean");
    evalNull("NULL");
    
    evalFails("'YEP'::Boolean");
    
    writeEquals("TRUE", "TRUE");
    
    returnType("TRUE and TRUE", DataType.BOOLEAN);
  }

  @Test
  public void Binary() throws Exception {

    // Hexadecimal
    evalEquals("0x1234567812345678", new byte[] {0x12, 0x34, 0x56, 0x78, 0x12, 0x34, 0x56, 0x78});
    evalFails("0X0F");
    evalFails("0X0FG");

    // Binary
    evalEquals("0b10::INTEGER", 2L);
    evalEquals("0b00000010::INTEGER", 2L);
    evalEquals("0b011::INTEGER", 3L);
    evalEquals("0b000000011111111::INTEGER", 255L);
    evalEquals("0b00001010101010101010101010101101010101010101010101010101010101010101::NUMBER",
        6.1489146933895936E18);
    evalFails("0B010101");
    evalFails("0B010501");

    writeEquals("0x12AF", "0x12AF");
  }

  @Test
  public void Integer() throws Exception {
    assertEquals(Literal.ZERO, Literal.of(0L));
    assertEquals(Literal.ONE, Literal.of(1L));
    
    // For internal use int.class are supported
    assertEquals(Literal.ZERO, Literal.of(0));
    assertEquals(Literal.ONE, Literal.of(1));
    
    assertEquals("-123456", Literal.of(-123456L).toString());

    // Integer
    evalEquals("-9223372036854775808", Long.MIN_VALUE);
    evalEquals("9223372036854775807", Long.MAX_VALUE);

    writeEquals("123456", "123456");
    
    returnType("FIELD_INTEGER", DataType.INTEGER);
  }

  @Test
  public void Number() throws Exception {
    assertEquals(Literal.ZERO, Literal.of(0D));  
    assertEquals(Literal.ONE, Literal.of(1D));   
    assertEquals(Double.valueOf(2.2), Literal.of(2.2D).getValue(null));
    assertEquals("-123456.789", Literal.of(-123456.789D).toString());

    // Number
    evalEquals("+.1", 0.1D);
    evalEquals("-.2", -0.2D);
    evalEquals("-0.2", -0.2D);
    evalEquals("-1.", -1L);
    evalEquals("2.3E2", 2.3E2D);
    evalEquals("2.3E+2", 2.3E2D);
    evalEquals("-2.3E-2", -2.3E-2D);
    evalEquals("-2.3e-2", -2.3E-2D);

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
    ZonedDateTime datetime = ZonedDateTime.of(LocalDate.of(2021, 2, 25), LocalTime.of(2, 59, 00),
        ZoneId.systemDefault());

    assertEquals(datetime, Literal.of(datetime).getValue(createExpressionContext() ));
    assertEquals(Literal.of(datetime), Literal.of(datetime));

    evalEquals("DaTe '2021-02-25'", LocalDate.of(2021, 2, 25));

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
    
    writeEquals("DATE '2021-02-25'");

    returnType("DATE '2021-02-25'", DataType.DATE);
  }

  @Test
  public void Timestamp() throws Exception {
    
    // ISO Timestamp Formats
    evalEquals("TimeSTAMP '2021-02-25 3'", ZonedDateTime.of(2021, 2, 25, 3, 0, 0, 0, ZoneOffset.systemDefault()));
    evalEquals("TimeSTAMP '2021-02-25 03'", ZonedDateTime.of(2021, 2, 25, 3, 0, 0, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-02-25 23'", ZonedDateTime.of(2021, 2, 25, 23, 0, 0, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-02-25T23'", ZonedDateTime.of(2021, 2, 25, 23, 0, 0, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-02-25 3:59'", ZonedDateTime.of(2021, 2, 25, 3, 59, 0, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-02-25 03:59'", ZonedDateTime.of(2021, 2, 25, 3, 59, 0, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-02-25 23:59'", ZonedDateTime.of(2021, 2, 25, 23, 59, 0, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-02-25T23:59'", ZonedDateTime.of(2021, 2, 25, 23, 59, 0, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-02-25 3:59:59'", ZonedDateTime.of(2021, 2, 25, 3, 59, 59, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-02-25 03:59:59'", ZonedDateTime.of(2021, 2, 25, 3, 59, 59, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-02-25 23:59:59'", ZonedDateTime.of(2021, 2, 25, 23, 59, 59, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-01-01T15:28:59'", ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-01-01T5:28:59'", ZonedDateTime.of(2021, 1, 1, 5, 28, 59, 0, ZoneOffset.systemDefault()));
    
    // ISO Timestamp Formats with fraction seconds
    evalEquals("TIMESTAMP '2021-12-01 5:01:01.123456789'", ZonedDateTime.of(2021, 12, 1, 5, 01, 01, 123456789, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-12-01 12:01:01.123456789'", ZonedDateTime.of(2021, 12, 1, 12, 01, 01, 123456789, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-12-01 12:01:01.123456'", ZonedDateTime.of(2021, 12, 1, 12, 01, 01, 123456000, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-12-01 12:01:01.123'", ZonedDateTime.of(2021, 12, 1, 12, 01, 01, 123000000, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '2021-12-01 12:01:01'", ZonedDateTime.of(2021, 12, 1, 12, 01, 01, 000000000, ZoneOffset.systemDefault()));
    
    // ISO Timestamp Formats with time zone offset
    evalEquals("TIMESTAMP '2021-01-01 5:28+02'", ZonedDateTime.of(2021, 1, 1, 5, 28, 0, 0, ZoneOffset.ofHoursMinutes(2, 0)));       
    evalEquals("TIMESTAMP '2021-01-01 15:28+02'", ZonedDateTime.of(2021, 1, 1, 15, 28, 0, 0, ZoneOffset.ofHoursMinutes(2, 0)));    
    evalEquals("TIMESTAMP '2021-01-01 15:28-02'", ZonedDateTime.of(2021, 1, 1, 15, 28, 0, 0, ZoneOffset.ofHoursMinutes(-2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 5:28+02:00'", ZonedDateTime.of(2021, 1, 1, 5, 28, 0, 0, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28+02:00'", ZonedDateTime.of(2021, 1, 1, 15, 28, 0, 0, ZoneOffset.ofHoursMinutes(2, 0)));    
    evalEquals("TIMESTAMP '2021-01-01 15:28 +02:00'", ZonedDateTime.of(2021, 1, 1, 15, 28, 0, 0, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 5:28 -02:00'", ZonedDateTime.of(2021, 1, 1, 5, 28, 0, 0, ZoneOffset.ofHoursMinutes(-2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28 -02:00'", ZonedDateTime.of(2021, 1, 1, 15, 28, 0, 0, ZoneOffset.ofHoursMinutes(-2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 5:28:59+02:00'", ZonedDateTime.of(2021, 1, 1, 5, 28, 59, 0, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59+02:00'", ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 0, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59+0200'", ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 0, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59 +02:00'", ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 0, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59 -02:00'", ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 0, ZoneOffset.ofHoursMinutes(-2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 5:28:59.123456789+0200'", ZonedDateTime.of(2021, 1, 1, 5, 28, 59, 123456789, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59.123456789+0200'", ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 123456789, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59.123456789+02:00'", ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 123456789, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 5:28:59.123456789 +02:00'", ZonedDateTime.of(2021, 1, 1, 5, 28, 59, 123456789, ZoneOffset.ofHoursMinutes(2, 0)));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59.123456789 +02:00'", ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 123456789, ZoneOffset.ofHoursMinutes(2, 0)));

    // With time zone region
    evalEquals("TIMESTAMP '2021-01-01 15:28'", ZonedDateTime.of(2021, 1, 1, 15, 28, 0, 0, ZoneId.of("UTC")));
    evalEquals("TIMESTAMP '2021-01-01 15:28' AT TIME ZONE 'UTC'", ZonedDateTime.of(2021, 1, 1, 15, 28, 0, 0, ZoneId.of("UTC")));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59' AT TIME ZONE 'America/New_York'", ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 0, ZoneId.of("America/New_York")));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59.123' AT TIME ZONE 'US/Pacific'", ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 123000000, ZoneId.of("US/Pacific")));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59.123456' AT TIME ZONE 'US/Pacific'", ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 123456000, ZoneId.of("US/Pacific")));
    evalEquals("TIMESTAMP '2021-01-01 15:28:59.123456789' AT TIME ZONE 'Europe/Paris'", ZonedDateTime.of(2021, 1, 1, 15, 28, 59, 123456789, ZoneId.of("Europe/Paris")));
    
    // The range of valid timestamp values
    evalEquals("TIMESTAMP '0001-01-1 00:00:00'", ZonedDateTime.of(1, 1, 1, 0, 0, 0, 0, ZoneOffset.systemDefault()));
    evalEquals("TIMESTAMP '9999-12-31 23:59:59.999999999'", ZonedDateTime.of(9999, 12, 31, 23, 59, 59, 999999999, ZoneOffset.systemDefault()));

    writeEquals("TIMESTAMP '9999-12-31 23:59:59'");
    writeEquals("TIMESTAMP '9999-12-31 23:59:59.999'");
    writeEquals("TIMESTAMP '9999-12-31 23:59:59.999999'");
    writeEquals("TIMESTAMP '9999-12-31 23:59:59.999999999'");
    writeEquals("TIMESTAMP '2021-12-01 12:01:01 +02:00'");
    writeEquals("TIMESTAMP '2021-12-01 12:01:01.999 +02:00'");
    writeEquals("TIMESTAMP '2021-12-01 12:01:01.999999 +02:00'");
    writeEquals("TIMESTAMP '2021-12-01 12:01:01.999999999 +02:00'");
    writeEquals("TIMESTAMP '2021-12-01 12:01:01' AT TIME ZONE 'Europe/Paris'");   
        
    evalFails("TIMESTAMP '21-02-25 23:59:59.999999999'");
    evalFails("TIMESTAMP '2021-01-01 15:28:59.123456789' AT TIME ZONE 'test'");
    
    // TODO: evalFails("TIMESTAMP '21-Feb-25 23:59:59.999999999'");
    
    returnType("TIMESTAMP '2021-12-01 12:01:01'", DataType.DATE);
    returnType("TIMESTAMP '2021-12-01 12:01:01 +02:00'", DataType.DATE);
    returnType("TIMESTAMP '2021-12-01 12:01:01' AT TIME ZONE 'America/New_York'", DataType.DATE);
  }
}

