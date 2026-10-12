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
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.fasterxml.jackson.databind.JsonNode;
import java.math.BigDecimal;
import java.net.InetAddress;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.IntervalQualifier;
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.util.JsonConversion;
import org.junit.jupiter.api.Test;

class TypeNameTest {

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
    assertFalse(TypeName.BOOLEAN.isCastable(TypeName.BINARY));

    assertTrue(TypeName.INTEGER.isCastable(TypeName.BOOLEAN));
    assertTrue(TypeName.INTEGER.isCastable(TypeName.STRING));
    assertTrue(TypeName.INTEGER.isCastable(TypeName.DATE));
    assertFalse(TypeName.INTEGER.isCastable(null));
    assertFalse(TypeName.INTEGER.isCastable(TypeName.JSON));
    assertFalse(TypeName.INTEGER.isCastable(TypeName.BINARY));

    assertTrue(TypeName.NUMBER.isCastable(TypeName.BOOLEAN));
    assertTrue(TypeName.NUMBER.isCastable(TypeName.STRING));
    assertTrue(TypeName.NUMBER.isCastable(TypeName.DATE));
    assertFalse(TypeName.NUMBER.isCastable(null));
    assertFalse(TypeName.NUMBER.isCastable(TypeName.JSON));
    assertFalse(TypeName.NUMBER.isCastable(TypeName.BINARY));

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
    assertTrue(TypeName.INTERVAL.isCastable(TypeName.STRING));
    assertTrue(TypeName.STRING.isCastable(TypeName.INTERVAL));
    assertFalse(TypeName.INTERVAL.isCastable(null));
    assertFalse(TypeName.INTERVAL.isCastable(TypeName.DATE));

    assertTrue(TypeName.JSON.isCastable(TypeName.STRING));
    assertFalse(TypeName.JSON.isCastable(null));
    assertFalse(TypeName.JSON.isCastable(TypeName.DATE));
  }

  @Test
  void coercibleImpliesCastable() {
    // Implicit coercion is only possible when the explicit cast is supported
    List<String> violations = new ArrayList<>();
    for (TypeName source : TypeName.values()) {
      for (TypeName target : TypeName.values()) {
        if (source.isCoercible(target) && !source.isCastable(target)) {
          violations.add(source + " -> " + target);
        }
      }
    }
    assertEquals(List.of(), violations);
  }

  @Test
  void of() {
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
  void fromClass() {
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
  void fromValue() throws Exception {
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
}
