/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression.jit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.math.BigDecimal;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.type.IntegerType;
import org.junit.jupiter.api.Test;

/** Unit tests for {@link JitRuntime}, independent of the code generator or Janino. */
class JitRuntimeTest {

  @Test
  void addLong() {
    assertEquals(3L, JitRuntime.addLong(1L, 2L));
    assertNull(JitRuntime.addLong(null, 2L));
    assertNull(JitRuntime.addLong(1L, null));
    assertThrows(
        ExpressionException.class, () -> JitRuntime.addLong(Long.MAX_VALUE, 1L));
  }

  @Test
  void subtractLong() {
    assertEquals(-1L, JitRuntime.subtractLong(1L, 2L));
    assertNull(JitRuntime.subtractLong(null, 2L));
    assertThrows(
        ExpressionException.class, () -> JitRuntime.subtractLong(Long.MIN_VALUE, 1L));
  }

  @Test
  void multiplyLong() {
    assertEquals(6L, JitRuntime.multiplyLong(2L, 3L));
    assertNull(JitRuntime.multiplyLong(null, 3L));
    assertThrows(
        ExpressionException.class, () -> JitRuntime.multiplyLong(Long.MAX_VALUE, 2L));
  }

  @Test
  void addNumber() {
    assertEquals(
        new BigDecimal("3.5"), JitRuntime.addNumber(new BigDecimal("1.5"), new BigDecimal("2")));
    assertNull(JitRuntime.addNumber(null, BigDecimal.ONE));
  }

  @Test
  void subtractNumber() {
    assertEquals(
        new BigDecimal("0.5"),
        JitRuntime.subtractNumber(new BigDecimal("2.5"), new BigDecimal("2")));
    assertNull(JitRuntime.subtractNumber(BigDecimal.ONE, null));
  }

  @Test
  void multiplyNumber() {
    assertEquals(
        new BigDecimal("6.00"),
        JitRuntime.multiplyNumber(new BigDecimal("2.0"), new BigDecimal("3.00")));
    assertNull(JitRuntime.multiplyNumber(null, BigDecimal.ONE));
  }

  @Test
  void divideNumber() {
    assertEquals(
        new BigDecimal("2"), JitRuntime.divideNumber(new BigDecimal("4"), new BigDecimal("2")));
    assertNull(JitRuntime.divideNumber(null, BigDecimal.ONE));
    assertThrows(
        ExpressionException.class,
        () -> JitRuntime.divideNumber(BigDecimal.ONE, BigDecimal.ZERO));
  }

  @Test
  void and() {
    assertEquals(Boolean.FALSE, JitRuntime.and(Boolean.FALSE, null));
    assertEquals(Boolean.FALSE, JitRuntime.and(null, Boolean.FALSE));
    assertEquals(Boolean.TRUE, JitRuntime.and(Boolean.TRUE, Boolean.TRUE));
    assertNull(JitRuntime.and(Boolean.TRUE, null));
    assertNull(JitRuntime.and(null, null));
  }

  @Test
  void or() {
    assertEquals(Boolean.TRUE, JitRuntime.or(Boolean.TRUE, null));
    assertEquals(Boolean.TRUE, JitRuntime.or(null, Boolean.TRUE));
    assertEquals(Boolean.FALSE, JitRuntime.or(Boolean.FALSE, Boolean.FALSE));
    assertNull(JitRuntime.or(Boolean.FALSE, null));
    assertNull(JitRuntime.or(null, null));
  }

  @Test
  void not() {
    assertEquals(Boolean.FALSE, JitRuntime.not(Boolean.TRUE));
    assertEquals(Boolean.TRUE, JitRuntime.not(Boolean.FALSE));
    assertNull(JitRuntime.not(null));
  }

  @Test
  void compare() {
    IntegerType type = IntegerType.INTEGER;
    assertEquals(Boolean.TRUE, JitRuntime.compareEqual(type, 1L, 1L));
    assertEquals(Boolean.FALSE, JitRuntime.compareEqual(type, 1L, 2L));
    assertNull(JitRuntime.compareEqual(type, null, 1L));

    assertEquals(Boolean.TRUE, JitRuntime.compareNotEqual(type, 1L, 2L));
    assertEquals(Boolean.TRUE, JitRuntime.compareLess(type, 1L, 2L));
    assertEquals(Boolean.TRUE, JitRuntime.compareLessOrEqual(type, 2L, 2L));
    assertEquals(Boolean.TRUE, JitRuntime.compareGreater(type, 2L, 1L));
    assertEquals(Boolean.TRUE, JitRuntime.compareGreaterOrEqual(type, 2L, 2L));
  }
}
