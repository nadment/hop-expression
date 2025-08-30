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

package org.apache.hop.expression.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.LocalDate;
import org.apache.hop.expression.ExpressionTest;
import org.junit.jupiter.api.Test;

public class RangeTest extends ExpressionTest {

  @Test
  void lessThan() {
    Range<Long> range = Range.lessThan(5L);
    assertEquals("(-∞..5)", range.toString());
    assertTrue(range.contains(2L));
    assertTrue(range.contains(-2L));
    assertFalse(range.contains(5L));
    assertFalse(range.contains(8L));

    Range<LocalDate> range2 = Range.lessThan(LocalDate.of(2024, 8, 12));
    assertEquals("(-∞..2024-08-12)", range2.toString());
    assertTrue(range2.contains(LocalDate.of(2024, 6, 24)));
    assertFalse(range2.contains(LocalDate.of(2024, 8, 12)));
    assertFalse(range2.contains(LocalDate.of(2025, 8, 12)));
  }

  @Test
  void lessThanOrEqual() {
    Range<Long> range = Range.lessThanOrEqual(5L);
    assertEquals("(-∞..5]", range.toString());

    assertTrue(range.contains(2L));
    assertTrue(range.contains(-2L));
    assertTrue(range.contains(5L));
    assertFalse(range.contains(8L));
  }

  @Test
  void greaterThan() {
    Range<Long> range = Range.greaterThan(5L);
    assertEquals("(5..+∞)", range.toString());
    assertFalse(range.contains(2L));
    assertFalse(range.contains(-2L));
    assertFalse(range.contains(5L));
    assertTrue(range.contains(8L));
  }

  @Test
  void greaterThanOrEqual() {
    Range<Long> range = Range.greaterThanOrEqual(5L);
    assertEquals("[5..+∞)", range.toString());
    assertTrue(range.contains(8L));
    assertTrue(range.contains(5L));
    assertFalse(range.contains(2L));
    assertFalse(range.contains(-2L));
  }

  @Test
  void singleton() {
    Range<Long> range = Range.singleton(5L);
    assertEquals("[5..5]", range.toString());
    assertEquals(Range.between(5L, 5L), range);
  }

  @Test
  void all() {
    assertEquals("(-∞..+∞)", Range.all().toString());
  }

  @Test
  void between() {
    Range<Long> range = Range.between(-5L, 5L);
    assertEquals("[-5..5]", range.toString());
  }

  @Test
  void isEmpty() {
    assertEquals(
        "[5..5)",
        Range.of(5L, Range.BoundType.INCLUSIVE, 5L, Range.BoundType.EXCLUSIVE).toString());
    assertEquals(
        "(5..5]",
        Range.of(5L, Range.BoundType.EXCLUSIVE, 5L, Range.BoundType.INCLUSIVE).toString());
    assertEquals(
        "(5..5)",
        Range.of(5L, Range.BoundType.EXCLUSIVE, 5L, Range.BoundType.EXCLUSIVE).toString());

    assertTrue(Range.of(5L, Range.BoundType.INCLUSIVE, 5L, Range.BoundType.EXCLUSIVE).isEmpty());
    assertTrue(Range.of(5L, Range.BoundType.EXCLUSIVE, 5L, Range.BoundType.INCLUSIVE).isEmpty());
    assertTrue(Range.of(5L, Range.BoundType.EXCLUSIVE, 5L, Range.BoundType.EXCLUSIVE).isEmpty());
    assertFalse(Range.singleton(5L).isEmpty());
    assertFalse(Range.between(-5L, 5L).isEmpty());
  }

  @Test
  void intersect() {
    Range<Long> range1 = Range.between(1L, 5L);
    Range<Long> range2 = Range.between(3L, 7L);
    assertEquals("[3..5]", range1.intersect(range2).toString());
    assertEquals("[3..5]", range2.intersect(range1).toString());

    // Return an empty range for disconnected ranges
    assertEquals("(5..5)", Range.between(1L, 5L).intersect(Range.between(7L, 9L)).toString());
    assertEquals("(5..5)", Range.between(7L, 9L).intersect(Range.between(1L, 5L)).toString());

    // Return a single point range
    assertEquals("[5..5]", Range.between(1L, 7L).intersect(Range.singleton(5L)).toString());
    assertEquals("[5..5]", Range.between(1L, 5L).intersect(Range.singleton(5L)).toString());
    assertEquals("[5..5]", Range.between(5L, 7L).intersect(Range.singleton(5L)).toString());
    assertEquals("[5..5]", Range.singleton(5L).intersect(Range.between(1L, 7L)).toString());
    assertEquals("[5..5]", Range.singleton(5L).intersect(Range.between(1L, 5L)).toString());
    assertEquals("[5..5]", Range.singleton(5L).intersect(Range.between(5L, 7L)).toString());

    // Return a single point range for adjacent ranges
    assertEquals("[5..5]", Range.between(1L, 5L).intersect(Range.between(5L, 9L)).toString());
    assertEquals("[5..5]", Range.between(5L, 9L).intersect(Range.between(1L, 5L)).toString());

    // Return an empty range for adjacent ranges with at least one exclusive bounds
    assertEquals(
        "(5..5)",
        Range.of(1L, Range.BoundType.EXCLUSIVE, 5L, Range.BoundType.EXCLUSIVE)
            .intersect(Range.of(5L, Range.BoundType.EXCLUSIVE, 9L, Range.BoundType.EXCLUSIVE))
            .toString());
    assertEquals(
        "(5..5)",
        Range.of(1L, Range.BoundType.EXCLUSIVE, 5L, Range.BoundType.INCLUSIVE)
            .intersect(Range.of(5L, Range.BoundType.EXCLUSIVE, 9L, Range.BoundType.EXCLUSIVE))
            .toString());
    assertEquals(
        "(5..5)",
        Range.of(1L, Range.BoundType.EXCLUSIVE, 5L, Range.BoundType.EXCLUSIVE)
            .intersect(Range.of(5L, Range.BoundType.INCLUSIVE, 9L, Range.BoundType.EXCLUSIVE))
            .toString());
  }

  @Test
  void x() {}
}
