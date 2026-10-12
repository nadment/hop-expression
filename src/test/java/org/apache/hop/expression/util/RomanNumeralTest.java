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
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

class RomanNumeralTest {

  @Test
  void format() {
    assertEquals("MCMXCIX", RomanNumeral.format(1999, false));
    assertEquals("xiv", RomanNumeral.format(14, true));
    assertThrows(IllegalArgumentException.class, () -> RomanNumeral.format(0, false));
    assertThrows(IllegalArgumentException.class, () -> RomanNumeral.format(4000, false));
  }

  @Test
  void parse() {
    assertEquals(1999L, RomanNumeral.parse("MCMXCIX"));
    assertEquals(14L, RomanNumeral.parse("xiv"));

    // The end of the range is exclusive
    assertEquals(11L, RomanNumeral.parse("XIV", 0, 2));

    assertThrows(IllegalArgumentException.class, () -> RomanNumeral.parse(""));
    assertThrows(IllegalArgumentException.class, () -> RomanNumeral.parse("ABC"));
  }
}
