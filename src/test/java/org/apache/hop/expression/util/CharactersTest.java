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
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;

class CharactersTest {

  @Test
  void transliterate() {
    assertEquals("Soren", Characters.transliterate("Søren"));
    assertEquals("AEaeOoTHthDdHhOEoe", Characters.transliterate("ÆæØøÞþĐđĦħŒœ"));
    // The letters with a decomposition are left for the removal of the diacritic marks
    assertEquals("Émile Łódź", Characters.transliterate("Émile Łódź"));

    // A string without any letter to transliterate is returned without any copy
    String str = "Hop expression";
    assertSame(str, Characters.transliterate(str));
    assertEquals("", Characters.transliterate(""));
  }
}
