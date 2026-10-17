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
  void unaccent() {
    assertEquals("Emile", Characters.unaccent("Émile"));
    assertEquals("AAAAAAaaaaaa", Characters.unaccent("ÀÁÂÃÄÅàáâãäå"));
    assertEquals("CcEeIiNnOoUuYy", Characters.unaccent("ÇçÉéÏïÑñÖöÜüÝý"));
    // An already decomposed letter, e followed by the combining acute accent
    assertEquals("e", Characters.unaccent("é"));
    // The letters Ł and ł have no decomposition but are replaced
    assertEquals("Lodz", Characters.unaccent("Łódź"));
    // Greek letters keep their base letter
    assertEquals("αωΩ", Characters.unaccent("άώὨ"));

    // The ligatures and the letters without decomposition are left as is
    assertEquals("ÆæØøŒœß", Characters.unaccent("ÆæØøŒœß"));
    // The modifier letters and symbols are not accents of a letter
    assertEquals("2^3 `x` ¨ ˜", Characters.unaccent("2^3 `x` ¨ ˜"));
    assertEquals("Cafe ラーメン", Characters.unaccent("Café ラーメン"));

    assertEquals("", Characters.unaccent(""));
    assertEquals("Hop expression", Characters.unaccent("Hop expression"));
  }

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
