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
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Duration;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.junit.jupiter.api.Test;

class LikeMatcherTest {

  private static boolean like(String value, String pattern) {
    return LikeMatcher.compile(pattern, "\\", false).matches(value);
  }

  private static boolean ilike(String value, String pattern) {
    return LikeMatcher.compile(pattern, "\\", true).matches(value);
  }

  @Test
  void wildcards() {
    assertTrue(like("", ""));
    assertFalse(like("a", ""));
    assertTrue(like("", "%"));
    assertTrue(like("", "%%"));
    assertFalse(like("", "_"));
    assertTrue(like("a", "_"));
    assertFalse(like("ab", "_"));

    assertTrue(like("amigo", "a%o"));
    assertTrue(like("ao", "a%o"));
    assertFalse(like("a", "a%o"));
    assertTrue(like("abcabc", "%bc"));
    assertTrue(like("abcabc", "abc%abc"));
    assertFalse(like("abcab", "abc%abc"));
    assertTrue(like("xaxbx", "%a%b%"));
    assertFalse(like("xbxax", "%a%b%"));
    assertTrue(like("aab", "%a_"));
    assertTrue(like("ami", "a_%_%"));
    assertFalse(like("am", "a_%_%"));
  }

  @Test
  void middleSegmentAtLeftmostPosition() {
    // The middle segment must not overlap the last segment anchored at the end
    assertFalse(like("abc", "%bc%c"));
    assertTrue(like("abcc", "%bc%c"));
    assertTrue(like("aXbXc", "a%X%c"));
    assertTrue(like("abab", "%ab%ab"));
    assertFalse(like("aba", "%ab%ab"));
  }

  @Test
  void anyCharacter() {
    // A new line is a character like any other
    assertTrue(like("A\nB", "A_B"));
    assertTrue(like("A\nB", "A%"));

    // A character outside the Basic Multilingual Plane is one character
    assertTrue(like("😀", "_"));
    assertFalse(like("😀", "__"));
    assertTrue(like("a😀b", "a_b"));
    assertTrue(like("a😀b", "%😀%"));
  }

  @Test
  void regexCharactersAreLiteral() {
    assertTrue(like(".*", ".*"));
    assertFalse(like("ab", ".*"));
    assertTrue(like("[a]{1}(b)|^$+?", "[a]{1}(b)|^$+?"));
  }

  @Test
  void escape() {
    assertTrue(like("100%", "100\\%"));
    assertFalse(like("100x", "100\\%"));
    assertTrue(like("a_b", "a\\_b"));
    assertFalse(like("axb", "a\\_b"));
    assertTrue(like("a\\b", "a\\\\b"));

    assertTrue(LikeMatcher.compile("%30!%%", "!", false).matches("give me 30% discount"));
    assertTrue(LikeMatcher.compile("a^^b", "^", false).matches("a^b"));

    assertEquals(
        ErrorCode.INVALID_REGEXP_ESCAPE,
        assertThrows(ExpressionException.class, () -> like("a", "a\\b")).getErrorCode());
    assertEquals(
        ErrorCode.INVALID_REGEXP_ESCAPE,
        assertThrows(ExpressionException.class, () -> like("a", "a\\")).getErrorCode());
    assertEquals(
        ErrorCode.INVALID_ARGUMENT,
        assertThrows(ExpressionException.class, () -> LikeMatcher.compile("a", "", false))
            .getErrorCode());
    assertEquals(
        ErrorCode.INVALID_ARGUMENT,
        assertThrows(ExpressionException.class, () -> LikeMatcher.compile("a", "!!", false))
            .getErrorCode());
  }

  @Test
  void ignoreCase() {
    assertFalse(like("ABC", "abc"));
    assertTrue(ilike("ABC", "abc"));
    assertTrue(ilike("abcdef", "A%_EF"));

    // Not only ASCII characters
    assertTrue(ilike("ÉTÉ", "été"));
    assertTrue(ilike("Straße", "STRA_E"));

    // An escaped letter is also compared without case
    assertTrue(LikeMatcher.compile("Axx", "x", true).matches("ax"));
  }

  @Test
  void noBacktracking() {
    // With a regular expression, a value that doesn't match takes an exponential time
    String value = "a".repeat(100_000);
    String pattern = "%a".repeat(20) + "%x";
    assertTimeoutPreemptively(
        Duration.ofSeconds(5),
        () -> {
          assertFalse(like(value, pattern));
          assertTrue(like(value + "x", pattern));
          // Each middle segment is searched in the whole value
          assertFalse(like(value, "%ab".repeat(20) + "%"));
        });
  }

  @Test
  void simplify() {
    assertEquals("", LikeMatcher.simplify("", '\\'));
    assertEquals("ABC", LikeMatcher.simplify("ABC", '\\'));
    assertEquals("%", LikeMatcher.simplify("%%%", '\\'));
    assertEquals("AB%C", LikeMatcher.simplify("AB%%%%C", '\\'));
    assertEquals("%AB%C%", LikeMatcher.simplify("%%%AB%%C%%%", '\\'));

    // An escaped '%' is not a wildcard
    assertEquals("\\%%AB%C%\\%", LikeMatcher.simplify("\\%%%AB%%C%%\\%", '\\'));
    assertEquals("%#%%A%", LikeMatcher.simplify("%%#%%A%%", '#'));

    // An escaped escape character is followed by a wildcard
    assertEquals("A\\\\%", LikeMatcher.simplify("A\\\\%%", '\\'));
  }
}
