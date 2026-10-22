/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.ui.expression;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.eclipse.jface.text.rules.Token;
import org.junit.jupiter.api.Test;

class LiteralNumberRuleTest {

  private final LiteralNumberRule rule = new LiteralNumberRule(new Token("number"));

  /** Returns the length of the number at the start of the text, or -1 if it is not a number. */
  private int scan(String text) {
    return StringScanner.scan(rule, text);
  }

  @Test
  void notNumber() {
    assertEquals(-1, scan("abc"));
    assertEquals(-1, scan("_1"));
    assertEquals(-1, scan(""));
  }

  @Test
  void decimal() {
    assertEquals(1, scan("0"));
    assertEquals(3, scan("123"));
    assertEquals(3, scan("123)"));
    assertEquals(5, scan("1_000 "));
    assertEquals(5, scan("12.50+1"));
    assertEquals(3, scan("12.)"));
    assertEquals(3, scan("1e5"));
    assertEquals(6, scan("1.5E-3)"));
    assertEquals(4, scan("2E+1"));
    assertEquals(2, scan("05,"));
  }

  @Test
  void hexadecimal() {
    assertEquals(4, scan("0xFF)"));
    assertEquals(6, scan("0XAB_C "));
  }

  @Test
  void binary() {
    assertEquals(5, scan("0b101)"));
    assertEquals(7, scan("0B01_10,"));
  }

  @Test
  void octal() {
    assertEquals(4, scan("0o17)"));
    // 8 is not an octal digit
    assertEquals(3, scan("0O78"));
  }
}
