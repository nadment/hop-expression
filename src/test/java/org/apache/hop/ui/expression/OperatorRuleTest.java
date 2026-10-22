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

class OperatorRuleTest {

  private final OperatorRule rule = new OperatorRule(new Token("operator"));

  /** Returns the length of the operator at the start of the text, or -1 if it is not one. */
  private int scan(String text) {
    return StringScanner.scan(rule, text);
  }

  @Test
  void singleCharOperator() {
    for (String operator : new String[] {"(", ")", ",", "=", "+", "-", "*", "/", "%", "&", "~"}) {
      assertEquals(1, scan(operator + "1"), operator);
    }
    assertEquals(1, scan("^1"));
    assertEquals(1, scan("[1]"));
    assertEquals(1, scan("]"));
  }

  @Test
  void twoCharsOperator() {
    assertEquals(1, scan("|1"));
    assertEquals(2, scan("||'a'"));
    assertEquals(1, scan("<1"));
    assertEquals(2, scan("<=1"));
    assertEquals(2, scan("<>1"));
    assertEquals(1, scan("<<"));
    assertEquals(1, scan(">1"));
    assertEquals(2, scan(">=1"));
    assertEquals(2, scan("::INTEGER"));
    assertEquals(2, scan("!=1"));
  }

  @Test
  void notOperator() {
    assertEquals(-1, scan(""));
    assertEquals(-1, scan("a"));
    assertEquals(-1, scan(" +"));
    assertEquals(-1, scan(":a"));
    assertEquals(-1, scan(":!="));
    assertEquals(-1, scan("!a"));
  }
}
