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
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.eclipse.jface.text.rules.Token;
import org.junit.jupiter.api.Test;

class NestedCommentRuleTest {

  private static int scan(String text) {
    return StringScanner.scan(new NestedCommentRule(new Token("comment")), text);
  }

  @Test
  void blockComment() {
    assertEquals(7, scan("/* a */ + 1"));
    assertEquals(4, scan("/**/ + 1"));
    // A comment on several lines
    assertEquals(9, scan("/* a\nb */ + 1"));
  }

  @Test
  void nestedComment() {
    // The comment ends with the end of the first comment
    assertEquals(17, scan("/* a /* b */ c */ + 1"));
    assertEquals(11, scan("/*/* b */*/ + 1"));
  }

  @Test
  void commentNotClosed() {
    // The comment continues to the end of the text
    assertEquals(4, scan("/* a"));
    assertEquals(13, scan("/* a /* b */ "));
  }

  @Test
  void notComment() {
    assertEquals(-1, scan("/ 2"));
    assertEquals(-1, scan("1 /* a */"));
    assertEquals(-1, scan(""));
  }

  @Test
  void resume() {
    // The comment is scanned from its start to count the nested comments
    NestedCommentRule rule = new NestedCommentRule(new Token("comment"));
    assertTrue(rule.evaluate(new StringScanner(" c */"), true).isUndefined());
  }
}
