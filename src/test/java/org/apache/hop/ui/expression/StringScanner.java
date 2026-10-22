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

import org.eclipse.jface.text.rules.ICharacterScanner;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;

/** Scans a string, like the scanner of the editor that returns EOF at the end. */
final class StringScanner implements ICharacterScanner {
  private final String text;
  private int offset;

  StringScanner(String text) {
    this.text = text;
  }

  /**
   * Evaluates a rule at the start of a text.
   *
   * @param rule the rule
   * @param text the text
   * @return the length of the text matched by the rule, or -1 if the rule does not match
   */
  static int scan(IRule rule, String text) {
    StringScanner scanner = new StringScanner(text);
    IToken token = rule.evaluate(scanner);
    if (token.isUndefined()) {
      // A rule that does not match must not move the scanner
      assertEquals(0, scanner.offset);
      return -1;
    }
    return scanner.offset;
  }

  @Override
  public char[][] getLegalLineDelimiters() {
    return new char[][] {{'\n'}};
  }

  @Override
  public int getColumn() {
    return offset;
  }

  @Override
  public int read() {
    // The offset is incremented at the end too, like RuleBasedScanner
    int c = offset < text.length() ? text.charAt(offset) : EOF;
    offset++;
    return c;
  }

  @Override
  public void unread() {
    offset--;
  }
}
