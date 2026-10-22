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
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;
import org.junit.jupiter.api.Test;

class LiteralNumberRuleTest {

  /** Scans a string, like the scanner of the editor that returns EOF at the end. */
  private static final class StringScanner implements ICharacterScanner {
    private final String text;
    private int offset;

    StringScanner(String text) {
      this.text = text;
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

  private final IToken number = new Token("number");

  /** Returns the length of the number at the start of the text, or -1 if it is not a number. */
  private int scan(String text) {
    StringScanner scanner = new StringScanner(text);
    IToken token = new LiteralNumberRule(number).evaluate(scanner);
    if (token.isUndefined()) {
      // A rule that does not match must not move the scanner
      assertEquals(0, scanner.offset);
      return -1;
    }
    return scanner.offset;
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
