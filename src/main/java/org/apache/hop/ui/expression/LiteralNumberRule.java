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
package org.apache.hop.ui.expression;

import java.util.Objects;
import java.util.function.IntPredicate;
import org.apache.hop.expression.util.Characters;
import org.eclipse.jface.text.rules.ICharacterScanner;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;

/**
 * A rule that returns a token for a numeric literal, with the same forms as the lexer: decimal
 * 1_000.5E-3, hexadecimal 0xFF, binary 0b0101 and octal 0o17. An invalid literal is highlighted
 * too, the error is reported by the validation of the expression.
 */
public class LiteralNumberRule implements IRule {
  private final IToken token;

  public LiteralNumberRule(IToken token) {
    this.token = Objects.requireNonNull(token);
  }

  @Override
  public IToken evaluate(ICharacterScanner scanner) {
    int c = scanner.read();
    if (!Characters.isDigit((char) c)) {
      scanner.unread();
      return Token.UNDEFINED;
    }

    if (c == '0') {
      c = scanner.read();
      // Hexadecimal
      if (c == 'x' || c == 'X') {
        readWhile(scanner, ch -> Characters.isHexDigit((char) ch) || ch == '_');
        return token;
      }
      // Binary
      if (c == 'b' || c == 'B') {
        readWhile(scanner, ch -> Characters.isBitDigit((char) ch) || ch == '_');
        return token;
      }
      // Octal
      if (c == 'o' || c == 'O') {
        readWhile(scanner, ch -> Characters.isOctDigit((char) ch) || ch == '_');
        return token;
      }
      scanner.unread();
    }

    // Integer part
    IntPredicate digit = ch -> Characters.isDigit((char) ch) || ch == '_';
    readWhile(scanner, digit);

    // Decimal part
    c = scanner.read();
    if (c == '.') {
      readWhile(scanner, digit);
      c = scanner.read();
    }

    // Exponent part
    if (Characters.isExponent((char) c)) {
      c = scanner.read();
      if (c != '+' && c != '-') {
        scanner.unread();
      }
      readWhile(scanner, digit);
    } else {
      scanner.unread();
    }

    return token;
  }

  /**
   * Reads the characters accepted by a predicate, the first character not accepted is not read.
   *
   * @param scanner the scanner
   * @param predicate the predicate that accepts a character
   */
  private static void readWhile(ICharacterScanner scanner, IntPredicate predicate) {
    int c;
    do {
      c = scanner.read();
    } while (c != ICharacterScanner.EOF && predicate.test(c));
    scanner.unread();
  }
}
