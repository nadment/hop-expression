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
import org.eclipse.jface.text.rules.ICharacterScanner;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;

/** A rule that returns a token for an operator, like {@code +}, {@code <=} or {@code ::}. */
public class OperatorRule implements IRule {
  private final IToken token;

  public OperatorRule(IToken token) {
    this.token = Objects.requireNonNull(token);
  }

  @Override
  public IToken evaluate(ICharacterScanner scanner) {
    int c = scanner.read();

    boolean matched =
        switch (c) {
            // Single char operator
          case '(', ')', ',', '=', '+', '-', '*', '/', '%', '&', '~', '^', '[', ']' -> true;
            // Bitwise |
            // Concat ||
          case '|' -> {
            readIf(scanner, '|');
            yield true;
          }
            // Less or equals <=
            // Less or greater <>
          case '<' -> {
            if (!readIf(scanner, '=')) {
              readIf(scanner, '>');
            }
            yield true;
          }
            // Greater or equals >=
          case '>' -> {
            readIf(scanner, '=');
            yield true;
          }
            // Cast operator ::
          case ':' -> readIf(scanner, ':');
            // Not equal !=
          case '!' -> readIf(scanner, '=');
          default -> false;
        };

    if (matched) {
      return token;
    }

    // put the scanner back to the original position if no match
    scanner.unread();

    return Token.UNDEFINED;
  }

  /**
   * Reads the next character if it is the expected one.
   *
   * @param scanner the scanner
   * @param expected the expected character
   * @return true if the character is read, false if it is left in the scanner
   */
  private static boolean readIf(ICharacterScanner scanner, char expected) {
    if (scanner.read() == expected) {
      return true;
    }
    scanner.unread();
    return false;
  }
}
