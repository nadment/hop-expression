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
import org.apache.hop.expression.util.Characters;
import org.eclipse.jface.text.rules.ICharacterScanner;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;

public class LiteralNumberRule implements IRule {
  private final IToken token;

  public LiteralNumberRule(IToken token) {
    this.token = Objects.requireNonNull(token);
  }

  @Override
  public IToken evaluate(ICharacterScanner scanner) {
    int c = scanner.read();
    if (Character.isDigit((char) c)) {

      if (c == '0') {
        c = scanner.read();
        // Hexa
        if (c == 'x') {
          do {
            c = scanner.read();
          } while (Characters.isHexDigit((char) c));
          return token;
        }

        // Binary
        if (c == 'b') {
          do {
            c = scanner.read();
          } while (c == '0' || c == '1');
          return token;
        }
        scanner.unread();
      }

      // Integer part
      do {
        c = scanner.read();
      } while (Characters.isDigit((char) c));

      if (c == '.') {
        scanner.read();
        // Decimal part
        do {
          c = scanner.read();
        } while (Characters.isDigit((char) c));
      }

      if (c == 'E') {
        c = scanner.read();
        if (c == '+' || c == '-') {
          scanner.read();
        } else scanner.unread();

        do {
          c = scanner.read();
        } while (Characters.isDigit((char) c));
      }

      scanner.unread();
      return token;
    }

    scanner.unread();
    return Token.UNDEFINED;
  }
}
