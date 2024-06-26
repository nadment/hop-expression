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

public class OperatorRule implements IRule {
  private final IToken token;

  public OperatorRule(IToken token) {
    this.token = Objects.requireNonNull(token);
  }

  @Override
  public IToken evaluate(ICharacterScanner scanner) {
    int c = scanner.read();

    if (c != ICharacterScanner.EOF) {

      // Single char operator
      if (c == '(' || c == ')' || c == ',' || c == '=' || c == '+' || c == '-' || c == '*'
          || c == '/' || c == '%' || c == '&' || c == '~' || c == '^' || c == '[' || c == ']') {
        return token;
      }

      // Bitwise |
      // Concat ||

      if (c == '|') {
        c = scanner.read();
        if (c != '|') {
          scanner.unread();
        }
        return token;
      }

      // Less or equals <=
      // Less or greater <>
      if (c == '<') {
        c = scanner.read();
        if (c != '=' && c != '>') {
          scanner.unread();
        }
        return token;
      }

      if (c == '>') {
        c = scanner.read();
        if (c != '=') {
          scanner.unread();
        }
        return token;
      }

      // Cast operator ::
      if (c == ':') {
        c = scanner.read();
        if (c == ':') {
          return token;
        }
        scanner.unread();
      }

      // Not equal !=
      if (c == '!') {
        c = scanner.read();
        if (c == '=') {
          return token;
        }
        scanner.unread();
      }
    }

    // put the scanner back to the original position if no match
    scanner.unread();

    return Token.UNDEFINED;
  }
}
