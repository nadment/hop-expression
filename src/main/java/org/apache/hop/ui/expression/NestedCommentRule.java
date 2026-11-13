/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.hop.ui.expression;

import java.util.Objects;
import org.eclipse.jface.text.rules.ICharacterScanner;
import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;
import org.jspecify.annotations.NullMarked;

/**
 * A rule that returns a token for a block comment, which can be nested as in the lexer: a block
 * comment inside a block comment does not end the first one. A comment that is not closed continues
 * to the end of the text.
 */
@NullMarked
public class NestedCommentRule implements IPredicateRule {
  private final IToken token;

  /**
   * Creates the rule.
   *
   * @param token the token returned for a block comment
   */
  public NestedCommentRule(IToken token) {
    this.token = Objects.requireNonNull(token);
  }

  @Override
  public IToken getSuccessToken() {
    return token;
  }

  @Override
  public IToken evaluate(ICharacterScanner scanner) {
    return evaluate(scanner, false);
  }

  @Override
  public IToken evaluate(ICharacterScanner scanner, boolean resume) {
    // The nested comments are counted from the start of the comment, the partitioner scans the
    // comment again from its start when the rule does not resume
    if (resume) {
      return Token.UNDEFINED;
    }
    if (scanner.read() != '/') {
      scanner.unread();
      return Token.UNDEFINED;
    }
    if (!readIf(scanner, '*')) {
      scanner.unread();
      return Token.UNDEFINED;
    }

    int level = 1;
    while (level > 0) {
      int c = scanner.read();
      if (c == ICharacterScanner.EOF) {
        scanner.unread();
        break;
      }
      if (c == '/' && readIf(scanner, '*')) {
        level++;
      } else if (c == '*' && readIf(scanner, '/')) {
        level--;
      }
    }
    return token;
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
