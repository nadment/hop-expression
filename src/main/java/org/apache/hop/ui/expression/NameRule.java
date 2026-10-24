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

import java.util.function.Predicate;
import org.eclipse.jface.text.rules.ICharacterScanner;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.IWordDetector;
import org.eclipse.jface.text.rules.Token;
import org.jspecify.annotations.NullMarked;

/**
 * A rule that returns a token for a word accepted by a predicate. Unlike {@code WordRule}, the
 * accepted words are checked when the text is scanned, so they can change after the rule is
 * created, for example the fields known later or a user-defined function created later.
 */
@NullMarked
public class NameRule implements IRule {

  private final IWordDetector detector;
  private final Predicate<String> predicate;
  private final IToken token;

  /**
   * Creates a rule for the words accepted by a predicate.
   *
   * @param detector the detector of the characters of a word
   * @param predicate the predicate that accepts a word
   * @param token the token returned for an accepted word
   */
  public NameRule(IWordDetector detector, Predicate<String> predicate, IToken token) {
    this.detector = detector;
    this.predicate = predicate;
    this.token = token;
  }

  @Override
  public IToken evaluate(ICharacterScanner scanner) {
    int c = scanner.read();
    if (c == ICharacterScanner.EOF || !detector.isWordStart((char) c)) {
      scanner.unread();
      return Token.UNDEFINED;
    }

    StringBuilder word = new StringBuilder();
    do {
      word.append((char) c);
      c = scanner.read();
    } while (c != ICharacterScanner.EOF && detector.isWordPart((char) c));
    scanner.unread();

    if (predicate.test(word.toString())) {
      return token;
    }

    // Not accepted, let the next rules scan the word
    for (int i = 0; i < word.length(); i++) {
      scanner.unread();
    }
    return Token.UNDEFINED;
  }
}
