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
 * or implied. See the License for the specific language governing permissions and limitations
 * under the License.
 */
package org.apache.hop.ui.expression;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.apache.hop.expression.Operator;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension3;
import org.eclipse.jface.text.TextUtilities;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Finds the operator written at an offset of an expression, like {@code +}, {@code <=}, {@code
 * BETWEEN} or {@code IS NOT NULL}, to show its documentation.
 *
 * <pre>{@code
 * Operator operator = OperatorFinder.find(operators, document, caretOffset);
 * }</pre>
 */
@NullMarked
final class OperatorFinder {

  /** A character of a word, a keyword next to it is a part of a longer word. */
  private static final String WORD = "[\\p{L}\\p{N}_]";

  /** A character of a symbol, a symbol next to it is a part of another symbol like {@code ->}. */
  private static final String SYMBOL = "[+\\-*/%|=!<>:]";

  /** The other keywords of an operator, by the name of the operator. */
  private static final Map<String, List<String>> KEYWORDS =
      Map.of("CASE", List.of("WHEN", "THEN", "ELSE", "END"));

  private OperatorFinder() {
    // Utility class
  }

  /**
   * Finds the operator whose name covers an offset, or ends at the offset when the caret is just
   * after the name. The longest name wins: {@code <=} rather than {@code <}, {@code IS NOT NULL}
   * rather than {@code NOT}.
   *
   * <p>The name of an operator is not case-sensitive, its words are separated by spaces with an
   * optional {@code NOT}, so {@code IS NOT NULL} is the operator {@code IS NULL}. An operator in a
   * string or a comment is not found. The keywords {@code WHEN}, {@code THEN}, {@code ELSE} and
   * {@code END} are the operator {@code CASE}.
   *
   * @param operators the operators to find, by their name
   * @param document the document of the expression
   * @param offset the offset in the document, the position of the caret
   * @return the operator, or {@code null} if there is no operator at the offset
   */
  static @Nullable Operator find(Collection<Operator> operators, IDocument document, int offset) {
    String text = document.get();
    try {
      // The caret after the last character of a string or a comment is not in its partition
      String contentType =
          TextUtilities.getContentType(
              document, IDocumentExtension3.DEFAULT_PARTITIONING, Math.max(0, offset - 1), false);
      if (!IDocument.DEFAULT_CONTENT_TYPE.equals(contentType)) {
        return null;
      }
    } catch (BadLocationException e) {
      // The document changed, no operator
      return null;
    }

    Operator found = null;
    int length = 0;
    for (Operator operator : operators) {
      List<String> names = new ArrayList<>();
      names.add(operator.getName());
      names.addAll(KEYWORDS.getOrDefault(operator.getName(), List.of()));
      for (String name : names) {
        Matcher matcher = pattern(name).matcher(text);
        while (matcher.find()) {
          if (matcher.start() <= offset
              && offset <= matcher.end()
              && matcher.end() - matcher.start() > length) {
            found = operator;
            length = matcher.end() - matcher.start();
          }
        }
      }
    }
    return found;
  }

  /** The pattern of the name of an operator, a keyword like BETWEEN or a symbol like <=. */
  private static Pattern pattern(String name) {
    if (Character.isLetter(name.charAt(0))) {
      String words =
          Arrays.stream(name.split(" "))
              .map(Pattern::quote)
              .collect(Collectors.joining("\\s+(?:NOT\\s+)?"));
      return Pattern.compile(
          "(?<!" + WORD + ")" + words + "(?!" + WORD + ")", Pattern.CASE_INSENSITIVE);
    }
    return Pattern.compile("(?<!" + SYMBOL + ")" + Pattern.quote(name) + "(?!" + SYMBOL + ")");
  }
}
