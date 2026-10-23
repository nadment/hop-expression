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
package org.apache.hop.expression;

import java.util.Collections;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.hop.expression.type.TypeName;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * The reserved words of the expression language, they cannot be used as identifiers without quotes.
 *
 * <p>The core words are the words of the grammar, each one has its own {@link Token.Id}. A function
 * with an extended syntax can register its own words with {@link #register(String)}, the lexer
 * creates a {@link Token.Id#KEYWORD} token for them.
 *
 * <p>Example: a function <code>MY_FUNC(value USING mode)</code> declares the word USING with {@link
 * Function#getKeywords()}, it is registered with the function.
 */
@NullMarked
public final class Keyword {
  private static final Set<String> CORE_WORDS =
      Set.of(
          "AND",
          "ASYMMETRIC",
          "AT",
          "BETWEEN",
          "BINARY",
          "CASE",
          "DATE",
          "DISTINCT",
          "ELSE",
          "END",
          "ESCAPE",
          "FALSE",
          "FROM",
          "ILIKE",
          "IN",
          "INET",
          "INTERVAL",
          "IS",
          "JSON",
          "LIKE",
          "NOT",
          "NULL",
          "OR",
          "SIMILAR",
          "SYMMETRIC",
          "THEN",
          "TIME",
          "TIMESTAMP",
          "TO",
          "TRUE",
          "WHEN",
          "XOR",
          "ZONE");

  /**
   * The words registered by the functions. Concurrent because the functions are registered while
   * expressions are compiled in other threads.
   */
  private static final Set<String> REGISTERED_WORDS = ConcurrentHashMap.newKeySet();

  /** Private constructor since this is a utility class. */
  private Keyword() {}

  /**
   * Returns the set of reserved words used by the expression language, the core words and the words
   * registered by the functions. Reserved words are keywords that cannot be used as identifiers in
   * expressions.
   *
   * @return an unmodifiable set containing all reserved words
   */
  public static Set<String> get() {
    Set<String> words = new HashSet<>(CORE_WORDS);
    words.addAll(REGISTERED_WORDS);
    return Collections.unmodifiableSet(words);
  }

  /**
   * Checks whether the specified name is a reserved word in the expression language. Reserved words
   * are keywords that cannot be used as identifiers in expressions. The comparison is
   * case-insensitive.
   *
   * @param name the name to check, may be null
   * @return true if the name is a reserved word, false if the name is null or not a reserved word
   */
  public static boolean is(@Nullable String name) {
    if (name == null) return false;
    String word = name.toUpperCase(Locale.ROOT);
    return CORE_WORDS.contains(word) || REGISTERED_WORDS.contains(word);
  }

  /**
   * Checks whether the specified name is a core word of the grammar, with its own {@link Token.Id}.
   * The comparison is case-insensitive.
   *
   * @param name the name to check, may be null
   * @return true if the name is a core word
   */
  public static boolean isCore(@Nullable String name) {
    if (name == null) return false;
    return CORE_WORDS.contains(name.toUpperCase(Locale.ROOT));
  }

  /**
   * Registers a reserved word used by the extended syntax of a function (ignore case). Registering
   * a core word or a word already registered by another function has no effect.
   *
   * @param word the word to register, only made of letters and underscores
   * @throws IllegalArgumentException if the word is not made of letters and underscores, or is a
   *     data type or a time unit name that would no longer be recognized
   */
  public static void register(String word) {
    if (word == null
        || word.isEmpty()
        || !word.chars().allMatch(c -> Character.isLetter(c) || c == '_')) {
      throw new IllegalArgumentException("Invalid keyword \"" + word + "\"");
    }
    // The lexer checks the reserved words first, a data type or a time unit would be hidden
    if (!isCore(word) && (TypeName.of(word) != null || TimeUnit.of(word) != null)) {
      throw new IllegalArgumentException(
          "Keyword \"" + word + "\" is a data type or a time unit name");
    }
    if (!isCore(word)) {
      REGISTERED_WORDS.add(word.toUpperCase(Locale.ROOT));
    }
  }
}
