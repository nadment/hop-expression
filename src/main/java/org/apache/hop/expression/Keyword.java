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

import java.util.Set;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

@NullMarked
public class Keyword {
  private static final Set<String> WORDS =
      Set.of(
          "AND",
          "AS",
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
          "FORMAT",
          "FROM",
          "IGNORE",
          "ILIKE",
          "IN",
          "INET",
          "INTERVAL",
          "IS",
          "JSON",
          "KEY",
          "LIKE",
          "NOT",
          "NULL",
          "NULLS",
          "OR",
          "RESPECT",
          "RETURNING",
          "RLIKE",
          "SIMILAR",
          "SYMMETRIC",
          "THEN",
          "TIME",
          "TIMESTAMP",
          "TO",
          "TRUE",
          "VALUE",
          "WHEN",
          "XOR",
          "ZONE");

  /** Private constructor since this is a utility class. */
  private Keyword() {}

  /**
   * Returns the set of reserved words used by the expression language. Reserved words are keywords
   * that cannot be used as identifiers in expressions.
   *
   * @return an unmodifiable set containing all reserved words
   */
  public static Set<String> get() {
    return WORDS;
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
    return WORDS.contains(name.toUpperCase());
  }
}
