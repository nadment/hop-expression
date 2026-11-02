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

package org.apache.hop.expression.util;

import java.util.ArrayList;
import java.util.List;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.jspecify.annotations.NullMarked;

/**
 * Matches a value against a LIKE pattern, where '%' matches any sequence of characters and '_'
 * matches exactly one character.
 *
 * <p>The pattern is split at each unescaped '%' into segments of fixed length. The first segment is
 * anchored at the start of the value, the last one at the end, and each middle segment is matched
 * at its leftmost position, which leaves the most room for the segments after it. There is no
 * backtracking, unlike a regular expression where a pattern like '%a%a%a%x' can take an exponential
 * time on a value that doesn't match.
 *
 * <p>The characters are compared by code point, '_' matches one character outside the Basic
 * Multilingual Plane like an emoji.
 *
 * <p>Example:
 *
 * <pre>{@code
 * LikeMatcher matcher = LikeMatcher.compile("a%o", "\\", false);
 * matcher.matches("amigo"); // true
 * }</pre>
 */
@NullMarked
public final class LikeMatcher {
  /** The code point of the '_' wildcard in a segment. */
  private static final int ANY = -1;

  /** The segments between the '%' wildcards, at least one. */
  private final int[][] segments;

  private final boolean ignoreCase;

  private LikeMatcher(int[][] segments, boolean ignoreCase) {
    this.segments = segments;
    this.ignoreCase = ignoreCase;
  }

  /**
   * Compiles a LIKE pattern.
   *
   * @param pattern the LIKE pattern
   * @param escape the escape string, a single character
   * @param ignoreCase whether the case is ignored, for ILIKE
   * @return the matcher of the pattern
   * @throws ExpressionException if the escape string is not a single character, or if the escape
   *     character isn't followed by '_', '%' or itself
   */
  public static LikeMatcher compile(String pattern, String escape, boolean ignoreCase)
      throws ExpressionException {
    if (escape.length() != 1) {
      throw new ExpressionException(ErrorCode.INVALID_ARGUMENT, escape);
    }
    final char escapeChar = escape.charAt(0);

    List<int[]> segments = new ArrayList<>();
    List<Integer> segment = new ArrayList<>();
    int i = 0;
    while (i < pattern.length()) {
      int c = pattern.codePointAt(i);
      if (c == escapeChar) {
        if (i == pattern.length() - 1) {
          throw new ExpressionException(ErrorCode.INVALID_REGEXP_ESCAPE, pattern, i);
        }
        char next = pattern.charAt(i + 1);
        if (next != '_' && next != '%' && next != escapeChar) {
          throw new ExpressionException(ErrorCode.INVALID_REGEXP_ESCAPE, pattern, i);
        }
        segment.add(ignoreCase ? fold(next) : next);
        i += 2;
        continue;
      }
      if (c == '%') {
        segments.add(toArray(segment));
        segment.clear();
      } else if (c == '_') {
        segment.add(ANY);
      } else {
        segment.add(ignoreCase ? fold(c) : c);
      }
      i += Character.charCount(c);
    }
    segments.add(toArray(segment));

    return new LikeMatcher(segments.toArray(new int[0][]), ignoreCase);
  }

  /**
   * Simplifies a LIKE pattern by merging the consecutive unescaped '%' wildcards into a single one.
   * An escaped '%' is kept as is.
   *
   * <p>Example: <code>'%%#%%A%%' ESCAPE '#'</code> is simplified to <code>'%#%%A%'</code>.
   *
   * @param pattern the LIKE pattern
   * @param escape the escape character
   * @return the simplified pattern, equal to the pattern if nothing is simplified
   */
  public static String simplify(String pattern, char escape) {
    int escapeCount = 0;
    int wildcardCount = 0;
    StringBuilder builder = new StringBuilder();
    for (int index = 0; index < pattern.length(); index++) {
      char c = pattern.charAt(index);
      if (c == escape) {
        builder.append(c);
        escapeCount++;
        wildcardCount = 0;
        continue;
      }
      if (c == '%') {
        if (escapeCount % 2 != 0) {
          builder.append('%');
        } else if (wildcardCount == 0) {
          builder.append('%');
          wildcardCount++;
        }
        escapeCount = 0;
        continue;
      }
      builder.append(c);
      escapeCount = 0;
      wildcardCount = 0;
    }
    return builder.toString();
  }

  /**
   * Returns whether the value matches the pattern.
   *
   * @param value the value to match
   * @return {@code true} if the whole value matches the pattern
   */
  public boolean matches(String value) {
    int[] text = value.codePoints().toArray();

    // Without '%' the value must match the single segment exactly
    int[] first = segments[0];
    if (segments.length == 1) {
      return text.length == first.length && matchesAt(first, text, 0);
    }

    // The first segment is anchored at the start and the last one at the end
    int[] last = segments[segments.length - 1];
    int start = first.length;
    int end = text.length - last.length;
    if (end < start || !matchesAt(first, text, 0) || !matchesAt(last, text, end)) {
      return false;
    }

    // Each middle segment is matched at its leftmost position
    for (int index = 1; index < segments.length - 1; index++) {
      int[] segment = segments[index];
      int position = start;
      while (position + segment.length <= end && !matchesAt(segment, text, position)) {
        position++;
      }
      if (position + segment.length > end) {
        return false;
      }
      start = position + segment.length;
    }
    return true;
  }

  /** Returns whether the segment matches the text at the position. */
  private boolean matchesAt(int[] segment, int[] text, int position) {
    for (int i = 0; i < segment.length; i++) {
      int c = segment[i];
      if (c != ANY && c != (ignoreCase ? fold(text[position + i]) : text[position + i])) {
        return false;
      }
    }
    return true;
  }

  /** Folds the case of a code point like {@link String#equalsIgnoreCase(String)}. */
  private static int fold(int c) {
    return Character.toLowerCase(Character.toUpperCase(c));
  }

  private static int[] toArray(List<Integer> codePoints) {
    return codePoints.stream().mapToInt(Integer::intValue).toArray();
  }
}
