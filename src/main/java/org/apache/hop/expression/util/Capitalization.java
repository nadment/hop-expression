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
package org.apache.hop.expression.util;

import org.apache.commons.lang3.StringUtils;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** Represents a capitalization strategy. */
@NullMarked
public enum Capitalization {

  /** All letters are upper case. */
  UPPER,

  /** All letters are lower case. */
  LOWER,

  /** The string is capitalized (first letter upper case, subsequent letters lower case). */
  CAPITALIZE;

  /**
   * Returns the capitalization strategy which should be used when the first and second letters have
   * the specified casing.
   *
   * @param up1 whether the first letter is upper case
   * @param up2 whether the second letter is upper case
   * @return the capitalization strategy
   */
  static Capitalization of(@Nullable Boolean up1, @Nullable Boolean up2) {
    if (up1 == null) {
      return Capitalization.CAPITALIZE;
    } else if (up2 == null) {
      return up1 ? Capitalization.UPPER : Capitalization.LOWER;
    } else if (up1) {
      return up2 ? Capitalization.UPPER : Capitalization.CAPITALIZE;
    } else {
      return Capitalization.LOWER;
    }
  }

  /**
   * Returns a capitalization strategy if the specified string contains any of the specified
   * substrings at the specified index. The capitalization strategy indicates the casing of the
   * substring that was found. If none of the specified substrings are found, this method returns
   * <code>null</code> .
   *
   * @param s the string to check
   * @param index the index to check at
   * @param substrings the substrings to check for within the string
   * @return a capitalization strategy if the specified string contains any of the specified
   *     substrings at the specified index, <code>null</code> otherwise
   */
  static @Nullable Capitalization match(String s, int index, String... substrings) {
    for (String substring : substrings) {
      if (index + substring.length() <= s.length()) {
        boolean found = true;
        Boolean up1 = null;
        Boolean up2 = null;
        for (int i = 0; i < substring.length(); i++) {
          char c1 = s.charAt(index + i);
          char c2 = substring.charAt(i);
          if (c1 != c2 && Character.toUpperCase(c1) != Character.toUpperCase(c2)) {
            found = false;
            break;
          } else if (Character.isLetter(c1)) {
            if (up1 == null) {
              up1 = Character.isUpperCase(c1);
            } else if (up2 == null) {
              up2 = Character.isUpperCase(c1);
            }
          }
        }
        if (found) {
          return Capitalization.of(up1, up2);
        }
      }
    }
    return null;
  }

  /**
   * Applies this capitalization strategy to the specified string.
   *
   * @param str the string to apply this strategy to
   * @return the resultant string
   */
  public @Nullable String apply(final @Nullable String str) {
    if (str == null || str.isEmpty()) {
      return str;
    }
    return switch (this) {
      case UPPER -> StringUtils.upperCase(str);
      case LOWER -> StringUtils.lowerCase(str);
      case CAPITALIZE ->
          Character.toUpperCase(str.charAt(0))
              + (str.length() > 1 ? StringUtils.lowerCase(str).substring(1) : "");
    };
  }
}
