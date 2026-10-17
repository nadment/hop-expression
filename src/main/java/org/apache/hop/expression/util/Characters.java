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

import java.text.Normalizer;
import java.util.regex.Pattern;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

@NullMarked
public final class Characters {

  // Only the combining marks left by the decomposition of the accented letters, not the modifier
  // letters (Lm) and symbols (Sk) which are characters on their own, like ^ or the Japanese ー
  private static final Pattern DIACRITICS = Pattern.compile("\\p{InCombiningDiacriticalMarks}+");

  private static final byte[] FLAGS = new byte[256];
  private static final byte IS_DIGIT = 0x01;
  private static final byte IS_HEX_DIGIT = 0x02;
  private static final byte IS_ALPHA = 0x04;
  private static final byte IS_DELIMITER = 0x08;
  private static final byte IS_OCT_DIGIT = 0x10;
  private static final byte IS_BIT = 0x20;

  static {
    FLAGS['0'] |= IS_BIT;
    FLAGS['1'] |= IS_BIT;

    for (int ch = '0'; ch <= '9'; ch++) {
      FLAGS[ch] |= IS_DIGIT | IS_HEX_DIGIT;
    }
    for (int ch = '0'; ch <= '7'; ch++) {
      FLAGS[ch] |= IS_OCT_DIGIT;
    }
    for (int ch = 'A'; ch <= 'F'; ch++) {
      FLAGS[ch] |= IS_HEX_DIGIT;
    }
    for (int ch = 'a'; ch <= 'f'; ch++) {
      FLAGS[ch] |= IS_HEX_DIGIT;
    }
    for (int ch = 'A'; ch <= 'Z'; ch++) {
      FLAGS[ch] |= IS_ALPHA;
    }
    for (int ch = 'a'; ch <= 'z'; ch++) {
      FLAGS[ch] |= IS_ALPHA;
    }

    char[] delimiters = {
      ' ', '.', ',', ':', ';', '\t', '\'', '?', '!', '@', '"', '^', '#', '$', '&', '~', '_', '=',
      '+', '-', '*', '%', '/', '|', '\\', '[', ']', '(', ')', '{', '}', '<', '>'
    };
    for (int ch : delimiters) {
      FLAGS[ch] |= IS_DELIMITER;
    }
  }

  /** Private constructor since this is a utility class. */
  private Characters() {}

  /**
   * Determines if the specified character is a digit.
   *
   * @param ch the character to check
   */
  public static boolean isDigit(char ch) {
    if (ch > 255) {
      return false;
    }
    return (FLAGS[ch] & IS_DIGIT) != 0;
  }

  public static boolean isBitDigit(char ch) {
    if (ch > 255) {
      return false;
    }
    return (FLAGS[ch] & IS_BIT) != 0;
  }

  public static boolean isHexDigit(char ch) {
    if (ch > 255) {
      return false;
    }
    return (FLAGS[ch] & IS_HEX_DIGIT) != 0;
  }

  public static boolean isOctDigit(char ch) {
    if (ch > 255) {
      return false;
    }
    return (FLAGS[ch] & IS_OCT_DIGIT) != 0;
  }

  /**
   * Determines if the specified character is alphabetic
   *
   * @param ch the character to check
   */
  public static boolean isAlpha(char ch) {
    if (ch > 255) {
      return false;
    }
    return (FLAGS[ch] & IS_ALPHA) != 0;
  }

  /**
   * Determines if the specified character is alphabetic or digit
   *
   * @param ch the character to check
   */
  public static boolean isAlphaOrDigit(char ch) {
    if (ch > 255) {
      return false;
    }
    return (FLAGS[ch] & (IS_ALPHA | IS_DIGIT)) != 0;
  }

  /**
   * Is the character a word delimiter
   *
   * <p>! ? @ " ^ # $ &amp; ~ _ , . : ; = + - * % / | \ [ ] ( ) { } &lt; &gt;
   *
   * @param ch the character to check
   * @return true if it is a delimiter
   */
  public static boolean isWordDelimiter(char ch) {
    if (ch > 255) {
      return false;
    }
    return (FLAGS[ch] & (IS_DELIMITER)) != 0;
  }

  /**
   * Determines if the specified character is space or tab
   *
   * @param ch the character to check
   */
  public static boolean isSpace(char ch) {
    return ch == ' '
        || ch == '\t'
        || ch == '\n'
        || ch == '\r'
        // No-break space
        || ch == '\u00a0'
        // Narrow no-break space
        || ch == '\u202f';
  }

  public static boolean isExponent(char ch) {
    return ch == 'e' || ch == 'E';
  }

  /**
   * Returns the number of characters of a string, a character outside the Basic Multilingual Plane
   * counting as one character although it is stored as a surrogate pair.
   *
   * @param str the string
   * @return the number of code points
   */
  public static int length(final String str) {
    return str.codePointCount(0, str.length());
  }

  /**
   * Returns the index in a string of the character at a 0-based character position, a character
   * outside the Basic Multilingual Plane counting as one character. A position beyond the end
   * returns the length of the string.
   *
   * <p>Example: {@code offset("😀ab", 1)} returns 2, the index of {@code 'a'}.
   *
   * @param str the string
   * @param position the 0-based position, in characters
   * @return the 0-based index in the string, between 0 and the length of the string
   */
  public static int offset(final String str, final long position) {
    int length = str.length();
    int index = 0;
    for (long i = 0; i < position && index < length; i++) {
      index += Character.charCount(str.codePointAt(index));
    }
    return index;
  }

  /**
   * Returns the 0-based character position of an index in a string, a character outside the Basic
   * Multilingual Plane counting as one character.
   *
   * @param str the string
   * @param index the 0-based index in the string
   * @return the 0-based position, in characters
   */
  public static int position(final String str, final int index) {
    return str.codePointCount(0, index);
  }

  /**
   * Returns a string of a number of characters, made of the characters of a pad repeated as many
   * times as necessary. A character outside the Basic Multilingual Plane counts as one character.
   *
   * <p>Example: {@code fill("ab", 5)} returns {@code "ababa"}.
   *
   * @param pad the characters to repeat
   * @param length the number of characters of the result
   * @return the padding, or an empty string if pad is empty
   */
  public static String fill(final String pad, final int length) {
    int[] chars = pad.codePoints().toArray();
    if (chars.length == 0) {
      return "";
    }
    StringBuilder builder = new StringBuilder(length);
    for (int i = 0; i < length; i++) {
      builder.appendCodePoint(chars[i % chars.length]);
    }
    return builder.toString();
  }

  /**
   * Removes the accents (diacritic marks) from a string, the ligatures are left as is.
   *
   * <p>Example: {@code unaccent("Émile")} returns {@code "Emile"}.
   *
   * @param value the string
   * @return the string without accents
   */
  public static String unaccent(final String value) {
    final StringBuilder decomposed =
        new StringBuilder(Normalizer.normalize(value, Normalizer.Form.NFD));

    for (int i = 0; i < decomposed.length(); i++) {
      if (decomposed.charAt(i) == 'Ł') {
        decomposed.deleteCharAt(i);
        decomposed.insert(i, 'L');
      } else if (decomposed.charAt(i) == 'ł') {
        decomposed.deleteCharAt(i);
        decomposed.insert(i, 'l');
      }
    }

    // Note that this doesn't correctly remove ligatures...
    return DIACRITICS.matcher(decomposed).replaceAll("");
  }

  /**
   * Transliterates the Latin letters that have no Unicode decomposition, and so no diacritic mark
   * to remove, into letters A to Z: Ø to O, Æ to AE, Œ to OE, Đ to D, Ħ to H and Þ to TH, in upper
   * and lower case.
   *
   * <p>The string is read in a single pass and is returned as is, without any copy, if it has no
   * letter to transliterate.
   *
   * <p>Example: {@code transliterate("Søren")} returns {@code "Soren"}.
   *
   * @param str the string
   * @return the transliterated string
   */
  public static String transliterate(final String str) {
    @Nullable StringBuilder builder = null;
    for (int i = 0; i < str.length(); i++) {
      char ch = str.charAt(i);
      // Fast path, Æ is the first letter to transliterate
      String latin = (ch < 'Æ') ? null : transliterate(ch);
      if (latin != null) {
        if (builder == null) {
          builder = new StringBuilder(str.length() + 4);
          builder.append(str, 0, i);
        }
        builder.append(latin);
      } else if (builder != null) {
        builder.append(ch);
      }
    }
    return (builder == null) ? str : builder.toString();
  }

  private static @Nullable String transliterate(final char ch) {
    return switch (ch) {
      case 'Æ' -> "AE"; // Æ
      case 'æ' -> "ae"; // æ
      case 'Ø' -> "O"; // Ø
      case 'ø' -> "o"; // ø
      case 'Þ' -> "TH"; // Þ
      case 'þ' -> "th"; // þ
      case 'Đ' -> "D"; // Đ
      case 'đ' -> "d"; // đ
      case 'Ħ' -> "H"; // Ħ
      case 'ħ' -> "h"; // ħ
      case 'Œ' -> "OE"; // Œ
      case 'œ' -> "oe"; // œ
      default -> null;
    };
  }
}
