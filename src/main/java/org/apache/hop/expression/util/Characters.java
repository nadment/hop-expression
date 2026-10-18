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

import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.StandardCharsets;
import java.text.Normalizer;
import java.util.HashMap;
import java.util.Map;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

@NullMarked
public final class Characters {

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
   * @param codePoint the character to check, a code point so that a character outside the Basic
   *     Multilingual Plane is not truncated to a delimiter
   * @return true if it is a delimiter
   */
  public static boolean isWordDelimiter(int codePoint) {
    // The word delimiters are all ASCII characters
    if (codePoint > 255) {
      return false;
    }
    return (FLAGS[codePoint] & (IS_DELIMITER)) != 0;
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
   * Decodes UTF-8 bytes to a string, raising an error for an invalid sequence instead of replacing
   * it with U+FFFD like {@code new String(bytes, UTF_8)}.
   *
   * @param bytes the bytes to decode
   * @param encoding the name of the decoded encoding, such as base64, for the error message
   * @return the string
   * @throws ExpressionException with {@link ErrorCode#INVALID_UTF8_SEQUENCE} if not valid UTF-8
   */
  public static String decodeUtf8(final byte[] bytes, final String encoding) {
    return decodeUtf8(bytes, 0, bytes.length, encoding);
  }

  /**
   * Decodes a range of UTF-8 bytes to a string, raising an error for an invalid sequence.
   *
   * @param bytes the bytes to decode
   * @param offset the index of the first byte
   * @param length the number of bytes
   * @param encoding the name of the decoded encoding, such as base64, for the error message
   * @return the string
   * @throws ExpressionException with {@link ErrorCode#INVALID_UTF8_SEQUENCE} if not valid UTF-8
   */
  public static String decodeUtf8(
      final byte[] bytes, final int offset, final int length, final String encoding) {
    try {
      // A new decoder reports the malformed sequences, it is not thread-safe
      return StandardCharsets.UTF_8
          .newDecoder()
          .decode(ByteBuffer.wrap(bytes, offset, length))
          .toString();
    } catch (CharacterCodingException e) {
      throw new ExpressionException(ErrorCode.INVALID_UTF8_SEQUENCE, encoding);
    }
  }

  /**
   * Removes the leading and trailing characters of a string that are in a set of characters, a
   * character outside the Basic Multilingual Plane counting as one character.
   *
   * <p>Example: {@code strip("xxaxx", "x")} returns {@code "a"}.
   *
   * @param str the string
   * @param chars the characters to remove, or {@code null} to remove the white spaces
   * @return the string without the leading and trailing characters
   */
  public static String strip(final String str, final @Nullable String chars) {
    // A single substring, the string is returned as is if there is nothing to remove
    int start = startIndex(str, chars);
    int end = start == str.length() ? start : endIndex(str, chars);
    return str.substring(start, end);
  }

  /**
   * Removes the leading characters of a string that are in a set of characters, a character outside
   * the Basic Multilingual Plane counting as one character.
   *
   * <p>Example: {@code stripStart("xxaxx", "x")} returns {@code "axx"}.
   *
   * @param str the string
   * @param chars the characters to remove, or {@code null} to remove the white spaces
   * @return the string without the leading characters
   */
  public static String stripStart(final String str, final @Nullable String chars) {
    return str.substring(startIndex(str, chars));
  }

  /**
   * Removes the trailing characters of a string that are in a set of characters, a character
   * outside the Basic Multilingual Plane counting as one character.
   *
   * <p>Example: {@code stripEnd("xxaxx", "x")} returns {@code "xxa"}.
   *
   * @param str the string
   * @param chars the characters to remove, or {@code null} to remove the white spaces
   * @return the string without the trailing characters
   */
  public static String stripEnd(final String str, final @Nullable String chars) {
    return str.substring(0, endIndex(str, chars));
  }

  /** Returns the index of the first character that is not removed, the length if none. */
  private static int startIndex(final String str, final @Nullable String chars) {
    int start = 0;
    while (start < str.length()) {
      int codePoint = str.codePointAt(start);
      if (!isStripped(codePoint, chars)) {
        break;
      }
      start += Character.charCount(codePoint);
    }
    return start;
  }

  /** Returns the index after the last character that is not removed, 0 if none. */
  private static int endIndex(final String str, final @Nullable String chars) {
    int end = str.length();
    while (end > 0) {
      int codePoint = str.codePointBefore(end);
      if (!isStripped(codePoint, chars)) {
        break;
      }
      end -= Character.charCount(codePoint);
    }
    return end;
  }

  private static boolean isStripped(final int codePoint, final @Nullable String chars) {
    return (chars == null) ? Character.isWhitespace(codePoint) : chars.indexOf(codePoint) >= 0;
  }

  /**
   * Returns the index of the first occurrence of a substring from an index, like {@link
   * String#indexOf(String, int)}, but an occurrence that starts or ends in the middle of a
   * surrogate pair is ignored.
   *
   * @param str the string to search in
   * @param substr the substring to search for
   * @param from the index to start the search from
   * @return the index of the occurrence, or -1 if not found
   */
  public static int indexOf(final String str, final String substr, final int from) {
    int index = str.indexOf(substr, from);
    while (index >= 0 && isSplittingPair(str, index, index + substr.length())) {
      index = str.indexOf(substr, index + 1);
    }
    return index;
  }

  /**
   * Returns the index of the last occurrence of a substring searching backward from an index, like
   * {@link String#lastIndexOf(String, int)}, but an occurrence that starts or ends in the middle of
   * a surrogate pair is ignored.
   *
   * @param str the string to search in
   * @param substr the substring to search for
   * @param from the index to start the search backward from
   * @return the index of the occurrence, or -1 if not found
   */
  public static int lastIndexOf(final String str, final String substr, final int from) {
    int index = str.lastIndexOf(substr, from);
    while (index > 0 && isSplittingPair(str, index, index + substr.length())) {
      index = str.lastIndexOf(substr, index - 1);
    }
    // The index 0 cannot be in the middle of a surrogate pair, but its end can
    if (index == 0 && isSplittingPair(str, 0, substr.length())) {
      return -1;
    }
    return index;
  }

  private static boolean isSplittingPair(final String str, final int start, final int end) {
    return isInsidePair(str, start) || isInsidePair(str, end);
  }

  private static boolean isInsidePair(final String str, final int index) {
    return index > 0
        && index < str.length()
        && Character.isHighSurrogate(str.charAt(index - 1))
        && Character.isLowSurrogate(str.charAt(index));
  }

  /**
   * Maps two strings to strings where each character, including a character outside the Basic
   * Multilingual Plane, is a single {@code char}. The same character is mapped to the same {@code
   * char} in both strings, so an algorithm that only compares the {@code char} values, like an edit
   * distance, counts a surrogate pair as one character. The strings are returned as is if they have
   * no surrogate pair.
   *
   * @param str1 the first string
   * @param str2 the second string
   * @return the two mapped strings
   */
  public static String[] toSingleChars(final String str1, final String str2) {
    if (str1.codePointCount(0, str1.length()) == str1.length()
        && str2.codePointCount(0, str2.length()) == str2.length()) {
      return new String[] {str1, str2};
    }
    // Each distinct character gets the next char value, more than 65536 distinct characters would
    // wrap, far beyond the strings an edit distance can compare
    Map<Integer, Character> mapping = new HashMap<>();
    return new String[] {toSingleChars(str1, mapping), toSingleChars(str2, mapping)};
  }

  private static String toSingleChars(final String str, final Map<Integer, Character> mapping) {
    StringBuilder builder = new StringBuilder(str.length());
    str.codePoints()
        .forEach(
            codePoint ->
                builder.append(mapping.computeIfAbsent(codePoint, key -> (char) mapping.size())));
    return builder.toString();
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
    // Fast path, a character before À has no canonical decomposition and is not a combining mark,
    // so a string like an ASCII string is returned without any copy
    int index = 0;
    while (index < value.length() && value.charAt(index) < 'À') {
      index++;
    }
    if (index == value.length()) {
      return value;
    }

    final String decomposed = Normalizer.normalize(value, Normalizer.Form.NFD);
    final StringBuilder builder = new StringBuilder(decomposed.length());
    for (int i = 0; i < decomposed.length(); i++) {
      char ch = decomposed.charAt(i);
      // Only the combining marks left by the decomposition of the accented letters (block
      // Combining Diacritical Marks), not the modifier letters (Lm) and symbols (Sk) which are
      // characters on their own, like ^ or the Japanese ー
      if (ch >= '̀' && ch <= 'ͯ') {
        continue;
      }
      // The letters Ł and ł have no decomposition
      if (ch == 'Ł') {
        ch = 'L';
      } else if (ch == 'ł') {
        ch = 'l';
      }
      builder.append(ch);
    }

    // Note that this doesn't correctly remove ligatures...
    // Recompose the characters without accents decomposed by NFD, like the Korean syllables or
    // the Japanese kana with a voiced sound mark
    return Normalizer.normalize(builder, Normalizer.Form.NFC);
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
