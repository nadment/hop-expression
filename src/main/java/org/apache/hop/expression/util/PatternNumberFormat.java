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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.text.DecimalFormatSymbols;
import java.util.Locale;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.commons.lang3.StringUtils;
import org.apache.hop.expression.ExpressionException;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Expression number format model for <code>TO_NUMBER(string, format)</code> and <code>
 * TO_CHAR(number, format)</code> functions.
 *
 * <p>
 *
 * <table border="1">
 * <th>
 * <td>Input</td>
 * <td>Output</td></th>
 * <tr>
 * <td>, (comma)</td>
 * <td>Grouping separator.</td>
 * <td>,</td>
 * </tr>
 * <tr>
 * <td>. (periode)</td>
 * <td>Decimal separator.</td>
 * <td>.</td>
 * </tr>
 * <tr>
 * <td>$</td>
 * <td>Leading dollar sign.</td>
 * <td>$</td>
 * </tr>
 * <tr>
 * <td>0</td>
 * <td>Leading or trailing zeroes.</td>
 * <td>0</td>
 * </tr>
 * <tr>
 * <td>9</td>
 * <td>Digit.</td>
 * <td>#</td>
 * </tr>
 * <tr>
 * <td>B</td>
 * <td>Returns blanks when the value is zero.</td>
 * <td>#</td>
 * </tr>
 * <tr>
 * <td>C</td>
 * <td>ISO currency abbreviation.</td>
 * <td>\u00A4</td>
 * </tr>
 * <tr>
 * <td>D</td>
 * <td>Local decimal separator.</td>
 * <td>.</td>
 * </tr>
 * <tr>
 * <td>EEEE</td>
 * <td>Returns a value in scientific notation (case sensitive).</td>
 * <td>E</td>
 * </tr>
 * <tr>
 * <td>FM</td>
 * <td>Returns values with no leading or trailing spaces.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>G</td>
 * <td>Local grouping separator.</td>
 * <td>,</td>
 * </tr>
 * <tr>
 * <td>L</td>
 * <td>Local currency symbol.</td>
 * <td>\u00A4</td>
 * </tr>
 * <tr>
 * <td>MI</td>
 * <td>Negative values get trailing minus sign, positive get trailing space.</td>
 * <td>-</td>
 * </tr>
 * <tr>
 * <td>PR</td>
 * <td>Negative values get enclosing angle brackets, positive get spaces.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>RN</td>
 * <td>Returns values in Roman numerals (case-sensitive).</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>S</td>
 * <td>Returns values with leading/trailing +/- signs.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>TM</td>
 * <td>Returns smallest number of characters possible.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>U</td>
 * <td>Returns the dual currency symbol.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>V</td>
 * <td>Returns a value multiplied by 10^n.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>X</td>
 * <td>Hex value.</td>
 * <td>None.</td>
 * </tr>
 * </table>
 */
@NullMarked
final class PatternNumberFormat extends NumberFormat {

  // Characters always preserved as-is at their position in the format
  private static final String LITERALS = ";:-=/()";

  // Symbols of the default locale, never modified
  private static final AtomicReference<@Nullable DecimalFormatSymbols> CACHED_SYMBOLS =
      new AtomicReference<>();

  // Original format
  private final String format;
  private final String pattern;
  // Fill mode suppress padding blanks and zeroes.
  private boolean fillMode = true;
  private boolean blank = false;
  // number of digits to the left of the decimal separator
  private int precision = 0;
  // number of digits to the right of the decimal separator
  private int scale = 0;
  // scientific fixed-width exponent
  private int scientific = 0;
  // decimal separator position in pattern
  private int separator = 0;
  // use local symbols for grouping (G) or decimal separator (D)
  private boolean localSymbols = true;
  private CurrencyMode currency = CurrencyMode.NONE;
  private SignMode sign = SignMode.DEFAULT;
  private int v = 0;

  PatternNumberFormat(final String format) {

    this.format = format;
    int index = parsePrefix();
    // A trailing sign detected by the prefix is not part of the digits
    int length = format.length() - trailingSignLength();

    StringBuilder builder = new StringBuilder();
    index = parseIntegerPart(builder, index, length);

    //    if (startsWithIgnoreCase(format, index, "RN")) {
    //      builder.append(format.substring(index, index + 2));
    //      index += 2;
    //    } else
    if (startsWithIgnoreCase(format, index, "V")) {
      index = parseShiftedDigits(builder, index + 1, length);
    } else if (startsWithIgnoreCase(format, index, ".", "D")) {
      index = parseDecimalPart(builder, index, length);
    }

    index = parseSuffix(index);

    if (index < format.length()) {
      throw createInvalidFormat(format);
    }

    // Pattern can be empty example: 'FMC'
    this.pattern = builder.toString();
  }

  /**
   * Parses the fill mode, the sign, the blank mode and the currency before the digits.
   *
   * @return the position in the format after them
   */
  private int parsePrefix() {
    int index = 0;

    if (startsWithIgnoreCase(format, index, "FM")) {
      this.fillMode = false;
      index += 2;
    }

    // S element can appear only in the first or last position of a number format.
    if (startsWithIgnoreCase(format, index, "S")) {
      this.sign = SignMode.S_LEADING;
      index += 1;
    } else if (endsWithIgnoreCase(format, "S")) {
      this.sign = SignMode.TRAILING_S;
    } else if (startsWithIgnoreCase(format, index, "MI")) {
      this.sign = SignMode.MI_LEADING;
      index += 2;
    }
    // MI element can appear only in the last position of a number format.
    else if (endsWithIgnoreCase(format, "MI")) {
      this.sign = SignMode.TRAILING_MI;
    }
    // PR element can appear only in the last position of a number format.
    else if (endsWithIgnoreCase(format, "PR")) {
      this.sign = SignMode.PR;
    }

    // Blank mode returns blanks when the value is zero
    if (startsWithIgnoreCase(format, index, "B")) {
      this.blank = true;
      index += 1;
    }

    // Prefix dollars currency
    if (startsWithIgnoreCase(format, index, "$")) {
      this.currency = CurrencyMode.DOLLARS;
      index++;
    }
    // Prefix local currency symbol
    else if (startsWithIgnoreCase(format, index, "L")) {
      this.currency = CurrencyMode.LOCAL_LEADING;
      index++;
    }
    // Prefix ISO currency abbreviation
    else if (startsWithIgnoreCase(format, index, "C")) {
      this.currency = CurrencyMode.ISO_LEADING;
      index++;
    }

    return index;
  }

  /** Returns the length of the trailing sign found by {@link #parsePrefix()}, or 0. */
  private int trailingSignLength() {
    return switch (this.sign) {
      case TRAILING_S -> 1;
      case TRAILING_MI, PR -> 2;
      default -> 0;
    };
  }

  /**
   * Parses the integer part: digits, grouping separators, literals and hexadecimal digits.
   *
   * @param builder the pattern being built
   * @param index the position in the format
   * @param length the end of the digits in the format
   * @return the position in the format after the integer part
   */
  private int parseIntegerPart(StringBuilder builder, int index, int length) {
    boolean leadZero = false;
    for (; index < length; index++, this.separator++) {
      char c = format.charAt(index);
      if (c == 'G' || c == 'g' || c == ',') {
        appendGroupingSeparator(builder, c);
      } else if (c == '0' || c == '9') {
        // any 9s to the left of the decimal separator but to the right of a
        // 0 behave the same as a 0, e.g. "09999.99" -> "00000.99"
        leadZero |= (c == '0');
        builder.append(leadZero ? '0' : '9');
        this.precision++;
      } else if (isLiteral(c)) {
        builder.append(c);
      } else if (c == 'X' || c == 'x') {
        index = parseHexDigits(builder, index, length, c == 'X');
      } else break;
    }
    return index;
  }

  /** Returns whether the pattern being built already has a grouping separator G or comma. */
  private static boolean hasGroupingSeparator(StringBuilder builder) {
    return builder.indexOf("G") >= 0 || builder.indexOf(",") >= 0;
  }

  /**
   * Appends the grouping separator G (local symbol) or comma, they can't be mixed.
   *
   * @param builder the pattern being built
   * @param c the grouping separator of the format
   */
  private void appendGroupingSeparator(StringBuilder builder, char c) {
    boolean local = (c != ',');
    if (hasGroupingSeparator(builder) && localSymbols != local) {
      throw createInvalidFormat(format);
    }
    localSymbols = local;
    builder.append(local ? 'G' : ',');
  }

  /**
   * Parses consecutive hexadecimal digits X.
   *
   * @return the position in the format of the character after the last digit
   */
  private int parseHexDigits(StringBuilder builder, int index, int length, boolean upper) {
    for (; index < length; index++) {
      builder.append(upper ? 'X' : 'x');
      this.precision++;
      this.separator++;
      char c = format.charAt(index);
      if (c != 'X' && c != 'x') break;
    }
    return index;
  }

  /**
   * Parses the digits after V, which are digits of the shifted integer part.
   *
   * @return the position in the format after the digits
   */
  private int parseShiftedDigits(StringBuilder builder, int index, int length) {
    for (; index < length; index++, this.v++) {
      char c = format.charAt(index);
      if (c != '0' && c != '9') break;
      // The digits after V are digits of the shifted integer part
      builder.append(c);
      this.precision++;
      this.separator++;
    }
    return index;
  }

  /**
   * Parses the decimal separator, '.' or 'D', and the digits of the decimal part.
   *
   * @return the position in the format after the decimal part
   */
  private int parseDecimalPart(StringBuilder builder, int index, int length) {
    char c = Character.toUpperCase(format.charAt(index++));
    if (hasGroupingSeparator(builder)) {
      // The decimal separator must use the same symbols as the grouping separator
      if (localSymbols != (c == 'D')) {
        throw createInvalidFormat(format);
      }
    } else {
      localSymbols = (c == 'D');
    }
    builder.append(c);

    int zero = lastZero(index, length);
    for (; index < length; index++) {
      c = format.charAt(index);
      if (isLiteral(c)) {
        builder.append(c);
        continue;
      }
      if (c == '9') {
        if (index < zero) c = '0';
      } else if (c != '0') break;
      builder.append(c);
      this.scale++;
    }

    // for (index++; index < length; index++, this.scale++) {
    // c = format.charAt(index);
    // if (c != '0' && c != '9')
    // break;
    // builder.append('0');
    // }
    return index;
  }

  /** Returns the position of the last 0 of the decimal part, or its start if it has none. */
  private int lastZero(int index, int length) {
    int zero = index;
    for (int i = index; i < length; i++) {
      char c = format.charAt(i);
      if (c == '0') zero = i;
      else if (c != '9' && !isLiteral(c)) break;
    }
    return zero;
  }

  /**
   * Parses the scientific notation, the currency and the sign after the digits.
   *
   * @return the position in the format after them
   */
  private int parseSuffix(int index) {
    // Scientific notation
    if (startsWithIgnoreCase(format, index, "EEEE")) {
      this.scientific = 4;
      index += 4;
    }

    // Dollars currency symbol
    if (startsWithIgnoreCase(format, index, "$")) {
      this.currency = CurrencyMode.DOLLARS;
      index += 1;
    }
    // Local currency symbol
    else if (startsWithIgnoreCase(format, index, "L")) {
      this.currency = CurrencyMode.TRAILING_LOCAL;
      index += 1;
    }
    // ISO currency abbreviation
    else if (startsWithIgnoreCase(format, index, "C")) {
      this.currency = CurrencyMode.TRAILING_ISO;
      index += 1;
    }

    // Sign
    if (startsWithIgnoreCase(format, index, "S")) {
      this.sign = SignMode.TRAILING_S;
      index += 1;
    } else if (startsWithIgnoreCase(format, index, "MI")) {
      this.sign = SignMode.TRAILING_MI;
      index += 2;
    } else if (startsWithIgnoreCase(format, index, "PR")) {
      this.sign = SignMode.PR;
      index += 2;
    }

    return index;
  }

  /**
   * Gets the symbols of the default locale. Creating the symbols is slow, so they are kept until
   * the default locale changes.
   */
  private static DecimalFormatSymbols getSymbols() {
    Locale locale = Locale.getDefault();
    DecimalFormatSymbols symbols = CACHED_SYMBOLS.get();
    if (symbols == null || !locale.equals(symbols.getLocale())) {
      symbols = DecimalFormatSymbols.getInstance(locale);
      CACHED_SYMBOLS.set(symbols);
    }
    return symbols;
  }

  @Override
  public String toString() {
    return format;
  }

  @Override
  public boolean equals(@Nullable Object obj) {
    if (obj == null) {
      return false;
    }
    if (this == obj) {
      return true;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    PatternNumberFormat other = (PatternNumberFormat) obj;
    return format.equals(other.format);
  }

  @Override
  public int hashCode() {
    return format.hashCode();
  }

  /**
   * Parses text from a string to produce a <code>Number</code>.
   *
   * @param text the string to be parsed
   * @return the parsed value
   */
  public BigDecimal parse(final String text) throws ExpressionException {

    int start = 0; // first not white space symbol
    try {
      int end = text.length(); // length of parsed string

      // Skip start space
      while (start < end && Characters.isSpace(text.charAt(start))) start++;

      // Skip end space
      while (start < end && Characters.isSpace(text.charAt(end - 1))) end--;

      // Detect sign, the leading and the trailing sign are found from the same bounds
      boolean isNegative = isNegative(text, start, end);
      int signEnd = skipTrailingSign(text, start, end);
      start = skipLeadingSign(text, start, end);
      end = signEnd;

      DecimalFormatSymbols symbols = getSymbols();
      start = skipLeadingCurrency(text, start, symbols);
      end = skipTrailingCurrency(text, end, symbols);

      // Scientific notation, the exponent follows the last E
      int exponent = 0;
      if (this.scientific > 0) {
        int e = exponentIndex(text, start, end);
        exponent = Integer.parseInt(text.substring(e + 1, end));
        end = e;
      }

      // Hex
      if (pattern.indexOf('X') >= 0 || pattern.indexOf('x') >= 0) {
        String str = text.substring(start, end);
        BigInteger bigInt = new BigInteger(str, 16);

        // Keep the sign already consumed
        return new BigDecimal(isNegative ? bigInt.negate() : bigInt);
      }

      StringBuilder digits = new StringBuilder();

      char decimalSeparator = (this.localSymbols) ? symbols.getDecimalSeparator() : '.';
      int dot = decimalSeparatorIndex(text, decimalSeparator);

      // integer part
      parseIntegerDigits(text, start, (dot < 0 ? end : dot), symbols, digits);

      // fraction part
      int fraction = (dot < 0) ? 0 : parseFractionDigits(text, dot, end, digits);

      if (isNegative) {
        digits.insert(0, '-');
      }
      String str = digits.toString();

      return new BigDecimal(new BigInteger(str), fraction).scaleByPowerOfTen(exponent);
    } catch (ExpressionException exception) {
      // Keep the position of the error
      throw exception;
    } catch (RuntimeException exception) {
      throw createUnparsableNumber(format, text, start);
    }
  }

  /** Returns whether the text without spaces between start and end has a negative sign. */
  private boolean isNegative(String text, int start, int end) {
    return switch (this.sign) {
      case PR -> isEnclosed(text, start, end);
      case TRAILING_MI, TRAILING_S -> text.charAt(end - 1) == '-';
      case MI_LEADING, S_LEADING, DEFAULT -> text.charAt(start) == '-';
    };
  }

  /** Returns whether the text between start and end is enclosed in angle brackets. */
  private static boolean isEnclosed(String text, int start, int end) {
    return text.charAt(start) == '<' && text.charAt(end - 1) == '>';
  }

  /**
   * Skips the leading sign.
   *
   * @return the start of the text after the sign
   */
  private int skipLeadingSign(String text, int start, int end) {
    switch (this.sign) {
      case PR:
        if (isEnclosed(text, start, end)) start++;
        break;
      case MI_LEADING:
        if (isPlusOrMinus(text.charAt(start))) {
          start++;
          // Skip start space
          while (start < end && Characters.isSpace(text.charAt(start))) start++;
        }
        break;
      case S_LEADING, DEFAULT:
        // A missing sign is a positive value
        if (isPlusOrMinus(text.charAt(start))) start++;
        break;
      default:
        break;
    }
    return start;
  }

  /**
   * Skips the trailing sign.
   *
   * @return the end of the text before the sign
   */
  private int skipTrailingSign(String text, int start, int end) {
    switch (this.sign) {
      case PR:
        if (isEnclosed(text, start, end)) end--;
        break;
      case TRAILING_MI:
        if (text.charAt(end - 1) == '-') {
          end--;
          // Skip end space
          while (start < end && Characters.isSpace(text.charAt(end - 1))) end--;
        }
        break;
      case TRAILING_S:
        // A missing sign is a positive value
        if (isPlusOrMinus(text.charAt(end - 1))) end--;
        break;
      default:
        break;
    }
    return end;
  }

  private static boolean isPlusOrMinus(char c) {
    return c == '-' || c == '+';
  }

  /**
   * Skips the leading currency symbol, if present.
   *
   * @return the start of the text after the symbol
   */
  private int skipLeadingCurrency(String text, int start, DecimalFormatSymbols symbols) {
    String symbol;
    switch (this.currency) {
      case LOCAL_LEADING:
        symbol = symbols.getCurrencySymbol();
        if (text.regionMatches(start, symbol, 0, symbol.length())) start += symbol.length();
        break;
      case ISO_LEADING:
        symbol = symbols.getCurrency().getCurrencyCode();
        if (text.regionMatches(start, symbol, 0, symbol.length())) start += symbol.length();
        break;
      case DOLLARS:
        if (text.charAt(start) == '$') start++;
        break;
      default:
        break;
    }
    return start;
  }

  /**
   * Skips the trailing currency symbol, if present.
   *
   * @return the end of the text before the symbol
   */
  private int skipTrailingCurrency(String text, int end, DecimalFormatSymbols symbols) {
    String symbol;
    switch (this.currency) {
      case TRAILING_LOCAL:
        symbol = symbols.getCurrencySymbol();
        if (text.regionMatches(end - symbol.length(), symbol, 0, symbol.length()))
          end -= symbol.length();
        break;
      case TRAILING_ISO:
        symbol = symbols.getCurrency().getCurrencyCode();
        if (text.regionMatches(end - symbol.length(), symbol, 0, symbol.length()))
          end -= symbol.length();
        break;
      default:
        break;
    }
    return end;
  }

  /**
   * Returns the position of the last E of the text, before the exponent.
   *
   * @throws ExpressionException if the text has no exponent
   */
  private int exponentIndex(String text, int start, int end) {
    int e = end - 1;
    while (e > start && Character.toUpperCase(text.charAt(e)) != 'E') e--;
    if (e <= start) {
      throw createUnparsableNumber(format, text, start);
    }
    return e;
  }

  /** Returns the position of the decimal separator in the text, or -1. */
  private int decimalSeparatorIndex(String text, char decimalSeparator) {
    // Without a decimal separator in the format, a separator that can be a grouping separator is
    // not the decimal separator, for example the comma of '12,345' with the format '99G999' in
    // a French locale
    boolean hasDecimal = pattern.indexOf('.') >= 0 || pattern.indexOf('D') >= 0;
    boolean hasGroup = pattern.indexOf('G') >= 0 || pattern.indexOf(',') >= 0;
    return (hasDecimal || !hasGroup) ? text.indexOf(decimalSeparator) : -1;
  }

  /**
   * Parses the integer part of the text, from right to left with the pattern.
   *
   * @param text the text
   * @param start the start of the integer part
   * @param end the end of the integer part
   * @param symbols the symbols of the default locale
   * @param digits the digits of the number
   * @throws ExpressionException if a character doesn't match the pattern
   */
  private void parseIntegerDigits(
      String text, int start, int end, DecimalFormatSymbols symbols, StringBuilder digits) {
    char groupingSeparator = (this.localSymbols) ? symbols.getGroupingSeparator() : ',';
    int j = this.separator;
    // Leading digit positions left blank, as output in fill mode
    boolean leadingBlank = false;
    for (int i = end - 1; i >= start; i--) {
      char c = text.charAt(i);
      j = Math.max(j - 1, 0);
      char p = pattern.charAt(j);
      if (p == '0' || p == '9') {
        if (!leadingBlank && Characters.isDigit(c)) {
          digits.insert(0, c);
          continue;
        }
        if (p == '9' && c == ' ') {
          leadingBlank = true;
          continue;
        }
      } else if (matchesSymbol(p, c, groupingSeparator)) {
        continue;
      }

      throw createUnparsableNumber(format, text, i);
    }
  }

  /**
   * Returns whether a character of the text matches a literal or a grouping separator of the
   * pattern.
   */
  private static boolean matchesSymbol(char p, char c, char groupingSeparator) {
    if (isLiteral(p)) {
      return c == p;
    }
    if (p == ',') {
      return c == ',';
    }
    return p == 'G'
        && (c == groupingSeparator
            // If grouping is non-breaking space or narrow non-breaking space (special case for
            // fr-FR locale)
            || (Characters.isSpace(groupingSeparator) && Characters.isSpace(c))
            || c == ',');
  }

  /**
   * Parses the fraction part of the text, after the decimal separator.
   *
   * @return the number of digits of the fraction part
   * @throws ExpressionException if a character doesn't match the pattern
   */
  private int parseFractionDigits(String text, int dot, int end, StringBuilder digits) {
    int fraction = 0;
    int k = this.separator + 1;
    for (int i = dot + 1; i < end; i++) {
      char c = text.charAt(i);
      char p = (k < pattern.length()) ? pattern.charAt(k++) : '9';
      if (isLiteral(p)) {
        if (c == p) continue;
        throw createUnparsableNumber(format, text, i);
      }
      if (Characters.isDigit(c)) {
        digits.append(c);
        fraction++;
      } else {
        throw createUnparsableNumber(format, text, i);
      }
    }
    return fraction;
  }

  /**
   * Format number with number format.
   *
   * @param number the number to format
   * @return the formatted number
   */
  public String format(BigDecimal number) {

    // Hexadecimal
    int index = pattern.indexOf('X');
    if (index < 0) {
      index = pattern.indexOf('x');
    }
    if (index >= 0) {
      return formatHex(number, index);
    }

    int power = 0;
    if (this.scientific > 0) {
      // Normalize the mantissa to one integer digit before rounding it to the format scale
      if (number.signum() != 0) {
        power = number.precision() - number.scale() - 1;
        number = number.movePointLeft(power);
      }
      number = number.setScale(this.scale, RoundingMode.HALF_UP);
      // The rounding can carry to a second integer digit, 9.99 => 10.0
      if (number.abs().compareTo(BigDecimal.TEN) >= 0) {
        number = number.movePointLeft(1).setScale(this.scale, RoundingMode.HALF_UP);
        power++;
      }
    } else if (this.v > 0) {
      // Multiply by 10^v before rounding, 12.34 with '99V9' => 123
      number = number.scaleByPowerOfTen(v).setScale(this.scale, RoundingMode.HALF_UP);
    } else if (this.scale < number.scale()) {
      // Adjust number scale to format scale
      number = number.setScale(this.scale, RoundingMode.HALF_UP);
    }

    String unscaled = number.unscaledValue().abs().toString();
    int dot = integerDigits(number);
    int length = 0;

    DecimalFormatSymbols symbols = getSymbols();

    StringBuilder output = new StringBuilder();
    if (this.precision > 0) {
      length += formatIntegerPart(output, unscaled, dot, symbols);
    }

    if (this.scale > 0) {
      length += formatFractionPart(output, unscaled, dot, symbols);
    }

    // Add currency symbol
    //
    length += addCurrency(output, symbols);

    // Add sign
    //
    length += addSign(output, number.signum());

    // Add scientific notation
    //
    if (scientific > 0) {
      length += addExponent(output, power);
    }

    return pad(output, length, number.signum());
  }

  /**
   * Formats the number in hexadecimal.
   *
   * @param number the number to format
   * @param index the position of the first X in the pattern
   * @return the formatted number
   */
  private String formatHex(BigDecimal number, int index) {
    boolean zeroPadded = (pattern.charAt(0) == '0');
    BigInteger value = number.setScale(0, RoundingMode.HALF_UP).toBigInteger();
    String hex = value.toString(16);

    // If the format precision was too small to hold the number
    if (precision < hex.length()) {
      return StringUtils.rightPad("", precision + 1, "#");
    }

    if (pattern.charAt(index) == 'X') {
      hex = StringUtils.upperCase(hex);
    }
    if (zeroPadded) {
      hex = StringUtils.leftPad(hex, precision, "0");
    }

    if (fillMode) {
      hex = StringUtils.leftPad(hex, pattern.length() + 1, " ");
    }

    return hex;
  }

  /** Returns the number of digits of the integer part of the rounded number to output. */
  private int integerDigits(BigDecimal number) {
    int dot = number.precision() - number.scale();
    // The integer part of zero is empty with a decimal format, "0" => " .00"
    if (this.fillMode && this.scale > 0 && number.signum() == 0) {
      dot = 0;
    }
    // The mantissa of the scientific notation always has one integer digit, "0" => "0.0E+00"
    if (this.scientific > 0) {
      dot = 1;
    }
    return dot;
  }

  /** Returns the position of the first literal in the integer part, or -1 if none. */
  private int firstLiteral() {
    for (int i = 0; i < this.separator; i++) {
      if (isLiteral(this.pattern.charAt(i))) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Formats the integer part, from right to left with the pattern.
   *
   * @return the length of the integer part in the pattern
   */
  private int formatIntegerPart(
      StringBuilder output, String unscaled, int dot, DecimalFormatSymbols symbols) {
    // Position of the first literal in the integer part, if any
    int firstLiteral = firstLiteral();
    int j = dot - 1;
    for (int i = this.separator - 1; i >= 0; i--) {
      char c = this.pattern.charAt(i);

      if (c == '0' || c == '9') {
        j = insertIntegerDigit(output, unscaled, j, c, i > firstLiteral && firstLiteral >= 0);
      } else if (isLiteral(c)) {
        output.insert(0, c);
      } else if (c == ',' || c == 'G') {
        insertGroupingSeparator(output, c, i, j, symbols);
      } else {
        throw createInvalidFormat(format);
      }
    }

    while (j >= 0) {
      output.insert(0, unscaled.charAt(j--));
    }

    return this.separator;
  }

  /**
   * Inserts the next digit of the integer part, or its padding.
   *
   * @param output the output
   * @param unscaled the digits of the number
   * @param j the position of the next digit, negative if there are no more digits
   * @param c the digit of the pattern, 0 or 9
   * @param afterLiteral true if a literal precedes this digit in the pattern
   * @return the position of the next digit
   */
  private int insertIntegerDigit(
      StringBuilder output, String unscaled, int j, char c, boolean afterLiteral) {
    if (j >= 0) {
      output.insert(0, unscaled.charAt(j));
      return j - 1;
    }
    if (c == '0') {
      if (this.scientific == 0) {
        output.insert(0, '0');
      }
    } else if (this.fillMode && afterLiteral) {
      // Blank the digit in place to keep the literals at their position
      output.insert(0, ' ');
    }
    // If zero, output is empty. The zero of the integer part is ignored "0.12" => " .12"
    return j;
  }

  /** Inserts the grouping separator G or comma at position i of the pattern. */
  private void insertGroupingSeparator(
      StringBuilder output, char c, int i, int j, DecimalFormatSymbols symbols) {
    // only add the grouping separator if we have more numbers
    if (j >= 0 || (i > 0 && pattern.charAt(i - 1) == '0')) {
      output.insert(0, (c == 'G') ? symbols.getGroupingSeparator() : ',');
    }
  }

  /**
   * Formats the decimal separator and the fraction part.
   *
   * @return the length of the fraction part in the pattern
   */
  private int formatFractionPart(
      StringBuilder output, String unscaled, int dot, DecimalFormatSymbols symbols) {

    // Add decimal separator
    int i = this.separator;
    char c = this.pattern.charAt(i++);
    if (c == 'D') {
      output.append(symbols.getDecimalSeparator());
    } else {
      output.append(c);
    }
    int length = 1;

    // Add decimal digits
    for (int j = dot; i < this.pattern.length(); i++, j++) {
      c = this.pattern.charAt(i);

      if (isLiteral(c)) {
        output.append(c);
        length++;
        // A literal does not consume a digit
        j--;
        continue;
      }

      if (c == '0' || c == '9') {
        appendFractionDigit(output, unscaled, j, c);
      }

      length++;
    }
    return length;
  }

  /** Appends the next digit of the fraction part, or its padding. */
  private void appendFractionDigit(StringBuilder output, String unscaled, int j, char c) {
    if (j >= 0 && j < unscaled.length()) {
      output.append(unscaled.charAt(j));
    } else if (c == '0' || j < 0 || this.fillMode) {
      // Trailing zeros are suppressed by the FM modifier
      output.append('0');
    }
  }

  /**
   * Adds the currency symbol.
   *
   * @return the length of the symbol
   */
  private int addCurrency(StringBuilder output, DecimalFormatSymbols symbols) {
      return switch (this.currency) {
          case LOCAL_LEADING -> {
              output.insert(0, symbols.getCurrencySymbol());
              yield symbols.getCurrencySymbol().length();
          }
          case TRAILING_LOCAL -> {
              output.append(symbols.getCurrencySymbol());
              yield symbols.getCurrencySymbol().length();
          }
          case ISO_LEADING -> {
              output.insert(0, symbols.getCurrency().getCurrencyCode());
              yield symbols.getCurrency().getCurrencyCode().length();
          }
          case TRAILING_ISO -> {
              output.append(symbols.getCurrency().getCurrencyCode());
              yield symbols.getCurrency().getCurrencyCode().length();
          }
          case DOLLARS -> {
              output.insert(0, '$');
              yield 1;
          }
          default -> 0;
      };
  }

  /**
   * Adds the exponent of the scientific notation.
   *
   * @return the length of the exponent
   */
  private int addExponent(StringBuilder output, int power) {
    output.append('E');
    output.append(power < 0 ? '-' : '+');
    output.append(Math.abs(power) < 10 ? "0" : "");
    output.append(Math.abs(power));
    return scientific + 1;
  }

  /**
   * Pads the output to the length of the format.
   *
   * @param output the formatted number
   * @param length the length of the format
   * @param signum the sign of the rounded number
   * @return the formatted number
   */
  private String pad(StringBuilder output, int length, int signum) {
    // Blank mode returns blanks when the value is zero
    if (this.blank && signum == 0) {
      return fillMode ? StringUtils.repeat(' ', length) : "";
    }

    // If the format was too small to hold the number
    if (output.length() > length) {
      return StringUtils.rightPad("", length, "#");
    }

    if (fillMode) {
      int position = (sign == SignMode.MI_LEADING) ? 1 : 0;
      while (output.length() < length) {
        output.insert(position, ' ');
      }
    }

    return output.toString();
  }

  private static boolean isLiteral(final char c) {
    return LITERALS.indexOf(c) >= 0;
  }

  private int addSign(StringBuilder output, int signum) {
    switch (this.sign) {

        // Returns negative value with a leading minus sign (-) and positive value with
        // a leading blank.
      case DEFAULT:
        if (signum < 0) {
          output.insert(0, '-');
        } else if (fillMode) {
          output.insert(0, ' ');
        } else return 0;
        break;

        // Returns negative value with a leading minus sign (-) and positive value with
        // a leading plus sign (+).
      case S_LEADING:
        output.insert(0, (signum < 0) ? '-' : '+');
        break;

        // Returns negative value with a trailing minus sign (-) and positive value with
        // a trailing plus sign (+).
      case TRAILING_S:
        output.append((signum < 0) ? '-' : '+');
        break;

        // Returns negative value with a leading minus sign (-) and positive value with
        // a leading blank.
      case MI_LEADING:
        if (signum < 0) {
          output.insert(0, '-');
        } else if (fillMode) {
          output.insert(0, ' ');
        } else return 0;
        break;

        // Returns negative value with a trailing minus sign (-) and positive value with
        // a trailing blank.
      case TRAILING_MI:
        if (signum < 0) {
          output.append('-');
        } else if (fillMode) {
          output.append(' ');
        } else return 0;
        break;

        // Returns negative value in <angle brackets> and positive value with a leading
        // and trailing blank.
      case PR:
        if (signum < 0) {
          output.insert(0, '<');
          output.append('>');
        } else if (fillMode) {
          output.insert(0, ' ');
          output.append(' ');
        } else return 0;
        return 2;
    }

    return 1;
  }

  public enum SignMode {
    DEFAULT,
    /** Trailing minus */
    TRAILING_MI,
    /** Leading minus */
    MI_LEADING,
    /** Trailing sign */
    TRAILING_S,
    /** Leading sign */
    S_LEADING,
    /** Angle brackets */
    PR
  }

  public enum CurrencyMode {
    NONE,
    /** Dollars symbol */
    DOLLARS,
    /** Leading local currency symbol */
    LOCAL_LEADING,
    /** Trailing local currency symbol */
    TRAILING_LOCAL,
    /** Leading ISO currency code */
    ISO_LEADING,
    /** Trailing ISO currency code */
    TRAILING_ISO
  }
}
