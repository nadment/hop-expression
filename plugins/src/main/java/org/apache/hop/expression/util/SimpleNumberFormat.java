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

import org.apache.commons.lang.StringUtils;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.exception.ParseNumberException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

/**
 * Expression number format model for <code>TO_NUMBER(string, format)</code> and
 * <code>TO_CHAR(number, format)</code> functions.
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
 * <td>Blanks integer part of a fixed point number less than 1.</td>
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
 * <td>Returns values in Roman numerals (case sensitive).</td>
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
final class SimpleNumberFormat extends NumberFormat {

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

  // Original format
  private final String format;

  // Fill mode suppress padding blanks and zeroes.
  private boolean fillMode = true;

  private boolean exactMode = false;

  private boolean blank = true;

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
  private String pattern = "";
  private int v = 0;

  protected SimpleNumberFormat(final String format) {

    if (format == null || format.length() == 0) {
      throw createInvalidFormat(format);
    }

    this.format = format;

    // short-circuit logic for formats that don't follow common logic below
    if (format.equalsIgnoreCase("TM") || format.equalsIgnoreCase("TM9")) {
      this.pattern = "TM";
      return;
    }

    // Preserve case for exponent case 'E' or 'e'
    if (format.equalsIgnoreCase("TME")) {
      this.pattern = format;
      return;
    }

    int index = 0;
    int length = format.length();

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
      length -= 1;
    } else if (startsWithIgnoreCase(format, index, "MI")) {
      this.sign = SignMode.MI_LEADING;
      index += 2;
    }
    // MI element can appear only in the last position of a number format.
    else if (endsWithIgnoreCase(format, "MI")) {
      this.sign = SignMode.TRAILING_MI;
      length -= 2;
    }
    // PR element can appear only in the last position of a number format.
    else if (endsWithIgnoreCase(format, "PR")) {
      this.sign = SignMode.PR;
      length -= 2;
    }

    // Zero blank
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

    // Integer part
    boolean leadZero = false;
    boolean definedGroups = false;
    StringBuilder builder = new StringBuilder();
    for (; index < length; index++, this.separator++) {
      char c = format.charAt(index);
      if (c == 'G' || c == 'g') {
        if (definedGroups && !localSymbols) {
          throw createInvalidFormat(format);
        }
        definedGroups = true;
        localSymbols = true;
        builder.append('G');
      } else if (c == ',') {
        if (definedGroups && localSymbols) {
          throw createInvalidFormat(format);
        }
        definedGroups = true;
        localSymbols = false;
        builder.append(',');
      } else if (c == '0') {
        builder.append('0');
        this.precision++;
        leadZero = true;
      } else if (c == '9') {
        // any 9s to the left of the decimal separator but to the right of a
        // 0 behave the same as a 0, e.g. "09999.99" -> "00000.99"
        builder.append((leadZero) ? '0' : c);
        this.precision++;
      } else if (c == 'X' || c == 'x') {
        boolean upper = (c == 'X');
        for (; index < length; index++) {
          builder.append(upper ? 'X' : 'x');
          this.precision++;
          this.separator++;
          c = format.charAt(index);
          if (c != 'X' && c != 'x')
            break;
        }
      } else
        break;
    }

    if (startsWithIgnoreCase(format, index, "RN")) {
      builder.append(format.substring(index, index + 2));
      index += 2;
    } else if (startsWithIgnoreCase(format, index, "V")) {
      for (index++; index < length; index++, this.v++) {
        char c = format.charAt(index);
        if (c != '0' && c != '9')
          break;
      }
    } else if (startsWithIgnoreCase(format, index, ".", "D")) {

      char c = Character.toUpperCase(format.charAt(index++));
      if (definedGroups) {
        if (localSymbols && c == '.' || !localSymbols && c == 'D') {
          throw createInvalidFormat(format);
        }
      } else {
        localSymbols = (c == 'D');
      }
      builder.append(c);


      int zero = index;
      for (int i = index; i < length; i++) {
        c = format.charAt(i);
        if (c == '0')
          zero = i;
        else if (c != '9')
          break;
      }

      for (; index < length; index++, this.scale++) {
        c = format.charAt(index);
        if (c == '9') {
          if (index < zero)
            c = '0';
        } else if (c != '0')
          break;
        builder.append(c);
      }


      // for (index++; index < length; index++, this.scale++) {
      // c = format.charAt(index);
      // if (c != '0' && c != '9')
      // break;
      // builder.append('0');
      // }
    }

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

    if (index < format.length()) {
      throw createInvalidFormat(format);
    }

    // Pattern can be empty example: 'FMC'
    this.pattern = builder.toString();
  }


  public String toString() {

    StringBuilder s = new StringBuilder();
    if (sign == SignMode.S_LEADING) {
      s.append('S');
    } else if (sign == SignMode.MI_LEADING) {
      s.append("MI");
    }

    if (!fillMode) {
      s.append("FM");
    }

    if (blank) {
      s.append('B');
    }

    if (currency == CurrencyMode.DOLLARS) {
      s.append('$');
    } else if (currency == CurrencyMode.LOCAL_LEADING) {
      s.append("L");
    } else if (currency == CurrencyMode.ISO_LEADING) {
      s.append("C");
    }

    s.append(pattern);

    if (scientific > 0) {
      s.append("EEEE");
    }

    // Currency
    if (currency == CurrencyMode.TRAILING_LOCAL) {
      s.append("L");
    } else if (currency == CurrencyMode.TRAILING_ISO) {
      s.append("C");
    }

    // Sign
    if (sign == SignMode.TRAILING_S) {
      s.append('S');
    } else if (sign == SignMode.TRAILING_MI) {
      s.append("MI");
    } else if (sign == SignMode.PR) {
      s.append("PR");
    }
    return s.toString();
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (this == obj) {
      return true;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    SimpleNumberFormat other = (SimpleNumberFormat) obj;
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
   * @throws ParseNumberException
   */
  public BigDecimal parse(String text) throws ParseNumberException {

    DecimalFormatSymbols symbols = DecimalFormatSymbols.getInstance(Locale.getDefault());

    int start = 0; // first not white space symbol
    try {
      int end = text.length(); // length of parsed string
      boolean isNegative = false;

      // Skip start space
      while (start < end && Characters.isSpace(text.charAt(start)))
        start++;

      // Skip end space
      while (start < end && Characters.isSpace(text.charAt(end - 1)))
        end--;

      // Text-minimal number
      if ("TM".equals(pattern)) {
        String s = text.substring(start, end);
        return new BigDecimal(s);
      }

      // Text-minimal number in scientific notation
      if ("TME".equalsIgnoreCase(pattern)) {
        String s = text.substring(start, end);
        return new BigDecimal(s);
      }

      // Parse roman numeral
      if ("RN".equalsIgnoreCase(pattern)) {
        return BigDecimal.valueOf(RomanNumeral.parse(text, start, end));
      }

      // Detect sign
      if (this.sign == SignMode.PR) {
        if (text.charAt(start) == '<' && text.charAt(end - 1) == '>') {
          start++;
          end--;
          isNegative = true;
        }
      } else if (this.sign == SignMode.TRAILING_MI) {
        if (text.charAt(end - 1) == '-') {
          end--;
          isNegative = true;

          // Skip end space
          while (start < end && Characters.isSpace(text.charAt(end - 1)))
            end--;
        }
      } else if (this.sign == SignMode.MI_LEADING) {
        char c = text.charAt(start);
        if (c == '-' || c == '+') {
          start++;
          if (c == '-')
            isNegative = true;
          // Skip start space
          while (start < end && Characters.isSpace(text.charAt(start)))
            start++;
        }
      } else if (this.sign == SignMode.TRAILING_S) {
        char c = text.charAt(end - 1);
        if (c == '-') {
          end--;
          isNegative = true;
        } else if (c == '+') {
          end--;
        } else {
          throw createUnparsableNumber(text, start);
        }
      } else if (this.sign == SignMode.S_LEADING) {
        char c = text.charAt(start);
        if (c == '-') {
          start++;
          isNegative = true;
        } else if (c == '+') {
          start++;
        } else {
          throw createUnparsableNumber(text, start);
        }
      } else if (this.sign == SignMode.DEFAULT) {
        char c = text.charAt(start);
        if (c == '-') {
          start++;
          isNegative = true;
        } else if (c == '+') {
          start++;
        }
      }

      String symbol;
      switch (this.currency) {
        case LOCAL_LEADING:
          symbol = symbols.getCurrencySymbol();
          if (text.regionMatches(start, symbol, 0, symbol.length()))
            start += symbol.length();
          break;
        case TRAILING_LOCAL:
          symbol = symbols.getCurrencySymbol();
          if (text.regionMatches(end - symbol.length(), symbol, 0, symbol.length()))
            end -= symbol.length();
          break;
        case ISO_LEADING:
          symbol = symbols.getCurrency().getCurrencyCode();
          if (text.regionMatches(start, symbol, 0, symbol.length()))
            start += symbol.length();
          break;
        case TRAILING_ISO:
          symbol = symbols.getCurrency().getCurrencyCode();
          if (text.regionMatches(end - symbol.length(), symbol, 0, symbol.length()))
            end -= symbol.length();
          break;
        case DOLLARS:
          char c = text.charAt(start);
          if (c == '$')
            start++;
          break;
        default:
          break;
      }

      // Hex
      if (pattern.charAt(0) == 'X') {
        String str = text.substring(start, end);
        BigInteger bigInt = new BigInteger(str, 16);

        return new BigDecimal(bigInt);
      }

      StringBuilder digits = new StringBuilder();

      int e = text.indexOf("E");
      if (e == -1) {
        e = text.indexOf("e");
      }

      char decimalSeparator = (this.localSymbols) ? symbols.getDecimalSeparator() : '.';

      int dot = text.indexOf(decimalSeparator);

      // integer part
      int pos = (dot < 0 ? end : dot);
      int j = this.pattern.length() - this.scale;
      if (this.scale > 0)
        j--;
      for (int i = pos - 1; i >= start; i--) {
        char c = text.charAt(i);
        if (j > 0)
          j--;
        char p = pattern.charAt(j);
        if (p == '0' || p == '9') {
          if (Characters.isDigit(c)) {
            digits.insert(0, c);
            continue;
          }
        } else if (p == ',') {
          if (c == ',')
            continue;
        } else if (p == 'G') {
          if (c == symbols.getGroupingSeparator())
            continue;
          else if (!this.exactMode && c == ',')
            continue;
        }

        throw createUnparsableNumber(text, i);
      }

      // fraction part
      int fraction = 0;
      if (dot != -1) {
        for (int i = dot + 1; i < end; i++) {
          char c = text.charAt(i);
          if (Characters.isDigit(c)) {
            digits.append(c);
            fraction++;
          } else {
            throw createUnparsableNumber(text, i);
          }
        }
      }

      if (isNegative) {
        digits.insert(0, '-');
      }
      String str = digits.toString();

      return new BigDecimal(new BigInteger(str), fraction);
    } catch (Exception exception) {
      throw createUnparsableNumber(text, start);
    }
  }

  /**
   * Format number with number format.
   *
   * @param number the number to format
   * @param locale the locale to use
   * @return the formatted number
   */
  public String format(BigDecimal number) {

    // Short-circuit logic for formats that don't follow common logic below

    // Text-minimal number
    if (pattern == null || pattern.equals("TM")) {
      String s = number.stripTrailingZeros().toPlainString();

      // TODO: To be compatible with SQL
      // if (s.startsWith("0.")) {
      // we want ".1" not "0.1"
      // return s.substring(1);
      // } else if (s.startsWith("-0.")) {
      // we want "-.1" not "-0.1"
      // return "-" + s.substring(2);
      // }

      return s;
    }

    // Text-minimal number in scientific notation
    if (pattern.equalsIgnoreCase("TME")) {
      int power = number.precision() - number.scale() - 1;
      number = number.movePointLeft(power);
      // Case of exponent
      char e = pattern.charAt(2);
      return number.toPlainString() + e + (power < 0 ? '-' : '+')
          + (Math.abs(power) < 10 ? "0" : "") + Math.abs(power);
    }

    // Roman numerals
    if (pattern.equalsIgnoreCase("RN")) {
      String rn = RomanNumeral.format(number.intValue());
      if (this.fillMode) {
        rn = StringUtils.leftPad(rn, 15, " ");
      }
      boolean lowercase = pattern.charAt(0) == 'r';
      return lowercase ? rn.toLowerCase() : rn;
    }

    // Hexadecimal
    int index = pattern.indexOf('X');
    if (index < 0) {
      index = pattern.indexOf('x');
    }
    if (index >= 0) {
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

    DecimalFormatSymbols symbols = DecimalFormatSymbols.getInstance(Locale.getDefault());

    // Adjust number scale to format scale
    if (this.scale < number.scale()) {
      number = number.setScale(this.scale, RoundingMode.DOWN);
    }

    int power = 0;
    if (this.scientific > 0) {
      power = number.precision() - number.scale() - 1;
      number = number.movePointLeft(power);
    } else if (this.v > 0) {
      number = number.scaleByPowerOfTen(v);
    }


    String unscaled = number.unscaledValue().abs().toString();

    int dot = unscaled.length();
    // dot = dot - number.scale();

    dot = number.precision() - number.scale();
    int length = 0;

    StringBuilder output = new StringBuilder();
    if (this.precision > 0) {
      int j = dot - 1;
      for (int i = this.separator - 1; i >= 0; i--) {
        char c = this.pattern.charAt(i);
        length++;

        if (c == '0') {
          if (j >= 0) {
            char digit = unscaled.charAt(j--);
            output.insert(0, digit);
          } else if (this.scientific == 0) {
            output.insert(0, '0');
          }
        } else if (c == '9') {
          if (j >= 0) {
            char digit = unscaled.charAt(j--);
            output.insert(0, digit);
          } else {
            // If zero, output is empty
            if (output.length() == 0 && scale > 0) {
              // If blank mode ignore zero "0.12" => " .12"
              if (!this.blank) {
                output.insert(0, '0');
              }
            }
          }
        } else if (c == ',') {
          // only add the grouping separator if we have more numbers
          if (j >= 0 || (i > 0 && pattern.charAt(i - 1) == '0')) {
            output.insert(0, ',');
          }
        } else if (c == 'G') {
          // only add the grouping separator if we have more numbers
          if (j >= 0 || (i > 0 && pattern.charAt(i - 1) == '0')) {
            output.insert(0, symbols.getGroupingSeparator());
          }
        } else {
          throw createInvalidFormat(format);
        }
      }

      while (j >= 0) {
        output.insert(0, unscaled.charAt(j--));
      }
    }

    if (this.scale > 0) {

      // Add decimal separator
      int i = this.separator;
      char c = this.pattern.charAt(i++);
      if (c == 'D') {
        output.append(symbols.getDecimalSeparator());
      } else {
        output.append(c);
      }
      length++;

      // Add decimal digits
      for (int j = dot; i < this.pattern.length(); i++, j++) {
        c = this.pattern.charAt(i);

        if (c == '0') {
          if (j >= 0 && j < unscaled.length()) {
            output.append(unscaled.charAt(j));
          } else {
            output.append('0');
          }
        } else if (c == '9') {
          if (j >= 0 && j < unscaled.length()) {
            output.append(unscaled.charAt(j));
          } else if (j < 0) {
            output.append('0');
          } else if (this.fillMode) {
            output.append(' ');
          }
        }

        length++;
      }
    }

    // Add currency symbol
    //
    switch (this.currency) {
      case LOCAL_LEADING:
        output.insert(0, symbols.getCurrencySymbol());
        length += symbols.getCurrencySymbol().length();
        break;
      case TRAILING_LOCAL:
        output.append(symbols.getCurrencySymbol());
        length += symbols.getCurrencySymbol().length();
        break;
      case ISO_LEADING:
        output.insert(0, symbols.getCurrency().getCurrencyCode());
        length += symbols.getCurrency().getCurrencyCode().length();
        break;
      case TRAILING_ISO:
        output.append(symbols.getCurrency().getCurrencyCode());
        length += symbols.getCurrency().getCurrencyCode().length();
        break;
      case DOLLARS:
        output.insert(0, '$');
        length += 1;
        break;
      default:
        break;
    }

    // Add sign
    //
    length += addSign(output, number.signum());

    // Add scientific notation
    //
    if (scientific > 0) {
      output.append('E');
      output.append(power < 0 ? '-' : '+');
      output.append(Math.abs(power) < 10 ? "0" : "");
      output.append(Math.abs(power));
      length += scientific + 1;
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



  private int addSign(StringBuilder output, int signum) {
    switch (this.sign) {

      // Returns negative value with a leading minus sign (-) and positive value with
      // a leading blank.
      case DEFAULT:
        if (signum < 0) {
          output.insert(0, '-');
        } else if (fillMode) {
          output.insert(0, ' ');
        } else
          return 0;
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
        } else
          return 0;
        break;

      // Returns negative value with a trailing minus sign (-) and positive value with
      // a trailing blank.
      case TRAILING_MI:
        if (signum < 0) {
          output.append('-');
        } else if (fillMode) {
          output.append(' ');
        } else
          return 0;
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
        } else
          return 0;
        return 2;
    }

    return 1;
  }

  protected final NumberFormatException createInvalidFormat(final String pattern) {
    return new NumberFormatException(ErrorCode.INVALID_NUMBER_FORMAT.message(pattern));
  }

  protected final ParseNumberException createUnparsableNumber(final String text, int index) {
    return new ParseNumberException(ErrorCode.UNPARSABLE_NUMBER_WITH_FORMAT, text, format,
        index);
  }
}
