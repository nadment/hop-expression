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
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.i18n.BaseMessages;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.ParseException;
import java.text.ParsePosition;
import java.util.Arrays;
import java.util.Currency;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Emulates Oracle's number format for TO_NUMBER(number) and TO_CHAR(number) functions.
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
public final class NumberFormat extends BaseFormat {

  private static final Map<String, NumberFormat> cache = new ConcurrentHashMap<>();

  public static enum Sign {
    DEFAULT,
    /** */
    _MI, MI_,
    /** Trailing minus */
    _S,
    /** */
    S_,
    /** Angle brackets */
    PR
  };

  public static enum CurrencyMode {
    NONE, DOLLARS, LOCAL_, _LOCAL, ISO_, _ISO
  };

  // Original format
  private final String format;

  // Fill mode suppress padding blanks and zeroes.
  private boolean fillMode = true;

  private boolean exactMode = false;

  private boolean b = false;

  private boolean localSymbols = true; // for D and G

  // number of digits in 'numbers' member
  private int precision = 0;

  // number of digits to the right of the decimal point
  private int scale = 0;

  // scientific fixed-width exponent
  private int scientific = 0;

  // decimal separator position in pattern
  private int separator = 0;

  // int firstNine = -1; // position in 'numbers' member the first 9 digit
  private CurrencyMode currency = CurrencyMode.NONE;
  private Sign sign = Sign.DEFAULT;
  private String pattern = "";
  private int v = 0;

  public static final BigDecimal parse(String value, String format) throws ParseException {
    if (format == null)
      format = "TM";
    NumberFormat parser = cache.get(format);
    if (parser == null) {
      parser = new NumberFormat(format);
      cache.put(format, parser);
    }

    return parser.parse(value);
  }

  public static final BigDecimal parse(String value, int precision, int scale)
      throws ParseException {

    DecimalFormat format = (DecimalFormat) DecimalFormat.getInstance();
    format.setParseBigDecimal(true);
    // format.setMaximumIntegerDigits(precision);
    format.setMaximumFractionDigits(scale);
    BigDecimal result = (BigDecimal) format.parse(value);
    result.setScale(scale, BigDecimal.ROUND_HALF_UP);

    return result;
  }

  public static String format(BigDecimal value, String format, Locale local) {
    if (format == null)
      format = "TM";
    NumberFormat formatter = cache.get(format);
    if (formatter == null) {
      formatter = new NumberFormat(format);
      cache.put(format, formatter);
    }

    return formatter.format(value, local);
  }

  protected NumberFormat(final String format) {

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
      this.sign = Sign.S_;
      index += 1;
    } else if (endsWithIgnoreCase(format, "S")) {
      this.sign = Sign._S;
      length -= 1;
    } else if (startsWithIgnoreCase(format, index, "MI")) {
      this.sign = Sign.MI_;
      index += 2;
    }
    // MI element can appear only in the last position of a number format.
    else if (endsWithIgnoreCase(format, "MI")) {
      this.sign = Sign._MI;
      length -= 2;
    }
    // PR element can appear only in the last position of a number format.
    else if (endsWithIgnoreCase(format, "PR")) {
      this.sign = Sign.PR;
      length -= 2;
    }

    // Zero blank
    if (startsWithIgnoreCase(format, index, "B")) {
      this.b = true;
      index += 1;
    }

    // Prefix dollars currency
    if (startsWithIgnoreCase(format, index, "$")) {
      this.currency = CurrencyMode.DOLLARS;
      index++;
    }
    // Prefix local currency symbol
    else if (startsWithIgnoreCase(format, index, "L")) {
      this.currency = CurrencyMode.LOCAL_;
      index++;
    }
    // Prefix ISO currency abbreviation
    else if (startsWithIgnoreCase(format, index, "C")) {
      this.currency = CurrencyMode.ISO_;
      index++;
    }

    // Integer part
    boolean leadZero = false;
    boolean definedGroups = false;
    boolean hexa = false;
    StringBuilder builder = new StringBuilder();
    for (; index < length; index++, this.separator++) {
      char c = format.charAt(index);
      if (c == 'G' || c == 'g') {
        if (definedGroups && !this.localSymbols) {
          throw createInvalidFormat(format);
        }
        definedGroups = true;
        this.localSymbols = true;
        builder.append(',');
      } else if (c == ',') {
        if (definedGroups && this.localSymbols) {
          throw createInvalidFormat(format);
        }
        definedGroups = true;
        this.localSymbols = false;
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
      } else if (c == 'X' | c == 'x') {
        hexa = true;
        boolean upper = (c == 'X');
        for (; index < length; index++) {
          builder.append(upper ? 'X' : 'x');
          this.precision++;
          this.separator++;
          c = format.charAt(index);
          if (c != 'X' && c != 'x')
            break;
        }

        // } else if (c == ' ') {
        // builder.append(c);
      } else
        break;
    }

    if (startsWithIgnoreCase(format, index, "RN")) {
      builder.append(format.substring(index, index + 2));
      index += 2;
    }
    else if (startsWithIgnoreCase(format, index, "V")) {
      for (index++; index < length; index++, this.v++) {
        char c = format.charAt(index);
        if (c != '0' && c != '9')
          break;
      }
    } else if (startsWithIgnoreCase(format, index, ".", "D")) {
      char c = Character.toUpperCase(format.charAt(index));
      if (definedGroups) {
        if (this.localSymbols && c == '.' || !this.localSymbols && c == 'D') {
          throw createInvalidFormat(format);
        }
      } else {
        this.localSymbols = (c == 'D');
      }

      builder.append('.');
      for (index++; index < length; index++, this.scale++) {
        c = format.charAt(index);
        if (c != '0' && c != '9')
          break;
        builder.append('0');
      }
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
      this.currency = CurrencyMode._LOCAL;
      index += 1;
    }
    // ISO currency abbreviation
    else if (startsWithIgnoreCase(format, index, "C")) {
      this.currency = CurrencyMode._ISO;
      index += 1;
    }

    // Sign
    if (startsWithIgnoreCase(format, index, "S")) {
      this.sign = Sign._S;
      index += 1;
    } else if (startsWithIgnoreCase(format, index, "MI")) {
      this.sign = Sign._MI;
      index += 2;
    } else if (startsWithIgnoreCase(format, index, "PR")) {
      this.sign = Sign.PR;
      index += 2;
    }

    if (index < format.length() || builder.length() == 0) {
      throw createInvalidFormat(format);
    }

    this.pattern = builder.toString();
  }


  public String toString() {

    StringBuilder s = new StringBuilder();
    if (sign == Sign.S_) {
      s.append('S');
    } else if (sign == Sign.MI_) {
      s.append("MI");
    }

    if (!fillMode) {
      s.append("FM");
    }

    if (b) {
      s.append('B');
    }

    if (currency == CurrencyMode.DOLLARS) {
      s.append('$');
    } else if (currency == CurrencyMode.LOCAL_) {
      s.append("L");
    } else if (currency == CurrencyMode.ISO_) {
      s.append("C");
    }

    if (this.localSymbols)
      s.append(pattern.replace('.', 'D').replace(',', 'G'));
    else
      s.append(pattern);

    if (scientific > 0) {
      s.append("EEEE");
    }

    // Currency
    if (currency == CurrencyMode._LOCAL) {
      s.append("L");
    } else if (currency == CurrencyMode._ISO) {
      s.append("C");
    }

    // Sign
    if (sign == Sign._S) {
      s.append('S');
    } else if (sign == Sign._MI) {
      s.append("MI");
    } else if (sign == Sign.PR) {
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
    NumberFormat other = (NumberFormat) obj;
    return format.equals(other.format);
  }

  @Override
  public int hashCode() {
    return format.hashCode();
  }

  public BigDecimal parse(String source) throws ParseException {

    ParsePosition position = new ParsePosition(0);

    StringBuilder value = new StringBuilder(source);
    int start = 0; // first not white space symbol
    try {
      boolean negate = false;
      int length = value.length(); // length of parsed string

      // Skip start space
      for (; start < length; start++) {
        if (!Character.isSpaceChar(value.charAt(start)))
          break;
      }
      
      // Skip end space
      for (; start < length; length--) {
        if (!Character.isSpaceChar(value.charAt(length-1)))
          break;
      }

      // Parse roman numeral
      if ("RN".equals(pattern)) {
        return BigDecimal.valueOf(parseRoman(source));
      }

      // Detect sign
      if (this.sign == Sign.PR) {
        if (value.charAt(start) == '<' && value.charAt(length - 1) == '>') {
          value.setCharAt(start++, ' ');
          value.setLength(--length);
          negate = true;
        }
      } else if (this.sign == Sign._MI) {
        if (value.charAt(length - 1) == '-') {
          value.setLength(--length);
          negate = true;
          
          // Skip end space
          for (; start < length; length--) {
            if (!Character.isSpaceChar(value.charAt(length-1)))
              break;
          }
        }
      } 
      else if (this.sign == Sign.MI_) {
        if (value.charAt(start) == '-') {
          value.setCharAt(start++, ' ');
          negate = true;

          // Skip start space
          for (; start < length; start++) {
            if (!Character.isSpaceChar(value.charAt(start)))
              break;
          }
        }
      } else if (this.sign == Sign._S) {
        char c = value.charAt(length - 1);
        if (c == '-') {
          value.setLength(--length);
          negate = true;
        } else if (c == '+') {
          value.setLength(--length);
        } else {
          position.setErrorIndex(start);
          return null;
        }
      } else if (this.sign == Sign.S_) {
        char c = value.charAt(start);
        if (c == '-') {
          value.setCharAt(start++, ' ');
          negate = true;
        } else if (c == '+') {
          value.setCharAt(start++, ' ');
        } else {
          position.setErrorIndex(start);
          return null;
        }
      } else if (this.sign == Sign.DEFAULT) {
        char c = value.charAt(start);
        if (c == '-') {
          value.setCharAt(start++, ' ');
          negate = true;
        } else if (c == '+') {
          value.setCharAt(start++, ' ');
        }
      }

      // Skip space
      for (; start < length; start++) {
        if (!Character.isSpaceChar(value.charAt(start)))
          break;
      }


      if (pattern.charAt(0) == 'X') {
        long v = 0;

        String s = value.substring(start);
        BigInteger bigInt = new BigInteger(s, 16);

        return new BigDecimal(bigInt);

        // for (int i = start, j=0; i < len; i++, j++) {
        //
        //
        // // int d = digits.indexOf(c);
        //
        // if (this.pattern.charAt(j) == 'X') {
        // char c = value.charAt(i);
        // if ( !Characters.isHexDigit(c) ) {
        // pos.setErrorIndex(start);
        // return null;
        // }
        //
        // v = 16*v + c;
        // }

      }

      int e = value.indexOf("E");
      if (e == -1) {
        e = value.indexOf("e");
      }
      int dot = source.indexOf('.');
      int coefflen = length - start;
      if (negate) {
        coefflen++;
      }
      char coeff[] = new char[coefflen];
      int scale = 0;
      int precision = 0;
      if (negate) {
        precision++;
        coeff[0] = '-';
      }
      if (this.scientific > 0) {
        if (this.pattern.length() == 0 || this.pattern.indexOf(',') != -1) {
          position.setErrorIndex(start);
          return null;
        }
        if (e == -1) {
          position.setErrorIndex(start);
          return null;
        }
        coeff[precision++] = value.charAt(start);
        scale = -Integer.valueOf(value.substring(e + 1));
        if (dot == -1) {
          if (start + 1 != e) {
            position.setErrorIndex(start);
            return null;
          }
        } else if (this.scale < e - dot - 1) {
          position.setErrorIndex(dot);
          return null;
        } else {
          if (start + 1 != dot) {
            position.setErrorIndex(start);
            return null;
          }
          scale += e - dot - 1;
          for (int i = dot + 1; i < e; i++) {
            coeff[precision++] = value.charAt(i);
          }
        }
      } else {
        if (e >= 0) {
          position.setErrorIndex(e);
          return null;
        }
        if (dot != -1) {
          coefflen--;
          scale = length - dot - 1;
          if (this.scale < scale) {
            position.setErrorIndex(dot);
            return null;
          }
        }
        try {
          int end = (dot < 0 ? length : dot);
          int j = this.pattern.length() - 1;
          for (int i = start; i < end; i++, j--) {
            char c = value.charAt(i);
            if (this.pattern.charAt(j) == ',') {
              if (c != ' ') {
                position.setErrorIndex(i);
                return null;
              }
            } else {
              if (Character.isDigit(c)) {
                coeff[precision++] = c;
              } else {
                position.setErrorIndex(i);
                return null;
              }
            }
          }
        } catch (Exception ex) {
          position.setErrorIndex(start);
          return null;
        }
        if (dot != -1) {
          for (int i = dot + 1; i < length; i++) {
            char c = value.charAt(i);
            if (Character.isDigit(c)) {
              coeff[precision++] = c;
            } else {
              position.setErrorIndex(i);
              return null;
            }
          }
        }
      }
      String str = new String(coeff, 0, precision);
      BigDecimal ret;
      // if (scale == 0 && precision < 10) {
      // ret = new Integer(Integer.valueOf(str));
      // } else {
      ret = new BigDecimal(new BigInteger(str), scale);
      // }
      // pos.setIndex(len);
      return ret;
    } catch (Exception exception) {
      position.setErrorIndex(start);
      return null;
    }
  }

  /**
   * See also TO_CHAR(number) and number format models in the Oracle documentation.
   *
   * @param number the number to format
   * @param locale the locale to use
   * @return the formatted number
   */
  public String format(BigDecimal number, Locale locale) {

    // Short-circuit logic for formats that don't follow common logic below

    // Text-minimal number
    if (pattern == null || pattern.equals("TM") ) {
      String s = number.toPlainString();
      return s.startsWith("0.") ? s.substring(1) : s;
    }

    // Text-minimal number in scientific notation
    if (pattern.equalsIgnoreCase("TME")) {
      int power = number.precision() - number.scale() - 1;
      number = number.movePointLeft(power);
      char e = pattern.charAt(2);
      return number.toPlainString() + e + (power < 0 ? '-' : '+')
          + (Math.abs(power) < 10 ? "0" : "") + Math.abs(power);
    }

    // Roman numerals
    if (pattern.charAt(0) == 'R' || pattern.charAt(0) == 'r') {
      String rn = formatRomanNumeral(number.intValue());
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

    DecimalFormatSymbols symbols = DecimalFormatSymbols.getInstance(locale);

    // Adjust number scale
    if (this.scale>0 && number.scale() > this.scale) {
      number = number.setScale(this.scale, RoundingMode.HALF_UP);
    }

    int power = 0;
    if (scientific > 0) {
      power = number.precision() - number.scale() - 1;
      number = number.movePointLeft(power);
    }

    String unscaled = number.unscaledValue().abs().toString();
    int i = this.separator - 1;
    int j = unscaled.length() - number.scale() - 1;
    int length = 0;

    StringBuilder output = new StringBuilder();
    for (; i >= 0 || j>=0; i--) {
      char c = '0';
      
      if ( i>=0) {
        c = this.pattern.charAt(i);
        length++;
      }
      
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

          // "0.12" => " .12"
          if (j <= 0 && digit == '0' && output.length() == 0 && scale > 0) {
            output.insert(0, ' ');
          } else {
            output.insert(0, digit);
          }
        }
      } else if (c == ',') {
        // only add the grouping separator if we have more numbers
        if (j >= 0 || (i > 0 && pattern.charAt(i - 1) == '0')) {
          output.insert(0, localSymbols ? c : symbols.getGroupingSeparator());
        }
        // } else if (c == ' ') {
        // output.insert(0, c);
      } else {
        throw createInvalidFormat(format);
      }
    }

    if (this.scale > 0) {

      // Add decimal separator
      i = this.separator;
      char c = this.pattern.charAt(i++);
      if (c == 'D') {
        output.append(symbols.getDecimalSeparator());
      } else {
        output.append(c);
      }
      length++;
      j = unscaled.length() - number.scale();


      // Add decimal digits
      for (i = 0; i < this.scale; i++) {
        if (j < unscaled.length()) {
          char digit = unscaled.charAt(j++);
          output.append(digit);
        } else if (this.fillMode) {
          output.append('0');
        }
        length++;
      }
    }  
    
    // Add currency symbol
    Currency currency = symbols.getCurrency();
    switch (this.currency) {
      case LOCAL_:
        output.insert(0, currency.getSymbol());
        length += 6;
        break;
      case _LOCAL:
        String cs = currency.getSymbol();
        output.append(cs);
        length += 6;
        // maxLength += cs.length() - 1;
        break;
      case ISO_:
        output.insert(0, currency.getCurrencyCode());
        length += 6;
        break;
      case _ISO:
        output.append(currency.getCurrencyCode());
        length += 6;
        break;
      case DOLLARS:
        output.insert(0, '$');
        length += 1;
        break;
      default:
        break;
    }

    // Add sign
    length += addSign(output, number.signum());

    // Add scientific notation
    if (scientific > 0) {
      output.append('E');
      output.append(power < 0 ? '-' : '+');
      output.append(Math.abs(power) < 10 ? "0" : "");
      output.append(Math.abs(power));
      length += scientific + 1;
    }

    // If the format was too small to hold the number
    if ( output.length() > length) {
      return StringUtils.rightPad("", length, "#");
    }
    
    if (fillMode) {
      int position = (sign == Sign.MI_) ? 1 : 0;
      while (output.length() < length) {
        output.insert(position, ' ');
      }
    }

    return output.toString();
  }

  private static String zeroesAfterDecimalSeparator(BigDecimal number) {
    final String numberStr = number.toPlainString();
    final int idx = numberStr.indexOf('.');
    if (idx < 0) {
      return "";
    }
    int i = idx + 1;
    boolean allZeroes = true;
    int length = numberStr.length();
    for (; i < length; i++) {
      if (numberStr.charAt(i) != '0') {
        allZeroes = false;
        break;
      }
    }
    final char[] zeroes = new char[allZeroes ? length - idx - 1 : i - 1 - idx];
    Arrays.fill(zeroes, '0');
    return String.valueOf(zeroes);
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
        }        
        else return 0;
        break;

      // Returns negative value with a leading minus sign (-) and positive value with
      // a leading plus sign (+).
      case S_:
        output.insert(0, (signum < 0) ? '-' : '+');
        break;

      // Returns negative value with a trailing minus sign (-) and positive value with
      // a trailing plus sign (+).
      case _S:
        output.append((signum < 0) ? '-' : '+');
        break;

      // Returns negative value with a leading minus sign (-) and positive value with
      // a leading blank.
      case MI_:
        if (signum < 0) {
          output.insert(0, '-');
        } else if (fillMode) {
          output.insert(0, ' ');
        }        
        else return 0;
        break;

      // Returns negative value with a trailing minus sign (-) and positive value with
      // a trailing blank.
      case _MI:
        if (signum < 0) {
          output.append('-');
        } else if (fillMode) {
          output.append(' ');
        }
        else return 0;
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
        }
        else return 0;
        return 2;
    }
    
    return 1;
  }

  protected static final ExpressionException createInvalidFormat(final String error) {
    return new ExpressionException(
        BaseMessages.getString(PKG, "Expression.InvalidNumberFormat", error));
  }
}
