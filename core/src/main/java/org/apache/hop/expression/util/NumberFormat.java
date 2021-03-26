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
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

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
public final class NumberFormat extends BaseFormat implements IFormat<BigDecimal> {

  private static final Map<String, IFormat<BigDecimal>> cache = new ConcurrentHashMap<>();

  public static enum Sign {
    DEFAULT,
    /** Trailing minus */
    _MI,
    /** Leading minus */
    MI_,
    /** Trailing sign */
    _S,
    /** Leading sign */
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
  private Sign sign = Sign.DEFAULT;
  private String pattern = "";
  private int v = 0;

  public static final BigDecimal parse(String value, String format, Locale locale)
      throws ParseException {
    if (format == null)
      format = "TM";
    if (locale == null)
      locale = Locale.ENGLISH;
    
    IFormat<BigDecimal> fmt = cache.get(format);
    if (fmt == null) {
      fmt = create(format);
      cache.put(format, fmt);
    }
    
    return fmt.parse(value, locale);
  }

  private static IFormat<BigDecimal> create(String format) {

    if ( format.indexOf('|')>0 ) {
      List<NumberFormat> formats = new ArrayList<>(); 
      for(String f: format.split("\\|")) {
        IFormat<BigDecimal> fmt = cache.get(f);
        if ( fmt==null ) {
          fmt = new NumberFormat(f);
          cache.put(format, fmt);
        }
        formats.add(new NumberFormat(f));          
      }
      return new CompositeNumberFormat(format, formats.toArray(new NumberFormat[0]));
    } 
    
    return new NumberFormat(format);
  }
  
  public static final BigDecimal parse(String value, int precision, int scale)
      throws ParseException {

    // TODO: not thread safe
    DecimalFormat format = (DecimalFormat) DecimalFormat.getInstance();
    format.setParseBigDecimal(true);
    // format.setMaximumIntegerDigits(precision);
    format.setMaximumFractionDigits(scale);
    BigDecimal result = (BigDecimal) format.parse(value);
    result = result.setScale(scale, BigDecimal.ROUND_HALF_UP);

    return result;
  }

  public static String format(BigDecimal value, String format) {
    return format(value, format, Locale.ENGLISH);
  }

  public static String format(BigDecimal value, String format, Locale local) {
    if (format == null)
      format = "TM";    
    
    IFormat<BigDecimal> fmt = cache.get(format);
    
    // Not in cache
    if (fmt == null) {
      fmt = create(format);
      
      cache.put(format, fmt);
    }

    return fmt.format(value, local);
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

    if (index < format.length()) {
      throw createInvalidFormat(format);
    }

    // Pattern can be empty example: 'FMC'
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

    if (blank) {
      s.append('B');
    }

    if (currency == CurrencyMode.DOLLARS) {
      s.append('$');
    } else if (currency == CurrencyMode.LOCAL_) {
      s.append("L");
    } else if (currency == CurrencyMode.ISO_) {
      s.append("C");
    }

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


  /**
   * Parses text from a string to produce a <code>Number</code>.
   * 
   * @param text the string to be parsed
   * @return the parsed value
   * @throws ParseException
   */
  public BigDecimal parse(String text, Locale locale) throws ParseException {

    DecimalFormatSymbols symbols = DecimalFormatSymbols.getInstance(locale);

    int start = 0; // first not white space symbol
    try {
      int end = text.length(); // length of parsed string
      boolean isNegative = false;

      // Skip start space
      while (start < end && Character.isSpaceChar(text.charAt(start)))
        start++;

      // Skip end space
      while (start < end && Character.isSpaceChar(text.charAt(end - 1)))
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
      if (this.sign == Sign.PR) {
        if (text.charAt(start) == '<' && text.charAt(end - 1) == '>') {
          start++;
          end--;
          isNegative = true;
        }
      } else if (this.sign == Sign._MI) {
        if (text.charAt(end - 1) == '-') {
          end--;
          isNegative = true;

          // Skip end space
          while (start < end && Character.isSpaceChar(text.charAt(end - 1)))
            end--;
        }
      } else if (this.sign == Sign.MI_) {
        char c = text.charAt(start);
        if (c == '-' || c == '+') {
          start++;
          if (c == '-')
            isNegative = true;
          // Skip start space
          while (start < end && Character.isSpaceChar(text.charAt(start)))
            start++;
        }
      } else if (this.sign == Sign._S) {
        char c = text.charAt(end - 1);
        if (c == '-') {
          end--;
          isNegative = true;
        } else if (c == '+') {
          end--;
        } else {
          throw createUnparsableNumber(text, start);
        }
      } else if (this.sign == Sign.S_) {
        char c = text.charAt(start);
        if (c == '-') {
          start++;
          isNegative = true;
        } else if (c == '+') {
          start++;
        } else {
          throw createUnparsableNumber(text, start);
        }
      } else if (this.sign == Sign.DEFAULT) {
        char c = text.charAt(start);
        if (c == '-') {
          start++;
          isNegative = true;
        } else if (c == '+') {
          start++;
        }
      }

      switch (this.currency) {
        case LOCAL_: {
          String symbol = symbols.getCurrencySymbol();
          if (text.regionMatches(start, symbol, 0, symbol.length()))
            start += symbol.length();
          break;
        }
        case _LOCAL: {
          String symbol = symbols.getCurrencySymbol();
          if (text.regionMatches(end - symbol.length(), symbol, 0, symbol.length()))
            end -= symbol.length();
          break;
        }
        case ISO_: {
          String code = symbols.getCurrency().getCurrencyCode();
          if (text.regionMatches(start, code, 0, code.length()))
            start += code.length();
          break;
        }
        case _ISO: {
          String code = symbols.getCurrency().getCurrencyCode();
          if (text.regionMatches(end - code.length(), code, 0, code.length()))
            end -= code.length();
          break;
        }
        case DOLLARS: {
          char c = text.charAt(start);
          if (c == '$')
            start++;
          break;
        }
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
          else if (this.exactMode == false && c == ',')
            continue;
        }

        throw createUnparsableNumber(text, i);
      }

      // fraction part
      int fraction = 0;
      if (dot != -1) {
        for (int i = dot + 1; i < end; i++) {
          char c = text.charAt(i);
          if (Character.isDigit(c)) {
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
  public String format(BigDecimal number, Locale locale) {

    // Short-circuit logic for formats that don't follow common logic below

    // Text-minimal number
    if (pattern == null || pattern.equals("TM")) {
      String s = number.toPlainString();
      return s.startsWith("0.") ? s.substring(1) : s;
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

    DecimalFormatSymbols symbols = DecimalFormatSymbols.getInstance(locale);

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
    dot = dot - number.scale();
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
          }
          else {
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

    switch (this.currency) {
      case LOCAL_:
        output.insert(0, symbols.getCurrencySymbol());
        length += symbols.getCurrencySymbol().length();
        break;
      case _LOCAL:
        String cs = symbols.getCurrencySymbol();
        output.append(cs);
        length += symbols.getCurrencySymbol().length();
        break;
      case ISO_: {
        String code = symbols.getCurrency().getCurrencyCode();
        output.insert(0, code);
        length += code.length();
        break;
      }
      case _ISO: {
        String code = symbols.getCurrency().getCurrencyCode();
        output.append(code);
        length += code.length();
        break;
      }
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
    if (output.length() > length) {
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
        } else
          return 0;
        break;

      // Returns negative value with a trailing minus sign (-) and positive value with
      // a trailing blank.
      case _MI:
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

  protected final ExpressionException createInvalidFormat(final String error) {
    return new ExpressionException(
        BaseMessages.getString(PKG, "Expression.InvalidNumberFormat", error));
  }
  
  protected final ParseException createUnparsableNumber(final String text, int index) {
    return new ParseException(BaseMessages.getString(PKG, "Expression.UnparsableNumber", text, format), index);
  }
  
  
}
