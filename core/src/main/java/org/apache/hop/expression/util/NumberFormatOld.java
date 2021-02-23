/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
 * <td>ISO currency symbol.</td>
 * <td>\u00A4</td>
 * </tr>
 * <tr>
 * <td>D</td>
 * <td>Local decimal separator.</td>
 * <td>.</td>
 * </tr>
 * <tr>
 * <td>EEEE</td>
 * <td>Returns a value in scientific notation.</td>
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
 * <td>Negative values get trailing minus sign, positive get trailing
 * space.</td>
 * <td>-</td>
 * </tr>
 * <tr>
 * <td>PR</td>
 * <td>Negative values get enclosing angle brackets, positive get spaces.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>RN</td>
 * <td>Returns values in Roman numerals.</td>
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
public final class NumberFormatOld extends BaseFormat {

  private static final Map<String, NumberFormatOld> cache = new ConcurrentHashMap<>();

  public static enum Sign {
    DEFAULT,
    /** */
    MI,
    /** Trailing minus */
    _S,
    /** */
    S_,
    /** Angle brackets */
    PR
  };

  public static enum CurrencyMode {
    NONE,
    DOLLARS,
    LOCAL,
    ISO
  };

  // Original format
  private final String format;
  // Fill mode suppress padding blanks and zeroes.
  private boolean fillMode = true;

  private boolean b = false;
  private boolean scientific;
  private boolean localSymbols = true; // for D and G

  // number of digits in 'numbers' member
  private int precision = 0;

  // number of digits to the right of the decimal point
  private int scale = 0;

  // int firstNine = -1; // position in 'numbers' member the first 9 digit
 private CurrencyMode currency = CurrencyMode.NONE;
  private Sign sign = Sign.DEFAULT;
  private String pattern = "";
  private int v = 0;

  public static final BigDecimal parse(String value, String format) throws ParseException {
    if (format == null) format = "TM";
    NumberFormatOld parser = cache.get(format);
    if (parser == null) {
      parser = new NumberFormatOld(format);
      cache.put(format, parser);
      System.out.println("Create parse format(" + format + ")=" + parser.toString()+" pattern="+parser.pattern);
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

    System.out.println(
        "NumberFormat.format(" + value + "," + precision + ',' + scale + ")=" + result);

    return result;
  }

  public static String format(BigDecimal value, String format, Locale local) {
    if (format == null) format = "TM";
    NumberFormatOld formatter = cache.get(format);
    if (formatter == null) {
      formatter = new NumberFormatOld(format);
      cache.put(format, formatter);
      System.out.println("Create format(" + format + ")=" + formatter.toString());
    }

    return formatter.format(value, local);
  }

  protected NumberFormatOld(final String format) {

    if (format == null || format.length() == 0) {
      throw createInvalidFormat(format);
    }
   
    // short-circuit logic for formats that don't follow common logic below        
    if ("TM".equalsIgnoreCase(format) || "TM9".equalsIgnoreCase(format) || "TME".equalsIgnoreCase(format)) {
      this.format = format.toUpperCase();
      return;
    }
    
    int index = 0;
    int length = format.length();
    this.format = format;

    if (startsWithIgnoreCase(format,index, "FM")) {
      this.fillMode = false;
      index += 2;
    }
    
    // S element can appear only in the first or last position of a number format.
    if (startsWithIgnoreCase(format, index, "S")) {
      this.sign=Sign.S_;
      index += 1;
    }
    else if (endsWithIgnoreCase(format,"S")) {
      this.sign=Sign._S;
      length -= 1;     
    }

    // MI element can appear only in the last position of a number format.
    if (endsWithIgnoreCase(format,"MI")) {
      this.sign=Sign.MI;
      length -= 2;
    }

    // PR element can appear only in the last position of a number format.
    if (endsWithIgnoreCase(format, "PR" )) {
      this.sign=Sign.PR;
      length -= 2;
    }

    // Dollars currency symbol
    if (format.charAt(index)=='$') {
      this.currency = CurrencyMode.DOLLARS;
      index += 1;
    }
    // Local currency symbol
    else if (format.charAt(index)=='L' || format.charAt(index)=='l' || format.charAt(index)=='U' || format.charAt(index)=='u' ) {
      this.currency = CurrencyMode.LOCAL;
      index += 1;
    }
    // ISO currency symbol
    else if (format.charAt(index)=='C' || format.charAt(index)=='c' ) {
      this.currency = CurrencyMode.ISO;
      index += 1;
    }
    
    // Zero blank
    if (format.charAt(index)=='B' || format.charAt(index)=='b' ) {
      this.b = true;
      index += 1;
    }
        
    StringBuilder builder = new StringBuilder();
    boolean leadZero = false;
    boolean definedGroups = false;
    char c = 0;
    for (; index < length; index++) {
      c = format.charAt(index);
      if (c == 'G' || c == 'g' ) {
        if (definedGroups && !this.localSymbols) {
          throw createInvalidFormat(format);
        }
        definedGroups = true;
        this.localSymbols = true;
        builder.append('G');
      } else if (c == ',') {
        if (definedGroups && this.localSymbols) {
          throw createInvalidFormat(format);
        }
        definedGroups = true;
        this.localSymbols = true;
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
      } else break;
    }

    if (c == 'X') {
      for (; index < length; index++, this.v++) {
        builder.append('X');
        c = format.charAt(index);        
        if (c != 'X') break;        
      }
    } else if (c == 'V') {
      for (index++; index < length; index++, this.v++) {
        c = format.charAt(index);
        if (c != '0' && c != '9') break;
      }
    } else if (c == '.' || c == 'D' || c == 'd') {
      c = Character.toUpperCase(c);
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
        if (c != '0' && c != '9') break;
        builder.append('0');
      }
    }

    this.pattern = builder.toString();    
  }
  
  protected NumberFormatOld(String format, boolean old) {


    if (format == null || format.length() == 0) {
      throw createInvalidFormat(format);
    }
    this.format = format;

    // Text-minimal number
    if ("TM".equals(format)) {
      pattern = "TM";
      return;
    }
    
    // Text-minimal number in positional notation.
    if ("TM9".equals(format)) {
      pattern = "TM9";
      return;
    }
    
    // Text-minimal number in scientific notation
    if ("TME".equals(format)) {
      pattern = "TME";
      return;
    }
    
    int index = 0;
    final int length = format.length();
    StringBuilder builder = new StringBuilder();
    
    
    // Prefix sign
    if (startsWithIgnoreCase(format, index, "S")) {
      this.sign = Sign.S_;
      index += 1;
    }    
    if (startsWithIgnoreCase(format, index, "FM")) {
      this.fillMode = false;
      index += 2;
    }
    
    // TODO: fix lower case 
    if (startsWithIgnoreCase(format, index, "RN")) {
      builder.append("RN");
      index += 2;
    }
    
    if (startsWithIgnoreCase(format, index, "B")) {
      this.b = true;
      index += 1;
    }
    
    
//    String prefix;
//    char c = format.charAt(0);
//    if (c != '0' && c != '9' && c != 'X') {
//      for (index = 1; index < length; index++) {
//        c = format.charAt(index);
//        if (c == '0' || c == '9' || c == 'X') break;
//      }
//      prefix = format.substring(0, index).toUpperCase();
//    } else {
//      prefix = "";
//    }


    boolean leadZero = false;
    boolean definedGroups = false;
    boolean definedCurrency = false;
    char c = format.charAt(index);
    
    
    // Prefix $, local or ISO curency symbol
    if (c=='$' || c=='C' || c=='c' || c=='C' || c=='c') {
      builder.append(Character.toUpperCase(c));
      definedCurrency = true;
      c = format.charAt(++index);
    }
    
    // Integer part
    for (; index < length; index++) {
      c = format.charAt(index);
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
        leadZero = false;
      } else if (c == '9') {
        builder.append((leadZero) ? '0' : c);
        this.precision++;
      } else break;
    }

    if (c == 'X') {
      for (; index < length; index++, this.v++) {
        builder.append('X');
        c = format.charAt(index);        
        if (c != 'X') break;        
      }
    } else if (c == 'V') {
      for (index++; index < length; index++, this.v++) {
        c = format.charAt(index);
        if (c != '0' && c != '9') break;
      }
    } else if (c == '.' || c == 'D') {
      if (definedGroups) {
        if (this.localSymbols && c == '.' || !this.localSymbols && c == 'D') {
          throw createInvalidFormat(format);
        }
      } else {
        this.localSymbols = (c == 'D');
      }
      for (index++; index < length; index++, this.scale++) {
        c = format.charAt(index);
        if (c != '0' && c != '9') break;
        builder.append('0');
      }
    }

 //   suffix = format.substring(index);
  //  int suffix_offset = 0;

    // Scientific notation
    if (startsWithIgnoreCase(format, index, "EEEE")) {
      this.scientific = true;
      index = 4;
    }
    
    // Sign
    if (startsWithIgnoreCase(format, index, "S")) {
      this.sign = Sign._S;
      index += 1;
    } else if (startsWithIgnoreCase(format, index, "MI")) {
      this.sign = Sign.MI;
      index += 2;
    } else if (startsWithIgnoreCase(format, index, "PR")) {
      this.sign = Sign.PR;
      index += 2;
    }
    
    if (index < format.length()) {
      throw createInvalidFormat(format);
    }

//    int prefix_offset = 0;
//    if (this.sign == Sign.DEFAULT && prefix.startsWith("S")) {
//      this.sign = Sign.S_;
//      prefix_offset += 1;
//    }
//    if (startsWithIgnoreCase(prefix, prefix_offset, "FM")) {
//      this.fillMode = false;
//      prefix_offset += 2;
//    }
//    
//    // TODO: fix lower case 
//    if (prefix.startsWith("RN", prefix_offset) || prefix.startsWith("rn", prefix_offset)) {
//      builder.append("RN");
//      prefix_offset += 2;
//    }
//    if (startsWithIgnoreCase(prefix, prefix_offset, "B")) {
//      this.b = true;
//      prefix_offset += 1;
//    }
//    if (this.sign == Sign.DEFAULT && prefix.startsWith("S", prefix_offset)) {
//      this.sign = Sign.S_;
//      prefix_offset += 1;
//    }

//    if (startsWithIgnoreCase(prefix, prefix_offset, "$")) {
//      this.currency = CurrencyMode.DOLLARS;
//      prefix_offset += 1;
//    }
//    // Local currency symbol
//    else if (prefix.startsWith("L", prefix_offset)) {
//      this.currency = CurrencyMode.LOCAL;
//      prefix_offset += 1;
//    }
//    // ISO currency symbol
//    else if (prefix.startsWith("C", prefix_offset)) {
//      this.currency = CurrencyMode.ISO;
//      prefix_offset += 1;
//    }

    if (index < format.length() ||  builder.length() == 0 ) {
      throw createInvalidFormat(format);
    }

    this.pattern = builder.toString();
  }

  public String toString() {
    
    if ( pattern==null ) return format;
    
    StringBuilder s = new StringBuilder();
    if (sign == Sign.S_) {
      s.append('S');
    }
    if (!fillMode) {
      s.append("FM");
    }
    if (b) {
      s.append('B');
    }
    if (currency == CurrencyMode.DOLLARS) {
      s.append('$');
    } else if (currency == CurrencyMode.LOCAL) {
      s.append('L');
    } else if (currency == CurrencyMode.ISO) {
      s.append('C');
    }
    s.append(pattern);
//    if (scale > 0) {
//      s.append('D');
//      for (int i = 0; i < scale; i++) {
//        s.append('9');
//      }
//    } else if (v > 0) {
//      s.append('V');
//      for (int i = 0; i < v; i++) {
//        s.append('9');
//      }
//    }
    if (scientific) {
      s.append("EEEE");
    }
    if (sign == Sign._S) {
      s.append('S');
    } else if (sign == Sign.MI) {
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
    NumberFormatOld other = (NumberFormatOld) obj;
    return format.equals(other.format);
  }

  @Override
  public int hashCode() {
    return format.hashCode();
  }

  public BigDecimal parse(String source) throws ParseException {

    ParsePosition pos = new ParsePosition(0);

    StringBuilder value = new StringBuilder(source);
    int start = 0; // first not white space symbol
    try {
      boolean negate = false;
      int len = value.length(); // length of parsed string

      // Skip space
      for (; start < len; start++) {
        if (!Character.isSpaceChar(value.charAt(start))) break;
      }

      // Parse roman numeral
      if ("RN".equals(pattern)) {
        return BigDecimal.valueOf(parseRoman(source));
      }

      // Detect sign
      if (this.sign == Sign.PR) {
        if (value.charAt(start) == '<' && value.charAt(len - 1) == '>') {
          value.setCharAt(start++, ' ');
          value.setLength(--len);
          negate = true;
        }
      } else if (this.sign == Sign.MI) {
        if (value.charAt(len - 1) == '-') {
          value.setLength(--len);
          negate = true;
        }
      } else if (this.sign == Sign._S) {
        char s = value.charAt(len - 1);
        if (s == '-') {
          value.setLength(--len);
          negate = true;
        } else if (s == '+') {
          value.setLength(--len);
        } else {
          pos.setErrorIndex(start);
          return null;
        }
      } else if (this.sign == Sign.S_) {
        char s = value.charAt(start);
        if (s == '-') {
          // TODO: try value.deleteCharAt(start++);
          value.setCharAt(start++, ' ');
          negate = true;
        } else if (s == '+') {
          value.setCharAt(start++, ' ');
        } else {
          pos.setErrorIndex(start);
          return null;
        }
      } else if (this.sign == Sign.DEFAULT) {
        char s = value.charAt(start);
        if (s == '-') {
          value.setCharAt(start++, ' ');
          negate = true;
        } else if (s == '+') {
          value.setCharAt(start++, ' ');
        }
      }

      // Skip space
      for (; start < len; start++) {
        if (!Character.isSpaceChar(value.charAt(start))) break;
      }

      
      if ( pattern.charAt(0)=='X' ) {
        long v = 0;
        
        String s = value.substring(start);
        BigInteger bigInt = new BigInteger(s, 16);
        
        return new BigDecimal(bigInt);
        
//        for (int i = start, j=0; i < len; i++, j++) {
//       
//          
//         // int d = digits.indexOf(c);  
//          
//          if (this.pattern.charAt(j) == 'X') {
//            char c = value.charAt(i);
//            if ( !Characters.isHexDigit(c) ) {
//              pos.setErrorIndex(start);
//              return null;              
//            }
//            
//            v = 16*v + c;
//          }
        
      }
      
      int e = value.indexOf("E");
      if (e == -1) {
        e = value.indexOf("e");
      }
      int dot = source.indexOf('.');
      int coefflen = len - start;
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
      if (this.scientific) {
        if (this.pattern.length() == 0 || this.pattern.indexOf('G') != -1) {
          pos.setErrorIndex(start);
          return null;
        }
        if (e == -1) {
          pos.setErrorIndex(start);
          return null;
        }
        coeff[precision++] = value.charAt(start);
        scale = -Integer.valueOf(value.substring(e + 1));
        if (dot == -1) {
          if (start + 1 != e) {
            pos.setErrorIndex(start);
            return null;
          }
        } else if (this.scale < e - dot - 1) {
          pos.setErrorIndex(dot);
          return null;
        } else {
          if (start + 1 != dot) {
            pos.setErrorIndex(start);
            return null;
          }
          scale += e - dot - 1;
          for (int i = dot + 1; i < e; i++) {
            coeff[precision++] = value.charAt(i);
          }
        }
      } else {
        if (e >= 0) {
          pos.setErrorIndex(e);
          return null;
        }
        if (dot != -1) {
          coefflen--;
          scale = len - dot - 1;
          if (this.scale < scale) {
            pos.setErrorIndex(dot);
            return null;
          }
        }
        try {
          int end = (dot < 0 ? len : dot);
          int j = this.pattern.length() - 1;
          for (int i = start; i < end; i++, j--) {
            char c = value.charAt(i);
            if (this.pattern.charAt(j) == 'G') {
              if (c != ' ') {
                pos.setErrorIndex(i);
                return null;
              }
            } else {
              if (Character.isDigit(c)) {
                coeff[precision++] = c;
              } else {
                pos.setErrorIndex(i);
                return null;
              }
            }
          }
        } catch (Exception ex) {
          pos.setErrorIndex(start);
          ex.printStackTrace();
          return null;
        }
        if (dot != -1) {
          for (int i = dot + 1; i < len; i++) {
            char c = value.charAt(i);
            if (Character.isDigit(c)) {
              coeff[precision++] = c;
            } else {
              pos.setErrorIndex(i);
              return null;
            }
          }
        }
      }
      String str = new String(coeff, 0, precision);
      BigDecimal ret;
      //			if (scale == 0 && precision < 10) {
      //				ret = new Integer(Integer.valueOf(str));
      //			} else {
      ret = new BigDecimal(new BigInteger(str), scale);
      // }
      // pos.setIndex(len);
      return ret;
    } catch (Exception e) {
      pos.setErrorIndex(start);
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
    if (format == null || format.equals("TM") || format.equals("TM9")) {
      String s = number.toPlainString();
      return s.startsWith("0.") ? s.substring(1) : s;
    }

    // Text-minimal number in scientific notation
    if (format.equals("TME")) {
      int power = number.precision() - number.scale() - 1;
      number = number.movePointLeft(power);
      return number.toPlainString()
          + "E"
          + (power < 0 ? '-' : '+')
          + (Math.abs(power) < 10 ? "0" : "")
          + Math.abs(power);
    }
    
    
    StringBuilder output = new StringBuilder();

    DecimalFormatSymbols symbols = DecimalFormatSymbols.getInstance(locale);

    // Adjust number scale
    if (number.scale() > this.scale) {
      number = number.setScale(this.scale, RoundingMode.HALF_UP);
    }

    // If the format precision was too small to hold the number
    if (number.precision() > this.precision+this.scale) {
         for (int i = 0; i < pattern.length()  + this.v + 1; i++) {
            output.append('#');
         }
         return output.toString();
    }
    
    int power=0;
    if (scientific) {
      power = number.precision() - number.scale() - 1;
      number = number.movePointLeft(power);   
    }

    String unscaled =  (number.abs().compareTo(BigDecimal.ONE) < 0 ? zeroesAfterDecimalSeparator(number) : "")+ number.unscaledValue().abs().toString();
    //String unscaled = number.unscaledValue().abs().toString();
    int i = this.precision - 1;
    int j = unscaled.length() - number.scale() - 1;
    int maxLength = precision;
    for (; i >= 0 && j >= 0; i--) {
      char c = this.pattern.charAt(i);
      //maxLength++;
      if (c == '0') {
        if (j >= 0) {
          char digit = unscaled.charAt(j--);
          output.insert(0, digit);
        }
        else if (!this.scientific) {
          output.insert(0, '0');
        }
      } else if (c == '9') {
        if (j >= 0) {
          char digit = unscaled.charAt(j--);
          output.insert(0, digit);
        }
      } else if (c == ',') {
        // only add the grouping separator if we have more numbers
        if (j >= 0 || (i > 0 && pattern.charAt(i - 1) == '0')) {
          output.insert(0, localSymbols ? c:symbols.getGroupingSeparator());
        }
      } else if (c == 'C') {
        Currency currency = symbols.getCurrency();
        output.insert(0, currency.getCurrencyCode());
        //maxLength += 6;
      } else if (c == 'L') {
        Currency currency = symbols.getCurrency();
        output.insert(0, currency.getSymbol());
        //maxLength += 9;
      } else if (c == '$') {
        Currency currency = symbols.getCurrency();
        String cs = currency.getSymbol();
        output.insert(0, cs);
        //maxLength += cs.length() - 1;
      } else {
        throw createInvalidFormat(format);
      }
    }

    if (this.scale > 0) {

      // Add decimal separator
      i =  this.precision;
      char c = this.pattern.charAt(i++);
      if (c == 'D') {
        output.append(symbols.getDecimalSeparator());
      } else {
        output.append(c);
      }
      maxLength++;
      j = unscaled.length() - number.scale();


      //  Add decimal digits 
      for (i = 0; i < this.scale; i++) {
        if (j < unscaled.length()) {
          char digit = unscaled.charAt(j++);
          output.append(digit);
        } else if (this.fillMode) {
          output.append('0');
        }
        maxLength++;
      }
    }

    addSign(output, number.signum());

    // Add scientific notation
    if (scientific) {
      output.append('E');
      output.append(power < 0 ? '-' : '+');
      output.append(Math.abs(power) < 10 ? "0" : "");
      output.append(Math.abs(power));
    }

    if (fillMode) {
      if (scientific) {
        output.insert(0, ' ');
      } else {
        //while (output.length() < maxLength) {
        while (output.length() < pattern.length()) {
          output.insert(0, ' ');
        }
      }
    }
    
    return output.toString();
  }

  public String format_OLD(BigDecimal number, Locale locale) {

    // System.out.println("NumberFormat.format(" + number + "," + format + ")");

    // short-circuit logic for formats that don't follow common logic below

    // Text-minimal number
    if (format == null || format.equals("TM") || format.equals("TM9")) {
      String s = number.toPlainString();
      return s.startsWith("0.") ? s.substring(1) : s;
    }

    // Text-minimal number in scientific notation
    if (format.equals("TME")) {
      int pow = number.precision() - number.scale() - 1;
      number = number.movePointLeft(pow);
      return number.toPlainString()
          + "E"
          + (pow < 0 ? '-' : '+')
          + (Math.abs(pow) < 10 ? "0" : "")
          + Math.abs(pow);
    }
    
    
    String formatUp = format != null ? StringUtils.upperCase(format) : null;
    if (formatUp.equals("RN")) {
      boolean lowercase = format.startsWith("r");
      String rn = StringUtils.leftPad(formatRomanNumeral(number.intValue()), 15, " ");
      return lowercase ? rn.toLowerCase() : rn;
    }

    if (formatUp.equals("FMRN")) {
      boolean lowercase = format.charAt(2) == 'r';
      String rn = formatRomanNumeral(number.intValue());
      return lowercase ? rn.toLowerCase() : rn;
    }

    if (formatUp.endsWith("X")) {
      return formatHex(number, format);
    }

    int maxLength = 1;
    String fmt = format;

    DecimalFormatSymbols symbols = DecimalFormatSymbols.getInstance(locale);
    char localGrouping = symbols.getGroupingSeparator();

    // The S format element can appear only in the first or last position of a
    // number format model.
    boolean leadingSign = formatUp.startsWith("S");
    if (leadingSign) {
      fmt = fmt.substring(1);
    }
    boolean trailingSign = formatUp.endsWith("S");
    if (trailingSign) {
      fmt = fmt.substring(0, fmt.length() - 1);
    }

    // The MI format element can appear only in the last position of a number format
    // model.
    boolean trailingMinus = formatUp.endsWith("MI");
    if (trailingMinus) {
      fmt = fmt.substring(0, fmt.length() - 2);
    }

    // The PR format element can appear only in the last position of a number format
    // model.
    boolean angleBrackets = formatUp.endsWith("PR");
    if (angleBrackets) {
      fmt = fmt.substring(0, fmt.length() - 2);
      maxLength += 1;
    }

    // Returns a value multiplied by 10n
    int v = formatUp.indexOf('V');
    if (v >= 0) {
      int digits = 0;
      for (int i = v + 1; i < fmt.length(); i++) {
        char c = fmt.charAt(i);
        if (c == '0' || c == '9') {
          digits++;
        }
      }
      number = number.movePointRight(digits);
      fmt = fmt.substring(0, v) + fmt.substring(v + 1);
    }

    Integer power = null;
    if (format.endsWith("EEEE")) {
      power = number.precision() - number.scale() - 1;
      number = number.movePointLeft(power);
      fmt = fmt.substring(0, fmt.length() - 4);
    }

    boolean fillMode = !formatUp.startsWith("FM");
    if (!fillMode) {
      fmt = fmt.substring(2);
    }

    // if we need to round the number to fit into the format specified,
    // go ahead and do that first
    int separator = findDecimalSeparator(fmt);
    int formatScale = calculateScale(fmt, separator);
    int numberScale = number.scale();
    if (formatScale < numberScale) {
      number = number.setScale(formatScale, RoundingMode.HALF_UP);
    }

    // any 9s to the left of the decimal separator but to the right of a
    // 0 behave the same as a 0, e.g. "09999.99" -> "00000.99"
//    for (int i = fmt.indexOf('0'); i >= 0 && i < separator; i++) {
//      if (fmt.charAt(i) == '9') {
//        fmt = fmt.substring(0, i) + "0" + format.substring(i + 1);
//      }
//    }

    StringBuilder output = new StringBuilder();
    String unscaled =
        (number.abs().compareTo(BigDecimal.ONE) < 0 ? zeroesAfterDecimalSeparator(number) : "")
            + number.unscaledValue().abs().toString();

    // start at the decimal point and fill in the numbers to the left,
    // working our way from right to left
    int i = separator - 1;
    int j = unscaled.length() - number.scale() - 1;
    for (; i >= 0; i--) {
      char c = fmt.charAt(i);
      maxLength++;
      if (c == '0') {
        if (j >= 0) {
          char digit = unscaled.charAt(j);
          output.insert(0, digit);
          j--;
        } else if (power == null) {
          output.insert(0, '0');
        }
      } else if (c == '9') {
        if (j >= 0) {
          char digit = unscaled.charAt(j);
          output.insert(0, digit);
          j--;
        }
      } else if (c == ',') {
        // only add the grouping separator if we have more numbers
        if (j >= 0 || (i > 0 && format.charAt(i - 1) == '0')) {
          output.insert(0, c);
        }
      } else if (c == 'G' || c == 'g') {
        // only add the grouping separator if we have more numbers
        if (j >= 0 || (i > 0 && fmt.charAt(i - 1) == '0')) {
          output.insert(0, localGrouping);
        }
      } else if (c == 'C' || c == 'c') {
        Currency currency = symbols.getCurrency();
        output.insert(0, currency.getCurrencyCode());
        maxLength += 6;
      } else if (c == 'L' || c == 'l' || c == 'U' || c == 'u') {
        Currency currency = symbols.getCurrency();
        output.insert(0, currency.getSymbol());
        maxLength += 9;
      } else if (c == '$') {
        Currency currency = symbols.getCurrency();
        String cs = currency.getSymbol();
        output.insert(0, cs);
        maxLength += cs.length() - 1;
      } else {
        throw createInvalidFormat(format);
      }
    }

    // if the format (to the left of the decimal point) was too small
    // to hold the number, return a big "######" string
    if (j >= 0) {
      return StringUtils.rightPad("", fmt.length() + 1, "#");
    }

    if (separator < fmt.length()) {

      // add the decimal point
      maxLength++;
      char pt = fmt.charAt(separator);
      if (pt == 'd' || pt == 'D') {
        output.append(symbols.getDecimalSeparator());
      } else {
        output.append(pt);
      }

      // start at the decimal point and fill in the numbers to the right,
      // working our way from left to right
      i = separator + 1;
      j = unscaled.length() - number.scale();
      for (; i < fmt.length(); i++) {
        char c = fmt.charAt(i);
        maxLength++;
        if (c == '9' || c == '0') {
          if (j < unscaled.length()) {
            char digit = unscaled.charAt(j);
            output.append(digit);
            j++;
          } else {
            if (c == '0' || fillMode) {
              output.append('0');
            }
          }
        } else {
          throw createInvalidFormat(format);
        }
      }
    }

    addSign(output, number.signum());

    if (power != null) {
      output.append('E');
      output.append(power < 0 ? '-' : '+');
      output.append(Math.abs(power) < 10 ? "0" : "");
      output.append(Math.abs(power));
    }

    if (fillMode) {
      if (power != null) {
        output.insert(0, ' ');
      } else {
        while (output.length() < maxLength) {
          output.insert(0, ' ');
        }
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

  private void addSign(StringBuilder output, int signum) {
    switch (this.sign) {

        // Returns negative value with a leading minus sign (-) and positive value with
        // a leading blank.
      case DEFAULT:
        if (signum < 0) {
          output.insert(0, '-');
        } else if (fillMode) {
          output.insert(0, ' ');
        }
        break;

        // Returns negative value with a trailing minus sign (-) and positive value with
        // a trailing blank.
      case MI:
        if (signum < 0) {
          output.append('-');
        } else if (fillMode) {
          output.append(' ');
        }
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
        break;

        // Returns negative value with a trailing minus sign (-) and positive value with
        // a trailing plus sign (+).
      case _S:
        output.append((signum < 0) ? '-' : '+');
        break;

        // Returns negative value with a leading minus sign (-) and positive value with
        // a leading plus sign (+).
      case S_:
        output.insert(0, (signum < 0) ? '-' : '+');
        break;
    }
  }


  private static int findDecimalSeparator(String format) {
    int index = format.indexOf('.');
    if (index == -1) {
      index = format.indexOf('D');
      if (index == -1) {
        index = format.indexOf('d');
        if (index == -1) {
          index = format.length();
        }
      }
    }
    return index;
  }

  private static int calculateScale(String format, int separator) {
    int scale = 0;
    for (int i = separator; i < format.length(); i++) {
      char c = format.charAt(i);
      if (c == '0' || c == '9') {
        scale++;
      }
    }
    return scale;
  }

protected static String formatHex(BigDecimal number, String format) {

  boolean fillMode = !StringUtils.upperCase(format).startsWith("FM");
  boolean uppercase = !format.contains("x");
  boolean zeroPadded = format.startsWith("0");
  int digits = 0;
  for (int i = 0; i < format.length(); i++) {
    char c = format.charAt(i);
    if (c == '0' || c == 'X' || c == 'x') {
      digits++;
    }
  }

  int i = number.setScale(0, RoundingMode.HALF_UP).intValue();
  String hex = Integer.toHexString(i);
  if (digits < hex.length()) {
    hex = StringUtils.rightPad("", digits + 1, "#");
  } else {
    if (uppercase) {
      hex = StringUtils.upperCase(hex);
    }
    if (zeroPadded) {
      hex = StringUtils.leftPad(hex, digits, "0");
    }
    if (fillMode) {
      hex = StringUtils.leftPad(hex, format.length() + 1, " ");
    }
  }

  return hex;
}
  
  protected static final ExpressionException createInvalidFormat(final String error) {
    return new ExpressionException(
        BaseMessages.getString(PKG, "Expression.InvalidNumberFormat", error));
  }
}
