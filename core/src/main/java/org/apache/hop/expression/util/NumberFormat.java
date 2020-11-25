package org.apache.hop.expression.util;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.text.DecimalFormatSymbols;
import java.text.ParseException;
import java.text.ParsePosition;
import java.util.Arrays;
import java.util.Currency;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang.StringUtils;
import org.apache.hop.expression.Expression;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.i18n.BaseMessages;

/**
 * Emulates Oracle's TO_NUMBER(number) function.
 *
 * <p>
 *
 * <table border="1">
 * <th>
 * <td>Input</td>
 * <td>Output</td></th>
 * <tr>
 * <td>9</td>
 * <td>Value with the specified number of digits</td>
 * </tr>
 * <tr>
 * <td>0</td>
 * <td>Value with leading zeros</td>
 * </tr>
 * <tr>
 * <tr>
 * <td>. (periode)</td>
 * <td>Decimal point</td>
 * </tr>
 * <tr>
 * <td>, (comma)</td>
 * <td>Group (thousand) separator</td>
 * </tr>
 * <tr>
 * <tr>
 * <td>G</td>
 * <td>Grouping separator.</td>
 * </tr>
 *
 * </table>
 */
public final class NumberFormat extends BaseFormat {

  protected static final Class<?> PKG = Expression.class; // for i18n purposes

  private static final String PATTERN_EXCEPTION = "Wrong pattern";
  private static final String NOT_IMPLIMENTED_EXCEPTION = "Not implimented";

  private static final Map<String, Format> formatsCache = new ConcurrentHashMap<>();

  private final String source;

  public static enum Sign {
    DEFAULT,
    MI,
    _S,
    S_,
    PR
  };

  public static enum CurrencyMode {
    NON,
    DOLLARS,
    LOCAL,
    ISO
  };

  private static class Format {
    boolean fillMode = false;
    boolean b = false;
    boolean scientific;
    boolean localGroups = true; // for D and G

    // number of digits in 'numbers' member
    int digits = 0;

    // number of digits to the right of the decimal point
    int scale = 0;

    // int firstNine = -1; // position in 'numbers' member the first 9 digit
    CurrencyMode currency = CurrencyMode.NON;
    Sign sign = Sign.DEFAULT;
    String pattern = "";

    int v = 0;

    public String toString() {
      StringBuilder s = new StringBuilder();
      if (sign == Sign.S_) {
        s.append('S');
      }
      if (fillMode) {
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
      if (scale > 0) {
        s.append('D');
        for (int i = 0; i < scale; i++) {
          s.append('9');
        }
      } else if (v > 0) {
        s.append('V');
        for (int i = 0; i < v; i++) {
          s.append('9');
        }
      }
      if (scientific) {
        s.append("EEEE");
      }
      if (sign == Sign._S) {
        s.append('S');
      } else if (sign == Sign.MI) {
        s.append("MI");
      }
      return s.toString();
    }
  };

  public static final BigDecimal parse(String value, String format) throws ParseException {
    NumberFormat parser = new NumberFormat(format);
    return parser.parse(value);
  }

  public static String format(BigDecimal value, String format, Locale local) throws ParseException {
    NumberFormat formatter = new NumberFormat(format);
    return formatter.format(value, local);
  }

  public NumberFormat(String format) {

    //		if (format == null)
    //			format = "999999999999.9999";

    this.source = format;
  }

  public String toString() {
    return (source != null) ? source.toString() : "null";
  }

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
    return (source != null) ? source.equals(other.source) : false;
  }

  private static Format compile(String source) throws ParseException {

    System.out.print("Compile Number format: " + source + " >> ");

    Format format = new Format();
    if (source == null || source.length() == 0) {
      return format;
    }

    if (source.equals("RN") || source.equals("rn")) {
      throw new ParseException(NOT_IMPLIMENTED_EXCEPTION, 0);
    }

    final int length = source.length();

    String prefix;
    String suffix;

    int index = 0;
    char c = source.charAt(0);
    if (c != '0' && c != '9') {
      for (index = 1; index < length; index++) {
        c = source.charAt(index);
        if (c == '0' || c == '9') break;
      }
      prefix = source.substring(0, index).toUpperCase();
    } else {
      prefix = "";
      index = 0;
    }
    StringBuilder pattern = new StringBuilder();
    boolean leadZero = false;
    boolean definedGroups = false;
    for (; index < length; index++) {
      c = source.charAt(index);
      if (c == 'G' || c == 'g') {
        if (definedGroups && !format.localGroups) {
          throw new ParseException(PATTERN_EXCEPTION, 0);
        }
        definedGroups = true;
        format.localGroups = true;
        pattern.append('G');
      } else if (c == ',') {
        if (definedGroups && format.localGroups) {
          throw new ParseException(PATTERN_EXCEPTION, index);
        }
        definedGroups = true;
        format.localGroups = true;
        pattern.append(',');
      } else if (c == '0') {
        pattern.append('0');
        format.digits++;
        leadZero = false;
      } else if (c == '9') {
        pattern.append((leadZero) ? '0' : c);
        format.digits++;
      } else break;
    }

    if (c == 'V') {
      for (index++; index < length; index++, format.v++) {
        c = source.charAt(index);
        if (c != '0' && c != '9') break;
      }
    } else if (c == '.' || c == 'D') {
      if (definedGroups) {
        if (format.localGroups && c == '.' || !format.localGroups && c == 'D') {
          throw new ParseException(PATTERN_EXCEPTION, index);
        }
      } else {
        format.localGroups = (c == 'D');
      }
      for (index++; index < length; index++, format.scale++) {
        c = source.charAt(index);
        if (c != '0' && c != '9') break;
        pattern.append('0');
      }
    }
    format.pattern = pattern.toString();
    if (index == prefix.length()) {
      throw new ParseException(PATTERN_EXCEPTION, index);
    }

    suffix = source.substring(index).toUpperCase();
    int suf_offset = 0;

    // Scientific notation
    if (suffix.startsWith("EEEE")) {
      format.scientific = true;
      suf_offset = 4;
    }

    // Sign
    if (suffix.startsWith("S", suf_offset)) {
      format.sign = Sign._S;
      suf_offset += 1;
    } else if (suffix.startsWith("MI", suf_offset)) {
      format.sign = Sign.MI;
      suf_offset += 2;
    } else if (suffix.startsWith("PR", suf_offset)) {
      format.sign = Sign.PR;
      suf_offset += 2;
    }

    if (suf_offset < suffix.length()) {
      throw new ParseException(PATTERN_EXCEPTION, index);
    }

    int pref_offset = 0;
    if (format.sign == Sign.DEFAULT && prefix.startsWith("S")) {
      format.sign = Sign.S_;
      pref_offset += 1;
    }
    if (prefix.startsWith("FM", pref_offset)) {
      format.fillMode = true;
      pref_offset += 2;
    }
    if (prefix.startsWith("B", pref_offset)) {
      format.b = true;
      pref_offset += 1;
    }
    if (format.sign == Sign.DEFAULT && prefix.startsWith("S", pref_offset)) {
      format.sign = Sign.S_;
      suf_offset += 1;
    }

    if (prefix.startsWith("$", pref_offset)) {
      format.currency = CurrencyMode.DOLLARS;
      pref_offset += 1;
    }
    // Local currency symbol
    else if (prefix.startsWith("L", pref_offset)) {
      format.currency = CurrencyMode.LOCAL;
      pref_offset += 1;
    }
    // ISO currency symbol
    else if (prefix.startsWith("C", pref_offset)) {
      format.currency = CurrencyMode.ISO;
      pref_offset += 1;
    }

    if (pref_offset < prefix.length()) {
      throw new ParseException(PATTERN_EXCEPTION, 0);
    }

    System.out.println(format);

    return format;
  }

  public BigDecimal parse(String source) throws ParseException {
    Format format = formatsCache.get(source);
    if (format == null) {
      format = compile(source);
      formatsCache.put(source, format);
    }

    ParsePosition pos = new ParsePosition(0);

    StringBuilder d = new StringBuilder(source);
    int start = 0; // first not white space symbol
    try {
      boolean negate = false;
      int len = d.length(); // length of parsed string
      for (; start < len; start++) {
        if (d.charAt(start) != ' ') break;
      }
      if (format.sign == Sign.PR) {
        if (d.charAt(start) == '<' && d.charAt(len - 1) == '>') {
          d.setCharAt(start++, ' ');
          d.setLength(--len);
          negate = true;
        }
      } else if (format.sign == Sign.MI) {
        if (d.charAt(len - 1) == '-') {
          d.setLength(--len);
          negate = true;
        }
      } else if (format.sign == Sign._S) {
        char s = d.charAt(len - 1);
        if (s == '-') {
          d.setLength(--len);
          negate = true;
        } else if (s == '+') {
          d.setLength(--len);
        } else {
          pos.setErrorIndex(start);
          return null;
        }
      } else if (format.sign == Sign.S_) {
        char s = d.charAt(start);
        if (s == '-') {
          d.setCharAt(start++, ' ');
          negate = true;
        } else if (s == '+') {
          d.setCharAt(start++, ' ');
        } else {
          pos.setErrorIndex(start);
          return null;
        }
      } else if (format.sign == Sign.DEFAULT) {
        char s = d.charAt(start);
        if (s == '-') {
          d.setCharAt(start++, ' ');
          negate = true;
        } else if (s == '+') {
          d.setCharAt(start++, ' ');
        }
      }
      for (; start < len; start++) {
        if (d.charAt(start) != ' ') break;
      }

      int e = d.indexOf("E");
      if (e == -1) {
        e = d.indexOf("e");
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
      if (format.scientific) {
        if (format.pattern.length() == 0 || format.pattern.indexOf('G') != -1) {
          pos.setErrorIndex(start);
          return null;
        }
        if (e == -1) {
          pos.setErrorIndex(start);
          return null;
        }
        coeff[precision++] = d.charAt(start);
        scale = -Integer.valueOf(d.substring(e + 1));
        if (dot == -1) {
          if (start + 1 != e) {
            pos.setErrorIndex(start);
            return null;
          }
        } else if (format.scale < e - dot - 1) {
          pos.setErrorIndex(dot);
          return null;
        } else {
          if (start + 1 != dot) {
            pos.setErrorIndex(start);
            return null;
          }
          scale += e - dot - 1;
          for (int i = dot + 1; i < e; i++) {
            coeff[precision++] = d.charAt(i);
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
          if (format.scale < scale) {
            pos.setErrorIndex(dot);
            return null;
          }
        }
        try {
          int end = (dot < 0 ? len : dot);
          int j = format.pattern.length() - 1;
          for (int i = start; i < end; i++, j--) {
            char c = d.charAt(i);
            if (format.pattern.charAt(j) == 'G') {
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
            char c = d.charAt(i);
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
      pos.setIndex(len);
      return ret;
    } catch (Exception e) {
      pos.setErrorIndex(start);
      return null;
    }
  }

  /**
   * Emulates Oracle's TO_CHAR(number) function.
   *
   * <p>
   *
   * <table border="1">
   * <th>
   * <td>Input</td>
   * <td>Output</td></th>
   * <tr>
   * <td>,</td>
   * <td>Grouping separator.</td>
   * <td>,</td>
   * </tr>
   * <tr>
   * <td>.</td>
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
   *
   * See also TO_CHAR(number) and number format models in the Oracle documentation.
   *
   * @param number the number to format
   * @param source the format pattern to use (if any) *
   * @return the formatted number
   */
  public String format(BigDecimal number, Locale locale) throws ParseException {

    // System.out.println("to_char(" + number + "," + format + ")");
    Format format = compile(source);

    StringBuilder output = new StringBuilder();

    DecimalFormatSymbols symbols = DecimalFormatSymbols.getInstance(locale);

    // Adjust number scale
    if (format.scale < number.scale()) {
      number = number.setScale(format.scale, RoundingMode.HALF_UP);
    }

    if (number.precision() - number.scale() > format.digits) {
      for (int i = 0; i < format.pattern.length() /* + format.v */ + 1; i++) {
        output.append('#');
      }
    }

    String unscaled = number.unscaledValue().abs().toString();
    int i = format.digits - 1;
    int j = unscaled.length() - number.scale() - 1;

    for (; i >= 0 && j >= 0; i--) {
      char c = format.pattern.charAt(i);
      // maxLength++;
      if (c == '0') {
        if (j >= 0) {
          char digit = unscaled.charAt(j--);
          output.insert(0, digit);
        }
        // else if (power == null) {
        //						output.insert(0, '0');
        // }
      } else if (c == '9') {
        if (j >= 0) {
          char digit = unscaled.charAt(j--);
          output.insert(0, digit);
        }
      } else if (c == ',') {
        // only add the grouping separator if we have more numbers
        if (j >= 0 || (i > 0 && source.charAt(i - 1) == '0')) {
          output.insert(0, c);
        }
      } else if (c == 'G' || c == 'g') {
        // only add the grouping separator if we have more numbers
        // if (j >= 0 || (i > 0 && fmt.numbers.charAt(i - 1) == '0')) {
        //						output.insert(0, localGrouping);
        // }
      } else if (c == 'C' || c == 'c') {
        Currency currency = symbols.getCurrency();
        output.insert(0, currency.getCurrencyCode());
        // maxLength += 6;
      } else if (c == 'L' || c == 'l' || c == 'U' || c == 'u') {
        Currency currency = symbols.getCurrency();
        output.insert(0, currency.getSymbol());
        // maxLength += 9;
      } else if (c == '$') {
        Currency currency = symbols.getCurrency();
        String cs = currency.getSymbol();
        output.insert(0, cs);
        // maxLength += cs.length() - 1;
      } else {
        throw new ExpressionException(
            BaseMessages.getString(PKG, "Expression.InvalidNumberFormat", source));
      }
    }

    // if the format (to the left of the decimal point) was too small
    // to hold the number, return a big "######" string
    if (j >= 0) {

      return StringUtils.rightPad("", format.digits + format.scale + 1, "#");
    }

    if (format.scale > 0) {
      // add the decimal point

      // maxLength++;
      // char pt = format.numbers.charAt(format.digits);
      // if (pt == 'D') {
      output.append(symbols.getDecimalSeparator());
      //			} else {
      // output.append(pt);
      // }

      j += 1;
      for (i = 0; i < format.scale; i++) {
        if (j < unscaled.length()) {
          char digit = unscaled.charAt(j++);
          output.append(digit);
        } else if (format.fillMode) {
          output.append('0');
        }
      }

      // start at the decimal point and fill in the numbers to the right,
      // working our way from left to right
      //			i = format.digits + 1;
      //			j = unscaled.length() - number.scale();
      //			for (; i < format.numbers.length(); i++) {
      //				char c = format.numbers.charAt(i);
      //				//maxLength++;
      //				if (c == '9' || c == '0') {
      //					if (j < unscaled.length()) {
      //						char digit = unscaled.charAt(j);
      //						output.append(digit);
      //						j++;
      //					} else {
      //						if (c == '0' || format.fillMode) {
      //							output.append('0');
      //						}
      //					}
      //				} else {
      //					throw new ExpressionException(
      //							BaseMessages.getString(PKG, "Expression.InvalidNumberFormat", source));
      //				}
      //			}
    }

    addSign(output, number.signum(), format);

    return output.toString();
  }

  public String format_old(BigDecimal number, Locale locale) {

    // System.out.println("to_char(" + number + "," + format + ")");

    try {
      Format compile = compile(source);
    } catch (ParseException e) {
    }

    // short-circuit logic for formats that don't follow common logic below
    String formatUp = source != null ? StringUtils.upperCase(source) : null;
    if (formatUp == null || formatUp.equals("TM") || formatUp.equals("TM9")) {
      String s = number.toPlainString();
      return s.startsWith("0.") ? s.substring(1) : s;
    }

    if (formatUp.equals("TME")) {
      int pow = number.precision() - number.scale() - 1;
      number = number.movePointLeft(pow);
      return number.toPlainString()
          + "E"
          + (pow < 0 ? '-' : '+')
          + (Math.abs(pow) < 10 ? "0" : "")
          + Math.abs(pow);
    }

    if (formatUp.equals("RN")) {
      boolean lowercase = source.startsWith("r");
      String rn = StringUtils.leftPad(formatRomanNumeral(number.intValue()), 15, " ");
      return lowercase ? rn.toLowerCase() : rn;
    }

    if (formatUp.equals("FMRN")) {
      boolean lowercase = source.charAt(2) == 'r';
      String rn = formatRomanNumeral(number.intValue());
      return lowercase ? rn.toLowerCase() : rn;
    }

    if (formatUp.endsWith("X")) {
      return formatHex(number, source);
    }

    int maxLength = 1;
    String fmt = source;
    // StringBuilder b= new StringBuilder(format);
    DecimalFormatSymbols symbols = DecimalFormatSymbols.getInstance(locale);
    char localGrouping = symbols.getGroupingSeparator();

    // The S format element can appear only in the first or last position of a
    // number format model.
    boolean leadingSign = formatUp.startsWith("S");
    if (leadingSign) {
      fmt = fmt.substring(1);
      // b.deleteCharAt(0);
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
    if (source.endsWith("EEEE")) {
      power = number.precision() - number.scale() - 1;
      number = number.movePointLeft(power);
      fmt = fmt.substring(0, fmt.length() - 4);
    }

    boolean fillMode = !formatUp.startsWith("FM");
    if (!fillMode) {
      fmt = fmt.substring(2);
    }

    // blanks flag doesn't seem to actually do anything
    // format = format.replaceAll("[Bb]", "");

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
    for (int i = fmt.indexOf('0'); i >= 0 && i < separator; i++) {
      if (fmt.charAt(i) == '9') {
        fmt = fmt.substring(0, i) + "0" + source.substring(i + 1);
      }
    }

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
        if (j >= 0 || (i > 0 && source.charAt(i - 1) == '0')) {
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
        throw new ExpressionException(
            BaseMessages.getString(PKG, "Expression.InvalidNumberFormat", source));
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
          throw new ExpressionException(
              BaseMessages.getString(PKG, "Expression.InvalidNumberFormat", source));
        }
      }
    }

    addSign(
        output, number.signum(), leadingSign, trailingSign, trailingMinus, angleBrackets, fillMode);

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

  private static void addSign(StringBuilder output, int signum, Format fmt) {

    switch (fmt.sign) {

        // Returns negative value with a leading minus sign (-) and positive value with
        // a leading blank.
      case DEFAULT:
        if (signum < 0) {
          output.insert(0, '-');
        } else if (fmt.fillMode) {
          output.insert(0, ' ');
        }
        break;

        // Returns negative value with a trailing minus sign (-) and positive value with
        // a trailing blank.
      case MI:
        if (signum < 0) {
          output.append('-');
        } else if (fmt.fillMode) {
          output.append(' ');
        }
        break;

        // Returns negative value in <angle brackets> and positive value with a leading
        // and trailing blank.
      case PR:
        if (signum < 0) {
          output.insert(0, '<');
          output.append('>');
        } else if (fmt.fillMode) {
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

  private static void addSign(
      StringBuilder output,
      int signum,
      boolean leadingSign,
      boolean trailingSign,
      boolean trailingMinus,
      boolean angleBrackets,
      boolean fillMode) {
    if (angleBrackets) {
      if (signum < 0) {
        output.insert(0, '<');
        output.append('>');
      } else if (fillMode) {
        output.insert(0, ' ');
        output.append(' ');
      }
    } else {
      String sign;
      if (signum == 0) {
        sign = "";
      } else if (signum < 0) {
        sign = "-";
      } else {
        if (leadingSign || trailingSign) {
          sign = "+";
        } else if (fillMode) {
          sign = " ";
        } else {
          sign = "";
        }
      }
      if (trailingMinus || trailingSign) {
        output.append(sign);
      } else {
        output.insert(0, sign);
      }
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
}
