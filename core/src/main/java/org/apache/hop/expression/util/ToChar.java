package org.apache.hop.expression.util;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.SimpleDateFormat;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;
import java.time.format.TextStyle;
import java.time.temporal.IsoFields;
import java.time.temporal.JulianFields;
import java.time.temporal.WeekFields;
import java.util.Arrays;
import java.util.Currency;
import java.util.IllegalFormatFlagsException;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;
import org.apache.hop.expression.Expression;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.i18n.BaseMessages;

/**
 * Emulates Oracle's TO_CHAR function.
 */
public class ToChar {
	protected static final Class<?> PKG = Expression.class; // for i18n purposes

	private static final int[] ROMAN_VALUES = { 1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1 };

	private static final String[] ROMAN_NUMERALS = { "M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV",
			"I" };

	private ToChar() {
		// Utility class
	}

	/**
	 * Emulates Oracle's TO_CHAR(number) function.
	 *
	 * <p>
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
	 * See also TO_CHAR(number) and number format models in the Oracle
	 * documentation.
	 *
	 * @param number the number to format
	 * @param format the format pattern to use (if any) *
	 * @return the formatted number
	 */
	@SuppressWarnings("unused")
	public static String toChar(BigDecimal number, String format, Locale locale) {

		// System.out.println("to_char(" + number + "," + format + ")");

		// short-circuit logic for formats that don't follow common logic below
		String formatUp = format != null ? StringUtils.upperCase(format) : null;
		if (formatUp == null || formatUp.equals("TM") || formatUp.equals("TM9")) {
			String s = number.toPlainString();
			return s.startsWith("0.") ? s.substring(1) : s;
		} else if (formatUp.equals("TME")) {
			int pow = number.precision() - number.scale() - 1;
			number = number.movePointLeft(pow);
			return number.toPlainString() + "E" + (pow < 0 ? '-' : '+') + (Math.abs(pow) < 10 ? "0" : "")
					+ Math.abs(pow);
		} else if (formatUp.equals("RN")) {
			boolean lowercase = format.startsWith("r");
			String rn = StringUtils.leftPad(toRomanNumeral(number.intValue()), 15, " ");
			return lowercase ? rn.toLowerCase() : rn;
		} else if (formatUp.equals("FMRN")) {
			boolean lowercase = format.charAt(2) == 'r';
			String rn = toRomanNumeral(number.intValue());
			return lowercase ? rn.toLowerCase() : rn;
		} else if (formatUp.endsWith("X")) {
			return toHex(number, format);
		}

		int maxLength = 1;
		String originalFormat = format;
		DecimalFormatSymbols symbols = DecimalFormatSymbols.getInstance(locale);
		char localGrouping = symbols.getGroupingSeparator();

		// The S format element can appear only in the first or last position of a
		// number format model.
		boolean leadingSign = formatUp.startsWith("S");
		if (leadingSign) {
			format = format.substring(1);
		}
		boolean trailingSign = formatUp.endsWith("S");
		if (trailingSign) {
			format = format.substring(0, format.length() - 1);
		}

		// The MI format element can appear only in the last position of a number format
		// model.
		boolean trailingMinus = formatUp.endsWith("MI");
		if (trailingMinus) {
			format = format.substring(0, format.length() - 2);
		}

		// The PR format element can appear only in the last position of a number format
		// model.
		boolean angleBrackets = formatUp.endsWith("PR");
		if (angleBrackets) {
			format = format.substring(0, format.length() - 2);
			maxLength += 1;
		}

		// Returns a value multiplied by 10n
		int v = formatUp.indexOf('V');
		if (v >= 0) {
			int digits = 0;
			for (int i = v + 1; i < format.length(); i++) {
				char c = format.charAt(i);
				if (c == '0' || c == '9') {
					digits++;
				}
			}
			number = number.movePointRight(digits);
			format = format.substring(0, v) + format.substring(v + 1);
		}

		Integer power;
		if (format.endsWith("EEEE")) {
			power = number.precision() - number.scale() - 1;
			number = number.movePointLeft(power);
			format = format.substring(0, format.length() - 4);
		} else {
			power = null;
		}

		boolean fillMode = !formatUp.startsWith("FM");
		if (!fillMode) {
			format = format.substring(2);
		}

		// blanks flag doesn't seem to actually do anything
		// format = format.replaceAll("[Bb]", "");

		// if we need to round the number to fit into the format specified,
		// go ahead and do that first
		int separator = findDecimalSeparator(format);
		int formatScale = calculateScale(format, separator);
		int numberScale = number.scale();
		if (formatScale < numberScale) {
			number = number.setScale(formatScale, RoundingMode.HALF_UP);
		}

		// any 9s to the left of the decimal separator but to the right of a
		// 0 behave the same as a 0, e.g. "09999.99" -> "00000.99"
		for (int i = format.indexOf('0'); i >= 0 && i < separator; i++) {
			if (format.charAt(i) == '9') {
				format = format.substring(0, i) + "0" + format.substring(i + 1);
			}
		}

		StringBuilder output = new StringBuilder();
		String unscaled = (number.abs().compareTo(BigDecimal.ONE) < 0 ? zeroesAfterDecimalSeparator(number) : "")
				+ number.unscaledValue().abs().toString();

		// start at the decimal point and fill in the numbers to the left,
		// working our way from right to left
		int i = separator - 1;
		int j = unscaled.length() - number.scale() - 1;
		for (; i >= 0; i--) {
			char c = format.charAt(i);
			maxLength++;
			if (c == '0') {
				if (j >= 0) {
					char digit = unscaled.charAt(j);
					output.insert(0, digit);
					j--;
				} else if ( power == null) {
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
				if (j >= 0 || (i > 0 && format.charAt(i - 1) == '0')) {
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
						BaseMessages.getString(PKG, "Expression.InvalidNumericFormat", originalFormat));
			}
		}

		// if the format (to the left of the decimal point) was too small
		// to hold the number, return a big "######" string
		if (j >= 0) {
			return StringUtils.rightPad("", format.length() + 1, "#");
		}

		if (separator < format.length()) {

			// add the decimal point
			maxLength++;
			char pt = format.charAt(separator);
			if (pt == 'd' || pt == 'D') {
				output.append(symbols.getDecimalSeparator());
			} else {
				output.append(pt);
			}

			// start at the decimal point and fill in the numbers to the right,
			// working our way from left to right
			i = separator + 1;
			j = unscaled.length() - number.scale();
			for (; i < format.length(); i++) {
				char c = format.charAt(i);
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
							BaseMessages.getString(PKG, "Expression.InvalidNumericFormat", originalFormat));
				}
			}
		}

		addSign(output, number.signum(), leadingSign, trailingSign, trailingMinus, angleBrackets, fillMode);

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

	/**
	 * Append a zero-padded number to a string builder.
	 *
	 * @param buff          the string builder
	 * @param length        the number of characters to append
	 * @param positiveValue the number to append
	 */
	private static void appendZeroPadded(StringBuilder buff, int length, long positiveValue) {
		if (length == 2) {
			if (positiveValue < 10) {
				buff.append('0');
			}
			buff.append(positiveValue);
		} else {
			String s = Long.toString(positiveValue);
			length -= s.length();
			while (length > 0) {
				buff.append('0');
				length--;
			}
			buff.append(s);
		}
	}

	private static void addSign(StringBuilder output, int signum, boolean leadingSign, boolean trailingSign,
			boolean trailingMinus, boolean angleBrackets, boolean fillMode) {
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

	/**
	 * Roman numeral month (I-XII; JAN = I).
	 * 
	 * @param number
	 * @return
	 */
	private static String toRomanNumeral(int number) {
		StringBuilder result = new StringBuilder();
		for (int i = 0; i < ROMAN_VALUES.length; i++) {
			int value = ROMAN_VALUES[i];
			String numeral = ROMAN_NUMERALS[i];
			while (number >= value) {
				result.append(numeral);
				number -= value;
			}
		}
		return result.toString();
	}

	private static String toHex(BigDecimal number, String format) {

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

	/**
	 * Returns time zone display name or ID for the specified date-time value.
	 *
	 * @param value value
	 * @param tzd   if {@code true} return TZD (time zone region with Daylight
	 *              Saving Time information included), if {@code false} return TZR
	 *              (time zone region)
	 * @return time zone display name or ID
	 */
//    private static String getTimeZone(Value value, boolean tzd) {
//        if (!(value instanceof ValueTimestampTimeZone)) {
//            TimeZone tz = TimeZone.getDefault();
//            if (tzd) {
//                boolean daylight = tz.inDaylightTime(value.getTimestamp());
//                return tz.getDisplayName(daylight, TimeZone.SHORT);
//            }
//            return tz.getID();
//        }
//        return DateTimeUtils.timeZoneNameFromOffsetMins(((ValueTimestampTimeZone) value).getTimeZoneOffsetMins());
//    }

	/**
	 * Returns a capitalization strategy if the specified string contains any of the
	 * specified substrings at the specified index. The capitalization strategy
	 * indicates the casing of the substring that was found. If none of the
	 * specified substrings are found, this method returns <code>null</code> .
	 *
	 * @param s          the string to check
	 * @param index      the index to check at
	 * @param substrings the substrings to check for within the string
	 * @return a capitalization strategy if the specified string contains any of the
	 *         specified substrings at the specified index, <code>null</code>
	 *         otherwise
	 */
	private static Capitalization containsAt(String s, int index, String... substrings) {
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
					return Capitalization.toCapitalization(up1, up2);
				}
			}
		}
		return null;
	}

	/** Represents a capitalization / casing strategy. */
	public enum Capitalization {

		/**
		 * All letters are uppercased.
		 */
		UPPERCASE,

		/**
		 * All letters are lowercased.
		 */
		LOWERCASE,

		/**
		 * The string is capitalized (first letter uppercased, subsequent letters
		 * lowercased).
		 */
		CAPITALIZE;

		/**
		 * Returns the capitalization / casing strategy which should be used when the
		 * first and second letters have the specified casing.
		 *
		 * @param up1 whether or not the first letter is uppercased
		 * @param up2 whether or not the second letter is uppercased
		 * @return the capitalization / casing strategy which should be used when the
		 *         first and second letters have the specified casing
		 */
		static Capitalization toCapitalization(Boolean up1, Boolean up2) {
			if (up1 == null) {
				return Capitalization.CAPITALIZE;
			} else if (up2 == null) {
				return up1 ? Capitalization.UPPERCASE : Capitalization.LOWERCASE;
			} else if (up1) {
				return up2 ? Capitalization.UPPERCASE : Capitalization.CAPITALIZE;
			} else {
				return Capitalization.LOWERCASE;
			}
		}

		/**
		 * Applies this capitalization strategy to the specified string.
		 *
		 * @param s the string to apply this strategy to
		 * @return the resultant string
		 */
		public String apply(String s) {
			if (s == null || s.isEmpty()) {
				return s;
			}
			switch (this) {
			case UPPERCASE:
				return StringUtils.upperCase(s);
			case LOWERCASE:
				return StringUtils.lowerCase(s);
			case CAPITALIZE:
				return Character.toUpperCase(s.charAt(0))
						+ (s.length() > 1 ? StringUtils.lowerCase(s).substring(1) : "");
			default:
				throw new IllegalArgumentException("Unknown capitalization strategy: " + this);
			}
		}
	}

	/**
	 * Emulates Oracle's TO_CHAR(datetime) function.
	 *
	 * <p>
	 * <table border="1">
	 * <th>
	 * <td>Input</td>
	 * <td>Output</td>
	 * <td>Closest {@link SimpleDateFormat} Equivalent</td></th>
	 * <tr>
	 * <td>- / , . ; : "text"</td>
	 * <td>Reproduced verbatim.</td>
	 * <td>'text'</td>
	 * </tr>
	 * <tr>
	 * <td>A.D. AD B.C. BC</td>
	 * <td>Era designator, with or without periods.</td>
	 * <td>G</td>
	 * </tr>
	 * <tr>
	 * <td>A.M. AM P.M. PM</td>
	 * <td>AM/PM marker.</td>
	 * <td>a</td>
	 * </tr>
	 * <tr>
	 * <td>CC SCC</td>
	 * <td>Century.</td>
	 * <td>None.</td>
	 * </tr>
	 * <tr>
	 * <td>D</td>
	 * <td>Day of week.</td>
	 * <td>u</td>
	 * </tr>
	 * <tr>
	 * <td>DAY</td>
	 * <td>Name of day.</td>
	 * <td>EEEE</td>
	 * </tr>
	 * <tr>
	 * <td>DY</td>
	 * <td>Abbreviated day name.</td>
	 * <td>EEE</td>
	 * </tr>
	 * <tr>
	 * <td>DD</td>
	 * <td>Day of month.</td>
	 * <td>d</td>
	 * </tr>
	 * <tr>
	 * <td>DDD</td>
	 * <td>Day of year.</td>
	 * <td>D</td>
	 * </tr>
	 * <tr>
	 * <td>DL</td>
	 * <td>Long date format.</td>
	 * <td>EEEE, MMMM d, yyyy</td>
	 * </tr>
	 * <tr>
	 * <td>DS</td>
	 * <td>Short date format.</td>
	 * <td>MM/dd/yyyy</td>
	 * </tr>
	 * <tr>
	 * <td>E</td>
	 * <td>Abbreviated era name (Japanese, Chinese, Thai)</td>
	 * <td>None.</td>
	 * </tr>
	 * <tr>
	 * <td>EE</td>
	 * <td>Full era name (Japanese, Chinese, Thai)</td>
	 * <td>None.</td>
	 * </tr>
	 * <tr>
	 * <td>FF[1-9]</td>
	 * <td>Fractional seconds.</td>
	 * <td>S</td>
	 * </tr>
	 * <tr>
	 * <td>FM</td>
	 * <td>Returns values with no leading or trailing spaces.</td>
	 * <td>None.</td>
	 * </tr>
	 * <tr>
	 * <td>FX</td>
	 * <td>Requires exact matches between character data and format model.</td>
	 * <td>None.</td>
	 * </tr>
	 * <tr>
	 * <td>HH HH12</td>
	 * <td>Hour in AM/PM (1-12).</td>
	 * <td>hh</td>
	 * </tr>
	 * <tr>
	 * <td>HH24</td>
	 * <td>Hour in day (0-23).</td>
	 * <td>HH</td>
	 * </tr>
	 * <tr>
	 * <td>IW</td>
	 * <td>Week in year.</td>
	 * <td>w</td>
	 * </tr>
	 * <tr>
	 * <td>WW</td>
	 * <td>Week in year.</td>
	 * <td>w</td>
	 * </tr>
	 * <tr>
	 * <td>W</td>
	 * <td>Week in month.</td>
	 * <td>W</td>
	 * </tr>
	 * <tr>
	 * <td>IYYY IYY IY I</td>
	 * <td>Last 4/3/2/1 digit(s) of ISO year.</td>
	 * <td>yyyy yyy yy y</td>
	 * </tr>
	 * <tr>
	 * <td>RRRR RR</td>
	 * <td>Last 4/2 digits of year.</td>
	 * <td>yyyy yy</td>
	 * </tr>
	 * <tr>
	 * <td>Y,YYY</td>
	 * <td>Year with comma.</td>
	 * <td>None.</td>
	 * </tr>
	 * <tr>
	 * <td>YEAR SYEAR</td>
	 * <td>Year spelled out (S prefixes BC years with minus sign).</td>
	 * <td>None.</td>
	 * </tr>
	 * <tr>
	 * <td>YYYY SYYYY</td>
	 * <td>4-digit year (S prefixes BC years with minus sign).</td>
	 * <td>yyyy</td>
	 * </tr>
	 * <tr>
	 * <td>YYY YY Y</td>
	 * <td>Last 3/2/1 digit(s) of year.</td>
	 * <td>yyy yy y</td>
	 * </tr>
	 * <tr>
	 * <td>J</td>
	 * <td>Julian day (number of days since January 1, 4712 BC).</td>
	 * <td>None.</td>
	 * </tr>
	 * <tr>
	 * <td>MI</td>
	 * <td>Minute in hour.</td>
	 * <td>mm</td>
	 * </tr>
	 * <tr>
	 * <td>MM</td>
	 * <td>Month in year.</td>
	 * <td>MM</td>
	 * </tr>
	 * <tr>
	 * <td>MON</td>
	 * <td>Abbreviated name of month.</td>
	 * <td>MMM</td>
	 * </tr>
	 * <tr>
	 * <td>MONTH</td>
	 * <td>Name of month, padded with spaces.</td>
	 * <td>MMMM</td>
	 * </tr>
	 * <tr>
	 * <td>RM</td>
	 * <td>Roman numeral month.</td>
	 * <td>None.</td>
	 * </tr>
	 * <tr>
	 * <td>Q</td>
	 * <td>Quarter of year.</td>
	 * <td>None.</td>
	 * </tr>
	 * <tr>
	 * <td>SS</td>
	 * <td>Seconds in minute.</td>
	 * <td>ss</td>
	 * </tr>
	 * <tr>
	 * <td>SSSSS</td>
	 * <td>Seconds in day.</td>
	 * <td>None.</td>
	 * </tr>
	 * <tr>
	 * <td>TS</td>
	 * <td>Short time format.</td>
	 * <td>h:mm:ss aa</td>
	 * </tr>
	 * <tr>
	 * <td>TZD</td>
	 * <td>Daylight savings time zone abbreviation.</td>
	 * <td>z</td>
	 * </tr>
	 * <tr>
	 * <td>TZR</td>
	 * <td>Time zone region information.</td>
	 * <td>zzzz</td>
	 * </tr>
	 * <tr>
	 * <td>X</td>
	 * <td>Local radix character.</td>
	 * <td>None.</td>
	 * </tr>
	 * </table>
	 * <p>
	 * See also TO_CHAR(datetime) and datetime format models in the Oracle
	 * documentation.
	 *
	 * @param value  the date-time value to format
	 * @param format the format pattern to use (if any)
	 * @return the formatted timestamp
	 */
	public static String toChar(ZonedDateTime value, String format, Locale local) {

		if (format == null) {
			format = "DD-MON-YY HH.MI.SS.FF PM";
		}

		// System.out.println("to_char(" + value + "," + format + ")");

		StringBuilder output = new StringBuilder();
		boolean fillMode = true;

		for (int index = 0, length = format.length(); index < length;) {

			Capitalization cap;

			// AD indicator with periods
			if ((cap = containsAt(format, index, "A.D.", "B.C.")) != null) {
				String era = (value.getYear() > 0) ? "A.D." : "B.C.";
				output.append(cap.apply(era));
				index += 4;
			}
			// AD indicator without periods
			else if ((cap = containsAt(format, index, "AD", "BC")) != null) {
				String era = (value.getYear() > 0) ? "AD" : "BC";
				output.append(cap.apply(era));
				index += 2;
			} else if ((cap = containsAt(format, index, "A.M.", "P.M.")) != null) {
				boolean isAM = value.getHour() < 12;
				String am = isAM ? "A.M." : "P.M.";
				output.append(cap.apply(am));
				index += 4;
			} else if ((cap = containsAt(format, index, "AM", "PM")) != null) {
				String am = (value.getHour() < 12) ? "AM" : "PM";
				output.append(cap.apply(am));
				index += 2;
			}

			// Long date format 'Tuesday, April 12, 1952 AD'
			else if (containsAt(format, index, "DL") != null) {
				DateTimeFormatter formatter = DateTimeFormatter.ofLocalizedDateTime(FormatStyle.FULL); // .withLocale(Locale.ENGLISH);
				output.append(value.format(formatter));
				index += 2;
			}
			// Short date format 'MM/DD/RRRR'.
			else if (containsAt(format, index, "DS") != null) {
				appendZeroPadded(output, 2, value.getMonthValue());
				output.append('/');
				appendZeroPadded(output, 2, value.getDayOfMonth());
				output.append('/');
				appendZeroPadded(output, 4, Math.abs(value.getYear()));
				index += 2;
			}
			// Short time format
			else if ((cap = containsAt(format, index, "TS")) != null) {
				int h12 = (value.getHour() + 11) % 12 + 1;
				output.append(h12).append(':');
				appendZeroPadded(output, 2, value.getMinute());
				output.append(':');
				appendZeroPadded(output, 2, value.getSecond());
				output.append(' ');
				String am = (value.getHour() < 12) ? "AM" : "PM";
				output.append(cap.apply(am));
				index += 2;
			}
			// Day of year (1-366)
			else if (containsAt(format, index, "DDD") != null) {
				if (fillMode) {
					appendZeroPadded(output, 3, value.getDayOfYear());
				} else {
					output.append(value.getDayOfYear());
				}
				index += 3;
			}
			// Day of month (1-31)
			else if (containsAt(format, index, "DD") != null) {

				if (fillMode) {
					appendZeroPadded(output, 2, value.getDayOfMonth());
				} else {
					output.append(value.getDayOfMonth());
				}
				index += 2;
			}
			// Abbreviated name of day
			else if ((cap = containsAt(format, index, "DY")) != null) {
				String day = value.getDayOfWeek().getDisplayName(TextStyle.SHORT, Locale.ENGLISH);
				output.append(cap.apply(day));
				index += 2;
			}
			// Name of day
			else if ((cap = containsAt(format, index, "DAY")) != null) {
				String day = cap.apply(value.getDayOfWeek().getDisplayName(TextStyle.FULL, Locale.ENGLISH));
				if (fillMode) {
					day = StringUtils.rightPad(day, "Wednesday".length(), " ");
				}
				output.append(day);
				index += 3;
			}
			// Day of week (1=Sunday-7)
			else if (containsAt(format, index, "D") != null) {
				output.append((value.getDayOfWeek().getValue() + 1) % 7);
				index += 1;
			}
			// Julian day; the number of days since January 1, 4712 BC
			else if (containsAt(format, index, "J") != null) {
				long julianDay = value.getLong(JulianFields.JULIAN_DAY);
				output.append(julianDay);
				index += 1;
			}
			// Hour of day in 24 hour format (0-23)
			else if (containsAt(format, index, "HH24") != null) {
				appendZeroPadded(output, 2, value.getHour());
				index += 4;
			}
			// Hour of day in 12 hour format (1-12)
			else if (containsAt(format, index, "HH12") != null) {
				int h12 = (value.getHour() + 11) % 12 + 1;
				appendZeroPadded(output, 2, h12);
				index += 4;
			}
			// Hour of day in 12 hour format (1-12)
			else if (containsAt(format, index, "HH") != null) {
				int h12 = (value.getHour() + 11) % 12 + 1;
				appendZeroPadded(output, 2, h12);
				index += 2;
			}
			// Minute (0-59)
			else if (containsAt(format, index, "MI") != null) {
				appendZeroPadded(output, 2, value.getMinute());
				index += 2;
			}
			// Seconds past midnight (0-86399)
			else if (containsAt(format, index, "SSSSS") != null) {
				int seconds = (int) (value.getNano() / 1_000_000_000);
				output.append(seconds);
				index += 5;
			}
			// Second (0-59)
			else if (containsAt(format, index, "SS") != null) {
				appendZeroPadded(output, 2, value.getSecond());
				index += 2;
			}
			// Fractional seconds
			else if (containsAt(format, index, "FF1", "FF2", "FF3", "FF4", "FF5", "FF6", "FF7", "FF8", "FF9") != null) {
				int x = format.charAt(index + 2) - '0';

				int nanos = value.getNano();

				int ff = (int) (nanos * Math.pow(10, x - 9));
				appendZeroPadded(output, x, ff);
				index += 3;
			} else if (containsAt(format, index, "FF") != null) {
				appendZeroPadded(output, 9, value.getNano());
				index += 2;
			}
			// TODO: Time zone region
			else if (containsAt(format, index, "TZR") != null) {

				// output.append(getTimeZone(value, false));
				index += 3;
			}
			// TODO: Time zone region with Daylight Saving Time information included
			else if (containsAt(format, index, "TZD") != null) {
				// output.append(getTimeZone(value, true));
				index += 3;
			}
			// Week of year (1-52 or 1-53) based on the ISO standard
			else if (containsAt(format, index, "IW") != null) {
				int week = value.get(WeekFields.ISO.weekOfYear());
				output.append(week);
				index += 2;
			}
			// Week of year (1-53) where week 1 starts on the first day of the year and
			// continues to the seventh day of the year.
			else if (containsAt(format, index, "WW") != null) {
				int weekOfYear = value.get(WeekFields.SUNDAY_START.weekOfYear());
				output.append(weekOfYear);
				index += 2;
			}
			// Century
			else if (containsAt(format, index, "CC") != null) {
				int year = Math.abs(value.getYear());
				int century = year / 100;
				if (((int) year % 100) != 0) {
					century += 1;
				}
				appendZeroPadded(output, 2, century);
				index += 2;
			}
			// Century
			else if (containsAt(format, index, "SCC") != null) {
				int year = value.getYear();
				int century = year / 100;
				if (((int) year % 100) != 0) {
					century += 1;
				}

				if (fillMode) {
					output.append(year < 0 ? '-' : ' ');
					appendZeroPadded(output, 2, Math.abs(century));
				} else {
					output.append(century);
				}

				index += 3;
			}

			// Week of month (1-5) where week 1 starts on the first day of the month and
			// ends on the seventh
			else if (containsAt(format, index, "W") != null) {
				int weekOfMonth = value.get(WeekFields.ISO.weekOfMonth());
				output.append(weekOfMonth);
				index += 1;
			}
			// Year with comma in this position.
			else if (containsAt(format, index, "Y,YYY") != null) {
				int year = Math.abs(value.getYear());
				output.append(new DecimalFormat("#,###").format(year));
				index += 5;
			}
			// 4-digit year; S prefixes BC dates with a minus sign.
			else if (containsAt(format, index, "SYYYY") != null) {
				int year = value.getYear();
				if (fillMode) {
					output.append(year < 0 ? '-' : ' ');
					appendZeroPadded(output, 4, Math.abs(year));
				} else {
					output.append(year);
				}
				index += 5;
			} else if ((cap = containsAt(format, index, "YEAR")) != null) {
				int year = Math.abs(value.getYear());
				output.append(cap.apply(toWord(year)));
				index += 4;
			} else if ((cap = containsAt(format, index, "SYEAR")) != null) {
				int year = value.getYear();
				output.append(year < 0 ? '-' : ' ');
				output.append(cap.apply(toWord(year)));
				index += 5;
			}
			// 4-digit year
			else if (containsAt(format, index, "YYYY") != null || containsAt(format, index, "RRRR") != null) {
				int year = Math.abs(value.getYear());
				if (fillMode) {
					appendZeroPadded(output, 4, year);
				} else {
					output.append(year);
				}
				index += 4;
			}
			// Last 3 digits of year.
			else if (containsAt(format, index, "YYY") != null) {
				int year = Math.abs(value.getYear());
				appendZeroPadded(output, 3, year % 1000);
				index += 3;
			}
			// Last 2 digits of year.
			else if (containsAt(format, index, "YY", "RR") != null) {
				int year = Math.abs(value.getYear());
				appendZeroPadded(output, 2, year % 100);
				index += 2;
			}
			// Last 1 digit of year.
			else if (containsAt(format, index, "Y") != null) {
				int year = Math.abs(value.getYear());
				output.append(year % 10);
				index += 1;
			}
			// 4-digit year based on the ISO standard.
			else if (containsAt(format, index, "IYYY") != null) {
				int weekYear = Math.abs(value.get(IsoFields.WEEK_BASED_YEAR));
				appendZeroPadded(output, 4, weekYear);
				index += 4;
			}
			// Last 3 digits of ISO year.
			else if (containsAt(format, index, "IYY") != null) {
				int weekYear = Math.abs(value.get(IsoFields.WEEK_BASED_YEAR));
				appendZeroPadded(output, 3, weekYear % 1000);
				index += 3;
			}
			// Last 2 digits of ISO year.
			else if (containsAt(format, index, "IY") != null) {
				int weekYear = Math.abs(value.get(IsoFields.WEEK_BASED_YEAR));
				appendZeroPadded(output, 2, weekYear % 100);
				index += 2;
			}

			// Last 1 digit of ISO year.
			else if (containsAt(format, index, "I") != null) {
				int weekYear = Math.abs(value.get(IsoFields.WEEK_BASED_YEAR));
				output.append(weekYear % 10);
				index += 1;
			}
			// Name of month, padded with blanks
			else if ((cap = containsAt(format, index, "MONTH")) != null) {
				String month = cap.apply(value.getMonth().getDisplayName(TextStyle.FULL, Locale.ENGLISH));
				if (fillMode) {
					month = StringUtils.rightPad(month, "September".length(), " ");
				}
				output.append(month);
				index += 5;
				// Abbreviated name of month
			} else if ((cap = containsAt(format, index, "MON")) != null) {
				String month = value.getMonth().getDisplayName(TextStyle.SHORT, Locale.ENGLISH);
				output.append(cap.apply(month));
				index += 3;
			}
			// Month (01-12; January = 01)
			else if (containsAt(format, index, "MM") != null) {
				if (fillMode) {
					appendZeroPadded(output, 2, value.getMonthValue());
				} else {
					output.append(value.getMonthValue());
				}
				index += 2;
			}
			// Roman numeral month (I-XII; January = I)
			else if ((cap = containsAt(format, index, "RM")) != null) {
				output.append(cap.apply(toRomanNumeral(value.getMonthValue())));
				index += 2;
			}
			// Quarter of year (1, 2, 3, 4; January - March = 1)
			else if (containsAt(format, index, "Q") != null) {
//				int q = 1 + ((value.getMonthValue() - 1) / 3);
				int q = value.get(IsoFields.QUARTER_OF_YEAR);
				output.append(q);
				index += 1;
			}
			// Local radix character
			else if (containsAt(format, index, "X") != null) {
				char c = DecimalFormatSymbols.getInstance().getDecimalSeparator();
				output.append(c);
				index += 1;
			}

			// Fill mode modifier; toggles between compact and fill modes for any elements
			// following the modifier in the model.
			else if (containsAt(format, index, "FM") != null) {
				fillMode = !fillMode;
				index += 2;
			}
			// TODO: Exact match modifier; toggles between lax and exact match modes for any
			// elements following the modifier in the model.
			else if (containsAt(format, index, "FX") != null) {
				index += 2;
			}
			// Literal text
			else if (containsAt(format, index, "\"") != null) {
				for (index = index + 1; index < format.length(); index++) {
					char c = format.charAt(index);
					if (c != '"') {
						output.append(c);
					} else {
						index++;
						break;
					}
				}
			}
			// Anything else
			else {
				char ch = format.charAt(index);
				if (!Characters.isAlphaOrDigit(ch)) {
					output.append(ch);
					index += 1;
				} else {
					throw new IllegalFormatFlagsException(format);
				}
			}
		}

		return output.toString();
	}

	private static String toWord(int number) {
		// variable to hold string representation of number
		String words = "";
		String unitsArray[] = { "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
				"eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen" };
		String tensArray[] = { "zero", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
				"ninety" };

		if (number == 0) {
			return "zero";
		}

		// check if number is divisible by 1 million
//		if ((number / 1000000) > 0) {
//			words += toWord(number / 1000000) + " million ";
//			number %= 1000000;
//		}
		// check if number is divisible by 1 thousand
		if ((number / 1000) > 0) {
			words += toWord(number / 1000) + " thousand ";
			number %= 1000;
		}
		// check if number is divisible by 1 hundred
		if ((number / 100) > 0) {
			words += toWord(number / 100) + " hundred ";
			number %= 100;
		}

		if (number > 0) {
			// check if number is within teens
			if (number < 20) {
				// fetch the appropriate value from unit array
				words += unitsArray[number];
			} else {
				// fetch the appropriate value from tens array
				words += tensArray[number / 10];
				if ((number % 10) > 0) {
					words += "-" + unitsArray[number % 10];
				}
			}
		}

		return words;
	}
}