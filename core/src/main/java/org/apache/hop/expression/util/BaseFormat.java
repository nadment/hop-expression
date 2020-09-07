package org.apache.hop.expression.util;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.ParseException;
import java.text.ParsePosition;

import org.apache.commons.lang.StringUtils;

public abstract class BaseFormat {

	private static final int[] ROMAN_VALUES = { 1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1 };

	private static final String[] ROMAN_NUMERALS = { "M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV",
			"I" };

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

	protected static boolean match(String value, int index, String substrings) {
		return value.regionMatches(true, index, substrings, 0, substrings.length());
	}

//	protected static boolean match(String value, int index, String... substrings) {
//		for (String substring : substrings) {
//			if (value.regionMatches(true, index, substring, 0, substring.length()))
//				return true;
//		}
//		return false;
//	}

	protected static int indexOfArray(String[] strings, String value) {
		if (value == null)
			return -1;
		for (int i = 0; i < strings.length; i++) {
			if (value.equals(strings[i]))
				return i;
		}
		return -1;
	}

	/**
	 * Parse an integer at the given position in a string
	 * 
	 * @param value    the string to parse
	 * @param position the start index for the integer in the string
	 * @param lenght   number of digits to parse in the string
	 * @return the int
	 * @throws NumberFormatException if the value is not a number
	 */
	protected static int parseInt(String value, ParsePosition position, int length)
			throws NumberFormatException {
		int index = position.getIndex();
		int result = 0;
		if (index + length > value.length())
			length = value.length() - index;

		for (int i = 0; i < length; i++) {
			int digit = Character.digit(value.charAt(index), 10);
			if (digit < 0) {
				
				if ( index== position.getIndex() ) {				
					position.setErrorIndex(index);
					throw new NumberFormatException(
						"Invalid number: " + value.substring(position.getIndex(), position.getIndex() + i));
				}
				
				break;
			}
			
			index++;
			result *= 10;
			result += digit;
		}

		position.setIndex(index);

		return result;
	}

	/**
	 * Parse an integer at the given position in a string
	 * 
	 * @param value    the string to parse
	 * @param position the start index for the integer in the string
	 * @param lenght   number of digits to parse in the string
	 * @return the signed int
	 * @throws NumberFormatException if the value is not a number
	 */
	protected static int parseSignedInt(String value, ParsePosition position, int length)
			throws NumberFormatException {
		int index = position.getIndex();
		int result = 0;
		if (index + length > value.length())
			length = value.length() - index;

		// boolean negative = false;
		char sign = value.charAt(index);
		if (sign == '-' || sign == '+') {
			index++;
			length--;
		} else  {
			sign = '+';
		}

		for (int i = 0; i < length; i++) {
			int digit = Character.digit(value.charAt(index++), 10);
			if (digit < 0) {
				position.setErrorIndex(index);
				throw new NumberFormatException(
						"Invalid number: " + value.substring(position.getIndex(), position.getIndex() + i));
			}
			result *= 10;
			result += digit;
		}

		position.setIndex(index);

		return (sign == '-') ? -result : result;
	}

	protected static String parseString(String value, ParsePosition position, String... substrings) throws ParseException {
		int index = position.getIndex();
		for (String substring : substrings) {
			if (value.regionMatches(true, index, substring, 0, substring.length())) {
				position.setIndex(index + substring.length());
				return substring;
			}
		}

		position.setErrorIndex(index);

		return null;
	}

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
	protected static Capitalization matchCapitalization(String s, int index, String... substrings) {
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

	/**
	 * Zero pad a number to a specified length
	 * 
	 * @param buffer buffer to use for padding
	 * @param value  the integer value to pad if necessary.
	 * @param length the length of the string we should zero pad
	 */
	protected static void padInt(StringBuilder buffer, int value, int length) {
		String strValue = Integer.toString(value);
		for (int i = length - strValue.length(); i > 0; i--) {
			buffer.append('0');
		}
		buffer.append(strValue);
	}

	/**
	 * Zero pad a number to a specified length
	 * 
	 * @param buffer buffer to use for padding
	 * @param value  the long value to pad if necessary.
	 * @param length the length of the string we should zero pad
	 */
	protected static void padLong(StringBuilder buffer, long value, int length) {
		String strValue = Long.toString(value);
		for (int i = length - strValue.length(); i > 0; i--) {
			buffer.append('0');
		}
		buffer.append(strValue);
	}

	/**
	 * Roman numeral month (I-XII; JAN = I).
	 * 
	 * @param number
	 * @return
	 */
	protected static String formatRomanNumeral(int number) {
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

	protected static String formatWord(int number) {
		// variable to hold string representation of number
		StringBuilder words = new StringBuilder();
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
			words.append(formatWord(number / 1000)).append(" thousand ");
			number %= 1000;
		}
		// check if number is divisible by 1 hundred
		if ((number / 100) > 0) {
			words.append(formatWord(number / 100)).append(" hundred ");
			number %= 100;
		}

		if (number > 0) {
			// check if number is within teens
			if (number < 20) {
				// fetch the appropriate value from unit array
				words.append(unitsArray[number]);
			} else {
				// fetch the appropriate value from tens array
				words.append(tensArray[number / 10]);
				if ((number % 10) > 0) {
					words.append('-').append(unitsArray[number % 10]);
				}
			}
		}

		return words.toString();
	}
}
