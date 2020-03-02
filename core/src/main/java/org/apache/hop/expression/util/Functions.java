package org.apache.hop.expression.util;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.regex.Pattern;

import org.apache.commons.math3.util.FastMath;
import org.apache.hop.expression.ExpressionException;

public class Functions {
	/**
	 * The maximum size to which the padding can expand.
	 */
	private static final int PAD_LIMIT = 8192;

	private static final char[] SOUNDEX = new char[128];
	private static final int SOUNDEX_LENGTH = 4;

	static {
		// SOUNDEX_INDEX
		String index = "7AEIOUY8HW1BFPV2CGJKQSXZ3DT4L5MN6R";
		char number = 0;
		for (int i = 0, length = index.length(); i < length; i++) {
			char c = index.charAt(i);
			if (c < '9') {
				number = c;
			} else {
				SOUNDEX[c] = number;
				SOUNDEX[Character.toLowerCase(c)] = number;
			}
		}
	}

	private Functions() {
		// utility class
	}

	public static String soundex(String s) {
		int len = s.length();
		char[] chars = { '0', '0', '0', '0' };
		char lastDigit = '0';
		for (int i = 0, j = 0; i < len && j < 4; i++) {
			char c = s.charAt(i);
			char newDigit = c > SOUNDEX.length ? 0 : SOUNDEX[c];
			if (newDigit != 0) {
				if (j == 0) {
					chars[j++] = c;
					lastDigit = newDigit;
				} else if (newDigit <= '6') {
					if (newDigit != lastDigit) {
						chars[j++] = newDigit;
						lastDigit = newDigit;
					}
				} else if (newDigit == '7') {
					lastDigit = newDigit;
				}
			}
		}
		return new String(chars);
	}

	public static int difference(String s0, String s1) {
		String result0 = soundex(s0);
		String result1 = soundex(s1);
		for (int i = 0; i < SOUNDEX_LENGTH; i++) {
			if (result0.charAt(i) != result1.charAt(i)) {
				return i;
			}
		}
		return SOUNDEX_LENGTH;
	}
	
	
	public static String lpad(String str, int length, String pad) {

	    if (length < 0) {
	    	length = 0;
        }		
	    else if (length > PAD_LIMIT) {
			new ExpressionException("Paddind length exceeds maximum limit: " + PAD_LIMIT);
		}

		// If this parameter is omitted, the function will pad spaces
		if (pad == null) {
			pad = " ";
		}

		final int size = pad.length();
		final int index = length - str.length();

		if (index <= 0) {
			return str.substring(0, length);
		} else if (size == 0) {
			// nothing to do
		} else if (index == size) {
			return pad.concat(str);
		} else if (index < size) {
			return pad.substring(0, index).concat(str);
		} else {
			final char[] padding = new char[index];
			final char[] padChars = pad.toCharArray();
			for (int i = 0; i < index; i++) {
				padding[i] = padChars[i % size];
			}
			str = new String(padding).concat(str);
		}

		return str;
	}

	public static String rpad(String str, int length, String pad) {

	    if (length < 0) {
	    	length = 0;
        }		
		if (length > PAD_LIMIT) {
			new ExpressionException("Paddind length exceeds maximum limit: " + PAD_LIMIT);
		}

		// If this parameter is omitted, the function will pad spaces
		if (pad == null) {
			pad = " ";
		}

		final int size = pad.length();
		final int index = length - str.length();

		if (index <= 0) {
			return str.substring(0, length);
		} else if (size == 0) {
			// nothing to do
		} else if (index == size) {
			return str.concat(pad);
		} else if (index < size) {
			return str.concat(pad.substring(0, index));
		} else {
			final char[] padding = new char[index];
			final char[] padChars = pad.toCharArray();
			for (int i = 0; i < index; i++) {
				padding[i] = padChars[i % size];
			}
			str = str.concat(new String(padding));
		}

		return str;
	}
	
	
	public static BigDecimal truncate(BigDecimal b0, int b1) {
		return b0.movePointRight(b1).setScale(0, RoundingMode.DOWN).movePointLeft(b1);
	}

	public static double acosh(double x) {
		//return Math.log(x + Math.sqrt(x*x - 1.0d));
		return FastMath.acosh(x);
	}
	
	public static double asinh(double x) {
		//return Math.log(x + Math.sqrt(1 + x * x));
		return FastMath.asinh(x);
	}

	public static double atanh(double x) {
		//return Math.log(Math.sqrt(1 + x) / Math.sqrt(1 - x));
		return FastMath.atanh(x);
	}

	
	private static int makeRegexpFlags(String stringFlags) {
		int flags = 0;
		if (stringFlags != null) {
			for (int i = 0; i < stringFlags.length(); ++i) {
				switch (stringFlags.charAt(i)) {
				case 'i':
					flags |= Pattern.CASE_INSENSITIVE;
					break;
				case 'c':
					flags &= ~Pattern.CASE_INSENSITIVE;
					break;
				case 'n':
					flags |= Pattern.DOTALL;
					break;
				case 'm':
					flags |= Pattern.MULTILINE;
					break;
				default:
					throw new ExpressionException("Invalid input for Regexp");
				}
			}
		}
		return flags;
	}
	
	public static String left(String s, int count) {
		if (count < 0) {
			count = 0;
			//throw new ExpressionException("LEFT: Length must be greater than or equal to 0");
		} else if (count > s.length()) {
			count = s.length();
		}
		return s.substring(0, count);
	}

	public static String right(String s, int count) {
        if (count < 0) {
            count = 0;
			//throw new ExpressionException("RIGHT: Length must be greater than or equal to 0");
        } else if (count > s.length()) {
            count = s.length();
        }
        return s.substring(s.length() - count);
    }


}
