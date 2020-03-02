package org.apache.hop.expression.util;

public class Soundex {
	private static final char[] SOUNDEX = new char[128];

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

	private Soundex() {
		// utility class
	}

	public static String getSoundex(String s) {
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

}
