package org.apache.hop.expression.util;

public class Characters {
	private Characters() {
		super();
	}

	private static final byte[] FLAGS = new byte[256];

	private static final byte IS_DIGIT = 0x01;

	private static final byte IS_HEXDIGIT = 0x02;

	private static final byte IS_ALPHA = 0x04;

	static {
		for (int ch = '0'; ch <= '9'; ch++) {
			FLAGS[ch] |= IS_DIGIT | IS_HEXDIGIT;
		}
		for (int ch = 'A'; ch <= 'F'; ch++) {
			FLAGS[ch] |= IS_HEXDIGIT;
		}
		for (int ch = 'a'; ch <= 'f'; ch++) {
			FLAGS[ch] |= IS_HEXDIGIT;
		}
		for (int ch = 'A'; ch <= 'Z'; ch++) {
			FLAGS[ch] |= IS_ALPHA;
		}
		for (int ch = 'a'; ch <= 'z'; ch++) {
			FLAGS[ch] |= IS_ALPHA;
		}
	}

	public static boolean isDigit(char ch) {
		if (ch > 255) {
			return false;
		}
		return (FLAGS[ch] & IS_DIGIT) != 0;
	}

	public static boolean isHexDigit(char ch) {
		if (ch > 255) {
			return false;
		}
		return (FLAGS[ch] & IS_HEXDIGIT) != 0;
	}

	public static boolean isAlphabetic(char ch) {
		if (ch > 255) {
			return false;
		}
		return (FLAGS[ch] & IS_ALPHA) != 0;
	}

	public static boolean isExponentChar(char ch) {
		return ch == 'e' || ch == 'E';
	}
}
