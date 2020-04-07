package org.apache.hop.expression.util;

import org.apache.hop.expression.ExpressionException;

public class Soundex {
	private static final char[] SOUNDEX = new char[128];

	private static final char[] HEX = "0123456789abcdef".toCharArray();

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

	/**
	 * Convert a string to a Java literal using the correct escape sequences. The
	 * literal is not enclosed in double quotes. The result can be used in
	 * properties files or in Java source code.
	 *
	 * @param s the text to convert
	 */
	public static String getStringEncode(String s) {
		StringBuilder builder = new StringBuilder(s.length());
		int length = s.length();
		for (int i = 0; i < length; i++) {
			char c = s.charAt(i);
			switch (c) {
			case '\t':
				// HT horizontal tab
				builder.append("\\t");
				break;
			case '\n':
				// LF linefeed
				builder.append("\\n");
				break;
			case '\f':
				// FF form feed
				builder.append("\\f");
				break;
			case '\r':
				// CR carriage return
				builder.append("\\r");
				break;
			case '"':
				// double quote
				builder.append("\\\"");
				break;
			case '\'':
				builder.append('\'');
				break;
			case '\\':
				// backslash
				builder.append("\\\\");
				break;
			default:
				if (c >= ' ' && (c < 0x80)) {
					builder.append(c);
				} else {
					builder.append("\\u").append(HEX[c >>> 12]).append(HEX[c >>> 8 & 0xf]).append(HEX[c >>> 4 & 0xf])
							.append(HEX[c & 0xf]);
				}
			}
		}

		return builder.toString();
	}

	/**
	 * Decode a text that is encoded as a Java string literal. The Java properties
	 * file format and Java source code format is supported.
	 *
	 * @param s the encoded string
	 * @return the string
	 */
	public static String getStringDecode(String s) {
		int length = s.length();
		StringBuilder buff = new StringBuilder(length);
		for (int i = 0; i < length; i++) {
			char c = s.charAt(i);
			if (c == '\\') {
				if (i + 1 >= s.length()) {
					throw createFormatException(s, i);
				}
				c = s.charAt(++i);
				switch (c) {
				case 't':
					buff.append('\t');
					break;
				case 'r':
					buff.append('\r');
					break;
				case 'n':
					buff.append('\n');
					break;
				case 'b':
					buff.append('\b');
					break;
				case 'f':
					buff.append('\f');
					break;
				case '#':
					// for properties files
					buff.append('#');
					break;
				case '=':
					// for properties files
					buff.append('=');
					break;
				case ':':
					// for properties files
					buff.append(':');
					break;
				case '"':
					buff.append('"');
					break;
				case '\\':
					buff.append('\\');
					break;
				case 'u': {
					try {
						c = (char) (Integer.parseInt(s.substring(i + 1, i + 5), 16));
					} catch (NumberFormatException e) {
						throw createFormatException(s, i);
					}
					i += 4;
					buff.append(c);
					break;
				}
				default:
					if (c >= '0' && c <= '9') {
						try {
							c = (char) (Integer.parseInt(s.substring(i, i + 3), 8));
						} catch (NumberFormatException e) {
							throw createFormatException(s, i);
						}
						i += 2;
						buff.append(c);
					} else {
						throw createFormatException(s, i);
					}
				}
			} else {
				buff.append(c);
			}
		}
		return buff.toString();
	}

	private static ExpressionException createFormatException(String s, int i) {
		return new ExpressionException("Bad format {0} at position {1}", s, i);
	}
}
