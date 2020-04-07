package org.apache.hop.expression.util;

import org.apache.hop.expression.ExpressionException;

public class StringEncoder {
	private static final char[] HEX = "0123456789abcdef".toCharArray();

	private StringEncoder() {
		// utility class
	}

	
	/**
	 * Convert a string to a Java literal using the correct escape sequences. The
	 * literal is not enclosed in double quotes. The result can be used in
	 * properties files or in Java source code.
	 *
	 * @param s the text to convert
	 */
	public static String encode(String s) {
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
	public static String decode(String s) {
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
