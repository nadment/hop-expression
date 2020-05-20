package org.apache.hop.expression;

import java.util.Arrays;
import java.util.Set;
import java.util.TreeSet;

import org.apache.hop.expression.ExpressionToken.Id;
import org.apache.hop.expression.util.Characters;

/**
 * Parses an expression string to return the individual tokens.
 */
public class ExpressionScanner {

	// TODO: Java 9 use unmodifiable Set.of()
	private static final Set<String> RESERVED_WORDS = new TreeSet<>(
			Arrays.asList("AS", "AND", "BETWEEN", "CASE", "DATE", "ELSE", "END", "ESCAPE", "FALSE", "FROM", "ILIKE",
					"IN", "IS", "LIKE", "NOT", "NULL", "OR", "THEN", "TIME", "TIMESTAMP", "TRUE", "WHEN", "XOR"));

	public static Set<String> getReservedWords() {
		return RESERVED_WORDS;
	}

	private String source;

	private int pos = 0;

	public ExpressionScanner(String text) {
		this.source = text;
	}

	public ExpressionToken tokenize() throws ExpressionParserException {

		while (pos < source.length()) {
			char c = source.charAt(pos);

			switch (c) {
			case ',':
				return new ExpressionToken(Id.COMMA, pos, ++pos);

			case '(':
				return new ExpressionToken(Id.LPARENTHESIS, pos, ++pos);

			case ')':
				return new ExpressionToken(Id.RPARENTHESIS, pos, ++pos);

			// Double-quoted literal text.
//			case '"': {
//				StringBuilder text = new StringBuilder();
//				int start = pos++;
//				while (pos < source.length()) {
//					c = source.charAt(pos++);
//					if (c == '"') {
//						if (pos < source.length()) {
//							char c2 = source.charAt(pos);
//							// encountered consecutive double-quotes
//							if (c2 == '"') {
//								++pos;
//								text.append(c);
//								continue;
//							}
//						}
//						break;
//					}
//					text.append(c);
//				}
//
//				if (c != '"')
//					throw new ExpressionParserException("ExpressionException.MissingEndDoubleQuotedString", source,
//							pos);
//
//				return new ExpressionToken(Id.LITERAL_TEXT, start, pos, text.toString());
//			}

			// Single-quoted literal text.
			case '\'': {
				StringBuilder text = new StringBuilder();
				int start = pos++;
				while (pos < source.length()) {
					c = source.charAt(pos++);
					if (c == '\'') {
						if (pos < source.length()) {
							char c2 = source.charAt(pos);
							// encountered consecutive single-quotes
							if (c2 == '\'') {
								++pos;
								text.append(c);
								continue;
							}
						}
						break;
					}
					text.append(c);
				}

				if (c != '\'')
					throw new ExpressionParserException("ExpressionException.MissingEndSingleQuotedString", source,
							pos);

				return new ExpressionToken(Id.LITERAL_STRING, start, pos, text.toString());
			}

			case '=': {
				int start = pos++;
				if (pos < source.length()) {
					c = source.charAt(pos);
					if (c == '~') {
						pos++;
						return new ExpressionToken(Id.CONTAINS, start, pos);
					}
				}
				return new ExpressionToken(Id.EQUAL, start, pos);
			}

			case '+':
				return new ExpressionToken(Id.PLUS, pos, ++pos);

			case '-':
				return new ExpressionToken(Id.MINUS, pos, ++pos);

			case '*':
				return new ExpressionToken(Id.MULTIPLY, pos, ++pos);

			case '%':
				return new ExpressionToken(Id.MODULUS, pos, ++pos);

			case '<': {
				// parse less symbol
				int start = pos++;
				if (pos < source.length()) {
					c = source.charAt(pos);
					if (c == '=') {
						pos++;
						return new ExpressionToken(Id.LESS_THAN_OR_EQUAL, start, pos);
					}
					if (c == '>') {
						pos++;
						return new ExpressionToken(Id.LESS_THAN_OR_GREATER_THAN, start, pos);
					}
				}
				return new ExpressionToken(Id.LESS_THAN, start, pos);
			}

			// parse greater symbol
			case '>': {
				int start = pos++;
				if (pos < source.length()) {
					c = source.charAt(pos);
					if (c == '=') {
						pos++;
						return new ExpressionToken(Id.GREATER_THAN_OR_EQUAL, start, pos);
					}
				}
				return new ExpressionToken(Id.GREATER_THAN, start, pos);
			}

			// parse not equal symbol
			case '!': {
				int start = pos++;
				if (pos < source.length()) {
					c = source.charAt(pos);
					if (c == '=') {
						pos++;
						return new ExpressionToken(Id.NOT_EQUAL, start, pos);
					}
				}
				throw new ExpressionParserException("ExpressionException.SyntaxError", source, pos);
			}

			// possible start of '/*' or '//' comment
			case '/': {
				int start = pos++;
				if (pos < source.length()) {
					char c1 = source.charAt(pos);
					if (c1 == '*') {
						int end = source.indexOf("*/", pos + 1);
						if (end < 0) {
							end = source.length();
						} else {
							end += "*/".length();
						}
						pos = end;
						return new ExpressionToken(Id.COMMENT, start, pos);
					}
					if (c1 == '/') {
						pos++;

						while (pos < source.length()) {
							c = source.charAt(pos);
							if (c == '\r' || c == '\n')
								break;
							pos++;
						}

						return new ExpressionToken(Id.COMMENT, start, pos);
					}
				}
				return new ExpressionToken(Id.DIVIDE, start, pos);
			}

			case '~':
				return new ExpressionToken(Id.BITWISE_NOT, pos, ++pos);

			case '&':
				return new ExpressionToken(Id.BITWISE_AND, pos, ++pos);

			case '^':
				return new ExpressionToken(Id.BITWISE_XOR, pos, ++pos);

			// Bitwise OR operator or concat symbol
			case '|': {
				int start = pos++;
				if (pos < source.length()) {
					c = source.charAt(pos);
					if (c == '|') {
						pos++;
						return new ExpressionToken(Id.CONCAT, start, pos);
					}
				}
				return new ExpressionToken(Id.BITWISE_OR, start, pos);
			}

			// Escape field name matching reserved words
			case '[': {
				int start = pos++;
				while (pos < source.length()) {
					c = source.charAt(pos++);
					if (!Characters.isAlphaOrDigit(c)) {
						break;
					}
				}

				if (c == ']') {
					String name = source.substring(start + 1, pos - 1).toUpperCase();
					return new ExpressionToken(Id.IDENTIFIER, start, pos, name);
				}

				throw new ExpressionParserException("ExpressionException.SyntaxError", source, pos);
			}

			// Variable
//			case '$': {
//				int start = pos++;
//				if (pos < source.length()) {
//
//					c = source.charAt(pos++);
//					if (c != '{')
//						throw new ExpressionParserException("ExpressionException.SyntaxError", source, pos);
//
//					while (pos < source.length()) {
//						c = source.charAt(++pos);
//						if (!Characters.isAlphaOrDigit(c)) {
//							break;
//						}
//					}
//
//					if (c != '}')
//						throw new ExpressionParserException("ExpressionException.SyntaxError", source, pos);
//
//					String name = source.substring(start + 2, pos++).toUpperCase();
//					return new ExpressionToken(Id.VARIABLE, start, pos, name);
//				}
//				throw new ExpressionParserException("ExpressionException.SyntaxError", source, pos);
//			}

			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9': {

				int start = pos++;

				// Hexadecimal number 0xABCDEF
				if (c == '0' && pos < source.length() && (source.charAt(pos) == 'x')) {
					do {
						pos++;
					} while (pos < source.length() && Characters.isHexDigit(source.charAt(pos)));

					return new ExpressionToken(Id.LITERAL_BINARY_HEX, start, pos, source.substring(start + 2, pos));
				}

				// Binary number 0b01101011
				if (c == '0' && pos < source.length() && (source.charAt(pos) == 'b')) {
					do {
						pos++;
					} while (pos < source.length() && (source.charAt(pos) == '0' || source.charAt(pos) == '1'));

					return new ExpressionToken(Id.LITERAL_BINARY_BIT, start, pos, source.substring(start + 2, pos));
				}

				// Integer part
				while (pos < source.length() && Characters.isDigit(source.charAt(pos))) {
					pos++;
				}

				// Use dot for decimal separator
				if (pos < source.length() && source.charAt(pos) == '.') {
					pos++;
				}

				// Decimal part
				while (pos < source.length() && Characters.isDigit(source.charAt(pos))) {
					pos++;
				}

				// Exponentiation part
				if (pos < source.length() && Characters.isExponentChar(source.charAt(pos))) {
					pos++;

					if (pos < source.length() && (source.charAt(pos) == '+' || source.charAt(pos) == '-')) {
						pos++;
					}
					while (pos < source.length() && Character.isDigit(source.charAt(pos))) {
						pos++;
					}
				}

				return new ExpressionToken(Id.LITERAL_NUMBER, start, pos, source.substring(start, pos));
			}

			default:
				if (Character.isWhitespace(c)) {
					++pos;
					continue;
				}

				// Probably a letter or digit. Start an identifier.
				// Other characters, e.g. *, ! are also included
				// in identifiers.
				int start = pos++;
				boolean isFunction = false;
				loop: while (pos < source.length()) {
					c = source.charAt(pos);
					switch (c) {
					case '(':
						isFunction = true;
					case ')':
					case '/':
					case '*':
					case ',':
					case '^':
					case '&':
					case '>':
					case '<':
					case '=':
					case '~':
					case '+':
					case '-':
					case '!':
					case '|':
					case '$':
					case '[':
					case ']':
						break loop;

					default:
						if (Character.isWhitespace(c)) {
							break loop;
						} else {
							++pos;
						}
					}
				}
				String identifier = source.substring(start, pos);
				String name = identifier.toUpperCase();

				if (isFunction && Operator.getFunction(name) != null) {
					return new ExpressionToken(Id.FUNCTION, start, pos, name);
				}

				// Reserved words: AS, AND, LIKE, NOT, TRUE, FALSE, OR
				if (RESERVED_WORDS.contains(name)) {
					return new ExpressionToken(Id.valueOf(name), start, pos, name);
				}

				DataType type = DataType.of(name);
				if (type != null) {
					return new ExpressionToken(Id.DATATYPE, start, pos, name);
				}

				DatePart part = DatePart.of(name);
				if (part != null) {
					return new ExpressionToken(Id.DATEPART, start, pos, name);
				}

				return new ExpressionToken(Id.IDENTIFIER, start, pos, identifier);
			}

		}
		return null;
	}
}
