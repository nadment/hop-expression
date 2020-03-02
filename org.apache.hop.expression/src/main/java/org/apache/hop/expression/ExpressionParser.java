package org.apache.hop.expression;

import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.EnumSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.TimeZone;

import org.apache.commons.lang.StringUtils;
import org.apache.hop.expression.util.Characters;

public class ExpressionParser {

	/**
	 * Enumerates the possible types of {@link Token}.
	 */

	private enum Id {
		/**
		 * Expression not covered by any other {@link Token} value.
		 *
		 * @see #OTHER_FUNCTION
		 */
		OTHER,

		AS,

		/**
		 * Case when operator
		 */
		CASE,

		CAST,

		/**
		 * Concat operator <code>||<code>
		 */
		CONCAT("||"),

		/**
		 * Contains operator <code>=~<code>
		 */
		CONTAINS("=~"),

		/**
		 * Comment
		 */
		COMMENT,

		/**
		 * Comma separator
		 */
		COMMA(","),

		/**
		 * Left parenthesis
		 */
		LPARENTHESIS("("),

		/**
		 * Right parenthesis
		 */
		RPARENTHESIS(")"),

		/**
		 * Literal integer.
		 */
		INTEGER,

		/**
		 * Literal number.
		 */
		NUMBER,

		/** Literal hex number */
		HEX,

		/**
		 * Identifier
		 */
		IDENTIFIER,

		/**
		 * Function
		 */
		FUNCTION,

		/**
		 * Literal string.
		 */
		STRING,

		/**
		 * The arithmetic division operator, "/".
		 */
		DIVIDE("/"),

		/**
		 * The arithmetic multiplication operator, "*".
		 */
		MULTIPLY("*"),

		/**
		 * ESCAPE keyword for like operator
		 */
		ESCAPE,

		/**
		 * The arithmetic power operator, "^".
		 */
		POWER("^"),

		/**
		 * The arithmetic remainder operator, "MOD" (and "%" in some dialects).
		 */
		MODULUS("%"),

		/**
		 * The arithmetic unary plus (positive) operator "+".
		 */
		PLUS("+"),

		/**
		 * The arithmetic unary minus (negative) operator "-".
		 */
		MINUS("-"),

		/**
		 * The arithmetic addition operator "+".
		 */
		ADD("+"),

		/**
		 * The arithmetic subtract operator "-".
		 */
		SUBTRACT("-"),

		/**
		 * The "IN" operator.
		 */
		IN,

		/**
		 * The "BETWEEN" operator.
		 */
		BETWEEN,

		/**
		 * The less-than operator "&lt;".
		 */
		LESS_THAN("<"),

		/**
		 * The greater-than operator "&gt;".
		 */
		GREATER_THAN(">"),

		/**
		 * The less-than-or-equal operator "&lt;=".
		 */
		LESS_THAN_OR_EQUAL("<="),

		/**
		 * The greater-than-or-equal operator "&gt;=".
		 */
		GREATER_THAN_OR_EQUAL(">="),

		/**
		 * The equals operator "=".
		 */
		EQUAL("="),

		/**
		 * Compares whether two expressions are equal.
		 * 
		 * The function is NULL-safe, meaning it treats NULLs as known values for
		 * comparing equality. Note that this is different from the EQUAL comparison
		 * operator (=), which treats NULLs as unknown values.
		 */
		EQUAL_NULL,

		/**
		 * The not-equals operator, "&#33;=" or "&lt;&gt;". The latter is standard, and
		 * preferred.
		 */
		NOT_EQUAL("<>"),

		/**
		 * The "DATE" litteral.
		 */
		DATE,
		/**
		 * The "TIMESTAMP" litteral.
		 */
		TIMESTAMP,

		/**
		 * The logical "OR" operator.
		 */
		OR,

		/**
		 * The logical "XOR" operator.
		 */
		XOR,

		/**
		 * The logical "AND" operator.
		 */
		AND,

		/**
		 * The "LIKE" operator.
		 */
		LIKE,

		/**
		 * The logical "NOT" operator.
		 */
		NOT,

		/**
		 * The value "NULL".
		 */
		NULL,

		/**
		 * The "IS NULL" or "IS TRUE" operator.
		 */
		IS,

		/**
		 * The value "TRUE".
		 */
		TRUE,

		/**
		 * The value "FALSE".
		 */
		FALSE,

		ELSE, THEN, END, WHEN,

		BOOLEAN, BIGNUMBER, BINARY;

		/**
		 * Expression keywords.
		 */
		public static final Set<Id> KEYWORDS = EnumSet.of(AND, AS, BETWEEN, CASE, CAST, DATE, ELSE, END, ESCAPE, FALSE,
				IN, IS, LIKE, NOT, NULL, TRUE, OR, TIMESTAMP, THEN, WHEN, XOR);

		public static final Set<Id> DATA_TYPES = EnumSet.of(INTEGER, BOOLEAN, NUMBER, BIGNUMBER, STRING, BINARY);

		/**
		 * Returns whether this {@code Type} belongs to a given category.
		 *
		 * @param category Category
		 * @return Whether this kind belongs to the given category
		 */
		public final boolean is(Collection<Id> category) {
			return category.contains(this);
		}

		private final String source;

		Id() {
			this.source = name();
		}

		Id(final String source) {
			this.source = source;
		}

		@Override
		public String toString() {
			return source;
		}
	}

	private static class Token {

		private final Id id;

		private final String text;

		// The position of the first character of this Token.
		private final int position;

		protected Token(Id id, int position) {
			this(id, position, null);
		}

		protected Token(Id id, int position, String text) {
			this.id = id;
			this.position = position;
			this.text = text;
		}

		public final boolean is(Id type) {
			return this.id == type;
		}

//			public final boolean is(Collection<Expression.Kind> category) {
//				return kind.is(category);
//			}

		public String toString() {
			String s = (text == null) ? id.toString() : (id + "(" + text + ")");

			return String.valueOf(position) + ":" + s;
		}

		public Id getId() {
			return id;
		}

		public int getPosition() {
			return position;
		}

		public String getText() {
			return text;
		}

	}

	/** locale-neutral big decimal format. */
	public static final DecimalFormat DECIMAL_FORMAT = new DecimalFormat("0.0b",
			new DecimalFormatSymbols(Locale.ENGLISH));

	/** The SimpleDateFormat string for ISO dates, "yyyy-MM-dd". */
	public static final String DATE_FORMAT_STRING = "yyyy-MM-dd";

	/** The SimpleDateFormat string for ISO times, "HH:mm:ss". */
	public static final String TIME_FORMAT_STRING = "HH:mm:ss";

	/** The SimpleDateFormat string for ISO timestamps, "yyyy-MM-dd HH:mm:ss". */
	public static final String TIMESTAMP_FORMAT_STRING = DATE_FORMAT_STRING + " " + TIME_FORMAT_STRING;

	/** The UTC time zone. */
	public static final TimeZone UTC_ZONE = TimeZone.getTimeZone("UTC");

	/** The Java default time zone. */
	public static final TimeZone DEFAULT_ZONE = TimeZone.getDefault();

	/**
	 * The number of milliseconds in a second.
	 */
	public static final long MILLIS_PER_SECOND = 1000L;

	/**
	 * The number of milliseconds in a minute.
	 */
	public static final long MILLIS_PER_MINUTE = 60000L;

	/**
	 * The number of milliseconds in an hour.
	 */
	public static final long MILLIS_PER_HOUR = 3600000L; // = 60 * 60 * 1000

	/**
	 * The number of milliseconds in a day.
	 *
	 * <p>
	 * This is the modulo 'mask' used when converting TIMESTAMP values to DATE and
	 * TIME values.
	 */
	public static final long MILLIS_PER_DAY = 86400000; // = 24 * 60 * 60 * 1000

	/**
	 * The number of seconds in a day.
	 */
	public static final long SECONDS_PER_DAY = 86_400; // = 24 * 60 * 60

	private final String source;

	private int pos = 0;
	private List<Token> tokens = new ArrayList<>();
	private int index = 0;

	protected ExpressionParser(String source) {
		super();
		this.source = source.trim();
	}

	private Token tokenize() throws ExpressionParserException {

		int start = 0;

		while (pos < source.length()) {
			char c = source.charAt(pos);
			final String sequence;
			boolean isEndFound = false;

			switch (c) {
			case ',':
				return new Token(Id.COMMA, pos++);

			case '(':
				return new Token(Id.LPARENTHESIS, pos++);

			case ')':
				return new Token(Id.RPARENTHESIS, pos++);

			case '"': // Parse double-quoted identifier.
				start = pos++;
				while (pos < source.length()) {
					c = source.charAt(pos);
					++pos;
					if (c == '"') {
						if (pos < source.length()) {
							char c1 = source.charAt(pos);
							if (c1 == '"') {
								// encountered consecutive
								// double-quotes; still in identifier
								++pos;
							} else {
								isEndFound = true;
								break;
							}
						} else {
							isEndFound = true;
							break;
						}
					}
				}

				if (!isEndFound)
					throw new ExpressionParserException("ExpressionException.MissingEndDoubleQuotedString", source,
							pos);

				sequence = source.substring(start + 1, pos - 1);

				return new Token(Id.STRING, start, sequence);

			case '\'': // Parse single-quoted identifier.
				start = pos++;
				while (pos < source.length()) {
					c = source.charAt(pos);
					++pos;
					if (c == '\'') {
						if (pos < source.length()) {
							char c1 = source.charAt(pos);
							if (c1 == '\'') {
								// encountered consecutive
								// single-quotes; still in identifier
								++pos;
							} else {
								isEndFound = true;
								break;
							}
						} else {
							isEndFound = true;
							break;
						}
					}
				}
				if (!isEndFound)
					throw new ExpressionParserException("ExpressionException.MissingEndSingleQuotedString", source,
							pos);

				sequence = source.substring(start + 1, pos - 1);
				return new Token(Id.STRING, start, sequence);

			case '=':
				start = pos++;
				if (pos < source.length()) {
					c = source.charAt(pos);
					if (c == '~') {
						pos++;
						return new Token(Id.CONTAINS, start, "=~");
					}
				}
				return new Token(Id.EQUAL, pos);

			case '+':
				return new Token(Id.PLUS, pos++);

			case '-':
				return new Token(Id.MINUS, pos++);

			case '^':
				return new Token(Id.POWER, pos++);

			case '*':
				return new Token(Id.MULTIPLY, pos++);

			case '%':
				return new Token(Id.MODULUS, pos++);

			case '<':
				// parse less symbol
				start = pos++;
				if (pos < source.length()) {
					c = source.charAt(pos);
					if (c == '=') {
						pos++;
						return new Token(Id.LESS_THAN_OR_EQUAL, start, "<=");
					}
					if (c == '>') {
						pos++;
						return new Token(Id.NOT_EQUAL, start, "<>");
					}
				}
				return new Token(Id.LESS_THAN, start, "<");

			case '>': // parse greater symbol
				start = pos++;
				if (pos < source.length()) {
					c = source.charAt(pos);
					if (c == '=') {
						pos++;
						return new Token(Id.GREATER_THAN_OR_EQUAL, start, ">=");
					}
				}
				return new Token(Id.GREATER_THAN, start, ">");

			case '!': // parse not equal symbol
				start = pos++;
				if (pos < source.length()) {
					c = source.charAt(pos);
					if (c == '=') {
						pos++;
						return new Token(Id.NOT_EQUAL, start, "<>");
					}
				}
				throw new ExpressionParserException("ExpressionException.SyntaxError", source, pos);

			case '/': // possible start of '/*' or '//' comment
				if (pos < source.length()) {
					start = pos;
					char c1 = source.charAt(pos + 1);
					if (c1 == '*') {
						int end = source.indexOf("*/", pos + 2);
						if (end < 0) {
							end = source.length();
						} else {
							end += "*/".length();
						}
						pos = end;
						return new Token(Id.COMMENT, start);
					}
					if (c1 == '/') {
						pos += 2;

						while (pos < source.length()) {
							c = source.charAt(pos);
							switch (c) {
							case '\r':
							case '\n':
								break;
							default:
								pos++;
							}
						}

						return new Token(Id.COMMENT, start);
					}
					pos++;
					return new Token(Id.DIVIDE, start, "/");
				}

			case '|': // parse concat symbol
				start = pos++;
				if (pos < source.length()) {
					c = source.charAt(pos);
					if (c == '|') {
						pos++;
						return new Token(Id.CONCAT, start, "||");
					}
				}

				throw new ExpressionParserException("ExpressionException.SyntaxError", source, pos);

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
				boolean isDecimalPartFound = false;
				boolean isDecimalSeparatorFound = false;
				boolean isExponentSymbolFound = false;

				start = pos++;

				// Hexadecimal number 0xABCDEF
				if (c == '0' && pos < source.length() && (source.charAt(pos) == 'x')) {
					do {
						pos++;
					} while (pos < source.length() && Characters.isHexDigit(source.charAt(pos)));

					return new Token(Id.HEX, start, source.substring(start + 2, pos));
				}

				// Binary number 0b01101011
				if (c == '0' && pos < source.length() && (source.charAt(pos) == 'b')) {
					do {
						pos++;
					} while (pos < source.length() && (source.charAt(pos) == '0' || source.charAt(pos) == '1'));

					return new Token(Id.BINARY, start, source.substring(start + 2, pos));
				}

				while (pos < source.length() && Characters.isDigit(source.charAt(pos))) {
					pos++;
				}
				// Use dot for decimal separator
				if (pos < source.length() && source.charAt(pos) == '.') {
					isDecimalSeparatorFound = true;
					pos++;
				}
				while (pos < source.length() && Characters.isDigit(source.charAt(pos))) {
					pos++;
					isDecimalPartFound = true;
				}
				if (pos < source.length() && Characters.isExponentChar(source.charAt(pos))) {
					pos++;
					isExponentSymbolFound = true;
				}
				if (pos < source.length() && (source.charAt(pos) == '+' || source.charAt(pos) == '-')
						&& isExponentSymbolFound) {
					pos++;
				}
				while (pos < source.length() && Character.isDigit(source.charAt(pos)) && isExponentSymbolFound) {
					pos++;
				}

				sequence = source.substring(start, pos);

				if (isDecimalSeparatorFound)
					return new Token(Id.NUMBER, start, sequence);

				// TODO: implement parsing BigNumber

				return new Token(Id.INTEGER, start, sequence);
			}

			default:
				if (Character.isWhitespace(c)) {
					++pos;
					break;
				} else {
					// Probably a letter or digit. Start an identifier.
					// Other characters, e.g. *, ! are also included
					// in identifiers.
					start = pos++;
					loop: while (pos < source.length()) {
						c = source.charAt(pos);
						switch (c) {
						case '(':
						case ')':
						case '/':
						case '*':
						case ',':
						case '^':
						case '>':
						case '<':
						case '=':
						case '~':
						case '+':
						case '-':
						case '!':
						case '|':
							break loop;

						default:
							if (Character.isWhitespace(c)) {
								break loop;
							} else {
								++pos;
							}
						}
					}
					String name = source.substring(start, pos).toUpperCase();

					// keyword, e.g. AS, AND, LIKE, NOT, TRUE, FALSE, OR
					try {

						Id token = Id.valueOf(name);
						if (token != null && (token.is(Id.KEYWORDS))) {
							return new Token(token, start, name);
						}

					} catch (Exception e) {

					}

					if (Function.getFunction(name) != null) {
						return new Token(Id.FUNCTION, start, name);
					}

					return new Token(Id.IDENTIFIER, start, name);
				}
			}
		}
		return null;
	}

	protected int getPosition() {

		if (index > 0 && index < tokens.size())
			return this.next().getPosition();

		return pos;
	}

	protected boolean hasNext() {
		return index < tokens.size();
	}

	protected Token next() {
		if (!hasNext())
			return null;
		Token token = tokens.get(index);
		index++;
		return token;
	}

	protected boolean next(Id id) {

		if (hasNext()) {
			if (tokens.get(index).is(id)) {
				index++;
				return true;
			}
		}

		return false;
	}

	protected boolean is(Id id) {

		if (hasNext()) {
			if (tokens.get(index).is(id)) {
				return true;
			}
		}
		return false;
	}

	public Expression parse() throws ExpressionException {
		// System.out.println("Parse: " + source);

		if (StringUtils.isEmpty(source))
			return Value.NULL;

		// Tokenize
		for (Token token = tokenize(); token != null; token = tokenize()) {
			tokens.add(token);
		}

		Expression expression = this.parseLogicalOr();

		if (hasNext()) {
			throw new ExpressionParserException("ExpressionException.SyntaxError", source, this.getPosition());
		}

		return expression;
	}

	/**
	 * Parse logical OR expression
	 * 
	 * LogicalXorExpression ( OR LogicalXorExpression())*
	 */
	private Expression parseLogicalOr() throws ExpressionException {
		Expression expression = this.parseLogicalXor();
		while (next(Id.OR)) {
			// System.out.println("Parse OR");
			expression = new ExpressionCall(Operator.OR, expression, parseLogicalXor());
		}

		return expression;
	}

	/**
	 * Parse logical XOR expression
	 * 
	 * LogicalAndExpression ( XOR LogicalAndExpression())
	 * 
	 * @return Expression
	 */
	private Expression parseLogicalXor() throws ExpressionException {
		Expression expression = this.parseLogicalAnd();
		while (next(Id.XOR)) {
			expression = new ExpressionCall(Operator.XOR, expression, parseLogicalAnd());
		}

		return expression;
	}

	/**
	 * Parse logical AND expression
	 * 
	 * UnaryLogicalExpression ( AND UnaryLogicalExpression)
	 * 
	 * @return Expression
	 */
	private Expression parseLogicalAnd() throws ExpressionException {
		Expression expression = this.parseUnaryLogical();
		while (next(Id.AND)) {
			expression = new ExpressionCall(Operator.AND, expression, parseUnaryLogical());
		}

		return expression;
	}

	/**
	 * ExponentExpression ( (* | / | %) ExponentExpression())*
	 * 
	 * @return Expression
	 */
	private Expression parseMultiplicative() throws ExpressionException {
		Expression expression = this.parseUnary();

		while (hasNext()) {
			if (next(Id.MULTIPLY)) {
				expression = new ExpressionCall(Operator.MULTIPLY, expression, this.parseUnary());
			} else if (next(Id.DIVIDE)) {
				expression = new ExpressionCall(Operator.DIVIDE, expression, this.parseUnary());
			} else if (next(Id.MODULUS)) {
				expression = new ExpressionCall(Operator.MODULUS, expression, this.parseUnary());
			} else
				break;
		}

		return expression;
	}

	/** UnaryExpression ( ^ UnaryExpression)* */
	private Expression parsePower() throws ExpressionException {
		Expression expression = this.parseTerm();
		if (next(Id.POWER)) {
			return new ExpressionCall(Operator.POWER, expression, this.parseTerm());
		}
		return expression;
	}

	/** (('+' | '-') PrimaryExpression ) */
	private Expression parseUnary() throws ExpressionException {

		if (next(Id.MINUS)) {
			return new ExpressionCall(Operator.NEGATE, this.parsePower());
		}
		if (next(Id.PLUS)) {
			// Ignore
		}

		return this.parsePower();
	}

	/** [NOT] RelationalExpression */
	private Expression parseUnaryLogical() throws ExpressionException {

		if (next(Id.NOT)) {
			return new ExpressionCall(Operator.NOT, this.parseCase());
		}

		return this.parseCase();
	}

	/** TRUE | FALSE | NULL */
	private Expression parseBasic() throws ExpressionParserException {

		Token token = next();

		if (token == null)
			throw new ExpressionParserException("ExpressionException.SyntaxError", source, this.getPosition());
		// System.out.println("Parse basic: " + token);

		switch (token.getId()) {
		case TRUE:
			return Value.of(true);
		case FALSE:
			return Value.of(false);
		case NULL:
			return Value.NULL;
		default:
			throw new ExpressionParserException("ExpressionException.SyntaxError", source, token.getPosition());
		}
	}

	/** Literal | Identifier | '(' Expression ')' | Function */
	private Expression parseTerm() throws ExpressionException {

		Token token = next();
		// System.out.println("Parse primary: " + token);

		if (token != null) {
			switch (token.getId()) {
			case TRUE:
				return Value.TRUE;
			case FALSE:
				return Value.FALSE;
			case NULL:
				return Value.NULL;
			case STRING:
				return Value.of(token.getText());
			case INTEGER:
				return parseLiteralInteger(token, 10);
			case HEX:
				return parseLiteralInteger(token, 16);
			case BINARY:
				return parseLiteralInteger(token, 2);
			case NUMBER:
			case BIGNUMBER:
				return parseLiteralNumber(token);
			case DATE:
				return parseLiteralDate();
			case TIMESTAMP:
				return parseLiteralTimestamp();
			case CAST:
				return parseCast();
			case IDENTIFIER:
				return new ExpressionIdentifier(token.getText());
			case FUNCTION:
				Function function = Function.getFunction(token.getText());
				List<Expression> params = new ArrayList<>();

				token = next();
				if (token.is(Id.LPARENTHESIS)) {
					// tokenizer.next();
					// No param function
					if (is(Id.RPARENTHESIS)) {
						next();
						return new ExpressionCall(function, params);
					}

					params.add(this.parseLogicalOr());
					token = next();

					while (token.is(Id.COMMA)) {
						params.add(this.parseLogicalOr());
						token = next();
					}

					if (token.is(Id.RPARENTHESIS)) {
						return new ExpressionCall(function, params);
					}
					throw new ExpressionParserException("ExpressionException.MissingRightParenthesis", source,
							token.getPosition());
				}

			case LPARENTHESIS:
				if (is(Id.RPARENTHESIS)) {
					throw new ExpressionParserException("ExpressionException.EmptyParenthesis", source,
							this.getPosition());
				}

				Expression expression = this.parseLogicalOr();
				token = next();
				if (token.is(Id.RPARENTHESIS)) {
					return expression;
				}
				throw new ExpressionParserException("ExpressionException.UnbalancedParenthesis", source,
						this.getPosition());
			default:
				// throw new ExpressionParserException("Not a valide expression",
				// tokenizer.getSource(), token.getPosition());
			}
		}
		throw new ExpressionParserException("ExpressionException.SyntaxError", source, this.getPosition());
	}

	/**
	 * AdditiveExpression ( Operator AdditiveExpression | InClause | BetweenClause |
	 * LikeClause | IsNullClause
	 */
	private Expression parseRelational() throws ExpressionException {
		Expression expression = this.parseAdditive();
		boolean not = false;

		if (next(Id.EQUAL)) {
			return new ExpressionCall(Operator.EQUALS, expression, this.parseAdditive());
		}
		if (next(Id.NOT_EQUAL)) {
			return new ExpressionCall(Operator.NOT_EQUALS, expression, this.parseAdditive());
		}
		if (next(Id.CONTAINS)) {
			return new ExpressionCall(Operator.CONTAINS, expression, this.parseAdditive());
		}
		if (next(Id.GREATER_THAN)) {
			return new ExpressionCall(Operator.GREATER_THAN, expression, this.parseAdditive());
		}
		if (next(Id.GREATER_THAN_OR_EQUAL)) {
			return new ExpressionCall(Operator.GREATER_THAN_OR_EQUAL, expression, this.parseAdditive());
		}
		if (next(Id.LESS_THAN)) {
			return new ExpressionCall(Operator.LESS_THAN, expression, this.parseAdditive());
		}
		if (next(Id.LESS_THAN_OR_EQUAL)) {
			return new ExpressionCall(Operator.LESS_THAN_OR_EQUAL, expression, this.parseAdditive());
		}

		// Special case NOT after operator: <exp> IS [NOT] <primaryExp>
		if (next(Id.IS)) {
			// System.out.println("Parse IS");
			if (next(Id.NOT)) {
				not = true;
			}
			expression = new ExpressionCall(Operator.IS, expression, this.parseBasic());
		} else if (next(Id.NOT)) {
			not = true;
		}

		// Special case NOT before operation: <exp> [NOT] LIKE <primaryExp>
		if (next(Id.LIKE)) {
			// System.out.println("Parse LIKE");
			Expression pattern = this.parseTerm();

			if (next(Id.ESCAPE)) {
				Expression escape = this.parseTerm();
				expression = new ExpressionCall(Operator.LIKE, expression, pattern, escape);
			} else
				expression = new ExpressionCall(Operator.LIKE, expression, pattern);
		} else if (next(Id.IN)) {
			// System.out.println("Parse IN");
			expression = new ExpressionCall(Operator.IN, expression, this.parseList());
		}

		else if (next(Id.BETWEEN)) {
			// System.out.println("Parse BETWEEN");

			Expression begin = this.parseTerm();
			if (!next(Id.AND)) {
				throw new ExpressionParserException("ExpressionException.InvalidBetween", source, this.getPosition());
			}
			Expression end = this.parseTerm();

			expression = new ExpressionCall(Operator.BETWEEN, expression, begin, end);
		}

		if (not) {
			return new ExpressionCall(Operator.NOT, expression);
		}

		return expression;
	}

	/**
	 * MultiplicativeExpression ( (+ | - | ||) MultiplicativeExpression )*
	 */
	private Expression parseAdditive() throws ExpressionException {
		Expression expression = this.parseMultiplicative();
		while (hasNext()) {
			if (next(Id.PLUS)) {
				// System.out.println("Parse simple +");
				expression = new ExpressionCall(Operator.ADD, expression, this.parseMultiplicative());
			} else if (next(Id.MINUS)) {
				// System.out.println("Parse simple -");
				expression = new ExpressionCall(Operator.SUBTRACT, expression, this.parseMultiplicative());
			} else if (next(Id.CONCAT)) {
				// System.out.println("Parse simple concat");
				expression = new ExpressionCall(Operator.CONCAT, expression, this.parseMultiplicative());
			} else
				break;
		}

		return expression;
	}

	private Value parseLiteralInteger(Token token, int radix) throws ExpressionParserException {
		return Value.of(Long.parseLong(token.getText(), radix));
	}

	private Value parseLiteralNumber(Token token) throws ExpressionParserException {
		return Value.of(Double.parseDouble(token.getText()));
	}

	/**
	 * Parses a date literal. The parsing is strict and requires months to be less
	 * than 12, days to be less than 31, etc.
	 */
	private Value parseLiteralDate() throws ExpressionParserException {

		// The real literal text DATE 'literal'
		Token token = next();

		DateFormat dateFormat = Format.PER_THREAD.get().date;

		ParsePosition pp = new ParsePosition(0);
		final Date date = dateFormat.parse(token.getText(), pp);
		if (null == date) {

		}
		LocalDateTime dt = LocalDateTime.ofInstant(date.toInstant(), ZoneOffset.systemDefault());

		return Value.of(dt);
	}

	/**
	 * Parses a date literal. The parsing is strict and requires months to be less
	 * than 12, days to be less than 31, etc.
	 */
	private Value parseLiteralTimestamp() throws ExpressionParserException {

		// The real literal text TIMESTAMP 'literal'
		Token token = next();

		DateFormat dateFormat = Format.PER_THREAD.get().timestamp;

		ParsePosition pp = new ParsePosition(0);
		final Date date = dateFormat.parse(token.getText(), pp);
		if (null == date) {

		}
		LocalDateTime dt = LocalDateTime.ofInstant(date.toInstant(), ZoneOffset.systemDefault());

		return Value.of(dt);
	}

	/**
	 * Parses a list of expressions separated by commas.
	 */
	private ExpressionList parseList() throws ExpressionException {

		// Token token = tokenizer.next();

		List<Expression> list = new ArrayList<>();

		// System.out.println("Parse expression list: " + token);
		if (next(Id.LPARENTHESIS)) {

			Token token;
			do {
				list.add(parseTerm());

				token = next();
				// System.out.println("Parse expression list: " + token);
				if (token == null)
					throw new ExpressionParserException("ExpressionException.MissingRightParenthesis", source,
							this.getPosition());
				if (token.is(Id.COMMA))
					continue;

				if (token.is(Id.RPARENTHESIS))
					return new ExpressionList(list);

			} while (token != null);
		}

		throw new ExpressionParserException("ExpressionException.InvalidExpressionList", source, this.getPosition());
	}

	/** Case When Then Else End ) */
	private Expression parseCase() throws ExpressionException {

		if (next(Id.CASE)) {

			Expression valueExpression = null;
			Expression elseExpression = null;
			List<Expression> whenList = new ArrayList<>();
			List<Expression> thenList = new ArrayList<>();

			// Form Switch value
			if (!is(Id.WHEN)) {
				valueExpression = this.parseLogicalOr();
			}

			// Form mutli boolean condition
			while (next(Id.WHEN)) {
				whenList.add(this.parseLogicalOr());
				if (!next(Id.THEN)) {
					throw new ExpressionParserException("ExpressionException.InvalidCaseWhen", source,
							this.getPosition());
				}
				thenList.add(this.parseLogicalOr());
			}

			if (!next(Id.ELSE)) {
				throw new ExpressionParserException("ExpressionException.InvalidCaseWhen", source, this.getPosition());
			}
			elseExpression = this.parseLogicalOr();

			if (!next(Id.END)) {
				throw new ExpressionParserException("ExpressionException.InvalidCaseWhen", source, this.getPosition());
			}

			return new ExpressionCall(Operator.CASE, valueExpression, new ExpressionList(whenList),
					new ExpressionList(thenList), elseExpression);
		}

		return this.parseRelational();
	}

	/** Cast(expression AS dataType) */
	private Expression parseCast() throws ExpressionException {
		if (!next(Id.LPARENTHESIS)) {
			throw new ExpressionParserException("ExpressionException.MissingLeftParenthesis", source,
					this.getPosition());
		}

		Expression expression = this.parseLogicalOr();

		if (!next(Id.AS)) {
			throw new ExpressionParserException("ExpressionException.MissingCastAs", source, this.getPosition());
		}

		Token token = next();
		DataType dataType = DataType.valueOf(token.getText());

		if (!next(Id.RPARENTHESIS)) {
			throw new ExpressionParserException("ExpressionException.MissingRightParenthesis", source,
					this.getPosition());
		}

		// Use Enum.ordinal as argument for performance
		return new ExpressionCall(Operator.CAST, expression, Value.of(dataType.ordinal()));
	}

	/**
	 * Pre-initialized {@link DateFormat} objects, to be used within the current
	 * thread, because {@code DateFormat} is not thread-safe.
	 */
	private static class Format {
		private static final ThreadLocal<Format> PER_THREAD = ThreadLocal.withInitial(Format::new);
		final DateFormat timestamp = new SimpleDateFormat(TIMESTAMP_FORMAT_STRING, Locale.ROOT);
		final DateFormat time = new SimpleDateFormat(TIME_FORMAT_STRING, Locale.ROOT);
		final DateFormat date = new SimpleDateFormat(DATE_FORMAT_STRING, Locale.ROOT);
	}
}
