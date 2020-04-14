package org.apache.hop.expression;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import org.apache.commons.lang.StringUtils;
import org.apache.hop.expression.ExpressionToken.Id;
import org.apache.hop.expression.util.Characters;

public class ExpressionParser {

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

	private List<ExpressionToken> tokens = new ArrayList<>();
	private int index = 0;

	public static Expression parse(String source) throws ExpressionException {
		ExpressionParser parser = new ExpressionParser(source);

		return parser.parse();
	}

	protected ExpressionParser(String source) {
		super();
		this.source = source;
	}

	protected int getPosition() {

		if (index > 0 && index < tokens.size())
			return this.next().getStart();

		return source.length();
	}

	protected boolean hasNext() {
		return index < tokens.size();
	}

	protected ExpressionToken next() {
		if (!hasNext())
			return null;
		ExpressionToken token = tokens.get(index);
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
		ExpressionScanner scanner = new ExpressionScanner(source);
		for (ExpressionToken token = scanner.tokenize(); token != null; token = scanner.tokenize()) {

			// Ignore comment
			if (token.is(Id.COMMENT))
				continue;

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
	 * LogicalXor ( OR LogicalXor )*
	 */
	private Expression parseLogicalOr() throws ExpressionException {
		Expression expression = this.parseLogicalXor();
		while (next(Id.OR)) {
			expression = new ExpressionCall(Operator.LOGICAL_OR, expression, parseLogicalXor());
		}

		return expression;
	}

	/**
	 * Parse logical XOR expression
	 * 
	 * LogicalAnd ( XOR LogicalAnd )*
	 * 
	 * @return Expression
	 */
	private Expression parseLogicalXor() throws ExpressionException {
		Expression expression = this.parseLogicalAnd();
		while (next(Id.XOR)) {
			expression = new ExpressionCall(Operator.LOGICAL_XOR, expression, parseLogicalAnd());
		}

		return expression;
	}

	/**
	 * Parse logical AND expression
	 * 
	 * LogicalNot ( AND LogicalNot )*
	 * 
	 * @return Expression
	 */
	private Expression parseLogicalAnd() throws ExpressionException {
		Expression expression = this.parseLogicalNot();
		while (next(Id.AND)) {
			expression = new ExpressionCall(Operator.LOGICAL_AND, expression, parseLogicalNot());
		}

		return expression;
	}

	/**
	 * Parse logical NOT expression
	 * 
	 * [NOT] RelationalExpression
	 */
	private Expression parseLogicalNot() throws ExpressionException {

		if (next(Id.NOT)) {
			return new ExpressionCall(Operator.LOGICAL_NOT, this.parseIs());
		}

		return this.parseIs();
	}

	/**
	 * Parse IS expression
	 * 
	 * Basic [NOT] BasicExpression
	 */
	private Expression parseIs() throws ExpressionException {
		Expression expression = this.parseRelational();
		if (next(Id.IS)) {
			boolean not = false;
			if (next(Id.NOT)) {
				not = true;
			}
			Expression result = new ExpressionCall(Operator.IS, expression, this.parseLiteralBasic());
			if (not)
				return new ExpressionCall(Operator.LOGICAL_NOT, result);
			return result;
		}
		return expression;
	}

	/**
	 * ExponentExpression ( (* | / | %) ExponentExpression())*
	 * 
	 * @return Expression
	 */
	private Expression parseMultiplicative() throws ExpressionException {
		Expression expression = this.parseBitwiseNot();

		while (hasNext()) {
			if (next(Id.MULTIPLY)) {
				expression = new ExpressionCall(Operator.MULTIPLY, expression, this.parseBitwiseNot());
			} else if (next(Id.DIVIDE)) {
				expression = new ExpressionCall(Operator.DIVIDE, expression, this.parseBitwiseNot());
			} else if (next(Id.MODULUS)) {
				expression = new ExpressionCall(Operator.MODULUS, expression, this.parseBitwiseNot());
			} else
				break;
		}

		return expression;
	}

	/** ( UnaryExpression)* */
	private Expression parseBitwiseNot() throws ExpressionException {
		if (next(Id.BITWISE_NOT)) {
			return new ExpressionCall(Operator.BITWISE_NOT, this.parseUnary());
		}
		return this.parseUnary();
	}

	/** UnaryExpression ( & UnaryExpression)* */
	private Expression parseBitwiseAnd() throws ExpressionException {
		Expression expression = this.parseMultiplicative();
		if (next(Id.BITWISE_AND)) {
			return new ExpressionCall(Operator.BITWISE_AND, expression, this.parseMultiplicative());
		}
		return expression;
	}

	/** UnaryExpression ( | UnaryExpression)* */
	private Expression parseBitwiseOr() throws ExpressionException {
		Expression expression = this.parseBitwiseXor();
		if (next(Id.BITWISE_OR)) {
			return new ExpressionCall(Operator.BITWISE_OR, expression, this.parseBitwiseXor());
		}
		return expression;
	}

	/** UnaryExpression ( ^ UnaryExpression)* */
	private Expression parseBitwiseXor() throws ExpressionException {
		Expression expression = this.parseBitwiseAnd();
		if (next(Id.BITWISE_XOR)) {
			return new ExpressionCall(Operator.BITWISE_XOR, expression, this.parseBitwiseAnd());
		}
		return expression;
	}

	/** (('+' | '-') PrimaryExpression ) */
	private Expression parseUnary() throws ExpressionException {

		if (next(Id.MINUS)) {
			return new ExpressionCall(Operator.NEGATE, this.parseTerm());
		}
		if (next(Id.PLUS)) {
			// Ignore
		}

		return this.parseTerm();
	}

	/** Literal TRUE | FALSE | NULL */
	private Expression parseLiteralBasic() throws ExpressionParserException {

		ExpressionToken token = next();

		if (token == null)
			throw new ExpressionParserException("ExpressionException.SyntaxError", source, this.getPosition());

		switch (token.getId()) {
		case TRUE:
			return Value.of(true);
		case FALSE:
			return Value.of(false);
		case NULL:
			return Value.NULL;
		default:
			throw new ExpressionParserException("ExpressionException.SyntaxError", source, token.getStart());
		}
	}

	/** Literal text */
	private Expression parseLiteralText(ExpressionToken token) throws ExpressionParserException {
		String text = token.getText();
		int length = text.length();
		StringBuilder builder = new StringBuilder(length);
		for (int i = 0; i < length; i++) {
			char c = text.charAt(i);

			// Convert backslash escape sequences
			if (c == '\\') {
				if (i + 1 >= text.length()) {
					throw createFormatException(text, i);
				}
				c = text.charAt(++i);
				switch (c) {
				// Tab
				case 't':
					builder.append('\t');
					continue;
				// Carriage return
				case 'r':
					builder.append('\r');
					continue;
				// Newline
				case 'n':
					builder.append('\n');
					continue;
				// Backspace
				case 'b':
					builder.append('\b');
					continue;
				// Form feed
				case 'f':
					builder.append('\f');
					continue;
				// Single quote
				case '\'':
					builder.append('\'');
					continue;
				case '"':
					builder.append('"');
					continue;
				case '\\':
					builder.append('\\');
					continue;
				// u####' 16-bit Unicode character where #### are four hex digits
				case 'u': {
					try {
						c = (char) (Integer.parseInt(text.substring(i + 1, i + 5), 16));
					} catch (NumberFormatException e) {
						throw new ExpressionException("Invalid escape sequence \\u#### at position {1}", text, i);
					}
					i += 4;
					break;
				}
				// U########' 32-bit Unicode character where ######## are four are eight hex digits
				case 'U': {
					try {
						c = (char) (Integer.parseInt(text.substring(i + 1, i + 9), 16));
					} catch (NumberFormatException e) {
						throw new ExpressionException("Invalid escape sequence \\U######## at position {1}", text, i);
					}
					i += 8;
					break;
				}

				default:
					throw createFormatException(text, i);
				}
			}
			builder.append(c);
		}

		return Value.of(builder.toString());
	}

	/** Term = Literal | Identifier | Function | '(' Expression ')' */
	private Expression parseTerm() throws ExpressionException {
		ExpressionToken token = next();

		if (token != null) {
			switch (token.getId()) {
			case TRUE:
				return Value.TRUE;
			case FALSE:
				return Value.FALSE;
			case NULL:
				return Value.NULL;
			case LITERAL_TEXT:
				return parseLiteralText(token);
			case LITERAL_NUMBER:
				return parseLiteralNumber(token);				
			case LITERAL_HEXNUMBER:
				return parseLiteralNumber(token, 16);
			case LITERAL_BITNUMBER:
				return parseLiteralNumber(token, 2);
			case DATE:
				return parseLiteralDate();
			case TIMESTAMP:
				return parseLiteralTimestamp();
			case CAST:
				return parseCast();
			case CASE:
				return parseCase();
			case IDENTIFIER:
				return new ExpressionIdentifier(token.getText());
			case FUNCTION:
				Function function = Operator.getFunction(token.getText());
				List<Expression> params = new ArrayList<>();

				token = next();
				if (token.is(Id.LPARENTHESIS)) {

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
							token.getStart());
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
				// Error
			}
		}
		throw new ExpressionParserException("ExpressionException.SyntaxError", source, this.getPosition());
	}

	/**
	 * AdditiveExpression ( Operator AdditiveExpression | InClause | BetweenClause |
	 * LikeClause
	 */
	private Expression parseRelational() throws ExpressionException {
		Expression expression = this.parseAdditive();

		if (next(Id.EQUAL)) {
			return new ExpressionCall(Operator.EQUALS, expression, this.parseAdditive());
		}
		if (next(Id.NOT_EQUAL)) {
			return new ExpressionCall(Operator.NOT_EQUALS, expression, this.parseAdditive());
		}
		if (next(Id.LESS_THAN_OR_GREATER_THAN)) {
			return new ExpressionCall(Operator.LESS_THAN_OR_GREATER_THAN, expression, this.parseAdditive());
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

		// Special case NOT before operation: <exp> [NOT] LIKE <primaryExp>
		boolean not = false;
		if (next(Id.NOT)) {
			not = true;
		}

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
			return new ExpressionCall(Operator.LOGICAL_NOT, expression);
		}

		return expression;
	}

	/**
	 * BitwiseExpression ( (+ | - | ||) BitwiseOrExpression )*
	 */
	private Expression parseAdditive() throws ExpressionException {
		Expression expression = this.parseBitwiseOr();
		while (hasNext()) {
			if (next(Id.PLUS)) {
				// System.out.println("Parse simple +");
				expression = new ExpressionCall(Operator.ADD, expression, this.parseBitwiseOr());
			} else if (next(Id.MINUS)) {
				// System.out.println("Parse simple -");
				expression = new ExpressionCall(Operator.SUBTRACT, expression, this.parseBitwiseOr());
			} else if (next(Id.CONCAT)) {
				// System.out.println("Parse simple concat");
				expression = new ExpressionCall(Operator.CONCAT, expression, this.parseBitwiseOr());
			} else
				break;
		}

		return expression;
	}

	private Value parseLiteralNumber(ExpressionToken token) throws ExpressionParserException {
		boolean isDecimalPartFound = false;
		boolean isDecimalSeparatorFound = false;
		boolean isExponentSymbolFound = false;

		String text = token.getText();
		int length = text.length();
		int pos = 0;
		char c = text.charAt(pos);

		while (pos < length && Characters.isDigit(text.charAt(pos))) {
			pos++;
		}
		
		// Use dot for decimal separator
		if (pos < length && text.charAt(pos) == '.') {
			isDecimalSeparatorFound = true;
			pos++;
		}
		while (pos < length && Characters.isDigit(text.charAt(pos))) {
			pos++;
			isDecimalPartFound = true;
		}
		if (pos < length && Characters.isExponentChar(text.charAt(pos))) {
			pos++;
			isExponentSymbolFound = true;
		}
		if (pos < length && (source.charAt(pos) == '+' || text.charAt(pos) == '-')
				&& isExponentSymbolFound) {
			pos++;
		}
		while (pos < length && Character.isDigit(source.charAt(pos)) && isExponentSymbolFound) {
			pos++;
		}

		if (length < 18 && isDecimalSeparatorFound==false ) {
			return Value.of(Long.parseLong(text,10));
		}

		return Value.of(new BigDecimal(text));
	}

	private Value parseLiteralNumber(ExpressionToken token, int radix) throws ExpressionParserException {
		return Value.of(Long.parseLong(token.getText(), radix));
	}

	/**
	 * Parses a date literal. The parsing is strict and requires months to be less
	 * than 12, days to be less than 31, etc.
	 */
	private Value parseLiteralDate() throws ExpressionParserException {

		// The real literal text DATE 'literal'
		ExpressionToken token = next();

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
		ExpressionToken token = next();

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

			ExpressionToken token;
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
				throw new ExpressionParserException("ExpressionException.InvalidCaseWhen", source, this.getPosition());
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

		ExpressionToken token = next();
		Type type = Type.valueOf(token.getText());

		if (!next(Id.RPARENTHESIS)) {
			throw new ExpressionParserException("ExpressionException.MissingRightParenthesis", source,
					this.getPosition());
		}

		// Use Enum.ordinal as argument for performance
		return new ExpressionCall(Operator.CAST, expression, Value.of(type.ordinal()));
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

	private static ExpressionException createFormatException(String s, int i) {
		return new ExpressionException("Bad format {0} at position {1}", s, i);
	}
}
