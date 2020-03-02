package org.apache.hop.expression;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;
import org.apache.hop.expression.Token.Id;
import org.apache.hop.expression.util.Characters;
import org.apache.hop.expression.util.ToDate;
import org.apache.hop.i18n.BaseMessages;

public class ExpressionParser {

	private static final Class<?> PKG = Expression.class; // for i18n purposes

	/** locale-neutral big decimal format. */
	public static final DecimalFormat DECIMAL_FORMAT = new DecimalFormat("0.0b",
			new DecimalFormatSymbols(Locale.ENGLISH));

	/** The DateTimeFormatter for timestamps, "yyyy-MM-dd HH:mm:ss". */
	public static final DateTimeFormatter TIMESTAMP_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

	/** The DateTimeFormatter for timestamps, "yyyy-MM-dd HH:mm:ss.nnnnnn". */
	public static final DateTimeFormatter TIMESTAMP_FORMAT_NANO6 = DateTimeFormatter
			.ofPattern("yyyy-MM-dd HH:mm:ss.nnnnnn");

//	/** The DateTimeFormatter for time, "HH:mm:ss.nnnnnn". */
//	public static final DateTimeFormatter TIME_FORMAT_NANO3 = DateTimeFormatter.ofPattern("HH:mm:ss.nnn");
//	public static final DateTimeFormatter TIME_FORMAT_NANO6 = DateTimeFormatter.ofPattern("HH:mm:ss.nnnnnn");
//	public static final DateTimeFormatter TIME_FORMAT_NANO7 = DateTimeFormatter.ofPattern("HH:mm:ss.nnnnnnn");

	/** The UTC time zone. */
	public static final ZoneId UTC_ZONE = ZoneId.of("UTC");

	private final String source;

	private List<Token> tokens = new ArrayList<>();
	private int index = 0;

	public static Expression parse(String source) throws ExpressionException {
		ExpressionParser parser = new ExpressionParser(source);
		Expression expression = parser.parse();

		return expression.optimize(new ExpressionContext());
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

	private Expression parse() throws ExpressionParserException {
		// System.out.println("Parse: " + source);

		if (StringUtils.isEmpty(source))
			return Value.NULL;

		// Tokenize
		Scanner scanner = new Scanner(source);
		for (Token token = scanner.tokenize(); token != null; token = scanner.tokenize()) {

			// Ignore comment
			if (token.is(Id.COMMENT))
				continue;

			// System.out.println("Token " + token);

			tokens.add(token);
		}

		Expression expression = this.parseLogicalOr();

		if (hasNext()) {
			throw new ExpressionParserException(next().getText(), source, this.getPosition());
		}

		return expression;
	}

	/**
	 * Parse logical OR expression
	 * 
	 * LogicalXor ( OR LogicalXor )*
	 */
	private Expression parseLogicalOr() throws ExpressionParserException {
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
	private Expression parseLogicalXor() throws ExpressionParserException {
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
	private Expression parseLogicalAnd() throws ExpressionParserException {
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
	private Expression parseLogicalNot() throws ExpressionParserException {

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
	private Expression parseIs() throws ExpressionParserException {
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
	private Expression parseFactor() throws ExpressionParserException {
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
	private Expression parseBitwiseNot() throws ExpressionParserException {
		if (next(Id.BITWISE_NOT)) {
			return new ExpressionCall(Operator.BITNOT, this.parseUnary());
		}
		return this.parseUnary();
	}

	/** UnaryExpression ( & UnaryExpression)* */
	private Expression parseBitwiseAnd() throws ExpressionParserException {
		Expression expression = this.parseFactor();
		if (next(Id.BITWISE_AND)) {
			return new ExpressionCall(Operator.BITAND, expression, this.parseFactor());
		}
		return expression;
	}

	/** UnaryExpression ( | UnaryExpression)* */
	private Expression parseBitwiseOr() throws ExpressionParserException {
		Expression expression = this.parseBitwiseXor();
		if (next(Id.BITWISE_OR)) {
			return new ExpressionCall(Operator.BITOR, expression, this.parseBitwiseXor());
		}
		return expression;
	}

	/** UnaryExpression ( ^ UnaryExpression)* */
	private Expression parseBitwiseXor() throws ExpressionParserException {
		Expression expression = this.parseBitwiseAnd();
		if (next(Id.BITWISE_XOR)) {
			return new ExpressionCall(Operator.BITXOR, expression, this.parseBitwiseAnd());
		}
		return expression;
	}

	/** (('+' | '-') PrimaryExpression ) */
	private Expression parseUnary() throws ExpressionException {

		if (next(Id.MINUS)) {
			return new ExpressionCall(Operator.NEGATIVE, this.parseTerm());
		}
		if (next(Id.PLUS)) {
			// Ignore
		}

		return this.parseTerm();
	}

	/** Literal TRUE | FALSE | NULL */
	private Expression parseLiteralBasic() throws ExpressionParserException {

		Token token = next();

		if (token == null)
			throw new ExpressionParserException(BaseMessages.getString(PKG, "ExpressionParser.UnexpectedEndOfExpression"), source,
					this.getPosition());

		switch (token.getId()) {
		case TRUE:
			return Value.of(true);
		case FALSE:
			return Value.of(false);
		case NULL:
			return Value.NULL;
		default:
			throw new ExpressionParserException(" ERROR2 ", source, token.getStart());
		}
	}

	/** Literal text */
	private Expression parseLiteralText(Token token) throws ExpressionParserException {
		String text = token.getText();
//		int length = text.length();
//		StringBuilder builder = new StringBuilder(length);
//		for (int i = 0; i < length; i++) {
//			char c = text.charAt(i);
//
//			// Convert backslash escape sequences
//			if (c == '\\') {
//				if (i + 1 >= text.length()) {					
//					throw new ExpressionException("Invalid escape sequence at position {1}", text, i);
//				}
//				c = text.charAt(++i);
//				switch (c) {
//				// Tab
//				case 't':
//					builder.append('\t');
//					continue;
//				// Carriage return
//				case 'r':
//					builder.append('\r');
//					continue;
//				// Newline
//				case 'n':
//					builder.append('\n');
//					continue;
//				// Backspace
//				case 'b':
//					builder.append('\b');
//					continue;
//				// Form feed
//				case 'f':
//					builder.append('\f');
//					continue;
//				// Single quote
//				case '\'':
//					builder.append('\'');
//					continue;
////				case '"':
////					builder.append('"');
////					continue;
//				case '\\':
//					builder.append('\\');
//					continue;
//				// u####' 16-bit Unicode character where #### are four hex digits
//				case 'u': {
//					try {
//						c = (char) (Integer.parseInt(text.substring(i + 1, i + 5), 16));
//					} catch (NumberFormatException e) {
//						throw new ExpressionException("Invalid escape sequence \\u#### at position {1}", text, i);
//					}
//					i += 4;
//					break;
//				}
//				// U########' 32-bit Unicode character where ######## are four are eight hex
//				// digits
//				case 'U': {
//					try {
//						c = (char) (Integer.parseInt(text.substring(i + 1, i + 9), 16));
//					} catch (NumberFormatException e) {
//						throw new ExpressionException("Invalid escape sequence \\U######## at position {1}", text, i);
//					}
//					i += 8;
//					break;
//				}
//
//				default:
//					throw createFormatException(text, i);
//				}
//			}
//			builder.append(c);
//		}
//		return Value.of(builder.toString());

		return Value.of(text);
	}

	/** Term = Literal | Identifier | Function | '(' Expression ')' */
	private Expression parseTerm() throws ExpressionParserException {
		Token token = next();

		if (token != null) {
			switch (token.getId()) {
			case TRUE:
				return Value.TRUE;
			case FALSE:
				return Value.FALSE;
			case NULL:
				return Value.NULL;
			case IDENTIFIER:
				return new ExpressionIdentifier(token.getText());
			case LITERAL_STRING:
				return parseLiteralText(token);
			case LITERAL_NUMBER:
				return parseLiteralNumber(token);
			case LITERAL_BINARY_HEX:
				return parseLiteralBinaryHexa(token);
			case LITERAL_BINARY_BIT:
				return parseLiteralBinaryBit(token);
			case DATE:
				return parseLiteralDate();
			case TIME:
				return parseLiteralTime();
			case TIMESTAMP:
				return parseLiteralTimestamp();
			case CASE:
				// FIXME: Case is not at the good place
				return parseCaseWhen();
			case FUNCTION:
				return parseFunction(token);
			case LPARENTHESIS:
				Expression expression = this.parseLogicalOr();

				token = next();
				if (token.is(Id.RPARENTHESIS)) {
					return expression;
				}
				throw new ExpressionParserException(BaseMessages.getString(PKG, "ExpressionParser.UnbalancedParenthesis"), source,
						this.getPosition());
			default:
				// Error
			}
		}
		throw new ExpressionParserException(BaseMessages.getString(PKG, "ExpressionParser.UnexpectedEndOfExpression"), source, this.getPosition());
	}

	/**
	 * AdditiveExpression ( Operator AdditiveExpression | InClause | BetweenClause |
	 * LikeClause
	 */
	private Expression parseRelational() throws ExpressionParserException {
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
//		if (next(Id.CONTAINS)) {
//			return new ExpressionCall(Operator.CONTAINS, expression, this.parseAdditive());
//		}
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

		// Special case NOT before operation: <exp> [NOT] LIKE|IN|BETWEEN <primaryExp>
		boolean not = false;
		if (next(Id.NOT)) {
			not = true;
		}

		if (next(Id.LIKE)) {
			Expression pattern = this.parseTerm();

			if (next(Id.ESCAPE)) {
				Expression escape = this.parseTerm();
				expression = new ExpressionCall(Operator.LIKE, expression, pattern, escape);
			} else
				expression = new ExpressionCall(Operator.LIKE, expression, pattern);
		} else if (next(Id.ILIKE)) {
			Expression pattern = this.parseTerm();

			if (next(Id.ESCAPE)) {
				Expression escape = this.parseTerm();
				expression = new ExpressionCall(Operator.ILIKE, expression, pattern, escape);
			} else
				expression = new ExpressionCall(Operator.ILIKE, expression, pattern);
		} else if (next(Id.IN)) {
			expression = new ExpressionCall(Operator.IN, expression, this.parseList());
		}

		else if (next(Id.BETWEEN)) {
			Expression begin = this.parseTerm();
			if (!next(Id.AND)) {
				throw new ExpressionParserException(BaseMessages.getString(PKG, "ExpressionParser.InvalidOperator",Id.BETWEEN), source,
						this.getPosition());
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
	private Expression parseAdditive() throws ExpressionParserException {
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

	private Value parseLiteralNumber(Token token) throws ExpressionParserException {
		boolean isDecimalPartFound = false;
		boolean isDecimalSeparatorFound = false;
		boolean isExponentSymbolFound = false;

		String text = token.getText();
		
		try {
			
			int length = text.length();
			int pos = 0;

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
			if (pos < length && (source.charAt(pos) == '+' || text.charAt(pos) == '-') && isExponentSymbolFound) {
				pos++;
			}
			while (pos < length && Character.isDigit(source.charAt(pos)) && isExponentSymbolFound) {
				pos++;
			}

			if (length < 18 && isDecimalSeparatorFound == false) {
				return Value.of(Long.parseLong(text, 10));
			}

			return Value.of(new BigDecimal(text));
		} catch (Exception e) {
			throw new ExpressionParserException(BaseMessages.getString(PKG,"Expression.InvalidNumeric",text), source, this.getPosition());
		}
	}

	private Value parseLiteralBinaryHexa(Token token) throws ExpressionParserException {

		String s = token.getText();

		if (s.length() % 2 > 0)
			s = '0' + s;

		byte[] bytes = new byte[s.length() / 2];

		for (int i = 0; i < bytes.length; i++) {
			int index = i * 2;
			bytes[i] = (byte) Integer.parseInt(s.substring(index, index + 2), 16);
		}

		if (bytes.length <= 8) {
			// Value as integer if less than or equals 8 bytes
			long result = 0;
			for (int i = 0; i < bytes.length; i++) {
				result = result << 8;
				result = result | (bytes[i] & 0xFF);
			}
			return Value.of(result);
		}

		return Value.of(bytes);
	}

	private Value parseLiteralBinaryBit(Token token) throws ExpressionParserException {

		String s = token.getText();
		BitSet bitset = new BitSet(s.length());

		int length = s.length();
		for (int i = length - 1; i >= 0; i--) {
			if (s.charAt(i) == '1') {
				bitset.set(length - i - 1);
			}
		}

		if (bitset.length() <= 32) {
			// Value as integer if less than or equals 32 bits
			return Value.of(bitset.toLongArray()[0]);
		}

		return Value.of(bitset.toByteArray());
	}

	/**
	 * Parses a date literal. The parsing is strict and requires months to be less
	 * than 12, days to be less than 31, etc.
	 */
	private Value parseLiteralDate() throws ExpressionParserException {

		// The real literal text DATE 'literal'
		Token token = next();

		// LocalDate date = LocalDate.parse(token.getText());
		// return Value.of(date.atStartOfDay(UTC_ZONE).toInstant());

		Instant instant = ToDate.parse(token.getText(), "YYYY-MM-DD");

		return Value.of(instant);
	}

	/**
	 * Parses a time literal.
	 */
	private Value parseLiteralTime() throws ExpressionParserException {

		try {
			// The real literal text TIME 'literal'
			Token token = next();

			DateTimeFormatter format = DateTimeFormatter.ISO_TIME;
//		if ( token.getLength()==16 )
//			format = TIME_FORMAT_NANO6;		
//		else if ( token.getLength()==17 )
//			format = TIME_FORMAT_NANO7;				
//		
			LocalTime time = LocalTime.parse(token.getText(), format);

			// System.out.println(token.getText() + " parse to " + time.toString());

			LocalDateTime datetime = LocalDateTime.of(LocalDate.of(1900, 1, 1), time);
			return Value.of(datetime.toInstant(ZoneOffset.UTC));
		} catch (Exception e) {
			throw new ExpressionParserException(BaseMessages.getString(PKG,"Expression.InvalidTime"), source, this.getPosition());
		}
	}

	/**
	 * Parses a date literal. The parsing is strict and requires months to be less
	 * than 12, days to be less than 31, etc.
	 */
	private Value parseLiteralTimestamp() throws ExpressionParserException {

		try {
		// The real literal text TIMESTAMP 'literal'
		Token token = next();

		DateTimeFormatter format = TIMESTAMP_FORMAT;
		if (token.getLength() == 26)
			format = TIMESTAMP_FORMAT_NANO6;

		LocalDateTime datetime = LocalDateTime.parse(token.getText(), format);

		// System.out.println(token.getText()+" parse to "+datetime.toString() );

		return Value.of(datetime.toInstant(ZoneOffset.UTC));
	} catch (Exception e) {
		throw new ExpressionParserException(BaseMessages.getString(PKG,"Expression.InvalidTimestamp"), source, this.getPosition());
	}
	}

	/**
	 * Parses a list of expressions separated by commas.
	 */
	private ExpressionList parseList() throws ExpressionParserException {

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
					throw new ExpressionParserException(BaseMessages.getString(PKG,"ExpressionParser.MissingRightParenthesis"), source,
							this.getPosition());
				if (token.is(Id.COMMA))
					continue;

				if (token.is(Id.RPARENTHESIS))
					return new ExpressionList(list);

			} while (token != null);
		}

		throw new ExpressionParserException(BaseMessages.getString(PKG,"ExpressionParser.InvalidExpressionList"), source, this.getPosition());
	}

	/** Case When Then Else End ) */
	private Expression parseCaseWhen() throws ExpressionException {
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
				throw new ExpressionParserException(BaseMessages.getString(PKG,"ExpressionParser.InvalidOperator",Id.CASE), source,
						this.getPosition());
			}
			thenList.add(this.parseLogicalOr());
		}

		if (!next(Id.ELSE)) {
			throw new ExpressionParserException(BaseMessages.getString(PKG,"ExpressionParser.InvalidOperator",Id.CASE), source, this.getPosition());
		}
		elseExpression = this.parseLogicalOr();

		if (!next(Id.END)) {
			throw new ExpressionParserException(BaseMessages.getString(PKG,"ExpressionParser.InvalidOperator",Id.CASE), source, this.getPosition());
		}

		return new ExpressionCall(Operator.CASE, valueExpression, new ExpressionList(whenList),
				new ExpressionList(thenList), elseExpression);
	}

	/** Function */
	private Expression parseFunction(Token token) throws ExpressionParserException {

		Function function = Function.getFunction(token.getText());
		List<Expression> operands = new ArrayList<>();

		if (is(Id.LPARENTHESIS))
			token = next();
		else {
			throw new ExpressionParserException(BaseMessages.getString(PKG,"ExpressionParser.MissingLeftParenthesis"), source, this.getPosition());
		}

		/** Cast(expression AS dataType) */
		if (Kind.CAST == function.kind) {

			operands.add(this.parseLogicalOr());

			if (!next(Id.AS)) {
				throw new ExpressionParserException(BaseMessages.getString(PKG,"ExpressionParser.InvalidFunctionCastAs"), source,
						this.getPosition());
			}

			DataType type = parseDataType(next());
			
			// Use Enum.ordinal as argument for evaluate performance
			operands.add(Value.of(type.ordinal()));
			
			
			if (is(Id.FORMAT)) {
				next();
				operands.add(this.parseLiteralText(next()));
			}
		}

		/** Extract(datePart FROM expression) */
		else if (Kind.EXTRACT == function.kind) {

			Token tokenPart = next();
			DatePart part = DatePart.of(tokenPart.getText());

			// Replace EXTRACT with the corresponding function
			switch (part) {
			case YEAR:
			case MONTH:
			case QUARTER:
			case DAY:
			case HOUR:
			case MINUTE:
			case SECOND:
			case WEEK:
			case DAYOFYEAR:
			case DAYOFWEEK:
				function = Function.getFunction(part.name());
				break;
			default:
				function = Function.getFunction(Kind.DATE_PART);
				// Use Enum.ordinal as argument for evaluate performance
				operands.add(Value.of(part.ordinal()));
				break;
			}

			if (!next(Id.FROM)) {
				throw new ExpressionParserException(BaseMessages.getString(PKG,"ExpressionParser.InvalidFunctionExtractFrom"), source,
						this.getPosition());
			}

			operands.add(this.parseLogicalOr());

		} else {

			// No param function
			if (is(Id.RPARENTHESIS)) {
				next();
				
				if ( !function.checkNumberOfArguments(operands.size()) ) {
					throw new ExpressionParserException(BaseMessages.getString(PKG,"ExpressionParser.InvalidNumberOfArguments", function.getName()), source, token.getStart());
				}	
				
				return new ExpressionCall(function, operands);
			}

			operands.add(this.parseLogicalOr());

			while (is(Id.COMMA)) {
				token = next();
				operands.add(this.parseLogicalOr());
			}
		}

		if (is(Id.RPARENTHESIS)) {
			token = next();
		} else {
			throw new ExpressionParserException(BaseMessages.getString(PKG,"ExpressionParser.MissingRightParenthesis"), source, token.getStart());
		}
		
		
		if ( !function.checkNumberOfArguments(operands.size()) ) {
			throw new ExpressionParserException(BaseMessages.getString(PKG,"ExpressionParser.InvalidNumberOfArguments", function.getName()), source, token.getStart());
		}	
		
		return new ExpressionCall(function, operands);
	}

	private DataType parseDataType(Token token) {
		try {
			return DataType.valueOf(token.getText());
		} catch (Exception e) {
			throw new ExpressionParserException(BaseMessages.getString(PKG,"ExpressionParser.InvalidDataType",token.getText()), source, token.getStart());
		}
	}
}
