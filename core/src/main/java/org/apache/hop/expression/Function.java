package org.apache.hop.expression;

import java.io.StringWriter;
import java.math.RoundingMode;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.DayOfWeek;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.format.TextStyle;
import java.time.temporal.ChronoField;
import java.util.Locale;

import org.apache.commons.codec.binary.Hex;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.WordUtils;
import org.apache.hop.expression.util.Bytes;
import org.apache.hop.expression.util.DateParser;
import org.apache.hop.expression.util.Soundex;
import org.apache.hop.expression.util.StringEncoder;
import org.apache.hop.expression.util.ToChar;

/**
 * A <code>Function</code> is a type of operator which has conventional
 * function-call syntax.
 */

//TODO: implement MONTHS_BETWEEN
//TODO: implement YEARS_BETWEEN
//TODO: implement TRY_TO_NUMBER
//TODO: implement TRUNCATE
//TODO: implement DATEPART, NANO
//TODO: implement EXTRACT, NANO

public class Function extends Operator {

	/**
	 * The maximum size to which the padding can expand.
	 */
	private static final int PAD_LIMIT = 8192;

	private final boolean isDeterministic;

	protected Function(Kind kind, String name, Category category, boolean isDeterministic) {
		super(kind, name, category, 10, 10);
		this.isDeterministic = isDeterministic;
	}

	/**
	 * Whether the function always returns the same result for the same parameters.
	 *
	 * @return true if it does
	 */
	public boolean isDeterministic() {
		return isDeterministic;
	}

	@Override
	public void unparse(StringWriter writer, ExpressionCall call, int leftPrec, int rightPrec) {
		writer.append(this.getName());
		writer.append('(');
		boolean first = true;
		for (Expression operand : call.getOperands()) {
			if (!first)
				writer.append(',');
			else
				first = false;
			operand.unparse(writer, leftPrec, rightPrec);
		}
		writer.append(')');
	}

	@Override
	public void checkNumberOfArguments(int len) throws ExpressionException {

		int min = 0, max = Integer.MAX_VALUE;
		switch (kind) {
		case CURRENT_DATE:
		case PI:
			max = 0;
			break;
		case ABS:
		case ACOS:
		case ASCII:
		case ASIN:
		case ATAN:

		case CEIL:
		case BIT_LENGTH:
		case OCTET_LENGTH:
		case CBRT:
		case CHR:
		case COS:
		case COSH:
		case COT:
		case DEGREES:
		case EXP:
		case FLOOR:
		case HOUR:
		case INITCAP:
		case LENGTH:
		case LN:
		case LOG10:
		case LOWER:
		case RADIANS:
		case ROUND:
		case SIGN:
		case SIN:
		case SINH:
		case SQRT:
		case SOUNDEX:
		case TAN:
		case TANH:
		case UPPER:
		case REVERSE:
		case UNICODE:
		case SPACE:
		case SECOND:
		case MINUTE:
		case ISO_DAY_OF_WEEK:
		case ISO_WEEK_OF_YEAR:
		case DAY_OF_WEEK:
		case DAY_OF_MONTH:
		case DAY_OF_YEAR:
		case DAY_NAME:
		case MONTH:
		case MONTH_NAME:
		case QUARTER:
		case URLDECODE:
		case URLENCODE:
		case WEEK_OF_YEAR:
		case LAST_DAY:
		case YEAR:
		case MD5:
		case SHA1:
		case SHA256:
		case SHA384:
		case SHA512:
		case STRINGENCODE:
		case STRINGDECODE:
		case TO_BOOLEAN:
			min = 1;
			max = 1;
			break;
		case TRIM:
		case LTRIM:
		case RTRIM:
			min = 1;
			max = 2;
			break;
		case ADD_MONTHS:
		case ATAN2:
		case BITGET:
		case CONTAINS:
		case IFNULL:
		case POWER:
		case MOD:
		case LEFT:
		case REPEAT:
		case RIGHT:
		case NULLIF:
		case EQUAL_NULL:
		case ENDSWITH:
		case STARTSWITH:
			min = 2;
			max = 2;
			break;			
		case INSTR:
		case LPAD:
		case REPLACE:
		case RPAD:
		case SUBSTR:
			min = 2;
			max = 3;
			break;
		case IFF:
		case TRANSLATE:
			min = 3;
			max = 3;
			break;
		case TO_CHAR:
		case TO_DATE:
		case TO_NUMBER:

			min = 1;
			max = 3;
			break;
		case COALESCE:
		case CONCAT:
		case GREATEST:
		case LEAST:
			min = 1;
			max = Integer.MAX_VALUE;
			break;
		case DECODE:
			min = 4;
			max = Integer.MAX_VALUE;
			break;
		case RAND:
			min = 0;
			max = 1;
			break;
		default:
			throw createInternalError(kind.name());
		}
		if (len < min || len > max) {
			throw createInvalidNumberOfArgumentsError();
		}
	}

	@Override
	@SuppressWarnings("incomplete-switch")
	public Value eval(final IExpressionContext context, final IExpression... args) throws ExpressionException {

		switch (kind) {
		case MOD:
		case CONTAINS:
		case POWER: // use operator implementation
			return super.eval(context, args);

		case CURRENT_DATE:
			return Value.of(LocalDateTime.now());
		case ADD_MONTHS: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			Value months = args[1].eval(context);
			if (months.isNull())
				return value;

			LocalDateTime date = value.toDate().plusMonths(months.toInteger());

			return Value.of(date);
		}

		case LAST_DAY: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			LocalDateTime dt = value.toDate().withDayOfMonth(value.toDate().toLocalDate().lengthOfMonth());
			return Value.of(dt);
		}

		case DAY_NAME: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			DayOfWeek weekday = DayOfWeek.of(value.toDate().get(ChronoField.DAY_OF_WEEK));
			return Value.of(weekday.getDisplayName(TextStyle.FULL, Locale.ENGLISH));
		}

		case MONTH_NAME: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			Month month = Month.of(value.toDate().get(ChronoField.MONTH_OF_YEAR));
			return Value.of(month.getDisplayName(TextStyle.FULL, Locale.ENGLISH));
		}

		case MONTH: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			return Value.of(value.toDate().get(ChronoField.MONTH_OF_YEAR));
		}

		case YEAR: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			return Value.of(value.toDate().get(ChronoField.YEAR));
		}

		case QUARTER: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			int month = value.toDate().get(ChronoField.MONTH_OF_YEAR);
			return Value.of(((month - 1) / 3) + 1);
		}

		case ISO_DAY_OF_WEEK: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			return Value.of(value.toDate().get(ChronoField.DAY_OF_WEEK));
		}

		case DAY_OF_WEEK: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			int result = value.toDate().get(ChronoField.DAY_OF_WEEK) + 1;
			if (result == 8)
				result = 1;

			return Value.of(result);
		}

		case DAY_OF_MONTH: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			return Value.of(value.toDate().get(ChronoField.DAY_OF_MONTH));
		}

		case DAY_OF_YEAR: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			return Value.of(value.toDate().get(ChronoField.DAY_OF_YEAR));
		}

		case WEEK_OF_YEAR: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			return Value.of(value.toDate().get(ChronoField.ALIGNED_WEEK_OF_YEAR));
		}

		case HOUR: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			return Value.of(value.toDate().get(ChronoField.HOUR_OF_DAY));
		}

		case MINUTE: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			return Value.of(value.toDate().get(ChronoField.MINUTE_OF_HOUR));
		}

		case SECOND: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			return Value.of(value.toDate().get(ChronoField.SECOND_OF_MINUTE));
		}

		case ABS: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			if (value.isBigNumber()) {
				return Value.of(value.toBigNumber().abs());
			}
			if (value.isNumber()) {
				return Value.of(Math.abs(value.toNumber()));
			}
			return Value.of(Math.abs(value.toInteger()));
		}

		case ACOS: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			Double d = value.toNumber();
			if (d < -1.0 || d > 1.0) {
				throw createArgumentOutOfRangeError(value);
			}
			return Value.of(Math.acos(value.toNumber()));
		}

		case ASIN: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			return Value.of(Math.asin(value.toNumber()));
		}

		case ATAN: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			return Value.of(Math.atan(value.toNumber()));
		}

		case ATAN2: {
			Value x = args[0].eval(context);
			Value y = args[0].eval(context);
			if (x.isNull() || y.isNull())
				return Value.NULL;

			return Value.of(Math.atan2(x.toNumber(), y.toNumber()));
		}

		case BITGET: {
			Value v0 = args[0].eval(context);
			if (v0.isNull())
				return v0;
			Value v1 = args[1].eval(context);
			if (v1.isNull())
				return v1;

			return Value.of((v0.toInteger() & (1L << v1.toInteger())) != 0);
		}

		case CEIL: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			if (value.isInteger())
				return value;
			if (value.isBigNumber()) {
				return Value.of(value.toBigNumber().setScale(0, RoundingMode.CEILING));
			}
			return Value.of(Math.ceil(value.toNumber()));
		}

		case COS: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			return Value.of(Math.cos(value.toNumber()));
		}

		case COSH: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			return Value.of(Math.cosh(value.toNumber()));
		}

		case COT: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			double d = Math.tan(value.toNumber());
			if (d == 0.0) {
				throw new ExpressionException("COT function division by zero");
			}
			return Value.of(1. / d);
		}

		case DEGREES: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			return Value.of(Math.toDegrees(value.toNumber()));
		}

		case FLOOR: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			if (value.isInteger())
				return value;
			if (value.isBigNumber()) {
				return Value.of(value.toBigNumber().setScale(0, RoundingMode.FLOOR));
			}
			return Value.of(Math.floor(value.toNumber()));
		}
		case EXP: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			return Value.of(Math.exp(value.toNumber()));
		}
		case LN: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			if (value.signum() <= 0)
				throw createArgumentOutOfRangeError(value);
			return Value.of(Math.log(value.toNumber()));
		}

		case LOG10: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			if (value.signum() <= 0)
				throw createArgumentOutOfRangeError(value);
			return Value.of(Math.log10(value.toNumber()));
		}

		case PI:
			return Value.PI;

		case RADIANS: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			return Value.of(Math.toRadians(value.toNumber()));
		}

		case RAND: {
			if (args.length == 1) {
				Value value = args[0].eval(context);
				context.getRandom().setSeed(value.toInteger());
			}
			return Value.of(context.getRandom().nextDouble());
		}

		case ROUND: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			return Value.of(Math.round(value.toNumber()));
		}

		case SIGN: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			return Value.of(value.signum());
		}

		case SIN: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			return Value.of(Math.sin(value.toNumber()));
		}

		case SINH: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			return Value.of(Math.sinh(value.toNumber()));
		}

		case CBRT: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			return Value.of(Math.cbrt(value.toNumber()));
		}

		case SQRT: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			if (value.signum() < 0)
				throw createArgumentOutOfRangeError(value);
			return Value.of(Math.sqrt(value.toNumber()));
		}

		case TAN: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			return Value.of(Math.tan(value.toNumber()));
		}

		case TANH: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			return Value.of(Math.tanh(value.toNumber()));
		}

		case ASCII: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			String string = value.toString();
			int ascii = 0;
			if (string.length() > 0) {
				ascii = string.charAt(0);
			}
			return Value.of(ascii);
		}

		case CHR: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			int codePoint = (int) value.toInteger();

			if (!Character.isValidCodePoint(codePoint)) {
				throw new ExpressionException("Invalid character code {0} in the {1} function", codePoint,
						this.getName());
			}
			return Value.of(new String(Character.toChars(codePoint)));
		}

		case IFF: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return Value.NULL;
			
			return args[(value.toBoolean()) ? 1:2].eval(context);
		}
		
		case IFNULL: {
			Value value = args[0].eval(context);
			if (value.isNull())
				value = args[1].eval(context);
			return value;
		}

		case COALESCE: {
			for (IExpression operand : args) {
				Value value = operand.eval(context);
				if (!value.isNull())
					return value;
			}

			return Value.NULL;
		}

		case DECODE: {
			Value value = args[0].eval(context);

			int index = -1;
			for (int i = 1, len = args.length - 1; i < len; i += 2) {
				Value search = args[i].eval(context);
				if (value.compareTo(search) == 0) {
					index = i + 1;
					break;
				}
			}
			if (index < 0 && args.length % 2 == 0) {
				index = args.length - 1;
			}
			if (index < 0)
				return Value.NULL;

			return args[index].eval(context);
		}

		case GREATEST: {
			Value result = Value.NULL;
			for (IExpression operand : args) {
				Value value = operand.eval(context);
				if (result.compareTo(value) < 0)
					result = value;
			}

			return result;
		}

		case LEAST: {
			Value result = Value.NULL;
			for (IExpression operand : args) {
				Value value = operand.eval(context);
				// null is always smaller
				if (value.isNull())
					continue;
				if (result.isNull() || value.compareTo(result) < 0) {
					result = value;
				}
			}

			return result;
		}

		case CONCAT: {
			StringBuilder builder = new StringBuilder();
			for (IExpression operand : args) {
				Value value = operand.eval(context);
				if (!value.isNull())
					builder.append(value);
			}

			if (builder.length() == 0)
				return Value.NULL;

			return Value.of(builder.toString());
		}

//		case LENGTH: {
//			Value value = operands[0].eval(context);
//			if (value.isNull())
//				return value;
//			return Value.of(value.toString().length());
//		}

		case LENGTH: {
			Value value = args[0].eval(context);
			return this.getLenght(value, 1);
		}

		case BIT_LENGTH: {
			Value value = args[0].eval(context);
			return this.getLenght(value, 16);
		}

		case OCTET_LENGTH: {
			Value value = args[0].eval(context);
			return this.getLenght(value, 2);
		}

		case UPPER: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			return Value.of(value.toString().toUpperCase(context.getLocale()));
		}

		case LOWER: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			return Value.of(value.toString().toLowerCase(context.getLocale()));
		}

		case INITCAP: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			return Value.of(WordUtils.capitalizeFully(value.toString()));
		}

		case TO_BOOLEAN: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			return value.convertTo(ValueType.BOOLEAN);
		}

		case TO_CHAR: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			String format = null;
			if (args.length > 1) {
				Value v = args[1].eval(context);
				if (!v.isNull())
					format = v.toString();
			}

			// TODO: to_char nlsParam
			String nlsParam = null;
			switch (value.getType()) {
			case INTEGER:
			case NUMBER:
			case BIGNUMBER:
				return Value.of(ToChar.toChar(value.toBigNumber(), format, nlsParam));
			case DATE:
				return Value.of(ToChar.toChar(value.toDate(), format, nlsParam));
			case STRING:
				return value;
			}
		}
		case TO_NUMBER: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			String format = null;
			if (args.length > 1) {
				Value v = args[1].eval(context);
				if (!v.isNull())
					format = v.toString();

				// TODO: Implement TO_NUMBER
			}

			break;
		}

		case TO_DATE: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			String format = null;
			if (args.length > 1) {
				Value v = args[1].eval(context);
				if (!v.isNull())
					format = v.toString();
			}

			switch (value.getType()) {
			case DATE:
				return value;
			case STRING:
				LocalDateTime dt = DateParser.parse(value.toString(), format);
				return Value.of(dt);
			}
		}

		case SOUNDEX: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			return Value.of(Soundex.getSoundex(value.toString()));
		}

		case LEFT: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return Value.NULL;
			Value count = args[1].eval(context);
			if (count.isNull())
				return Value.NULL;

			String string = value.toString();
			Long length = count.toInteger();
			if (length < 0) {
				throw new ExpressionException("LEFT: Length must be greater than or equal to 0");
			} else if (length > string.length()) {
				return value;
			}

			return Value.of(string.substring(0, length.intValue()));
		}

		case NULLIF: {
			Value value = args[0].eval(context);
			Value compare = args[1].eval(context);

			if (value.compareTo(compare) == 0)
				return Value.NULL;

			return value;
		}

		case RIGHT: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			Value count = args[1].eval(context);
			if (count.isNull())
				return Value.NULL;

			String string = value.toString();
			int length = (int) count.toInteger();
			if (length < 0) {
				throw new ExpressionException("RIGHT: Length must be greater than or equal to 0");
			} else if (length > string.length()) {
				return value;
			}

			return Value.of(string.substring(string.length() - length));
		}

		case TRIM: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			String string = value.toString();
			String characters = null;

			if (args.length == 2) {
				Value strip = args[1].eval(context);
				if (!strip.isNull())
					characters = strip.toString();
			}

			return Value.of(StringUtils.strip(string, characters));
		}

		case LTRIM: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			String string = value.toString();
			String characters = null;
			// int i = 0;

			if (args.length == 2) {
				Value stripChars = args[1].eval(context);
				if (!stripChars.isNull())
					characters = stripChars.toString();
			}

			return Value.of(StringUtils.stripStart(string, characters));
		}

		case RTRIM: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			String string = value.toString();
			String characters = null;

			if (args.length == 2) {
				Value stripChars = args[1].eval(context);
				if (!stripChars.isNull())
					characters = stripChars.toString();
			}

			return Value.of(StringUtils.stripEnd(string, characters));

			// int i = string.length() - 1;
			// while (i >= 0 && Character.isWhitespace(string.charAt(i))) i--;
			// return new StringValue(string.substring(0, i + 1));
		}

		case REPEAT: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			Value number = args[1].eval(context);

			String s = value.toString();
			int count = (int) number.toInteger();
			StringBuilder builder = new StringBuilder(s.length() * count);
			while (count-- > 0) {
				builder.append(s);
			}
			return Value.of(builder.toString());
		}

		case REPLACE: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			Value param2 = args[1].eval(context);

			if (param2.isNull()) {
				return Value.NULL;
			}

			String string = value.toString();
			String search = param2.toString();

			if (args.length == 3) {
				Value param3 = args[2].eval(context);

				String replacement = param3.toString();
				return Value.of(string.replace(search, replacement));
			}

			String result = StringUtils.remove(string, search);
			return Value.of(result);
		}

		case INSTR: {
			Value param1 = args[0].eval(context);
			Value param2 = args[1].eval(context);

			if (param1.isNull() || param2.isNull()) {
				return Value.NULL;
			}

			String string = param1.toString();
			String pattern = param2.toString();

			// If 3 operands
			int start = 0;
			if (args.length == 3) {
				start = (int) args[2].eval(context).toInteger();

				if (start > 0)
					start -= 1;
				else if (start < 0) {
					return Value.of(string.lastIndexOf(pattern, string.length() + start) + 1);
				}
			}

			return Value.of(string.indexOf(pattern, start) + 1);
		}

		case SUBSTR: {
			String string = args[0].eval(context).toString();
			int length = string.length();
			int start = (int) args[1].eval(context).toInteger();

			// These compatibility conditions violate the Standard
			if (start == 0) {
				start = 1;
			} else if (start < 0) {
				start = length + start + 1;
			}

			// Only 2 operands
			if (args.length == 2) {
				return Value.of(string.substring(start - 1));
			}

			int end = start + (int) args[2].eval(context).toInteger();
			// SQL Standard requires "data exception - substring error" when
			// end < start but expression does not throw it for compatibility
			start = Math.max(start, 1);
			end = Math.min(end, length + 1);
			if (start > length || end <= start) {
				// TODO: option to treatEmptyStringsAsNull
				return Value.NULL;
			}

			return Value.of(string.substring(start - 1, end - 1));
		}

		case EQUAL_NULL: {
			Value left = args[0].eval(context);
			Value right = args[1].eval(context);

			// Treats NULLs as known values
			if (left.isNull() && right.isNull()) {
				return Value.TRUE;
			}
			if (left.isNull() || right.isNull()) {
				return Value.FALSE;
			}

			return Value.of(left.compareTo(right) == 0);
		}

		case ENDSWITH: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			Value prefix = args[1].eval(context);

			if (value.isBinary()) {
				return Value.of(Bytes.endsWith(value.toBinary(), prefix.toBinary()));
			}

			return Value.of(value.toString().endsWith(prefix.toString()));
		}

		case STARTSWITH: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			Value prefix = args[1].eval(context);

			if (value.isBinary()) {
				return Value.of(Bytes.startsWith(value.toBinary(), prefix.toBinary()));
			}

			return Value.of(value.toString().startsWith(prefix.toString()));
		}

		case TRANSLATE: {
			Value stringValue = args[0].eval(context);
			Value findCharsValue = args[1].eval(context);
			Value replaceCharsValue = args[2].eval(context);

			if (stringValue.isNull() || findCharsValue.isNull()) {
				return Value.NULL;
			}

			String string = stringValue.toString();
			String findChars = findCharsValue.toString();
			String replaceChars = replaceCharsValue.toString();

			// if it stays null, then no replacements have been made
			StringBuilder buffer = null;
			// if shorter than findChars, then characters are removed
			// (if null, we don't access replaceChars at all)
			int replaceSize = replaceChars == null ? 0 : replaceChars.length();

			for (int i = 0, size = string.length(); i < size; i++) {
				char ch = string.charAt(i);
				int index = findChars.indexOf(ch);
				if (index >= 0) {
					if (buffer == null) {
						buffer = new StringBuilder(size);
						if (i > 0) {
							buffer.append(string, 0, i);
						}
					}
					if (index < replaceSize) {
						ch = replaceChars.charAt(index);
					}
				}
				if (buffer != null) {
					buffer.append(ch);
				}
			}
			return Value.of(buffer == null ? string : buffer.toString());
		}

		case STRINGENCODE: {
			Value value = args[0].eval(context);
	        String result = StringEncoder.encode(value.toString());	        
	        return Value.of(result);
			
		}
		
		case STRINGDECODE: {
			Value value = args[0].eval(context);
	        String result = StringEncoder.decode(value.toString());	        
	        return Value.of(result);
		}
		
		case URLENCODE: {
			Value value = args[0].eval(context);
			try {
				return Value.of(URLEncoder.encode(value.toString(), StandardCharsets.UTF_8.name()));
			} catch (Exception e) {
				throw new ExpressionException("Error encoding url", e);
			}
		}

		case URLDECODE: {
			Value value = args[0].eval(context);
			try {
				return Value.of(URLDecoder.decode(value.toString(), StandardCharsets.UTF_8.name()));
			} catch (Exception e) {
				throw new ExpressionException("Error decoding url", e);
			}
		}

		case UNICODE: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			String string = value.toString();
			int codePoint = 0;
			if (string.length() > 0) {
				codePoint = string.codePointAt(0);
			}
			return Value.of(codePoint);
		}

		case LPAD: {
			Value param1 = args[0].eval(context);
			Value param2 = args[1].eval(context);
			Value param3 = args[2].eval(context);

			if (param1.isNull()) {
				return Value.NULL;
			}

			final int size = (int) param2.toInteger();
			if (size > PAD_LIMIT) {
				new ExpressionException("Paddind length exceeds maximum limit: " + PAD_LIMIT);
			}

			// If this parameter is omitted, the function will pad spaces
			String pad = " ";
			if (param3.isString()) {
				pad = param3.toString();
			}

			String str = param1.toString();
			final int padLen = pad.length();
			final int pads = size - str.length();

			if (pads <= 0) {
				str = str.substring(0, size);
			} else if (padLen == 0) {
				// nothing to do
			} else if (pads == padLen) {
				str = pad.concat(str);
			} else if (pads < padLen) {
				str = pad.substring(0, pads).concat(str);
			} else {
				final char[] padding = new char[pads];
				final char[] padChars = pad.toCharArray();
				for (int i = 0; i < pads; i++) {
					padding[i] = padChars[i % padLen];
				}
				str = new String(padding).concat(str);
			}

			return Value.of(str);
		}

		case RPAD: {
			Value param1 = args[0].eval(context);
			Value param2 = args[1].eval(context);
			Value param3 = args[2].eval(context);

			if (param1.isNull()) {
				return Value.NULL;
			}

			final int size = (int) param2.toInteger();
			if (size > PAD_LIMIT) {
				new ExpressionException("Paddind length exceeds maximum limit: " + PAD_LIMIT);
			}

			// If this parameter is omitted, the function will pad spaces
			String pad = " ";
			if (param3.isString()) {
				pad = param3.toString();
			}

			String str = param1.toString();
			final int padLen = pad.length();
			final int pads = size - str.length();

			if (pads <= 0) {
				str = str.substring(0, size);
			} else if (padLen == 0) {
				// nothing to do
			} else if (pads == padLen) {
				str = str.concat(pad);
			} else if (pads < padLen) {
				str = str.concat(pad.substring(0, pads));
			} else {
				final char[] padding = new char[pads];
				final char[] padChars = pad.toCharArray();
				for (int i = 0; i < pads; i++) {
					padding[i] = padChars[i % padLen];
				}
				str = str.concat(new String(padding));
			}

			return Value.of(str);
		}

		case SPACE: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;
			int length = Math.max(0, (int) value.toInteger());
			char[] chars = new char[length];
			for (int i = length - 1; i >= 0; i--) {
				chars[i] = ' ';
			}

			return Value.of(new String(chars));
		}

		case REVERSE: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			if (value.isBinary()) {
				return Value.of(Bytes.reverse(value.toBinary()));
			}

			StringBuilder sb = new StringBuilder(value.toString()).reverse();
			return Value.of(sb.toString());
		}

		case MD5: {
			Value value = args[0].eval(context);
			return getHash(value, "MD5");
		}

		case SHA1: {
			Value value = args[0].eval(context);
			return getHash(value, "SHA-1");
		}

		case SHA256: {
			Value value = args[0].eval(context);
			return getHash(value, "SHA-256");
		}

		case SHA384: {
			Value value = args[0].eval(context);
			return getHash(value,  "SHA-384");
		}

		case SHA512: {
			Value value = args[0].eval(context);
			return getHash(value, "SHA-512");
		}

		}

		throw createInternalError(kind.name());
	}

	@Override
	public Expression optimize(IExpressionContext context, Expression... operands) throws ExpressionException {

		Expression[] args = new Expression[operands.length];

		boolean isAllConstant = true;
		for (int index = 0; index < args.length; index++) {
			Expression operand = operands[index];

			if (operand instanceof ExpressionCall) {
				operand = operand.optimize(context);
			}

			if (!operand.isConstant())
				isAllConstant = false;

			args[index] = operand;
		}

		if (this.isDeterministic() && isAllConstant) {
			return eval(context, args);
		}
		return new ExpressionCall(this, args);
	}

	private Value getLenght(Value value, int size) {
		if (value.isNull())
			return value;
		return Value.of(value.toString().length() * size);
	}

	private Value getHash(Value value, String algorithm) {
		if (value.isNull())
			return value;

		try {
			MessageDigest md = MessageDigest.getInstance(algorithm);
			md.update(value.toBinary());
			String result = Hex.encodeHexString(md.digest());
			return Value.of(result);
		} catch (NoSuchAlgorithmException e) {
			new ExpressionException("Unknow algorithm: " + algorithm);
		}
		return Value.NULL;
	}
}