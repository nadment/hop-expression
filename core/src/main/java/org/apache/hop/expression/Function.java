package org.apache.hop.expression;

import java.io.StringWriter;
import java.math.RoundingMode;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.DayOfWeek;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.Month;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.TextStyle;
import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.time.temporal.IsoFields;
import java.time.temporal.TemporalAdjusters;
import java.util.HashMap;
import java.util.Locale;
import java.util.regex.Pattern;

import org.apache.commons.codec.binary.Hex;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.WordUtils;
import org.apache.commons.math3.util.FastMath;
import org.apache.hop.expression.util.Bytes;
import org.apache.hop.expression.util.Functions;
import org.apache.hop.expression.util.StringEncoder;
import org.apache.hop.expression.util.ToChar;
import org.apache.hop.expression.util.ToDate;
import org.apache.hop.expression.util.ToNumber;
import org.apache.hop.i18n.BaseMessages;

/**
 * A <code>Function</code> is a type of operator which has conventional
 * function-call syntax.
 */

//TODO: implement TRY_TO_NUMBER
//TODO: implement TRY_CAST
//TODO: implement TRY_TO_BOOLEAN
//TODO: implement DATEDIFF
//TODO: implement DATEPART
//TODO: implement BIT_COUNT
//TODO: implement PREVIOUS_DAY
//TODO: implement REGEXP_INSTR
//TODO: implement REGEXP_SUBSTR
//TODO: implement REGEXP_REPLACE
//TODO: implement OVERLAY

public class Function extends Operator {

	/**
	 * Set of functions or alias by name.
	 */
	private static final HashMap<String, Function> FUNCTIONS_BY_NAME = new HashMap<>(256);

	// -------------------------------------------------------------
	// FUNCTIONS
	// -------------------------------------------------------------
	static {
		addFunction(Kind.ABS);
		addFunction(Kind.ADD);
		addFunction(Kind.ADD_DAYS);
		addFunction(Kind.ADD_HOURS);
		addFunction(Kind.ADD_MINUTES);
		addFunction(Kind.ADD_MONTHS);
		addFunction(Kind.ADD_SECONDS);
		addFunction(Kind.ADD_WEEKS);
		addFunction(Kind.ADD_YEARS);
		addFunction(Kind.ACOS);
		addFunction(Kind.ACOSH);
		addFunction(Kind.ASCII);
		addFunction(Kind.ASIN);
		addFunction(Kind.ASINH);
		addFunction(Kind.ATAN);
		addFunction(Kind.ATANH);
		addFunction(Kind.ATAN2);
		addFunction(Kind.BITAND);
		addFunction(Kind.BITGET);
		addFunction(Kind.BITNOT);
		addFunction(Kind.BITOR);
		addFunction(Kind.BITXOR);
		addFunction(Kind.CAST);
		addFunction(Kind.CBRT);
		addFunction(Kind.CEIL, "CEILING");
		addFunction(Kind.CHR);
		addFunction(Kind.COALESCE);
		addFunction(Kind.CONCAT);
		addFunction(Kind.COS);
		addFunction(Kind.COSH);
		addFunction(Kind.COT);
		addFunction(Kind.CONTAINS);
		addFunctionNotDeterministic(Kind.CURRENT_DATE,"SYSDATE");
		addFunction(Kind.DATE);
		addFunction(Kind.DATE_PART);
		addFunction(Kind.DAYNAME);
		addFunction(Kind.DAY, "DAYOFMONTH");
		addFunction(Kind.DAYOFWEEK);
		addFunction(Kind.DAYOFWEEK_ISO);
		addFunction(Kind.DAYOFYEAR);
		addFunction(Kind.DAYS_BETWEEN);
		addFunction(Kind.DECODE);
		addFunction(Kind.DEGREES);
		addFunction(Kind.DIVIDE);
		addFunction(Kind.EXTRACT);
		addFunction(Kind.EQUAL_NULL);
		addFunction(Kind.ENDSWITH);
		addFunction(Kind.EXP);
		addFunction(Kind.FIRST_DAY);
		addFunction(Kind.FLOOR);
		addFunction(Kind.GREATEST);
		addFunction(Kind.HOUR);
		addFunction(Kind.HOURS_BETWEEN);
		addFunction(Kind.IF);
		addFunction(Kind.IFNULL, "NVL");
		addFunction(Kind.INITCAP);
		addFunction(Kind.INSTR);
		addFunction(Kind.LAST_DAY);
		addFunction(Kind.LEAST);
		addFunction(Kind.LEFT);
		addFunction(Kind.LENGTH);
		addFunction(Kind.LN);
		addFunction(Kind.LOG);
		addFunction(Kind.LOG10);
		addFunction(Kind.LOWER, "LCASE");
		addFunction(Kind.LPAD);
		addFunction(Kind.LTRIM);
		addFunction(Kind.MD5);
		addFunction(Kind.MINUTE);
		addFunction(Kind.MINUTES_BETWEEN);
		addFunction(Kind.MOD);
		addFunction(Kind.MONTH);
		addFunction(Kind.MONTHNAME);
		addFunction(Kind.MONTHS_BETWEEN);
		addFunction(Kind.MULTIPLY);
		addFunction(Kind.NEXT_DAY);
		addFunction(Kind.NULLIF);
		addFunction(Kind.NVL2);
		addFunction(Kind.PI);
		addFunction(Kind.POWER);
		addFunction(Kind.QUARTER);
		addFunction(Kind.RADIANS);
		addFunctionNotDeterministic(Kind.RAND);
		addFunction(Kind.REGEXP_LIKE);
		addFunction(Kind.REPEAT);
		addFunction(Kind.REPLACE);
		addFunction(Kind.REVERSE);
		addFunction(Kind.RIGHT);
		addFunction(Kind.ROUND);
		addFunction(Kind.RPAD);
		addFunction(Kind.RTRIM);
		addFunction(Kind.SHA1);
		addFunction(Kind.SHA256);
		addFunction(Kind.SHA384);
		addFunction(Kind.SHA512);
		addFunction(Kind.SECOND);
		addFunction(Kind.SECONDS_BETWEEN);
		addFunction(Kind.SIGN);
		addFunction(Kind.SIN);
		addFunction(Kind.SINH);
		addFunction(Kind.SOUNDEX);
		addFunction(Kind.SPACE);
		addFunction(Kind.SQRT);
		addFunction(Kind.STARTSWITH);
		addFunction(Kind.STRINGDECODE);
		addFunction(Kind.STRINGENCODE);
		addFunction(Kind.SUBSTRING, "SUBSTR", "MID");
		addFunction(Kind.SUBTRACT);
		addFunction(Kind.TAN);
		addFunction(Kind.TANH);
		addFunction(Kind.TO_BOOLEAN);
		addFunction(Kind.TO_CHAR);
		addFunction(Kind.TO_DATE);
		addFunction(Kind.TO_NUMBER);
		addFunction(Kind.TRIM);
		addFunction(Kind.TRUNCATE, "TRUNC");
		addFunction(Kind.TRANSLATE);
		addFunction(Kind.UNICODE);
		addFunction(Kind.UPPER, "UCASE");
		addFunction(Kind.URLDECODE);
		addFunction(Kind.URLENCODE);
		addFunction(Kind.WEEK);
		addFunction(Kind.WEEKOFMONTH);
		addFunction(Kind.WEEK_ISO);
		addFunction(Kind.YEAR);
		addFunction(Kind.YEARS_BETWEEN);
	}

	protected static void addFunction(Kind kind) {
		createFunction(kind, kind.name(), false, true);
	}

	protected static void addFunction(Kind kind, String... alias) {
		createFunction(kind, kind.name(), false, true);
		for (String name : alias) {
			createFunction(kind, name, true, true);
		}
	}

	protected static void addFunctionNotDeterministic(Kind kind) {
		addFunctionNotDeterministic(kind, kind.name());
	}

	protected static void addFunctionNotDeterministic(Kind kind, String... alias) {
		createFunction(kind, kind.name(), false, false);
		for (String name : alias) {
			createFunction(kind, name, true, true);
		}
	}

	private static void createFunction(Kind kind, String name, boolean isAlias, boolean isDeterministic) {
		Function function = new Function(kind, name, isAlias, isDeterministic);
		register(function);
		FUNCTIONS_BY_NAME.put(name, function);
	}

	public static Function getFunction(final Kind kind) {
		if (kind == null)
			return null;

		return getFunction(kind.name());
	}

	public static Function getFunction(final String name) {
		if (name == null)
			return null;

		return FUNCTIONS_BY_NAME.get(name.toUpperCase());
	}
	
	private final boolean isDeterministic;

	protected Function(Kind kind, String name, boolean isAlias, boolean isDeterministic) {
		super(kind, name, 10, 10, isAlias);
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
		switch (kind) {

		case CAST: {
			Expression[] operands = call.getOperands();
			writer.append(this.getName());
			writer.append('(');
			operands[0].unparse(writer, leftPrec, rightPrec);
			writer.append(" AS ");
			int ordinal = (int) ((Value) operands[1]).toInteger();
			writer.append(DataType.of(ordinal).name());
			writer.append(')');
			break;
		}
		case EXTRACT: {
			Expression[] operands = call.getOperands();
			writer.append(this.getName());
			writer.append('(');
			operands[0].unparse(writer, leftPrec, rightPrec);
			writer.append(" FROM ");
			int ordinal = (int) ((Value) operands[1]).toInteger();
			writer.append(DatePart.of(ordinal).name());
			writer.append(')');
			break;
		}
		default:
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
	}

	@Override
	public boolean checkNumberOfArguments(int len) throws ExpressionException {

		int min = 0, max = Integer.MAX_VALUE;
		switch (kind) {
		case CURRENT_DATE:
		case PI:
			max = 0;
			break;
		case ABS:
		case ACOS:
		case ACOSH:
		case ASCII:
		case ASIN:
		case ASINH:
		case ATAN:
		case ATANH:
		case CEIL:
		case CBRT:
		case CHR:
		case COS:
		case COSH:
		case COT:
		case DEGREES:
		case EXP:
		case FIRST_DAY:
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
		case DAYOFWEEK_ISO:
		case DAYOFWEEK:
		case DAY:
		case DAYOFYEAR:
		case DAYNAME:
		case MONTH:
		case MONTHNAME:
		case QUARTER:
		case URLDECODE:
		case URLENCODE:
		case WEEK:
		case WEEKOFMONTH:
		case WEEK_ISO:
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
		case TO_CHAR:
		case TO_DATE:
		case TO_NUMBER:
			min = 1;
			max = 2;
			break;

		case ADD:
		case ADD_DAYS:
		case ADD_HOURS:
		case ADD_MINUTES:
		case ADD_MONTHS:
		case ADD_SECONDS:
		case ADD_WEEKS:
		case ADD_YEARS:
		case ATAN2:
		case BITAND:
		case BITNOT:
		case BITOR:
		case BITXOR:
			// case BIT_GET:
		case CONTAINS:
		case DATE_PART:
		case DAYS_BETWEEN:
		case DIVIDE:
		case ENDSWITH:
		case EQUAL_NULL:
		case EXTRACT:
		case HOURS_BETWEEN:
		case IFNULL:
		case LOG:
		case POWER:
		case MINUTES_BETWEEN:
		case MOD:
		case MONTHS_BETWEEN:
		case MULTIPLY:
		case NEXT_DAY:
		case LEFT:
		case REPEAT:
		case RIGHT:
		case NULLIF:
		case SECONDS_BETWEEN:
		case STARTSWITH:
		case SUBTRACT:
		case REGEXP_LIKE:
		case TRUNCATE:
		case YEARS_BETWEEN:
			min = 2;
			max = 2;
			break;
		case CAST:
		case INSTR:
		case LPAD:
		case REPLACE:
		case RPAD:
		case SUBSTRING:
			min = 2;
			max = 3;
			break;
		case IF:
		case NVL2:
		case TRANSLATE:
		case DATE:
			min = 3;
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
			return false;
			// throw new
			// ExpressionException(BaseMessages.getString(PKG,"Expression.InvalidNumberOfArguments",
			// this.getName()));
		}

		return true;
	}

	@Override
	@SuppressWarnings("incomplete-switch")
	public Value eval(final IExpressionContext context, final Expression... args) throws ExpressionException {
		try {
			switch (kind) {
			case ADD:
			case SUBTRACT:
			case MOD:
			case MULTIPLY:
			case DIVIDE:
			case CONCAT:
			case POWER:
			case BITAND:
			case BITNOT:
			case BITOR:
			case BITXOR:
				// use operator implementation
				return super.eval(context, args);

			case CAST: {
				Value value = args[0].eval(context);
				Value type = args[1].eval(context);

				if (value.isNull())
					return Value.NULL;
				if (type.isNull())
					return type;

				DataType targetType = DataType.of((int) type.toInteger());


				if (args.length == 3) {
					String format = null;
					Value f = args[2].eval(context);
					if (!f.isNull())
						format = f.toString();
					
					return value.convertTo(context, targetType, format);
				}
				
				return value.convertTo(targetType);
			}

			case CURRENT_DATE:
				return Value.of(context.getCurrentDate());

			case ADD_DAYS: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;
				Value months = args[1].eval(context);
				if (months.isNull())
					return Value.NULL;

				ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone())
						.plusDays(months.toInteger());
				return Value.of(dt.toInstant());
			}
			case ADD_HOURS: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;
				Value months = args[1].eval(context);
				if (months.isNull())
					return Value.NULL;

				ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone())
						.plusHours(months.toInteger());
				return Value.of(dt.toInstant());
			}
			case ADD_MINUTES: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;
				Value months = args[1].eval(context);
				if (months.isNull())
					return Value.NULL;

				ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone())
						.plusMinutes(months.toInteger());
				return Value.of(dt.toInstant());
			}

			case ADD_MONTHS: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;
				Value months = args[1].eval(context);
				if (months.isNull())
					return Value.NULL;

				ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone())
						.plusMonths(months.toInteger());
				return Value.of(dt.toInstant());
			}

			case ADD_SECONDS: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;
				Value months = args[1].eval(context);
				if (months.isNull())
					return Value.NULL;

				ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone())
						.plusSeconds(months.toInteger());
				return Value.of(dt.toInstant());
			}
			case ADD_WEEKS: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;
				Value months = args[1].eval(context);
				if (months.isNull())
					return Value.NULL;

				ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone())
						.plusWeeks(months.toInteger());
				return Value.of(dt.toInstant());
			}

			case ADD_YEARS: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;
				Value months = args[1].eval(context);
				if (months.isNull())
					return Value.NULL;

				ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone())
						.plusYears(months.toInteger());
				return Value.of(dt.toInstant());
			}

			case DATE: {
				Value v0 = args[0].eval(context);
				if (v0.isNull())
					return Value.NULL;

				Value v1 = args[1].eval(context);
				if (v1.isNull())
					return Value.NULL;

				Value v2 = args[2].eval(context);
				if (v2.isNull())
					return Value.NULL;

				int year = (int) v0.toInteger();
				int month = (int) v1.toInteger();
				int day = (int) v2.toInteger();

				// Locar LocalDate.of(year, month, day).atStartOfDay();

				Instant instant = LocalDate.of(year, month, day).atStartOfDay(ZoneOffset.UTC).toInstant();
				return Value.of(instant);
			}

			case EXTRACT:
			case DATE_PART: {
				Value part = args[0].eval(context);
				if (part.isNull())
					return Value.NULL;

				Value value = args[1].eval(context);
				if (value.isNull())
					return Value.NULL;

				ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
				DatePart datePart = DatePart.of((int) part.toInteger());

				return Value.of(datePart.get(dt));
			}

			case FIRST_DAY: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return Value.NULL;

				ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone())
						.with(TemporalAdjusters.firstDayOfMonth());
				return Value.of(dt.toInstant());
			}

			case LAST_DAY: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return Value.NULL;

				ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone())
						.with(TemporalAdjusters.lastDayOfMonth());
				return Value.of(dt.toInstant());
			}

			case NEXT_DAY: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return Value.NULL;

				Value dow = args[1].eval(context);
				if (dow.isNull())
					return Value.NULL;

				DayOfWeek dayofweek = DayOfWeek.valueOf(dow.toString().toUpperCase());
				ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone())
						.with(TemporalAdjusters.next(dayofweek));

				return Value.of(dt.toInstant());
			}

			case MONTH: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return Value.NULL;

				ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
				return Value.of(dt.getMonthValue());
			}

			case MONTHNAME: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return Value.NULL;

				Month month = ZonedDateTime.ofInstant(value.toDate(), context.getZone()).getMonth();
				return Value.of(month.getDisplayName(TextStyle.FULL, Locale.ENGLISH));
			}

			case MONTHS_BETWEEN: {
				Value value1 = args[0].eval(context);
				if (value1.isNull())
					return Value.NULL;
				Value value2 = args[1].eval(context);
				if (value2.isNull())
					return Value.NULL;

				LocalDate startDate = value1.toDate().atZone(context.getZone()).toLocalDate();
				LocalDate endDate = value2.toDate().atZone(context.getZone()).toLocalDate();
				long days = startDate.until(endDate, ChronoUnit.DAYS);
				return Value.of(days / 31d);
//			long months = startDate.until(endDate, ChronoUnit.MONTHS);
//			return Value.of(months);
			}

			case YEAR: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return Value.NULL;

				ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
				return Value.of(dt.getYear());
			}

			case YEARS_BETWEEN: {
				Value value1 = args[0].eval(context);
				if (value1.isNull())
					return Value.NULL;
				Value value2 = args[1].eval(context);
				if (value2.isNull())
					return Value.NULL;

				LocalDate startDate = value1.toDate().atZone(context.getZone()).toLocalDate();
				LocalDate endDate = value2.toDate().atZone(context.getZone()).toLocalDate();
				long years = startDate.until(endDate, ChronoUnit.YEARS);
				return Value.of(years);
			}

			case QUARTER: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return Value.NULL;

				ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
				return Value.of(dt.get(IsoFields.QUARTER_OF_YEAR));
			}

			case DAYOFWEEK: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;

				DayOfWeek dow = ZonedDateTime.ofInstant(value.toDate(), context.getZone()).getDayOfWeek();

				int result = dow.getValue() + 1;
				if (result == 8)
					result = 1;

				return Value.of(result);
			}

			case DAYOFWEEK_ISO: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return Value.NULL;

				DayOfWeek dow = ZonedDateTime.ofInstant(value.toDate(), context.getZone()).getDayOfWeek();
				return Value.of(dow.getValue());

//			ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
//			return Value.of(DatePart.DAYOFWEEKISO.get(dt));
			}

			case DAY: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;

				ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
				return Value.of(dt.getDayOfMonth());
			}

			case DAYOFYEAR: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;

				ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
				return Value.of(dt.getDayOfYear());
			}

			case DAYNAME: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return Value.NULL;

				DayOfWeek weekday = ZonedDateTime.ofInstant(value.toDate(), context.getZone()).getDayOfWeek();
				return Value.of(weekday.getDisplayName(TextStyle.FULL, Locale.ENGLISH));
			}

			case DAYS_BETWEEN: {
				Value value1 = args[0].eval(context);
				if (value1.isNull())
					return Value.NULL;
				Value value2 = args[1].eval(context);
				if (value2.isNull())
					return Value.NULL;

				LocalDate startDate = value1.toDate().atZone(context.getZone()).toLocalDate();
				LocalDate endDate = value2.toDate().atZone(context.getZone()).toLocalDate();
				long days = startDate.until(endDate, ChronoUnit.DAYS);
				return Value.of(days);
			}

			case WEEK: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;

				ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
				return Value.of(dt.get(ChronoField.ALIGNED_WEEK_OF_YEAR));
			}

			case WEEKOFMONTH: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;

				ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
				return Value.of(dt.get(ChronoField.ALIGNED_WEEK_OF_MONTH));
			}

			case HOUR: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;

				LocalTime time = LocalTime.from(value.toDate().atZone(context.getZone()));
				return Value.of(time.getHour());
			}

			case HOURS_BETWEEN: {
				Value value1 = args[0].eval(context);
				if (value1.isNull())
					return Value.NULL;
				Value value2 = args[1].eval(context);
				if (value2.isNull())
					return Value.NULL;

				LocalDateTime startDateTime = value1.toDate().atZone(context.getZone()).toLocalDateTime();
				LocalDateTime endDateTime = value2.toDate().atZone(context.getZone()).toLocalDateTime();
				long hours = startDateTime.until(endDateTime, ChronoUnit.HOURS);
				return Value.of(hours);
			}

			case MINUTE: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;

				LocalTime time = LocalTime.from(value.toDate().atZone(context.getZone()));
				return Value.of(time.getMinute());
			}

			case MINUTES_BETWEEN: {
				Value value1 = args[0].eval(context);
				if (value1.isNull())
					return Value.NULL;
				Value value2 = args[1].eval(context);
				if (value2.isNull())
					return Value.NULL;

				LocalDateTime startDateTime = value1.toDate().atZone(context.getZone()).toLocalDateTime();
				LocalDateTime endDateTime = value2.toDate().atZone(context.getZone()).toLocalDateTime();
				long minutes = startDateTime.until(endDateTime, ChronoUnit.MINUTES);
				return Value.of(minutes);
			}

			case SECOND: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;
				LocalTime time = LocalTime.from(value.toDate().atZone(context.getZone()));
				return Value.of(time.getSecond());
			}

			case SECONDS_BETWEEN: {
				Value value1 = args[0].eval(context);
				if (value1.isNull())
					return Value.NULL;
				Value value2 = args[1].eval(context);
				if (value2.isNull())
					return Value.NULL;

				LocalDateTime startDateTime = value1.toDate().atZone(context.getZone()).toLocalDateTime();
				LocalDateTime endDateTime = value2.toDate().atZone(context.getZone()).toLocalDateTime();
				long seconds = startDateTime.until(endDateTime, ChronoUnit.SECONDS);
				return Value.of(seconds);
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
				return Value.of(Math.acos(d));
			}

			case ACOSH: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;
				return Value.of(FastMath.acosh(value.toNumber()));
			}

			case ASIN: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;
				return Value.of(Math.asin(value.toNumber()));
			}

			case ASINH: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;

				return Value.of(FastMath.asinh(value.toNumber()));
			}

			case ATAN: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;

				return Value.of(Math.atan(value.toNumber()));
			}

			case ATANH: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;

				return Value.of(FastMath.atanh(value.toNumber()));
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
					throw createDivisionByZeroError();
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

			case LOG: {
				Value base = args[0].eval(context);

				if (base.isNull())
					return Value.NULL;

				Value value = args[1].eval(context);
				if (value.isNull())
					return value;
				if (value.signum() <= 0)
					throw createArgumentOutOfRangeError(value);

				return Value.of(Math.log(value.toNumber()) / Math.log(base.toNumber()));
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
					throw new ExpressionException(BaseMessages.getString(PKG,"Invalid character code {0}", codePoint));
				}
				return Value.of(new String(Character.toChars(codePoint)));
			}

			case IF: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return Value.NULL;

				return args[(value.toBoolean()) ? 1 : 2].eval(context);
			}

			case IFNULL:
			case COALESCE: {
				for (Expression operand : args) {
					Value value = operand.eval(context);
					if (!value.isNull())
						return value;
				}

				return Value.NULL;
			}

			case NVL2: {
				Value condition = args[0].eval(context);

				if (condition.isNull()) {
					return args[2].eval(context);
				}

				return args[1].eval(context);
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
				for (Expression operand : args) {
					Value value = operand.eval(context);
					if (result.compareTo(value) < 0)
						result = value;
				}

				return result;
			}

			case LEAST: {
				Value result = Value.NULL;
				for (Expression operand : args) {
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

			case LENGTH: {
				Value value = args[0].eval(context);
				return this.getLenght(value, 1);
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

				return value.convertTo(DataType.BOOLEAN);
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

				switch (value.getDataType()) {
				case INTEGER:
				case NUMBER:
				case BIGNUMBER:
					return Value.of(ToChar.toChar(value.toBigNumber(), format, context.getLocale()));
				case DATE:
					ZonedDateTime dt = value.toDate().atZone(context.getZone());
					return Value.of(ToChar.toChar(dt, format, context.getLocale()));
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
				}

				return Value.of(ToNumber.toNumber(value.toString(), format));				
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

				switch (value.getDataType()) {
				case DATE:
					return value;
				case STRING:
					Instant instant = ToDate.parse(value.toString(), format);
					return Value.of(instant);
				}
			}

			case SOUNDEX: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return value;
				return Value.of(Functions.soundex(value.toString()));
			}

			case LEFT: {
				Value value = args[0].eval(context);
				if (value.isNull())
					return Value.NULL;

				Value count = args[1].eval(context);
				if (count.isNull())
					return Value.NULL;

				return Value.of(Functions.left(value.toString(), (int) count.toInteger()));
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

				return Value.of(Functions.right(value.toString(), (int) count.toInteger()));
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

			case SUBSTRING: {
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
			
			case CONTAINS: {
				Value left = args[0].eval(context);
				Value right = args[1].eval(context);

				if (left.isNull() && right.isNull())
					return Value.TRUE;

				if (left.isNull() || right.isNull())
					return Value.FALSE;

				if (left.toString().contains(right.toString()))
					return Value.TRUE;

				return Value.FALSE;
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

			case REGEXP_LIKE: {
				Value input = args[0].eval(context);
				Value pattern = args[1].eval(context);

				if (input.isNull() || pattern.isNull()) {
					return Value.FALSE;
				}

				Pattern p = Pattern.compile(pattern.toString(), Pattern.DOTALL);

				return Value.of(p.matcher(input.toString()).find());
			}

			case TRUNCATE: {
				Value value = args[0].eval(context);
				Value pattern = args[1].eval(context);

				if (value.isNull() || pattern.isNull())
					return Value.NULL;

				switch (value.getDataType()) {
				case INTEGER:
				case NUMBER:
				case BIGNUMBER:
					int scale = (int) pattern.toInteger();
					return Value.of(Functions.truncate(value.toBigNumber(), scale));

				case DATE:
					Instant instant = value.toDate();

					// TODO: optimize with ordinal: DatePart datePart = DatePart.valueOf((int)
					// right.toInteger());
					DatePart datePart = DatePart.MONTH;

					try {
						datePart = DatePart.of(pattern.toString());
					} catch (Exception e) {
						throw new ExpressionException("Invalid date part: " + pattern);
					}

					switch (datePart) {
					case YEAR: {
						ZonedDateTime datetime = ZonedDateTime.ofInstant(instant.truncatedTo(ChronoUnit.DAYS),
								context.getZone());
						return Value.of(datetime.withDayOfYear(1).toInstant());
					}
					case MONTH: {
						ZonedDateTime datetime = ZonedDateTime.ofInstant(instant.truncatedTo(ChronoUnit.DAYS),
								context.getZone());
						return Value.of(datetime.withDayOfMonth(1).toInstant());
					}
					case DAY:
						return Value.of(instant.truncatedTo(ChronoUnit.DAYS));
					case HOUR:
						return Value.of(instant.truncatedTo(ChronoUnit.HOURS));
					case MINUTE:
						return Value.of(instant.truncatedTo(ChronoUnit.MINUTES));
					case SECOND:
						return Value.of(instant.truncatedTo(ChronoUnit.SECONDS));
					case MILLISECOND:
						return Value.of(instant.truncatedTo(ChronoUnit.MILLIS));
					case MICROSECOND:
						return Value.of(instant.truncatedTo(ChronoUnit.MICROS));
					case NANOSECOND:
						return Value.of(instant.truncatedTo(ChronoUnit.NANOS));
					}

					return value;

				case STRING:
				case BOOLEAN:
				default:
					throw new ExpressionException("Truncate date not implemented");
				}
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
					throw new ExpressionException(BaseMessages.getString(PKG, "Error encoding url"), e);
				}
			}

			case URLDECODE: {
				Value value = args[0].eval(context);
				try {
					return Value.of(URLDecoder.decode(value.toString(), StandardCharsets.UTF_8.name()));
				} catch (Exception e) {
					throw new ExpressionException(BaseMessages.getString(PKG, "Error decoding url"), e);
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
				if (param1.isNull()) {
					return Value.NULL;
				}

				Value param2 = args[1].eval(context);
				final int length = (int) param2.toInteger();

				// If this parameter is omitted, the function will pad spaces
				String pad = null;
				if (args.length == 3) {
					Value param3 = args[2].eval(context);
					pad = param3.toString();
				}

				return Value.of(Functions.lpad(param1.toString(), length, pad));
			}

			case RPAD: {
				Value param1 = args[0].eval(context);

				if (param1.isNull()) {
					return Value.NULL;
				}

				Value param2 = args[1].eval(context);
				final int length = (int) param2.toInteger();

				// If this parameter is omitted, the function will pad spaces
				String pad = null;
				if (args.length == 3) {
					Value param3 = args[2].eval(context);
					pad = param3.toString();
				}

				return Value.of(Functions.rpad(param1.toString(), length, pad));
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
				return getHash(value, "SHA-384");
			}

			case SHA512: {
				Value value = args[0].eval(context);
				return getHash(value, "SHA-512");
			}

			}
		}
		catch (RuntimeException e) {
			throw new ExpressionException(
					BaseMessages.getString(PKG, "Expression.FunctionError", this.getName(), e.getMessage()), e);
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