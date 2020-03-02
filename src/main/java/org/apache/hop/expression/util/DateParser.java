package org.apache.hop.expression.util;

import java.text.DateFormatSymbols;
import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.Month;
import java.util.ArrayList;
import java.util.Collections;
import java.util.IllegalFormatFlagsException;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import org.apache.hop.expression.ExpressionException;

public class DateParser {

	/**
	 * The beginning of the Julian calendar.
	 */
	private static final int JULIAN_EPOCH = -2_440_588;

	private static final String[] ROMAN_MONTH = { "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI",
			"XII" };

	private static final List<DateFormat> INLINE_LIST = Collections.singletonList(DateFormat.INLINE);

//    /**
//     * The pattern for inline.
//     */
//    static final Pattern PATTERN_INLINE = Pattern.compile("(\"[^\"]*\")");
//	
//	/**
//	 * The pattern for a number.
//	 */
//	static final Pattern PATTERN_NUMBER = Pattern.compile("^([+-]?[0-9]+)");
//
//	/**
//	 * The pattern for four digits (typically a year).
//	 */
//	static final Pattern PATTERN_FOUR_DIGITS = Pattern.compile("^([+-]?[0-9]{4})");
//
//	/**
//	 * The pattern 2-4 digits (e.g. for RRRR).
//	 */
//	static final Pattern PATTERN_TWO_TO_FOUR_DIGITS = Pattern.compile("^([+-]?[0-9]{2,4})");
//	/**
//	 * The pattern for three digits.
//	 */
//	static final Pattern PATTERN_THREE_DIGITS = Pattern.compile("^([+-]?[0-9]{3})");
//
//	/**
//	 * The pattern for two digits.
//	 */
//	static final Pattern PATTERN_TWO_DIGITS = Pattern.compile("^([+-]?[0-9]{2})");
//
//	/**
//	 * The pattern for one or two digits.
//	 */
//	static final Pattern PATTERN_TWO_DIGITS_OR_LESS = Pattern.compile("^([+-]?[0-9][0-9]?)");
//
//	/**
//	 * The pattern for one digit.
//	 */
//	static final Pattern PATTERN_ONE_DIGIT = Pattern.compile("^([+-]?[0-9])");
//
//	/**
//	 * The pattern for a fraction (of a second for example).
//	 */
//	static final Pattern PATTERN_FF = Pattern.compile("^(FF[0-9]?)", Pattern.CASE_INSENSITIVE);
//
//	/**
//	 * The pattern for "am" or "pm".
//	 */
//	static final Pattern PATTERN_AM_PM = Pattern.compile("^(AM|A\\.M\\.|PM|P\\.M\\.)", Pattern.CASE_INSENSITIVE);
//
//	/**
//	 * The pattern for "bc" or "ad".
//	 */
//	static final Pattern PATTERN_BC_AD = Pattern.compile("^(BC|B\\.C\\.|AD|A\\.D\\.)", Pattern.CASE_INSENSITIVE);

	private boolean doyValid = false, absoluteDayValid = false, hour12Valid = false, timeZoneHMValid = false;

	private boolean bc;

	private long absoluteDay;

	private int year, month, day = 1;

	private int dayOfYear;

	private int hour, minute, second, nanos;

	private int hour12;

	private boolean isAM = true;

	private TimeZone timeZone;

	private int timeZoneHour, timeZoneMinute;

	private int currentYear, currentMonth;

	public static LocalDateTime parse(String value, String format) {
		DateParser parser = new DateParser(value, format);
		return parser.build();
	}

	/**
	 * Match the pattern, or if not possible throw an exception.
	 *
	 * @param p      the pattern
	 * @param params the parameters with the input string
	 * @param aEnum  the pattern name
	 * @return the matched value
	 */
//	private static String matchOrThrow(DateFormat format, String value) {
//		Pattern pattern = format.pattern();
//		Matcher matcher = pattern.matcher(value);
//		if (!matcher.find()) {
//			throw new ExpressionException("Issue happened when parsing token '%s'", format.name());
//		}
//		return matcher.group(1);
//	}

	private static int getIndexOfIgnoreCase(String[] strings, String item) {
		for (int i = 0; i < strings.length; i++) {
			if (item.equalsIgnoreCase(strings[i]))
				return i;
		}
		return -1;
	}

	@SuppressWarnings("unchecked")
	private static List<DateFormat>[] TOKENS = new List[25];

	static {
		for (DateFormat format : DateFormat.values()) {
			for (String name : format.names()) {
				int idx = name.charAt(0) - 'A';
				List<DateFormat> list = TOKENS[idx];
				if (list == null) {
					list = new ArrayList<>(1);
					TOKENS[idx] = list;
					System.out.println(name);
				}
				list.add(format);
			}
		}
	}

	/**
	 * Optimization: Only return a list of {@link DateFormat} that share the same
	 * 1st char using the 1st char of the 'to parse' format. Or return {@code null}
	 * if no match.
	 *
	 * @param format the format string
	 * @return the list of tokens, or {@code null}
	 */
	static List<DateFormat> getTokens(String format) {
		if (format != null && !format.isEmpty()) {
			char key = Character.toUpperCase(format.charAt(0));
			if (key >= 'A' && key <= 'Y') {
				return TOKENS[key - 'A'];
			} else if (key == '"') {
				return INLINE_LIST;
			}
		}
		return null;
	}

	private final String format;
	private int pos = 0;

	protected DateParser(String value, String format) {
		this.format = format;

		List<DateFormat> tokens = this.getTokens(format);
	}

	protected void parse(String value, DateFormat format) {
		String str = null;
		switch (format) {
		case SYYYY:
		case YYYY:
			str = format.matchOrThrow(value);
			int dateNr = Integer.parseInt(str);
			// Gregorian calendar does not have a year 0.
			// 0 = 0001 BC, -1 = 0002 BC, ... so we adjust
			if (dateNr == 0) {
				throw new DateTimeException("Year may not be zero");
			}
			this.setYear(dateNr >= 0 ? dateNr : dateNr + 1);
			break;
		case YYY:
			str = format.matchOrThrow(value);
			dateNr = Integer.parseInt(str);
			if (dateNr > 999) {
				throw new ExpressionException("Year may have only three digits with format YYY");
			}
			dateNr += (this.getCurrentYear() / 1_000) * 1_000;
			// Gregorian calendar does not have a year 0.
			// 0 = 0001 BC, -1 = 0002 BC, ... so we adjust
			this.setYear(dateNr >= 0 ? dateNr : dateNr + 1);
			break;
		case YY:
			str = format.matchOrThrow(value);
			dateNr = Integer.parseInt(str);
			if (dateNr > 99) {
				throw new ExpressionException("Year may have only two digits with format YY");
			}
			dateNr += (this.getCurrentYear() / 100) * 100;
			// Gregorian calendar does not have a year 0.
			// 0 = 0001 BC, -1 = 0002 BC, ... so we adjust
			this.setYear(dateNr >= 0 ? dateNr : dateNr + 1);
			break;
		case Y:
			break;
		case MONTH:
			Month month = Month.valueOf(value.toUpperCase());
			this.setMonth(month.getValue());
		case MON:
			DateFormatSymbols dfs = DateFormatSymbols.getInstance(Locale.ENGLISH);
			int index = getIndexOfIgnoreCase(dfs.getShortMonths(), value);
			if (index < 0) {
				throw new ExpressionException("Error parsing date format '%s'", format.name());
			}
			this.setMonth(index + 1);
		case MM:
			str = format.matchOrThrow(value);
			this.setMonth(Integer.parseInt(str));
			break;
		case RM: {
			int monthNumber = 0;
			for (String monthName : ROMAN_MONTH) {
				monthNumber++;
				int len = monthName.length();
				if (value.length() >= len && monthName.equalsIgnoreCase(value.substring(0, len))) {
					this.setMonth(monthNumber + 1);
					str = monthName;
					break;
				}
			}
			if (str == null || str.isEmpty()) {
				throw new ExpressionException("Error parsing date format '%s'", format.name());
			}
			break;
		}
		case Q: /* NOT supported yet */
			throw new IllegalFormatFlagsException("Parsing format Q not supported yet");

		case DDD:
			str = format.matchOrThrow(value);
			this.setDayOfYear(Integer.parseInt(str));
			break;
		case DD:
			str = format.matchOrThrow(value);
			this.setDay(Integer.parseInt(str));
			break;
		case D:
			str = format.matchOrThrow(value);
			this.setDay(Integer.parseInt(str));
			break;
		case DAY:
			// str = setByName(params, ToChar.WEEKDAYS);
			break;
		case DY:
			// str = setByName(params, ToChar.SHORT_WEEKDAYS);
			break;
		case J:
			str = format.matchOrThrow(value);
			this.setAbsoluteDay(JULIAN_EPOCH + Integer.parseInt(str));
			break;
		case AM_PM:
			str = format.matchOrThrow(value);
			if (str.toUpperCase().startsWith("A")) {
				this.setAmPm(true);
			} else {
				this.setAmPm(false);
			}
			break;
		case HH24:
			str = format.matchOrThrow(value);
			this.setHour(Integer.parseInt(str));
			break;
		case HH12:
		case HH:
			str = format.matchOrThrow(value);
			this.setHour12(Integer.parseInt(str));
			break;
		case MI:
			str = format.matchOrThrow(value);
			this.setMinute(Integer.parseInt(str));
			break;
		case SS:
			str = format.matchOrThrow(value);
			this.setSecond(Integer.parseInt(str));
			break;
		case SSSSS:

		case FF:
		case BC_AD:
			break;
		case CC:
			break;
		case E:
			break;
		case EE:
			break;

		case RR:
			break;
		case RRRR:
			break;
		case SCC:
			break;
		case TZD:
			break;
		case TZH:
			break;
		case TZM:
			break;
		case TZR:
			break;

		default:
			break;
		}
	}

	protected void setAbsoluteDay(int absoluteDay) {
		doyValid = false;
		absoluteDayValid = true;
		this.absoluteDay = absoluteDay;
	}

	protected void setBC(boolean bc) {
		doyValid = false;
		absoluteDayValid = false;
		this.bc = bc;
	}

	protected void setYear(int year) {
		doyValid = false;
		absoluteDayValid = false;
		this.year = year;
	}

	protected void setMonth(int month) {
		doyValid = false;
		absoluteDayValid = false;
		this.month = month;
		if (year == 0) {
			year = 1970;
		}
	}

	protected void setDay(int day) {
		doyValid = false;
		absoluteDayValid = false;
		this.day = day;
		if (year == 0) {
			year = 1970;
		}
	}

	protected void setDayOfYear(int dayOfYear) {
		doyValid = true;
		absoluteDayValid = false;
		this.dayOfYear = dayOfYear;
	}

	protected void setHour(int hour) {
		hour12Valid = false;
		this.hour = hour;
	}

	protected void setMinute(int minute) {
		this.minute = minute;
	}

	protected void setSecond(int second) {
		this.second = second;
	}

	protected void setNanos(int nanos) {
		this.nanos = nanos;
	}

	protected void setAmPm(boolean isAM) {
		hour12Valid = true;
		this.isAM = isAM;
	}

	protected void setHour12(int hour12) {
		hour12Valid = true;
		this.hour12 = hour12;
	}

	protected void setTimeZone(TimeZone timeZone) {
		timeZoneHMValid = false;
		this.timeZone = timeZone;
	}

	protected void setTimeZoneHour(int timeZoneHour) {
		timeZoneHMValid = true;
		this.timeZoneHour = timeZoneHour;
	}

	protected void setTimeZoneMinute(int timeZoneMinute) {
		timeZoneHMValid = true;
		this.timeZoneMinute = timeZoneMinute;
	}

	private void queryCurrentYearAndMonth() {

		LocalDate date = LocalDate.now();

		currentYear = date.getYear();
		currentMonth = date.getMonthValue();
	}

	int getCurrentYear() {
		if (currentYear == 0) {
			queryCurrentYearAndMonth();
		}
		return currentYear;
	}

	int getCurrentMonth() {
		if (currentMonth == 0) {
			queryCurrentYearAndMonth();
		}
		return currentMonth;
	}

	protected LocalDateTime build() {
		LocalDate date = null;
		if (absoluteDayValid) {
			// dateValue = DateTimeUtils.dateValueFromAbsoluteDay(absoluteDay);
		} else {
			int year = this.year;
			if (year == 0) {
				year = LocalDate.now().getYear();
			}
			if (bc) {
				year = 1 - year;
			}
			if (doyValid) {
				date = LocalDate.ofYearDay(year, dayOfYear);
			} else {
				int month = this.month;
				if (month == 0) {
					// Oracle uses current month as default
					month = LocalDate.now().getMonthValue();
				}
				date = LocalDate.of(year, month, day);
			}
		}

		int hour;
		if (hour12Valid) {
			hour = hour12 % 12;
			if (!isAM) {
				hour += 12;
			}
		} else {
			hour = this.hour;
		}
		LocalTime time = LocalTime.of(hour, minute, second, nanos);

		return LocalDateTime.of(date, time);
	}
}
