package org.apache.hop.expression.util;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.util.IllegalFormatFlagsException;
import java.util.TimeZone;

import org.apache.hop.expression.ExpressionException;

public class ToDate {

	// Specifies the “century start” year for 2-digit years. This parameter prevents
	// ambiguous dates when importing or converting data with the YY date format
	// component.
	int TWO_DIGIT_CENTURY_START = 1970;

	/**
	 * The offset from Julian to EPOCH DAY.
	 */
	private static final long JULIAN_DAY_OFFSET = 2440588L;

	private static final String[] ROMAN_MONTHS = { "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI",
			"XII" };

	private static final String[] SHORT_MONTHS = { "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT",
			"NOV", "DEC" };
	private static final String[] MONTHS = { "JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE", "JULY", "AUGUST",
			"SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER" };

	private static final String[] AM_PM = { "AM", "A.M.", "PM", "P.M." };

	private static final String[] AD_BC = { "AD", "A.D.", "BC", "B.C." };

	private boolean isDoyValid = false;
	private boolean isEpochDayValid = false;
	private boolean isHour12Valid = false;
	private boolean isTimeZoneHMValid = false;
	private boolean isAM = true;
	private boolean bc;

	private long epochDay;
	private int year;
	private int month;
	private int day = 1;
	private int dayOfYear;
	private int hour;
	private int minute;
	private int second;
	private int nanos;
	private int timeZoneHour;
	private int timeZoneMinute;
	private int currentYear;
	private int currentMonth;

	public static void main(String[] args) {
//		Instant instant = ToDate.parse("2019-05-28 14:15:16", "YYYY-MM-DD HH24:MI:SS");
//		Instant instant = ToDate.parse("12:59:33 365-2003", "HH24:MI:SS DDD-YYYY");
//		Instant instant = ToDate.parse("1/II/2020", "DD/RM/YYYY");
//		Instant instant = ToDate.parse("2019-02-13 11:34:56", "YYYY-MM-DD HH12:MI:SS");
		// Instant instant = ToDate.parse("2009-12-24 11:00:00 PM", "YYYY-MM-DD
		// HH12:MI:SS AM");
		Instant instant = ToDate.parse("01/02/-100", "DD/MM/SYYYY");

//		Instant instant = ToDate.parse("2020-MarCH", "YYYY-MONTH");
//		Instant instant = ToDate.parse("2020,feb,25", "YYYY,MON,DD");

	}

	public static Instant parse(String value, String format) {

		ToDate parser = new ToDate(value, format);
		Instant instant = parser.build();
		System.out.println("ToDate('" + value + "','" + format + "')=" + instant);

		return instant;
	}

	private ToDate(String value, String fmt) {
		int pos = 0;
		int index = 0;

		// Ignore case for parsing
		String format = fmt.toUpperCase();

		while (pos < format.length()) {
			char c = format.charAt(pos);

			String str = null;

			// Use first letter for optimization
			switch (c) {

			// Ignore space and punctuation such as hyphen (-), slash (/), comma (,), period
			// (.) and colons (:)
			case ' ':
			case '-':
			case '_':
			case '/':
			case ',':
			case '.':
			case ';':
			case ':':
			case '\\':
				pos++;
				index++;
				continue;

			// Character string literals enclosed in double quotation marks.

			case 'A':
				// Meridian indicator
				if (format.startsWith("AM", pos)) {
					str = this.parseAmPm(value, index);
					pos += 2;
				}
				// Meridian indicator with period
				else if (format.startsWith("A.M.", pos)) {
					str = this.parseAmPm(value, index);
					pos += 4;
				}
				// Era designator
				else if (format.startsWith("AD", pos)) {
					str = this.parseAdBc(value, index);
					pos += 2;
				}
				// Era designator with period
				else if (format.startsWith("A.D.", pos)) {
					str = this.parseAdBc(value, index);
					pos += 4;
				}

				break;

			case 'B':
				// Era designator
				if (format.startsWith("BC", pos)) {
					str = this.parseAdBc(value, index);
					pos += 2;
				}
				// Era designator with period
				else if (format.startsWith("B.C.", pos)) {
					str = this.parseAdBc(value, index);
					pos += 4;
				}
				break;

			case 'D':
				// Day of year (1-366)
				if (format.startsWith("DDD", pos)) {
					str = matchDigit(value, index, 3);
					this.setDayOfYear(Integer.parseInt(str));
					pos += 3;
				}
				// Day of month (1-31)
				else if (format.startsWith("DD", pos)) {
					str = matchDigit(value, index, 2);
					this.setDay(Integer.parseInt(str));
					pos += 2;
				}
				// FIXME: Day of week (1-7)
				else {
					str = matchDigit(value, index, 1);
					this.setDay(Integer.parseInt(str));
					pos += 1;
				}
				break;

			case 'F':
				// Fractional seconds FF("^(FF[0-9]?)"),
				throw new IllegalFormatFlagsException("Parsing format F not supported yet");

			case 'J': {
				// Julian day; the number of days since Jan 1, 4712 BC.

				pos += 1;
				str = matchDigit(value, index, 7);
				this.setEpochDay(Integer.parseInt(str) - JULIAN_DAY_OFFSET);
				// throw new IllegalFormatFlagsException("Parsing format J not supported yet");
			}
			case 'M':
				// Month (1-12)
				if (format.startsWith("MM", pos)) {
					pos += 2;
					str = matchDigit(value, index, 2);

					// Rule to try alternate format MON and MONTH
					if (str == null) {
						str = parseMonth(value, index);
					} else {
						this.setMonth(Integer.parseInt(str));
					}
				}
				// Month full name
				else if (format.startsWith("MONTH", pos)) {
					pos += 5;
					str = parseMonth(value, index);
				}
				// Abbreviated name of month (parse after MONTH)
				else if (format.startsWith("MON", pos)) {
					pos += 3;
					str = parseMonth(value, index);
				}
				// Minutes (0-59)
				else if (format.startsWith("MI", pos)) {
					str = matchDigit(value, index, 2);
					this.minute = Integer.parseInt(str);
					pos += 2;
				}
				break;

			case 'H':
				// Hour of day (1-23)
				if (format.startsWith("HH24", pos)) {
					str = matchDigit(value, index, 2);
					this.setHour(Integer.parseInt(str));
					pos += 4;
				}
				// Hour of day (1-12)
				else if (format.startsWith("HH12", pos)) {
					str = matchDigit(value, index, 2);
					this.setHour12(Integer.parseInt(str));
					pos += 4;
				}
				// Hour of day (1-12)
				else if (format.startsWith("HH", pos)) {
					str = matchDigit(value, index, 2);
					this.setHour12(Integer.parseInt(str));
					pos += 2;
				}
				break;

			case 'Q':
				/* NOT supported yet */
				throw new IllegalFormatFlagsException("Parsing format Q not supported yet");

			case 'R':
				// Roman numeral month (I-XII; January = I).
				if (format.startsWith("RM", pos)) {
					str = matchRoman(value, index, 3);
					month = 1;
					for (String name : ROMAN_MONTHS) {
						if (name.equals(str)) {
							this.setMonth(month);
							pos += 2;
							break;
						}
						month++;
					}

					if (str == null)
						throw new ExpressionException("Invalid month name when parsing date with format RM");
				}
				// 4-digit year
				else if (format.startsWith("RRRR", pos)) {
					str = matchDigit(value, index, 4);
					int year = Integer.parseInt(str);
					// Years between 00-49 will be given the 21st century (the year 2000)
					if (year >= 0 && year <= 49)
						year += 2000;
					// Years between 50-99 will be given the 20th century (the year 1900).
					else if (year >= 50 && year <= 99)
						year += 1900;
					this.setYear(year);
					pos += 4;
				}
			case 'S':
				// Seconds
				if (format.startsWith("SS", pos)) {
					str = matchDigit(value, index, 2);
					this.second = Integer.parseInt(str);
					pos += 2;
				}
				// 4-digit year; S prefixes BC dates with a minus sign
				else if (format.startsWith("SYYYY", pos)) {
					str = matchSignedDigit(value, index, 5);
					this.setYear(Integer.parseInt(str));
					pos += 5;
				}
				break;

			case 'Y':
				// 4-digit year
				if (format.startsWith("YYYY", pos)) {
					str = matchDigit(value, index, 4);
					this.setYear(Integer.parseInt(str));
					pos += 4;
				}
				// Last 2-digit year
				else if (format.startsWith("YY", pos)) {
					str = matchDigit(value, index, 2);
					int yy = Integer.parseInt(str);
					yy += (1900 + yy > TWO_DIGIT_CENTURY_START) ? 2000 : 1900;
					this.setYear(yy);
					pos += 2;
				}
				break;
			}

			if (str != null)
				index += str.length();
			else
				throw new ExpressionException("Error parsing value '" + value + "' with format '" + fmt + '\'');
		}
	}

	protected String matchDigit(String s, int index, int length) {
		StringBuilder output = new StringBuilder();
		int end = index + length;
		if (end > s.length()) {
			end = s.length();
		}

		while (index < end) {
			char c = s.charAt(index++);
			if (!Characters.isDigit(c))
				break;
			output.append(c);
		}

		return (output.length() == 0) ? null : output.toString();
	}

	protected String matchSignedDigit(String s, int index, int length) {
		StringBuilder output = new StringBuilder();

		int end = index + length;
		if (end > s.length()) {
			end = s.length();
		}

		if (s.charAt(index) == '-') {
			index++;
			output.append('-');
		}

		while (index < end) {
			char c = s.charAt(index++);
			if (!Characters.isDigit(c))
				break;
			output.append(c);
		}

		return (output.length() == 0) ? null : output.toString();
	}

	protected String matchRoman(String value, int index, int length) {
		StringBuilder output = new StringBuilder();
		int count = 0;
		while (count < length) {
			char c = value.charAt(index++);
			if (c != 'I' && c != 'V' && c != 'X') {
				break;
			}

			output.append(c);
			count++;

		}

		return (output.length() == 0) ? null : output.toString();
	}

	private String parseMonth(String value, int index) {
		int month = 1;
		for (String name : SHORT_MONTHS) {
			if (value.regionMatches(true, index, name, 0, name.length())) {
				this.setMonth(month);

				// Try full name to provide rule alternative '01/February/2020' with format
				// 'DD/MON/YYYY'
				String fullName = MONTHS[month - 1];
				if (value.regionMatches(true, index, fullName, 0, fullName.length())) {
					return fullName;
				}

				return name;
			}
			month++;
		}

		return null;
	}

	private String parseAmPm(String value, int index) {
		for (String name : AM_PM) {
			if (value.regionMatches(true, index, name, 0, name.length())) {
				this.setAm(name.charAt(0) == 'A');
				return name;
			}
		}
		return null;
	}

	private String parseAdBc(String value, int index) {
		for (String name : AD_BC) {
			if (value.regionMatches(true, index, name, 0, name.length())) {
				this.setBC(name.charAt(0) == 'B');
				return name;
			}
		}
		return null;
	}

	
	protected void setEpochDay(long epochDay) {
		isDoyValid = false;
		isEpochDayValid = true;
		this.epochDay = epochDay;
	}

	protected void setBC(boolean bc) {
		isDoyValid = false;
		isEpochDayValid = false;
		this.bc = bc;
	}

	protected void setYear(int year) {
		// isDoyValid = false;
		isEpochDayValid = false;
		this.year = year;
	}

	protected void setMonth(int month) {
		isDoyValid = false;
		isEpochDayValid = false;
		this.month = month;
		if (year == 0) {
			year = 1970;
		}
	}

	protected void setDay(int day) {
		isDoyValid = false;
		isEpochDayValid = false;
		this.day = day;
		if (year == 0) {
			year = 1970;
		}
	}

	protected void setDayOfYear(int dayOfYear) {
		isDoyValid = true;
		isEpochDayValid = false;
		this.dayOfYear = dayOfYear;
	}

	protected void setHour(int hour) {
		isHour12Valid = false;
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

	protected void setAm(boolean isAM) {
		isHour12Valid = true;
		this.isAM = isAM;
	}

	protected void setHour12(int hour12) {
		isHour12Valid = true;
		this.hour = hour12;
	}

	protected void setTimeZone(TimeZone timeZone) {
		isTimeZoneHMValid = false;
		// this.timeZone = timeZone;
	}

	protected void setTimeZoneHour(int timeZoneHour) {
		isTimeZoneHMValid = true;
		this.timeZoneHour = timeZoneHour;
	}

	protected void setTimeZoneMinute(int timeZoneMinute) {
		isTimeZoneHMValid = true;
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

	protected Instant build() {
		LocalDate date = null;
		if (isEpochDayValid) {
			date = LocalDate.ofEpochDay(epochDay);
		} else {
			int year = this.year;
			if (year == 0) {
				year = LocalDate.now().getYear();
			}
			if (bc) {
				year = 1 - year;
			}
			if (isDoyValid) {
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

		if (isHour12Valid) {
			hour = hour % 12;
			if (!isAM) {
				hour += 12;
			}
		}
		LocalTime time = LocalTime.of(hour, minute, second, nanos);

		return LocalDateTime.of(date, time).toInstant(ZoneOffset.UTC);
	}
}
