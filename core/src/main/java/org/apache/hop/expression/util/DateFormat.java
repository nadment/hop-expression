package org.apache.hop.expression.util;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.hop.expression.ExpressionException;

public enum DateFormat {
	/** 4-digit year */
	YYYY("^([0-9]{2,4})"),
	/** 4-digit year with sign (- = B.C.) */
	SYYYY("^([+-]?[0-9]{2,4})"),
	/** Last 3-digit year */
	YYY("^([0-9]{3})"),
	/** Last 2-digit year */
	YY("^([0-9]{2})"),
	/** Last 1-digit year */
	Y("^([0-9])"),
	/** 4-digit year based on the ISO standard. */
	IYYY,
	/** Last 3-digit of ISO year */
	IYY,
	/** Last 2-digit of ISO year */
	IY,
	/** Last 1 digit of ISO year. */
	I,
	/** Two-digit century with sign (- = B.C.) */
	SCC,
	/** Two-digit century. */
	CC,
	/** 2-digit -> 4-digit year 0-49 -> 20xx , 50-99 -> 19xx */
	RRRR,
	/** last 2-digit of the year using "current" century value. */
	RR,
	/** Meridian indicator */
	BC_AD("^(BC|B\\.C\\.|AD|A\\.D\\.)", "^(BC|B\\.C\\.|AD|A\\.D\\.)"),
	/** Full Name of month */
	MONTH,
	/** Abbreviated name of month */
	MON,
	/** Month (01-12; JAN = 01) */
	MM("^([0-9]{1,2})"),
	/** Roman numeral month (I-XII; JAN = I) */
	RM,
	/** Day of year (1-366) */
	DDD("^([0-9]{1,3})"),
	/** Name of day */
	DAY,
	/** Day of month (1-31) */
	DD("^([0-9]{1,2})"),
	/** Abbreviated name of day */
	DY,
	/** Hour of day (1-23). */
	HH24("^([0-9]{1,2})"),
	/** Hour of day (1-12). */
	HH12("^([0-9]{1,2})"),
	/** Hour of day (1-12). */
	HH("^([0-9]{1,2})"),
	/** Minutes */
	MI("^([0-9]{1,2})"),
	/** Seconds past midnight (0-86399) */
	SSSSS, SS("^([0-9]{1,2})"),
	/** Fractional seconds */
	FF("^(FF[0-9]?)"),
	/** Time zone hour. */
	TZH,
	/** Time zone minute. */
	TZM,
	/** Time zone region ID */
	TZR,
	/** Daylight savings information. Example: PST (for US/Pacific standard time) */
	TZD,
	/** Meridian indicator */
	AM_PM("^(AM|A\\.M\\.|PM|P\\.M\\.)", "^(AM|A\\.M\\.|PM|P\\.M\\.)"),
	// NOT supported yet -
	// Full era name (Japanese Imperial, ROC Official,
	// and Thai Buddha calendars).
	EE,
	/**
	 * NOT supported yet - Abbreviated era name (Japanese Imperial, ROC Official,
	 * and Thai Buddha calendars).
	 */
	E("^([0-9])"),
	/** Quarter of year (1, 2, 3, 4; JAN-MAR = 1). */
	Q("^([0-9])"),
	/** Day of week (1-7). */
	D,
	// NOT supported yet -
	// Julian day; the number of days since Jan 1, 4712 BC.
	J,
	/**
	 * Week of month (1-5) where week 1 starts on the first day of the month and
	 * ends on the seventh.
	 */
	W("^([0-9])"),
	/** Week of year (1-53) */
	WW("^([0-9]{1,2})"),
	// Inline text e.g. to_date('2017-04-21T00:00:00Z',
	// 'YYYY-MM-DD"T"HH24:MI:SS"Z"')
	// where "T" and "Z" are inlined
	INLINE("(\"[^\"]*\")");

	private final String[] names;
	private final Pattern pattern;
	private final Pattern format;

	private DateFormat() {
		this(null);
	}

	private DateFormat(String patternValue, String patternFormat) {

		this.pattern = Pattern.compile(patternValue);
		this.format = Pattern.compile(patternFormat, Pattern.CASE_INSENSITIVE);
		this.names = this.name().split("_");
	}

	private DateFormat(String patternValue) {
		this.pattern = Pattern.compile(patternValue);
		this.format = Pattern.compile(String.format("^(%s)", this.name()), Pattern.CASE_INSENSITIVE);
		this.names = this.name().split("_");
	}

	public String[] names() {
		return names;
	}

	public Pattern format() {
		return format;
	}

	/**
	 * Match the pattern, or if not possible throw an exception.
	 *
	 * @param p      the pattern
	 * @param params the parameters with the input string
	 * @param aEnum  the pattern name
	 * @return the matched value
	 */
	public String matchOrThrow(String value) throws ExpressionException {

		Matcher matcher = pattern.matcher(value);
		if (!matcher.find()) {
			throw new ExpressionException("Issue happened when parsing token '%s'", name());
		}
		return matcher.group(1);
	}
}
