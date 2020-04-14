package org.apache.hop.expression;

import java.util.Collection;
import java.util.EnumSet;
import java.util.Set;

/**
 * Enumerates the possible types of {@link Expression}.
 */

public enum Kind {

	VALUE,

	LIST,
	IDENTIFIER,
	
	
	BITGET,

	/**
	 * The bitwise AND operation.
	 */
	BITWISE_AND_OPERATOR("&"),

	/**
	 * The bitwise OR operation.
	 */
	BITWISE_OR_OPERATOR("|"),

	/**
	 * The bitwise NOT operation.
	 */
	BITWISE_NOT_OPERATOR("~"),

	/**
	 * The bitwise XOR operation.
	 */
	BITWISE_XOR_OPERATOR("^"),

	LSHIFT_OPERATOR("<<"),
	
	RSHIFT_OPERATOR(">>"),

	/**
	 * Case when operator
	 */
	CASE_WHEN_OPERATOR("CASE"),

	/**
	 * Converts a value of one data type into another data type.
	 */
	CAST_OPERATOR("CAST"),

	/**
	 * Returns the length of a string or binary value in bits.
	 */
	BIT_LENGTH,

	OCTET_LENGTH,

	/**
	 * String concatenation operator '<code>||</code>'.
	 * 
	 * @See {@link #CONCAT}
	 */
	CONCAT_OPERATOR("||"),

	/**
	 * Concat function
	 */
	CONCAT,

	/**
	 * Contains operator <code>=~<code>
	 */
	CONTAINS_OPERATOR("=~"),

	/**
	 * Contains function
	 */
	CONTAINS,

	/**
	 * The arithmetic division operator '/'.
	 */
	DIVIDE_OPERATOR("/"),

	/**
	 * The arithmetic multiplication operator '*'.
	 */
	MULTIPLY_OPERATOR("*"),

	/**
	 * Returns the exponential value of a numeric expression.
	 */
	EXP,

//	/**
//	 * The arithmetic power operator '^'.
//	 * 
//	 * @See {@link #POWER}
//	 */
//	POWER_OPERATOR("^"),

	/**
	 * The function raises a Number to a Power.
	 * 
//	 * @See {@link #POWER_OPERATOR}
	 */
	POWER,

	/**
	 * The function returns the remainder division.
	 * 
	 * Equivalent to the modulo arithmetic operator (e.g. expr1 % expr2).
	 * 
	 * @see {@link #MODULUS_OPERATOR}
	 */
	MOD,

	/**
	 * The arithmetic remainder operator '%'.
	 * 
	 * @see {@link #MOD}
	 */
	MODULUS_OPERATOR("%"),

//	/**
//	 * The arithmetic unary plus (positive) operator '+'.
//	 */
//	PLUS_OPERATOR("+"),

	/**
	 * The arithmetic unary minus (negative) operator '-'.
	 */
	NEGATE_OPERATOR("-"),

	/**
	 * The arithmetic addition operator '+'.
	 */
	ADD_OPERATOR("+"),

	/**
	 * The arithmetic subtract operator '-'.
	 */
	SUBTRACT_OPERATOR("-"),

	/**
	 * The "IN" operator.
	 */
	IN_OPERATOR("IN"),

	/**
	 * The "BETWEEN" operator.
	 */
	BETWEEN_OPERATOR("BETWEEN"),

	/**
	 * The less-than operator '&lt;'.
	 */
	LESS_THAN_OPERATOR("<"),

	/**
	 * The greater-than operator '&gt;'.
	 */
	GREATER_THAN_OPERATOR(">"),

	/**
	 * The less-than-or-equal operator '&lt;='.
	 */
	LESS_THAN_OR_EQUAL_OPERATOR("<="),

	/**
	 * The greater-than-or-equal operator '&gt;='.
	 */
	GREATER_THAN_OR_EQUAL_OPERATOR(">="),

	/**
	 * The equals operator '='.
	 */
	EQUAL_OPERATOR("="),

	/**
	 * Compares whether two expressions are equal.
	 * 
	 * The function is NULL-safe, meaning it treats NULLs as known values for
	 * comparing equality. Note that this is different from the EQUAL comparison
	 * operator (=), which treats NULLs as unknown values.
	 */
	EQUAL_NULL,

	/**
	 * The not-equals operator "&lt;&gt;".
	 * 
	 *  @See {@link #LESS_THAN_OR_GREATER_THEN}
	 */
	NOT_EQUAL_OPERATOR("!="),

	/**
	 * The not-equals operator '!='
	 * 
	 * @See {@link #NOT_EQUAL_OPERATOR}
	 */
	LESS_THAN_OR_GREATER_THEN("<>"),
	
	/**
	 * The logical OR operator.
	 */
	LOGICAL_OR_OPERATOR("OR"),

	/**
	 * The logical XOR operator.
	 */
	LOGICAL_XOR_OPERATOR("XOR"),

	/**
	 * The logical AND operator.
	 */
	LOGICAL_AND_OPERATOR("AND"),

	/**
	 * The LIKE operator.
	 */
	LIKE_OPERATOR("LIKE"),

	/**
	 * The logical NOT operator.
	 */
	LOGICAL_NOT_OPERATOR("NOT"),

	/**
	 * The function NULLIF
	 */
	NULLIF,

	/**
	 * Single-level if-then-else expression. Similar to CASE, but only allows a single condition.
	 */
	IF,
	
	/**
	 * The IS NULL or <code>IS TRUE</code> operator.
	 */
	IS_OPERATOR("IS"),

	/**
	 * Returns the absolute (positive) value of the numeric value.
	 */
	ABS,

	/**
	 * Returns the arc cosine, the angle in radians whose cosine is the specified
	 * float expression.
	 */
	ACOS,

	/**
	 * Adds or subtracts a specified number of months to a date or timestamp
	 */
	ADD_MONTHS,

	ASIN,

	ATAN,

	ATAN2,
	
	/**
	 * Returns the values rounded to the nearest equal or larger integer.
	 */
	CEIL,

	/**
	 * The COALESCE function returns the first of its arguments that is not null.
	 * Null is returned only if all arguments are null.
	 */
	COALESCE,

	/**
	 * The IFNULL function replace the null with value.
	 */
	IFNULL,

	/**
	 * Returns the trigonometric cosine of the specified angle in radians in the specified number.
	 */
	COS,

	/**
	 * Returns the hyperbolic cosine of its argument.
	 */
	COSH,

	/**
	 * Returns the trigonometric cotangent of the angle in radians specified by
	 * float expression.
	 */
	COT,

	/**
	 * Compares the select expression to each search expression in order. As soon as a search expression matches the selection expression, the corresponding result expression is returned.
	 */
	DECODE,
	
	/**
	 * Function to converts radians to degrees.
	 */
	DEGREES,

	/**
	 * Returns the values rounded to the nearest equal or smaller integer.
	 */
	FLOOR,

	/**
	 * Returns the current date value.
	 */
	CURRENT_DATE,

	/** Returns the last day of the month. */
	LAST_DAY,

	/** The year of a date */
	YEAR,

	/** Quarter of the year (number from 1-4). */
	QUARTER,

	/** Month of the year (number from 1-12). */
	MONTH,

	/** Returns the name of the month (in English). */
	MONTH_NAME,

	/** Week of the year (number from 1-54). */
	WEEK_OF_YEAR,

	/** Returns the name of the weekday (in English). */
	DAY_NAME,

	/** Day of the month (number from 1-31). */
	DAY_OF_MONTH,

	/** Day of the week (Sunday=1 to Saturday=7). */
	DAY_OF_WEEK,

	/** Day of the year (number from 1-366). */
	DAY_OF_YEAR,

	/** Day of the week (Monday=1 to Sunday=7). */
	ISO_DAY_OF_WEEK,

	/** Week of the year (number from 1-53). */
	ISO_WEEK_OF_YEAR,

	/** 
	 * The hour (0-23).
	 * 
	 * @See {@link #MINUTE}, {@link #SECOND}
	 */
	HOUR,

	/** 
	 * The minute (0-59).
	 * 
	 * @See {@link #HOUR}, {@link #SECOND}
	 */
	MINUTE,

	/** 
	 * The second (0-59). 
	 * 
	 * @See {@link #HOUR}, {@link #MINUTE}
	 */
	SECOND,

	/**
	 * Returns the number of PI.
	 */
	PI,

	/**
	 * The function converts degrees to radians.
	 */
	RADIANS, RAND,

	/**
	 * Returns the values rounded to the nearest integer.
	 */
	ROUND,

	/**
	 * Returns the natural logarithm of a numeric value.
	 */
	LN,

	/**
	 * Returns the base 10 logarithm of a numeric value.
	 */
	LOG10,

	/**
	 * Returns the sign of a number.
	 */
	SIGN,

	/**
	 * Calculates the trigonometric sine of the angle in radians.
	 */
	SIN,

	/**
	 * Calculates the hyperbolic sine of its argument.
	 */
	SINH,

	/**
	 * Returns the cubic root of a numeric expression.
	 *
	 * @See {@link #SQRT}
	 */
	CBRT,

	/**
	 * Returns the square-root of a non-negative numeric expression.
	 * 
	 * @See {@link #CBRT}
	 */
	SQRT,

	/**
	 * Calculates the tangent of its argument, the argument should be expressed in
	 * radians.
	 */
	TAN,

	/**
	 * Calculates the hyperbolic tangent of its argument.
	 */
	TANH,

	/**
	 * The function decode string using the Java string literal encoding format.
	 * Special characters are \b, \t, \n, \f, \r, \", \\, \<octal>, \\u<unicode>.
	 */
	STRINGDECODE,
	/**
	 * The function encode special characters in a string using the Java string literal encoding format.
	 * Special characters are \b, \t, \n, \f, \r, \", \\, \<octal>, \\u<unicode>.
	 */
	STRINGENCODE,
	
	/**
	 * Returns a string that contains a phonetic representation of the input string.
	 */
	SOUNDEX,
	
	
	/**
	 * The function convert a string value to lower case.
	 * 
	 * @See {@link #LOWER}, {@link #INITCAP}
	 *
	 */
	UPPER,

	/**
	 * The function convert a string value to upper case.
	 * 
	 * @See {@link #INITCAP}, {@link #UPPER}
	 *
	 */
	LOWER,

	/**
	 * Returns a string with the first letter of each word in uppercase and the
	 * subsequent letters in lowercase.
	 * 
	 * @See {@link #LOWER}, {@link #UPPER}
	 */
	INITCAP,

	/**
	 * The function extracts a number of characters from a string (starting from
	 * left).
	 * 
	 * @See {@link #RIGHT}
	 */
	LEFT,

	/**
	 * The function extracts a number of characters from a string (starting from
	 * right).
	 * 
	 * @See {@link #LEFT}
	 */
	RIGHT,

	/**
	 * The function returns the number of characters of the specified string.
	 */
	LENGTH,

	/**
	 * The function calculate the MD5 hash of a data value. The hash will be
	 * returned as a 32 characters hex-encoded string.
	 * 
	 * @see {@link #SHA1}, {@link #SHA256}, {@link #SHA384}, {@link #SHA512}
	 */
	MD5,

	/**
	 * The function calculate the SHA-1 hash of a data value. The hash will be
	 * returned as a 40 characters hex-encoded string.
	 * 
	 * @see {@link #MD5}, {@link #SHA256}, {@link #SHA384}, {@link #SHA512}
	 */
	SHA1,

	/**
	 * The function calculate the SHA-256 hash of a data value. The hash will be
	 * returned as a 64 characters hex-encoded string.
	 * 
	 * @see {@link #MD5}, {@link #SHA1}, {@link #SHA384}, {@link #SHA512}
	 */
	SHA256,

	/**
	 * The function calculate the SHA-384 hash of a data value. The hash will be
	 * returned as a 96 characters hex-encoded string.
	 * 
	 * @see {@link #MD5}, {@link #SHA1}, {@link #SHA256}, {@link #SHA512}
	 */
	SHA384,

	/**
	 * The function calculate the SHA-512 hash of a data value. The hash will be
	 * returned as a 128 characters hex-encoded string.
	 * 
	 * @see {@link #MD5}, {@link #SHA1}, {@link #SHA256}, {@link #SHA384}
	 */
	SHA512,

	/**
	 * Converts the string or numeric expression to a boolean value.
	 */
	TO_BOOLEAN,
	
	TO_CHAR,

	/**
	 * Converts a string to a date value.
	 */
	TO_DATE,

	/**
	 * Converts a string to a number value.
	 */
	TO_NUMBER,

	/**
	 * The function return the ASCII value of the first character in a string. If
	 * the string is empty, a value of 0 is returned.
	 * 
	 */
	ASCII,

	/**
	 * The function return the Unicode code point for the first Unicode character in
	 * a string. If the string is empty, a value of 0 is returned.
	 * 
	 * @see {@link #CHR}, {@link #ASCII},
	 */
	UNICODE,

	/**
	 * The function converts a Unicode code point (including 7-bit ASCII) into the
	 * character that matches the input Unicode. If an invalid code point is
	 * specified, an error is returned.
	 * 
	 * @see {@link #ASCII}
	 */
	CHR,

	/**
	 * The function encode the string as a URL.
	 * 
	 * @see {@link #URLDECODE}
	 */
	URLENCODE,

	/**
	 * The function decode the URL to a string.
	 * 
	 * @see {@link #URLENCODE}
	 */
	URLDECODE,

	/**
	 * Returns the position in the string that is the first character of a specified
	 * occurrence of the substring.
	 */
	INSTR,

	/**
	 * The function removes leading and trailing characters from a string.
	 * 
	 * @see {@link #LTRIM}, {@link #RTRIM}
	 */
	TRIM,

	/**
	 * The function removes leading characters from a string.
	 * 
	 * @see {@link #TRIM}, {@link #RTRIM}
	 */
	LTRIM,

	/**
	 * The function removes leading characters from a string.
	 * 
	 * @see {@link #TRIM}, {@link #LTRIM}
	 */
	RTRIM,

	/**
	 * The function left-pads a string with another string, to a certain length.
	 * 
	 * @see {@link #RPAD}
	 */
	LPAD,

	/**
	 * The function right-pads a string with another string, to a certain length.
	 * 
	 * @see {@link #LPAD}
	 */
	RPAD,

	/**
	 * Returns a string consisting of a the specified number of blank spaces.
	 */
	SPACE,

	/**
	 * The function repeats a string as many times as specified.
	 */
	REPEAT,
	
	/**
	 * Removes all occurrences of a specified substring, and optionally replaces
	 * them with another string.
	 */
	REPLACE,

	/**
	 * The function reverses the order of characters in a string value, or of bytes
	 * in a binary value.
	 */
	REVERSE,

	/**
	 * Returns the portion of the string from string, startingfrom the
	 * character/byte specified by start, with optionally limited length.
	 */
	SUBSTR,

	/**
	 * Translates original from the characters in findChars to the characters in
	 * replaceChars.
	 */
	TRANSLATE,

	/**
	 * The function returns TRUE if the first value ends with second value. Both
	 * values must be data type of string or binary.
	 * 
	 * @see {@link #STARTSWITH}
	 */
	ENDSWITH,

	/**
	 * The function returns TRUE if the first value starts with second value. Both
	 * values must be data type string or binary.
	 * 
	 * @see {@link #ENDSWITH}
	 */
	STARTSWITH,

	/**
	 * The function returns the largest value that is not NULL, or NULL if all
	 * values are NULL.
	 * 
	 * @see {@link #LEAST}
	 */
	GREATEST,

	/**
	 * The function returns the smallest value that is not NULL, or NULL if all
	 * values are NULL.
	 * 
	 * @see {@link #GREATEST}
	 */
	LEAST;

	/**
	 * Category of comparison operators.
	 *
	 * <p>
	 * Consists of: {@link #IN_OPERATOR}, {@link #EQUAL_OPERATOR},
	 * {@link #NOT_EQUAL_OPERATOR}, {@link #LESS_THAN_OPERATOR},
	 * {@link #GREATER_THAN_OPERATOR}, {@link #LESS_THAN_OR_EQUAL_OPERATOR},
	 * {@link #GREATER_THAN_OR_EQUAL_OPERATOR}.
	 */
	public static final Set<Kind> COMPARISON = EnumSet.of(IN_OPERATOR, EQUAL_OPERATOR, NOT_EQUAL_OPERATOR,
			LESS_THAN_OPERATOR, GREATER_THAN_OPERATOR, GREATER_THAN_OR_EQUAL_OPERATOR, LESS_THAN_OR_EQUAL_OPERATOR);

	/**
	 * Returns whether this {@link Kind} belongs to a given category.
	 *
	 * @param category Category
	 * @return Whether this kind belongs to the given category
	 */
	public final boolean is(Collection<Kind> category) {
		return category.contains(this);
	}
	
	private final String source;

	Kind() {
		this.source = name();
	}
	
	Kind(final String source) {
		this.source = source;
	}
	
	@Override
	public String toString() {
		return source;
	}
	

}
