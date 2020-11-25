package org.apache.hop.expression;

import java.util.Collection;

/** Enumerates the possible types of {@link Expression}. */
public enum Kind {
  VALUE(OperatorCategory.None),

  LIST(OperatorCategory.None),

  IDENTIFIER(OperatorCategory.None),

  // -------------------------------------------------------------
  // BITWISE
  // -------------------------------------------------------------

  BITGET(OperatorCategory.Bitwise),

  /** The bitwise AND operation. */
  BITAND(OperatorCategory.Bitwise),

  /** The bitwise OR operation. */
  BITOR(OperatorCategory.Bitwise),

  /** The bitwise NOT operation. */
  BITNOT(OperatorCategory.Bitwise),

  /** The bitwise XOR operation. */
  BITXOR(OperatorCategory.Bitwise),

  // LSHIFT(Category.Bitwise),

  // RSHIFT(Category.Bitwise),

  // -------------------------------------------------------------
  // COMPARISON
  // -------------------------------------------------------------

  /** Contains function */
  CONTAINS(OperatorCategory.Comparison),

  /** The "IN" operator. */
  IN(OperatorCategory.Comparison),

  /** The "BETWEEN" operator. */
  BETWEEN(OperatorCategory.Comparison),

  /** The less-than operator '&lt;'. */
  LESS_THAN(OperatorCategory.Comparison),

  /** The greater-than operator '&gt;'. */
  GREATER_THAN(OperatorCategory.Comparison),

  /** The less-than-or-equal operator '&lt;='. */
  LESS_THAN_OR_EQUAL(OperatorCategory.Comparison),

  /** The greater-than-or-equal operator '&gt;='. */
  GREATER_THAN_OR_EQUAL(OperatorCategory.Comparison),

  /** The equals operator '='. */
  EQUAL(OperatorCategory.Comparison),

  /**
   * Compares whether two expressions are equal.
   *
   * <p>The function is NULL-safe, meaning it treats NULLs as known values for comparing equality.
   * Note that this is different from the EQUAL comparison operator (=), which treats NULLs as
   * unknown values.
   */
  EQUAL_NULL(OperatorCategory.Comparison),

  /** The not-equals operator "&lt;&gt;". @See {@link #LESS_THAN_OR_GREATER_THEN} */
  NOT_EQUALS(OperatorCategory.Comparison),

  /** The not-equals operator '!=' @See {@link #NOT_EQUAL_OPERATOR} */
  LESS_THAN_OR_GREATER_THEN(OperatorCategory.Comparison),

  /** The IS NULL or <code>IS TRUE</code> operator. */
  IS(OperatorCategory.Comparison),

  /** The LIKE operator. */
  LIKE(OperatorCategory.Comparison),

  /** The ILIKE case-insensitive operator. */
  ILIKE(OperatorCategory.Comparison),

  REGEXP_LIKE(OperatorCategory.Comparison),

  /**
   * The function returns TRUE if the first value starts with second value. Both values must be data
   * type string or binary.
   *
   * @see {@link #ENDSWITH}
   */
  STARTSWITH(OperatorCategory.Comparison),

  /**
   * The function returns TRUE if the first value ends with second value. Both values must be data
   * type of string or binary.
   *
   * @see {@link #STARTSWITH}
   */
  ENDSWITH(OperatorCategory.Comparison),

  // -------------------------------------------------------------
  // CONDITIONAL
  // -------------------------------------------------------------

  /** Case when operator */
  CASE_WHEN(OperatorCategory.Conditional),

  /**
   * The COALESCE function returns the first of its arguments that is not null. Null is returned
   * only if all arguments are null.
   */
  COALESCE(OperatorCategory.Conditional),

  /** Single-level if-then-else expression. Similar to CASE, but only allows a single condition. */
  IF(OperatorCategory.Conditional),

  /** The IFNULL function replace the null with value (Alias NVL). */
  IFNULL(OperatorCategory.Conditional),

  /** The function NULLIF */
  NULLIF(OperatorCategory.Conditional),

  NVL2(OperatorCategory.Conditional),

  /**
   * The function returns the largest value that is not NULL, or NULL if all values are NULL.
   *
   * @see {@link #LEAST}
   */
  GREATEST(OperatorCategory.Conditional),

  /**
   * The function returns the smallest value that is not NULL, or NULL if all values are NULL.
   *
   * @see {@link #GREATEST}
   */
  LEAST(OperatorCategory.Conditional),

  // -------------------------------------------------------------
  // LOGICAL
  // -------------------------------------------------------------

  /** The logical AND operator. */
  LOGICAL_AND(OperatorCategory.Logical),

  /** The logical NOT operator. */
  LOGICAL_NOT(OperatorCategory.Logical),

  /** The logical OR operator. */
  LOGICAL_OR(OperatorCategory.Logical),

  /** The logical XOR operator. */
  LOGICAL_XOR(OperatorCategory.Logical),

  // -------------------------------------------------------------
  // STRING
  // -------------------------------------------------------------

  /** String concatenation operator '<code>||</code>'. @See {@link #CONCAT} */
  CONCAT(OperatorCategory.String),

  /**
   * Compares the select expression to each search expression in order. As soon as a search
   * expression matches the selection expression, the corresponding result expression is returned.
   */
  DECODE(OperatorCategory.String),

  /**
   * The function decode string using the Java string literal encoding format. Special characters
   * are \b, \t, \n, \f, \r, \", \\, \<octal>, \\u<unicode>.
   */
  STRINGDECODE(OperatorCategory.String),
  /**
   * The function encode special characters in a string using the Java string literal encoding
   * format. Special characters are \b, \t, \n, \f, \r, \", \\, \<octal>, \\u<unicode>.
   */
  STRINGENCODE(OperatorCategory.String),

  /** Returns a string that contains a phonetic representation of the input string. */
  SOUNDEX(OperatorCategory.String),

  /** The function convert a string value to lower case. @See {@link #LOWER}, {@link #INITCAP} */
  UPPER(OperatorCategory.String),

  /** The function convert a string value to upper case. @See {@link #INITCAP}, {@link #UPPER} */
  LOWER(OperatorCategory.String),

  /**
   * Returns a string with the first letter of each word in uppercase and the subsequent letters in
   * lowercase. @See {@link #LOWER}, {@link #UPPER}
   */
  INITCAP(OperatorCategory.String),

  /**
   * The function extracts a number of characters from a string (starting from left). @See {@link
   * #RIGHT}
   */
  LEFT(OperatorCategory.String),

  /**
   * The function extracts a number of characters from a string (starting from right). @See {@link
   * #LEFT}
   */
  RIGHT(OperatorCategory.String),

  /** The function returns the number of characters of the specified string. */
  LENGTH(OperatorCategory.String),

  /**
   * The function return the ASCII value of the first character in a string. If the string is empty,
   * a value of 0 is returned.
   */
  ASCII(OperatorCategory.String),

  /**
   * The function return the Unicode code point for the first Unicode character in a string. If the
   * string is empty, a value of 0 is returned.
   *
   * @see {@link #CHR}, {@link #ASCII},
   */
  UNICODE(OperatorCategory.String),

  /**
   * The function converts a Unicode code point (including 7-bit ASCII) into the character that
   * matches the input Unicode. If an invalid code point is specified, an error is returned.
   *
   * @see {@link #ASCII}
   */
  CHR(OperatorCategory.String),

  /**
   * The function encode the string as a URL.
   *
   * @see {@link #URLDECODE}
   */
  URLENCODE(OperatorCategory.String),

  /**
   * The function decode the URL to a string.
   *
   * @see {@link #URLENCODE}
   */
  URLDECODE(OperatorCategory.String),

  /**
   * Returns the position in the string that is the first character of a specified occurrence of the
   * substring.
   */
  INSTR(OperatorCategory.String),

  /**
   * The function removes leading and trailing characters from a string.
   *
   * @see {@link #LTRIM}, {@link #RTRIM}
   */
  TRIM(OperatorCategory.String),

  /**
   * The function removes leading characters from a string.
   *
   * @see {@link #TRIM}, {@link #RTRIM}
   */
  LTRIM(OperatorCategory.String),

  /**
   * The function removes leading characters from a string.
   *
   * @see {@link #TRIM}, {@link #LTRIM}
   */
  RTRIM(OperatorCategory.String),

  /**
   * The function left-pads a string with another string, to a certain length.
   *
   * @see {@link #RPAD}
   */
  LPAD(OperatorCategory.String),

  /**
   * The function right-pads a string with another string, to a certain length.
   *
   * @see {@link #LPAD}
   */
  RPAD(OperatorCategory.String),

  /** Returns a string consisting of a the specified number of blank spaces. */
  SPACE(OperatorCategory.String),

  /** The function repeats a string as many times as specified. */
  REPEAT(OperatorCategory.String),

  /**
   * Removes all occurrences of a specified substring, and optionally replaces them with another
   * string.
   */
  REPLACE(OperatorCategory.String),

  /**
   * The function reverses the order of characters in a string value, or of bytes in a binary value.
   */
  REVERSE(OperatorCategory.String),

  /**
   * Returns the portion of the string from string, startingfrom the character/byte specified by
   * start, with optionally limited length.
   */
  SUBSTRING(OperatorCategory.String),

  /** Translates original from the characters in findChars to the characters in replaceChars. */
  TRANSLATE(OperatorCategory.String),

  // -------------------------------------------------------------
  // MATHEMATICAL
  // -------------------------------------------------------------

  /** The arithmetic division operator '/'. */
  DIVIDE(OperatorCategory.Mathematical),

  /** The arithmetic multiplication operator '*'. */
  MULTIPLY(OperatorCategory.Mathematical),

  /** Returns the exponential value of a numeric expression. */
  EXP(OperatorCategory.Mathematical),

  /** The arithmetic power operator '**' or function. */
  POWER(OperatorCategory.Mathematical),

  /** The arithmetic remainder operator '%'. The function returns the remainder division. */
  MOD(OperatorCategory.Mathematical),

  //	/**
  //	 * The arithmetic unary plus (positive) operator '+'.
  //	 */
  //	POSITIVE(Category.Arithmetic),

  /** The arithmetic unary minus (negative) operator '-'. */
  NEGATIVE(OperatorCategory.Mathematical),

  /** The arithmetic addition operator '+'. */
  ADD(OperatorCategory.Mathematical),

  /** The arithmetic subtract operator '-'. */
  SUBTRACT(OperatorCategory.Mathematical),

  /** Returns the absolute (positive) value of the numeric value. */
  ABS(OperatorCategory.Mathematical),

  /** Returns the values rounded to the nearest equal or larger integer. */
  CEIL(OperatorCategory.Mathematical),

  /** Function to converts radians to degrees. */
  DEGREES(OperatorCategory.Mathematical),

  /** Returns the values rounded to the nearest equal or smaller integer. */
  FLOOR(OperatorCategory.Mathematical),

  /** Returns the number of PI. */
  PI(OperatorCategory.Mathematical),

  /** The function converts degrees to radians. */
  RADIANS(OperatorCategory.Mathematical),

  RAND(OperatorCategory.Mathematical),

  /** Returns the values rounded to the nearest integer. */
  ROUND(OperatorCategory.Mathematical),

  /** Returns the natural logarithm of a numeric value. */
  LN(OperatorCategory.Mathematical),

  /** Returns the specified base logarithm of a numeric value. */
  LOG(OperatorCategory.Mathematical),

  /** Returns the base 10 logarithm of a numeric value. */
  LOG10(OperatorCategory.Mathematical),

  /** Returns the sign of a number. */
  SIGN(OperatorCategory.Mathematical),

  /** Returns the cubic root of a numeric expression. @See {@link #SQRT} */
  CBRT(OperatorCategory.Mathematical),

  /** Returns the square-root of a non-negative numeric expression. @See {@link #CBRT} */
  SQRT(OperatorCategory.Mathematical),

  /** Round down numeric expressions or truncates a date or timestamp to the specified part. */
  TRUNCATE(OperatorCategory.Mathematical),

  // -------------------------------------------------------------
  // TRIGONOMETRY
  // -------------------------------------------------------------

  /**
   * Returns the arc cosine, the angle in radians whose cosine is the specified float expression.
   */
  ACOS(OperatorCategory.Trigonometry),
  ACOSH(OperatorCategory.Trigonometry),

  ASIN(OperatorCategory.Trigonometry),
  ASINH(OperatorCategory.Trigonometry),

  ATAN(OperatorCategory.Trigonometry),
  ATANH(OperatorCategory.Trigonometry),

  ATAN2(OperatorCategory.Trigonometry),

  /** Returns the trigonometric cosine of the specified angle in radians in the specified number. */
  COS(OperatorCategory.Trigonometry),

  /** Returns the hyperbolic cosine of its argument. */
  COSH(OperatorCategory.Trigonometry),

  /** Returns the trigonometric cotangent of the angle in radians specified by float expression. */
  COT(OperatorCategory.Trigonometry),

  /** Calculates the trigonometric sine of the angle in radians. */
  SIN(OperatorCategory.Trigonometry),

  /** Calculates the hyperbolic sine of its argument. */
  SINH(OperatorCategory.Trigonometry),

  /** Calculates the tangent of its argument, the argument should be expressed in radians. */
  TAN(OperatorCategory.Trigonometry),

  /** Calculates the hyperbolic tangent of its argument. */
  TANH(OperatorCategory.Trigonometry),

  // -------------------------------------------------------------
  // DATE AND TIME
  // -------------------------------------------------------------

  /** Adds or subtracts a specified number of days to a date or timestamp */
  ADD_DAYS(OperatorCategory.Date),

  /** Adds or subtracts a specified number of hours to a date or timestamp */
  ADD_HOURS(OperatorCategory.Date),

  /** Adds or subtracts a specified number of minutes to a date or timestamp */
  ADD_MINUTES(OperatorCategory.Date),

  /** Adds or subtracts a specified number of months to a date or timestamp */
  ADD_MONTHS(OperatorCategory.Date),

  /** Adds or subtracts a specified number of seconds to a date or timestamp */
  ADD_SECONDS(OperatorCategory.Date),

  /** Adds or subtracts a specified number of weeks to a date or timestamp */
  ADD_WEEKS(OperatorCategory.Date),

  /** Adds or subtracts a specified number of years to a date or timestamp */
  ADD_YEARS(OperatorCategory.Date),

  /** DATE function */
  DATE(OperatorCategory.Date),

  /** Returns the current date value. */
  CURRENT_DATE(OperatorCategory.Date),

  /** Returns the first day of the month. */
  FIRST_DAY(OperatorCategory.Date),

  /** Returns the last day of the month. */
  LAST_DAY(OperatorCategory.Date),

  /** Returns the date of the first specified day of week that occurs after the input date. */
  NEXT_DAY(OperatorCategory.Date),

  /** The year of a date */
  YEAR(OperatorCategory.Date),

  /** Quarter of the year (number from 1-4). */
  QUARTER(OperatorCategory.Date),

  /** Month of the year (number from 1-12). */
  MONTH(OperatorCategory.Date),

  /** Returns the name of the month (in English). */
  MONTHNAME(OperatorCategory.Date),

  /** Week of the year (number from 1-54). */
  WEEK(OperatorCategory.Date),

  /** Week from the beginning of the month (0-5) */
  WEEKOFMONTH(OperatorCategory.Date),

  /** Returns the name of the weekday (in English). */
  DAYNAME(OperatorCategory.Date),

  /** Day of the month (number from 1-31). */
  DAY(OperatorCategory.Date),

  /** Day of the year (number from 1-366). */
  DAYOFYEAR(OperatorCategory.Date),

  /** Day of the week (Sunday=1 to Saturday=7). */
  DAYOFWEEK(OperatorCategory.Date),

  /** Day of the week (Monday=1 to Sunday=7). */
  DAYOFWEEK_ISO(OperatorCategory.Date),

  /** Week of the year (number from 1-53). */
  WEEK_ISO(OperatorCategory.Date),

  /** The hour (0-23). @See {@link #MINUTE}, {@link #SECOND} */
  HOUR(OperatorCategory.Date),

  /** The minute (0-59). @See {@link #HOUR}, {@link #SECOND} */
  MINUTE(OperatorCategory.Date),

  /** The second (0-59). @See {@link #HOUR}, {@link #MINUTE} */
  SECOND(OperatorCategory.Date),

  /** Returns number of days between two date values. */
  DAYS_BETWEEN(OperatorCategory.Date),

  /** Returns number of months between two date. */
  MONTHS_BETWEEN(OperatorCategory.Date),

  /** Returns number of years between two date. */
  YEARS_BETWEEN(OperatorCategory.Date),
  /** Return the number of minutes between two timestamps */
  MINUTES_BETWEEN(OperatorCategory.Date),

  /** Return the number of hours between two timestamps */
  HOURS_BETWEEN(OperatorCategory.Date),

  /** Return the number of seconds between two timestamps */
  SECONDS_BETWEEN(OperatorCategory.Date),

  /**
   * Function to extract date part: DECADE | YEAR | MONTH | WEEK | DAY | HOUR | MINUTE | SECOND...
   */
  EXTRACT(OperatorCategory.Date),

  // -------------------------------------------------------------
  // CONVERTION
  // -------------------------------------------------------------

  /**
   * Converts a value of one data type into another data type '<code>
   * CAST(value AS type FORMAT format)</code>'.
   */
  CAST(OperatorCategory.Conversion),

  /** Converts a string or numeric expression to a boolean value. */
  TO_BOOLEAN(OperatorCategory.Conversion),

  /** Converts a numeric or date expression to a string value. */
  TO_CHAR(OperatorCategory.Conversion),

  /** Converts a string expression to a date value. */
  TO_DATE(OperatorCategory.Conversion),

  /** Converts a string expression to a number value. */
  TO_NUMBER(OperatorCategory.Conversion),

  // -------------------------------------------------------------
  // CRYPTOGRAPHIC
  // -------------------------------------------------------------

  /**
   * The function calculate the MD5 hash of a data value. The hash will be returned as a 32
   * characters hex-encoded string.
   *
   * @see {@link #SHA1}, {@link #SHA256}, {@link #SHA384}, {@link #SHA512}
   */
  MD5(OperatorCategory.Cryptographic),

  /**
   * The function calculate the SHA-1 hash of a data value. The hash will be returned as a 40
   * characters hex-encoded string.
   *
   * @see {@link #MD5}, {@link #SHA256}, {@link #SHA384}, {@link #SHA512}
   */
  SHA1(OperatorCategory.Cryptographic),

  /**
   * The function calculate the SHA-256 hash of a data value. The hash will be returned as a 64
   * characters hex-encoded string.
   *
   * @see {@link #MD5}, {@link #SHA1}, {@link #SHA384}, {@link #SHA512}
   */
  SHA256(OperatorCategory.Cryptographic),

  /**
   * The function calculate the SHA-384 hash of a data value. The hash will be returned as a 96
   * characters hex-encoded string.
   *
   * @see {@link #MD5}, {@link #SHA1}, {@link #SHA256}, {@link #SHA512}
   */
  SHA384(OperatorCategory.Cryptographic),

  /**
   * The function calculate the SHA-512 hash of a data value. The hash will be returned as a 128
   * characters hex-encoded string.
   *
   * @see {@link #MD5}, {@link #SHA1}, {@link #SHA256}, {@link #SHA384}
   */
  SHA512(OperatorCategory.Cryptographic);

  /**
   * Returns whether this {@link Kind} belongs to a given category.
   *
   * @param category Category
   * @return Whether this kind belongs to the given category
   */
  public final boolean is(Collection<Kind> collection) {
    return collection.contains(this);
  }

  private final OperatorCategory category;

  Kind(final OperatorCategory category) {
    this.category = category;
  }

  public String category() {
    return category.name();
  }
}
