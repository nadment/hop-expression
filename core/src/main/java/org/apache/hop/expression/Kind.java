/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression;

/** Enumerates the possible types of {@link IExpression}. */
public enum Kind {
  VALUE(Category.None),

  LIST(Category.None),

  IDENTIFIER(Category.None),

  // -------------------------------------------------------------
  // BITWISE
  // -------------------------------------------------------------

  BITGET(Category.Bitwise),

  /** The bitwise AND operation. */
  BITAND(Category.Bitwise),

  /** The bitwise OR operation. */
  BITOR(Category.Bitwise),

  /** The bitwise NOT operation. */
  BITNOT(Category.Bitwise),

  /** The bitwise XOR operation. */
  BITXOR(Category.Bitwise),

  // LSHIFT(Category.Bitwise),

  // RSHIFT(Category.Bitwise),

  // -------------------------------------------------------------
  // COMPARISON
  // -------------------------------------------------------------

  /** Contains function */
  CONTAINS(Category.Comparison),

  /** The "IN" operator. */
  IN(Category.Comparison),

  /** The "BETWEEN" operator. */
  BETWEEN(Category.Comparison),

  /** The less-than operator '&lt;'. */
  LESS_THAN(Category.Comparison),

  /** The greater-than operator '&gt;'. */
  GREATER_THAN(Category.Comparison),

  /** The less-than-or-equal operator '&lt;='. */
  LESS_THAN_OR_EQUAL(Category.Comparison),

  /** The greater-than-or-equal operator '&gt;='. */
  GREATER_THAN_OR_EQUAL(Category.Comparison),

  /** The equals operator '='. */
  EQUAL(Category.Comparison),

  /**
   * Compares whether two expressions are equal.
   *
   * <p>The function is NULL-safe, meaning it treats NULLs as known values for comparing equality.
   * Note that this is different from the EQUAL comparison operator (=), which treats NULLs as
   * unknown values.
   */
  EQUAL_NULL(Category.Comparison),

  /** The not-equals operator "&lt;&gt;". @See {@link #LESS_THAN_OR_GREATER_THEN} */
  NOT_EQUALS(Category.Comparison),

  /** The not-equals operator '!=' @See {@link #NOT_EQUAL_OPERATOR} */
  LESS_THAN_OR_GREATER_THEN(Category.Comparison),

  /** The IS NULL or <code>IS TRUE</code> operator. */
  IS(Category.Comparison),

  /** The LIKE operator. */
  LIKE(Category.Comparison),

  /** The ILIKE case-insensitive operator. */
  ILIKE(Category.Comparison),

  REGEXP_LIKE(Category.Comparison),

  /**
   * The function returns TRUE if the first value starts with second value. Both values must be data
   * type string or binary.
   *
   * @see {@link #ENDSWITH}
   */
  STARTSWITH(Category.Comparison),

  /**
   * The function returns TRUE if the first value ends with second value. Both values must be data
   * type of string or binary.
   *
   * @see {@link #STARTSWITH}
   */
  ENDSWITH(Category.Comparison),

  // -------------------------------------------------------------
  // CONDITIONAL
  // -------------------------------------------------------------

  /** Case when operator */
  CASE_WHEN(Category.Conditional),

  /**
   * The COALESCE function returns the first of its arguments that is not null. Null is returned
   * only if all arguments are null.
   */
  COALESCE(Category.Conditional),

  /** Single-level if-then-else expression. Similar to CASE, but only allows a single condition. */
  IF(Category.Conditional),

  /** The IFNULL function replace the null with value (Alias NVL). */
  IFNULL(Category.Conditional),

  /** The function NULLIF */
  NULLIF(Category.Conditional),

  NVL2(Category.Conditional),

  /**
   * The function returns the largest value that is not NULL, or NULL if all values are NULL.
   *
   * @see {@link #LEAST}
   */
  GREATEST(Category.Conditional),

  /**
   * The function returns the smallest value that is not NULL, or NULL if all values are NULL.
   *
   * @see {@link #GREATEST}
   */
  LEAST(Category.Conditional),

  // -------------------------------------------------------------
  // LOGICAL
  // -------------------------------------------------------------

  /** The logical <code>AND</code> operator. */
  LOGICAL_AND(Category.Logical),

  /** The logical <code>NOT</code> operator. */
  LOGICAL_NOT(Category.Logical),

  /** The logical <code>OR</code> operator. */
  LOGICAL_OR(Category.Logical),

  /** The logical <code>XOR</code> operator. */
  LOGICAL_XOR(Category.Logical),

  // -------------------------------------------------------------
  // STRING
  // -------------------------------------------------------------

  /** String concatenation operator '<code>||</code>'. @See {@link #CONCAT} */
  CONCAT(Category.String),

  /**
   * Compares the select expression to each search expression in order. As soon as a search
   * expression matches the selection expression, the corresponding result expression is returned.
   */
  DECODE(Category.String),

  /**
   * The function decode string using the Java string literal encoding format. Special characters
   * are \b, \t, \n, \f, \r, \", \\, \<octal>, \\u<unicode>.
   */
  STRINGDECODE(Category.String),
  /**
   * The function encode special characters in a string using the Java string literal encoding
   * format. Special characters are \b, \t, \n, \f, \r, \", \\, \<octal>, \\u<unicode>.
   */
  STRINGENCODE(Category.String),

  /** Returns a string that contains a phonetic representation of the input string. */
  SOUNDEX(Category.String),

  /** The function convert a string value to lower case. @See {@link #LOWER}, {@link #INITCAP} */
  UPPER(Category.String),

  /** The function convert a string value to upper case. @See {@link #INITCAP}, {@link #UPPER} */
  LOWER(Category.String),

  /**
   * Returns a string with the first letter of each word in uppercase and the subsequent letters in
   * lowercase. @See {@link #LOWER}, {@link #UPPER}
   */
  INITCAP(Category.String),

  /**
   * The function extracts a number of characters from a string (starting from left). @See {@link
   * #RIGHT}
   */
  LEFT(Category.String),

  /**
   * The function extracts a number of characters from a string (starting from right). @See {@link
   * #LEFT}
   */
  RIGHT(Category.String),

  /** The function returns the number of characters of the specified string. */
  LENGTH(Category.String),

  /**
   * The function return the ASCII value of the first character in a string. If the string is empty,
   * a value of 0 is returned.
   */
  ASCII(Category.String),

  /**
   * The function return the Unicode code point for the first Unicode character in a string. If the
   * string is empty, a value of 0 is returned.
   *
   * @see {@link #CHR}, {@link #ASCII},
   */
  UNICODE(Category.String),

  /**
   * The function converts a Unicode code point (including 7-bit ASCII) into the character that
   * matches the input Unicode. If an invalid code point is specified, an error is returned.
   *
   * @see {@link #ASCII}
   */
  CHR(Category.String),

  /**
   * The function encode the string as a URL.
   *
   * @see {@link #URLDECODE}
   */
  URLENCODE(Category.String),

  /**
   * The function decode the URL to a string.
   *
   * @see {@link #URLENCODE}
   */
  URLDECODE(Category.String),

  /**
   * Returns the position in the string that is the first character of a specified occurrence of the
   * substring.
   */
  INSTR(Category.String),

  /**
   * The function removes leading and trailing characters from a string.
   *
   * @see {@link #LTRIM}, {@link #RTRIM}
   */
  TRIM(Category.String),

  /**
   * The function removes leading characters from a string.
   *
   * @see {@link #TRIM}, {@link #RTRIM}
   */
  LTRIM(Category.String),

  /**
   * The function removes leading characters from a string.
   *
   * @see {@link #TRIM}, {@link #LTRIM}
   */
  RTRIM(Category.String),

  /**
   * The function left-pads a string with another string, to a certain length.
   *
   * @see {@link #RPAD}
   */
  LPAD(Category.String),

  /**
   * The function right-pads a string with another string, to a certain length.
   *
   * @see {@link #LPAD}
   */
  RPAD(Category.String),

  /** Returns a string consisting of a the specified number of blank spaces. */
  SPACE(Category.String),

  /** The function repeats a string as many times as specified. */
  REPEAT(Category.String),

  /**
   * Removes all occurrences of a specified substring, and optionally replaces them with another
   * string.
   */
  REPLACE(Category.String),

  /**
   * The function reverses the order of characters in a string value, or of bytes in a binary value.
   */
  REVERSE(Category.String),

  /**
   * Returns the portion of the string from string, startingfrom the character/byte specified by
   * start, with optionally limited length.
   */
  SUBSTRING(Category.String),

  /** Translates original from the characters in findChars to the characters in replaceChars. */
  TRANSLATE(Category.String),

  // -------------------------------------------------------------
  // MATHEMATICAL
  // -------------------------------------------------------------

  /** The arithmetic division operator '/'. */
  DIVIDE(Category.Mathematical),

  /** The arithmetic multiplication operator '*'. */
  MULTIPLY(Category.Mathematical),

  /** Returns the exponential value of a numeric expression. */
  EXP(Category.Mathematical),

  /** The arithmetic power operator '**' or function. */
  POWER(Category.Mathematical),

  /** The arithmetic remainder operator '%'. The function returns the remainder division. */
  MOD(Category.Mathematical),

  //	/**
  //	 * The arithmetic unary plus (positive) operator '+'.
  //	 */
  //	POSITIVE(Category.Arithmetic),

  /** The arithmetic unary minus (negative) operator '-'. */
  NEGATIVE(Category.Mathematical),

  /** The arithmetic addition operator '+'. */
  ADD(Category.Mathematical),

  /** The arithmetic subtract operator '-'. */
  SUBTRACT(Category.Mathematical),

  /** Returns the absolute (positive) value of the numeric value. */
  ABS(Category.Mathematical),

  /** Returns the values rounded to the nearest equal or larger integer. */
  CEIL(Category.Mathematical),

  /** Function to converts radians to degrees. */
  DEGREES(Category.Mathematical),

  /** Returns the values rounded to the nearest equal or smaller integer. */
  FLOOR(Category.Mathematical),

  /** Returns the number of PI. */
  PI(Category.Mathematical),

  /** The function converts degrees to radians. */
  RADIANS(Category.Mathematical),

  RAND(Category.Mathematical),

  /** Returns the values rounded to the nearest integer. */
  ROUND(Category.Mathematical),

  /** Returns the natural logarithm of a numeric value. */
  LN(Category.Mathematical),

  /** Returns the specified base logarithm of a numeric value. */
  LOG(Category.Mathematical),

  /** Returns the base 10 logarithm of a numeric value. */
  LOG10(Category.Mathematical),

  /** Returns the sign of a number. */
  SIGN(Category.Mathematical),

  /** Returns the cubic root of a numeric expression. @See {@link #SQRT} */
  CBRT(Category.Mathematical),

  /** Returns the square-root of a non-negative numeric expression. @See {@link #CBRT} */
  SQRT(Category.Mathematical),

  /** Round down numeric expressions or truncates a date or timestamp to the specified part. */
  TRUNCATE(Category.Mathematical),

  // -------------------------------------------------------------
  // TRIGONOMETRY
  // -------------------------------------------------------------

  /**
   * Returns the arc cosine, the angle in radians whose cosine is the specified float expression.
   */
  ACOS(Category.Trigonometry),
  ACOSH(Category.Trigonometry),

  ASIN(Category.Trigonometry),
  ASINH(Category.Trigonometry),

  ATAN(Category.Trigonometry),
  ATANH(Category.Trigonometry),

  ATAN2(Category.Trigonometry),

  /** Returns the trigonometric cosine of the specified angle in radians in the specified number. */
  COS(Category.Trigonometry),

  /** Returns the hyperbolic cosine of its argument. */
  COSH(Category.Trigonometry),

  /** Returns the trigonometric cotangent of the angle in radians specified by float expression. */
  COT(Category.Trigonometry),

  /** Calculates the trigonometric sine of the angle in radians. */
  SIN(Category.Trigonometry),

  /** Calculates the hyperbolic sine of its argument. */
  SINH(Category.Trigonometry),

  /** Calculates the tangent of its argument, the argument should be expressed in radians. */
  TAN(Category.Trigonometry),

  /** Calculates the hyperbolic tangent of its argument. */
  TANH(Category.Trigonometry),

  // -------------------------------------------------------------
  // DATE AND TIME
  // -------------------------------------------------------------

  /** Adds or subtracts a specified number of days to a date or timestamp */
  ADD_DAYS(Category.Date),

  /** Adds or subtracts a specified number of hours to a date or timestamp */
  ADD_HOURS(Category.Date),

  /** Adds or subtracts a specified number of minutes to a date or timestamp */
  ADD_MINUTES(Category.Date),

  /** Adds or subtracts a specified number of months to a date or timestamp */
  ADD_MONTHS(Category.Date),

  /** Adds or subtracts a specified number of seconds to a date or timestamp */
  ADD_SECONDS(Category.Date),

  /** Adds or subtracts a specified number of weeks to a date or timestamp */
  ADD_WEEKS(Category.Date),

  /** Adds or subtracts a specified number of years to a date or timestamp */
  ADD_YEARS(Category.Date),

  /** DATE function */
  DATE(Category.Date),

  /** Returns the current date value. */
  CURRENT_DATE(Category.Date),

  /** Returns the first day of the month. */
  FIRST_DAY(Category.Date),

  /** Returns the last day of the month. */
  LAST_DAY(Category.Date),

  /** Returns the date of the first specified day of week that occurs after the input date. */
  NEXT_DAY(Category.Date),

  /** Returns the date of the first specified day of week that occurs before the input date. */
  PREVIOUS_DAY(Category.Date),
  
  /** The year of a date */
  YEAR(Category.Date),

  /** Quarter of the year (number from 1-4). */
  QUARTER(Category.Date),

  /** Month of the year (number from 1-12). */
  MONTH(Category.Date),

  /** Returns the name of the month (in English). */
  MONTHNAME(Category.Date),

  /** Week of the year (number from 1-54). */
  WEEK(Category.Date),

  /** Week from the beginning of the month (0-5) */
  WEEKOFMONTH(Category.Date),

  /** Returns the name of the weekday (in English). */
  DAYNAME(Category.Date),

  /** Day of the month (number from 1-31). */
  DAY(Category.Date),

  /** Day of the year (number from 1-366). */
  DAYOFYEAR(Category.Date),

  /** Day of the week (Sunday=1 to Saturday=7). */
  DAYOFWEEK(Category.Date),

  /** Day of the week (Monday=1 to Sunday=7). */
  DAYOFWEEK_ISO(Category.Date),

  /** Week of the year (number from 1-53). */
  WEEK_ISO(Category.Date),

  /** The hour (0-23). @See {@link #MINUTE}, {@link #SECOND} */
  HOUR(Category.Date),

  /** The minute (0-59). @See {@link #HOUR}, {@link #SECOND} */
  MINUTE(Category.Date),

  /** The second (0-59). @See {@link #HOUR}, {@link #MINUTE} */
  SECOND(Category.Date),

  /** Returns number of days between two date values. */
  DAYS_BETWEEN(Category.Date),

  /** Returns number of months between two date. */
  MONTHS_BETWEEN(Category.Date),

  /** Returns number of years between two date. */
  YEARS_BETWEEN(Category.Date),
  /** Return the number of minutes between two timestamps */
  MINUTES_BETWEEN(Category.Date),

  /** Return the number of hours between two timestamps */
  HOURS_BETWEEN(Category.Date),

  /** Return the number of seconds between two timestamps */
  SECONDS_BETWEEN(Category.Date),

  /**
   * Function to extract date part: DECADE | YEAR | MONTH | WEEK | DAY | HOUR | MINUTE | SECOND...
   */
  EXTRACT(Category.Date),

  // -------------------------------------------------------------
  // CONVERTION
  // -------------------------------------------------------------

  /**
   * Converts a value of one data type into another data type <code>::</code> or <code>
   * CAST(value AS type FORMAT format)</code>.
   */
  CAST(Category.Conversion),

  /** Converts a string or numeric expression to a boolean value. */
  TO_BOOLEAN(Category.Conversion),
  TRY_TO_BOOLEAN(Category.Conversion),
  
  /** Converts a numeric or date expression to a string value. */
  TO_CHAR(Category.Conversion),

  /** Converts a string expression to a date value. */
  TO_DATE(Category.Conversion),
  TRY_TO_DATE(Category.Conversion),
  
  /** Converts a string expression to a number value. */
  TO_NUMBER(Category.Conversion),
  TRY_TO_NUMBER(Category.Conversion),

  // -------------------------------------------------------------
  // CRYPTOGRAPHIC
  // -------------------------------------------------------------

  /**
   * The function calculate the MD5 hash of a data value. The hash will be returned as a 32
   * characters hex-encoded string.
   *
   * @see {@link #SHA1}, {@link #SHA256}, {@link #SHA384}, {@link #SHA512}
   */
  MD5(Category.Cryptographic),

  /**
   * The function calculate the SHA-1 hash of a data value. The hash will be returned as a 40
   * characters hex-encoded string.
   *
   * @see {@link #MD5}, {@link #SHA256}, {@link #SHA384}, {@link #SHA512}
   */
  SHA1(Category.Cryptographic),

  /**
   * The function calculate the SHA-256 hash of a data value. The hash will be returned as a 64
   * characters hex-encoded string.
   *
   * @see {@link #MD5}, {@link #SHA1}, {@link #SHA384}, {@link #SHA512}
   */
  SHA256(Category.Cryptographic),

  /**
   * The function calculate the SHA-384 hash of a data value. The hash will be returned as a 96
   * characters hex-encoded string.
   *
   * @see {@link #MD5}, {@link #SHA1}, {@link #SHA256}, {@link #SHA512}
   */
  SHA384(Category.Cryptographic),

  /**
   * The function calculate the SHA-512 hash of a data value. The hash will be returned as a 128
   * characters hex-encoded string.
   *
   * @see {@link #MD5}, {@link #SHA1}, {@link #SHA256}, {@link #SHA384}
   */
  SHA512(Category.Cryptographic);

  private final Category category;

  Kind(final Category category) {
    this.category = category;
  }

  public String category() {
    return category.name();
  }
}
