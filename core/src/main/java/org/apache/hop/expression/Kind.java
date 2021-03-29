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
  VALUE(Category.NONE),

  LIST(Category.NONE),

  IDENTIFIER(Category.NONE),

  // -------------------------------------------------------------
  // BITWISE
  // -------------------------------------------------------------

  BITGET(Category.BITWISE),

  /** The bitwise AND operation. */
  BITAND(Category.BITWISE),

  /** The bitwise OR operation. */
  BITOR(Category.BITWISE),

  /** The bitwise NOT operation. */
  BITNOT(Category.BITWISE),

  /** The bitwise XOR operation. */
  BITXOR(Category.BITWISE),

  // LSHIFT(Category.Bitwise),

  // RSHIFT(Category.Bitwise),

  // -------------------------------------------------------------
  // COMPARISON
  // -------------------------------------------------------------

  /** Contains function */
  CONTAINS(Category.COMPARISON),

  /** The "IN" operator. */
  IN(Category.COMPARISON),

  /** The "BETWEEN" operator. */
  BETWEEN(Category.COMPARISON),

  /** The less-than operator '&lt;'. */
  LESS_THAN(Category.COMPARISON),

  /** The greater-than operator '&gt;'. */
  GREATER_THAN(Category.COMPARISON),

  /** The less-than-or-equal operator '&lt;='. */
  LESS_THAN_OR_EQUAL(Category.COMPARISON),

  /** The greater-than-or-equal operator '&gt;='. */
  GREATER_THAN_OR_EQUAL(Category.COMPARISON),

  /** The equals operator '='. */
  EQUAL(Category.COMPARISON),

  /**
   * Compares whether two expressions are equal.
   *
   * <p>The function is NULL-safe, meaning it treats NULLs as known values for comparing equality.
   * Note that this is different from the EQUAL comparison operator (=), which treats NULLs as
   * unknown values.
   */
  EQUAL_NULL(Category.COMPARISON),

  /** The not-equals operator "&lt;&gt;". @See {@link #LESS_THAN_OR_GREATER_THEN} */
  NOT_EQUAL(Category.COMPARISON),

  /** The not-equals operator '!=' @See {@link #NOT_EQUAL_OPERATOR} */
  LESS_THAN_OR_GREATER_THEN(Category.COMPARISON),

  /** The IS NULL or <code>IS TRUE</code> operator. */
  IS(Category.COMPARISON),

  /** The LIKE operator. */
  LIKE(Category.COMPARISON),

  /** The ILIKE case-insensitive operator. */
  ILIKE(Category.COMPARISON),

  REGEXP_LIKE(Category.COMPARISON),

  /**
   * The function returns TRUE if the first value starts with second value. Both values must be data
   * type string or binary.
   *
   * @see {@link #ENDSWITH}
   */
  STARTSWITH(Category.COMPARISON),

  /**
   * The function returns TRUE if the first value ends with second value. Both values must be data
   * type of string or binary.
   *
   * @see {@link #STARTSWITH}
   */
  ENDSWITH(Category.COMPARISON),

  // -------------------------------------------------------------
  // CONDITIONAL
  // -------------------------------------------------------------

  /** Case when operator */
  CASE_WHEN(Category.CONDITIONAL),

  /**
   * The COALESCE function returns the first of its arguments that is not null. Null is returned
   * only if all arguments are null.
   */
  COALESCE(Category.CONDITIONAL),

  /** Single-level if-then-else expression. Similar to CASE, but only allows a single condition. */
  IF(Category.CONDITIONAL),

  /** The IFNULL function replace the null with value (Alias NVL). */
  IFNULL(Category.CONDITIONAL),

  /** The function NULLIF */
  NULLIF(Category.CONDITIONAL),

  NVL2(Category.CONDITIONAL),

  /**
   * The function returns the largest value that is not NULL, or NULL if all values are NULL.
   *
   * @see {@link #LEAST}
   */
  GREATEST(Category.CONDITIONAL),

  /**
   * The function returns the smallest value that is not NULL, or NULL if all values are NULL.
   *
   * @see {@link #GREATEST}
   */
  LEAST(Category.CONDITIONAL),

  // -------------------------------------------------------------
  // LOGICAL
  // -------------------------------------------------------------

  /** The logical <code>AND</code> operator. */
  LOGICAL_AND(Category.LOGICAL),

  /** The logical <code>NOT</code> operator. */
  LOGICAL_NOT(Category.LOGICAL),

  /** The logical <code>OR</code> operator. */
  LOGICAL_OR(Category.LOGICAL),

  /** The logical <code>XOR</code> operator. */
  LOGICAL_XOR(Category.LOGICAL),

  // -------------------------------------------------------------
  // STRING
  // -------------------------------------------------------------

  /** String concatenation operator '<code>||</code>'. @See {@link #CONCAT} */
  CONCAT(Category.STRING),

  /**
   * Compares the select expression to each search expression in order. As soon as a search
   * expression matches the selection expression, the corresponding result expression is returned.
   */
  DECODE(Category.STRING),

  /**
   * The function decode string using the Java string literal encoding format. Special characters
   * are \b, \t, \n, \f, \r, \", \\, \<octal>, \\u<unicode>.
   */
  STRINGDECODE(Category.STRING),
  /**
   * The function encode special characters in a string using the Java string literal encoding
   * format. Special characters are \b, \t, \n, \f, \r, \", \\, \<octal>, \\u<unicode>.
   */
  STRINGENCODE(Category.STRING),

  /** Returns a string that contains a phonetic representation of the input string. */
  SOUNDEX(Category.STRING),

  /** The function convert a string value to lower case. @See {@link #LOWER}, {@link #INITCAP} */
  UPPER(Category.STRING),

  /** The function convert a string value to upper case. @See {@link #INITCAP}, {@link #UPPER} */
  LOWER(Category.STRING),

  /**
   * Returns a string with the first letter of each word in uppercase and the subsequent letters in
   * lowercase. @See {@link #LOWER}, {@link #UPPER}
   */
  INITCAP(Category.STRING),

  /**
   * The function extracts a number of characters from a string (starting from left). @See {@link
   * #RIGHT}
   */
  LEFT(Category.STRING),

  /**
   * The function extracts a number of characters from a string (starting from right). @See {@link
   * #LEFT}
   */
  RIGHT(Category.STRING),

  /** The function returns the number of characters of the specified string. */
  LENGTH(Category.STRING),

  /**
   * The function return the ASCII value of the first character in a string. If the string is empty,
   * a value of 0 is returned.
   */
  ASCII(Category.STRING),

  /**
   * The function return the Unicode code point for the first Unicode character in a string. If the
   * string is empty, a value of 0 is returned.
   *
   * @see {@link #CHR}, {@link #ASCII},
   */
  UNICODE(Category.STRING),

  /**
   * The function converts a Unicode code point (including 7-bit ASCII) into the character that
   * matches the input Unicode. If an invalid code point is specified, an error is returned.
   *
   * @see {@link #ASCII}
   */
  CHR(Category.STRING),

  /**
   * The function encode the string as a URL.
   *
   * @see {@link #URLDECODE}
   */
  URLENCODE(Category.STRING),

  /**
   * The function decode the URL to a string.
   *
   * @see {@link #URLENCODE}
   */
  URLDECODE(Category.STRING),

  /**
   * Returns the position in the string that is the first character of a specified occurrence of the
   * substring.
   */
  INSTR(Category.STRING),

  /**
   * The function removes leading and trailing characters from a string.
   *
   * @see {@link #LTRIM}, {@link #RTRIM}
   */
  TRIM(Category.STRING),

  /**
   * The function removes leading characters from a string.
   *
   * @see {@link #TRIM}, {@link #RTRIM}
   */
  LTRIM(Category.STRING),

  /**
   * The function removes leading characters from a string.
   *
   * @see {@link #TRIM}, {@link #LTRIM}
   */
  RTRIM(Category.STRING),

  /**
   * The function left-pads a string with another string, to a certain length.
   *
   * @see {@link #RPAD}
   */
  LPAD(Category.STRING),

  /**
   * The function right-pads a string with another string, to a certain length.
   *
   * @see {@link #LPAD}
   */
  RPAD(Category.STRING),

  /** Returns a string consisting of a the specified number of blank spaces. */
  SPACE(Category.STRING),

  /** The function repeats a string as many times as specified. */
  REPEAT(Category.STRING),

  /**
   * Removes all occurrences of a specified substring, and optionally replaces them with another
   * string.
   */
  REPLACE(Category.STRING),

  /**
   * The function reverses the order of characters in a string value, or of bytes in a binary value.
   */
  REVERSE(Category.STRING),

  /**
   * Returns the portion of the string from string, startingfrom the character/byte specified by
   * start, with optionally limited length.
   */
  SUBSTRING(Category.STRING),

  /** Translates original from the characters in findChars to the characters in replaceChars. */
  TRANSLATE(Category.STRING),

  // -------------------------------------------------------------
  // MATHEMATICAL
  // -------------------------------------------------------------

  /** The arithmetic division operator '/'. */
  DIVIDE(Category.MATHEMATICAL),

  /** The arithmetic multiplication operator '*'. */
  MULTIPLY(Category.MATHEMATICAL),

  /** Returns the exponential value of a numeric expression. */
  EXP(Category.MATHEMATICAL),

  /** The arithmetic power operator '**' or function. */
  POWER(Category.MATHEMATICAL),

  /** The arithmetic remainder operator '%'. The function returns the remainder division. */
  MOD(Category.MATHEMATICAL),

  //	/**
  //	 * The arithmetic unary plus (positive) operator '+'.
  //	 */
  //	POSITIVE(Category.Arithmetic),

  /** The arithmetic unary minus (negative) operator '-'. */
  NEGATIVE(Category.MATHEMATICAL),

  /** The arithmetic addition operator '+'. */
  ADD(Category.MATHEMATICAL),

  /** The arithmetic subtract operator '-'. */
  SUBTRACT(Category.MATHEMATICAL),

  /** Returns the absolute (positive) value of the numeric value. */
  ABS(Category.MATHEMATICAL),

  /** Returns the values rounded to the nearest equal or larger integer. */
  CEIL(Category.MATHEMATICAL),

  /** Function to converts radians to degrees. */
  DEGREES(Category.MATHEMATICAL),

  /** Returns the values rounded to the nearest equal or smaller integer. */
  FLOOR(Category.MATHEMATICAL),

  /** Returns the number of PI. */
  PI(Category.MATHEMATICAL),

  /** The function converts degrees to radians. */
  RADIANS(Category.MATHEMATICAL),

  RAND(Category.MATHEMATICAL),

  /** Returns the values rounded to the nearest integer. */
  ROUND(Category.MATHEMATICAL),

  /** Returns the natural logarithm of a numeric value. */
  LN(Category.MATHEMATICAL),

  /** Returns the specified base logarithm of a numeric value. */
  LOG(Category.MATHEMATICAL),

  /** Returns the base 10 logarithm of a numeric value. */
  LOG10(Category.MATHEMATICAL),

  /** Returns the sign of a number. */
  SIGN(Category.MATHEMATICAL),

  /** Returns the cubic root of a numeric expression. @See {@link #SQRT} */
  CBRT(Category.MATHEMATICAL),

  /** Returns the square-root of a non-negative numeric expression. @See {@link #CBRT} */
  SQRT(Category.MATHEMATICAL),

  /** Round down numeric expressions or truncates a date or timestamp to the specified part. */
  TRUNCATE(Category.MATHEMATICAL),

  // -------------------------------------------------------------
  // TRIGONOMETRY
  // -------------------------------------------------------------

  /**
   * Returns the arc cosine, the angle in radians whose cosine is the specified float expression.
   */
  ACOS(Category.TRIGONOMETRY),
  ACOSH(Category.TRIGONOMETRY),

  ASIN(Category.TRIGONOMETRY),
  ASINH(Category.TRIGONOMETRY),

  ATAN(Category.TRIGONOMETRY),
  ATANH(Category.TRIGONOMETRY),

  ATAN2(Category.TRIGONOMETRY),

  /** Returns the trigonometric cosine of the specified angle in radians in the specified number. */
  COS(Category.TRIGONOMETRY),

  /** Returns the hyperbolic cosine of its argument. */
  COSH(Category.TRIGONOMETRY),

  /** Returns the trigonometric cotangent of the angle in radians specified by float expression. */
  COT(Category.TRIGONOMETRY),

  /** Calculates the trigonometric sine of the angle in radians. */
  SIN(Category.TRIGONOMETRY),

  /** Calculates the hyperbolic sine of its argument. */
  SINH(Category.TRIGONOMETRY),

  /** Calculates the tangent of its argument, the argument should be expressed in radians. */
  TAN(Category.TRIGONOMETRY),

  /** Calculates the hyperbolic tangent of its argument. */
  TANH(Category.TRIGONOMETRY),

  // -------------------------------------------------------------
  // DATE AND TIME
  // -------------------------------------------------------------

  /** Adds or subtracts a specified number of days to a date or timestamp */
  ADD_DAYS(Category.DATE),

  /** Adds or subtracts a specified number of hours to a date or timestamp */
  ADD_HOURS(Category.DATE),

  /** Adds or subtracts a specified number of minutes to a date or timestamp */
  ADD_MINUTES(Category.DATE),

  /** Adds or subtracts a specified number of months to a date or timestamp */
  ADD_MONTHS(Category.DATE),

  /** Adds or subtracts a specified number of seconds to a date or timestamp */
  ADD_SECONDS(Category.DATE),

  /** Adds or subtracts a specified number of weeks to a date or timestamp */
  ADD_WEEKS(Category.DATE),

  /** Adds or subtracts a specified number of years to a date or timestamp */
  ADD_YEARS(Category.DATE),

  /** DATE function */
  DATE(Category.DATE),

  /** Returns the current date value. */
  CURRENT_DATE(Category.DATE),

  /** Returns the first day of the month. */
  FIRST_DAY(Category.DATE),

  /** Returns the last day of the month. */
  LAST_DAY(Category.DATE),

  /** Returns the date of the first specified day of week that occurs after the input date. */
  NEXT_DAY(Category.DATE),

  /** Returns the date of the first specified day of week that occurs before the input date. */
  PREVIOUS_DAY(Category.DATE),
  
  /** The year of a date */
  YEAR(Category.DATE),

  /** Quarter of the year (number from 1-4). */
  QUARTER(Category.DATE),

  /** Month of the year (number from 1-12). */
  MONTH(Category.DATE),

  /** Returns the name of the month (in English). */
  MONTHNAME(Category.DATE),

  /** Week of the year (number from 1-54). */
  WEEK(Category.DATE),

  /** Week from the beginning of the month (0-5) */
  WEEKOFMONTH(Category.DATE),

  /** Returns the name of the weekday (in English). */
  DAYNAME(Category.DATE),

  /** Day of the month (number from 1-31). */
  DAY(Category.DATE),

  /** Day of the year (number from 1-366). */
  DAYOFYEAR(Category.DATE),

  /** Day of the week (Sunday=1 to Saturday=7). */
  DAYOFWEEK(Category.DATE),

  /** Day of the week (Monday=1 to Sunday=7). */
  DAYOFWEEK_ISO(Category.DATE),

  /** Week of the year (number from 1-53). */
  WEEK_ISO(Category.DATE),

  /** The hour (0-23). @See {@link #MINUTE}, {@link #SECOND} */
  HOUR(Category.DATE),

  /** The minute (0-59). @See {@link #HOUR}, {@link #SECOND} */
  MINUTE(Category.DATE),

  /** The second (0-59). @See {@link #HOUR}, {@link #MINUTE} */
  SECOND(Category.DATE),

  /** Returns number of days between two date values. */
  DAYS_BETWEEN(Category.DATE),

  /** Returns number of months between two date. */
  MONTHS_BETWEEN(Category.DATE),

  /** Returns number of years between two date. */
  YEARS_BETWEEN(Category.DATE),
  /** Return the number of minutes between two timestamps */
  MINUTES_BETWEEN(Category.DATE),

  /** Return the number of hours between two timestamps */
  HOURS_BETWEEN(Category.DATE),

  /** Return the number of seconds between two timestamps */
  SECONDS_BETWEEN(Category.DATE),

  /**
   * Function to extract date part: DECADE | YEAR | MONTH | WEEK | DAY | HOUR | MINUTE | SECOND...
   */
  EXTRACT(Category.DATE),

  // -------------------------------------------------------------
  // CONVERTION
  // -------------------------------------------------------------

  /**
   * Converts a value of one data type into another data type <code>::</code> or <code>
   * CAST(value AS type FORMAT format)</code>.
   */
  CAST(Category.CONVERSION),
  /** Converts a value of one data type into another data type if the cast succeeds; otherwise, returns null.*/ 
  TRY_CAST(Category.CONVERSION),
  
  /** Converts a string or numeric expression to a boolean value. */
  TO_BOOLEAN(Category.CONVERSION),
  TRY_TO_BOOLEAN(Category.CONVERSION),
  
  /** Converts a numeric or date expression to a string value. */
  TO_CHAR(Category.CONVERSION),

  /** Converts a string expression to a date value. */
  TO_DATE(Category.CONVERSION),
  TRY_TO_DATE(Category.CONVERSION),
  
  /** Converts a string expression to a number value. */
  TO_NUMBER(Category.CONVERSION),
  TRY_TO_NUMBER(Category.CONVERSION),

  // -------------------------------------------------------------
  // CRYPTOGRAPHIC
  // -------------------------------------------------------------

  /**
   * The function calculate the MD5 hash of a data value. The hash will be returned as a 32
   * characters hex-encoded string.
   *
   * @see {@link #SHA1}, {@link #SHA256}, {@link #SHA384}, {@link #SHA512}
   */
  MD5(Category.CRYPTOGRAPHIC),

  /**
   * The function calculate the SHA-1 hash of a data value. The hash will be returned as a 40
   * characters hex-encoded string.
   *
   * @see {@link #MD5}, {@link #SHA256}, {@link #SHA384}, {@link #SHA512}
   */
  SHA1(Category.CRYPTOGRAPHIC),

  /**
   * The function calculate the SHA-256 hash of a data value. The hash will be returned as a 64
   * characters hex-encoded string.
   *
   * @see {@link #MD5}, {@link #SHA1}, {@link #SHA384}, {@link #SHA512}
   */
  SHA256(Category.CRYPTOGRAPHIC),

  /**
   * The function calculate the SHA-384 hash of a data value. The hash will be returned as a 96
   * characters hex-encoded string.
   *
   * @see {@link #MD5}, {@link #SHA1}, {@link #SHA256}, {@link #SHA512}
   */
  SHA384(Category.CRYPTOGRAPHIC),

  /**
   * The function calculate the SHA-512 hash of a data value. The hash will be returned as a 128
   * characters hex-encoded string.
   *
   * @see {@link #MD5}, {@link #SHA1}, {@link #SHA256}, {@link #SHA384}
   */
  SHA512(Category.CRYPTOGRAPHIC);

  private final Category category;

  Kind(final Category category) {
    this.category = category;
  }

  public String category() {
    return category.name();
  }
}
