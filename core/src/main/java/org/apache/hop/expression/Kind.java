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
  VALUE,

  LIST,

  IDENTIFIER,

  // -------------------------------------------------------------
  // BITWISE
  // -------------------------------------------------------------

  BITGET,

  /** The bitwise AND operation. */
  BITAND,

  /** The bitwise OR operation. */
  BITOR,

  /** The bitwise NOT operation. */
  BITNOT,

  /** The bitwise XOR operation. */
  BITXOR,

  // LSHIFT,

  // RSHIFT,

  // -------------------------------------------------------------
  // COMPARISON
  // -------------------------------------------------------------

  /** Contains function */
  CONTAINS,

  /** The "IN" operator. */
  IN,

  /** The "BETWEEN" operator. */
  BETWEEN,

  /** The less-than operator '&lt;'. */
  LESS_THAN,

  /** The greater-than operator '&gt;'. */
  GREATER_THAN,

  /** The less-than-or-equal operator '&lt;='. */
  LESS_THAN_OR_EQUAL,

  /** The greater-than-or-equal operator '&gt;='. */
  GREATER_THAN_OR_EQUAL,

  /** The equals operator '='. */
  EQUAL,

  /**
   * Compares whether two expressions are equal.
   *
   * <p>The function is NULL-safe, meaning it treats NULLs as known values for comparing equality.
   * Note that this is different from the EQUAL comparison operator (=), which treats NULLs as
   * unknown values.
   */
  EQUAL_NULL,

  /** The not-equals operator "&lt;&gt;". @See {@link #LESS_THAN_OR_GREATER_THEN} */
  NOT_EQUAL,

  /** The not-equals operator '!=' @See {@link #NOT_EQUAL_OPERATOR} */
  LESS_THAN_OR_GREATER_THEN,

  /** The IS NULL or <code>IS TRUE</code> operator. */
  IS,

  /** The LIKE operator. */
  LIKE,

  /** The ILIKE case-insensitive operator. */
  ILIKE,

  REGEXP_LIKE,

  /**
   * The function returns TRUE if the first value starts with second value. Both values must be data
   * type string or binary.
   *
   * @see {@link #ENDSWITH}
   */
  STARTSWITH,

  /**
   * The function returns TRUE if the first value ends with second value. Both values must be data
   * type of string or binary.
   *
   * @see {@link #STARTSWITH}
   */
  ENDSWITH,

  // -------------------------------------------------------------
  // CONDITIONAL
  // -------------------------------------------------------------

  /** Case when operator */
  CASE_WHEN,

  /**
   * The COALESCE function returns the first of its arguments that is not null. Null is returned
   * only if all arguments are null.
   */
  COALESCE,

  /** Single-level if-then-else expression. Similar to CASE, but only allows a single condition. */
  IF,

  /** The IFNULL function replace the null with value (Alias NVL). */
  IFNULL,

  /** The function NULLIF */
  NULLIF,

  NVL2,

  /**
   * The function returns the largest value that is not NULL, or NULL if all values are NULL.
   *
   * @see {@link #LEAST}
   */
  GREATEST,

  /**
   * The function returns the smallest value that is not NULL, or NULL if all values are NULL.
   *
   * @see {@link #GREATEST}
   */
  LEAST,

  // -------------------------------------------------------------
  // LOGICAL
  // -------------------------------------------------------------

  /** The logical <code>AND</code> operator. */
  LOGICAL_AND,

  /** The logical <code>NOT</code> operator. */
  LOGICAL_NOT,

  /** The logical <code>OR</code> operator. */
  LOGICAL_OR,

  /** The logical <code>XOR</code> operator. */
  LOGICAL_XOR,

  // -------------------------------------------------------------
  // STRING
  // -------------------------------------------------------------

  /** String concatenation operator '<code>||</code>'. @See {@link #CONCAT} */
  CONCAT,

  /**
   * Compares the select expression to each search expression in order. As soon as a search
   * expression matches the selection expression, the corresponding result expression is returned.
   */
  DECODE,

  /**
   * The function decode string using the Java string literal encoding format. Special characters
   * are \b, \t, \n, \f, \r, \", \\, \<octal>, \\u<unicode>.
   */
  STRINGDECODE,
  /**
   * The function encode special characters in a string using the Java string literal encoding
   * format. Special characters are \b, \t, \n, \f, \r, \", \\, \<octal>, \\u<unicode>.
   */
  STRINGENCODE,

  /** Returns a string that contains a phonetic representation of the input string. */
  SOUNDEX,

  /** The function convert a string value to lower case. @See {@link #LOWER}, {@link #INITCAP} */
  UPPER,

  /** The function convert a string value to upper case. @See {@link #INITCAP}, {@link #UPPER} */
  LOWER,

  /**
   * Returns a string with the first letter of each word in uppercase and the subsequent letters in
   * lowercase. @See {@link #LOWER}, {@link #UPPER}
   */
  INITCAP,

  /**
   * The function extracts a number of characters from a string (starting from left). @See {@link
   * #RIGHT}
   */
  LEFT,

  /**
   * The function extracts a number of characters from a string (starting from right). @See {@link
   * #LEFT}
   */
  RIGHT,

  /** The function returns the number of characters of the specified string. */
  LENGTH,

  /**
   * The function return the ASCII value of the first character in a string. If the string is empty,
   * a value of 0 is returned.
   */
  ASCII,

  /**
   * The function return the Unicode code point for the first Unicode character in a string. If the
   * string is empty, a value of 0 is returned.
   *
   * @see {@link #CHR}, {@link #ASCII},
   */
  UNICODE,

  /**
   * The function converts a Unicode code point (including 7-bit ASCII) into the character that
   * matches the input Unicode. If an invalid code point is specified, an error is returned.
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
   * Returns the position in the string that is the first character of a specified occurrence of the
   * substring.
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

  /** Returns a string consisting of a the specified number of blank spaces. */
  SPACE,

  /** The function repeats a string as many times as specified. */
  REPEAT,

  /**
   * Removes all occurrences of a specified substring, and optionally replaces them with another
   * string.
   */
  REPLACE,

  /**
   * The function reverses the order of characters in a string value, or of bytes in a binary value.
   */
  REVERSE,

  /**
   * Returns the portion of the string from string, startingfrom the character/byte specified by
   * start, with optionally limited length.
   */
  SUBSTRING,

  /** Translates original from the characters in findChars to the characters in replaceChars. */
  TRANSLATE,

  // -------------------------------------------------------------
  // MATHEMATICAL
  // -------------------------------------------------------------

  /** The arithmetic division operator '/'. */
  DIVIDE,

  /** The arithmetic multiplication operator '*'. */
  MULTIPLY,

  /** Returns the exponential value of a numeric expression. */
  EXP,

  /** The arithmetic power operator '**' or function. */
  POWER,

  /** The arithmetic remainder operator '%'. The function returns the remainder division. */
  MOD,

  //	/**
  //	 * The arithmetic unary plus (positive) operator '+'.
  //	 */
  //	POSITIVE(Category.Arithmetic),

  /** The arithmetic unary minus (negative) operator '-'. */
  NEGATIVE,

  /** The arithmetic addition operator '+'. */
  ADD,

  /** The arithmetic subtract operator '-'. */
  SUBTRACT,

  /** Returns the absolute (positive) value of the numeric value. */
  ABS,

  /** Returns the values rounded to the nearest equal or larger integer. */
  CEIL,

  /** Function to converts radians to degrees. */
  DEGREES,

  /** Returns the values rounded to the nearest equal or smaller integer. */
  FLOOR,

  /** Returns the number of PI. */
  PI,

  /** The function converts degrees to radians. */
  RADIANS,

  RAND,

  /** Returns the values rounded to the nearest integer. */
  ROUND,

  /** Returns the natural logarithm of a numeric value. */
  LN,

  /** Returns the specified base logarithm of a numeric value. */
  LOG,

  /** Returns the base 10 logarithm of a numeric value. */
  LOG10,

  /** Returns the sign of a number. */
  SIGN,

  /** Returns the cubic root of a numeric expression. @See {@link #SQRT} */
  CBRT,

  /** Returns the square-root of a non-negative numeric expression. @See {@link #CBRT} */
  SQRT,

  /** Round down numeric expressions or truncates a date or timestamp to the specified part. */
  TRUNCATE,

  // -------------------------------------------------------------
  // TRIGONOMETRY
  // -------------------------------------------------------------

  /**
   * Returns the arc cosine, the angle in radians whose cosine is the specified float expression.
   */
  ACOS,
  ACOSH,

  ASIN,
  ASINH,

  ATAN,
  ATANH,

  ATAN2,

  /** Returns the trigonometric cosine of the specified angle in radians in the specified number. */
  COS,

  /** Returns the hyperbolic cosine of its argument. */
  COSH,

  /** Returns the trigonometric cotangent of the angle in radians specified by float expression. */
  COT,

  /** Calculates the trigonometric sine of the angle in radians. */
  SIN,

  /** Calculates the hyperbolic sine of its argument. */
  SINH,

  /** Calculates the tangent of its argument, the argument should be expressed in radians. */
  TAN,

  /** Calculates the hyperbolic tangent of its argument. */
  TANH,

  // -------------------------------------------------------------
  // DATE AND TIME
  // -------------------------------------------------------------

  /** Adds or subtracts a specified number of days to a date or timestamp */
  ADD_DAYS,

  /** Adds or subtracts a specified number of hours to a date or timestamp */
  ADD_HOURS,

  /** Adds or subtracts a specified number of minutes to a date or timestamp */
  ADD_MINUTES,

  /** Adds or subtracts a specified number of months to a date or timestamp */
  ADD_MONTHS,

  /** Adds or subtracts a specified number of seconds to a date or timestamp */
  ADD_SECONDS,

  /** Adds or subtracts a specified number of weeks to a date or timestamp */
  ADD_WEEKS,

  /** Adds or subtracts a specified number of years to a date or timestamp */
  ADD_YEARS,

  /** DATE function */
  DATE,

  /** Returns the current date value. */
  CURRENT_DATE,

  /** Returns the first day of the month. */
  FIRST_DAY,

  /** Returns the last day of the month. */
  LAST_DAY,

  /** Returns the date of the first specified day of week that occurs after the input date. */
  NEXT_DAY,

  /** Returns the date of the first specified day of week that occurs before the input date. */
  PREVIOUS_DAY,
  
  /** The year of a date */
  YEAR,

  /** Quarter of the year (number from 1-4). */
  QUARTER,

  /** Month of the year (number from 1-12). */
  MONTH,

  /** Returns the name of the month (in English). */
  MONTHNAME,

  /** Week of the year (number from 1-54). */
  WEEK,

  /** Week from the beginning of the month (0-5) */
  WEEKOFMONTH,

  /** Returns the name of the weekday (in English). */
  DAYNAME,

  /** Day of the month (number from 1-31). */
  DAY,

  /** Day of the year (number from 1-366). */
  DAYOFYEAR,

  /** Day of the week (Sunday=1 to Saturday=7). */
  DAYOFWEEK,

  /** Day of the week (Monday=1 to Sunday=7). */
  DAYOFWEEK_ISO,

  /** Week of the year (number from 1-53). */
  WEEK_ISO,

  /** The hour (0-23). @See {@link #MINUTE}, {@link #SECOND} */
  HOUR,

  /** The minute (0-59). @See {@link #HOUR}, {@link #SECOND} */
  MINUTE,

  /** The second (0-59). @See {@link #HOUR}, {@link #MINUTE} */
  SECOND,

  /** Returns number of days between two date values. */
  DAYS_BETWEEN,

  /** Returns number of months between two date. */
  MONTHS_BETWEEN,

  /** Returns number of years between two date. */
  YEARS_BETWEEN,
  /** Return the number of minutes between two timestamps */
  MINUTES_BETWEEN,

  /** Return the number of hours between two timestamps */
  HOURS_BETWEEN,

  /** Return the number of seconds between two timestamps */
  SECONDS_BETWEEN,

  /**
   * Function to extract date part: DECADE | YEAR | MONTH | WEEK | DAY | HOUR | MINUTE | SECOND...
   */
  EXTRACT,

  // -------------------------------------------------------------
  // CONVERTION
  // -------------------------------------------------------------

  /**
   * Converts a value of one data type into another data type <code>::</code> or <code>
   * CAST(value AS type FORMAT format)</code>.
   */
  CAST,
  /** Converts a value of one data type into another data type if the cast succeeds; otherwise, returns null.*/ 
  TRY_CAST,
  
  /** Converts a string or numeric expression to a boolean value. */
  TO_BOOLEAN,
  TRY_TO_BOOLEAN,
  
  /** Converts a numeric or date expression to a string value. */
  TO_CHAR,

  /** Converts a string expression to a date value. */
  TO_DATE,
  TRY_TO_DATE,
  
  /** Converts a string expression to a number value. */
  TO_NUMBER,
  TRY_TO_NUMBER,

  // -------------------------------------------------------------
  // CRYPTOGRAPHIC
  // -------------------------------------------------------------

  /**
   * The function calculate the MD5 hash of a data value. The hash will be returned as a 32
   * characters hex-encoded string.
   *
   * @see {@link #SHA1}, {@link #SHA256}, {@link #SHA384}, {@link #SHA512}
   */
  MD5,

  /**
   * The function calculate the SHA-1 hash of a data value. The hash will be returned as a 40
   * characters hex-encoded string.
   *
   * @see {@link #MD5}, {@link #SHA256}, {@link #SHA384}, {@link #SHA512}
   */
  SHA1,

  /**
   * The function calculate the SHA-256 hash of a data value. The hash will be returned as a 64
   * characters hex-encoded string.
   *
   * @see {@link #MD5}, {@link #SHA1}, {@link #SHA384}, {@link #SHA512}
   */
  SHA256,

  /**
   * The function calculate the SHA-384 hash of a data value. The hash will be returned as a 96
   * characters hex-encoded string.
   *
   * @see {@link #MD5}, {@link #SHA1}, {@link #SHA256}, {@link #SHA512}
   */
  SHA384,

  /**
   * The function calculate the SHA-512 hash of a data value. The hash will be returned as a 128
   * characters hex-encoded string.
   *
   * @see {@link #MD5}, {@link #SHA1}, {@link #SHA256}, {@link #SHA384}
   */
  SHA512;
}
