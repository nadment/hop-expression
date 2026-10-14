/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.apache.hop.expression;

import org.apache.hop.i18n.BaseMessages;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Error codes raised while parsing, compiling or evaluating an expression.
 *
 * <p>Each code is bound to a localized message template stored in the message bundle under the key
 * {@code Expression.ErrorCode.<NAME>}. The placeholders <code>{0}</code>, <code>{1}</code>… quoted
 * in the documentation of each code are the values to supply, in that order, to {@link
 * #message(Object...)} or to the exception constructor.
 *
 * <pre>{@code
 * throw new ExpressionException(ErrorCode.ARGUMENT_OUT_OF_RANGE, 2, value);
 * }</pre>
 *
 * @see ExpressionException
 * @see ParseExpressionException
 */
@NullMarked
public enum ErrorCode {
  /**
   * Error raised on purpose by the user with the {@code ERROR} function; the message is the one
   * supplied by the user.
   *
   * <p>Message: <code>{0}</code>
   */
  USER_DEFINED_ERROR,
  /**
   * Unexpected state that reveals a defect of the expression engine rather than a user mistake.
   *
   * <p>Message: <code>Internal error {0}</code>
   */
  INTERNAL_ERROR,
  /**
   * The source text of the expression to parse is {@code null}.
   *
   * <p>Message: <code>Expression source is null</code>
   */
  NULL_SOURCE_ERROR,
  /**
   * The parser found an unexpected token or reached the end of the expression while parsing the
   * construct introduced by the given keyword.
   *
   * <p>Message: <code>Incorrect syntax near the keyword “{0}”</code>
   */
  SYNTAX_ERROR,
  /**
   * A function with a special syntax, such as {@code CAST(value AS type)}, is not written properly.
   *
   * <p>Message: <code>Incorrect syntax for function “{0}”</code>
   */
  SYNTAX_ERROR_FUNCTION,
  /**
   * A {@code CASE} expression is malformed.
   *
   * <p>Message: <code>Syntax error at or near CASE when using a case statement</code>
   */
  SYNTAX_ERROR_CASE_STATEMENT,
  /**
   * The evaluation of an operator failed; wraps the underlying error.
   *
   * <p>Message: <code>Operator {0} encountered an error: {1}</code>
   */
  CALL_OPERATOR_ERROR,
  /**
   * The evaluation of a function failed; wraps the underlying error.
   *
   * <p>Message: <code>Function {0} encountered an error: {1}</code>
   */
  CALL_FUNCTION_ERROR,
  /**
   * The evaluation context has no current row to read the value of a field from.
   *
   * <p>Message: <code>Context error</code>
   */
  CONTEXT_ERROR,
  /**
   * The divisor of a division or of a modulus is zero.
   *
   * <p>Message: <code>Division by zero</code>
   */
  DIVISION_BY_ZERO,
  /**
   * The value list of an {@code IN} predicate is empty.
   *
   * <p>Message: <code>Empty value list for the “IN” predicate</code>
   */
  EMPTY_VALUE_LIST_FOR_IN_PREDICATE,
  /**
   * The precision of a data type is not an integer literal.
   *
   * <p>Message: <code>Expected an integer constant as precision</code>
   */
  EXPECTED_INTEGER_CONSTANT_AS_PRECISION,
  /**
   * The scale of a data type is not an integer literal.
   *
   * <p>Message: <code>Expected an integer constant as a scale</code>
   */
  EXPECTED_INTEGER_CONSTANT_AS_SCALE,
  /**
   * The value of an argument is not accepted by the operator.
   *
   * <p>Message: <code>Invalid argument “{0}”</code>
   *
   * @see #ILLEGAL_ARGUMENT_TYPE
   * @see #ARGUMENT_OUT_OF_RANGE
   */
  INVALID_ARGUMENT,
  /**
   * The data types of the arguments match none of the operand types accepted by the operator.
   *
   * <p>Message: <code>
   * Illegal argument type to call {0}. You might need to add explicit type casts.</code>
   *
   * @see #INVALID_ARGUMENT
   */
  ILLEGAL_ARGUMENT_TYPE,
  /**
   * An identifier does not match any field of the row.
   *
   * <p>Message: <code>Field not found “{0}”</code>
   */
  UNRESOLVED_IDENTIFIER,
  /**
   * No function is registered with the given name.
   *
   * <p>Message: <code>Function “{0}” does not exist</code>
   */
  FUNCTION_DOES_NOT_EXIST,
  /**
   * An aggregate function is required but the expression is not one.
   *
   * <p>Message: <code>Not an aggregation expression</code>
   */
  NOT_AN_AGGREGATE_EXPRESSION,
  /**
   * The function is called with fewer arguments than required.
   *
   * <p>Message: <code>Not enough arguments to call function {0}</code>
   */
  NOT_ENOUGH_ARGUMENTS,
  /**
   * The function is called with more arguments than accepted.
   *
   * <p>Message: <code>Too many arguments to call function {0}</code>
   */
  TOO_MANY_ARGUMENTS,
  /**
   * An element is missing in a list of expressions, as after a trailing comma.
   *
   * <p>Message: <code>Missing element</code>
   */
  MISSING_ELEMENT,
  /**
   * A block comment is opened but never closed.
   *
   * <p>Message: <code>Missing end block comment</code>
   */
  MISSING_END_BLOCK_COMMENT,
  /**
   * A single-quoted string literal is not terminated.
   *
   * <p>Message: <code>Missing end of the single-quotes string</code>
   */
  MISSING_END_SINGLE_QUOTED_STRING,
  /**
   * A double-quoted identifier is not terminated.
   *
   * <p>Message: <code>Missing end of the double-quotes string</code>
   */
  MISSING_END_DOUBLE_QUOTED_STRING,
  /**
   * A left parenthesis {@code (} is expected.
   *
   * <p>Message: <code>Missing left parenthesis</code>
   */
  MISSING_LEFT_PARENTHESIS,
  /**
   * A right bracket <code>]</code> is expected.
   *
   * <p>Message: <code>Missing right bracket</code>
   */
  MISSING_RIGHT_BRACKET,
  /**
   * A right parenthesis {@code )} is expected.
   *
   * <p>Message: <code>Missing right parenthesis</code>
   */
  MISSING_RIGHT_PARENTHESIS,
  /**
   * The lexer found a character that cannot start any token.
   *
   * <p>Message: <code>Unexpected character “{0}”</code>
   */
  UNEXPECTED_CHARACTER,
  /**
   * The function cannot handle the given data type.
   *
   * <p>Message: <code>Unexpected data type {0} with function {1}</code>
   */
  UNEXPECTED_DATA_TYPE,
  /**
   * The result of an arithmetic operator exceeds the capacity of the {@code INTEGER} data type.
   *
   * <p>Message: <code>
   * Arithmetic overflow in “{0}” operator (consider adding explicit CAST to NUMBER)</code>
   */
  ARITHMETIC_OVERFLOW,
  /**
   * A number is too large or too small to be converted to the {@code INTEGER} data type.
   *
   * <p>Message: <code>Integer conversion overflow with value '{0}'</code>
   */
  INTEGER_CONVERSION_OVERFLOW,
  /**
   * The value of the argument at the given position is outside the range accepted by the function.
   *
   * <p>Message: <code>Argument {0} is out of range with value “{1}”</code>
   */
  ARGUMENT_OUT_OF_RANGE,
  /**
   * The length requested for a padding function exceeds the maximum allowed.
   *
   * <p>Message: <code>Padding length exceeds maximum limit {0}</code>
   */
  PADDING_LENGTH_EXCEEDS_MAXIMUM_LIMIT,
  /**
   * A precision is specified for a data type that does not have one.
   *
   * <p>Message: <code>Precision is not supported for {0} data type</code>
   */
  PRECISION_NOT_SUPPORTED,
  /**
   * The precision is outside the range allowed for the data type.
   *
   * <p>Message: <code>The precision of {0} must be within the range {1} to {2}</code>
   */
  PRECISION_OUT_OF_RANGE,
  /**
   * A scale is specified for a data type that does not have one.
   *
   * <p>Message: <code>Scale is not supported for {0} data type</code>
   */
  SCALE_NOT_SUPPORTED,
  /**
   * The scale is outside the range allowed for the data type.
   *
   * <p>Message: <code>The scale of {0} must be within the range {1} to {2}</code>
   */
  SCALE_OUT_OF_RANGE,
  /**
   * The scale of the data type is greater than its precision.
   *
   * <p>Message: <code>The scale of {0} must be less than or equal to the precision</code>
   */
  SCALE_GREATER_THAN_PRECISION,
  /**
   * The index used to access an array element is outside the bounds of the array.
   *
   * <p>Message: <code>The index {0} is out of bounds</code>
   */
  ARRAY_INDEX_OUT_OF_BOUNDS,
  /**
   * The binary operands of a bitwise function do not have the same length.
   *
   * <p>Message: <code>Binary operands of bitwise operators must be of equal length</code>
   */
  INVALID_BITWISE_OPERANDS_SIZE,
  /**
   * The text of a {@code DATE} literal is not a valid date.
   *
   * <p>Message: <code>Date value “{0}” is not recognized</code>
   */
  INVALID_DATE,
  /**
   * The name is not a known time unit, such as {@code YEAR} or {@code MONTH}.
   *
   * <p>Message: <code>Time unit “{0}” is not recognized</code>
   *
   * @see #UNSUPPORTED_TIME_UNIT
   */
  INVALID_TIME_UNIT,
  /**
   * A date format pattern contains an unknown element at the given position.
   *
   * <p>Message: <code>Invalid date format “{0}” at position {1}</code>
   */
  INVALID_DATE_FORMAT,
  /**
   * The text of a {@code TIMESTAMP} literal is not a valid timestamp.
   *
   * <p>Message: <code>Timestamp “{0}” is not recognized</code>
   */
  INVALID_TIMESTAMP,
  /**
   * The name is not a known time zone.
   *
   * <p>Message: <code>Time zone “{0}” is not recognized</code>
   */
  INVALID_TIMEZONE,
  /**
   * The text is not a valid interval.
   *
   * <p>Message: <code>Interval value “{0}” is not recognized</code>
   */
  INVALID_INTERVAL,
  /**
   * The text is not a valid number.
   *
   * <p>Message: <code>Number value “{0}” is not recognized</code>
   */
  INVALID_NUMBER,
  /**
   * A number format pattern is malformed.
   *
   * <p>Message: <code>Invalid number format “{0}”</code>
   */
  INVALID_NUMBER_FORMAT,
  /**
   * The name is not a known binary format, such as {@code HEX} or {@code BASE64}.
   *
   * <p>Message: <code>Invalid binary format “{0}”</code>
   */
  INVALID_BINARY_FORMAT,
  /**
   * A value to decode contains a symbol outside the alphabet of its encoding, such as base32 or
   * base64.
   *
   * <p>Message: <code>Invalid symbol '{0}' found while decoding the {1} sequence</code>
   */
  INVALID_DECODING_SYMBOL,
  /**
   * A value to decode, such as base32 or base64, is truncated: its last symbols do not form whole
   * bytes.
   *
   * <p>Message: <code>Truncated {0} sequence, the last symbols do not form whole bytes</code>
   */
  TRUNCATED_DECODING_SEQUENCE,
  /**
   * A JSON path is malformed.
   *
   * <p>Message: <code>Invalid json path “{0}”</code>
   */
  INVALID_JSON_PATH,
  /**
   * A regular expression pattern is malformed.
   *
   * <p>Message: <code>Invalid regexp pattern “{0}”</code>
   */
  INVALID_REGEXP_PATTERN,
  /**
   * A pattern contains an invalid escape sequence at the given position.
   *
   * <p>Message: <code>Invalid escape sequence “{0}”, {1}</code>
   */
  INVALID_REGEXP_ESCAPE,
  /**
   * The name is not a known data type.
   *
   * <p>Message: <code>Data type “{0}” is not recognized</code>
   */
  INVALID_DATA_TYPE,
  /**
   * The source of a user defined function cannot be compiled; wraps the underlying error.
   *
   * <p>Message: <code>Could not compile user defined function “{0}”: {1}</code>
   */
  UDF_COMPILATION_ERROR,
  /**
   * No conversion exists between the two data types, whatever the value.
   *
   * <p>Message: <code>Conversion from {1} to {2} is not supported for value “{0}”</code>
   *
   * @see #CONVERSION_ERROR
   */
  UNSUPPORTED_CONVERSION,
  /**
   * The Hop value type of a field has no equivalent expression data type.
   *
   * <p>Message: <code>Field “{0}” of type “{1}” not supported</code>
   */
  UNSUPPORTED_FIELD_TYPE,
  /**
   * A JSON node type cannot be handled.
   *
   * <p>Message: <code>Unsupported json type “{0}”</code>
   */
  UNSUPPORTED_JSON_TYPE,
  /**
   * The time unit is valid but not supported by the function.
   *
   * <p>Message: <code>Unsupported time unit {0}</code>
   *
   * @see #INVALID_TIME_UNIT
   */
  UNSUPPORTED_TIME_UNIT,
  /**
   * The conversion between the two data types exists, but this particular value cannot be
   * converted.
   *
   * <p>Message: <code>Could not convert {0} value “{2}” to {1}</code>
   *
   * @see #UNSUPPORTED_CONVERSION
   */
  CONVERSION_ERROR,
  /**
   * The compression of a binary value failed.
   *
   * <p>Message: <code>Compression error</code>
   */
  COMPRESSION_ERROR,
  /**
   * The decompression of a binary value failed.
   *
   * <p>Message: <code>Decompression error</code>
   */
  DECOMPRESSION_ERROR,
  /**
   * The result of a function would exceed the maximum allowed size.
   *
   * <p>Message: <code>Result size too large: {0}</code>
   */
  RESULT_SIZE_TOO_LARGE,
  /**
   * A date or timestamp value cannot be formatted with the given format.
   *
   * <p>Message: <code>Could not format datetime “{0}” with format “{1}”</code>
   */
  DATE_FORMATTING_ERROR,
  /**
   * The text does not match the date format used to parse it.
   *
   * <p>Message: <code>Date “{0}” not recognized with format “{1}”</code>
   */
  UNPARSABLE_DATE_WITH_FORMAT,
  /**
   * The text does not match the number format used to parse it.
   *
   * <p>Message: <code>Number “{0}” not recognized with format “{1}”</code>
   */
  UNPARSABLE_NUMBER_WITH_FORMAT,
  /**
   * The text does not match the binary format used to parse it.
   *
   * <p>Message: <code>Binary “{0}” not recognized with format “{1}”</code>
   */
  UNPARSABLE_BINARY_WITH_FORMAT;

  /**
   * Returns the message template of this error code, with its placeholders left as is.
   *
   * @return the localized message template
   */
  @Override
  public String toString() {
    return message();
  }

  /**
   * Returns the localized message of this error code, formatted with the given values.
   *
   * @param objects the values that replace the placeholders of the message template, in order
   * @return the formatted message
   */
  public String message(@Nullable Object... objects) {
    return BaseMessages.getString(ErrorCode.class, "Expression.ErrorCode." + name(), objects);
  }
}
