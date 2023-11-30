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

/**
 * Enumeration of the error which can be used in expression exception.
 */
public enum ErrorCode {
  /** Internal error {0} */
  INTERNAL_ERROR("Expression.InternalError"),
  /** Expression source is null */  
  NULL_SOURCE_ERROR("Expression.NullSourceError"),
  /** Syntax error: {0} */
  SYNTAX_ERROR("Expression.SyntaxError"), 
  /** Incorrect syntax near keyword ''{0}'' */
  SYNTAX_ERROR_NEAR_KEYWORD("Expression.SyntaxErrorNearKeyword"),
  /** Incorrect syntax for function ''{0}'' */
  SYNTAX_ERROR_FUNCTION("Expression.SyntaxErrorFunction"),
  /** Syntax error: Invalid data type ''{0}'' */
  SYNTAX_ERROR_DATATYPE("Expression.SyntaxErrorDataType"),
  /** {0} {1} */
  OPERATOR_ERROR("Expression.OperatorError"),
  //
  CONTEXT_ERROR("Expression.ContextError"),
  /** Division by zero */
  DIVISION_BY_ZERO("Expression.DivisionByZero"),
  /** Illegal argument to the operator ''{0}'' */
  ILLEGAL_ARGUMENT("Expression.IllegalArgument"),
  //
  ILLEGAL_ARGUMENT_TYPE("Expression.IllegalArgumentType"),
  /** Field not found ''{0}'' */
  UNRESOLVED_IDENTIFIER("Expression.UnresolvedIdentifier"),
  /** Function ''{0}'' does not exist */
  FUNCTION_DOES_NOT_EXIST("Expression.FunctionDoesNotExist"),
  //
  NOT_ENOUGH_ARGUMENT("Expression.NotEnoughArguments"),
  //
  TOO_MANY_ARGUMENT("Expression.TooManyArguments"),
  //
  MISSING_END_BLOCK_COMMENT("Expression.MissingEndBlockComment"),
  //
  MISSING_END_SINGLE_QUOTED_STRING("Expression.MissingEndSingleQuotedString"),
  //
  MISSING_END_DOUBLE_QUOTED_STRING("Expression.MissingEndDoubleQuotedString"),
  //
  MISSING_RIGHT_PARENTHESIS("Expression.MissingRightParenthesis"),
  //
  MISSING_LEFT_PARENTHESIS("Expression.MissingLeftParenthesis"),
  //
  UNBALANCE_PARENTHESIS("Expression.UnbalancedParenthesis"),
  /** Unexpected character ''{0}'' */
  UNEXPECTED_CHARACTER("Expression.UnexpectedCharacter"),
  //
  UNEXPECTED_END_OF_EXPRESSION("Expression.UnexpectedEndOfExpression"),
  //
  UNEXPECTED_DATA_TYPE("Expression.UnexpectedDataType"),
  //
  ARITHMETIC_OVERFLOW("Expression.ArithmeticOverflow"),
  /** Argument {0} is out of range with value ''{1}'' */
  ARGUMENT_OUT_OF_RANGE("Expression.ArgumentOutOfRange"),
  /** Precision out of range ''{0}'' */
  PRECISION_OUT_OF_RANGE("Expression.PrecisionOutOfRange"),
  //
  VARIABLE_VALUE_ERROR("Expression.InvalidVariable"),
  /** Invalid Boolean ''{0}'' */
  INVALID_BOOLEAN("Expression.InvalidBoolean"),
  //
  // INVALID_DATE("Expression.InvalidDate"),
  //
  INVALID_TIMEUNIT("Expression.InvalidTimeUnit"),
  //
  INVALID_DATE_FORMAT("Expression.InvalidDateFormat"),
  //
  INVALID_TIME("Expression.InvalidTime"),
  //
  INVALID_TIMESTAMP("Expression.InvalidTimestamp"),
  //
  INVALID_TIMEZONE("Expression.InvalidTimeZone"),
  //
  INVALID_INTEGER("Expression.InvalidInteger"),
  //
  INVALID_INTERVAL("Expression.InvalidInterval"),
  //
  INVALID_NUMBER("Expression.InvalidNumber"),
  //
  INVALID_NUMBER_FORMAT("Expression.InvalidNumberFormat"),
  //
  INVALID_BINARY_FORMAT("Expression.InvalidBinaryFormat"),
  //
  INVALID_JSON("Expression.InvalidJson"),
  //
  INVALID_JSON_PATH("Expression.InvalidJsonPath"),

  //
  INVALID_VALUES("Expression.InvalidValues"),
  // Invalid regexp pattern ''{0}''
  INVALID_REGEXP_PATTERN("Expression.InvalidRegexpPattern"),
  //
  INVALID_REGEXP_ESCAPE("Expression.InvalidRegexpEscape"),
  //
  JSON_PATH_IS_NULL("Expression.JsonPathIsNull"),
  //
  JSON_PATH_NOT_FOUND("Expression.JsonPathNotFound"),
  //
  UDF_COMPILATION_ERROR("Expression.UdfCompilationError"),
  //
  UNSUPPORTED_COERCION("Expression.UnsupportedCoercion"),

  UNSUPPORTED_CONVERSION("Expression.UnsupportedConversion"),
  //
  UNSUPPORTED_VALUEMETA("Expression.UnsupportedValueMeta"),
  //
  UNSUPPORTED_JSON_TYPE("Expression.UnsupportedJsonType"),
  //
  UNSUPPORTED_ARRAY_TYPE("Expression.UnsupportedArrayType"),
  /** Error converting {0} value ''{1}'' to data type {2} */
  CONVERSION_ERROR("Expression.ConversionError"),
  // 
  COMPRESSION_ERROR("Expression.CompressionError"),
  //
  DECOMPRESSION_ERROR("Expression.DecompressionError"),
  //
  UNPARSABLE_DATE_WITH_FORMAT("Expression.UnparsableDateWithFormat"),
  //
  UNPARSABLE_NUMBER_WITH_FORMAT("Expression.UnparsableNumberWithFormat"),
  //
  UNPARSABLE_BINARY("Expression.UnparsableBinary"),
  //
  REGEXP_REPLACE_ERROR("Expression.RegexpReplaceError");

  private final String message;

  ErrorCode(final String message) {
    this.message = message;
  }

  @Override
  public String toString() {
    return BaseMessages.getString(ErrorCode.class, message);
  }

  public String message(Object... objects) {
    return BaseMessages.getString(ErrorCode.class, message, objects);
  }
}
