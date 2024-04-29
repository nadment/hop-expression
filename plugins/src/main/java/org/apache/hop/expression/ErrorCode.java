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

/** Enumeration of the error which can be used in expression exception. */
public enum ErrorCode {
  /** Internal error {0} */
  INTERNAL_ERROR,
  /** Expression source is null */
  NULL_SOURCE_ERROR,
  /** Syntax error: {0} */
  SYNTAX_ERROR,
  /** Incorrect syntax near keyword ''{0}'' */
  SYNTAX_ERROR_NEAR_KEYWORD,
  /** Incorrect syntax for function ''{0}'' */
  SYNTAX_ERROR_FUNCTION,
  /** Syntax error: Invalid data type ''{0}'' */
  SYNTAX_ERROR_DATATYPE,
  /** {0} {1} */
  OPERATOR_ERROR,
  /** Context error */
  CONTEXT_ERROR,
  /** Division by zero */
  DIVISION_BY_ZERO,
  /** Illegal argument to the operator ''{0}'' */
  ILLEGAL_ARGUMENT,
  /** {0}: Illegal argument data type */
  ILLEGAL_ARGUMENT_TYPE,
  /** Field not found ''{0}'' */
  UNRESOLVED_IDENTIFIER,
  /** Function ''{0}'' does not exist */
  FUNCTION_DOES_NOT_EXIST,
  /** Not enough arguments to call function {0} */
  NOT_ENOUGH_ARGUMENT,
  /** Too many arguments to call function {0} */
  TOO_MANY_ARGUMENT,
  /** Missing end block comment */
  MISSING_END_BLOCK_COMMENT,
  /** Missing end of single-quotes string */
  MISSING_END_SINGLE_QUOTED_STRING,
  /** Missing end of double-quotes string */
  MISSING_END_DOUBLE_QUOTED_STRING,
  /** Missing left bracket */
  MISSING_LEFT_BRACKET,
  /** Missing left parenthesis */
  MISSING_LEFT_PARENTHESIS,
  /** Missing right bracket */
  MISSING_RIGHT_BRACKET,
  /** Missing right parenthesis */
  MISSING_RIGHT_PARENTHESIS,
  /** Unbalanced parenthesis */
  UNBALANCE_PARENTHESIS,
  /** Unexpected character ''{0}'' */
  UNEXPECTED_CHARACTER,
  /** Unexpected end of expression */
  UNEXPECTED_END_OF_EXPRESSION,
  /** Unexpected data type {1} with function {0} */
  UNEXPECTED_DATA_TYPE,
  // Arithmetic overflow error converting ''{0}''
  ARITHMETIC_OVERFLOW,
  /** Argument {0} is out of range with value ''{1}'' */
  ARGUMENT_OUT_OF_RANGE,
  /** Precision of {0} must be within the range {1} to {2} */
  PRECISION_OUT_OF_RANGE,
  /** The scale of {0} must be within the range {1} to {2} */
  SCALE_OUT_OF_RANGE,
  /** The scale of {0} must be less than or equal to the precision */
  SCALE_GREATER_THAN_PRECISION,
  /** The index ''{0}'' is out of bounds */
  INVALID_ARRAY_INDEX,
  /** Invalid Boolean ''{0}'' */
  INVALID_BOOLEAN,
  /** Date ''{0}'' is not recognized */
  INVALID_DATE,
  /** Time unit ''{0}'' is not recognized */
  INVALID_TIMEUNIT,
  /** Invalid date format ''{0}'' at position {1} */
  INVALID_DATE_FORMAT,
  /** Time value ''{0}'' is not recognized */
  INVALID_TIME,
  /** Timestamp ''{0}'' is not recognized */
  INVALID_TIMESTAMP,
  /** Time zone ''{0}'' is not recognized */
  INVALID_TIMEZONE,
  /** Integer ''{0}'' is not recognized */
  INVALID_INTEGER,
  /** Interval ''{0}'' is not recognized */
  INVALID_INTERVAL,
  /** Invalid name of month ''{0}'' at position {1} */
  INVALID_NAME_OF_MONTH,
  /** Number ''{0}'' is not recognized */
  INVALID_NUMBER,
  /** Invalid number format ''{0}'' */
  INVALID_NUMBER_FORMAT,
  /** Invalid binary format ''{0}'' */
  INVALID_BINARY_FORMAT,
  // Invalid json ''{0}'' */
  INVALID_JSON,
  /** Invalid json path ''{0}'' */
  INVALID_JSON_PATH,
  // Invalid internet address ''{0}'' */
  INVALID_INET,
  /** Invalid regexp pattern ''{0}'' */
  INVALID_REGEXP_PATTERN,
  /** Invalid escape sequence ''{0}'', {1} */
  INVALID_REGEXP_ESCAPE,
  /** Json path is null */
  JSON_PATH_IS_NULL,
  /** Json property cannot be found in specified path ''{0}'' */
  JSON_PATH_NOT_FOUND,
  /** Error compile user defined function ''{0}'' */
  UDF_COMPILATION_ERROR,
  /** Implicit conversion from {1} to {2} is not supported for value ''{0}'' */
  UNSUPPORTED_COERCION,
  /* Conversion from {1} to {2} is not supported for value ''{0}'' */
  UNSUPPORTED_CONVERSION,
  /** Field ''{0}'' of type ''{1}'' not supported */
  UNSUPPORTED_VALUEMETA,
  /** Unsupported json type ''{0}'' */
  UNSUPPORTED_JSON_TYPE,
  /** Unsupported array type ''{0}'' */
  UNSUPPORTED_ARRAY_TYPE,
  /** Error converting {0} value ''{1}'' to data type {2} */
  CONVERSION_ERROR,
  /** Compression error */
  COMPRESSION_ERROR,
  /** decompression error */
  DECOMPRESSION_ERROR,
  /** Date ''{0}'' not recognized with format ''{1}'' */
  UNPARSABLE_DATE_WITH_FORMAT,
  /** Number ''{0}'' not recognized with format ''{1}'' */
  UNPARSABLE_NUMBER_WITH_FORMAT,
  /** Unable to parse binary ''{0}' */
  UNPARSABLE_BINARY;

  @Override
  public String toString() {
    return message();
  }

  public String message(Object... objects) {
    return BaseMessages.getString(ErrorCode.class, "Expression.ErrorCode." + name(), objects);
  }
}
