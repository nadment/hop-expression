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
  /** {0} */
  MESSAGE_ERROR,
  /** Internal error {0} */
  INTERNAL_ERROR,
  /** Expression source is null */
  NULL_SOURCE_ERROR,
  /** Incorrect syntax near keyword “{0}” */
  SYNTAX_ERROR,
  /** Incorrect syntax for function ''{0}'' */
  SYNTAX_ERROR_FUNCTION,
  /** Syntax error at or near “case” when using a case statement */
  SYNTAX_ERROR_CASE_STATEMENT,
  /** Operator call {0} error: {1} */
  CALL_OPERATOR_ERROR,
  /** Function call {0} error: {1} */
  CALL_FUNCTION_ERROR,
  /** Context error */
  CONTEXT_ERROR,
  /** Division by zero */
  DIVISION_BY_ZERO,
  /** Invalid argument ''{0}'' */
  INVALID_ARGUMENT,
  /** Illegal argument type to call {0}. You might need to add explicit type casts. */
  ILLEGAL_ARGUMENT,
  /** Field not found ''{0}'' */
  UNRESOLVED_IDENTIFIER,
  /** Function ''{0}'' does not exist */
  FUNCTION_DOES_NOT_EXIST,
  /** Not an aggregation expression */
  NOT_AN_AGGREGATE_EXPRESSION,
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
  /** Unexpected character ''{0}'' */
  UNEXPECTED_CHARACTER,
  /** Unexpected data type {1} with function {0} */
  UNEXPECTED_DATA_TYPE,
  /** Arithmetic overflow in ''{0}'' operator (consider adding explicit CAST to NUMBER) */
  ARITHMETIC_OVERFLOW,
  /** Integer conversion overflow with value ''{0}'' */
  CONVERSION_OVERFLOW,
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
  /** Date ''{0}'' is not recognized */
  INVALID_DATE,
  /** Time unit ''{0}'' is not recognized */
  INVALID_TIMEUNIT,
  /** Invalid date format ''{0}'' at position {1} */
  INVALID_DATE_FORMAT,
  /** Timestamp ''{0}'' is not recognized */
  INVALID_TIMESTAMP,
  /** Time zone ''{0}'' is not recognized */
  INVALID_TIMEZONE,
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
  /** Invalid json path ''{0}'' */
  INVALID_JSON_PATH,
  /** Invalid regexp pattern ''{0}'' */
  INVALID_REGEXP_PATTERN,
  /** Invalid escape sequence ''{0}'', {1} */
  INVALID_REGEXP_ESCAPE,
  /** Data type ''{0}'' is not recognized */
  INVALID_TYPE,
  /** Json path is null */
  JSON_PATH_IS_NULL,
  /** Json property cannot be found in specified path ''{0}'' */
  JSON_PATH_NOT_FOUND,
  /** Return type is unknown */
  RETURN_TYPE_UNKNOWN,
  /** Error compile user defined function ''{0}'' */
  UDF_COMPILATION_ERROR,
  /** Conversion from {1} to {2} is not supported for value ''{0}'' */
  UNSUPPORTED_CONVERSION,
  /** Field ''{0}'' of type ''{1}'' not supported */
  UNSUPPORTED_VALUEMETA,
  /** Unsupported json type ''{0}'' */
  UNSUPPORTED_JSON_TYPE,
  /** Unsupported time unit ''{0}'' */
  UNSUPPORTED_TIME_UNIT,
  /** Unsupported array type ''{0}'' */
  UNSUPPORTED_ARRAY_TYPE,
  /** Error converting {0} value ''{2}'' to {1} */
  CONVERSION_ERROR,
  /** Error converting {0} value ''{1}'' to BINARY */
  CONVERSION_ERROR_TO_BINARY,
  /** Error converting {0} value ''{1}'' to BOOLEAN */
  CONVERSION_ERROR_TO_BOOLEAN,
  /** Error converting {0} value ''{1}'' to DATE */
  CONVERSION_ERROR_TO_DATE,
  /** Error converting {0} value ''{1}'' to INET */
  CONVERSION_ERROR_TO_INET,
  /** Error converting {0} value ''{1}'' to INTEGER */
  CONVERSION_ERROR_TO_INTEGER,
  /** Error converting {0} value ''{1}'' to INTERVAL */
  CONVERSION_ERROR_TO_INTERVAL,
  /** Error converting {0} value ''{1}'' to JSON */
  CONVERSION_ERROR_TO_JSON,
  /** Error converting {0} value ''{1}'' to NUMBER */
  CONVERSION_ERROR_TO_NUMBER,
  /** Error converting {0} value ''{1}'' to STRING */
  CONVERSION_ERROR_TO_STRING,
  /** Compression error */
  COMPRESSION_ERROR,
  /** Decompression error */
  DECOMPRESSION_ERROR,
  /** Result size too large: {0} */
  RESULT_SIZE_TOO_LARGE,
  /** Error formating datetime {0} with format ''{1}'' */
  FORMAT_DATE_WITH_FORMAT,
  /** Date ''{0}'' not recognized with format ''{1}'' */
  UNPARSABLE_DATE_WITH_FORMAT,
  /** Number ''{0}'' not recognized with format ''{1}'' */
  UNPARSABLE_NUMBER_WITH_FORMAT,
  /** Unable to parse binary ''{0}'' */
  UNPARSABLE_BINARY;

  @Override
  public String toString() {
    return message();
  }

  public String message(Object... objects) {
    return BaseMessages.getString(ErrorCode.class, "Expression.ErrorCode." + name(), objects);
  }
}
