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
public enum ExpressionError {
  INTERNAL_ERROR("Expression.InternalError"),
  SYNTAX_ERROR("Expression.SyntaxError"),
  OPERATOR_ERROR("Expression.OperatorError"),
  DIVISION_BY_ZERO("Expression.DivisionByZero"),
  ILLEGAL_ARGUMENT("Expression.IllegalArgument"), 
  UNRESOLVED_IDENTIFIER("Expression.UnresolvedIdentifier"),
  NOT_ENOUGH_ARGUMENT("Expression.NotEnoughArguments"),
  TOO_MANY_ARGUMENT("Expression.TooManyArguments"),
  MISSING_END_BLOCK_COMMENT("Expression.MissingEndBlockComment"),
  MISSING_END_SINGLE_QUOTED_STRING("Expression.MissingEndSingleQuotedString"),
  MISSING_RIGHT_PARENTHESIS("Expression.MissingRightParenthesis"),
  MISSING_LEFT_PARENTHESIS("Expression.MissingLeftParenthesis"),
  UNBALANCE_PARENTHESIS("Expression.UnbalancedParenthesis"),
  UNEXPECTED_CHARACTER("Expression.UnexpectedCharacter"),
  UNEXPECTED_END_OF_EXPRESSION("Expression.UnexpectedEndOfExpression"),
  UNEXPECTED_DATA_TYPE("Expression.UnexpectedDataType"),
  ARITHMETIC_OVERFLOW("Expression.ArithmeticOverflow"),
  ARGUMENT_OUT_OF_RANGE("Expression.ArgumentOutOfRange"),
  VARIABLE_VALUE_ERROR("Expression.InvalidVariable"),
  INVALID_OPERATOR("Expression.InvalidOperator"),
  INVALID_BOOLEAN("Expression.InvalidBoolean"),
  INVALID_DATE("Expression.InvalidDate"),
  INVALID_TIME("Expression.InvalidTime"),
  INVALID_TIMESTAMP("Expression.InvalidTimestamp"),  
  INVALID_INTEGER("Expression.InvalidInteger"),
  INVALID_NUMBER("Expression.InvalidNumber"), 
  INVALID_BIGNUMBER("Expression.InvalidBigNumber"),
  INVALID_JSON("Expression.InvalidJson"),
  INVALID_JSON_PATH("Expression.InvalidJsonPath"),
  INVALID_DATATYPE("Expression.InvalidDataType"),
  INVALID_DATEPART("Expression.InvalidDatePart"),
  INVALID_REGEXP_PATTERN("Expression.InvalidRegexpPattern"),
  INVALID_REGEXP_ESCAPE("Expression.InvalidRegexpEscape"),
  JSON_PATH_NOT_FOUND("Expression.JsonPathNotFound"),
  UDF_COMPILATION_ERROR("Expression.UdfCompilationError"),
  UNSUPPORTED_CONVERSION("Expression.UnsupportedConversion"),
  UNSUPPORTED_VALUEMETA("Expression.UnsupportedValueMeta"),
  UNSUPPORTED_JSON_TYPE("Expression.UnsupportedJsonType"),
  UNKNOWN_DATATYPE("Expression.UnknownDataType"),
  UNKNOWN_TIMEZONE("Expression.UnknownTimeZone"),
  BAD_FORMAT_PATTERN("Bad format {0} at position {1}"),
  CONVERSION_ERROR("Expression.ConversionError"),
  REGEXP_REPLACE_ERROR("Expression.RegexpReplaceError");

  private final String message;

  ExpressionError(final String message) {
    this.message = message;
  }

  public String message() {
    return BaseMessages.getString(ExpressionError.class, message);
  }

  public String message(Object... objects) {
    return BaseMessages.getString(ExpressionError.class, message, objects);
  }
}
