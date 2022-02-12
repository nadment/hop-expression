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

/**
 * Enumeration of the error which can be used in expression exception.
 */
public enum Error {
  INTERNAL_ERROR("Expression.InternalError"),
  SYNTAX_ERROR("Expression.SyntaxError"),
  DIVISION_BY_ZERO("Expression.DivisionByZero"),
  ILLEGAL_ARGUMENT("Expression.IllegalArgument"),
  UNRESOLVED_IDENTIFIER("Expression.UnresolvedIdentifier"),
  INVALID_IDENTIFIER_TYPE("Expression.InvalidIdentifierType"),
  //NOT_ENOUGH_ARGUMENT("Expression.NotEnoughArguments"),
  //TOO_MANY_ARGUMENT("Expression.TooManyArguments"),
  ARITHMETIC_OVERFLOW("Expression.ArithmeticOverflow"),
  ARGUMENT_OUT_OF_RANGE("Expression.ArgumentOutOfRange"),
  INVALID_DATE("Expression.InvalidDate"),
  INVALID_INTEGER("Expression.InvalidInteger"),
  INVALID_NUMBER("Expression.InvalidNumber"),
  INVALID_BIGNUMBER("Expression.InvalidBigNumber"),  
  INVALID_DATA_TYPE("Expression.InvalidDataType"),
  INVALID_DATE_PART("Expression.InvalidDatePart"),
  INVALID_REGEXP_PATTERN("Expression.InvalidRegexpPattern"),
  INVALID_REGEXP_ESCAPE("Expression.InvalidRegexpEscape"),
  UNEXPECTED_DATA_TYPE("Expression.UnexpectedDataType"),  
  UNSUPPORTED_CONVERSION("Expression.UnsupportedConversion"),
  FUNCTION_CALL_ERROR("Expression.FunctionError"),
  PARSE_ERROR("Expression.ParseError"),
  BAD_FORMAT_PATTERN("Bad format {0} at position {1}"),
  CONVERSION_ERROR("Expression.ConversionError"),
  REGEXP_REPLACE_ERROR("Expression.RegexpReplaceError"),
  URLENCODE_ERROR("Expression.UrlEncodeError"),
  URLDECODE_ERROR("Expression.UrlDecodeError")
  ;
  
  private final String message;
  
  Error(String message) {
    this.message = message;
  }

  public String message() {
    return message;
  }
}
