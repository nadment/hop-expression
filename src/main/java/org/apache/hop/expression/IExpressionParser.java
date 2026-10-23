/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression;

import java.util.List;
import org.apache.hop.expression.Token.Id;
import org.jspecify.annotations.NullMarked;

/**
 * The parser API given to a function to parse the arguments of its extended syntax, see {@link
 * Function#parse(IExpressionParser, Token)}.
 *
 * <p>It reads the tokens of the expression being parsed by {@link ExpressionParser}, it does not
 * start the parsing of a new expression.
 *
 * <p>The reserved words of the syntax are matched by their text, a core word of the grammar like
 * FROM or a word registered by the function with {@link Function#getKeywords()}.
 *
 * <p>Example: parse <code>MY_FUNC(value USING mode)</code>, the left parenthesis is already read
 *
 * <pre>{@code
 * IExpression value = parser.parseExpression();
 * parser.expectKeyword("USING", ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());
 * IExpression mode = parser.parseExpression();
 * parser.expect(Id.RPARENTHESIS, ErrorCode.MISSING_RIGHT_PARENTHESIS);
 * return new Call(token.start(), this, value, mode);
 * }</pre>
 */
@NullMarked
public interface IExpressionParser {

  /** Gets the position in the source of the next token, or the length of the source at the end. */
  int getPosition();

  /** Returns whether there is a next token. */
  boolean hasNext() throws ExpressionException;

  /** Raises an error if there is no next token. */
  void expectNext(ErrorCode errorCode, Object... values) throws ExpressionException;

  /** Reads the next token. */
  Token next() throws ExpressionException;

  /** Returns whether the next token is of the specified type, without reading it. */
  boolean isNext(Id id) throws ExpressionException;

  /** Reads the next token if it is of the specified type. */
  boolean accept(Id id) throws ExpressionException;

  /** Reads the next token, an error is raised if it is not of the specified type. */
  void expect(Id id, ErrorCode errorCode, Object... values) throws ExpressionException;

  /** Returns whether the next token is the reserved word (ignore case), without reading it. */
  boolean isNextKeyword(String word) throws ExpressionException;

  /** Reads the next token if it is the reserved word (ignore case). */
  boolean acceptKeyword(String word) throws ExpressionException;

  /** Reads the next token, an error is raised if it is not the reserved word (ignore case). */
  void expectKeyword(String word, ErrorCode errorCode, Object... values) throws ExpressionException;

  /** Parses a complete expression, like a function argument. */
  IExpression parseExpression() throws ExpressionException;

  /**
   * Parses a list of expressions separated by commas, empty if the next token is the right
   * parenthesis. The right parenthesis is not read.
   */
  List<IExpression> parseExpressionList() throws ExpressionException;

  /** Parses a primary expression: a term with its cast, element access or time zone. */
  IExpression parsePrimary() throws ExpressionException;

  /** Parses a string literal from the token. */
  IExpression parseLiteralString(Token token) throws ExpressionException;

  /** Parses a time unit literal from the token, like YEAR. */
  IExpression parseLiteralTimeUnit(Token token) throws ExpressionException;

  /** Parses a data type literal from the token, with its optional precision and scale. */
  IExpression parseDataType(Token token) throws ExpressionException;
}
