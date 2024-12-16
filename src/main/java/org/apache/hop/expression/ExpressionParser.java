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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.apache.hop.expression.Token.Id;
import org.apache.hop.expression.operator.FirstValueFunction;
import org.apache.hop.expression.operator.LastValueFunction;
import org.apache.hop.expression.operator.NthValueFunction;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.type.Types;
import org.apache.hop.expression.util.Characters;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.NumberFormat;

public class ExpressionParser {

  private static final Set<String> RESERVED_WORDS =
      Set.of(
          "AND",
          "ARRAY",
          "AS",
          "ASYMMETRIC",
          "AT",
          "BETWEEN",
          "BINARY",
          "CASE",
          "DATE",
          "DISTINCT",
          "ELSE",
          "END",
          "ESCAPE",
          "FALSE",
          "FORMAT",
          "FROM",
          "IGNORE",
          "ILIKE",
          "IN",
          "INET",
          "INTERVAL",
          "IS",
          "JSON",
          "KEY",
          "LIKE",
          "NOT",
          "NULL",
          "NULLS",
          "OR",
          "RESPECT",
          "RLIKE",
          "SIMILAR",
          "SYMMETRIC",
          "THEN",
          "TIME",
          "TIMESTAMP",
          "TO",
          "TRUE",
          "VALUE",
          "WHEN",
          "XOR",
          "ZONE");

  public static Set<String> getReservedWords() {
    return RESERVED_WORDS;
  }

  public static boolean isReservedWord(final String name) {
    if (name == null) return false;
    return RESERVED_WORDS.contains(name.toUpperCase());
  }

  private final String source;

  // Char position in source
  private int position = 0;

  // Index in tokens
  private int index = 0;

  private List<Token> tokens = new ArrayList<>();

  public ExpressionParser(final String source) {
    super();
    this.source = source;
  }

  public String getSource() {
    return source;
  }

  protected int getPosition() {

    if (index > 0 && index < tokens.size()) return tokens.get(index).start();

    return source.length();
  }

  protected boolean hasNext() {
    return index < tokens.size();
  }

  protected Token next() throws ExpressionException {
    if (!hasNext()) {
      throw new ExpressionParseException(
          getPosition(), ErrorCode.INTERNAL_ERROR, "Unexpected end of expression");
    }

    return tokens.get(index++);
  }

  protected void checkEndOfExpression(final Id id) {
    if (!hasNext()) {
      throw new ExpressionParseException(getPosition(), ErrorCode.SYNTAX_ERROR, id);
    }
  }

  protected Token previous() throws ExpressionException {
    if (index == 0) {
      throw new ExpressionParseException(
          getPosition(), ErrorCode.INTERNAL_ERROR, "No previous token");
    }

    return tokens.get(--index);
  }

  protected boolean is(final Id id) {
    if (hasNext()) {
      return tokens.get(index).is(id);
    }
    return false;
  }

  protected boolean isThenNext(final Id id) {
    if (hasNext() && tokens.get(index).is(id)) {
      index++;
      return true;
    }
    return false;
  }

  protected boolean isThenNext(final Id... ids) {
    for (Id id : ids) {
      if (hasNext() && tokens.get(index).is(id)) {
        index++;
        continue;
      }
      return false;
    }
    return true;
  }

  protected boolean isNotThenNext(final Id id) {
    return !this.isThenNext(id);
  }

  /** Parse the expression */
  public IExpression parse() throws ExpressionException {

    if (source == null) throw new ExpressionParseException(0, ErrorCode.NULL_SOURCE_ERROR);

    // Tokenize
    for (Token token = tokenize(); token != null; token = tokenize()) {

      // Ignore comment
      if (token.is(Id.COMMENT)) continue;

      tokens.add(token);
    }

    // Empty source return literal NULL
    if (tokens.isEmpty()) {
      return Literal.NULL;
    }

    IExpression expression = this.parseLogicalOr();

    // Unexpected end of expression
    if (hasNext()) {
      Token token = next();
      throw new ExpressionParseException(
          token.start(), ErrorCode.UNEXPECTED_CHARACTER, token.text());
    }

    return expression;
  }

  /**
   * Parse logical OR expression (Disjunction)
   *
   * <p>LogicalXorExpression ( OR LogicalXorExpression )*
   */
  private IExpression parseLogicalOr() throws ExpressionException {
    IExpression expression = this.parseLogicalXor();
    while (isThenNext(Id.OR)) {
      checkEndOfExpression(Id.OR);
      expression = new Call(getPosition(), Operators.BOOLOR, expression, parseLogicalXor());
    }

    return expression;
  }

  /**
   * Parse logical XOR expression (Exclusive disjunction)
   *
   * <p>LogicalAndExpression ( XOR LogicalAndExpression )*
   */
  private IExpression parseLogicalXor() throws ExpressionException {
    IExpression expression = this.parseLogicalAnd();
    while (isThenNext(Id.XOR)) {
      checkEndOfExpression(Id.XOR);
      expression = new Call(getPosition(), Operators.BOOLXOR, expression, parseLogicalAnd());
    }

    return expression;
  }

  /**
   * Parse logical AND expression (Conjunction)
   *
   * <p>LogicalNotExpression ( AND LogicalNotExpression )*
   *
   * @return Expression
   */
  private IExpression parseLogicalAnd() throws ExpressionException {
    IExpression expression = this.parseLogicalNot();
    while (isThenNext(Id.AND)) {
      checkEndOfExpression(Id.AND);
      expression = new Call(getPosition(), Operators.BOOLAND, expression, parseLogicalNot());
    }

    return expression;
  }

  /**
   * Parse logical NOT expression
   *
   * <p>[NOT] IdentityExpression
   */
  private IExpression parseLogicalNot() throws ExpressionException {
    if (isThenNext(Id.NOT)) {
      checkEndOfExpression(Id.NOT);
      return new Call(getPosition(), Operators.BOOLNOT, parseLogicalNot());
    }

    return this.parseConditional();
  }

  /**
   * Parse assertion IS expression
   *
   * <p>RelationalExpression IS [NOT] TRUE|FALSE|NULL RelationalExpression IS [NOT] DISTINCT FROM
   * RelationalExpression
   */
  private IExpression parseConditional() throws ExpressionException {
    IExpression expression = this.parseComparison();
    if (isThenNext(Id.IS)) {
      boolean not = false;
      int start = this.getPosition();

      if (isThenNext(Id.NOT)) {
        not = true;
      }

      checkEndOfExpression(Id.IS);

      switch (next().id()) {
        case TRUE:
          return new Call(start, (not) ? Operators.IS_NOT_TRUE : Operators.IS_TRUE, expression);
        case FALSE:
          return new Call(start, (not) ? Operators.IS_NOT_FALSE : Operators.IS_FALSE, expression);
        case NULL:
          return new Call(start, (not) ? Operators.IS_NOT_NULL : Operators.IS_NULL, expression);
        case DISTINCT:
          if (isThenNext(Id.FROM)) {
            checkEndOfExpression(Id.DISTINCT);
            return new Call(
                start,
                (not) ? Operators.IS_NOT_DISTINCT_FROM : Operators.IS_DISTINCT_FROM,
                expression,
                parseLogicalNot());
          }
          throw new ExpressionParseException(start, ErrorCode.SYNTAX_ERROR, Id.DISTINCT);
        default:
          throw new ExpressionParseException(start, ErrorCode.SYNTAX_ERROR, Id.IS);
      }
    }
    return expression;
  }

  /**
   * Parse IN, LIKE, BETWEEN, SIMILAR TO expression
   *
   * <p>BitwiseOrExpression ( [NOT] | InClause | BetweenClause | LikeClause BitwiseOrExpression)
   */
  private IExpression parseRelational() throws ExpressionException {
    IExpression expression = this.parseBitwiseOr();

    // Special case NOT before operation: <exp> [NOT] LIKE|ILIKE|IN|BETWEEN|SIMILAR <primaryExp>
    boolean not = false;
    if (isThenNext(Id.NOT)) {
      not = true;
    }

    if (isThenNext(Id.LIKE)) {
      IExpression pattern = this.parseBitwiseOr();

      if (isThenNext(Id.ESCAPE)) {
        IExpression escape = this.parseLiteralString(next());
        expression = new Call(getPosition(), Operators.LIKE, expression, pattern, escape);
      } else {
        expression = new Call(getPosition(), Operators.LIKE, expression, pattern);
      }

      if (not) {
        return new Call(Operators.BOOLNOT, expression);
      }
      return expression;
    } else if (isThenNext(Id.ILIKE)) {
      IExpression pattern = this.parseBitwiseOr();

      if (isThenNext(Id.ESCAPE)) {
        IExpression escape = this.parseBitwiseOr();
        expression = new Call(getPosition(), Operators.ILIKE, expression, pattern, escape);
      } else {
        expression = new Call(getPosition(), Operators.ILIKE, expression, pattern);
      }

      if (not) {
        return new Call(Operators.BOOLNOT, expression);
      }
      return expression;
    } else if (isThenNext(Id.IN)) {
      return new Call(
          getPosition(), not ? Operators.NOT_IN : Operators.IN, expression, this.parseElements());
    } else if (isThenNext(Id.BETWEEN)) {
      Operator operator = Operators.BETWEEN_ASYMMETRIC;
      if (isThenNext(Id.ASYMMETRIC)) {
        // Ignore
      } else if (isThenNext(Id.SYMMETRIC)) {
        operator = Operators.BETWEEN_SYMMETRIC;
      }
      checkEndOfExpression(Id.BETWEEN);
      IExpression start = this.parseBitwiseOr();
      if (isNotThenNext(Id.AND)) {
        throw new ExpressionParseException(getPosition(), ErrorCode.SYNTAX_ERROR, Id.BETWEEN);
      }
      checkEndOfExpression(Id.BETWEEN);
      IExpression end = this.parseBitwiseOr();

      expression = new Call(getPosition(), operator, expression, start, end);

      if (not) {
        return new Call(Operators.BOOLNOT, expression);
      }
      return expression;
    } else if (isThenNext(Id.SIMILAR)) {
      if (isThenNext(Id.TO)) {
        return new Call(
            getPosition(),
            not ? Operators.NOT_SIMILAR_TO : Operators.SIMILAR_TO,
            expression,
            parseBitwiseOr());
      } else throw new ExpressionParseException(getPosition(), ErrorCode.SYNTAX_ERROR, Id.SIMILAR);
    }

    if (not) {
      throw new ExpressionParseException(getPosition(), ErrorCode.SYNTAX_ERROR, Id.NOT);
    }

    return expression;
  }

  /**
   * BitwiseNotExpression ( (* | / | %) BitwiseNotExpression())*
   *
   * @return Expression
   */
  private IExpression parseFactor() throws ExpressionException {
    IExpression expression = this.parseBitwiseNot();

    while (hasNext()) {

      int start = this.getPosition();

      if (isThenNext(Id.MULTIPLY)) {
        checkEndOfExpression(Id.MULTIPLY);
        expression = new Call(start, Operators.MULTIPLY, expression, this.parseBitwiseNot());
      } else if (isThenNext(Id.DIVIDE)) {
        checkEndOfExpression(Id.DIVIDE);
        expression = new Call(start, Operators.DIVIDE, expression, this.parseBitwiseNot());
      } else if (isThenNext(Id.MODULUS)) {
        checkEndOfExpression(Id.MODULUS);
        expression = new Call(start, Operators.MODULUS, expression, this.parseBitwiseNot());
      } else break;
    }

    return expression;
  }

  /** ( UnaryExpression)* */
  private IExpression parseBitwiseNot() throws ExpressionException {

    int start = this.getPosition();

    if (isThenNext(Id.BITWISE_NOT)) {
      if (!hasNext()) {
        throw new ExpressionParseException(getPosition(), ErrorCode.SYNTAX_ERROR, Id.BITWISE_NOT);
      }
      return new Call(start, Operators.BITNOT, this.parseUnary());
    }
    return this.parseUnary();
  }

  /** ConcatenationExpression ( & ConcatenationExpression)* */
  private IExpression parseBitwiseAnd() throws ExpressionException {
    IExpression expression = this.parseConcatenation();
    while (isThenNext(Id.BITWISE_AND)) {
      checkEndOfExpression(Id.BITWISE_AND);
      expression = new Call(getPosition(), Operators.BITAND, expression, parseConcatenation());
    }
    return expression;
  }

  /** BitwiseXorExpression ( | BitwiseXorExpression)* */
  private IExpression parseBitwiseOr() throws ExpressionException {
    IExpression expression = this.parseBitwiseXor();
    while (isThenNext(Id.BITWISE_OR)) {
      checkEndOfExpression(Id.BITWISE_OR);
      expression = new Call(getPosition(), Operators.BITOR, expression, parseBitwiseXor());
    }
    return expression;
  }

  /** BitwiseAndExpression ( ^ BitwiseAndExpression)* */
  private IExpression parseBitwiseXor() throws ExpressionException {
    IExpression expression = this.parseBitwiseAnd();
    while (isThenNext(Id.BITWISE_XOR)) {
      checkEndOfExpression(Id.BITWISE_XOR);
      expression = new Call(getPosition(), Operators.BITXOR, expression, this.parseBitwiseAnd());
    }
    return expression;
  }

  /** (('+' | '-') PrimaryExpression ) */
  private IExpression parseUnary() throws ExpressionException {
    int start = this.getPosition();
    if (isThenNext(Id.MINUS)) {
      return new Call(start, Operators.NEGATE, this.parsePrimary());
    }
    if (isThenNext(Id.PLUS)) {
      // Ignore
    }

    return this.parsePrimary();
  }

  /** Literal String */
  private IExpression parseLiteralString(Token token) {
    return Literal.of(token.text());
  }

  /** Literal Json */
  private IExpression parseLiteralJson(Token token) throws ExpressionException {
    return new Call(
        token.start(), Operators.CAST, Literal.of(token.text()), Literal.of(Types.JSON));
  }

  /** Literal Inet */
  private IExpression parseLiteralInet(Token token) throws ExpressionException {
    return new Call(
        token.start(), Operators.CAST, Literal.of(token.text()), Literal.of(Types.INET));
  }

  /** Cast operator <term>::<datatype> | <term> AT TIMEZONE <timezone> | <array>[<index>] */
  private IExpression parsePrimary() throws ExpressionException {
    IExpression expression = this.parseTerm();

    // Cast operator ::
    if (isThenNext(Id.CAST)) {
      checkEndOfExpression(Id.CAST);
      IExpression type = parseLiteralDataType(next());
      return new Call(getPosition(), Operators.CAST, expression, type);
    }

    // Array element at ARRAY[index]
    if (isThenNext(Id.LBRACKET)) {
      IExpression term = this.parseBitwiseOr();
      Call call = new Call(getPosition(), Operators.ELEMENT_AT, expression, term);
      if (isNotThenNext(Id.RBRACKET)) {
        throw new ExpressionParseException(getPosition(), ErrorCode.MISSING_RIGHT_BRACKET);
      }
      return call;
    }
    if (isThenNext(Id.AT)) {
      checkEndOfExpression(Id.AT);
      if (isThenNext(Id.TIME)) {
        checkEndOfExpression(Id.TIME);
        if (isThenNext(Id.ZONE)) {
          checkEndOfExpression(Id.ZONE);
          return new Call(getPosition(), Operators.AT_TIME_ZONE, expression, this.parseTerm());
        }
      }
      throw new ExpressionParseException(getPosition(), ErrorCode.SYNTAX_ERROR, Id.AT);
    }
    return expression;
  }

  /** Term = Literal | Identifier | Function | '(' Expression ')' */
  private IExpression parseTerm() throws ExpressionException {
    Token token = next();
    switch (token.id()) {
      case TRUE:
        return Literal.TRUE;
      case FALSE:
        return Literal.FALSE;
      case NULL:
        return Literal.NULL;
      case IDENTIFIER:
        return new Identifier(token.start(), token.text());
      case LITERAL_STRING:
        return parseLiteralString(token);
      case LITERAL_NUMERIC_DECIMAL:
        return parseLiteralNumericDecimal(token);
      case LITERAL_NUMERIC_HEXA:
        return parseLiteralNumericHexa(token);
      case LITERAL_NUMERIC_BINARY:
        return parseLiteralNumericBinary(token);
      case LITERAL_NUMERIC_OCTAL:
        return parseLiteralNumericOctal(token);
      case LITERAL_TIMEUNIT:
        return parseLiteralTimeUnit(token);
      case LITERAL_DATATYPE:
        return parseLiteralDataType(token);
      case DATE:
        checkEndOfExpression(Id.DATE);
        token = next();
        if (token == null) break;
        return parseLiteralDate(token);
      case TIMESTAMP:
        checkEndOfExpression(Id.TIMESTAMP);
        token = next();
        if (token == null) break;
        return parseLiteralTimestamp(token);
      case BINARY:
        checkEndOfExpression(Id.BINARY);
        token = next();
        if (token == null) break;
        return parseLiteralBinary(token);
      case INET:
        checkEndOfExpression(Id.INET);
        token = next();
        if (token == null) break;
        return parseLiteralInet(token);
      case JSON:
        checkEndOfExpression(Id.JSON);
        token = next();
        if (token == null) break;
        return parseLiteralJson(token);
      case INTERVAL:
        checkEndOfExpression(Id.INTERVAL);
        return parseLiteralInterval(token);
      case CASE:
        return parseCase();
      case FUNCTION:
        return parseFunction(token);
      case LBRACKET:
        return parseArray(token);
      case LPARENTHESIS:
        if (hasNext()) {
          IExpression expression = this.parseLogicalOr();
          if (isThenNext(Id.RPARENTHESIS)) {
            return expression;
          }
        }
        throw new ExpressionParseException(token.start(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
      default:
        // Syntax error
    }
    throw new ExpressionParseException(position, ErrorCode.SYNTAX_ERROR, token);
  }

  /** RelationalExpression ( Operator RelationalExpression ) */
  private IExpression parseComparison() throws ExpressionException {
    IExpression expression = this.parseRelational();
    int start = this.getPosition();
    if (isThenNext(Id.EQUAL)) {
      checkEndOfExpression(Id.EQUAL);
      return new Call(start, Operators.EQUAL, expression, this.parseRelational());
    }
    if (isThenNext(Id.NOT_EQUAL)) {
      checkEndOfExpression(Id.NOT_EQUAL);
      return new Call(start, Operators.NOT_EQUAL, expression, this.parseRelational());
    }
    if (isThenNext(Id.GT)) {
      checkEndOfExpression(Id.GT);
      return new Call(start, Operators.GREATER_THAN, expression, this.parseRelational());
    }
    if (isThenNext(Id.GTE)) {
      checkEndOfExpression(Id.GTE);
      return new Call(start, Operators.GREATER_THAN_OR_EQUAL, expression, this.parseRelational());
    }
    if (isThenNext(Id.LT)) {
      checkEndOfExpression(Id.LT);
      return new Call(start, Operators.LESS_THAN, expression, this.parseRelational());
    }
    if (isThenNext(Id.LTE)) {
      checkEndOfExpression(Id.LTE);
      return new Call(start, Operators.LESS_THAN_OR_EQUAL, expression, this.parseRelational());
    }

    return expression;
  }

  /** FactorExpression ( (+ | - ) FactorExpression )* */
  private IExpression parseAdditive() throws ExpressionException {
    IExpression expression = this.parseFactor();
    while (hasNext()) {
      int start = this.getPosition();

      if (isThenNext(Id.PLUS)) {
        checkEndOfExpression(Id.PLUS);
        expression = new Call(start, Operators.ADD, expression, this.parseFactor());
      } else if (isThenNext(Id.MINUS)) {
        checkEndOfExpression(Id.MINUS);
        expression = new Call(start, Operators.SUBTRACT, expression, this.parseFactor());
      } else if (isThenNext(Id.CONCAT)) {
        checkEndOfExpression(Id.CONCAT);
        expression = new Call(start, Operators.CONCAT, expression, this.parseFactor());
      } else break;
    }

    return expression;
  }

  /** [exp1, exp2...] * */
  private IExpression parseArray(final Token token) throws ExpressionException {

    if (!hasNext()) {
      throw new ExpressionParseException(token.start(), ErrorCode.MISSING_RIGHT_BRACKET);
    }

    List<IExpression> operands = new ArrayList<>();

    // Empty array
    if (isThenNext(Id.RBRACKET)) {
      return new Array(operands);
    }

    do {
      checkEndOfExpression(Id.RBRACKET);
      operands.add(this.parseLogicalOr());
    } while (isThenNext(Id.COMMA));

    if (isNotThenNext(Id.RBRACKET)) {
      throw new ExpressionParseException(token.start(), ErrorCode.MISSING_RIGHT_BRACKET);
    }

    return new Array(operands);
  }

  /** AdditiveExpression ( || AdditiveExpression )* */
  private IExpression parseConcatenation() throws ExpressionException {
    IExpression expression = this.parseAdditive();
    while (isThenNext(Id.CONCAT)) {
      expression = new Call(getPosition(), Operators.CONCAT, expression, this.parseAdditive());
    }
    return expression;
  }

  private IExpression parseLiteralNumericDecimal(final Token token) throws ExpressionException {
    BigDecimal number = NumberFormat.of("TM").parse(token.text());
    try {
      return Literal.of(number.longValueExact());
    } catch (ArithmeticException e) {
      return Literal.of(number);
    }
  }

  private IExpression parseLiteralBinary(final Token token) throws ExpressionException {
    try {
      String str = token.text();
      if (str.length() % 2 > 0) str = '0' + str;
      byte[] bytes = new byte[str.length() / 2];
      for (int i = 0; i < bytes.length; i++) {
        int start = i * 2;
        bytes[i] = (byte) Integer.parseInt(str.substring(start, start + 2), 16);
      }

      // Return as BINARY
      return Literal.of(bytes);
    } catch (Exception e) {
      throw new ExpressionParseException(token.start(), ErrorCode.UNPARSABLE_BINARY, token.text());
    }
  }

  private IExpression parseLiteralNumericHexa(final Token token) {
    String str = token.text().substring(2);
    BigInteger value = new BigInteger(str, 16);
    try {
      return Literal.of(value.longValueExact());
    } catch (ArithmeticException e) {
      return Literal.of(new BigDecimal(value));
    }
  }

  private IExpression parseLiteralNumericOctal(final Token token) {
    String str = token.text().substring(2);
    BigInteger value = new BigInteger(str, 8);
    try {
      return Literal.of(value.longValueExact());
    } catch (ArithmeticException e) {
      return Literal.of(new BigDecimal(value));
    }
  }

  private IExpression parseLiteralNumericBinary(final Token token) {
    String str = token.text().substring(2);
    BigInteger value = new BigInteger(str, 2);
    try {
      return Literal.of(value.longValueExact());
    } catch (ArithmeticException e) {
      return Literal.of(new BigDecimal(value));
    }
  }

  /**
   * Parses a date literal. The parsing is strict and requires months to be between 1 and 12, days
   * to be less than 31, etc.
   */
  private IExpression parseLiteralDate(final Token token) throws ExpressionException {
    try {
      // Literal date use exact mode ISO 8601
      DateTimeFormat format = DateTimeFormat.of("FXYYYY-MM-DD");
      ZonedDateTime datetime = format.parse(token.text());

      return Literal.of(datetime);
    } catch (Exception e) {
      throw new ExpressionParseException(token.start(), ErrorCode.INVALID_DATE, token.text());
    }
  }

  /**
   * Parses a timestamp literal with ISO Formats.
   *
   * <p>The parsing is strict and requires months to be between 1 and 12, days to be less than 31,
   * etc.
   */
  private IExpression parseLiteralTimestamp(final Token token) throws ExpressionException {
    try {
      DateTimeFormat format = DateTimeFormat.of("AUTO");
      ZonedDateTime datetime = format.parse(token.text());

      return Literal.of(datetime);
    } catch (Exception e) {
      throw new ExpressionParseException(token.start(), ErrorCode.INVALID_TIMESTAMP, token.text());
    }
  }

  /** Parses a list of expressions separated by commas. (expression [,expression...] ) */
  private Array parseElements() throws ExpressionException {

    List<IExpression> list = new ArrayList<>();

    if (isNotThenNext(Id.LPARENTHESIS)) {
      throw new ExpressionParseException(getPosition(), ErrorCode.MISSING_LEFT_PARENTHESIS);
    }

    do {
      try {
        list.add(parseLogicalOr());
      } catch (ExpressionException e) {
        throw new ExpressionParseException(getPosition(), ErrorCode.SYNTAX_ERROR, Id.IN);
      }

      if (isThenNext(Id.COMMA)) {
        continue;
      }

      if (isThenNext(Id.RPARENTHESIS)) {
        return new Array(list);
      }

      break;
    } while (true);

    throw new ExpressionParseException(getPosition(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
  }

  /** Case When Then Else End ) */
  private IExpression parseCase() throws ExpressionException {
    IExpression valueExpression = Literal.NULL;
    IExpression elseExpression = Literal.NULL;
    List<IExpression> whenList = new ArrayList<>();
    List<IExpression> thenList = new ArrayList<>();

    int start = this.getPosition();
    boolean simple = false;

    // Simple case
    if (!is(Id.WHEN)) {
      valueExpression = this.parseLogicalOr();
      simple = true;
    }

    while (isThenNext(Id.WHEN)) {
      IExpression expression = this.parseLogicalOr();

      // Simple case with multi values
      if (simple && isThenNext(Id.COMMA)) {
        List<IExpression> values = new ArrayList<>();
        values.add(expression);

        do {
          values.add(this.parseLogicalOr());
        } while (this.isThenNext(Id.COMMA));

        expression = new Array(values);
      }
      whenList.add(expression);

      if (isNotThenNext(Id.THEN)) {
        throw new ExpressionParseException(
            getPosition(), ErrorCode.SYNTAX_ERROR_CASE_STATEMENT, Id.CASE);
      }
      thenList.add(this.parseLogicalOr());
    }

    if (isThenNext(Id.ELSE)) {
      elseExpression = this.parseLogicalOr();
    }

    if (isNotThenNext(Id.END)) {
      throw new ExpressionParseException(
          getPosition(), ErrorCode.SYNTAX_ERROR_CASE_STATEMENT, Id.CASE);
    }

    return new Call(
        start,
        simple ? Operators.CASE_SIMPLE : Operators.CASE_SEARCH,
        valueExpression,
        new Array(whenList),
        new Array(thenList),
        elseExpression);
  }

  /** Cast function CAST(value AS type [FORMAT pattern]) */
  private IExpression parseFunctionCast(final Token token, final Function function)
      throws ExpressionException {
    List<IExpression> operands = new ArrayList<>();

    operands.add(this.parseLogicalOr());

    if (isNotThenNext(Id.AS)) {
      throw new ExpressionParseException(
          token.start(), ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());
    }

    if (!hasNext() || is(Id.RPARENTHESIS)) {
      throw new ExpressionParseException(
          token.end(), ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());
    }

    operands.add(this.parseLiteralDataType(next()));

    if (isThenNext(Id.FORMAT)) {
      if (!hasNext()) {
        throw new ExpressionParseException(
            token.end(), ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());
      }
      Token format = next();
      if (format.is(Id.LITERAL_STRING)) operands.add(this.parseLiteralString(format));
      else
        throw new ExpressionParseException(
            format.start(), ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());
    }

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionParseException(token.end(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
    }

    return new Call(token.start(), function, operands);
  }

  /** Parse POSITION(expression IN expression) */
  private IExpression parseFunctionPosition(final Token token, final Function function)
      throws ExpressionException {

    if (!hasNext()) {
      throw new ExpressionParseException(
          token.end(), ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());
    }

    List<IExpression> operands = new ArrayList<>();
    operands.add(this.parseConcatenation());

    if (isNotThenNext(Id.IN)) {
      throw new ExpressionParseException(
          getPosition(), ErrorCode.SYNTAX_ERROR_FUNCTION, function.getName());
    }

    if (!hasNext()) {
      throw new ExpressionParseException(
          token.end(), ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());
    }

    operands.add(this.parseConcatenation());

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionParseException(token.end(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
    }

    return new Call(token.start(), function, operands);
  }

  /** Parse <code>FIRST_VALUE(expression) [ IGNORE NULLS | RESPECT NULLS ]</code> */
  private IExpression parseFunctionFirstValue(Token token, Function function)
      throws ExpressionException {

    IExpression operand = this.parseLogicalOr();

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionParseException(token.end(), ErrorCode.MISSING_LEFT_PARENTHESIS);
    }

    // NULL treatment clause
    if (isThenNext(Id.IGNORE, Id.NULLS)) {
      function = FirstValueFunction.FIRST_VALUE_IGNORE_NULLS;
    } else if (isThenNext(Id.RESPECT, Id.NULLS)) {
      function = FirstValueFunction.FIRST_VALUE_RESPECT_NULLS;
    }

    return new Call(token.start(), function, operand);
  }

  /** Parse <code>LAST_VALUE(expression) [ IGNORE NULLS | RESPECT NULLS ]</code> */
  private IExpression parseFunctionLastValue(Token token, Function function)
      throws ExpressionException {

    IExpression operand = this.parseLogicalOr();

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionParseException(token.end(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
    }

    // NULL treatment clause
    if (isThenNext(Id.IGNORE, Id.NULLS)) {
      function = LastValueFunction.LAST_VALUE_IGNORE_NULLS;
    } else if (isThenNext(Id.RESPECT, Id.NULLS)) {
      function = LastValueFunction.LAST_VALUE_RESPECT_NULLS;
    }

    return new Call(token.start(), function, operand);
  }

  /** Parse <code>NTH_VALUE(expression, offset) [ IGNORE NULLS | RESPECT NULLS ]</code> */
  private IExpression parseFunctionNthValue(Token token, Function function)
      throws ExpressionException {

    List<IExpression> operands = new ArrayList<>();
    operands.add(this.parseLogicalOr());

    if (isNotThenNext(Id.COMMA)) {
      throw new ExpressionParseException(
          getPosition(), ErrorCode.SYNTAX_ERROR_FUNCTION, function.getName());
    }
    operands.add(this.parseLogicalOr());

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionParseException(token.end(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
    }

    // NULL treatment clause
    if (isThenNext(Id.IGNORE, Id.NULLS)) {
      function = NthValueFunction.NTH_VALUE_IGNORE_NULLS;
    } else if (isThenNext(Id.RESPECT, Id.NULLS)) {
      function = NthValueFunction.NTH_VALUE_RESPECT_NULLS;
    }

    return new Call(token.start(), function, operands);
  }

  /** EXTRACT(part FROM expression) */
  private IExpression parseFunctionExtract(Token token, final Function function)
      throws ExpressionException {

    if (!hasNext()) {
      throw new ExpressionParseException(
          getPosition(), ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());
    }

    List<IExpression> operands = new ArrayList<>();
    operands.add(this.parseLiteralTimeUnit(next()));

    if (isNotThenNext(Id.FROM)) {
      throw new ExpressionParseException(
          getPosition(), ErrorCode.SYNTAX_ERROR_FUNCTION, function.getName());
    }

    if (!hasNext()) {
      throw new ExpressionParseException(
          getPosition(), ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());
    }

    operands.add(this.parseLogicalOr());

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionParseException(token.start(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
    }

    return new Call(token.start(), function, operands);
  }

  /** COUNT(*) | COUNT([DISTINCT] expression) */
  private IExpression parseFunctionCount(Token token, Function function)
      throws ExpressionException {

    AggregateFunction aggregator = Operators.COUNT_VALUE;
    List<IExpression> operands = new ArrayList<>();

    // COUNT(*) no operand
    if (isThenNext(Id.MULTIPLY)) {
      aggregator = Operators.COUNT_ALL;
    } else if (isThenNext(Id.DISTINCT)) {
      operands.add(this.parsePrimary());
      aggregator = Operators.COUNT_DISTINCT;
    } else {
      operands.add(this.parsePrimary());
    }

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionParseException(token.start(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
    }

    return new Call(token.start(), aggregator, operands);
  }

  /** LISTAGG([DISTINCT] expression [, delimiter] ) */
  private IExpression parseFunctionListAgg(Token token, final Function function)
      throws ExpressionException {

    AggregateFunction aggregator = Operators.LISTAGG_ALL;

    List<IExpression> operands = new ArrayList<>();

    if (isThenNext(Id.DISTINCT)) {
      operands.add(this.parsePrimary());
      aggregator = Operators.LISTAGG_DISTINCT;
    } else {
      operands.add(this.parsePrimary());
    }

    // Optional delimiter
    if (isThenNext(Id.COMMA)) {
      operands.add(this.parsePrimary());
    }

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionParseException(token.start(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
    }

    return new Call(token.start(), aggregator, operands);
  }

  /** JSON_OBJECT([KEY] key VALUE expression [, [KEY] key VALUE expression]...) */
  private IExpression parseFunctionJsonObject(Token token, final Function function)
      throws ExpressionException {

    List<IExpression> operands = new ArrayList<>();
    boolean comma;
    do {

      if (isThenNext(Id.KEY)) {
        // KEY is optional
      }

      operands.add(this.parseLiteralString(next()));

      if (isThenNext(Id.VALUE)) {
        operands.add(this.parsePrimary());
      } else {
        throw new ExpressionParseException(
            token.start(), ErrorCode.SYNTAX_ERROR, function.getName());
      }

      comma = false;
      if (is(Id.COMMA)) {
        comma = true;
        token = next();
      }
    } while (comma);

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionParseException(token.start(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
    }

    return new Call(token.start(), function, operands);
  }

  /** Function */
  private IExpression parseFunction(final Token token) throws ExpressionException {
    Function function = FunctionRegistry.getFunction(token.text());

    // Function is never null
    if (function == null) {
      throw new ExpressionParseException(
          token.start(), ErrorCode.FUNCTION_DOES_NOT_EXIST, token.text());
    }

    if (isNotThenNext(Id.LPARENTHESIS)) {
      throw new ExpressionParseException(token.end(), ErrorCode.MISSING_LEFT_PARENTHESIS);
    }

    List<IExpression> operands = new ArrayList<>();

    // No param function
    if (isThenNext(Id.RPARENTHESIS)) {
      return new Call(token.start(), function, operands);
    }

    // Function with custom syntax
    switch (token.text()) {
      case "CAST", "TRY_CAST":
        return parseFunctionCast(token, function);
      case "EXTRACT":
        return parseFunctionExtract(token, function);
      case "POSITION":
        return parseFunctionPosition(token, function);
      case "COUNT":
        return parseFunctionCount(token, function);
      case "FIRST_VALUE":
        return this.parseFunctionFirstValue(token, function);
      case "LAST_VALUE":
        return this.parseFunctionLastValue(token, function);
      case "NTH_VALUE":
        return this.parseFunctionNthValue(token, function);
      case "LISTAGG":
        return this.parseFunctionListAgg(token, function);
      case "JSON_OBJECT":
        return this.parseFunctionJsonObject(token, function);
    }

    if (!hasNext()) {
      throw new ExpressionParseException(
          getPosition(), ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());
    }
    operands.add(this.parseLogicalOr());

    while (isThenNext(Id.COMMA)) {
      if (!hasNext()) {
        throw new ExpressionParseException(
            getPosition(), ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());
      }
      operands.add(this.parseLogicalOr());
    }

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionParseException(token.start(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
    }

    return new Call(token.start(), function, operands);
  }

  private IExpression parseLiteralTimeUnit(Token token) throws ExpressionException {
    TimeUnit unit = TimeUnit.of(token.text());
    if (unit == null) {
      throw new ExpressionParseException(token.start(), ErrorCode.INVALID_TIMEUNIT, token.text());
    }
    return Literal.of(unit);
  }

  private IExpression parseLiteralInterval(Token token) throws ExpressionException {

    boolean negative = false;
    this.checkEndOfExpression(Id.INTERVAL);
    Token value = next();
    if (value == null)
      throw new ExpressionParseException(token.start(), ErrorCode.INVALID_INTERVAL);
    if (value.is(Id.MINUS)) {
      negative = true;
      this.checkEndOfExpression(Id.INTERVAL);
      value = next();
      if (value == null)
        throw new ExpressionParseException(token.start(), ErrorCode.INVALID_INTERVAL);
    }

    TimeUnit startUnit = null;
    TimeUnit endUnit = null;

    checkEndOfExpression(Id.INTERVAL);
    Token start = next();
    startUnit = TimeUnit.of(start.text());

    // Verbose format
    if (startUnit == null) {
      previous();

      Interval interval = Interval.of(value.text());
      return Literal.of(interval);
    }

    // Short format with qualifier

    String text = value.text();
    if (value.is(Id.LITERAL_STRING)) {
      Token end = null;
      if (this.isThenNext(Id.TO)) {
        checkEndOfExpression(Id.INTERVAL);
        end = next();
        if (end == null)
          throw new ExpressionParseException(token.start(), ErrorCode.INVALID_INTERVAL);
        endUnit = TimeUnit.of(end.text());
      }

      if (text.length() > 0 && text.charAt(0) == '-') {
        negative = true;
        text = text.substring(1);
      }
    }

    IntervalQualifier qualifier = IntervalQualifier.of(startUnit, endUnit);
    if (qualifier == null)
      throw new ExpressionParseException(token.start(), ErrorCode.INVALID_INTERVAL);

    Interval interval = qualifier.parse(text);
    if (interval == null)
      throw new ExpressionParseException(token.start(), ErrorCode.INVALID_INTERVAL);

    if (negative) interval = interval.negate();

    return Literal.of(interval);
  }

  private IExpression parseLiteralDataType(Token token) throws ExpressionException {
    return Literal.of(parseType(token));
  }

  private Type parseType(Token token) throws ExpressionException {

    TypeName name = TypeName.of(token.text());
    if (name != null) {
      int precision = Type.PRECISION_NOT_SPECIFIED;
      int scale = Type.SCALE_NOT_SPECIFIED;

      if (isThenNext(Id.LPARENTHESIS)) {
        boolean scaleFound = false;

        // Precision
        precision = Integer.parseInt(this.next().text());

        // Scale
        if (isThenNext(Id.COMMA)) {
          scale = Integer.parseInt(this.next().text());
          scaleFound = true;
        }

        if (isNotThenNext(Id.RPARENTHESIS)) {
          throw new ExpressionParseException(token.start(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
        }

        if (!name.supportsPrecision())
          throw new ExpressionParseException(token.start(), ErrorCode.INVALID_TYPE, token.text());
        if (!name.supportsScale() && scaleFound)
          throw new ExpressionParseException(token.start(), ErrorCode.INVALID_TYPE, token.text());
      }

      switch (name) {
        case BOOLEAN:
          return Types.BOOLEAN;
        case INTEGER:
          return IntegerType.of(precision);
        case NUMBER:
          return NumberType.of(precision, scale);
        case STRING:
          return StringType.of(precision);
        case BINARY:
          return BinaryType.of(precision);
        case DATE:
          return Types.DATE;
        case INET:
          return Types.INET;
        case JSON:
          return Types.JSON;
        case INTERVAL:
          return Types.INTERVAL;
        default:
      }
    }

    throw new ExpressionParseException(token.start(), ErrorCode.INVALID_TYPE, token.text());
  }

  /** Parses an expression string to return the individual tokens. */
  protected Token tokenize() throws ExpressionException {

    while (position < source.length()) {
      char c = source.charAt(position);

      switch (c) {
        case ',':
          return new Token(Id.COMMA, position++);

        case '(':
          return new Token(Id.LPARENTHESIS, position++);

        case ')':
          return new Token(Id.RPARENTHESIS, position++);

        case '[':
          return new Token(Id.LBRACKET, position++);

        case ']':
          return new Token(Id.RBRACKET, position++);

          // Single-quoted literal text.
        case '\'':
          {
            StringBuilder text = new StringBuilder();
            int start = position++;
            while (position < source.length()) {
              c = source.charAt(position++);
              if (c == '\'') {
                if (position < source.length()) {
                  char c2 = source.charAt(position);
                  // encountered consecutive single-quotes
                  if (c2 == '\'') {
                    ++position;
                    text.append(c);
                    continue;
                  }
                }
                break;
              }
              text.append(c);
            }

            if (c != '\'') {
              throw new ExpressionParseException(start, ErrorCode.MISSING_END_SINGLE_QUOTED_STRING);
            }

            return new Token(Id.LITERAL_STRING, start, position, text.toString());
          }

        case '=':
          return new Token(Id.EQUAL, position++);

        case '+':
          return new Token(Id.PLUS, position++);

        case '-':
          {
            // Single line comment --
            if (position + 1 < source.length() && source.charAt(position + 1) == '-') {
              int start = position;
              position++;
              while (position < source.length()) {
                c = source.charAt(position);
                if (c == '\r' || c == '\n') break;
                position++;
              }
              return new Token(Id.COMMENT, start, position, source.substring(start, position));
            }

            // Minus sign
            return new Token(Id.MINUS, position++);
          }

        case '*':
          return new Token(Id.MULTIPLY, position++);

        case '%':
          return new Token(Id.MODULUS, position++);

        case '<':
          {
            // parse less symbol
            int start = position++;
            if (position < source.length()) {
              c = source.charAt(position);
              if (c == '=') {
                position++;
                return new Token(Id.LTE, start);
              }
              if (c == '>') {
                position++;
                return new Token(Id.NOT_EQUAL, start);
              }
            }
            return new Token(Id.LT, start);
          }

          // parse greater symbol
        case '>':
          {
            int start = position++;
            if (position < source.length()) {
              c = source.charAt(position);
              if (c == '=') {
                position++;
                return new Token(Id.GTE, start);
              }
            }
            return new Token(Id.GT, start);
          }

          // parse bang equal symbol "!="
        case '!':
          {
            int start = position++;
            if (position < source.length()) {
              c = source.charAt(position);
              if (c == '=') {
                position++;
                return new Token(Id.NOT_EQUAL, start);
              }
            }
            throw new ExpressionParseException(start, ErrorCode.UNEXPECTED_CHARACTER, '!');
          }

          // cast operator
        case ':':
          {
            int start = position++;
            if (position < source.length()) {
              c = source.charAt(position);
              if (c == ':') {
                position++;
                return new Token(Id.CAST, start);
              }
            }
            throw new ExpressionParseException(start, ErrorCode.UNEXPECTED_CHARACTER, ':');
          }

          // possible start of '/*' or '//' comment
        case '/':
          {
            int start = position++;
            if (position < source.length()) {
              char c1 = source.charAt(position);
              // Block comment
              if (c1 == '*') {
                int level = 1;

                while (level > 0) {
                  int end = source.indexOf('*', position + 1);
                  if (end > 0 && end < source.length() - 1) {
                    // nested block comment
                    if (source.charAt(end - 1) == '/') {
                      level++;
                      position = end;
                      continue;
                    }
                    if (source.charAt(end + 1) == '/') {
                      level--;
                      position = end + 2;
                    } else position++;
                  } else {
                    throw new ExpressionParseException(start, ErrorCode.MISSING_END_BLOCK_COMMENT);
                  }
                }

                return new Token(Id.COMMENT, start, position, source.substring(start, position));
              }
              // Line comment
              if (c1 == '/') {
                position++;

                while (position < source.length()) {
                  c = source.charAt(position);
                  if (c == '\r' || c == '\n') break;
                  position++;
                }

                return new Token(Id.COMMENT, start, position, source.substring(start, position));
              }
            }
            return new Token(Id.DIVIDE, start);
          }

        case '~':
          return new Token(Id.BITWISE_NOT, position++);

        case '&':
          return new Token(Id.BITWISE_AND, position++);

        case '^':
          return new Token(Id.BITWISE_XOR, position++);

          // Bitwise OR operator or concat symbol
        case '|':
          {
            int start = position++;
            if (position < source.length()) {
              c = source.charAt(position);
              if (c == '|') {
                position++;
                return new Token(Id.CONCAT, start);
              }
            }
            return new Token(Id.BITWISE_OR, start);
          }

          // Quoted identifier matching reserved words or with white space
        case '"':
          {
            int start = position++;
            while (position < source.length()) {
              c = source.charAt(position++);
              if (c == '"') {
                String value = source.substring(start + 1, position - 1).toUpperCase();
                return new Token(Id.IDENTIFIER, start, position, value);
              }
            }
            throw new ExpressionParseException(start, ErrorCode.MISSING_END_DOUBLE_QUOTED_STRING);
          }

        case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.': // Number without zero .1
          {
            int start = position;
            char previous;
            boolean error = false;

            if (c == '0' && position + 1 < source.length()) {
              c = source.charAt(position + 1);

              // Literal hexadecimal numeric 0xABC_DEF
              if (c == 'x' || c == 'X') {
                position += 2;
                previous = c;
                while (position < source.length()) {
                  c = source.charAt(position);
                  if (Characters.isHexDigit(c) || c == '_') {
                    position++;
                    if (c == '_' && c == previous) error = true;
                    previous = c;
                  } else {
                    break;
                  }
                }

                String str = source.substring(start, position);
                // Empty, consecutive underscore or last char is underscore
                if (str.length() == 2 || previous == '_' || error) {
                  throw new ExpressionParseException(position, ErrorCode.INVALID_NUMBER, str);
                }

                return new Token(Id.LITERAL_NUMERIC_HEXA, start, position, str.replace("_", ""));
              }

              // Literal binary numeric 0b0110_1011
              if (source.charAt(position + 1) == 'b' || source.charAt(position + 1) == 'B') {
                position += 2;
                previous = c;
                while (position < source.length()) {
                  c = source.charAt(position);
                  if (Characters.isBitDigit(c) || c == '_') {
                    position++;
                    if (c == '_' && c == previous) error = true;
                    previous = c;
                  } else {
                    break;
                  }
                }

                String str = source.substring(start, position);
                // Empty, consecutive underscore or last char is underscore
                if (str.length() == 2 || previous == '_' || error) {
                  throw new ExpressionParseException(position, ErrorCode.INVALID_NUMBER, str);
                }

                return new Token(Id.LITERAL_NUMERIC_BINARY, start, position, str.replace("_", ""));
              }

              // Literal octal numeric 0o1234567
              if (source.charAt(position + 1) == 'o' || source.charAt(position + 1) == 'O') {
                position += 2;
                previous = c;
                while (position < source.length()) {
                  c = source.charAt(position);
                  if (Characters.isOctDigit(c) || c == '_') {
                    position++;
                    if (c == '_' && c == previous) error = true;
                    previous = c;
                  } else {
                    break;
                  }
                }

                String str = source.substring(start, position);
                // Empty, consecutive underscore or last char is underscore
                if (str.length() == 2 || previous == '_' || error) {
                  throw new ExpressionParseException(position, ErrorCode.INVALID_NUMBER, str);
                }

                return new Token(Id.LITERAL_NUMERIC_OCTAL, start, position, str.replace("_", ""));
              }
            }

            // Integer part
            previous = c;
            while (position < source.length()) {
              c = source.charAt(position);
              if (Characters.isDigit(c) || c == '_') {
                position++;
                if (c == '_' && c == previous) error = true;
                previous = c;
              } else {
                break;
              }
            }

            // Last char should not be a underscore
            if (previous == '_') {
              error = true;
            }

            // Use dot for decimal separator
            if (position < source.length() && c == '.') {
              c = source.charAt(position++);
              previous = '_';
            }

            // Decimal part
            while (position < source.length()) {
              c = source.charAt(position);
              if (Characters.isDigit(c) || c == '_') {
                position++;
                if (c == '_' && c == previous) error = true;
                previous = c;
              } else {
                break;
              }
            }

            // Last char should not be a underscore
            if (previous == '_') {
              error = true;
            }

            // Exponentiation part
            if (position < source.length() && Characters.isExponent(c)) {
              position++;
              if (position < source.length()) {
                c = source.charAt(position);
                if (c == '+' || c == '-') {
                  position++;
                }
              }

              previous = '_';
              int digit = 0;
              while (position < source.length()) {
                c = source.charAt(position);
                if (Characters.isDigit(c) || c == '_') {
                  position++;
                  if (Characters.isDigit(c)) digit++;
                  if (c == '_' && c == previous) error = true;
                  previous = c;
                } else {
                  break;
                }
              }

              if (previous == '_' || digit == 0) {
                error = true;
              }
            }

            String str = source.substring(start, position);
            // Empty, consecutive underscore or last char is underscore
            if (str.length() == 0 || previous == '_' || error) {
              throw new ExpressionParseException(position, ErrorCode.INVALID_NUMBER, str);
            }

            // Literal decimal number
            return new Token(Id.LITERAL_NUMERIC_DECIMAL, start, position, str.replace("_", ""));
          }

        default:
          if (Characters.isSpace(c) || c == '\n' || c == '\r') {
            position++;
            continue;
          }

          // Probably a letter or digit. Start an identifier.
          // Other characters, e.g. *, ! are also included
          // in identifiers.
          int start = position++;
          while (position < source.length()) {
            c = source.charAt(position);
            if (Characters.isSpace(c) || "()/*%,^&><=~+-.!|$:[]\n\r".indexOf(c) >= 0) break;

            position++;
          }

          String identifier = source.substring(start, position);
          String name = identifier.toUpperCase();

          if (c == '(') {

            // Syntax NOT(TRUE)
            if ("NOT".equals(name)) {
              return new Token(Id.NOT, start, position, name);
            }

            return new Token(Id.FUNCTION, start, position, name);
          }

          // Reserved words: AS, AND, LIKE, NOT, TRUE, FALSE, OR
          if (isReservedWord(name)) {
            return new Token(Id.valueOf(name), start, position, name);
          }

          if (TypeName.of(name) != null) {
            return new Token(Id.LITERAL_DATATYPE, start, position, name);
          }

          if (TimeUnit.of(name) != null) {
            return new Token(Id.LITERAL_TIMEUNIT, start, position, name);
          }

          return new Token(Id.IDENTIFIER, start, position, identifier);
      }
    }
    return null;
  }
}
