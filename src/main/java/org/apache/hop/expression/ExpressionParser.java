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
import org.apache.hop.expression.Token.Id;
import org.apache.hop.expression.operator.AddOperator;
import org.apache.hop.expression.operator.ArrayElementAtOperator;
import org.apache.hop.expression.operator.AtTimeZoneOperator;
import org.apache.hop.expression.operator.BetweenAsymmetricOperator;
import org.apache.hop.expression.operator.BetweenSymmetricOperator;
import org.apache.hop.expression.operator.BoolAndOperator;
import org.apache.hop.expression.operator.BoolNotFunction;
import org.apache.hop.expression.operator.BoolOrOperator;
import org.apache.hop.expression.operator.BoolXorOperator;
import org.apache.hop.expression.operator.CaseSearchOperator;
import org.apache.hop.expression.operator.CaseSimpleOperator;
import org.apache.hop.expression.operator.CastOperator;
import org.apache.hop.expression.operator.ConcatFunction;
import org.apache.hop.expression.operator.DivOperator;
import org.apache.hop.expression.operator.EqualOperator;
import org.apache.hop.expression.operator.GreaterThanOperator;
import org.apache.hop.expression.operator.GreaterThanOrEqualOperator;
import org.apache.hop.expression.operator.ILikeOperator;
import org.apache.hop.expression.operator.InListOperator;
import org.apache.hop.expression.operator.IsDistinctFromOperator;
import org.apache.hop.expression.operator.IsFalseOperator;
import org.apache.hop.expression.operator.IsNotDistinctFromOperator;
import org.apache.hop.expression.operator.IsNotFalseOperator;
import org.apache.hop.expression.operator.IsNotNullOperator;
import org.apache.hop.expression.operator.IsNotTrueOperator;
import org.apache.hop.expression.operator.IsNullOperator;
import org.apache.hop.expression.operator.IsTrueOperator;
import org.apache.hop.expression.operator.LessThanOperator;
import org.apache.hop.expression.operator.LessThanOrEqualOperator;
import org.apache.hop.expression.operator.LikeOperator;
import org.apache.hop.expression.operator.ModFunction;
import org.apache.hop.expression.operator.MultiplyOperator;
import org.apache.hop.expression.operator.NegativeOperator;
import org.apache.hop.expression.operator.NotEqualOperator;
import org.apache.hop.expression.operator.NotInListOperator;
import org.apache.hop.expression.operator.NotSimilarToOperator;
import org.apache.hop.expression.operator.PositiveOperator;
import org.apache.hop.expression.operator.SimilarToOperator;
import org.apache.hop.expression.operator.SubtractOperator;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.BooleanType;
import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.InetType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.IntervalType;
import org.apache.hop.expression.type.JsonType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.InetConversion;
import org.apache.hop.expression.util.JsonConversion;
import org.apache.hop.expression.util.NumberFormat;
import org.jspecify.annotations.NullMarked;

/**
 * Expression parser.
 *
 * <p>EBNF Syntax: <code>
 * Literal := BooleanLiteral | StringLiteral | IntegerLiteral | NumberLiteral | BinaryLiteral | JsonLiteral
 * Constant := 'NULL' | 'TRUE' | 'FALSE'
 * TermExpression := Literal | Identifier | ParenthesisExpression  | FunctionExpression | CaseExpression
 * ParenthesisExpression  := ('(' LogicalOrExpression ')') | LogicalOrExpression
 * FunctionExpression := FunctionName '(' ExpressionList ')', or the extended syntax parsed by the function
 * ExpressionList := LogicalOrExpression ( ',' LogicalOrExpression )*
 * CaseExpression := SimpleCaseExpression | SearchedCaseExpression
 * SimpleCaseExpression := 'CASE' Expr ('WHEN' Expr 'THEN' Expr)+ ('ELSE' Expr)? 'END'
 * SearchedCaseExpression := 'CASE' ('WHEN' Expr 'THEN' Expr)+ ('ELSE' Expr)? 'END'
 * CastOperatorExpression := TermExpression '::' TypeExpression
 * ElementAtExpression := TermExpression '[' AdditiveExpression ']'
 * AtTimeZoneExpression := TermExpression AT TIME ZONE TimezoneExpression
 * PrimaryExpression := CastOperatorExpression | ElementAtExpression | AtTimeZoneExpression
 * UnaryExpression :=
 * PrimaryExpression
 * - PrimaryExpression
 * + PrimaryExpression
 * MultiplicativeExpression  :=
 * UnaryExpression
 * UnaryExpression * UnaryExpression
 * UnaryExpression / UnaryExpression
 * UnaryExpression % UnaryExpression
 * AdditiveExpression :=
 * MultiplicativeExpression
 * MultiplicativeExpression  + MultiplicativeExpression
 * MultiplicativeExpression  - MultiplicativeExpression
 * MultiplicativeExpression  || MultiplicativeExpression
 * RelationExpression :=
 * AdditiveExpression
 * AdditiveExpression [NOT] IN '(' ExpressionList ')'
 * AdditiveExpression [NOT] BETWEEN [ASYMMETRIC|SYMMETRIC] LogicalOrExpression AND LogicalOrExpression
 * AdditiveExpression [NOT] LIKE AdditiveExpression [ESCAPE StringExpression]
 * AdditiveExpression [NOT] ILIKE AdditiveExpression [ESCAPE StringExpression]
 * AdditiveExpression [NOT] SIMILAR TO AdditiveExpression
 * ComparisonExpression :=
 * RelationalExpression
 * RelationalExpression = RelationalExpression
 * RelationalExpression &lt; RelationalExpression
 * RelationalExpression &lt;= RelationalExpression
 * RelationalExpression &gt; RelationalExpression
 * RelationalExpression &gt;= RelationalExpression
 * RelationalExpression &lt;&gt; RelationalExpression
 * RelationalExpression != RelationalExpressio
 * ConditionalExpression :=
 * ComparisonExpression
 * ComparisonExpression IS [NOT] TRUE|FALSE|NULL
 * ComparisonExpression IS [NOT] DISTINCT FROM LogicalNotExpression
 * LogicalNotExpression := ConditionalExpression [NOT] LogicalNotExpression
 * LogicalAndExpression := LogicalNotExpression ( AND LogicalNotExpression )*
 * LogicalXorExpression := LogicalAndExpression ( OR LogicalAndExpression )*
 * LogicalOrExpression := LogicalXorExpression ( OR LogicalXorExpression )*
 * </code>
 */
@NullMarked
public class ExpressionParser implements IExpressionParser {

  private final ExpressionLexer lexer;

  public ExpressionParser(ExpressionLexer lexer) {
    super();
    this.lexer = lexer;
  }

  @Override
  public int getPosition() {
    return lexer.getPosition();
  }

  @Override
  public boolean hasNext() throws ExpressionException {
    return lexer.hasNext();
  }

  @Override
  public void expectNext(ErrorCode errorCode, Object... values) throws ExpressionException {
    lexer.expectNext(errorCode, values);
  }

  @Override
  public Token next() throws ExpressionException {
    return lexer.next();
  }

  @Override
  public boolean isNext(Id id) throws ExpressionException {
    return lexer.isNext(id);
  }

  @Override
  public boolean accept(Id id) throws ExpressionException {
    return lexer.accept(id);
  }

  @Override
  public void expect(Id id, ErrorCode errorCode, Object... values) throws ExpressionException {
    lexer.expect(id, errorCode, values);
  }

  @Override
  public boolean isNextKeyword(String word) throws ExpressionException {
    return lexer.isNextKeyword(word);
  }

  @Override
  public boolean acceptKeyword(String word) throws ExpressionException {
    return lexer.acceptKeyword(word);
  }

  @Override
  public void expectKeyword(String word, ErrorCode errorCode, Object... values)
      throws ExpressionException {
    lexer.expectKeyword(word, errorCode, values);
  }

  @Override
  public IExpression parseExpression() throws ExpressionException {
    return this.parseLogicalOr();
  }

  /** Parse the expression */
  public IExpression parse() throws ExpressionException {

    // Empty source return literal NULL
    if (!lexer.hasNext()) {
      return Literal.NULL;
    }

    IExpression expression = this.parseLogicalOr();

    // Unexpected end of expression
    if (lexer.hasNext()) {
      Token token = lexer.next();
      throw new ParseExpressionException(
          token.start(), ErrorCode.UNEXPECTED_CHARACTER, token.text());
    }

    return expression;
  }

  /**
   * Parse logical OR expression (Disjunction).
   *
   * <p><code>
   * LogicalXorExpression ( OR LogicalXorExpression )*
   * </code>
   */
  private IExpression parseLogicalOr() throws ExpressionException {
    IExpression expression = this.parseLogicalXor();
    while (lexer.acceptAndNotEnd(Id.OR)) {
      expression =
          new Call(lexer.getPosition(), BoolOrOperator.INSTANCE, expression, parseLogicalXor());
    }

    return expression;
  }

  /**
   * Parse logical XOR expression (Exclusive disjunction).
   *
   * <p><code>
   * LogicalAndExpression ( XOR LogicalAndExpression )*
   * </code>
   */
  private IExpression parseLogicalXor() throws ExpressionException {
    IExpression expression = this.parseLogicalAnd();
    while (lexer.acceptAndNotEnd(Id.XOR)) {
      expression =
          new Call(lexer.getPosition(), BoolXorOperator.INSTANCE, expression, parseLogicalAnd());
    }

    return expression;
  }

  /**
   * Parse logical AND expression (Conjunction).
   *
   * <p><code>
   * LogicalNotExpression ( AND LogicalNotExpression )
   * </code>
   */
  private IExpression parseLogicalAnd() throws ExpressionException {
    IExpression expression = this.parseLogicalNot();
    while (lexer.acceptAndNotEnd(Id.AND)) {
      expression =
          new Call(lexer.getPosition(), BoolAndOperator.INSTANCE, expression, parseLogicalNot());
    }

    return expression;
  }

  /**
   * Parse logical NOT expression.
   *
   * <p><code>[NOT] ConditionalExpression</code>
   */
  private IExpression parseLogicalNot() throws ExpressionException {
    if (lexer.acceptAndNotEnd(Id.NOT)) {
      return new Call(lexer.getPosition(), BoolNotFunction.INSTANCE, parseLogicalNot());
    }

    return this.parseConditional();
  }

  /**
   * Parse assertion IS expression.
   *
   * <p><code>
   * RelationalExpression IS [NOT] TRUE|FALSE|NULL
   * RelationalExpression IS [NOT] DISTINCT FROM
   * RelationalExpression
   * </code>
   */
  private IExpression parseConditional() throws ExpressionException {
    IExpression expression = this.parseComparison();
    if (lexer.accept(Id.IS)) {
      boolean not = false;
      int start = lexer.getPosition();

      if (lexer.accept(Id.NOT)) {
        not = true;
      }

      Token token = lexer.nextOrThrows(ErrorCode.SYNTAX_ERROR, Id.IS);
      return switch (token.id()) {
        case TRUE ->
            new Call(
                start, (not) ? IsNotTrueOperator.INSTANCE : IsTrueOperator.INSTANCE, expression);
        case FALSE ->
            new Call(
                start, (not) ? IsNotFalseOperator.INSTANCE : IsFalseOperator.INSTANCE, expression);
        case NULL ->
            new Call(
                start, (not) ? IsNotNullOperator.INSTANCE : IsNullOperator.INSTANCE, expression);
        case DISTINCT -> {
          if (lexer.acceptAndNotEnd(Token.Id.FROM)) {
            yield new Call(
                start,
                (not) ? IsNotDistinctFromOperator.INSTANCE : IsDistinctFromOperator.INSTANCE,
                expression,
                parseLogicalNot());
          }
          throw new ParseExpressionException(start, ErrorCode.SYNTAX_ERROR, Token.Id.DISTINCT);
        }
        default -> throw new ParseExpressionException(start, ErrorCode.SYNTAX_ERROR, Token.Id.IS);
      };
    }
    return expression;
  }

  /**
   * Parse IN, LIKE, BETWEEN, SIMILAR TO expression <code>
   * AdditiveExpression ( [NOT] | InClause | BetweenClause | LikeClause AdditiveExpression)</code>
   */
  private IExpression parseRelational() throws ExpressionException {
    IExpression expression = this.parseAdditive();

    // Special case NOT before operation: <exp> [NOT] LIKE|ILIKE|IN|BETWEEN|SIMILAR <primaryExp>
    boolean not = lexer.accept(Id.NOT);

    int start = lexer.getPosition();

    if (lexer.accept(Id.LIKE)) {
      IExpression pattern = this.parseAdditive();

      if (lexer.acceptAndNotEnd(Id.ESCAPE)) {
        IExpression escape = this.parseEscape();
        expression = new Call(start, LikeOperator.INSTANCE, expression, pattern, escape);
      } else {
        expression = new Call(start, LikeOperator.INSTANCE, expression, pattern);
      }

      if (not) {
        return new Call(start, BoolNotFunction.INSTANCE, expression);
      }
      return expression;
    } else if (lexer.accept(Id.ILIKE)) {
      IExpression pattern = this.parseAdditive();
      if (lexer.acceptAndNotEnd(Id.ESCAPE)) {
        IExpression escape = this.parseEscape();
        expression = new Call(start, ILikeOperator.INSTANCE, expression, pattern, escape);
      } else {
        expression = new Call(start, ILikeOperator.INSTANCE, expression, pattern);
      }

      if (not) {
        return new Call(start, BoolNotFunction.INSTANCE, expression);
      }
      return expression;
    } else if (lexer.accept(Id.IN)) {

      lexer.expect(Id.LPARENTHESIS, ErrorCode.MISSING_LEFT_PARENTHESIS);

      List<IExpression> list = this.parseExpressionList();

      lexer.expect(Id.RPARENTHESIS, ErrorCode.MISSING_RIGHT_PARENTHESIS);

      if (list.isEmpty()) {
        throw new ParseExpressionException(
            lexer.getPosition(), ErrorCode.EMPTY_VALUE_LIST_FOR_IN_PREDICATE);
      }

      return new Call(
          start,
          not ? NotInListOperator.INSTANCE : InListOperator.INSTANCE,
          expression,
          new Array(list));
    } else if (lexer.acceptAndNotEnd(Id.BETWEEN)) {
      Operator operator;
      if (lexer.accept(Id.ASYMMETRIC)) {
        operator = BetweenAsymmetricOperator.INSTANCE;
      } else if (lexer.accept(Id.SYMMETRIC)) {
        operator = BetweenSymmetricOperator.INSTANCE;
      } else {
        // Default is asymmetric
        operator = BetweenAsymmetricOperator.INSTANCE;
      }
      IExpression lower = this.parseAdditive();

      lexer.expect(Id.AND, ErrorCode.SYNTAX_ERROR, Id.BETWEEN);
      lexer.expectNext(ErrorCode.SYNTAX_ERROR, Id.BETWEEN);
      IExpression upper = this.parseAdditive();

      expression = new Call(start, operator, expression, lower, upper);

      if (not) {
        return new Call(start, BoolNotFunction.INSTANCE, expression);
      }
      return expression;
    } else if (lexer.acceptAndNotEnd(Id.SIMILAR)) {
      lexer.expect(Id.TO, ErrorCode.SYNTAX_ERROR, Id.SIMILAR);
      lexer.expectNext(ErrorCode.SYNTAX_ERROR, Id.SIMILAR);

      return new Call(
          start,
          not ? NotSimilarToOperator.INSTANCE : SimilarToOperator.INSTANCE,
          expression,
          parseAdditive());
    }

    if (not) {
      throw new ParseExpressionException(start, ErrorCode.SYNTAX_ERROR, Id.NOT);
    }

    return expression;
  }

  /** <code>UnaryExpression ( (* | / | %) UnaryExpression())</code>* */
  private IExpression parseMultiplicative() throws ExpressionException {
    IExpression expression = this.parseUnary();

    while (lexer.hasNext()) {

      int start = lexer.getPosition();

      if (lexer.acceptAndNotEnd(Id.STAR)) {
        expression = new Call(start, MultiplyOperator.INSTANCE, expression, this.parseUnary());
      } else if (lexer.acceptAndNotEnd(Id.SLASH)) {
        expression = new Call(start, DivOperator.INSTANCE, expression, this.parseUnary());
      } else if (lexer.acceptAndNotEnd(Id.PERCENT)) {
        expression = new Call(start, ModFunction.INSTANCE, expression, this.parseUnary());
      } else break;
    }

    return expression;
  }

  /** <code>('+' | '-') PrimaryExpression</code> */
  private IExpression parseUnary() throws ExpressionException {
    int start = lexer.getPosition();
    if (lexer.acceptAndNotEnd(Id.MINUS)) {
      return new Call(start, NegativeOperator.INSTANCE, this.parsePrimary());
    }

    if (lexer.acceptAndNotEnd(Id.PLUS)) {
      return new Call(start, PositiveOperator.INSTANCE, this.parsePrimary());
    }

    return this.parsePrimary();
  }

  /** Literal String */
  @Override
  public IExpression parseLiteralString(Token token) {
    return Literal.of(token.text());
  }

  /** Parses the escape character of LIKE and ILIKE, it must be a string literal. */
  private IExpression parseEscape() throws ExpressionException {
    Token token = lexer.next();
    if (!token.is(Id.LITERAL_STRING)) {
      throw new ParseExpressionException(token.start(), ErrorCode.INVALID_ARGUMENT, token.text());
    }
    return parseLiteralString(token);
  }

  /** Literal Json */
  private IExpression parseLiteralJson(Token token) {
    return Literal.of(JsonConversion.convert(token.text()));
  }

  /** Literal Inet */
  private IExpression parseLiteralInet(Token token) {
    return Literal.of(InetConversion.convert(token.text()));
  }

  /**
   * Parse primary <code>
   * TermExpression::TypeExpression | TermExpression AT TIMEZONE TimezoneExpression | TermExpression[index]
   * </code>
   */
  @Override
  public IExpression parsePrimary() throws ExpressionException {
    IExpression expression = this.parseTerm();

    // Cast operator '::'
    if (lexer.acceptAndNotEnd(Id.CAST)) {
      IExpression type = parseDataType(lexer.next());
      return new Call(lexer.getPosition(), CastOperator.INSTANCE, expression, type);
    }

    // Array element at ARRAY[index]
    if (lexer.accept(Id.LBRACKET)) {
      IExpression term = this.parseAdditive();
      Call call = new Call(lexer.getPosition(), ArrayElementAtOperator.INSTANCE, expression, term);

      lexer.expect(Id.RBRACKET, ErrorCode.MISSING_RIGHT_BRACKET);

      return call;
    }

    //  <term> AT TIMEZONE <timezone>
    if (lexer.acceptAndNotEnd(Id.AT)) {
      if (lexer.acceptAndNotEnd(Id.TIME) && lexer.acceptAndNotEnd(Id.ZONE)) {
        return new Call(
            lexer.getPosition(), AtTimeZoneOperator.INSTANCE, expression, this.parseTerm());
      }
      throw new ParseExpressionException(lexer.getPosition(), ErrorCode.SYNTAX_ERROR, Id.AT);
    }
    return expression;
  }

  /** Parse term <code>Literal | Array | Identifier | Function | '(' Expression ')'</code> */
  private IExpression parseTerm() throws ExpressionException {
    Token token = lexer.next();
    return switch (token.id()) {
      case TRUE -> Literal.TRUE;
      case FALSE -> Literal.FALSE;
      case NULL -> Literal.NULL;
      case IDENTIFIER -> new Identifier(token.start(), token.text());
      case LITERAL_STRING -> parseLiteralString(token);
      case LITERAL_NUMERIC_DECIMAL -> parseLiteralNumericDecimal(token);
      case LITERAL_NUMERIC_HEXA -> parseLiteralNumericHexa(token);
      case LITERAL_NUMERIC_BINARY -> parseLiteralNumericBinary(token);
      case LITERAL_NUMERIC_OCTAL -> parseLiteralNumericOctal(token);
      case LITERAL_TIMEUNIT -> parseLiteralTimeUnit(token);
      case DATE -> parseLiteralDate(lexer.nextOrThrows(ErrorCode.SYNTAX_ERROR, Id.DATE));
      case TIMESTAMP ->
          parseLiteralTimestamp(lexer.nextOrThrows(ErrorCode.SYNTAX_ERROR, Id.TIMESTAMP));
      case BINARY -> parseLiteralBinary(lexer.nextOrThrows(ErrorCode.SYNTAX_ERROR, Id.BINARY));
      case INET -> parseLiteralInet(lexer.nextOrThrows(ErrorCode.SYNTAX_ERROR, Id.INET));
      case JSON -> parseLiteralJson(lexer.nextOrThrows(ErrorCode.SYNTAX_ERROR, Id.JSON));
      case INTERVAL -> parseLiteralInterval(lexer.nextOrThrows(ErrorCode.INVALID_INTERVAL, ""));
      case CASE -> parseCase();
      case FUNCTION -> parseFunction(token);
      case LBRACKET -> parseArray(token);
      case LPARENTHESIS -> {
        if (lexer.hasNext()) {
          IExpression expression = this.parseLogicalOr();
          if (lexer.accept(Id.RPARENTHESIS)) {
            yield expression;
          }
        }
        throw new ParseExpressionException(token.start(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
      }
      default -> throw new ParseExpressionException(token.start(), ErrorCode.SYNTAX_ERROR, token);
    };
  }

  /** <code>RelationalExpression ( Operator RelationalExpression )</code> */
  private IExpression parseComparison() throws ExpressionException {
    IExpression expression = this.parseRelational();
    int start = lexer.getPosition();
    if (lexer.acceptAndNotEnd(Id.EQUAL)) {
      return new Call(start, EqualOperator.INSTANCE, expression, this.parseRelational());
    }
    if (lexer.acceptAndNotEnd(Id.NOT_EQUAL)) {
      return new Call(start, NotEqualOperator.INSTANCE, expression, this.parseRelational());
    }
    if (lexer.acceptAndNotEnd(Id.GT)) {
      return new Call(start, GreaterThanOperator.INSTANCE, expression, this.parseRelational());
    }
    if (lexer.acceptAndNotEnd(Id.GTE)) {
      return new Call(
          start, GreaterThanOrEqualOperator.INSTANCE, expression, this.parseRelational());
    }
    if (lexer.acceptAndNotEnd(Id.LT)) {
      return new Call(start, LessThanOperator.INSTANCE, expression, this.parseRelational());
    }
    if (lexer.acceptAndNotEnd(Id.LTE)) {
      return new Call(start, LessThanOrEqualOperator.INSTANCE, expression, this.parseRelational());
    }

    return expression;
  }

  /** <code>MultiplicativeExpression ( (+ | - | "||") MultiplicativeExpression )*</code> */
  private IExpression parseAdditive() throws ExpressionException {
    IExpression expression = this.parseMultiplicative();
    while (lexer.hasNext()) {
      int start = lexer.getPosition();

      if (lexer.acceptAndNotEnd(Id.PLUS)) {
        expression = new Call(start, AddOperator.INSTANCE, expression, this.parseMultiplicative());
      } else if (lexer.acceptAndNotEnd(Id.MINUS)) {
        expression =
            new Call(start, SubtractOperator.INSTANCE, expression, this.parseMultiplicative());
      } else if (lexer.acceptAndNotEnd(Id.CONCAT)) {
        expression =
            new Call(start, ConcatFunction.INSTANCE, expression, this.parseMultiplicative());
      } else break;
    }

    return expression;
  }

  /** Parse array <code>[exp1, exp2...]</code> */
  private IExpression parseArray(final Token token) throws ExpressionException {

    if (!lexer.hasNext()) {
      throw new ParseExpressionException(token.start(), ErrorCode.MISSING_RIGHT_BRACKET);
    }

    List<IExpression> operands = new ArrayList<>();

    // Empty array
    if (lexer.accept(Id.RBRACKET)) {
      return new Array(operands);
    }

    do {
      lexer.expectNext(ErrorCode.SYNTAX_ERROR, Id.LBRACKET);
      operands.add(this.parseLogicalOr());
    } while (lexer.accept(Id.COMMA));

    lexer.expect(Id.RBRACKET, ErrorCode.MISSING_RIGHT_BRACKET);

    return new Array(operands);
  }

  private IExpression parseLiteralNumericDecimal(Token token) throws ExpressionException {
    BigDecimal number = NumberFormat.of("TM").parse(token.text());
    try {
      return Literal.of(number.longValueExact());
    } catch (ArithmeticException e) {
      return Literal.of(number);
    }
  }

  private IExpression parseLiteralBinary(Token token) throws ExpressionException {
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
      throw new ParseExpressionException(
          token.start(), ErrorCode.UNPARSABLE_BINARY_WITH_FORMAT, token.text(), "HEX");
    }
  }

  private IExpression parseLiteralNumericHexa(Token token) {
    String str = token.text().substring(2);
    BigInteger value = new BigInteger(str, 16);
    try {
      return Literal.of(value.longValueExact());
    } catch (ArithmeticException e) {
      return Literal.of(new BigDecimal(value));
    }
  }

  private IExpression parseLiteralNumericOctal(Token token) {
    String str = token.text().substring(2);
    BigInteger value = new BigInteger(str, 8);
    try {
      return Literal.of(value.longValueExact());
    } catch (ArithmeticException e) {
      return Literal.of(new BigDecimal(value));
    }
  }

  private IExpression parseLiteralNumericBinary(Token token) {
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
  private IExpression parseLiteralDate(Token token) throws ExpressionException {
    try {
      // Literal date use exact mode ISO 8601
      DateTimeFormat format = DateTimeFormat.of("FXYYYY-MM-DD");
      ZonedDateTime datetime = format.parse(token.text());

      return Literal.of(datetime);
    } catch (Exception e) {
      throw new ParseExpressionException(token.start(), ErrorCode.INVALID_DATE, token.text());
    }
  }

  /**
   * Parses a timestamp literal with ISO Formats.
   *
   * <p>The parsing is strict and requires months to be between 1 and 12, days to be less than 31,
   * etc.
   */
  private IExpression parseLiteralTimestamp(Token token) throws ExpressionException {
    try {
      DateTimeFormat format = DateTimeFormat.of("AUTO");
      ZonedDateTime datetime = format.parse(token.text());

      return Literal.of(datetime);
    } catch (Exception e) {
      throw new ParseExpressionException(token.start(), ErrorCode.INVALID_TIMESTAMP, token.text());
    }
  }

  /**
   * Parses a list of expressions separated by commas (function arguments or IN predicate value
   * list).
   *
   * <p><code>expression [,expression...]</code>
   */
  @Override
  public List<IExpression> parseExpressionList() {
    List<IExpression> list = new ArrayList<>();

    // No argument function
    if (lexer.isNext(Id.RPARENTHESIS)) {
      return list;
    }

    // <expression [, expression>]
    boolean first = true;
    do {
      if (lexer.accept(Id.RPARENTHESIS)) {
        if (!first) {
          throw new ParseExpressionException(lexer.getPosition(), ErrorCode.MISSING_ELEMENT);
        }
        return list;
      }
      if (lexer.hasNext()) {
        list.add(this.parseLogicalOr());
      }
      first = false;
    } while (lexer.accept(Id.COMMA));

    return list;
  }

  /** Case When Then Else End */
  private IExpression parseCase() throws ExpressionException {
    IExpression valueExpression = Literal.NULL;
    IExpression elseExpression = Literal.NULL;
    List<IExpression> whenList = new ArrayList<>();
    List<IExpression> thenList = new ArrayList<>();

    int start = lexer.getPosition();
    boolean simple = false;

    // Simple case
    if (!lexer.isNext(Id.WHEN)) {
      valueExpression = this.parseLogicalOr();
      simple = true;
    }

    while (lexer.accept(Id.WHEN)) {
      IExpression expression = this.parseLogicalOr();

      // Simple case with multi values
      if (simple && lexer.accept(Id.COMMA)) {
        List<IExpression> values = new ArrayList<>();
        values.add(expression);

        do {
          values.add(this.parseLogicalOr());
        } while (lexer.accept(Id.COMMA));

        expression = new Array(values);
      }
      whenList.add(expression);

      lexer.expect(Id.THEN, ErrorCode.SYNTAX_ERROR_CASE_STATEMENT);

      thenList.add(this.parseLogicalOr());
    }

    if (lexer.accept(Id.ELSE)) {
      elseExpression = this.parseLogicalOr();
    }

    lexer.expect(Id.END, ErrorCode.SYNTAX_ERROR_CASE_STATEMENT);

    return new Call(
        start,
        simple ? CaseSimpleOperator.INSTANCE : CaseSearchOperator.INSTANCE,
        valueExpression,
        new Array(whenList),
        new Array(thenList),
        elseExpression);
  }

  /** Parse function */
  private IExpression parseFunction(Token token) throws ExpressionException {
    Function function = FunctionRegistry.getFunction(token.text());

    // Function is never null
    if (function == null) {
      throw new ParseExpressionException(
          token.start(), ErrorCode.FUNCTION_DOES_NOT_EXIST, token.text());
    }

    lexer.expect(Id.LPARENTHESIS, ErrorCode.MISSING_LEFT_PARENTHESIS);

    // The function parses its arguments, with its extended syntax if any
    return function.parse(this, token);
  }

  @Override
  public IExpression parseLiteralTimeUnit(Token token) throws ExpressionException {
    TimeUnit unit = TimeUnit.of(token.text());
    if (unit == null) {
      throw new ParseExpressionException(token.start(), ErrorCode.INVALID_TIME_UNIT, token.text());
    }
    return Literal.of(unit);
  }

  private IExpression parseLiteralInterval(Token token) throws ExpressionException {

    boolean negative = false;
    if (token.is(Id.MINUS)) {
      negative = true;
      token = lexer.nextOrThrows(ErrorCode.INVALID_INTERVAL, "");
    }

    // Verbose format at the end of the expression, like INTERVAL '1 month -1 day'
    if (token.is(Id.LITERAL_STRING) && !lexer.hasNext()) {
      return Literal.of(Interval.of(token.text()));
    }

    Token start = lexer.nextOrThrows(ErrorCode.INVALID_INTERVAL, "");
    TimeUnit startUnit = TimeUnit.of(start.text());

    // Verbose format
    if (startUnit == null) {
      lexer.previous();
      Interval interval = Interval.of(token.text());
      return Literal.of(interval);
    }

    // Short format with qualifier
    TimeUnit endUnit = null;
    String text = token.text();
    if (token.is(Id.LITERAL_STRING)) {
      Token end;
      if (lexer.accept(Id.TO)) {

        end = lexer.nextOrThrows(ErrorCode.INVALID_INTERVAL, text);
        endUnit = TimeUnit.of(end.text());
      }

      if (!text.isEmpty() && text.charAt(0) == '-') {
        negative = true;
        text = text.substring(1);
      }
    }

    IntervalQualifier qualifier = IntervalQualifier.of(startUnit, endUnit);
    if (qualifier == null)
      throw new ParseExpressionException(token.start(), ErrorCode.INVALID_INTERVAL, text);

    Interval interval = qualifier.parse(text);
    if (interval == null)
      throw new ParseExpressionException(token.start(), ErrorCode.INVALID_INTERVAL, text);

    if (negative) interval = interval.negate();

    return Literal.of(interval);
  }

  @Override
  public IExpression parseDataType(Token token) throws ExpressionException {

    TypeName name = TypeName.of(token.text());
    if (name == null) {
      throw new ParseExpressionException(token.start(), ErrorCode.INVALID_DATA_TYPE, token.text());
    }

    int precision = Type.PRECISION_NOT_SPECIFIED;
    int scale = Type.SCALE_NOT_SPECIFIED;
    if (lexer.accept(Id.LPARENTHESIS)) {

      // Precision
      try {
        precision = Integer.parseInt(lexer.next().text());
      } catch (NumberFormatException e) {
        throw new ParseExpressionException(
            token.start(), ErrorCode.EXPECTED_INTEGER_CONSTANT_AS_PRECISION);
      }
      if (!name.supportsPrecision()) {
        throw new ParseExpressionException(
            token.start(), ErrorCode.PRECISION_NOT_SUPPORTED, token.text());
      }

      // Scale
      if (lexer.acceptAndNotEnd(Id.COMMA)) {
        try {
          scale = Integer.parseInt(lexer.next().text());
        } catch (NumberFormatException e) {
          throw new ParseExpressionException(
              token.start(), ErrorCode.EXPECTED_INTEGER_CONSTANT_AS_SCALE);
        }
        if (!name.supportsScale()) {
          throw new ParseExpressionException(
              token.start(), ErrorCode.SCALE_NOT_SUPPORTED, token.text());
        }
      }

      lexer.expect(Id.RPARENTHESIS, ErrorCode.MISSING_RIGHT_PARENTHESIS);
    }

    Type type =
        switch (name) {
          case BOOLEAN -> BooleanType.BOOLEAN;
          case INTEGER -> IntegerType.of(precision);
          case NUMBER -> NumberType.of(precision, scale);
          case STRING -> StringType.of(precision);
          case BINARY -> BinaryType.of(precision);
          case DATE -> DateType.DATE;
          case INET -> InetType.INET;
          case JSON -> JsonType.JSON;
          case INTERVAL -> IntervalType.INTERVAL;
          default ->
              throw new ParseExpressionException(
                  token.start(), ErrorCode.INVALID_DATA_TYPE, token.text());
        };

    return Literal.of(type);
  }
}
