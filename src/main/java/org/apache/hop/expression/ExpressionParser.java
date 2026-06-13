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
import lombok.Getter;
import org.apache.hop.expression.Token.Id;
import org.apache.hop.expression.operator.AddOperator;
import org.apache.hop.expression.operator.ArrayElementAtOperator;
import org.apache.hop.expression.operator.AtTimeZoneOperator;
import org.apache.hop.expression.operator.BetweenAsymmetricOperator;
import org.apache.hop.expression.operator.BetweenSymmetricOperator;
import org.apache.hop.expression.operator.BoolAndOperator;
import org.apache.hop.expression.operator.BoolNotOperator;
import org.apache.hop.expression.operator.BoolOrOperator;
import org.apache.hop.expression.operator.BoolXorOperator;
import org.apache.hop.expression.operator.CaseSearchOperator;
import org.apache.hop.expression.operator.CaseSimpleOperator;
import org.apache.hop.expression.operator.CastOperator;
import org.apache.hop.expression.operator.ConcatFunction;
import org.apache.hop.expression.operator.CountFunction;
import org.apache.hop.expression.operator.DivOperator;
import org.apache.hop.expression.operator.EqualOperator;
import org.apache.hop.expression.operator.FirstValueFunction;
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
import org.apache.hop.expression.operator.LastValueFunction;
import org.apache.hop.expression.operator.LessThanOperator;
import org.apache.hop.expression.operator.LessThanOrEqualOperator;
import org.apache.hop.expression.operator.LikeOperator;
import org.apache.hop.expression.operator.ListAggFunction;
import org.apache.hop.expression.operator.ModFunction;
import org.apache.hop.expression.operator.MultiplyOperator;
import org.apache.hop.expression.operator.NegativeOperator;
import org.apache.hop.expression.operator.NotEqualOperator;
import org.apache.hop.expression.operator.NotInListOperator;
import org.apache.hop.expression.operator.NotSimilarToOperator;
import org.apache.hop.expression.operator.NthValueFunction;
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

/**
 * Expression parser.
 *
 * <p>EBNF Syntax: <code>
 *  Literal :=
 *     BooleanLiteral
 * 	StringLiteral
 *     IntegerLiteral
 *     NumberLiteral
 *     BinaryLiteral
 *     JsonLiteral
 *     'NULL'
 *     'TRUE'
 *     'FALSE'
 * TermExpression :=
 *     Literal
 *     Identifier
 *     ParenthesizedExpression
 *     FunctionExpression
 *     CaseExpression
 * PrimaryExpression :=
 *     CastOperatorExpression
 *     ElementAtExpression
 *     AtTimeZoneExpression
 * ParenthesizedExpression := ('(' LogicalOrExpression ')') | LogicalOrExpression
 * FunctionExpression := FunctionName '(' ListExpression ')'
 * ListExpression := LogicalOrExpression ( ',' LogicalOrExpression )*
 * CaseExpression := SimpleCaseExpression | SearchedCaseExpression
 * SimpleCaseExpression := 'CASE' Expr ('WHEN' Expr 'THEN' Expr)+ ('ELSE' Expr)? 'END'
 * SearchedCaseExpression := 'CASE' ('WHEN' Expr 'THEN' Expr)+ ('ELSE' Expr)? 'END'
 * CastOperatorExpression := TermExpression '::' TypeExpression
 * ElementAtExpression := TermExpression '[' AdditiveExpression ']'
 * AtTimeZoneExpression := TermExpression AT TIME ZONE TimezoneExpression
 * UnaryExpression :=
 *     PrimaryExpression
 *     - PrimaryExpression
 *     + PrimaryExpression
 * FactorExpression :=
 *     UnaryExpression
 *     UnaryExpression * UnaryExpression
 *     UnaryExpression / UnaryExpression
 *     UnaryExpression % UnaryExpression
 * AdditiveExpression :=
 *     FactorExpression
 *     FactorExpression + FactorExpression
 *     FactorExpression - FactorExpression
 *     FactorExpression || FactorExpression
 * RelationExpression :=
 *     AdditiveExpression
 *     AdditiveExpression [NOT] IN '(' ListExpression ')'
 *     AdditiveExpression [NOT] BETWEEN [ASYMMETRIC|SYMMETRIC] LogicalOrExpression AND LogicalOrExpression
 *     AdditiveExpression [NOT] LIKE AdditiveExpression [ESCAPE StringExpression]
 *     AdditiveExpression [NOT] ILIKE AdditiveExpression [ESCAPE StringExpression]
 *     AdditiveExpression [NOT] SIMILAR TO AdditiveExpression
 * ComparisonExpression :=
 *     RelationalExpression
 *     RelationalExpression = RelationalExpression
 *     RelationalExpression &lt; RelationalExpression
 *     RelationalExpression &lt;= RelationalExpression
 *     RelationalExpression &gt; RelationalExpression
 *     RelationalExpression &gt;= RelationalExpression
 *     RelationalExpression &lt;&gt; RelationalExpression
 *     RelationalExpression != RelationalExpression
 * ConditionalExpression :=
 *     ComparisonExpression
 *     ComparisonExpression IS [NOT] TRUE|FALSE|NULL
 *     ComparisonExpression IS [NOT] DISTINCT FROM LogicalNotExpression
 * LogicalNotExpression :=
 *     ConditionalExpression
 *     [NOT] LogicalNotExpression
 * LogicalAndExpression := LogicalNotExpression ( AND LogicalNotExpression )*
 * LogicalXorExpression := LogicalAndExpression ( OR LogicalAndExpression )*
 * LogicalOrExpression := LogicalXorExpression ( OR LogicalXorExpression )*
 * </code>
 */
public class ExpressionParser {

  @Getter private final String source;
  private List<Token> tokens;
  // Index in tokens
  private int index = 0;

  public ExpressionParser(String source) {
    super();
    this.source = source;
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

  protected void checkEndOfExpression(Id id) {
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

  protected boolean is(Id id) {
    if (hasNext()) {
      return tokens.get(index).is(id);
    }
    return false;
  }

  protected boolean isThenNext(Id id) {
    if (hasNext() && tokens.get(index).is(id)) {
      index++;
      return true;
    }
    return false;
  }

  protected boolean isThenNext(Id... ids) {
    for (Id id : ids) {
      if (hasNext() && tokens.get(index).is(id)) {
        index++;
        continue;
      }
      return false;
    }
    return true;
  }

  protected boolean isNotThenNext(Id id) {
    return !this.isThenNext(id);
  }

  /** Parse the expression */
  public IExpression parse() throws ExpressionException {

    if (source == null) throw new ExpressionParseException(0, ErrorCode.NULL_SOURCE_ERROR);

    // Tokenize the source string
    ExpressionLexer lexer = new ExpressionLexer(source);
    this.tokens = lexer.getTokens();

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
   * Parse logical OR expression (Disjunction).
   *
   * <p><code>
   * LogicalXorExpression ( OR LogicalXorExpression )*
   * </code>
   */
  private IExpression parseLogicalOr() throws ExpressionException {
    IExpression expression = this.parseLogicalXor();
    while (isThenNext(Id.OR)) {
      checkEndOfExpression(Id.OR);
      expression = new Call(getPosition(), BoolOrOperator.INSTANCE, expression, parseLogicalXor());
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
    while (isThenNext(Id.XOR)) {
      checkEndOfExpression(Id.XOR);
      expression = new Call(getPosition(), BoolXorOperator.INSTANCE, expression, parseLogicalAnd());
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
    while (isThenNext(Id.AND)) {
      checkEndOfExpression(Id.AND);
      expression = new Call(getPosition(), BoolAndOperator.INSTANCE, expression, parseLogicalNot());
    }

    return expression;
  }

  /**
   * Parse logical NOT expression.
   *
   * <p><code>[NOT] ConditionalExpression</code>
   */
  private IExpression parseLogicalNot() throws ExpressionException {
    if (isThenNext(Id.NOT)) {
      checkEndOfExpression(Id.NOT);
      return new Call(getPosition(), BoolNotOperator.INSTANCE, parseLogicalNot());
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
    if (isThenNext(Id.IS)) {
      boolean not = false;
      int start = this.getPosition();

      if (isThenNext(Id.NOT)) {
        not = true;
      }

      checkEndOfExpression(Id.IS);

      return switch (next().id()) {
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
          if (isThenNext(Token.Id.FROM)) {
            checkEndOfExpression(Token.Id.DISTINCT);
            yield new Call(
                start,
                (not) ? IsNotDistinctFromOperator.INSTANCE : IsDistinctFromOperator.INSTANCE,
                expression,
                parseLogicalNot());
          }
          throw new ExpressionParseException(start, ErrorCode.SYNTAX_ERROR, Token.Id.DISTINCT);
        }
        default -> throw new ExpressionParseException(start, ErrorCode.SYNTAX_ERROR, Token.Id.IS);
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
    boolean not = isThenNext(Id.NOT);

    int start = this.getPosition();

    if (isThenNext(Id.LIKE)) {
      IExpression pattern = this.parseAdditive();

      if (isThenNext(Id.ESCAPE)) {
        checkEndOfExpression(Id.LIKE);
        IExpression escape = this.parseLiteralString(next());
        expression = new Call(start, LikeOperator.INSTANCE, expression, pattern, escape);
      } else {
        expression = new Call(start, LikeOperator.INSTANCE, expression, pattern);
      }

      if (not) {
        return new Call(start, BoolNotOperator.INSTANCE, expression);
      }
      return expression;
    } else if (isThenNext(Id.ILIKE)) {
      IExpression pattern = this.parseAdditive();
      if (isThenNext(Id.ESCAPE)) {
        checkEndOfExpression(Id.ILIKE);
        IExpression escape = this.parseLiteralString(next());
        expression = new Call(start, ILikeOperator.INSTANCE, expression, pattern, escape);
      } else {
        expression = new Call(start, ILikeOperator.INSTANCE, expression, pattern);
      }

      if (not) {
        return new Call(start, BoolNotOperator.INSTANCE, expression);
      }
      return expression;
    } else if (isThenNext(Id.IN)) {
      if (isNotThenNext(Id.LPARENTHESIS)) {
        throw new ExpressionParseException(start, ErrorCode.MISSING_LEFT_PARENTHESIS);
      }

      List<IExpression> list = this.parseListExpression();

      if (isNotThenNext(Id.RPARENTHESIS)) {
        throw new ExpressionParseException(start, ErrorCode.MISSING_RIGHT_PARENTHESIS);
      }

      if (list.isEmpty()) {
        throw new ExpressionParseException(
            getPosition(), ErrorCode.EMPTY_VALUE_LIST_FOR_IN_PREDICATE);
      }

      return new Call(
          start,
          not ? NotInListOperator.INSTANCE : InListOperator.INSTANCE,
          expression,
          new Array(list));
    } else if (isThenNext(Id.BETWEEN)) {
      Operator operator;
      if (isThenNext(Id.ASYMMETRIC)) {
        operator = BetweenAsymmetricOperator.INSTANCE;
      } else if (isThenNext(Id.SYMMETRIC)) {
        operator = BetweenSymmetricOperator.INSTANCE;
      } else {
        // Default is asymmetric
        operator = BetweenAsymmetricOperator.INSTANCE;
      }
      checkEndOfExpression(Id.BETWEEN);
      IExpression lower = this.parseAdditive();
      if (isNotThenNext(Id.AND)) {
        throw new ExpressionParseException(getPosition(), ErrorCode.SYNTAX_ERROR, Id.BETWEEN);
      }
      checkEndOfExpression(Id.BETWEEN);
      IExpression upper = this.parseAdditive();

      expression = new Call(start, operator, expression, lower, upper);

      if (not) {
        return new Call(start, BoolNotOperator.INSTANCE, expression);
      }
      return expression;
    } else if (isThenNext(Id.SIMILAR)) {
      if (isNotThenNext(Id.TO)) {
        throw new ExpressionParseException(start, ErrorCode.SYNTAX_ERROR, Id.SIMILAR);
      }
      checkEndOfExpression(Id.SIMILAR);
      return new Call(
          start,
          not ? NotSimilarToOperator.INSTANCE : SimilarToOperator.INSTANCE,
          expression,
          parseAdditive());
    }

    if (not) {
      throw new ExpressionParseException(start, ErrorCode.SYNTAX_ERROR, Id.NOT);
    }

    return expression;
  }

  /** <code>UnaryExpression ( (* | / | %) UnaryExpression())</code>* */
  private IExpression parseFactor() throws ExpressionException {
    IExpression expression = this.parseUnary();

    while (hasNext()) {

      int start = this.getPosition();

      if (isThenNext(Id.STAR)) {
        checkEndOfExpression(Id.STAR);
        expression = new Call(start, MultiplyOperator.INSTANCE, expression, this.parseUnary());
      } else if (isThenNext(Id.SLASH)) {
        checkEndOfExpression(Id.SLASH);
        expression = new Call(start, DivOperator.INSTANCE, expression, this.parseUnary());
      } else if (isThenNext(Id.PERCENT)) {
        checkEndOfExpression(Id.PERCENT);
        expression = new Call(start, ModFunction.INSTANCE, expression, this.parseUnary());
      } else break;
    }

    return expression;
  }

  /** <code>('+' | '-') PrimaryExpression</code> */
  private IExpression parseUnary() throws ExpressionException {
    int start = this.getPosition();
    if (isThenNext(Id.MINUS)) {
      return new Call(start, NegativeOperator.INSTANCE, this.parsePrimary());
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
  private IExpression parsePrimary() throws ExpressionException {
    IExpression expression = this.parseTerm();

    // Cast operator ::
    if (isThenNext(Id.CAST)) {
      checkEndOfExpression(Id.CAST);
      IExpression type = parseLiteralType(next());
      return new Call(getPosition(), CastOperator.INSTANCE, expression, type);
    }

    // Array element at ARRAY[index]
    if (isThenNext(Id.LBRACKET)) {
      IExpression term = this.parseAdditive();
      Call call = new Call(getPosition(), ArrayElementAtOperator.INSTANCE, expression, term);
      if (isNotThenNext(Id.RBRACKET)) {
        throw new ExpressionParseException(getPosition(), ErrorCode.MISSING_RIGHT_BRACKET);
      }
      return call;
    }

    //  <term> AT TIMEZONE <timezone>
    if (isThenNext(Id.AT)) {
      checkEndOfExpression(Id.AT);
      if (isThenNext(Id.TIME)) {
        checkEndOfExpression(Id.TIME);
        if (isThenNext(Id.ZONE)) {
          checkEndOfExpression(Id.ZONE);
          return new Call(getPosition(), AtTimeZoneOperator.INSTANCE, expression, this.parseTerm());
        }
      }
      throw new ExpressionParseException(getPosition(), ErrorCode.SYNTAX_ERROR, Id.AT);
    }
    return expression;
  }

  /** Parse term <code>Literal | Array | Identifier | Function | '(' Expression ')'</code> */
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
        return parseLiteralType(token);
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

    throw new ExpressionParseException(token.start(), ErrorCode.SYNTAX_ERROR, token);
  }

  /** <code>RelationalExpression ( Operator RelationalExpression )</code> */
  private IExpression parseComparison() throws ExpressionException {
    IExpression expression = this.parseRelational();
    int start = this.getPosition();
    if (isThenNext(Id.EQUAL)) {
      checkEndOfExpression(Id.EQUAL);
      return new Call(start, EqualOperator.INSTANCE, expression, this.parseRelational());
    }
    if (isThenNext(Id.NOT_EQUAL)) {
      checkEndOfExpression(Id.NOT_EQUAL);
      return new Call(start, NotEqualOperator.INSTANCE, expression, this.parseRelational());
    }
    if (isThenNext(Id.GT)) {
      checkEndOfExpression(Id.GT);
      return new Call(start, GreaterThanOperator.INSTANCE, expression, this.parseRelational());
    }
    if (isThenNext(Id.GTE)) {
      checkEndOfExpression(Id.GTE);
      return new Call(
          start, GreaterThanOrEqualOperator.INSTANCE, expression, this.parseRelational());
    }
    if (isThenNext(Id.LT)) {
      checkEndOfExpression(Id.LT);
      return new Call(start, LessThanOperator.INSTANCE, expression, this.parseRelational());
    }
    if (isThenNext(Id.LTE)) {
      checkEndOfExpression(Id.LTE);
      return new Call(start, LessThanOrEqualOperator.INSTANCE, expression, this.parseRelational());
    }

    return expression;
  }

  /** <code>FactorExpression ( (+ | - | "||") FactorExpression )*</code> */
  private IExpression parseAdditive() throws ExpressionException {
    IExpression expression = this.parseFactor();
    while (hasNext()) {
      int start = this.getPosition();

      if (isThenNext(Id.PLUS)) {
        checkEndOfExpression(Id.PLUS);
        expression = new Call(start, AddOperator.INSTANCE, expression, this.parseFactor());
      } else if (isThenNext(Id.MINUS)) {
        checkEndOfExpression(Id.MINUS);
        expression = new Call(start, SubtractOperator.INSTANCE, expression, this.parseFactor());
      } else if (isThenNext(Id.CONCAT)) {
        checkEndOfExpression(Id.CONCAT);
        expression = new Call(start, ConcatFunction.INSTANCE, expression, this.parseFactor());
      } else break;
    }

    return expression;
  }

  /** Parse array <code>[exp1, exp2...]</code> */
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
      throw new ExpressionParseException(
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
      throw new ExpressionParseException(token.start(), ErrorCode.INVALID_DATE, token.text());
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
      throw new ExpressionParseException(token.start(), ErrorCode.INVALID_TIMESTAMP, token.text());
    }
  }

  /**
   * Parses a list of expressions separated by commas (function arguments or IN predicate value
   * list).
   *
   * <p><code>expression [,expression...]</code>
   */
  private List<IExpression> parseListExpression() {
    List<IExpression> list = new ArrayList<>();

    // No argument function
    if (is(Id.RPARENTHESIS)) {
      return list;
    }

    // <expression [, expression>]
    boolean first = true;
    do {
      if (isThenNext(Id.RPARENTHESIS)) {
        if (!first) {
          throw new ExpressionParseException(getPosition(), ErrorCode.MISSING_ELEMENT);
        }
        return list;
      }
      if (hasNext()) {
        list.add(this.parseLogicalOr());
      }
      first = false;
    } while (isThenNext(Id.COMMA));

    return list;
  }

  /** Case When Then Else End */
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
        simple ? CaseSimpleOperator.INSTANCE : CaseSearchOperator.INSTANCE,
        valueExpression,
        new Array(whenList),
        new Array(thenList),
        elseExpression);
  }

  /** Cast function <code>CAST(value AS type [FORMAT pattern])</code> */
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

    operands.add(this.parseLiteralType(next()));

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

  /** Parse function <code>POSITION(expression IN expression)</code> */
  private IExpression parseFunctionPosition(final Token token, final Function function)
      throws ExpressionException {
    List<IExpression> operands = new ArrayList<>();

    // Check argument will detect an error later
    if (isThenNext(Id.RPARENTHESIS)) {
      return new Call(token.start(), function, operands);
    }

    if (!hasNext()) {
      throw new ExpressionParseException(
          token.end(), ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());
    }

    operands.add(this.parseAdditive());

    // Check argument will detect an error later
    if (isThenNext(Id.RPARENTHESIS)) {
      return new Call(token.start(), function, operands);
    }

    if (isNotThenNext(Id.IN)) {
      throw new ExpressionParseException(
          getPosition(), ErrorCode.SYNTAX_ERROR_FUNCTION, function.getName());
    }

    if (!hasNext()) {
      throw new ExpressionParseException(
          token.end(), ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());
    }

    operands.add(this.parseAdditive());

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionParseException(token.end(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
    }

    return new Call(token.start(), function, operands);
  }

  /** Parse function <code>FIRST_VALUE(expression) [ IGNORE NULLS | RESPECT NULLS ]</code> */
  private IExpression parseFunctionFirstValue(Token token, Function function)
      throws ExpressionException {

    List<IExpression> operands = this.parseListExpression();

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionParseException(token.end(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
    }

    // NULL treatment clause
    if (this.parseNullTreatment(function)) {
      function = FirstValueFunction.FIRST_VALUE_IGNORE_NULLS;
    } else {
      function = FirstValueFunction.FIRST_VALUE_RESPECT_NULLS;
    }

    return new Call(token.start(), function, operands);
  }

  /** Parse function <code>LAST_VALUE(expression) [ IGNORE NULLS | RESPECT NULLS ]</code> */
  private IExpression parseFunctionLastValue(Token token, Function function)
      throws ExpressionException {

    List<IExpression> operands = this.parseListExpression();

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionParseException(token.end(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
    }

    // NULL treatment clause
    if (this.parseNullTreatment(function)) {
      function = LastValueFunction.LAST_VALUE_IGNORE_NULLS;
    } else {
      function = LastValueFunction.LAST_VALUE_RESPECT_NULLS;
    }

    return new Call(token.start(), function, operands);
  }

  /** Parse function <code>NTH_VALUE(expression, offset) [ IGNORE NULLS | RESPECT NULLS ]</code> */
  private IExpression parseFunctionNthValue(Token token, Function function)
      throws ExpressionException {

    List<IExpression> operands = this.parseListExpression();

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionParseException(token.end(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
    }

    // NULL treatment clause
    if (this.parseNullTreatment(function)) {
      function = NthValueFunction.NTH_VALUE_IGNORE_NULLS;
    } else {
      function = NthValueFunction.NTH_VALUE_RESPECT_NULLS;
    }

    return new Call(token.start(), function, operands);
  }

  /**
   * Parse NULL treatment clause <code>[ IGNORE NULLS | RESPECT NULLS ]</code>
   *
   * @return true if the function should ignore null values
   */
  private boolean parseNullTreatment(Function function) throws ExpressionException {
    if (isThenNext(Id.IGNORE)) {
      if (isThenNext(Id.NULLS)) {
        return true;
      }
      throw new ExpressionParseException(getPosition(), ErrorCode.SYNTAX_ERROR_FUNCTION, function);
    }
    if (isThenNext(Id.RESPECT)) {
      if (isThenNext(Id.NULLS)) {
        return false;
      }
      throw new ExpressionParseException(getPosition(), ErrorCode.SYNTAX_ERROR_FUNCTION, function);
    }

    return false;
  }

  /** Parse function <code>EXTRACT(part FROM expression)</code> */
  private IExpression parseFunctionExtract(Token token, Function function)
      throws ExpressionException {

    List<IExpression> operands = new ArrayList<>();

    // Check argument will detect an error later
    if (isThenNext(Id.RPARENTHESIS)) {
      return new Call(token.start(), function, operands);
    }

    if (!hasNext()) {
      throw new ExpressionParseException(
          getPosition(), ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());
    }

    operands.add(this.parseLiteralTimeUnit(next()));

    // Check argument will detect an error later
    if (isThenNext(Id.RPARENTHESIS)) {
      return new Call(token.start(), function, operands);
    }

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

  /** Parse function <code>COUNT(*) | COUNT([DISTINCT] expression)</code> */
  private IExpression parseFunctionCount(Token token, Function function)
      throws ExpressionException {

    AggregateFunction aggregator = CountFunction.COUNT_VALUE;
    List<IExpression> operands = new ArrayList<>();

    // COUNT(*) no operand
    if (isThenNext(Id.STAR)) {
      aggregator = CountFunction.COUNT_ALL;
      // Use fictive operand
      operands.add(Literal.NULL);
    } else if (isThenNext(Id.DISTINCT)) {
      operands = this.parseListExpression();
      aggregator = CountFunction.COUNT_DISTINCT;
    } else {
      operands = this.parseListExpression();
    }

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionParseException(token.start(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
    }

    return new Call(token.start(), aggregator, operands);
  }

  /** Parse function <code>LISTAGG([DISTINCT] expression [, delimiter] )</code> */
  private IExpression parseFunctionListAgg(Token token, Function function)
      throws ExpressionException {

    AggregateFunction aggregator = ListAggFunction.LISTAGG_ALL;

    if (isThenNext(Id.DISTINCT)) {
      aggregator = ListAggFunction.LISTAGG_DISTINCT;
    }

    List<IExpression> operands = this.parseListExpression();

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionParseException(token.start(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
    }

    return new Call(token.start(), aggregator, operands);
  }

  /**
   * Parse function <code>JSON_OBJECT([KEY] key VALUE expression [, [KEY] key VALUE expression]...)
   * </code>
   */
  private IExpression parseFunctionJsonObject(Token token, Function function)
      throws ExpressionException {

    List<IExpression> operands = new ArrayList<>();

    // Check argument will detect an error later
    if (isThenNext(Id.RPARENTHESIS)) {
      return new Call(token.start(), function, operands);
    }

    if (!hasNext()) {
      throw new ExpressionParseException(token.start(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
    }

    do {
      if (isThenNext(Id.KEY)) {
        // KEY is optional
      }

      if (hasNext()) {
        operands.add(this.parseLiteralString(next()));
      } else {
        throw new ExpressionParseException(
            token.start(), ErrorCode.SYNTAX_ERROR_FUNCTION, function.getName());
      }

      if (isThenNext(Id.VALUE)) {
        operands.add(this.parsePrimary());
      } else {
        throw new ExpressionParseException(
            token.start(), ErrorCode.SYNTAX_ERROR_FUNCTION, function.getName());
      }

    } while (isThenNext(Id.COMMA));

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionParseException(token.start(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
    }

    return new Call(token.start(), function, operands);
  }

  /** Parse function <code>JSON_VALUE(json, path [RETURNING type])</code> */
  private IExpression parseFunctionJsonValue(Token token, Function function)
      throws ExpressionException {

    List<IExpression> operands = new ArrayList<>();
    if (isThenNext(Id.RPARENTHESIS)) {
      return new Call(token.start(), function, operands);
    }

    operands.add(this.parseLogicalOr());
    if (isThenNext(Id.COMMA)) {
      operands.add(this.parseLogicalOr());
      if (isThenNext(Id.RETURNING)) {
        operands.add(this.parseLiteralType(next()));
      }
    }
    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionParseException(token.start(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
    }

    return new Call(token.start(), function, operands);
  }

  /** Parse function */
  private IExpression parseFunction(Token token) throws ExpressionException {
    Function function = FunctionRegistry.getFunction(token.text());

    // Function is never null
    if (function == null) {
      throw new ExpressionParseException(
          token.start(), ErrorCode.FUNCTION_DOES_NOT_EXIST, token.text());
    }

    if (isNotThenNext(Id.LPARENTHESIS)) {
      throw new ExpressionParseException(token.end(), ErrorCode.MISSING_LEFT_PARENTHESIS);
    }

    // Parse function with custom syntax
    switch (token.text()) {
      case "CAST", "TRY_CAST" -> {
        return parseFunctionCast(token, function);
      }
      case "EXTRACT" -> {
        return parseFunctionExtract(token, function);
      }
      case "POSITION" -> {
        return parseFunctionPosition(token, function);
      }
      case "COUNT" -> {
        return parseFunctionCount(token, function);
      }
      case "FIRST_VALUE" -> {
        return this.parseFunctionFirstValue(token, function);
      }
      case "LAST_VALUE" -> {
        return this.parseFunctionLastValue(token, function);
      }
      case "NTH_VALUE" -> {
        return this.parseFunctionNthValue(token, function);
      }
      case "LISTAGG" -> {
        return this.parseFunctionListAgg(token, function);
      }
      case "JSON_OBJECT" -> {
        return this.parseFunctionJsonObject(token, function);
      }
      case "JSON_VALUE" -> {
        return this.parseFunctionJsonValue(token, function);
      }
    }

    List<IExpression> operands = this.parseListExpression();

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
      throw new ExpressionParseException(token.start(), ErrorCode.INVALID_INTERVAL, "");
    if (value.is(Id.MINUS)) {
      negative = true;
      this.checkEndOfExpression(Id.INTERVAL);
      value = next();
      if (value == null)
        throw new ExpressionParseException(token.start(), ErrorCode.INVALID_INTERVAL, "");
    }

    checkEndOfExpression(Id.INTERVAL);
    Token start = next();
    TimeUnit startUnit = TimeUnit.of(start.text());

    // Verbose format
    if (startUnit == null) {
      previous();

      Interval interval = Interval.of(value.text());
      return Literal.of(interval);
    }

    // Short format with qualifier
    TimeUnit endUnit = null;
    String text = value.text();
    if (value.is(Id.LITERAL_STRING)) {
      Token end;
      if (this.isThenNext(Id.TO)) {
        checkEndOfExpression(Id.INTERVAL);
        end = next();
        if (end == null)
          throw new ExpressionParseException(token.start(), ErrorCode.INVALID_INTERVAL, text);
        endUnit = TimeUnit.of(end.text());
      }

      if (!text.isEmpty() && text.charAt(0) == '-') {
        negative = true;
        text = text.substring(1);
      }
    }

    IntervalQualifier qualifier = IntervalQualifier.of(startUnit, endUnit);
    if (qualifier == null)
      throw new ExpressionParseException(token.start(), ErrorCode.INVALID_INTERVAL, text);

    Interval interval = qualifier.parse(text);
    if (interval == null)
      throw new ExpressionParseException(token.start(), ErrorCode.INVALID_INTERVAL, text);

    if (negative) interval = interval.negate();

    return Literal.of(interval);
  }

  private IExpression parseLiteralType(Token token) throws ExpressionException {

    TypeName name = TypeName.of(token.text());
    if (name == null) {
      throw new ExpressionParseException(token.start(), ErrorCode.INVALID_TYPE, token.text());
    }

    int precision = Type.PRECISION_NOT_SPECIFIED;
    int scale = Type.SCALE_NOT_SPECIFIED;
    if (isThenNext(Id.LPARENTHESIS)) {

      // Precision
      try {
        precision = Integer.parseInt(next().text());
      } catch (NumberFormatException e) {
        throw new ExpressionParseException(
            token.start(), ErrorCode.EXPECTED_INTEGER_CONSTANT_AS_PRECISION);
      }
      if (!name.supportsPrecision()) {
        throw new ExpressionParseException(
            token.start(), ErrorCode.PRECISION_NOT_SUPPORTED, token.text());
      }

      // Scale
      if (isThenNext(Id.COMMA)) {
        checkEndOfExpression(token.id());
        try {
          scale = Integer.parseInt(next().text());
        } catch (NumberFormatException e) {
          throw new ExpressionParseException(
              token.start(), ErrorCode.EXPECTED_INTEGER_CONSTANT_AS_SCALE);
        }
        if (!name.supportsScale()) {
          throw new ExpressionParseException(
              token.start(), ErrorCode.SCALE_NOT_SUPPORTED, token.text());
        }
      }

      if (isNotThenNext(Id.RPARENTHESIS)) {
        throw new ExpressionParseException(token.start(), ErrorCode.MISSING_RIGHT_PARENTHESIS);
      }
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
              throw new ExpressionParseException(
                  token.start(), ErrorCode.INVALID_TYPE, token.text());
        };

    return Literal.of(type);
  }
}
