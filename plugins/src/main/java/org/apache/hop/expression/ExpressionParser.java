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

import org.apache.commons.lang.StringUtils;
import org.apache.hop.expression.Token.Id;
import org.apache.hop.expression.operator.Function;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.NumberFormat;
import org.apache.hop.i18n.BaseMessages;
import java.math.BigDecimal;
import java.text.ParseException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

public class ExpressionParser {

  private static final Class<?> PKG = IExpression.class; // for i18n purposes

  private final String source;

  private List<Token> tokens = new ArrayList<>();
  private int index = 0;

  public static IExpression parse(String source) throws ExpressionException {
    ExpressionParser parser = new ExpressionParser(source);
    try {
      IExpression expression = parser.parse();
      return expression;
    } catch (ParseException e) {
      throw createExpressionException(source, e.getErrorOffset(), e);
    } catch (ExpressionException | IllegalArgumentException e) {
      throw createExpressionException(source, parser.getPosition(), e);
    }
  }

  protected static ExpressionException createExpressionException(String source, int offset,
      Exception e) {
    int line = 1;
    int column = 1;
    for (int index = 0; index <= offset; index++) {
      char c = source.charAt(index);
      if (c == '\n' || c == '\r') {
        line++;
        column = 1;
      }
      else column++;
    }
    String message = BaseMessages.getString(PKG, "Expression.SyntaxError", line, column, e.getMessage());
    return new ExpressionException(message, e);
  }

  protected ExpressionParser(String source) {
    super();
    this.source = source;
  }

  protected int getPosition() {

    if (index > 0 && index < tokens.size())
      return this.next().start();

    return source.length();
  }

  protected boolean hasNext() {
    return index < tokens.size();
  }

  protected Token next() {
    if (!hasNext())
      return null;
    Token token = tokens.get(index);
    index++;
    return token;
  }

  protected boolean next(Id id) {
    if (hasNext()) {
      if (tokens.get(index).is(id)) {
        index++;
        return true;
      }
    }
    return false;
  }

  protected boolean is(Id id) {
    if (hasNext()) {
      if (tokens.get(index).is(id)) {
        return true;
      }
    }
    return false;
  }

  private IExpression parse() throws ParseException {
    // System.out.println("Parse: " + source);

    if (StringUtils.isEmpty(source))
      return Literal.NULL;

    // Tokenize
    ExpressionScanner scanner = new ExpressionScanner(source);
    for (Token token = scanner.tokenize(); token != null; token = scanner.tokenize()) {

      // Ignore comment
      if (token.is(Id.COMMENT))
        continue;

      tokens.add(token);
    }

    IExpression expression = this.parseLogicalOr();

    if (hasNext()) {
      throw new ParseException(next().text(), this.getPosition());
    }

    return expression;
  }

  /**
   * Parse logical OR expression
   *
   * <p>
   * LogicalAndExpression ( OR LogicalAndExpression )*
   */
  private IExpression parseLogicalOr() throws ParseException {
    IExpression expression = this.parseLogicalAnd();
    while (next(Id.OR)) {
      expression = new OperatorCall(Operator.BOOLOR, expression, parseLogicalAnd());
    }

    return expression;
  }

  /**
   * Parse logical AND expression
   *
   * <p>
   * LogicalNotExpression ( AND LogicalNotExpression )*
   *
   * @return Expression
   */
  private IExpression parseLogicalAnd() throws ParseException {
    IExpression expression = this.parseLogicalNot();
    while (next(Id.AND)) {
      expression = new OperatorCall(Operator.BOOLAND, expression, parseLogicalNot());
    }

    return expression;
  }

  /**
   * Parse logical NOT expression
   *
   * <p>
   * [NOT] RelationalExpression
   */
  private IExpression parseLogicalNot() throws ParseException {

    if (next(Id.NOT)) {
      return new OperatorCall(Operator.BOOLNOT, parseLogicalNot());
    }

    return this.parseIs();
  }

  /**
   * Parse IS expression
   *
   * <p>
   * RelationalExpression [NOT] LiteralBasicExpression
   */
  private IExpression parseIs() throws ParseException {
    IExpression expression = this.parseComparaison();
    if (next(Id.IS)) {
      boolean not = false;
      if (next(Id.NOT)) {
        not = true;
      }
      IExpression result = new OperatorCall(Operator.IS, expression, parseLiteralBasic());
      if (not)
        return new OperatorCall(Operator.BOOLNOT, result);
      return result;
    }
    return expression;
  }

  /**
   * Parse IN, LIKE, BETWEEN expression
   *
   * <p>
   * AdditiveExpression ( [NOT] | InClause | BetweenClause | LikeClause AdditiveExpression)
   */
  private IExpression parseRelational() throws ParseException {
    IExpression expression = this.parseAdditive();

    // Special case NOT before operation: <exp> [NOT] LIKE|IN|BETWEEN <primaryExp>
    boolean not = false;
    if (next(Id.NOT)) {
      not = true;
    }

    if (next(Id.LIKE)) {
      IExpression pattern = this.parseAdditive();

      if (next(Id.ESCAPE)) {
        IExpression escape = this.parseCast();
        expression = new OperatorCall(Operator.LIKE, expression, pattern, escape);
      } else
        expression = new OperatorCall(Operator.LIKE, expression, pattern);
    } else if (next(Id.ILIKE)) {
      IExpression pattern = this.parseAdditive();

      if (next(Id.ESCAPE)) {
        IExpression escape = this.parseAdditive();
        expression = new OperatorCall(Operator.ILIKE, expression, pattern, escape);
      } else
        expression = new OperatorCall(Operator.ILIKE, expression, pattern);
    } else if (next(Id.IN)) {
      expression = new OperatorCall(Operator.IN, expression, this.parseList());
    } else if (next(Id.BETWEEN)) {
      IExpression begin = this.parseAdditive();
      if (!next(Id.AND)) {
        throw new ParseException(
            BaseMessages.getString(PKG, "Expression.InvalidOperator", Id.BETWEEN),
            this.getPosition());
      }
      IExpression end = this.parseAdditive();

      expression = new OperatorCall(Operator.BETWEEN, expression, begin, end);
    }

    if (not) {
      return new OperatorCall(Operator.BOOLNOT, expression);
    }

    return expression;
  }

  /**
   * BitwiseNotExpression ( (* | / | %) BitwiseNotExpression())*
   *
   * @return Expression
   */
  private IExpression parseFactor() throws ParseException {
    IExpression expression = this.parseBitwiseNot();

    while (hasNext()) {
      if (next(Id.MULTIPLY)) {
        expression = new OperatorCall(Operator.MULTIPLY, expression, this.parseBitwiseNot());
      } else if (next(Id.DIVIDE)) {
        expression = new OperatorCall(Operator.DIVIDE, expression, this.parseBitwiseNot());
      } else if (next(Id.MODULUS)) {
        expression = new OperatorCall(Operator.MODULUS, expression, this.parseBitwiseNot());
      } else
        break;
    }

    return expression;
  }

  /** ( UnaryExpression)* */
  private IExpression parseBitwiseNot() throws ParseException {
    if (next(Id.BITWISE_NOT)) {
      return new OperatorCall(Operator.BITNOT, this.parseUnary());
    }
    return this.parseUnary();
  }

  /** UnaryExpression ( & UnaryExpression)* */
  private IExpression parseBitwiseAnd() throws ParseException {
    IExpression expression = this.parseFactor();
    if (next(Id.BITWISE_AND)) {
      return new OperatorCall(Operator.BITAND, expression, this.parseFactor());
    }
    return expression;
  }

  /** BitwiseXorExpression ( | BitwiseXorExpression)* */
  private IExpression parseBitwiseOr() throws ParseException {
    IExpression expression = this.parseBitwiseXor();
    if (next(Id.BITWISE_OR)) {
      return new OperatorCall(Operator.BITOR, expression, this.parseBitwiseXor());
    }
    return expression;
  }

  /** BitwiseAndExpression ( ^ BitwiseAndExpression)* */
  private IExpression parseBitwiseXor() throws ParseException {
    IExpression expression = this.parseBitwiseAnd();
    if (next(Id.BITWISE_XOR)) {
      return new OperatorCall(Operator.BITXOR, expression, this.parseBitwiseAnd());
    }
    return expression;
  }

  /** (('+' | '-') PrimaryExpression ) */
  private IExpression parseUnary() throws ParseException {

    if (next(Id.MINUS)) {
      return new OperatorCall(Operator.NEGATIVE, this.parseCast());
    }
    if (next(Id.PLUS)) {
      // Ignore
    }

    return this.parseCast();
  }

  /** Literal TRUE | FALSE | NULL */
  private Literal parseLiteralBasic() throws ParseException {

    Token token = next();

    if (token == null)
      throw new ParseException(BaseMessages.getString(PKG, "Expression.UnexpectedEndOfExpression"),
          this.getPosition());

    switch (token.id()) {
      case TRUE:
        return Literal.TRUE;
      case FALSE:
        return Literal.FALSE;
      case NULL:
        return Literal.NULL;
      default:
        // Syntax error
        throw new ParseException(" ERROR2 ", token.start());
    }
  }

  /** Literal text */
  private Literal parseLiteralText(Token token) {
    return new Literal(token.text());
  }

  /** Term = Literal | Identifier | Function | '(' Expression ')' */
  private IExpression parseTerm() throws ParseException {
    Token token = next();

    if (token != null) {
      switch (token.id()) {
        case TRUE:
          return Literal.TRUE;
        case FALSE:
          return Literal.FALSE;
        case NULL:
          return Literal.NULL;
        case IDENTIFIER:
          return new Identifier(token.text());
        case LITERAL_STRING:
          return parseLiteralText(token);
        case LITERAL_NUMBER:
          return parseLiteralNumber(token);
        case LITERAL_BINARY_HEX:
          return parseLiteralBinaryHexa(token);
        case LITERAL_BINARY_BIT:
          return parseLiteralBinaryBit(token);
        case DATE:
          return parseLiteralDate(next());
        case TIME:
          return parseLiteralTime(next());
        case TIMESTAMP:
          return parseLiteralTimestamp(next());
        case CASE:
          // FIXME: Case is not at the good place
          return parseCaseWhen();
        case FUNCTION:
          return parseFunction(token);
        case LPARENTHESIS:
          IExpression expression = this.parseLogicalOr();

          token = next();
          if (token.is(Id.RPARENTHESIS)) {
            return expression;
          }
          throw new ParseException(BaseMessages.getString(PKG, "Expression.UnbalancedParenthesis"),
              token.start());
        default:
          // Syntax error
      }
    }
    throw new ParseException(BaseMessages.getString(PKG, "Expression.UnexpectedEndOfExpression"),
        this.getPosition());
  }

  /** RelationalExpression ( Operator RelationalExpression ) */
  private IExpression parseComparaison() throws ParseException {
    IExpression expression = this.parseRelational();

    if (next(Id.EQUAL)) {
      return new OperatorCall(Operator.EQUAL, expression, this.parseRelational());
    }
    if (next(Id.NOT_EQUAL)) {
      return new OperatorCall(Operator.NOT_EQUAL, expression, this.parseRelational());
    }
    if (next(Id.LESS_THAN_OR_GREATER_THAN)) {
      return new OperatorCall(Operator.LESS_THAN_OR_GREATER_THAN, expression,
          this.parseRelational());
    }
    if (next(Id.GREATER_THAN)) {
      return new OperatorCall(Operator.GREATER_THAN, expression, this.parseRelational());
    }
    if (next(Id.GREATER_THAN_OR_EQUAL)) {
      return new OperatorCall(Operator.GREATER_THAN_OR_EQUAL, expression, this.parseRelational());
    }
    if (next(Id.LESS_THAN)) {
      return new OperatorCall(Operator.LESS_THAN, expression, this.parseRelational());
    }
    if (next(Id.LESS_THAN_OR_EQUAL)) {
      return new OperatorCall(Operator.LESS_THAN_OR_EQUAL, expression, this.parseRelational());
    }

    return expression;
  }

  /** BitwiseOrExpression ( (+ | - | ||) BitwiseOrExpression )* */
  private IExpression parseAdditive() throws ParseException {
    IExpression expression = this.parseBitwiseOr();
    while (hasNext()) {
      if (next(Id.PLUS)) {
        expression = new OperatorCall(Operator.ADD, expression, this.parseBitwiseOr());
      } else if (next(Id.MINUS)) {
        expression = new OperatorCall(Operator.SUBTRACT, expression, this.parseBitwiseOr());
      } else if (next(Id.CONCAT)) {
        expression = new OperatorCall(Operator.CONCAT, expression, this.parseBitwiseOr());
      } else
        break;
    }

    return expression;
  }

  private Literal parseLiteralNumber(Token token) throws ParseException {
    BigDecimal number = NumberFormat.ofPattern("TM").parse(token.text());
    return Literal.of(number);
  }

  private Literal parseLiteralBinaryHexa(Token token) throws ParseException {

    String s = token.text();

    if (s.length() % 2 > 0)
      s = '0' + s;

    byte[] bytes = new byte[s.length() / 2];

    for (int i = 0; i < bytes.length; i++) {
      int start = i * 2;
      bytes[i] = (byte) Integer.parseInt(s.substring(start, start + 2), 16);
    }

    if (bytes.length <= 8) {
      // Value as integer if less than or equals 8 bytes
      long result = 0;
      for (int i = 0; i < bytes.length; i++) {
        result = result << 8;
        result = result | (bytes[i] & 0xFF);
      }
      return new Literal(result);
    }

    return new Literal(bytes);
  }

  private Literal parseLiteralBinaryBit(Token token) throws ParseException {

    String s = token.text();
    BitSet bitset = new BitSet(s.length());

    int length = s.length();
    for (int i = length - 1; i >= 0; i--) {
      if (s.charAt(i) == '1') {
        bitset.set(length - i - 1);
      }
    }

    if (bitset.length() <= 32) {
      // Value as integer if less than or equals 32 bits
      return new Literal(bitset.toLongArray()[0]);
    }

    return new Literal(bitset.toByteArray());
  }

  /**
   * Parses a date literal. The parsing is strict and requires months to be less than 12, days to be
   * less than 31, etc.
   */
  private Literal parseLiteralDate(Token token) throws ParseException {
    try {
      DateTimeFormat format = DateTimeFormat.ofPattern("YYYY-MM-DD");
      Instant instant = format.parse(token.text());
      return new Literal(instant);
    } catch (Exception e) {
      throw new ParseException(BaseMessages.getString(PKG, "Expression.InvalidDate", token.text()),
          token.start());
    }
  }

  /** Parses a time literal. */
  private Literal parseLiteralTime(Token token) throws ParseException {
    try {
      DateTimeFormat format =
          DateTimeFormat.ofPattern("HH12:MI:SS AM|HH24:MI:SS|HH12:MI AM|HH24:MI");
      Instant instant = format.parse(token.text());
      return new Literal(instant);
    } catch (Exception e) {
      throw new ParseException(BaseMessages.getString(PKG, "Expression.InvalidTime", token.text()),
          token.start());
    }
  }

  /**
   * Parses a timestamp literal. The parsing is strict and requires months to be less than 12, days
   * to be
   * less than 31, etc.
   */
  private Literal parseLiteralTimestamp(Token token) throws ParseException {
    try {
      String pattern =
          "YYYY-MM-DD HH24:MI:SS.FF|YYYY-MM-DD HH24:MI:SS TZH:TZM|YYYY-MM-DD HH24:MI:SS";
      switch (token.text().length()) {
        case 29:
          pattern = "YYYY-MM-DD HH24:MI:SS.FF9";
          break;
        case 26:
          if (token.text().indexOf('.') > 0)
            pattern = "YYYY-MM-DD HH24:MI:SS.FF6";
          else
            pattern = "YYYY-MM-DD HH24:MI:SS TZH:TZM";
          break;
        case 23:
          pattern = "YYYY-MM-DD HH24:MI:SS.FF3";
          break;
        case 20:
          pattern = "YYYY-MM-DD HH24:MI:SS";
          break;

        default:
          pattern = "YYYY-MM-DD HH24:MI:SS.FF|YYYY-MM-DD HH24:MI:SS TZH:TZM|YYYY-MM-DD HH24:MI:SS";
      }

      DateTimeFormat format = DateTimeFormat.ofPattern(pattern);
      Instant instant = format.parse(token.text());
      return new Literal(instant);
    } catch (Exception e) {
      throw new ParseException(
          BaseMessages.getString(PKG, "Expression.InvalidTimestamp", token.text()), token.start());
    }
  }

  /** Parses a list of expressions separated by commas. */
  private ExpressionList parseList() throws ParseException {

    List<IExpression> list = new ArrayList<>();

    if (next(Id.LPARENTHESIS)) {

      do {
        list.add(parseCast());

        if (is(Id.COMMA)) {
          next();
          continue;
        }

        if (is(Id.RPARENTHESIS)) {
          next();
          return new ExpressionList(list);
        }

        break;
      } while (true);
    }
    throw new ParseException(BaseMessages.getString(PKG, "Expression.MissingRightParenthesis"),
        this.getPosition());
  }

  /** Case When Then Else End ) */
  private IExpression parseCaseWhen() throws ParseException {
    IExpression valueExpression = null;
    IExpression elseExpression = Literal.NULL;
    List<IExpression> whenList = new ArrayList<>();
    List<IExpression> thenList = new ArrayList<>();

    // Form Switch value
    if (!is(Id.WHEN)) {
      valueExpression = this.parseLogicalOr();
    }

    // Form multi boolean condition
    while (next(Id.WHEN)) {
      whenList.add(this.parseLogicalOr());
      if (!next(Id.THEN)) {
        throw new ParseException(BaseMessages.getString(PKG, "Expression.InvalidOperator", Id.CASE),
            this.getPosition());
      }
      thenList.add(this.parseLogicalOr());
    }

    if (next(Id.ELSE)) {
      elseExpression = this.parseLogicalOr();
    }

    if (!next(Id.END)) {
      throw new ParseException(BaseMessages.getString(PKG, "Expression.InvalidOperator", Id.CASE),
          this.getPosition());
    }

    return new OperatorCall(Operator.CASE, valueExpression, new ExpressionList(whenList),
        new ExpressionList(thenList), elseExpression);
  }

  /**
   * Cast operator ::
   *
   */
  private IExpression parseCast() throws ParseException {
    IExpression expression = this.parseTerm();
    if (next(Id.CAST)) {
      Type type = parseType();
      return new OperatorCall(Operator.CAST, expression, new Literal(type));
    }
    return expression;
  }

  /** Function */
  private IExpression parseFunction(Token token) throws ParseException {

    Function function = OperatorRegistry.getFunction(token.text());
    List<IExpression> operands = new ArrayList<>();

    if (is(Id.LPARENTHESIS))
      token = next();
    else {
      throw new ParseException(BaseMessages.getString(PKG, "Expression.MissingLeftParenthesis"),
          token.start());
    }

    /** Cast(expression AS dataType FORMAT pattern) */
    if ("CAST".equals(function.getName()) || "TRY_CAST".equals(function.getName())) {

      operands.add(this.parseLogicalOr());

      if (!next(Id.AS)) {
        throw new ParseException(
            BaseMessages.getString(PKG, "Expression.InvalidSyntaxFunctionCast"), token.start());
      }

      Type type = parseType();
      operands.add(new Literal(type));

      if (is(Id.FORMAT)) {
        next();
        token = next();
        if (token.is(Token.Id.LITERAL_STRING))
          operands.add(this.parseLiteralText(token));
        else
          throw new ParseException(
              BaseMessages.getString(PKG, "Expression.InvalidSyntaxFunctionCast"), token.start());
      }
    }

    /** Extract(datePart FROM expression) */
    else if ("EXTRACT".equals(function.getName())) {

      DatePart part = DatePart.of(next().text());
      operands.add(new Literal(part));

      if (!next(Id.FROM)) {
        throw new ParseException(
            BaseMessages.getString(PKG, "Expression.InvalidSyntaxFunctionExtract"),
            this.getPosition());
      }

      operands.add(this.parseLogicalOr());

      // if (next(Id.AT)) {
      // if (next(Id.TIME)) {
      // if (next(Id.ZONE)) {
      // operands.add(this.parseLogicalOr());
      // }
      // }
      // }


    } else {

      // No param function
      if (is(Id.RPARENTHESIS)) {
        next();
        function.checkNumberOfArguments(operands);
        return new OperatorCall(function, operands);
      }

      operands.add(this.parseLogicalOr());

      while (is(Id.COMMA)) {
        token = next();
        operands.add(this.parseLogicalOr());
      }
    }

    if (is(Id.RPARENTHESIS)) {
      next();
    } else {
      throw new ParseException(BaseMessages.getString(PKG, "Expression.MissingRightParenthesis"),
          token.start());
    }

    function.checkNumberOfArguments(operands);

    return new OperatorCall(function, operands);
  }

  private Type parseType() throws ParseException {
    Token token = next();
    if (token == null) {
      throw new ParseException(BaseMessages.getString(PKG, "Expression.MissingDataType"),
          this.getPosition());
    }

    try {
      return Type.of(token.text());
    } catch (RuntimeException e) {
      throw new ParseException(
          BaseMessages.getString(PKG, "Expression.InvalidDataType", token.text()), token.start());
    }
  }
}
