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
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.NumberFormat;
import org.apache.hop.i18n.BaseMessages;
import java.math.BigDecimal;
import java.text.ParseException;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.zone.ZoneRulesException;
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
      return parser.parse();
    } catch (ParseException e) {
      throw createException(source, e.getErrorOffset(), e);
    } catch (IllegalArgumentException e) {
      throw createException(source, parser.getPosition(), e);
    }
  }

  protected static ExpressionException createException(String source, int offset, Exception e) {
    int line = 1;
    int column = 1;
    for (int index = 0; index < offset; index++) {
      char c = source.charAt(index);
      if (c == '\n' || c == '\r') {
        line++;
        column = 1;
      } else
        column++;
    }
    String message =
        BaseMessages.getString(PKG, "Expression.SyntaxError", line, column, e.getMessage());
    return new ExpressionException(message);
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
    if (hasNext() && tokens.get(index).is(id)) {
      index++;
      return true;
    }
    return false;
  }

  protected boolean is(Id id) {
    if (hasNext()) {
      return tokens.get(index).is(id);
    }
    return false;
  }

  private IExpression parse() throws ParseException {

    // Tokenize
    ExpressionScanner scanner = new ExpressionScanner(source);    
    for (Token token = scanner.tokenize(); token != null; token = scanner.tokenize()) {

      // Ignore comment
      if (token.is(Id.COMMENT))
        continue;

      tokens.add(token);
    }
    
    // Empty source return NULL
    if ( tokens.isEmpty())
      return Literal.NULL;   
    
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
      expression = new OperatorCall(OperatorRegistry.BOOLOR, expression, parseLogicalAnd());
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
      expression = new OperatorCall(OperatorRegistry.BOOLAND, expression, parseLogicalNot());
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
      return new OperatorCall(OperatorRegistry.BOOLNOT, parseLogicalNot());
    }

    return this.parseIs();
  }

  /**
   * Parse IS expression
   *
   * <p>
   * RelationalExpression [NOT] LiteralBooleanExpression
   */
  private IExpression parseIs() throws ParseException {
    IExpression expression = this.parseComparaison();
    if (next(Id.IS)) {
      boolean not = false;
      if (next(Id.NOT)) {
        not = true;
      }
      IExpression result = new OperatorCall(OperatorRegistry.IS, expression, parseLiteralBoolean());
      if (not)
        return new OperatorCall(OperatorRegistry.BOOLNOT, result);
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
        IExpression escape = this.parseCastOperator();
        expression = new OperatorCall(OperatorRegistry.LIKE, expression, pattern, escape);
      } else
        expression = new OperatorCall(OperatorRegistry.LIKE, expression, pattern);
    } else if (next(Id.ILIKE)) {
      IExpression pattern = this.parseAdditive();

      if (next(Id.ESCAPE)) {
        IExpression escape = this.parseAdditive();
        expression = new OperatorCall(OperatorRegistry.ILIKE, expression, pattern, escape);
      } else
        expression = new OperatorCall(OperatorRegistry.ILIKE, expression, pattern);
    } else if (next(Id.IN)) {
      expression = new OperatorCall(OperatorRegistry.IN, expression, this.parseList());
    } else if (next(Id.BETWEEN)) {
      IExpression begin = this.parseAdditive();
      if (!next(Id.AND)) {
        throw new ParseException(
            BaseMessages.getString(PKG, "Expression.InvalidOperator", Id.BETWEEN),
            this.getPosition());
      }
      IExpression end = this.parseAdditive();

      expression = new OperatorCall(OperatorRegistry.BETWEEN, expression, begin, end);
    }

    if (not) {
      return new OperatorCall(OperatorRegistry.BOOLNOT, expression);
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
        expression =
            new OperatorCall(OperatorRegistry.MULTIPLY, expression, this.parseBitwiseNot());
      } else if (next(Id.DIVIDE)) {
        expression = new OperatorCall(OperatorRegistry.DIVIDE, expression, this.parseBitwiseNot());
      } else if (next(Id.MODULUS)) {
        expression = new OperatorCall(OperatorRegistry.MODULUS, expression, this.parseBitwiseNot());
      } else
        break;
    }

    return expression;
  }

  /** ( UnaryExpression)* */
  private IExpression parseBitwiseNot() throws ParseException {
    if (next(Id.BITWISE_NOT)) {
      return new OperatorCall(OperatorRegistry.BITNOT, this.parseUnary());
    }
    return this.parseUnary();
  }

  /** UnaryExpression ( & UnaryExpression)* */
  private IExpression parseBitwiseAnd() throws ParseException {
    IExpression expression = this.parseFactor();
    if (next(Id.BITWISE_AND)) {
      return new OperatorCall(OperatorRegistry.BITAND, expression, this.parseFactor());
    }
    return expression;
  }

  /** BitwiseXorExpression ( | BitwiseXorExpression)* */
  private IExpression parseBitwiseOr() throws ParseException {
    IExpression expression = this.parseBitwiseXor();
    if (next(Id.BITWISE_OR)) {
      return new OperatorCall(OperatorRegistry.BITOR, expression, this.parseBitwiseXor());
    }
    return expression;
  }

  /** BitwiseAndExpression ( ^ BitwiseAndExpression)* */
  private IExpression parseBitwiseXor() throws ParseException {
    IExpression expression = this.parseBitwiseAnd();
    if (next(Id.BITWISE_XOR)) {
      return new OperatorCall(OperatorRegistry.BITXOR, expression, this.parseBitwiseAnd());
    }
    return expression;
  }

  /** (('+' | '-') PrimaryExpression ) */
  private IExpression parseUnary() throws ParseException {

    if (next(Id.MINUS)) {
      return new OperatorCall(OperatorRegistry.NEGATIVE, this.parseCastOperator());
    }
    if (next(Id.PLUS)) {
      // Ignore
    }

    return this.parseCastOperator();
  }

  /** Literal TRUE | FALSE | NULL */
  private Literal parseLiteralBoolean() throws ParseException {

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
    return Literal.of(token.text());
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
        case LITERAL_DATEPART:
          return parseLiteralDatePart(token);
        case LITERAL_DATATYPE:
          return parseLiteralDataType(token);
        case DATE:
          return parseLiteralDate(next());
        case TIME:
          return parseLiteralTime(next());
        case TIMESTAMP:
          return parseLiteralTimestamp(next());
        case CASE:
          return parseCaseWhen();
        case CAST:
        case TRY_CAST:
          return parseCastFunction(token);
        case EXTRACT:
          return parseExtractFunction(token);
        case POSITION:
          return parsePositionFunction(token);
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
      return new OperatorCall(OperatorRegistry.EQUAL, expression, this.parseRelational());
    }
    if (next(Id.NOT_EQUAL)) {
      return new OperatorCall(OperatorRegistry.NOT_EQUAL, expression, this.parseRelational());
    }
    if (next(Id.LESS_THAN_OR_GREATER_THAN)) {
      return new OperatorCall(OperatorRegistry.LESS_THAN_OR_GREATER_THAN, expression,
          this.parseRelational());
    }
    if (next(Id.GREATER_THAN)) {
      return new OperatorCall(OperatorRegistry.GREATER_THAN, expression, this.parseRelational());
    }
    if (next(Id.GREATER_THAN_OR_EQUAL)) {
      return new OperatorCall(OperatorRegistry.GREATER_THAN_OR_EQUAL, expression,
          this.parseRelational());
    }
    if (next(Id.LESS_THAN)) {
      return new OperatorCall(OperatorRegistry.LESS_THAN, expression, this.parseRelational());
    }
    if (next(Id.LESS_THAN_OR_EQUAL)) {
      return new OperatorCall(OperatorRegistry.LESS_THAN_OR_EQUAL, expression,
          this.parseRelational());
    }

    return expression;
  }

  /** BitwiseOrExpression ( (+ | - | ||) BitwiseOrExpression )* */
  private IExpression parseAdditive() throws ParseException {
    IExpression expression = this.parseBitwiseOr();
    while (hasNext()) {
      if (next(Id.PLUS)) {
        expression = new OperatorCall(OperatorRegistry.ADD, expression, this.parseBitwiseOr());
      } else if (next(Id.MINUS)) {
        expression = new OperatorCall(OperatorRegistry.SUBTRACT, expression, this.parseBitwiseOr());
      } else if (next(Id.CONCAT)) {
        expression = new OperatorCall(OperatorRegistry.CONCAT, expression, this.parseBitwiseOr());
      } else
        break;
    }

    return expression;
  }

  private Literal parseLiteralNumber(Token token) throws ParseException {
    BigDecimal number = NumberFormat.of("TM").parse(token.text());
    return Literal.of(number);
  }

  private Literal parseLiteralBinaryHexa(Token token) throws ParseException {
    String str = token.text();
    if (str.length() % 2 > 0)
      str = '0' + str;
    byte[] bytes = new byte[str.length() / 2];
    for (int i = 0; i < bytes.length; i++) {
      int start = i * 2;
      bytes[i] = (byte) Integer.parseInt(str.substring(start, start + 2), 16);
    }

    return Literal.of(bytes);
  }

  private Literal parseLiteralBinaryBit(Token token) throws ParseException {

    String str = token.text();
    BitSet bitset = new BitSet(str.length());

    int length = str.length();
    for (int i = length - 1; i >= 0; i--) {
      if (str.charAt(i) == '1') {
        bitset.set(length - i - 1);
      }
    }

    return Literal.of(bitset.toByteArray());
  }

  /**
   * Parses a date literal.
   * The parsing is strict and requires months to be less than 12, days to be less than 31, etc.
   */
  private Literal parseLiteralDate(Token token) throws ParseException {
    try {
      DateTimeFormat format = DateTimeFormat.of("YYYY-MM-DD");
      ZonedDateTime datetime = format.parse(token.text());
      return Literal.of(datetime);
    } catch (Exception e) {
      throw new ParseException(BaseMessages.getString(PKG, "Expression.InvalidDate", token.text()),
          token.start());
    }
  }

  /** Parses a time literal. */
  private Literal parseLiteralTime(Token token) throws ParseException {
    try {
      DateTimeFormat format = DateTimeFormat.of("HH12:MI:SS AM|HH24:MI:SS|HH12:MI AM|HH24:MI");
      ZonedDateTime datetime = format.parse(token.text());
      return Literal.of(datetime);
    } catch (Exception e) {
      throw new ParseException(BaseMessages.getString(PKG, "Expression.InvalidTime", token.text()),
          token.start());
    }
  }

  /**
   * Parses a timestamp literal with ISO Formats.
   * 
   * The parsing is strict and requires months to be less than 12, days to be less than 31, etc.
   */
  private Literal parseLiteralTimestamp(Token token) throws ParseException {
    try {
      String pattern;
      String str = token.text();
      switch (str.length()) {
        case 36: // 2021-01-01 15:28:59.123456789 +02:00
        case 35: // 2021-01-01 15:28:59.123456789+02:00
          pattern = "YYYY-MM-DD HH24:MI:SS.FF9TZH:TZM";
          break;          
        case 33: // 2021-01-01 5:28:59.123456789+0200
        case 34: // 2021-01-01 15:28:59.123456789+0200
          pattern = "YYYY-MM-DD HH24:MI:SS.FF9TZHTZM";
          break;
        case 28: // 2021-12-01 2:01:01.123456789
        case 29: // 2021-12-01 12:01:01.123456789
          pattern = "YYYY-MM-DD HH24:MI:SS.FF9";
          break;            
        case 26:
          if ( str.indexOf('.',10)>0 )
            pattern = "YYYY-MM-DD HH24:MI:SS.FF6";
          else
            pattern = "YYYY-MM-DD HH24:MI:SSTZH:TZM";
          break;
        case 25: // 2021-01-01 15:28:59+02:00
          pattern = "YYYY-MM-DD HH24:MI:SSTZH:TZM";
          break;          
        case 24: // 2021-01-01 15:28:59+0200
                 // 2021-01-01 5:28:59+02:00
          if ( str.indexOf(':',20)>0 )
            pattern = "YYYY-MM-DD HH24:MI:SSTZH:TZM";
          else
            pattern = "YYYY-MM-DD HH24:MI:SSTZHTZM";
          break;
        case 23:
          if ( str.indexOf('.',10)>0 )
            pattern = "YYYY-MM-DD HH24:MI:SS.FF3";
          else
            pattern = "YYYY-MM-DD HH24:MI TZH:TZM";
          break;          
        case 21: // 2021-01-01 5:28+02:00
        case 22: // 2021-01-01 15:28+02:00 or  2021-01-01 5:28 +02:00          
          pattern = "YYYY-MM-DD HH24:MITZH:TZM";
          break;
        case 18:
        case 19: // 2021-04-28 20:57:48
          if (str.indexOf('+') > 0 || str.indexOf('-', 15) > 0 )
            pattern = "YYYY-MM-DD HH24:MITZH";
          else
            pattern = "YYYY-MM-DD HH24:MI:SS";                    
          break;
        case 16: // 2021-02-25 03:59
          pattern = "YYYY-MM-DD HH24:MI";
          break;
        case 13: // 2021-02-25T23
          pattern = "YYYY-MM-DD HH24";
          break;
        default:
          pattern = "YYYY-MM-DD HH24:MI:SS.FF";
      }

      DateTimeFormat format = DateTimeFormat.of(pattern);
      ZonedDateTime datetime = format.parse(token.text());
      
      // AT TIME ZONE <zoneId> 
      if (is(Id.AT)) {
        next();
        if (!next(Id.TIME) || !next(Id.ZONE)) {
          throw new ParseException(BaseMessages.getString(PKG, "Expression.InvalidTimestamp", Id.AT),
              this.getPosition());
        }
        token = next();
        ZoneId zoneId = ZoneId.of(token.text());           
        datetime =  datetime.withZoneSameLocal(zoneId);
      }
      return Literal.of(datetime);
    } catch (ZoneRulesException e) {
      throw new ParseException(
          BaseMessages.getString(PKG, "Expression.UnknownTimeZone", token.text()), token.start());
    } catch (Exception e) {
      throw new ParseException(
          BaseMessages.getString(PKG, "Expression.InvalidTimestamp", token.text()), token.start());
    }
  }

  /**
   * Parses a list of expressions separated by commas.
   * (expression [,expression...] )
   */
  private ExpressionList parseList() throws ParseException {

    List<IExpression> list = new ArrayList<>();

    if (next(Id.LPARENTHESIS)) {

      do {
        list.add(parseCastOperator());

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

    return new OperatorCall(OperatorRegistry.CASE, valueExpression, new ExpressionList(whenList),
        new ExpressionList(thenList), elseExpression);
  }

  /**
   * Cast operator [TRY_]CAST(value AS type [FORMAT pattern])
   *
   */
  private IExpression parseCastFunction(Token token) throws ParseException {
    Operator operator =
        ("CAST".equals(token.text())) ? OperatorRegistry.CAST : OperatorRegistry.TRY_CAST;

    List<IExpression> operands = new ArrayList<>();

    if (is(Id.LPARENTHESIS))
      token = next();
    else {
      throw new ParseException(BaseMessages.getString(PKG, "Expression.MissingLeftParenthesis"),
          token.start());
    }

    operands.add(this.parseLogicalOr());

    if (!next(Id.AS)) {
      throw new ParseException(BaseMessages.getString(PKG, "Expression.InvalidSyntaxFunctionCast"),
          token.start());
    }

    operands.add(this.parseLiteralDataType(next()));

    if (is(Id.FORMAT)) {
      next();
      token = next();
      if (token.is(Token.Id.LITERAL_STRING))
        operands.add(this.parseLiteralText(token));
      else
        throw new ParseException(
            BaseMessages.getString(PKG, "Expression.InvalidSyntaxFunctionCast"), token.start());
    }

    if (is(Id.RPARENTHESIS)) {
      next();
    } else {
      throw new ParseException(BaseMessages.getString(PKG, "Expression.MissingRightParenthesis"),
          token.start());
    }

    return new OperatorCall(operator, operands);
  }

  /**
   * Cast operator ::
   *
   */
  private IExpression parseCastOperator() throws ParseException {
    IExpression expression = this.parseTerm();
    if (next(Id.CAST)) {
      Literal type = parseLiteralDataType(next());
      return new OperatorCall(OperatorRegistry.CAST, expression, type);
    }
    return expression;
  }

  /** POSITION(<expression> IN <expression>) */
  private IExpression parsePositionFunction(Token token) throws ParseException {

    List<IExpression> operands = new ArrayList<>();

    if (is(Id.LPARENTHESIS))
      token = next();
    else {
      throw new ParseException(BaseMessages.getString(PKG, "Expression.MissingLeftParenthesis"),
          token.start());
    }

    operands.add(this.parseAdditive());

    if (!next(Id.IN)) {
      throw new ParseException(
          BaseMessages.getString(PKG, "Expression.InvalidSyntaxFunctionPosition"),
          this.getPosition());
    }

    operands.add(this.parseAdditive());

    if (is(Id.RPARENTHESIS)) {
      next();
    } else {
      throw new ParseException(BaseMessages.getString(PKG, "Expression.MissingRightParenthesis"),
          token.start());
    }

    return new OperatorCall(OperatorRegistry.POSITION, operands);
  }

  /** <expression> AT TIMEZONE <term> ) */
  private IExpression parseAtTimeZone() throws ParseException {
    IExpression operand = this.parseAdditive();

    if (next(Id.AT)) {
      if (next(Id.TIME) && next(Id.ZONE)) {
        return new OperatorCall(OperatorRegistry.AT_TIME_ZONE, operand, this.parseTerm());
      }
      throw new ParseException(BaseMessages.getString(PKG, "Expression.InvalidOperator", Id.AT),
          this.getPosition());
    }

    return operand;
  }

  /** EXTRACT(<part> FROM <expression>) */
  private IExpression parseExtractFunction(Token token) throws ParseException {

    if (is(Id.LPARENTHESIS))
      token = next();
    else {
      throw new ParseException(BaseMessages.getString(PKG, "Expression.MissingLeftParenthesis"),
          token.start());
    }

    List<IExpression> operands = new ArrayList<>();

    operands.add(this.parseTerm());

    if (!next(Id.FROM)) {
      throw new ParseException(
          BaseMessages.getString(PKG, "Expression.InvalidSyntaxFunctionExtract"),
          this.getPosition());
    }

    operands.add(this.parseAdditive());

    if (is(Id.RPARENTHESIS)) {
      next();
    } else {
      throw new ParseException(BaseMessages.getString(PKG, "Expression.MissingRightParenthesis"),
          token.start());
    }

    return new OperatorCall(OperatorRegistry.EXTRACT, operands);
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

    // No param function
    if (is(Id.RPARENTHESIS)) {
      next();
      return new OperatorCall(function, operands);
    }

    operands.add(this.parseAdditive());

    while (is(Id.COMMA)) {
      token = next();
      operands.add(this.parseAdditive());
    }

    if (is(Id.RPARENTHESIS)) {
      next();
    } else {
      throw new ParseException(BaseMessages.getString(PKG, "Expression.MissingRightParenthesis"),
          token.start());
    }

    return new OperatorCall(function, operands);
  }

  private Literal parseLiteralDatePart(Token token) throws ParseException {
    if (token == null) {
      throw new ParseException(BaseMessages.getString(PKG, "Expression.MissingDatePart"),
          this.getPosition());
    }
    DatePart part = DatePart.of(token.text());
    return Literal.of(part);
  }

  private Literal parseLiteralDataType(Token token) throws ParseException {
    if (token == null) {
      throw new ParseException(BaseMessages.getString(PKG, "Expression.MissingDataType"),
          this.getPosition());
    }

    try {
      DataType type = DataType.of(token.text());
      return Literal.of(type);
    } catch (RuntimeException e) {
      throw new ParseException(
          BaseMessages.getString(PKG, "Expression.InvalidDataType", token.text()), token.start());
    }
  }
}
