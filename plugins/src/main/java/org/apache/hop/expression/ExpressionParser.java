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

import org.apache.hop.expression.Token.Id;
import org.apache.hop.expression.util.Characters;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.NumberFormat;
import java.math.BigDecimal;
import java.text.ParseException;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.zone.ZoneRulesException;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;
import java.util.Set;

public class ExpressionParser {

  private static final Class<?> PKG = IExpression.class; // for i18n purposes

  private static final Set<String> RESERVED_WORDS =
     Set.of("AND", "AS",  "AT", "BETWEEN", "CASE", "DATE", "ELSE", "END",
          "ESCAPE", "FALSE", "FORMAT", "FROM", "ILIKE", "IN", "IS", "LIKE", "NOT", "NULL", "OR", "SYMMETRY",
          "THEN", "TIME", "TIMESTAMP", "TRUE", "WHEN", "XOR", "ZONE");
    
  public static Set<String> getReservedWords() {
    return RESERVED_WORDS;
  }

  public static boolean isReservedWord(final String name) {
    if (name == null)
      return false;
    return RESERVED_WORDS.contains(name.toUpperCase());
  }
  
  private final String source;

  // Char position in source
  private int position = 0;
  // Index in tokens
  private int index = 0;

  private List<Token> tokens = new ArrayList<>();
  
  public static IExpression parse(final String source) throws ExpressionException {
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
    return new ExpressionException(Error.SYNTAX_ERROR, line, column, e.getMessage());
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
    for (Token token = tokenize(); token != null; token = tokenize()) {

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
      Token token = next();
      throw new ParseException(Error.UNEXPECTED_CHARACTER.message(token.text()), token.start());
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
      expression = new Call(Operators.BOOLOR, expression, parseLogicalAnd());
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
      expression = new Call(Operators.BOOLAND, expression, parseLogicalNot());
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
      return new Call(Operators.BOOLNOT, parseLogicalNot());
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
      IExpression result = new Call(Operators.IS, expression, parseLiteralBoolean());
      if (not)
        return new Call(Operators.BOOLNOT, result);
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
        expression = new Call(Operators.LIKE, expression, pattern, escape);
      } else
        expression = new Call(Operators.LIKE, expression, pattern);
    } else if (next(Id.ILIKE)) {
      IExpression pattern = this.parseAdditive();

      if (next(Id.ESCAPE)) {
        IExpression escape = this.parseAdditive();
        expression = new Call(Operators.ILIKE, expression, pattern, escape);
      } else
        expression = new Call(Operators.ILIKE, expression, pattern);
    } else if (next(Id.IN)) {
      expression = new Call(Operators.IN, expression, this.parseTuple());
    } else if (next(Id.BETWEEN)) {
      IExpression begin = this.parseAdditive();
      if (!next(Id.AND)) {
        throw new ParseException(Error.INVALID_OPERATOR.message(Id.BETWEEN), this.getPosition());
      }
      IExpression end = this.parseAdditive();

      expression = new Call(Operators.BETWEEN, expression, begin, end);
    }

    if (not) {
      return new Call(Operators.BOOLNOT, expression);
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
            new Call(Operators.MULTIPLY, expression, this.parseBitwiseNot());
      } else if (next(Id.DIVIDE)) {
        expression = new Call(Operators.DIVIDE, expression, this.parseBitwiseNot());
      } else if (next(Id.MODULUS)) {
        expression = new Call(Operators.MODULUS, expression, this.parseBitwiseNot());
      } else
        break;
    }

    return expression;
  }

  /** ( UnaryExpression)* */
  private IExpression parseBitwiseNot() throws ParseException {
    if (next(Id.BITWISE_NOT)) {
      return new Call(Operators.BITNOT, this.parseUnary());
    }
    return this.parseUnary();
  }

  /** UnaryExpression ( & UnaryExpression)* */
  private IExpression parseBitwiseAnd() throws ParseException {
    IExpression expression = this.parseFactor();
    if (next(Id.BITWISE_AND)) {
      return new Call(Operators.BITAND, expression, this.parseFactor());
    }
    return expression;
  }

  /** BitwiseXorExpression ( | BitwiseXorExpression)* */
  private IExpression parseBitwiseOr() throws ParseException {
    IExpression expression = this.parseBitwiseXor();
    if (next(Id.BITWISE_OR)) {
      return new Call(Operators.BITOR, expression, this.parseBitwiseXor());
    }
    return expression;
  }

  /** BitwiseAndExpression ( ^ BitwiseAndExpression)* */
  private IExpression parseBitwiseXor() throws ParseException {
    IExpression expression = this.parseBitwiseAnd();
    if (next(Id.BITWISE_XOR)) {
      return new Call(Operators.BITXOR, expression, this.parseBitwiseAnd());
    }
    return expression;
  }

  /** (('+' | '-') PrimaryExpression ) */
  private IExpression parseUnary() throws ParseException {

    if (next(Id.MINUS)) {
      return new Call(Operators.NEGATIVE, this.parseCastOperator());
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
      throw new ParseException(Error.UNEXPECTED_END_OF_EXPRESSION.message(), this.getPosition());

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

  /** Literal String */
  private Literal parseLiteralString(Token token) {
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
          return parseLiteralString(token);
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
          token = next();
          if ( token==null) break;
          return parseLiteralDate(token);
        case TIME:
          token = next();
          if ( token==null) break;
          return parseLiteralTime(token);
        case TIMESTAMP:
          token = next();
          if ( token==null) break;
          return parseLiteralTimestamp(token);
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
          throw new ParseException(Error.UNBALANCE_PARENTHESIS.message(), token.start());
        default:
          // Syntax error
      }
    }
    throw new ParseException(Error.UNEXPECTED_END_OF_EXPRESSION.message(), this.getPosition());
  }

  /** RelationalExpression ( Operator RelationalExpression ) */
  private IExpression parseComparaison() throws ParseException {
    IExpression expression = this.parseRelational();

    if (next(Id.EQUAL)) {
      return new Call(Operators.EQUAL, expression, this.parseRelational());
    }
    if (next(Id.NOT_EQUAL)) {
      return new Call(Operators.NOT_EQUAL, expression, this.parseRelational());
    }
    if (next(Id.GT)) {
      return new Call(Operators.GREATER_THAN, expression, this.parseRelational());
    }
    if (next(Id.GTE)) {
      return new Call(Operators.GREATER_THAN_OR_EQUAL, expression,
          this.parseRelational());
    }
    if (next(Id.LT)) {
      return new Call(Operators.LESS_THAN, expression, this.parseRelational());
    }
    if (next(Id.LTE)) {
      return new Call(Operators.LESS_THAN_OR_EQUAL, expression,
          this.parseRelational());
    }

    return expression;
  }

  /** BitwiseOrExpression ( (+ | - | ||) BitwiseOrExpression )* */
  private IExpression parseAdditive() throws ParseException {
    IExpression expression = this.parseBitwiseOr();
    while (hasNext()) {
      if (next(Id.PLUS)) {
        expression = new Call(Operators.ADD, expression, this.parseBitwiseOr());
      } else if (next(Id.MINUS)) {
        expression = new Call(Operators.SUBTRACT, expression, this.parseBitwiseOr());
      } else if (next(Id.CONCAT)) {
        expression = new Call(Operators.CONCAT, expression, this.parseBitwiseOr());
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
    if (token==null) {
      throw new ParseException(Error.UNEXPECTED_END_OF_EXPRESSION.message(), this.getPosition());
    }

    try {
      DateTimeFormat format = DateTimeFormat.of("YYYY-MM-DD");
      ZonedDateTime datetime = format.parse(token.text());
      return Literal.of(datetime);
    } catch (Exception e) {
      throw new ParseException(Error.INVALID_DATE.message(token.text()), token.start());
    }
  }

  /** Parses a time literal. */
  private Literal parseLiteralTime(Token token) throws ParseException {
    if (token==null) {
      throw new ParseException(Error.UNEXPECTED_END_OF_EXPRESSION.message(), this.getPosition());
    }

    try {
      DateTimeFormat format = DateTimeFormat.of("HH12:MI:SS AM|HH24:MI:SS|HH12:MI AM|HH24:MI");
      ZonedDateTime datetime = format.parse(token.text());
      return Literal.of(datetime);
    } catch (Exception e) {
      throw new ParseException(Error.INVALID_TIME.message(token.text()), token.start());
    }
  }

  /**
   * Parses a timestamp literal with ISO Formats.
   * 
   * The parsing is strict and requires months to be less than 12, days to be less than 31, etc.
   */
  private Literal parseLiteralTimestamp(Token token) throws ParseException {
    if (token==null) {
      throw new ParseException(Error.UNEXPECTED_END_OF_EXPRESSION.message(), this.getPosition());
    }
    
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
          throw new ParseException(Error.INVALID_OPERATOR.message("AT TIME ZONE"), this.getPosition());
        }
        token = next();
        ZoneId zoneId = ZoneId.of(token.text());           
        datetime =  datetime.withZoneSameLocal(zoneId);
      }
      return Literal.of(datetime);
    } catch (ZoneRulesException e) {
      throw new ParseException(Error.UNKNOWN_TIMEZONE.message(token.text()), token.start());
    } catch (Exception e) {
      throw new ParseException(Error.INVALID_TIMESTAMP.message(token.text()), token.start());
    }
  }

  /**
   * Parses a list of expressions separated by commas.
   * (expression [,expression...] )
   */
  private Tuple parseTuple() throws ParseException {

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
          return new Tuple(list);
        }

        break;
      } while (true);
    }
    throw new ParseException(Error.MISSING_RIGHT_PARENTHESIS.message(),
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
        throw new ParseException(Error.INVALID_OPERATOR.message(Id.CASE), this.getPosition());
      }
      thenList.add(this.parseLogicalOr());
    }

    if (next(Id.ELSE)) {
      elseExpression = this.parseLogicalOr();
    }

    if (!next(Id.END)) {
      throw new ParseException(Error.INVALID_OPERATOR.message(Id.CASE), this.getPosition());
    }

    return new Call(Operators.CASE, valueExpression, new Tuple(whenList),
        new Tuple(thenList), elseExpression);
  }

  /**
   * Cast operator [TRY_]CAST(value AS type [FORMAT pattern])
   *
   */
  private IExpression parseCastFunction(Token token) throws ParseException {
    Operator operator =
        ("CAST".equals(token.text())) ? Operators.CAST : Operators.TRY_CAST;

    List<IExpression> operands = new ArrayList<>();

    if (is(Id.LPARENTHESIS))
      token = next();
    else {
      throw new ParseException(Error.MISSING_LEFT_PARENTHESIS.message(), token.start());
    }

    operands.add(this.parseLogicalOr());

    if (!next(Id.AS)) {
      throw new ParseException(Error.INVALID_OPERATOR.message(Id.CASE), token.start());
    }

    operands.add(this.parseLiteralDataType(next()));

    if (is(Id.FORMAT)) {
      next();
      token = next();
      if (token.is(Token.Id.LITERAL_STRING))
        operands.add(this.parseLiteralString(token));
      else
        throw new ParseException(Error.INVALID_OPERATOR.message(Id.CASE), token.start());
    }

    if (is(Id.RPARENTHESIS)) {
      next();
    } else {
      throw new ParseException(Error.MISSING_RIGHT_PARENTHESIS.message(), token.start());
    }

    return new Call(operator, operands);
  }

  /**
   * Cast operator ::
   *
   */
  private IExpression parseCastOperator() throws ParseException {
    IExpression expression = this.parseTerm();
    if (next(Id.CAST)) {
      Literal type = parseLiteralDataType(next());
      return new Call(Operators.CAST, expression, type);
    }
    return expression;
  }

  /** POSITION(<expression> IN <expression>) */
  private IExpression parsePositionFunction(Token token) throws ParseException {

    List<IExpression> operands = new ArrayList<>();

    if (is(Id.LPARENTHESIS))
      token = next();
    else {
      throw new ParseException(Error.MISSING_LEFT_PARENTHESIS.message(), token.start());
    }

    operands.add(this.parseAdditive());

    if (!next(Id.IN)) {
      throw new ParseException(Error.INVALID_OPERATOR.message(Id.POSITION), this.getPosition());
    }

    operands.add(this.parseAdditive());

    if (is(Id.RPARENTHESIS)) {
      next();
    } else {
      throw new ParseException(Error.MISSING_LEFT_PARENTHESIS.message(), token.start());
    }

    return new Call(Operators.POSITION, operands);
  }

  /** <expression> AT TIMEZONE <term> ) */
  private IExpression parseAtTimeZone() throws ParseException {
    IExpression operand = this.parseAdditive();

    if (next(Id.AT)) {
      if (next(Id.TIME) && next(Id.ZONE)) {
        return new Call(Operators.AT_TIME_ZONE, operand, this.parseTerm());
      }
      throw new ParseException(Error.INVALID_OPERATOR.message(Id.AT), this.getPosition());
    }

    return operand;
  }

  /** EXTRACT(<part> FROM <expression>) */
  private IExpression parseExtractFunction(Token token) throws ParseException {

    if (is(Id.LPARENTHESIS))
      token = next();
    else {
      throw new ParseException(Error.MISSING_LEFT_PARENTHESIS.message(), token.start());
    }

    List<IExpression> operands = new ArrayList<>();

    operands.add(this.parseTerm());

    if (!next(Id.FROM)) {
      throw new ParseException(Error.INVALID_OPERATOR.message(Id.EXTRACT), this.getPosition());
    }

    operands.add(this.parseAdditive());

    if (is(Id.RPARENTHESIS)) {
      next();
    } else {
      throw new ParseException(Error.MISSING_RIGHT_PARENTHESIS.message(), token.start());
    }

    return new Call(Operators.EXTRACT, operands);
  }

  /** Function */
  private IExpression parseFunction(Token token) throws ParseException {

    Function function = FunctionRegistry.getFunction(token.text());
    List<IExpression> operands = new ArrayList<>();

    if (is(Id.LPARENTHESIS))
      token = next();
    else {
      throw new ParseException(Error.MISSING_LEFT_PARENTHESIS.message(), token.start());
    }

    // No param function
    if (is(Id.RPARENTHESIS)) {
      next();
      return new Call(function, operands);
    }

    operands.add(this.parseAdditive());

    while (is(Id.COMMA)) {
      token = next();
      operands.add(this.parseAdditive());
    }

    if (is(Id.RPARENTHESIS)) {
      next();
    } else {
      throw new ParseException(Error.MISSING_RIGHT_PARENTHESIS.message(), token.start());
    }

    return new Call(function, operands);
  }

  private Literal parseLiteralDatePart(Token token) throws ParseException {
    if (token==null) {
      throw new ParseException(Error.UNEXPECTED_END_OF_EXPRESSION.message(), this.getPosition());
    }

    try {
      DatePart part = DatePart.of(token.text());
      return Literal.of(part);
    } catch (RuntimeException e) {
      throw new ParseException(Error.INVALID_DATEPART.message(token.text()), token.start());
    }
  }

  private Literal parseLiteralDataType(Token token) throws ParseException {
    if (token==null) {
      throw new ParseException(Error.UNEXPECTED_END_OF_EXPRESSION.message(), this.getPosition());
    }
    
    try {
      DataType datatype = DataType.of(token.text());
      return Literal.of(datatype);
    } catch (RuntimeException e) {
      throw new ParseException(Error.INVALID_DATATYPE.message(token.text()), token.start());
    }
  }

  /** Parses an expression string to return the individual tokens. */
  protected Token tokenize() throws ParseException {

    while (position < source.length()) {
      char c = source.charAt(position);

      switch (c) {
        case ',':
          return new Token(Id.COMMA, position++);

        case '(':
          return new Token(Id.LPARENTHESIS, position++);

        case ')':
          return new Token(Id.RPARENTHESIS, position++);

        // Single-quoted literal text.
        case '\'': {
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
            throw new ParseException(Error.MISSING_END_SINGLE_QUOTED_STRING.message(), start);
          }

          return new Token(Id.LITERAL_STRING, start, position, text.toString());
        }

        case '=':
          return new Token(Id.EQUAL, position++);

        case '+':
          return new Token(Id.PLUS, position++);

        case '-': {
          // Single line comment --
          if (position+1 < source.length()) {
            if (source.charAt(position+1) == '-') {
              int start = position;
              position++;
              while (position < source.length()) {
                c = source.charAt(position);
                if (c == '\r' || c == '\n')
                  break;
                position++;
              }
              return new Token(Id.COMMENT, start, position, source.substring(start, position));
            }
          }
          
          // Minus sign
          return new Token(Id.MINUS, position++);
        }
        
        case '*':
          return new Token(Id.MULTIPLY, position++);

        case '%':
          return new Token(Id.MODULUS, position++);

        case '<': {
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
        case '>': {
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

        // parse not equal symbol
        case '!': {
          int start = position++;
          if (position < source.length()) {
            c = source.charAt(position);
            if (c == '=') {
              position++;
              return new Token(Id.NOT_EQUAL, start);
            }
          }
          throw new ParseException(Error.UNEXPECTED_CHARACTER.message('!'), start);
        }


        // cast operator
        case ':': {
          int start = position++;
          if (position < source.length()) {
            c = source.charAt(position);
            if (c == ':') {
              position++;
              return new Token(Id.CAST, start);
            }
          }
          throw new ParseException(Error.UNEXPECTED_CHARACTER.message(':'), start);
        }

        // possible start of '/*' or '//' comment
        case '/': {
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
                  } else
                    position++;
                } else {
                  throw new ParseException(Error.MISSING_END_BLOCK_COMMENT.message(), start);
                }
              }

              return new Token(Id.COMMENT, start, position, source.substring(start, position));
            }
            // Line comment
            if (c1 == '/') {
              position++;

              while (position < source.length()) {
                c = source.charAt(position);
                if (c == '\r' || c == '\n')
                  break;
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
        case '|': {
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
        case '"': {
          int start = position++;
          while (position < source.length()) {
            c = source.charAt(position++);
            if (c == '"') {
              String value = source.substring(start + 1, position - 1).toUpperCase();
              return new Token(Id.IDENTIFIER, start, position, value);
            }
          }
          throw new ParseException(Error.UNEXPECTED_CHARACTER.message(), start);
        }

        case '.': // Number without zero .1
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9': {
          int start = position++;

          // Hexadecimal number 0xABCDEF
          if (c == '0' && position < source.length() && (source.charAt(position) == 'x')) {
            do {
              position++;
            } while (position < source.length() && Characters.isHexDigit(source.charAt(position)));

            return new Token(Id.LITERAL_BINARY_HEX, start, position,
                source.substring(start + 2, position));
          }

          // Binary number 0b01101011
          if (c == '0' && position < source.length() && (source.charAt(position) == 'b')) {
            do {
              position++;
            } while (position < source.length()
                && (source.charAt(position) == '0' || source.charAt(position) == '1'));

            return new Token(Id.LITERAL_BINARY_BIT, start, position,
                source.substring(start + 2, position));
          }

          // Integer part
          while (position < source.length() && Characters.isDigit(source.charAt(position))) {
            position++;
          }

          // Use dot for decimal separator
          if (position < source.length() && source.charAt(position) == '.') {
            position++;
          }

          // Decimal part
          while (position < source.length() && Characters.isDigit(source.charAt(position))) {
            position++;
          }

          // Exponentiation part
          if (position < source.length() && Characters.isExponent(source.charAt(position))) {
            position++;

            if (position < source.length()
                && (source.charAt(position) == '+' || source.charAt(position) == '-')) {
              position++;
            }
            while (position < source.length() && Characters.isDigit(source.charAt(position))) {
              position++;
            }
          }

          return new Token(Id.LITERAL_NUMBER, start, position, source.substring(start, position));
        }

        default:
          if (Characters.isSpace(c) || c=='\n' || c=='\r' ) {
            ++position;
            continue;
          }

          // Probably a letter or digit. Start an identifier.
          // Other characters, e.g. *, ! are also included
          // in identifiers.
          int start = position++;
          while (position < source.length()) {
            c = source.charAt(position);
            if (Characters.isSpace(c) || "()/*%,^&><=~+-.!|$:[]\n\r".indexOf(c) >= 0)
              break;

            position++;
          }

          
          String identifier = source.substring(start, position);
          String name = identifier.toUpperCase();

          if (c == '(' ) {
            // Special operator with name
            if ("CAST".equals(name) || "TRY_CAST".equals(name) || "EXTRACT".equals(name) || "POSITION".equals(name) ) {
              return new Token(Id.valueOf(name), start, position, name);
            }
            if ( FunctionRegistry.isFunction(name)) {          
              return new Token(Id.FUNCTION, start, position, name);
            }
          }

          // Reserved words: AS, AND, LIKE, NOT, TRUE, FALSE, OR
          if (isReservedWord(name)) {
            return new Token(Id.valueOf(name), start, position, name);
          }

          if (DataType.exist(name)) {
            return new Token(Id.LITERAL_DATATYPE, start, position, name);
          }

          if (DatePart.exist(name)) {
            return new Token(Id.LITERAL_DATEPART, start, position, name);
          }

          return new Token(Id.IDENTIFIER, start, position, identifier);
      }
    }
    return null;
  }
}
