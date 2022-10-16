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
import org.apache.hop.expression.optimizer.ArithmeticOptimizer;
import org.apache.hop.expression.optimizer.BooleanOptimizer;
import org.apache.hop.expression.optimizer.CombineConcatOptimizer;
import org.apache.hop.expression.optimizer.DeterministicOptimizer;
import org.apache.hop.expression.optimizer.ExpressionCompiler;
import org.apache.hop.expression.optimizer.ExtractOptimizer;
import org.apache.hop.expression.optimizer.IdentifierOptimizer;
import org.apache.hop.expression.optimizer.InOptimizer;
import org.apache.hop.expression.optimizer.LikeOptimizer;
import org.apache.hop.expression.optimizer.ReturnTypeOptimizer;
import org.apache.hop.expression.optimizer.SymmetricalOptimizer;
import org.apache.hop.expression.type.DataTypeName;
import org.apache.hop.expression.type.IOperandCountRange;
import org.apache.hop.expression.type.IOperandTypeChecker;
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

public class ExpressionBuilder {

  private static final Set<ExpressionCompiler> OPTIMIZERS =
      Set.of(new SymmetricalOptimizer(), new ArithmeticOptimizer(), new LikeOptimizer(),
          new InOptimizer(), new ExtractOptimizer(), new CombineConcatOptimizer(),
          new BooleanOptimizer(), new DeterministicOptimizer(), new IdentifierOptimizer());

  private static final ExpressionCompiler RETURN_TYPE = new ReturnTypeOptimizer();

  private static final Set<String> RESERVED_WORDS = Set.of("AND", "AS", "ASYMMETRIC", "AT",
      "BETWEEN", "CASE", "DATE", "DISTINCT", "ELSE", "END", "ESCAPE", "FALSE", "FORMAT", "FROM", "IGNORE",
      "ILIKE", "IN", "IS", "KEY", "LIKE", "NOT", "NULL", "NULLS", "OR", "RESPECT", "RLIKE", "SYMMETRIC", "THEN", "TIME",
      "TIMESTAMP", "TRUE", "VALUE", "WHEN", "XOR", "ZONE");

  private static final Set<String> FUNCTION_WITH_CUSTOM_SYNTAX = Set.of("CAST", "COUNT", "EXTRACT", "POSITION", "LISTAGG", "FIRST_VALUE", "LAST_VALUE", "JSON_OBJECT");
  
  
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

  public static IExpression compile(IExpressionContext context, final String source)
      throws ExpressionException {
    ExpressionBuilder builder = new ExpressionBuilder(source);
    try {
      return builder.compile(context, builder.parse());
    } catch (ParseException e) {
      throw createException(source, e.getErrorOffset(), e);
    } catch (IllegalArgumentException e) {
      throw createException(source, builder.getPosition(), e);
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
    return new ExpressionException(ExpressionError.SYNTAX_ERROR, line, column, e.getMessage());
  }

  protected ExpressionBuilder(final String source) {
    super();
    this.source = source;
  }

  protected int getPosition() {

    if (index > 0 && index < tokens.size())
      return tokens.get(index).start();

    return source.length();
  }

  protected boolean hasNext() {
    return index < tokens.size();
  }

  protected Token next() throws ParseException {
    if (!hasNext()) {
      throw new ParseException(ExpressionError.UNEXPECTED_END_OF_EXPRESSION.message(),
          this.getPosition());
    }

    return tokens.get(index++);
  }

  protected boolean is(final Id id) {
    if (hasNext()) {
      return tokens.get(index).is(id);
    }
    return false;
  }

  protected boolean isAndNext(final Id id) {
    if (hasNext() && tokens.get(index).is(id)) {
      index++;
      return true;
    }
    return false;
  }

  protected boolean isNotAndNext(final Id id) {
    return !this.isAndNext(id);
  }

  /** Parse the expression */
  private IExpression parse() throws ParseException {

    if (source == null)
      return Literal.NULL;

    // Tokenize
    for (Token token = tokenize(); token != null; token = tokenize()) {

      // Ignore comment
      if (token.is(Id.COMMENT))
        continue;

      tokens.add(token);
    }

    // Empty source return NULL
    if (tokens.isEmpty())
      return Literal.NULL;

    IExpression expression = this.parseLogicalOr();

    if (hasNext()) {
      Token token = next();
      throw new ParseException(ExpressionError.UNEXPECTED_CHARACTER.message(token.text()),
          token.start());
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
    while (isAndNext(Id.OR)) {
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
    while (isAndNext(Id.AND)) {
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

    if (isAndNext(Id.NOT)) {
      return new Call(Operators.BOOLNOT, parseLogicalNot());
    }

    return this.parseIs();
  }

  /**
   * Parse IS expression
   *
   * <p>
   * RelationalExpression [NOT] TRUE|FALSE|NULL
   */
  private IExpression parseIs() throws ParseException {
    IExpression expression = this.parseComparaison();
    if (isAndNext(Id.IS)) {
      boolean not = false;
      if (isAndNext(Id.NOT)) {
        not = true;
      }

      Token token = next();
      Operator operator;
      switch (token.id()) {
        case TRUE:
          operator = (not) ? Operators.IS_FALSE : Operators.IS_TRUE;
          break;
        case FALSE:
          operator = (not) ? Operators.IS_TRUE : Operators.IS_FALSE;
          break;
        case NULL:
          operator = (not) ? Operators.IS_NOT_NULL : Operators.IS_NULL;
          break;
        default:
          throw new ParseException(ExpressionError.INVALID_BOOLEAN.message(token.text()),
              token.start());
      }
      expression = new Call(operator, expression);
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
    if (isAndNext(Id.NOT)) {
      not = true;
    }

    if (isAndNext(Id.LIKE)) {
      IExpression pattern = this.parseAdditive();

      if (isAndNext(Id.ESCAPE)) {
        IExpression escape = this.parsePrimary();
        expression = new Call(Operators.LIKE, expression, pattern, escape);
      } else {
        expression = new Call(Operators.LIKE, expression, pattern);
      }
    } else if (isAndNext(Id.ILIKE)) {
      IExpression pattern = this.parseAdditive();

      if (isAndNext(Id.ESCAPE)) {
        IExpression escape = this.parseAdditive();
        expression = new Call(Operators.ILIKE, expression, pattern, escape);
      } else {
        expression = new Call(Operators.ILIKE, expression, pattern);
      }
    } else if (isAndNext(Id.IN)) {
      expression = new Call(Operators.IN, expression, this.parseTuple());
    } else if (isAndNext(Id.BETWEEN)) {
      Operator operator = Operators.BETWEEN_ASYMMETRIC;
      if (isAndNext(Id.ASYMMETRIC)) {
        // Ignore
      } else if (isAndNext(Id.SYMMETRIC)) {
        operator = Operators.BETWEEN_SYMMETRIC;
      }

      IExpression start = this.parseAdditive();
      if (isNotAndNext(Id.AND)) {
        throw new ParseException(ExpressionError.INVALID_OPERATOR.message(Id.BETWEEN),
            this.getPosition());
      }
      IExpression end = this.parseAdditive();

      expression = new Call(operator, expression, start, end);
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
      if (isAndNext(Id.MULTIPLY)) {
        expression = new Call(Operators.MULTIPLY, expression, this.parseBitwiseNot());
      } else if (isAndNext(Id.DIVIDE)) {
        expression = new Call(Operators.DIVIDE, expression, this.parseBitwiseNot());
      } else if (isAndNext(Id.MODULUS)) {
        expression = new Call(Operators.MODULUS, expression, this.parseBitwiseNot());
      } else
        break;
    }

    return expression;
  }

  /** ( UnaryExpression)* */
  private IExpression parseBitwiseNot() throws ParseException {
    if (isAndNext(Id.BITWISE_NOT)) {
      return new Call(Operators.BITNOT, this.parseUnary());
    }
    return this.parseUnary();
  }

  /** UnaryExpression ( & UnaryExpression)* */
  private IExpression parseBitwiseAnd() throws ParseException {
    IExpression expression = this.parseFactor();
    if (isAndNext(Id.BITWISE_AND)) {
      return new Call(Operators.BITAND, expression, this.parseFactor());
    }
    return expression;
  }

  /** BitwiseXorExpression ( | BitwiseXorExpression)* */
  private IExpression parseBitwiseOr() throws ParseException {
    IExpression expression = this.parseBitwiseXor();
    if (isAndNext(Id.BITWISE_OR)) {
      return new Call(Operators.BITOR, expression, this.parseBitwiseXor());
    }
    return expression;
  }

  /** BitwiseAndExpression ( ^ BitwiseAndExpression)* */
  private IExpression parseBitwiseXor() throws ParseException {
    IExpression expression = this.parseBitwiseAnd();
    if (isAndNext(Id.BITWISE_XOR)) {
      return new Call(Operators.BITXOR, expression, this.parseBitwiseAnd());
    }
    return expression;
  }

  /** (('+' | '-') PrimaryExpression ) */
  private IExpression parseUnary() throws ParseException {

    if (isAndNext(Id.MINUS)) {
      return new Call(Operators.NEGATIVE, this.parsePrimary());
    }
    if (isAndNext(Id.PLUS)) {
      // Ignore
    }

    return this.parsePrimary();
  }

  /** Literal String */
  private Literal parseLiteralString(Token token) {
    return Literal.of(token.text());
  }

  /**
   * Cast operator <term>::<datatype> | <term> AT TIMEZONE <timezone>)
   *
   */
  private IExpression parsePrimary() throws ParseException {
    IExpression expression = this.parseTerm();
    if (isAndNext(Id.CAST)) {
      Literal type = parseLiteralDataType(next());
      return new Call(Operators.CAST, expression, type);
    }
    if (isAndNext(Id.AT)) {
      if (isAndNext(Id.TIME) && isAndNext(Id.ZONE)) {
        return new Call(Operators.AT_TIME_ZONE, expression, this.parseTerm());
      }
      throw new ParseException(ExpressionError.INVALID_OPERATOR.message(Id.AT), this.getPosition());
    }
    return expression;
  }

  /** Term = Literal | Identifier | Function | '(' Expression ')' */
  private IExpression parseTerm() throws ParseException {
    Token token = next();
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
        if (token == null)
          break;
        return parseLiteralDate(token);
      case TIME:
        token = next();
        if (token == null)
          break;
        return parseLiteralTime(token);
      case TIMESTAMP:
        token = next();
        if (token == null)
          break;
        return parseLiteralTimestamp(token);
      case CASE:
        return parseCase();
      case CAST:
      case TRY_CAST:
        return parseFunctionCast(token);
      case COUNT:
        return parseFunctionCount(token);
      case EXTRACT:
        return parseFunctionExtract(token);
      case POSITION:
        return parseFunctionPosition(token);
      case JSON_OBJECT:
        return parseFunctionJsonObject(token);
      case LISTAGG:
        return parseFunctionListAgg(token);
      case FIRST_VALUE:
      case LAST_VALUE:
        return parseFunctionFirstLastValue(token);
      case FUNCTION:
        return parseFunction(token);
      case LPARENTHESIS:
        IExpression expression = this.parseLogicalOr();

        token = next();
        if (token.is(Id.RPARENTHESIS)) {
          return expression;
        }
        throw new ParseException(ExpressionError.UNBALANCE_PARENTHESIS.message(),
            this.getPosition());
      default:
        // Syntax error
    }
    throw new ParseException(ExpressionError.UNEXPECTED_END_OF_EXPRESSION.message(),
        this.getPosition());
  }

  /** RelationalExpression ( Operator RelationalExpression ) */
  private IExpression parseComparaison() throws ParseException {
    IExpression expression = this.parseRelational();

    if (isAndNext(Id.EQUAL)) {
      return new Call(Operators.EQUAL, expression, this.parseRelational());
    }
    if (isAndNext(Id.NOT_EQUAL)) {
      return new Call(Operators.NOT_EQUAL, expression, this.parseRelational());
    }
    if (isAndNext(Id.GT)) {
      return new Call(Operators.GREATER_THAN, expression, this.parseRelational());
    }
    if (isAndNext(Id.GTE)) {
      return new Call(Operators.GREATER_THAN_OR_EQUAL, expression, this.parseRelational());
    }
    if (isAndNext(Id.LT)) {
      return new Call(Operators.LESS_THAN, expression, this.parseRelational());
    }
    if (isAndNext(Id.LTE)) {
      return new Call(Operators.LESS_THAN_OR_EQUAL, expression, this.parseRelational());
    }

    return expression;
  }

  /** BitwiseOrExpression ( (+ | - | ||) BitwiseOrExpression )* */
  private IExpression parseAdditive() throws ParseException {
    IExpression expression = this.parseBitwiseOr();
    while (hasNext()) {
      if (isAndNext(Id.PLUS)) {
        expression = new Call(Operators.ADD, expression, this.parseBitwiseOr());
      } else if (isAndNext(Id.MINUS)) {
        expression = new Call(Operators.SUBTRACT, expression, this.parseBitwiseOr());
      } else if (isAndNext(Id.CONCAT)) {
        expression = new Call(Operators.CONCAT, expression, this.parseBitwiseOr());
      } else
        break;
    }

    return expression;
  }

  private Literal parseLiteralNumber(Token token) throws ParseException {
    BigDecimal number = NumberFormat.of("TM").parse(token.text());

    // if ( number.remainder(BigDecimal.ONE).compareTo(BigDecimal.ZERO) == 0 &&
    // number.precision()<17 ) {
    // return Literal.of(number.longValue());
    // }

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
      throw new ParseException(ExpressionError.INVALID_DATE.message(token.text()), token.start());
    }
  }

  /** Parses a time literal. */
  private Literal parseLiteralTime(Token token) throws ParseException {
    try {
      DateTimeFormat format = DateTimeFormat.of("HH12:MI:SS AM|HH24:MI:SS|HH12:MI AM|HH24:MI");
      ZonedDateTime datetime = format.parse(token.text());
      return Literal.of(datetime);
    } catch (Exception e) {
      throw new ParseException(ExpressionError.INVALID_TIME.message(token.text()), token.start());
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
          if (str.indexOf('.', 10) > 0)
            pattern = "YYYY-MM-DD HH24:MI:SS.FF6";
          else
            pattern = "YYYY-MM-DD HH24:MI:SSTZH:TZM";
          break;
        case 25: // 2021-01-01 15:28:59+02:00
          pattern = "YYYY-MM-DD HH24:MI:SSTZH:TZM";
          break;
        case 24: // 2021-01-01 15:28:59+0200
                 // 2021-01-01 5:28:59+02:00
          if (str.indexOf(':', 20) > 0)
            pattern = "YYYY-MM-DD HH24:MI:SSTZH:TZM";
          else
            pattern = "YYYY-MM-DD HH24:MI:SSTZHTZM";
          break;
        case 23:
          if (str.indexOf('.', 10) > 0)
            pattern = "YYYY-MM-DD HH24:MI:SS.FF3";
          else
            pattern = "YYYY-MM-DD HH24:MI TZH:TZM";
          break;
        case 21: // 2021-01-01 5:28+02:00
        case 22: // 2021-01-01 15:28+02:00 or 2021-01-01 5:28 +02:00
          pattern = "YYYY-MM-DD HH24:MITZH:TZM";
          break;
        case 18:
        case 19: // 2021-04-28 20:57:48
          if (str.indexOf('+') > 0 || str.indexOf('-', 15) > 0)
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
      if (isAndNext(Id.AT)) {
        if (isNotAndNext(Id.TIME) || isNotAndNext(Id.ZONE)) {
          throw new ParseException(ExpressionError.INVALID_OPERATOR.message("AT TIME ZONE"),
              this.getPosition());
        }
        token = next();
        ZoneId zoneId = ZoneId.of(token.text());
        datetime = datetime.withZoneSameLocal(zoneId);
      }
      return Literal.of(datetime);
    } catch (ZoneRulesException e) {
      throw new ParseException(ExpressionError.UNKNOWN_TIMEZONE.message(token.text()),
          token.start());
    } catch (Exception e) {
      throw new ParseException(ExpressionError.INVALID_TIMESTAMP.message(token.text()),
          token.start());
    }
  }

  /**
   * Parses a list of expressions separated by commas.
   * (expression [,expression...] )
   */
  private Tuple parseTuple() throws ParseException {

    List<IExpression> list = new ArrayList<>();

    if (isAndNext(Id.LPARENTHESIS)) {

      do {
        IExpression value = parsePrimary();
        
        // Only literal in values list
        if ( value.is(Kind.LITERAL) ) {
          list.add(value);
        } else {
          throw new ParseException(ExpressionError.INVALID_VALUES.message(), this.getPosition());
        }

        if (isAndNext(Id.COMMA)) {
          continue;
        }

        if (isAndNext(Id.RPARENTHESIS)) {
          return new Tuple(list);
        }

        break;
      } while (true);
    }
    throw new ParseException(ExpressionError.MISSING_RIGHT_PARENTHESIS.message(),
        this.getPosition());
  }

  /** Case When Then Else End ) */
  private IExpression parseCase() throws ParseException {
    IExpression valueExpression = null;
    IExpression elseExpression = Literal.NULL;
    List<IExpression> whenList = new ArrayList<>();
    List<IExpression> thenList = new ArrayList<>();

    // Form Switch value
    if (!is(Id.WHEN)) {
      valueExpression = this.parseLogicalOr();
    }

    // Form multi boolean condition
    while (isAndNext(Id.WHEN)) {
      whenList.add(this.parseLogicalOr());
      if (isNotAndNext(Id.THEN)) {
        throw new ParseException(ExpressionError.INVALID_OPERATOR.message(Id.CASE),
            this.getPosition());
      }
      thenList.add(this.parseLogicalOr());
    }

    if (isAndNext(Id.ELSE)) {
      elseExpression = this.parseLogicalOr();
    }

    if (isNotAndNext(Id.END)) {
      throw new ParseException(ExpressionError.INVALID_OPERATOR.message(Id.CASE),
          this.getPosition());
    }

    return new Call(Operators.CASE, valueExpression, new Tuple(whenList), new Tuple(thenList),
        elseExpression);
  }

  /**
   * Cast operator CAST(value AS type [FORMAT pattern])
   *
   */
  private IExpression parseFunctionCast(Token token) throws ParseException {

    List<IExpression> operands = new ArrayList<>();

    if (isNotAndNext(Id.LPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_LEFT_PARENTHESIS.message(), token.end());
    }

    operands.add(this.parseLogicalOr());

    if (isNotAndNext(Id.AS)) {
      throw new ParseException(ExpressionError.INVALID_OPERATOR.message(Id.CASE), token.start());
    }

    operands.add(this.parseLiteralDataType(next()));

    if (isAndNext(Id.FORMAT)) {
      token = next();
      if (token.is(Id.LITERAL_STRING))
        operands.add(this.parseLiteralString(token));
      else
        throw new ParseException(ExpressionError.INVALID_OPERATOR.message(Id.CASE), token.start());
    }

    if (isNotAndNext(Id.RPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_RIGHT_PARENTHESIS.message(), token.end());
    }

    return new Call(Operators.CAST, operands);
  }

  /** POSITION(<expression> IN <expression>) */
  private IExpression parseFunctionPosition(Token token) throws ParseException {

    List<IExpression> operands = new ArrayList<>();

    if (isNotAndNext(Id.LPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_LEFT_PARENTHESIS.message(), token.end());
    }

    operands.add(this.parseAdditive());

    if (isNotAndNext(Id.IN)) {
      throw new ParseException(ExpressionError.INVALID_OPERATOR.message(Id.POSITION),
          this.getPosition());
    }

    operands.add(this.parseAdditive());

    if (isNotAndNext(Id.RPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_LEFT_PARENTHESIS.message(), token.end());
    }

    return new Call(Operators.POSITION, operands);
  }
  
  /** 
   * FIRST_VALUE(<expression> [ IGNORE NULLS | RESPECT NULLS ] ) 
   * LAST_VALUE(<expression> [ IGNORE NULLS | RESPECT NULLS ] )
   */
  private IExpression parseFunctionFirstLastValue(Token token) throws ParseException {

    Function function = FunctionRegistry.getFunction(token.text());  

    if (isNotAndNext(Id.LPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_LEFT_PARENTHESIS.message(), token.end());
    }
    List<IExpression> operands = new ArrayList<>();
    operands.add(this.parseLogicalOr());


    if (isAndNext(Id.IGNORE)) {
      if (isAndNext(Id.NULLS)) {
        operands.add(Literal.TRUE);
      }
      else 
        throw new ParseException(ExpressionError.INVALID_OPERATOR.message(function.getName()),
            this.getPosition());
    }
    else if (isAndNext(Id.RESPECT)) {
      
      // Default respect null, no operand
      
      if (isNotAndNext(Id.NULLS)) {
        throw new ParseException(ExpressionError.INVALID_OPERATOR.message(function.getName()),
            this.getPosition());
      }
    }
    
    if (isNotAndNext(Id.RPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_LEFT_PARENTHESIS.message(), token.end());
    }

    return new Call(function, operands);
  }

  /** EXTRACT(<part> FROM <expression>) */
  private IExpression parseFunctionExtract(Token token) throws ParseException {

    if (isNotAndNext(Id.LPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_LEFT_PARENTHESIS.message(), token.end());
    }

    List<IExpression> operands = new ArrayList<>();

    operands.add(this.parseLiteralDatePart(next()));

    if (isNotAndNext(Id.FROM)) {
      throw new ParseException(ExpressionError.INVALID_OPERATOR.message(Id.EXTRACT),
          this.getPosition());
    }

    operands.add(this.parseAdditive());

    if (isNotAndNext(Id.RPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_RIGHT_PARENTHESIS.message(), token.end());
    }

    return new Call(Operators.EXTRACT, operands);
  }

  /**
   * COUNT(*) | COUNT([DISTINCT] <expression>)
   */
  private IExpression parseFunctionCount(Token token) throws ParseException {

    AggregateFunction aggregator = Operators.COUNT_VALUE;

    if (isNotAndNext(Id.LPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_LEFT_PARENTHESIS.message(), token.start());
    }

    List<IExpression> operands = new ArrayList<>();

    // COUNT(*) no operand
    if (isAndNext(Id.MULTIPLY)) {      
      aggregator = Operators.COUNT_ALL;
    } else if (isAndNext(Id.DISTINCT)) {
      operands.add(this.parseTerm());
      aggregator = Operators.COUNT_DISTINCT;
    } else {
      operands.add(this.parseTerm());
    }

    if (isNotAndNext(Id.RPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_RIGHT_PARENTHESIS.message(), token.start());
    }

    return new Call(aggregator, operands);
  }

  /**
   * LISTAGG([DISTINCT] <expression> [, delimiter] )
   */
  private IExpression parseFunctionListAgg(Token token) throws ParseException {

    AggregateFunction aggregator = Operators.LISTAGG_ALL;

    if (isNotAndNext(Id.LPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_LEFT_PARENTHESIS.message(), token.start());
    }

    List<IExpression> operands = new ArrayList<>();

    if (isAndNext(Id.DISTINCT)) {
      operands.add(this.parseTerm());
      aggregator = Operators.LISTAGG_DISTINCT;
    } else {
      operands.add(this.parseTerm());
    }

    // Optional delimiter
    if (isAndNext(Id.COMMA)) {
      operands.add(this.parseTerm());
    }
    
    if (isNotAndNext(Id.RPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_RIGHT_PARENTHESIS.message(), token.start());
    }

    return new Call(aggregator, operands);
  }
  
  /** JSON_OBJECT([KEY] <key> VALUE <expression> [, [KEY] <key> VALUE <expression>]...) */
  private IExpression parseFunctionJsonObject(Token token) throws ParseException {

    if (isNotAndNext(Id.LPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_LEFT_PARENTHESIS.message(), token.start());
    }

    List<IExpression> operands = new ArrayList<>();
    boolean comma;
    do {

      if (isAndNext(Id.KEY)) {
        // KEY is optional
      }
      token = next();
      operands.add(this.parseLiteralString(token));

      if (isAndNext(Id.VALUE)) {
        operands.add(this.parseTerm());
      } else {
        throw new ParseException(ExpressionError.INVALID_OPERATOR.message(Id.JSON_OBJECT),
            token.start());
      }

      comma = false;
      if (is(Id.COMMA)) {
        comma = true;
        token = next();
      }
    } while (comma);

    if (isNotAndNext(Id.RPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_RIGHT_PARENTHESIS.message(), token.start());
    }

    return new Call(Operators.JSON_OBJECT, operands);
  }

  /** Function */
  private IExpression parseFunction(Token token) throws ParseException {

    Function function = FunctionRegistry.getFunction(token.text());
    List<IExpression> operands = new ArrayList<>();

    if (isNotAndNext(Id.LPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_LEFT_PARENTHESIS.message(), token.start());
    }

    // No param function
    if (isAndNext(Id.RPARENTHESIS)) {
      return new Call(function, operands);
    }

    operands.add(this.parseLogicalOr());

    while (isAndNext(Id.COMMA)) {
      operands.add(this.parseLogicalOr());
    }

    if (isNotAndNext(Id.RPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_RIGHT_PARENTHESIS.message(), token.start());
    }

    return new Call(function, operands);
  }

  private Literal parseLiteralDatePart(Token token) throws ParseException {
    try {
      DatePart part = DatePart.of(token.text());
      return Literal.of(part);
    } catch (RuntimeException e) {
      throw new ParseException(ExpressionError.INVALID_DATEPART.message(token.text()),
          token.start());
    }
  }

  private Literal parseLiteralDataType(Token token) throws ParseException {
    try {
      DataTypeName datatype = DataTypeName.of(token.text());
      return Literal.of(datatype);
    } catch (RuntimeException e) {
      throw new ParseException(ExpressionError.INVALID_DATATYPE.message(token.text()),
          token.start());
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
            throw new ParseException(ExpressionError.MISSING_END_SINGLE_QUOTED_STRING.message(),
                start);
          }

          return new Token(Id.LITERAL_STRING, start, position, text.toString());
        }

        case '=':
          return new Token(Id.EQUAL, position++);

        case '+':
          return new Token(Id.PLUS, position++);

        case '-': {
          // Single line comment --
          if (position + 1 < source.length()) {
            if (source.charAt(position + 1) == '-') {
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
          throw new ParseException(ExpressionError.UNEXPECTED_CHARACTER.message('!'), start);
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
          throw new ParseException(ExpressionError.UNEXPECTED_CHARACTER.message(':'), start);
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
                  throw new ParseException(ExpressionError.MISSING_END_BLOCK_COMMENT.message(),
                      start);
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
          throw new ParseException(ExpressionError.UNEXPECTED_CHARACTER.message(), start);
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
          if (Characters.isSpace(c) || c == '\n' || c == '\r') {
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

          if (c == '(') {
            // Special function with custom syntax
            if (FUNCTION_WITH_CUSTOM_SYNTAX.contains(name)) {
              return new Token(Id.valueOf(name), start, position, name);
            }
            if (FunctionRegistry.isFunction(name)) {
              return new Token(Id.FUNCTION, start, position, name);
            }
          }

          // Reserved words: AS, AND, LIKE, NOT, TRUE, FALSE, OR
          if (isReservedWord(name)) {
            return new Token(Id.valueOf(name), start, position, name);
          }

          if (DataTypeName.exist(name)) {
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

  /**
   * Compile and optimize the expression.
   *
   * @param context the context
   * @param expression the expression to compile
   * @return the optimized expression
   */
  protected IExpression compile(IExpressionContext context, IExpression expression)
      throws ExpressionException {
    if (expression == null)
      return null;

    IExpression original = expression;

    int cycle = 20;
    do {
      // Compile operands first
      if (expression instanceof Call) {
        expression = compileCall(context, (Call) expression);
      } else if (expression instanceof Tuple) {
        expression = compileTuple(context, (Tuple) expression);
      }

      // Apply optimizers
      for (ExpressionCompiler optimizer : OPTIMIZERS) {
        expression = expression.accept(context, optimizer);
      }

      if (expression.equals(original)) {

        // Return type inference
        expression = expression.accept(context, RETURN_TYPE);

        // Check operand types expected
        if (expression instanceof Call) {
          Call call = (Call) expression;
          Operator operator = call.getOperator();
          IOperandTypeChecker operandTypeChecker = operator.getOperandTypeChecker();
          if (!operandTypeChecker.checkOperandTypes(call)) {
            throw new ExpressionException(
                ExpressionError.ILLEGAL_ARGUMENT.message(operator.getName()));
          }
        }

        return expression;
      }

      original = expression;
    } while (--cycle > 0);

    return expression;
  }

  protected IExpression compileTuple(IExpressionContext context, Tuple tuple)
      throws ExpressionException {
    List<IExpression> elements = new ArrayList<>(tuple.size());
    for (IExpression expression : tuple) {
      elements.add(compile(context, expression));
    }
    return new Tuple(elements);
  }

  protected IExpression compileCall(IExpressionContext context, Call call)
      throws ExpressionException {

    // Check the number of operands expected
    Operator operator = call.getOperator();
    IOperandCountRange operandCountRange = operator.getOperandCountRange();
    if (!operandCountRange.isValid(call.getOperandCount())) {
      if (call.getOperandCount() < operandCountRange.getMin()) {
        throw new ExpressionException(
            ExpressionError.NOT_ENOUGH_ARGUMENT.message(operator.getId()));
      }
      if (call.getOperandCount() > operandCountRange.getMax()) {
        throw new ExpressionException(ExpressionError.TOO_MANY_ARGUMENT.message(operator.getId()));
      }
    }

    // Replace arguments in User Defined Function by the operands of the call.
    if (call.getOperator() instanceof UserDefinedFunction) {
      return compile(context, call, (UserDefinedFunction) call.getOperator());
    }

    // Optimize all operands
    List<IExpression> operands = new ArrayList<>(call.getOperandCount());
    for (IExpression expression : call.getOperands()) {
      operands.add(compile(context, expression));
    }

    return new Call(operator, operands);
  }

  protected IExpression inferReturnType​(IExpressionContext context, Call call) {
    // Inference return data type
    DataTypeName type = call.getOperator().getReturnTypeInference().getReturnType​(context, call);
    return new Call(type, call.getOperator(), call.getOperands());
  }

  protected IExpression compile(IExpressionContext context, Call call, UserDefinedFunction udf)
      throws ExpressionException {
    try {
      IExpressionContext udfContext = new ExpressionContext(context, udf.createRowMeta());
      IExpression expression = ExpressionBuilder.compile(udfContext, udf.getSource());
      return expression.accept(context, new UserDefinedFunctionResolver(call.getOperands()));
    } catch (Exception e) {
      throw new ExpressionException(ExpressionError.UDF_COMPILATION_ERROR, udf.getName());
    }
  }

}
