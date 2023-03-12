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

import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.expression.Token.Id;
import org.apache.hop.expression.type.DataTypeFamily;
import org.apache.hop.expression.type.DataTypeName;
import org.apache.hop.expression.type.IOperandCountRange;
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.util.Characters;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.ExpressionUtils;
import org.apache.hop.expression.util.NumberFormat;
import java.math.BigDecimal;
import java.text.ParseException;
import java.time.ZonedDateTime;
import java.time.zone.ZoneRulesException;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;
import java.util.Set;
import com.fasterxml.jackson.core.json.JsonReadFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.json.JsonMapper;

public class ExpressionBuilder {

  private static final Set<String> RESERVED_WORDS = Set.of("AND", "AS", "ASYMMETRIC", "AT",
      "BETWEEN", "CASE", "DATE", "DISTINCT", "ELSE", "END", "ESCAPE", "FALSE", "FORMAT", "FROM",
      "IGNORE", "ILIKE", "IN", "IS", "JSON", "KEY", "LIKE", "NOT", "NULL", "NULLS", "OR", "RESPECT",
      "RLIKE", "SYMMETRIC", "THEN", "TIME", "TIMESTAMP", "TRUE", "VALUE", "WHEN", "ZONE");

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

  public static IExpression build(IExpressionContext context, final String source)
      throws ExpressionException {
    ExpressionBuilder builder = new ExpressionBuilder(source);
    try {
      IExpression expression = builder.parse();
      return builder.validate(context, expression);
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

  protected boolean isThenNext(final Id id) {
    if (hasNext() && tokens.get(index).is(id)) {
      index++;
      return true;
    }
    return false;
  }

  protected boolean isNotThenNext(final Id id) {
    return !this.isThenNext(id);
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
    while (isThenNext(Id.OR)) {
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
    while (isThenNext(Id.AND)) {
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

    if (isThenNext(Id.NOT)) {
      return new Call(Operators.BOOLNOT, parseLogicalNot());
    }

    return this.parseIs();
  }

  /**
   * Parse IS expression
   *
   * <p>
   * RelationalExpression IS [NOT] TRUE|FALSE|NULL
   * RelationalExpression IS [NOT] DISTINCT FROM RelationalExpression
   */
  private IExpression parseIs() throws ParseException {
    IExpression expression = this.parseComparaison();
    if (isThenNext(Id.IS)) {
      boolean not = false;
      if (isThenNext(Id.NOT)) {
        not = true;
      }

      Token token = next();
      switch (token.id()) {
        case TRUE:
          return new Call((not) ? Operators.IS_FALSE : Operators.IS_TRUE, expression);
        case FALSE:
          return new Call((not) ? Operators.IS_TRUE : Operators.IS_FALSE, expression);
        case NULL:
          return new Call((not) ? Operators.IS_NOT_NULL : Operators.IS_NULL, expression);
        case DISTINCT:
          if (isThenNext(Id.FROM)) {
            return new Call((not) ? Operators.IS_NOT_DISTINCT_FROM : Operators.IS_DISTINCT_FROM,
                expression, parseLogicalNot());
          }
        default:
          throw new ParseException(ExpressionError.INVALID_OPERATOR.message(Id.IS),
              this.getPosition());
      }
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
    if (isThenNext(Id.NOT)) {
      not = true;
    }

    if (isThenNext(Id.LIKE)) {
      IExpression pattern = this.parseAdditive();

      if (isThenNext(Id.ESCAPE)) {
        IExpression escape = this.parseLiteralString(next());
        expression = new Call(Operators.LIKE, expression, pattern, escape);
      } else {
        expression = new Call(Operators.LIKE, expression, pattern);
      }
    } else if (isThenNext(Id.ILIKE)) {
      IExpression pattern = this.parseAdditive();

      if (isThenNext(Id.ESCAPE)) {
        IExpression escape = this.parseAdditive();
        expression = new Call(Operators.ILIKE, expression, pattern, escape);
      } else {
        expression = new Call(Operators.ILIKE, expression, pattern);
      }
    } else if (isThenNext(Id.IN)) {
      expression = new Call(Operators.IN, expression, this.parseTuple());
    } else if (isThenNext(Id.BETWEEN)) {
      Operator operator = Operators.BETWEEN_ASYMMETRIC;
      if (isThenNext(Id.ASYMMETRIC)) {
        // Ignore
      } else if (isThenNext(Id.SYMMETRIC)) {
        operator = Operators.BETWEEN_SYMMETRIC;
      }

      IExpression start = this.parseAdditive();
      if (isNotThenNext(Id.AND)) {
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
      if (isThenNext(Id.MULTIPLY)) {
        expression = new Call(Operators.MULTIPLY, expression, this.parseBitwiseNot());
      } else if (isThenNext(Id.DIVIDE)) {
        expression = new Call(Operators.DIVIDE, expression, this.parseBitwiseNot());
      } else if (isThenNext(Id.MODULUS)) {
        expression = new Call(Operators.MODULUS, expression, this.parseBitwiseNot());
      } else
        break;
    }

    return expression;
  }

  /** ( UnaryExpression)* */
  private IExpression parseBitwiseNot() throws ParseException {
    if (isThenNext(Id.BITWISE_NOT)) {
      return new Call(Operators.BITNOT, this.parseUnary());
    }
    return this.parseUnary();
  }

  /** UnaryExpression ( & UnaryExpression)* */
  private IExpression parseBitwiseAnd() throws ParseException {
    IExpression expression = this.parseFactor();
    if (isThenNext(Id.BITWISE_AND)) {
      return new Call(Operators.BITAND, expression, this.parseFactor());
    }
    return expression;
  }

  /** BitwiseXorExpression ( | BitwiseXorExpression)* */
  private IExpression parseBitwiseOr() throws ParseException {
    IExpression expression = this.parseBitwiseXor();
    if (isThenNext(Id.BITWISE_OR)) {
      return new Call(Operators.BITOR, expression, this.parseBitwiseXor());
    }
    return expression;
  }

  /** BitwiseAndExpression ( ^ BitwiseAndExpression)* */
  private IExpression parseBitwiseXor() throws ParseException {
    IExpression expression = this.parseBitwiseAnd();
    if (isThenNext(Id.BITWISE_XOR)) {
      return new Call(Operators.BITXOR, expression, this.parseBitwiseAnd());
    }
    return expression;
  }

  /** (('+' | '-') PrimaryExpression ) */
  private IExpression parseUnary() throws ParseException {

    if (isThenNext(Id.MINUS)) {
      return new Call(Operators.NEGATIVE, this.parsePrimary());
    }
    if (isThenNext(Id.PLUS)) {
      // Ignore
    }

    return this.parsePrimary();
  }

  /** Literal String */
  private Literal parseLiteralString(Token token) {
    return Literal.of(token.text());
  }

  /** Literal Json */
  private Literal parseLiteralJson(Token token) throws ParseException {
    try {
      ObjectMapper objectMapper =
          JsonMapper.builder().enable(JsonReadFeature.ALLOW_UNQUOTED_FIELD_NAMES).build();
      return Literal.of(objectMapper.readTree(token.text()));
    } catch (Exception e) {
      throw new ParseException(ExpressionError.INVALID_JSON.message(token.text()), token.start());
    }
  }

  /**
   * Cast operator <term>::<datatype> | <term> AT TIMEZONE <timezone>)
   *
   */
  private IExpression parsePrimary() throws ParseException {
    IExpression expression = this.parseTerm();
    if (isThenNext(Id.CAST)) {
      Literal type = parseLiteralDataType(next());
      return new Call(Operators.CAST, expression, type);
    }
    if (isThenNext(Id.AT)) {
      if (isThenNext(Id.TIME) && isThenNext(Id.ZONE)) {
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
      case LITERAL_TIMEUNIT:
        return parseLiteralTimeUnit(token);
      case LITERAL_DATATYPE:
        return parseLiteralDataType(token);
      // Date can be Literal or DataType
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
      case JSON:
        token = next();
        if (token == null)
          break;
        return parseLiteralJson(token);
      case CASE:
        return parseCase();
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

    if (isThenNext(Id.EQUAL)) {
      return new Call(Operators.EQUAL, expression, this.parseRelational());
    }
    if (isThenNext(Id.NOT_EQUAL)) {
      return new Call(Operators.NOT_EQUAL, expression, this.parseRelational());
    }
    if (isThenNext(Id.GT)) {
      return new Call(Operators.GREATER_THAN, expression, this.parseRelational());
    }
    if (isThenNext(Id.GTE)) {
      return new Call(Operators.GREATER_THAN_OR_EQUAL, expression, this.parseRelational());
    }
    if (isThenNext(Id.LT)) {
      return new Call(Operators.LESS_THAN, expression, this.parseRelational());
    }
    if (isThenNext(Id.LTE)) {
      return new Call(Operators.LESS_THAN_OR_EQUAL, expression, this.parseRelational());
    }

    return expression;
  }

  /**
   * BitwiseOrExpression ( (+ | - | ||) BitwiseOrExpression )*
   **/
  private IExpression parseAdditive() throws ParseException {
    IExpression expression = this.parseBitwiseOr();
    while (hasNext()) {
      if (isThenNext(Id.PLUS)) {
        // Supports the basic addition and subtraction of days to DATE values, in the form of { + |
        // - } <integer>
        if (expression.getType().isSameFamily(DataTypeFamily.TEMPORAL)) {
         expression = new Call(Operators.ADD_DAYS, expression, this.parseBitwiseOr());
        } else {
          expression = new Call(Operators.ADD, expression, this.parseBitwiseOr());
        }
      } else if (isThenNext(Id.MINUS)) {
        if (expression.getType().isSameFamily(DataTypeFamily.TEMPORAL)) {
          expression = new Call(Operators.ADD_DAYS, expression,
              new Call(Operators.NEGATIVE, this.parseBitwiseOr()));
        } else {
         expression = new Call(Operators.SUBTRACT, expression, this.parseBitwiseOr());
       }
      } else if (isThenNext(Id.CONCAT)) {
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

    // Return as INTEGER
    // if ( bytes.length<=8 ) {
    // long result = 0;
    // for (int i = 0; i < bytes.length; i++) {
    // result <<= Byte.SIZE;
    // result |= (bytes[i] & 0xFF);
    // }
    // return Literal.of(result);
    // }

    // Return as BINARY
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

    byte[] bytes = bitset.toByteArray();

    // Return as INTEGER
    // if ( bytes.length<=8 ) {
    // long result = 0;
    // for (int i = 0; i < bytes.length; i++) {
    // result <<= Byte.SIZE;
    // result |= (bytes[i] & 0xFF);
    // }
    // return Literal.of(result);
    // }

    // Return as BINARY
    return Literal.of(bytes);
  }

  /**
   * Parses a date literal.
   * The parsing is strict and requires months to be less than 12, days to be less than 31, etc.
   */
  private Literal parseLiteralDate(Token token) throws ParseException {
    try {
      DateTimeFormat format = DateTimeFormat.of("FXYYYY-MM-DD");
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
      
      // TODO: Move to an AUTO format
      int length = str.length();
      switch (length) {
        case 36: // 2021-01-01 15:28:59.123456789 +02:00
        case 35: // 2021-01-01 15:28:59.123456789+02:00
          pattern = "YYYY-MM-DD HH24:MI:SS.FF9TZH:TZM";
          break;
        case 33: // 2021-01-01 5:28:59.123456789+0200
          if (str.indexOf(':', 20) > 0)
            pattern = "YYYY-MM-DD HH24:MI:SS.FF6 TZH:TZM";
          else
          pattern = "YYYY-MM-DD HH24:MI:SS.FF9TZHTZM";
          break;    
        case 34: // 2021-01-01 15:28:59.123456789+0200
          pattern = "YYYY-MM-DD HH24:MI:SS.FF9TZHTZM";
          break;
        case 30: // 2021-01-01 15:28:59.123 +02:00
          pattern = "YYYY-MM-DD HH24:MI:SS.FF3 TZH:TZM";
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

    if (isThenNext(Id.LPARENTHESIS)) {

      do {
        IExpression value = parsePrimary();
        list.add(value);

        if (isThenNext(Id.COMMA)) {
          continue;
        }

        if (isThenNext(Id.RPARENTHESIS)) {
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
    while (isThenNext(Id.WHEN)) {
      whenList.add(this.parseLogicalOr());
      if (isNotThenNext(Id.THEN)) {
        throw new ParseException(ExpressionError.INVALID_OPERATOR.message(Id.CASE),
            this.getPosition());
      }
      thenList.add(this.parseLogicalOr());
    }

    if (isThenNext(Id.ELSE)) {
      elseExpression = this.parseLogicalOr();
    }

    if (isNotThenNext(Id.END)) {
      throw new ParseException(ExpressionError.INVALID_OPERATOR.message(Id.CASE),
          this.getPosition());
    }

    return new Call(Operators.CASE, valueExpression, new Tuple(whenList), new Tuple(thenList),
        elseExpression);
  }

  /**
   * Cast function CAST(value AS type [FORMAT pattern])
   */
  private IExpression parseFunctionCast(Token token, Function function) throws ParseException {
    List<IExpression> operands = new ArrayList<>();

    if (isNotThenNext(Id.LPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_LEFT_PARENTHESIS.message(), token.end());
    }

    operands.add(this.parseLogicalOr());

    if (isNotThenNext(Id.AS)) {
      throw new ParseException(ExpressionError.INVALID_OPERATOR.message(Id.CAST), token.start());
    }

    operands.add(this.parseLiteralDataType(next()));

    if (isThenNext(Id.FORMAT)) {
      token = next();
      if (token.is(Id.LITERAL_STRING))
        operands.add(this.parseLiteralString(token));
      else
        throw new ParseException(ExpressionError.INVALID_OPERATOR.message(Id.CAST), token.start());
    }

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_RIGHT_PARENTHESIS.message(), token.end());
    }

    return new Call(function, operands);
  }

  /** 
   * Parse function POSITION(<expression> IN <expression>)
   */
  private IExpression parseFunctionPosition(Token token, final Function function) throws ParseException {

    List<IExpression> operands = new ArrayList<>();

    if (isNotThenNext(Id.LPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_LEFT_PARENTHESIS.message(), token.end());
    }

    operands.add(this.parseAdditive());

    if (isNotThenNext(Id.IN)) {
      throw new ParseException(ExpressionError.INVALID_OPERATOR.message(function.getName()),
          this.getPosition());
    }

    operands.add(this.parseAdditive());

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_LEFT_PARENTHESIS.message(), token.end());
    }

    return new Call(function, operands);
  }

  /**
   * FIRST_VALUE(<expression> [ IGNORE NULLS | RESPECT NULLS ] )
   * LAST_VALUE(<expression> [ IGNORE NULLS | RESPECT NULLS ] )
   */
  private IExpression parseFunctionFirstLastValue(Token token, final Function function) throws ParseException {

    if (isNotThenNext(Id.LPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_LEFT_PARENTHESIS.message(), token.end());
    }
    List<IExpression> operands = new ArrayList<>();
    operands.add(this.parseLogicalOr());


    if (isThenNext(Id.IGNORE)) {
      if (isThenNext(Id.NULLS)) {
        operands.add(Literal.TRUE);
      } else
        throw new ParseException(ExpressionError.INVALID_OPERATOR.message(function.getName()),
            this.getPosition());
    } else if (isThenNext(Id.RESPECT)) {

      // Default respect null, no operand

      if (isNotThenNext(Id.NULLS)) {
        throw new ParseException(ExpressionError.INVALID_OPERATOR.message(function.getName()),
            this.getPosition());
      }
    }

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_LEFT_PARENTHESIS.message(), token.end());
    }

    return new Call(function, operands);
  }

  /** EXTRACT(<part> FROM <expression>) */
  private IExpression parseFunctionExtract(Token token, final Function function) throws ParseException {

    if (isNotThenNext(Id.LPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_LEFT_PARENTHESIS.message(), token.end());
    }

    List<IExpression> operands = new ArrayList<>();

    operands.add(this.parseLiteralTimeUnit(next()));

    if (isNotThenNext(Id.FROM)) {
      throw new ParseException(ExpressionError.INVALID_OPERATOR.message(function.getName()),
          this.getPosition());
    }

    operands.add(this.parseAdditive());

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_RIGHT_PARENTHESIS.message(), token.end());
    }

    return new Call(function, operands);
  }

  /**
   * COUNT(*) | COUNT([DISTINCT] <expression>)
   */
  private IExpression parseFunctionCount(Token token, Function function) throws ParseException {

    AggregateFunction aggregator = Operators.COUNT_VALUE;

    if (isNotThenNext(Id.LPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_LEFT_PARENTHESIS.message(), token.start());
    }

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
      throw new ParseException(ExpressionError.MISSING_RIGHT_PARENTHESIS.message(), token.start());
    }

    return new Call(aggregator, operands);
  }

  /**
   * LISTAGG([DISTINCT] <expression> [, delimiter] )
   */
  private IExpression parseFunctionListAgg(Token token, final Function function) throws ParseException {

    AggregateFunction aggregator = Operators.LISTAGG_ALL;

    if (isNotThenNext(Id.LPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_LEFT_PARENTHESIS.message(), token.start());
    }

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
      throw new ParseException(ExpressionError.MISSING_RIGHT_PARENTHESIS.message(), token.start());
    }

    return new Call(aggregator, operands);
  }

  /** JSON_OBJECT([KEY] <key> VALUE <expression> [, [KEY] <key> VALUE <expression>]...) */
  private IExpression parseFunctionJsonObject(Token token, final Function function) throws ParseException {

    if (isNotThenNext(Id.LPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_LEFT_PARENTHESIS.message(), token.start());
    }

    List<IExpression> operands = new ArrayList<>();
    boolean comma;
    do {

      if (isThenNext(Id.KEY)) {
        // KEY is optional
      }
      token = next();
      operands.add(this.parseLiteralString(token));

      if (isThenNext(Id.VALUE)) {
        operands.add(this.parsePrimary());
      } else {
        throw new ParseException(ExpressionError.INVALID_OPERATOR.message(function.getName()),
            token.start());
      }

      comma = false;
      if (is(Id.COMMA)) {
        comma = true;
        token = next();
      }
    } while (comma);

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_RIGHT_PARENTHESIS.message(), token.start());
    }

    return new Call(function, operands);
  }

  /** Function */
  private IExpression parseFunction(Token token) throws ParseException {

    Function function = FunctionRegistry.getFunction(token.text());
    
    // Function with custom syntax
    switch(token.text()) {
      case "CAST":
        return parseFunctionCast(token, function);
      case "EXTRACT":
        return parseFunctionExtract(token, function);
      case "POSITION":
        return parseFunctionPosition(token, function);
      case "COUNT":
        return parseFunctionCount(token, function);
      case "FIRST_VALUE":
      case "LAST_VALUE":
        return this.parseFunctionFirstLastValue(token, function);
      case "LISTAGG":
        return this.parseFunctionListAgg(token, function);
      case "JSON_OBJECT":
        return this.parseFunctionJsonObject(token, function);
    }
    
    List<IExpression> operands = new ArrayList<>();

    if (isNotThenNext(Id.LPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_LEFT_PARENTHESIS.message(), token.start());
    }

    // No param function
    if (isThenNext(Id.RPARENTHESIS)) {
      return new Call(function, operands);
    }

    operands.add(this.parseLogicalOr());

    while (isThenNext(Id.COMMA)) {
      operands.add(this.parseLogicalOr());
    }

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ParseException(ExpressionError.MISSING_RIGHT_PARENTHESIS.message(), token.start());
    }

    return new Call(function, operands);
  }

  private Literal parseLiteralTimeUnit(Token token) throws ParseException {
    try {
      TimeUnit unit = TimeUnit.of(token.text());
      return Literal.of(unit);
    } catch (RuntimeException e) {
      throw new ParseException(ExpressionError.INVALID_TIMEUNIT.message(token.text()),
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

        // parse bang equal symbol "!="
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

          if (TimeUnit.exist(name)) {
            return new Token(Id.LITERAL_TIMEUNIT, start, position, name);
          }

          return new Token(Id.IDENTIFIER, start, position, identifier);
      }
    }
    return null;
  }

  /**
   * Validate and optimize the expression.
   *
   * @param context the context
   * @param expression the expression to compile
   * @return the optimized expression
   */
  protected IExpression validate(final IExpressionContext context, IExpression expression)
      throws ExpressionException {

    if (expression == null)
      return expression;

    // Validate
    switch (expression.getKind()) {
      case CALL:
        expression = validateCall(context, (Call) expression);
        break;
      case IDENTIFIER:
        expression = validateIdentifier(context, (Identifier) expression);
        break;
      case LITERAL:
        expression = validateLiteral(context, (Literal) expression);
        break;
      case TUPLE:
        expression = validateTuple(context, (Tuple) expression);
        break;
    }

    // Optimize
    Optimizer optimizer = new Optimizer();
    IExpression original;
    do {
      original = expression;
      expression = expression.accept(context, optimizer);
    } while (!expression.equals(original));

    return expression;
  }

  /**
   * Validate a identifier.
   * 
   * <ul>
   * <li>Resolve index in IRowMeta</li>
   * <li>Determine data type of a value in row.</li>
   * </ul>
   */
  protected IExpression validateIdentifier(final IExpressionContext context,
      final Identifier identifier) throws ExpressionException {
    IRowMeta rowMeta = context.getRowMeta();

    int indexOfValue = rowMeta.indexOfValue(identifier.getName());
    if (indexOfValue < 0) {
      throw new ExpressionException(ExpressionError.UNRESOLVED_IDENTIFIER, identifier);
    }

    DataTypeName type = ExpressionUtils.createDataType(rowMeta.getValueMeta(indexOfValue));

    return new Identifier(identifier.getName(), type, indexOfValue);
  }

  /**
   * Validate a literal.
   * 
   * @param context The context against which the expression will be validated.
   * @param literal Literal
   */
  protected IExpression validateLiteral(final IExpressionContext context, final Literal literal) {
    return literal;
  }

  protected IExpression validateTuple(IExpressionContext context, Tuple tuple)
      throws ExpressionException {
    List<IExpression> elements = new ArrayList<>(tuple.size());
    for (IExpression expression : tuple) {
      elements.add(validate(context, expression));
    }
    return new Tuple(elements);
  }

  /**
   * Validate a call.
   * 
   * @param context The context against which the expression will be validated.
   * @param call Call
   */
  protected IExpression validateCall(IExpressionContext context, Call call)
      throws ExpressionException {

    Operator operator = call.getOperator();

    // Validate all operands
    List<IExpression> operands = new ArrayList<>();
    for (IExpression expression : call.getOperands()) {
      operands.add(validate(context, expression));
    }
    call = new Call(operator, operands);


    // Check the number of operands expected
    IOperandCountRange operandCountRange = operator.getOperandCountRange();
    if (!operandCountRange.isValid(call.getOperandCount())) {
      if (call.getOperandCount() < operandCountRange.getMin()) {
        throw new ExpressionException(ExpressionError.NOT_ENOUGH_ARGUMENT.message(operator));
      }
      if (call.getOperandCount() > operandCountRange.getMax()) {
        throw new ExpressionException(ExpressionError.TOO_MANY_ARGUMENT.message(operator));
      }
    }

    // Check operand types expected
    IOperandTypeChecker operandTypeChecker = operator.getOperandTypeChecker();
    if (!operandTypeChecker.checkOperandTypes(call)) {
      throw new ExpressionException(ExpressionError.ILLEGAL_ARGUMENT_TYPE.message(operator));
    }

    // Replace arguments in User Defined Function by the operands of the call.
    if (operator instanceof UserDefinedFunction) {
      UserDefinedFunction udf = (UserDefinedFunction) operator;
      try {
        IExpressionContext udfContext = new ExpressionContext(context, udf.createRowMeta());
        IExpression expression = ExpressionBuilder.build(udfContext, udf.getSource());
        return expression.accept(context, new UserDefinedFunctionResolver(call.getOperands()));
      } catch (Exception e) {
        throw new ExpressionException(ExpressionError.UDF_COMPILATION_ERROR, udf.getName());
      }
    }

    return call;
  }
}

