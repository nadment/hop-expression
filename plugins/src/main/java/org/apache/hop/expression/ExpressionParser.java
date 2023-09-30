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
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.BooleanType;
import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.JsonType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.util.Characters;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.NumberFormat;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class ExpressionParser {

  private static final Set<String> RESERVED_WORDS =
      Set.of("AND", "AS", "ASYMMETRIC", "AT", "BETWEEN", "BINARY", "CASE", "DATE", "DISTINCT",
          "ELSE", "END", "ESCAPE", "FALSE", "FORMAT", "FROM", "IGNORE", "ILIKE", "IN", "INTERVAL",
          "IS", "JSON", "KEY", "LIKE", "NOT", "NULL", "NULLS", "OR", "RESPECT", "RLIKE", "SIMILAR",
          "SYMMETRIC", "THEN", "TIME", "TIMESTAMP", "TO", "TRUE", "VALUE", "WHEN", "ZONE");

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

  public ExpressionParser(final String source) {
    super();
    this.source = source;
  }

  public String getSource() {
    return source;
  }

  protected int getPosition() {

    if (index > 0 && index < tokens.size())
      return tokens.get(index).start();

    return source.length();
  }

  protected boolean hasNext() {
    return index < tokens.size();
  }

  protected Token next() throws ExpressionException {
    if (!hasNext()) {
      throw new ExpressionException(getPosition(), ExpressionError.UNEXPECTED_END_OF_EXPRESSION);
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
  public IExpression parse() throws ExpressionException {

    if (source == null)
      throw new ExpressionException(0, ExpressionError.NULL_SOURCE_ERROR);

    // Tokenize
    for (Token token = tokenize(); token != null; token = tokenize()) {

      // Ignore comment
      if (token.is(Id.COMMENT))
        continue;

      tokens.add(token);
    }

    // Empty source return NULL
    if (tokens.isEmpty())
      return Literal.UNKNOWN;

    IExpression expression = this.parseLogicalOr();

    // Unexpected end of expression
    if (hasNext()) {
      Token token = next();
      throw new ExpressionException(token.start(), ExpressionError.UNEXPECTED_CHARACTER,
          token.text());
    }

    // Symbol only are not expected here
    if (expression.getType().is(TypeName.SYMBOL)) {
      throw new ExpressionException(0, ExpressionError.SYNTAX_ERROR_NEAR_KEYWORD, source);
    }

    return expression;
  }

  /**
   * Parse logical OR expression
   *
   * <p>
   * LogicalAndExpression ( OR LogicalAndExpression )*
   */
  private IExpression parseLogicalOr() throws ExpressionException {
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
  private IExpression parseLogicalAnd() throws ExpressionException {
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
   * [NOT] IdentityExpression
   */
  private IExpression parseLogicalNot() throws ExpressionException {

    if (isThenNext(Id.NOT)) {
      return new Call(Operators.BOOLNOT, parseLogicalNot());
    }

    return this.parseIs();
  }

  /**
   * Parse identity expression
   *
   * <p>
   * RelationalExpression IS [NOT] TRUE|FALSE|NULL
   * RelationalExpression IS [NOT] DISTINCT FROM RelationalExpression
   */
  private IExpression parseIs() throws ExpressionException {
    IExpression expression = this.parseComparison();
    if (isThenNext(Id.IS)) {
      boolean not = false;
      int start = this.getPosition();

      if (isThenNext(Id.NOT)) {
        not = true;
      }

      Token token = next();
      switch (token.id()) {
        case TRUE:
          return new Call((not) ? Operators.IS_NOT_TRUE : Operators.IS_TRUE, expression);
        case FALSE:
          return new Call((not) ? Operators.IS_NOT_FALSE : Operators.IS_FALSE, expression);
        case NULL:
          return new Call((not) ? Operators.IS_NOT_NULL : Operators.IS_NULL, expression);
        case DISTINCT:
          if (isThenNext(Id.FROM)) {
            return new Call(start,
                (not) ? Operators.IS_NOT_DISTINCT_FROM : Operators.IS_DISTINCT_FROM, expression,
                parseLogicalNot());
          }
          throw new ExpressionException(start, ExpressionError.SYNTAX_ERROR_NEAR_KEYWORD,
              Id.DISTINCT);
        default:
          throw new ExpressionException(start, ExpressionError.SYNTAX_ERROR_NEAR_KEYWORD, Id.IS);
      }
    }
    return expression;
  }

  /**
   * Parse IN, LIKE, BETWEEN, SIMILAR TO expression
   *
   * <p>
   * AdditiveExpression ( [NOT] | InClause | BetweenClause | LikeClause AdditiveExpression)
   */
  private IExpression parseRelational() throws ExpressionException {
    IExpression expression = this.parseAdditive();

    // Special case NOT before operation: <exp> [NOT] LIKE|ILIKE|IN|BETWEEN|SIMILAR <primaryExp>
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
        throw new ExpressionException(getPosition(), ExpressionError.SYNTAX_ERROR_NEAR_KEYWORD,
            Id.BETWEEN);
      }
      IExpression end = this.parseAdditive();

      expression = new Call(operator, expression, start, end);
    }

    if (isThenNext(Id.SIMILAR)) {
      if (isThenNext(Id.TO)) {
        expression = new Call(Operators.SIMILAR_TO, expression, parseAdditive());
      } else
        throw new ExpressionException(getPosition(), ExpressionError.SYNTAX_ERROR_NEAR_KEYWORD,
            Id.SIMILAR);
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
  private IExpression parseFactor() throws ExpressionException {
    IExpression expression = this.parseBitwiseNot();

    while (hasNext()) {

      int start = this.getPosition();

      if (isThenNext(Id.MULTIPLY)) {
        expression = new Call(start, Operators.MULTIPLY, expression, this.parseBitwiseNot());
      } else if (isThenNext(Id.DIVIDE)) {
        expression = new Call(start, Operators.DIVIDE, expression, this.parseBitwiseNot());
      } else if (isThenNext(Id.MODULUS)) {
        expression = new Call(start, Operators.MODULUS, expression, this.parseBitwiseNot());
      } else
        break;
    }

    return expression;
  }

  /** ( UnaryExpression)* */
  private IExpression parseBitwiseNot() throws ExpressionException {

    int start = this.getPosition();

    if (isThenNext(Id.BITWISE_NOT)) {
      return new Call(start, Operators.BITNOT, this.parseUnary());
    }
    return this.parseUnary();
  }

  /** UnaryExpression ( & UnaryExpression)* */
  private IExpression parseBitwiseAnd() throws ExpressionException {
    IExpression expression = this.parseFactor();
    int start = this.getPosition();
    if (isThenNext(Id.BITWISE_AND)) {
      return new Call(start, Operators.BITAND, expression, this.parseFactor());
    }
    return expression;
  }

  /** BitwiseXorExpression ( | BitwiseXorExpression)* */
  private IExpression parseBitwiseOr() throws ExpressionException {
    IExpression expression = this.parseBitwiseXor();
    int start = this.getPosition();
    if (isThenNext(Id.BITWISE_OR)) {
      return new Call(start, Operators.BITOR, expression, this.parseBitwiseXor());
    }
    return expression;
  }

  /** BitwiseAndExpression ( ^ BitwiseAndExpression)* */
  private IExpression parseBitwiseXor() throws ExpressionException {
    IExpression expression = this.parseBitwiseAnd();

    int start = this.getPosition();
    if (isThenNext(Id.BITWISE_XOR)) {
      return new Call(start, Operators.BITXOR, expression, this.parseBitwiseAnd());
    }
    return expression;
  }

  /** (('+' | '-') PrimaryExpression ) */
  private IExpression parseUnary() throws ExpressionException {
    int start = this.getPosition();
    if (isThenNext(Id.MINUS)) {
      return new Call(start, Operators.NEGATIVE, this.parsePrimary());
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
  private IExpression parseLiteralJson(Token token) throws ExpressionException  {
    
    return new Call(token.start(), Operators.CAST, Literal.of(token.text()), Literal.of(JsonType.JSON));
//    
//    try {
//      ObjectMapper objectMapper =
//          JsonMapper.builder().enable(JsonReadFeature.ALLOW_UNQUOTED_FIELD_NAMES).build();
//      return Literal.of(objectMapper.readTree(token.text()));
//    } catch (Exception e) {
//      throw new ExpressionException(token.start(), ExpressionError.INVALID_JSON, token.text());
//    }
  }

  /**
   * Cast operator <term>::<datatype> | <term> AT TIMEZONE <timezone>)
   *
   */
  private IExpression parsePrimary() throws ExpressionException {
    IExpression expression = this.parseTerm();
    if (isThenNext(Id.CAST)) {
      Literal type = parseLiteralDataType(next());
      return new Call(Operators.CAST, expression, type);
    }
    if (isThenNext(Id.AT)) {
      if (isThenNext(Id.TIME) && isThenNext(Id.ZONE)) {
        return new Call(getPosition(), Operators.AT_TIME_ZONE, expression, this.parseTerm());
      }
      throw new ExpressionException(getPosition(), ExpressionError.SYNTAX_ERROR_NEAR_KEYWORD,
          Id.AT);
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
        return Literal.UNKNOWN;
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
        token = next();
        if (token == null)
          break;
        return parseLiteralDate(token);
      // case TIME:
      // token = next();
      // if (token == null)
      // break;
      // return parseLiteralTime(token);
      case TIMESTAMP:
        token = next();
        if (token == null)
          break;
        return parseLiteralTimestamp(token);
      case BINARY:
        token = next();
        if (token == null)
          break;
        return parseLiteralBinary(token);
      case JSON:
        token = next();
        if (token == null)
          break;
        return parseLiteralJson(token);
      case INTERVAL:
        return parseLiteralInterval(token);
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
        throw new ExpressionException(token.start(), ExpressionError.UNBALANCE_PARENTHESIS);
      default:
        // Syntax error
    }
    throw new ExpressionException(position, ExpressionError.UNEXPECTED_END_OF_EXPRESSION);
  }

  /** RelationalExpression ( Operator RelationalExpression ) */
  private IExpression parseComparison() throws ExpressionException {
    IExpression expression = this.parseRelational();
    int start = this.getPosition();
    if (isThenNext(Id.EQUAL)) {
      return new Call(start, Operators.EQUAL, expression, this.parseRelational());
    }
    if (isThenNext(Id.NOT_EQUAL)) {
      return new Call(start, Operators.NOT_EQUAL, expression, this.parseRelational());
    }
    if (isThenNext(Id.GT)) {
      return new Call(start, Operators.GREATER_THAN, expression, this.parseRelational());
    }
    if (isThenNext(Id.GTE)) {
      return new Call(start, Operators.GREATER_THAN_OR_EQUAL, expression, this.parseRelational());
    }
    if (isThenNext(Id.LT)) {
      return new Call(start, Operators.LESS_THAN, expression, this.parseRelational());
    }
    if (isThenNext(Id.LTE)) {
      return new Call(start, Operators.LESS_THAN_OR_EQUAL, expression, this.parseRelational());
    }

    return expression;
  }

  /**
   * BitwiseOrExpression ( (+ | - | ||) BitwiseOrExpression )*
   **/
  private IExpression parseAdditive() throws ExpressionException {
    IExpression expression = this.parseBitwiseOr();
    while (hasNext()) {
      int start = this.getPosition();

      if (isThenNext(Id.PLUS)) {
          expression = new Call(start, Operators.ADD, expression, this.parseBitwiseOr());
      } else if (isThenNext(Id.MINUS)) {
          expression = new Call(start, Operators.SUBTRACT, expression, this.parseBitwiseOr());
      } else if (isThenNext(Id.CONCAT)) {
        expression = new Call(start, Operators.CONCAT, expression, this.parseBitwiseOr());
      } else
        break;
    }

    return expression;
  }

  private Literal parseLiteralNumericDecimal(Token token) throws ExpressionException {
    BigDecimal number = NumberFormat.of("TM").parse(token.text());
    try {
      return Literal.of(number.longValueExact());
    } catch (ArithmeticException e) {
      return Literal.of(number);
    }
  }

  private Literal parseLiteralBinary(Token token) throws ExpressionException {
    try {
      String str = token.text();
      if (str.length() % 2 > 0)
        str = '0' + str;
      byte[] bytes = new byte[str.length() / 2];
      for (int i = 0; i < bytes.length; i++) {
        int start = i * 2;
        bytes[i] = (byte) Integer.parseInt(str.substring(start, start + 2), 16);
      }

      // Return as BINARY
      return Literal.of(bytes);
    } catch (Exception e) {
      throw new ExpressionException(token.start(), ExpressionError.UNPARSABLE_BINARY, token.text());
    }
  }

  private Literal parseLiteralNumericHexa(Token token) {
    String str = token.text();
    BigInteger value = new BigInteger(str.substring(2), 16);
    try {
      return Literal.of(value.longValueExact());
    } catch (ArithmeticException e) {
      return Literal.of(new BigDecimal(value));
    }
  }

  private Literal parseLiteralNumericOctal(Token token) {
    String str = token.text();
    BigInteger value = new BigInteger(str.substring(2), 8);
    try {
      return Literal.of(value.longValueExact());
    } catch (ArithmeticException e) {
      return Literal.of(new BigDecimal(value));
    }
  }

  private Literal parseLiteralNumericBinary(Token token) {
    String str = token.text();
    BigInteger value = new BigInteger(str.substring(2), 2);
    try {
      return Literal.of(value.longValueExact());
    } catch (ArithmeticException e) {
      return Literal.of(new BigDecimal(value));
    }
  }

  /**
   * Parses a date literal.
   * The parsing is strict and requires months to be between 1 and 12, days to be less than 31, etc.
   */
  private Literal parseLiteralDate(Token token) throws ExpressionException {
    
    // return new Call(token.start(), Operators.CAST, Literal.of(token.text()), Literal.of(DateType.DATE));
    
    // Literal date use exact mode
    DateTimeFormat format = DateTimeFormat.of("FXYYYY-MM-DD");
    try {
      ZonedDateTime datetime = format.parse(token.text());
      return Literal.of(datetime);
    } catch (Exception e) {
      throw new ExpressionException(token.start(), ExpressionError.UNPARSABLE_DATE_WITH_FORMAT,
          token.text(), format);
    }
  }

  /**
   * Parses a timestamp literal with ISO Formats.
   * 
   * The parsing is strict and requires months to be between 1 and 12, days to be less than 31, etc.
   */
  private Literal parseLiteralTimestamp(Token token) throws ExpressionException {
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
    } catch (Exception e) {
      throw new ExpressionException(token.start(), ExpressionError.INVALID_TIMESTAMP, token.text());
    }
  }

  /**
   * Parses a list of expressions separated by commas.
   * (expression [,expression...] )
   */
  private Tuple parseTuple() throws ExpressionException {

    List<IExpression> list = new ArrayList<>();

    if (isNotThenNext(Id.LPARENTHESIS)) {
      throw new ExpressionException(getPosition(), ExpressionError.MISSING_LEFT_PARENTHESIS);
    }

    do {
      try {
        list.add(parseAdditive());
      } catch (ExpressionException e) {
        throw new ExpressionException(getPosition(), ExpressionError.SYNTAX_ERROR_NEAR_KEYWORD,
            Id.IN);
      }

      if (isThenNext(Id.COMMA)) {
        continue;
      }

      if (isThenNext(Id.RPARENTHESIS)) {
        return new Tuple(list);
      }

      break;
    } while (true);

    throw new ExpressionException(getPosition(), ExpressionError.MISSING_RIGHT_PARENTHESIS);
  }

  /** Case When Then Else End ) */
  private IExpression parseCase() throws ExpressionException {
    IExpression valueExpression = Literal.UNKNOWN;
    IExpression elseExpression = Literal.UNKNOWN;
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

        expression = new Tuple(values);
      }
      whenList.add(expression);

      if (isNotThenNext(Id.THEN)) {
        throw new ExpressionException(getPosition(), ExpressionError.SYNTAX_ERROR_NEAR_KEYWORD,
            Id.CASE);
      }
      thenList.add(this.parseLogicalOr());
    }

    if (isThenNext(Id.ELSE)) {
      elseExpression = this.parseLogicalOr();
    }

    if (isNotThenNext(Id.END)) {
      throw new ExpressionException(getPosition(), ExpressionError.SYNTAX_ERROR_NEAR_KEYWORD,
          Id.CASE);
    }

    return new Call(start, Operators.CASE, valueExpression, new Tuple(whenList),
        new Tuple(thenList), elseExpression);
  }

  /**
   * Cast function CAST(value AS type [FORMAT pattern])
   */
  private IExpression parseFunctionCast(Token token, Function function) throws ExpressionException {
    List<IExpression> operands = new ArrayList<>();

    operands.add(this.parseLogicalOr());

    if (isNotThenNext(Id.AS)) {
      throw new ExpressionException(token.start(), ExpressionError.SYNTAX_ERROR_FUNCTION, Id.CAST);
    }

    operands.add(this.parseLiteralDataType(next()));

    if (isThenNext(Id.FORMAT)) {
      token = next();
      if (token.is(Id.LITERAL_STRING))
        operands.add(this.parseLiteralString(token));
      else
        throw new ExpressionException(token.start(), ExpressionError.SYNTAX_ERROR_NEAR_KEYWORD,
            Id.CAST);
    }

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionException(token.end(), ExpressionError.MISSING_RIGHT_PARENTHESIS);
    }

    return new Call(function, operands);
  }

  /**
   * Parse function POSITION(<expression> IN <expression>)
   */
  private IExpression parseFunctionPosition(Token token, final Function function)
      throws ExpressionException {

    List<IExpression> operands = new ArrayList<>();
    operands.add(this.parseAdditive());

    if (isNotThenNext(Id.IN)) {
      throw new ExpressionException(getPosition(), ExpressionError.SYNTAX_ERROR_FUNCTION,
          function.getName());
    }

    operands.add(this.parseAdditive());

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionException(token.end(), ExpressionError.MISSING_LEFT_PARENTHESIS);
    }

    return new Call(function, operands);
  }

  /**
   * FIRST_VALUE(<expression> [ IGNORE NULLS | RESPECT NULLS ] )
   * LAST_VALUE(<expression> [ IGNORE NULLS | RESPECT NULLS ] )
   */
  private IExpression parseFunctionFirstLastValue(Token token, final Function function)
      throws ExpressionException {

    List<IExpression> operands = new ArrayList<>();
    operands.add(this.parseLogicalOr());


    if (isThenNext(Id.IGNORE)) {
      if (isThenNext(Id.NULLS)) {
        operands.add(Literal.TRUE);
      } else
        throw new ExpressionException(getPosition(), ExpressionError.SYNTAX_ERROR_FUNCTION,
            function.getName());
    } else if (isThenNext(Id.RESPECT)) {

      // Default respect null, no operand

      if (isNotThenNext(Id.NULLS)) {
        throw new ExpressionException(getPosition(), ExpressionError.SYNTAX_ERROR_FUNCTION,
            function.getName());
      }
    }

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionException(token.end(), ExpressionError.MISSING_LEFT_PARENTHESIS);
    }

    return new Call(function, operands);
  }

  /** EXTRACT(<part> FROM <expression>) */
  private IExpression parseFunctionExtract(Token token, final Function function)
      throws ExpressionException {

    List<IExpression> operands = new ArrayList<>();

    operands.add(this.parseLiteralTimeUnit(next()));

    if (isNotThenNext(Id.FROM)) {
      throw new ExpressionException(getPosition(), ExpressionError.SYNTAX_ERROR_FUNCTION,
          function.getName());
    }

    operands.add(this.parseAdditive());

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionException(token.start(), ExpressionError.MISSING_RIGHT_PARENTHESIS);
    }

    return new Call(function, operands);
  }

  /**
   * COUNT(*) | COUNT([DISTINCT] <expression>)
   */
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
      throw new ExpressionException(token.start(), ExpressionError.MISSING_RIGHT_PARENTHESIS);
    }

    return new Call(aggregator, operands);
  }

  /**
   * LISTAGG([DISTINCT] <expression> [, delimiter] )
   */
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
      throw new ExpressionException(token.start(), ExpressionError.MISSING_RIGHT_PARENTHESIS);
    }

    return new Call(aggregator, operands);
  }

  /** JSON_OBJECT([KEY] <key> VALUE <expression> [, [KEY] <key> VALUE <expression>]...) */
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
        throw new ExpressionException(token.start(), ExpressionError.SYNTAX_ERROR_NEAR_KEYWORD,
            function.getName());
      }

      comma = false;
      if (is(Id.COMMA)) {
        comma = true;
        token = next();
      }
    } while (comma);

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionException(token.start(), ExpressionError.MISSING_RIGHT_PARENTHESIS);
    }

    return new Call(function, operands);
  }

  /** Function */
  private IExpression parseFunction(Token token) throws ExpressionException {
    Function function = FunctionRegistry.getFunction(token.text());

    // Function is never null
    if (function == null) {
      throw new ExpressionException(token.start(), ExpressionError.FUNCTION_DOES_NOT_EXIST,
          token.text());
    }

    if (isNotThenNext(Id.LPARENTHESIS)) {
      throw new ExpressionException(token.end(), ExpressionError.MISSING_LEFT_PARENTHESIS);
    }

    // Function with custom syntax
    switch (token.text()) {
      case "CAST":
      case "TRY_CAST":
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



    // No param function
    if (isThenNext(Id.RPARENTHESIS)) {
      return new Call(token.start(), function, operands);
    }

    operands.add(this.parseLogicalOr());

    while (isThenNext(Id.COMMA)) {
      operands.add(this.parseLogicalOr());
    }

    if (isNotThenNext(Id.RPARENTHESIS)) {
      throw new ExpressionException(token.start(), ExpressionError.MISSING_RIGHT_PARENTHESIS);
    }

    return new Call(token.start(), function, operands);
  }

  private Literal parseLiteralTimeUnit(Token token) throws ExpressionException {
    TimeUnit unit = TimeUnit.of(token.text());

    if (unit == null) {
      throw new ExpressionException(token.start(), ExpressionError.INVALID_TIMEUNIT, token.text());
    }
    return Literal.of(unit);
  }

  private Literal parseLiteralInterval(Token token) throws ExpressionException {
    
    boolean negative = false;
    Token value = next();
    if (value == null)
      throw new ExpressionException(token.start(), ExpressionError.INVALID_INTERVAL);
    if (value.is(Id.MINUS)) {
      negative = true;
      value = next();
      if (value == null)
        throw new ExpressionException(token.start(), ExpressionError.INVALID_INTERVAL);
    }
        
    Token start = next();
    if (start == null)
      throw new ExpressionException(token.start(), ExpressionError.INVALID_INTERVAL);
    TimeUnit startUnit = TimeUnit.of(start.text());
    TimeUnit endUnit = null;

    String text = value.text();
    if (value.is(Id.LITERAL_STRING)) {      
      Token end = null;
      if (this.isThenNext(Id.TO)) {
        end = next();
        if (end == null)
          throw new ExpressionException(token.start(), ExpressionError.INVALID_INTERVAL);
        endUnit = TimeUnit.of(end.text());
      }
      
      if ( text.length()>0 &&  text.charAt(0)=='-' ) {
        negative = true;
        text = text.substring(1);
      }
    }
    
    IntervalQualifier qualifier = IntervalQualifier.of(startUnit, endUnit);
    if ( qualifier==null )
      throw new ExpressionException(token.start(), ExpressionError.INVALID_INTERVAL);
    
    Interval interval = qualifier.parse(text);
    if ( interval==null )
      throw new ExpressionException(token.start(), ExpressionError.INVALID_INTERVAL);

    if ( negative ) 
      interval = interval.negate();
    
    return Literal.of(interval);    
  }
  
  private Literal parseLiteralDataType(Token token) throws ExpressionException {

    TypeName name = TypeName.of(token.text());
    if (name != null) {
      int precision = name.getMaxPrecision();
      int scale = Type.SCALE_NOT_SPECIFIED;
      boolean precisionFound = false;
      boolean scaleFound = false;

      if (isThenNext(Id.LPARENTHESIS)) {

        // Precision
        token = this.next();
        precision = Integer.parseInt(token.text());
        precisionFound = true;

        // Scale
        if (isThenNext(Id.COMMA)) {
          token = this.next();
          scale = Integer.parseInt(token.text());
          scaleFound = true;
        }

        if (isNotThenNext(Id.RPARENTHESIS)) {
          throw new ExpressionException(token.start(), ExpressionError.MISSING_RIGHT_PARENTHESIS);
        }
      }

      switch (name) {
        case BOOLEAN:
          if (precisionFound)
            break;
          return Literal.of(BooleanType.BOOLEAN);
        case INTEGER:
          if (precisionFound)
            break;
          return Literal.of(IntegerType.INTEGER);
        case NUMBER:
          return Literal.of(new NumberType(precision, scale));
        case STRING:
          if (scaleFound)
            break;
          return Literal.of(new StringType(precision));
        case BINARY:
          if (scaleFound)
            break;
          return Literal.of(new BinaryType(precision));
        case DATE:
          if (precisionFound)
            break;
          return Literal.of(DateType.DATE);
        case JSON:
          if (precisionFound)
            break;
          return Literal.of(JsonType.JSON);
        default:
      }
    }

    throw new ExpressionException(token.start(), ExpressionError.SYNTAX_ERROR_DATATYPE,
        token.text());
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
            throw new ExpressionException(start, ExpressionError.MISSING_END_SINGLE_QUOTED_STRING);
          }

          return new Token(Id.LITERAL_STRING, start, position, text.toString());
        }

        case '=':
          return new Token(Id.EQUAL, position++);

        case '+':
          return new Token(Id.PLUS, position++);

        case '-': {
          // Single line comment --
          if (position + 1 < source.length() && source.charAt(position + 1) == '-') {
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
          throw new ExpressionException(start, ExpressionError.UNEXPECTED_CHARACTER, c);
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
          throw new ExpressionException(start, ExpressionError.UNEXPECTED_CHARACTER, c);
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
                  throw new ExpressionException(start, ExpressionError.MISSING_END_BLOCK_COMMENT);
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
          throw new ExpressionException(start, ExpressionError.MISSING_END_DOUBLE_QUOTED_STRING);
        }

        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
        case '.': // Number without zero .1
        {
          int start = position;
          char previous;
          boolean error = false;

          if (c == '0' && position + 1 < source.length()) {
            c = source.charAt(position + 1);

            // Literal hexadecimal number 0xABC_DEF
            if (c == 'x' || c == 'X') {
              position += 2;
              previous = c;
              while (position < source.length()) {
                c = source.charAt(position);
                if (Characters.isHexDigit(c) || c == '_') {
                  position++;
                  if (c == '_' && c == previous)
                    error = true;
                  previous = c;
                } else {
                  break;
                }
              }

              String str = source.substring(start, position);
              // Empty, consecutive underscore or last char is underscore
              if (str.length() == 2 || previous == '_' || error) {
                throw new ExpressionException(position, ExpressionError.INVALID_NUMBER, str);
              }

              return new Token(Id.LITERAL_NUMERIC_HEXA, start, position, str.replace("_", ""));
            }

            // Literal binary number 0b0110_1011
            if (source.charAt(position + 1) == 'b' || source.charAt(position + 1) == 'B') {
              position += 2;
              previous = c;
              while (position < source.length()) {
                c = source.charAt(position);
                if (Characters.isBitDigit(c) || c == '_') {
                  position++;
                  if (c == '_' && c == previous)
                    error = true;
                  previous = c;
                } else {
                  break;
                }
              }

              String str = source.substring(start, position);
              // Empty, consecutive underscore or last char is underscore
              if (str.length() == 2 || previous == '_' || error) {
                throw new ExpressionException(position, ExpressionError.INVALID_NUMBER, str);
              }

              return new Token(Id.LITERAL_NUMERIC_BINARY, start, position, str.replace("_", ""));
            }

            // Literal octal number 0o1234567
            if (source.charAt(position + 1) == 'o' || source.charAt(position + 1) == 'O') {
              position += 2;
              previous = c;
              while (position < source.length()) {
                c = source.charAt(position);
                if (Characters.isOctDigit(c) || c == '_') {
                  position++;
                  if (c == '_' && c == previous)
                    error = true;
                  previous = c;
                } else {
                  break;
                }
              }

              String str = source.substring(start, position);
              // Empty, consecutive underscore or last char is underscore
              if (str.length() == 2 || previous == '_' || error) {
                throw new ExpressionException(position, ExpressionError.INVALID_NUMBER, str);
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
              if (c == '_' && c == previous)
                error = true;
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
              if (c == '_' && c == previous)
                error = true;
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
                if (Characters.isDigit(c))
                  digit++;
                if (c == '_' && c == previous)
                  error = true;
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
            throw new ExpressionException(position, ExpressionError.INVALID_NUMBER, str);
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
            if (Characters.isSpace(c) || "()/*%,^&><=~+-.!|$:[]\n\r".indexOf(c) >= 0)
              break;

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

