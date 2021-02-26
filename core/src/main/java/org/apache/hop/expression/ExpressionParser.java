/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.ParseException;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;
import org.apache.hop.expression.Token.Id;
import org.apache.hop.expression.util.Characters;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.i18n.BaseMessages;

public class ExpressionParser {

  private static final Class<?> PKG = IExpression.class; // for i18n purposes

  /** locale-neutral big decimal format. */
  //public static final DecimalFormat DECIMAL_FORMAT = new DecimalFormat("0.0b", new DecimalFormatSymbols(Locale.ENGLISH));

  /** The DateTimeFormatter for timestamps, "yyyy-MM-dd HH:mm:ss". */
  public static final DateTimeFormatter TIMESTAMP_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

  /** The DateTimeFormatter for timestamps, "yyyy-MM-dd HH:mm:ss.nnnnnn". */
  public static final DateTimeFormatter TIMESTAMP_FORMAT_NANO6 =
      DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.nnnnnn");

  /** The UTC time zone. */
  public static final ZoneId UTC_ZONE = ZoneId.of("UTC");

  private final String source;

  private List<Token> tokens = new ArrayList<>();
  private int index = 0;

  public static IExpression parse(String source) throws ExpressionException {
    ExpressionParser parser = new ExpressionParser(source);
    IExpression expression = parser.parse();

    return expression.optimize(new ExpressionContext());
  }

  protected ExpressionParser(String source) {
    super();
    this.source = source;
  }

  protected int getPosition() {

    if (index > 0 && index < tokens.size()) return this.next().start();

    return source.length();
  }

  protected boolean hasNext() {
    return index < tokens.size();
  }

  protected Token next() {
    if (!hasNext()) return null;
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

  private IExpression parse() throws ExpressionParserException {
    // System.out.println("Parse: " + source);

    if (StringUtils.isEmpty(source)) return Value.NULL;

    // Tokenize
    ExpressionScanner scanner = new ExpressionScanner(source);
    for (Token token = scanner.tokenize(); token != null; token = scanner.tokenize()) {

      // Ignore comment
      if (token.is(Id.COMMENT)) continue;

      // System.out.println("Token " + token);

      tokens.add(token);
    }

    IExpression expression = this.parseLogicalOr();

    if (hasNext()) {
      throw new ExpressionParserException(next().value(), source, this.getPosition());
    }

    return expression;
  }

  /**
   * Parse logical OR expression
   *
   * <p>LogicalXor ( OR LogicalXor )*
   */
  private IExpression parseLogicalOr() throws ExpressionParserException {
    IExpression expression = this.parseLogicalXor();
    while (next(Id.OR)) {
      expression = new ExpressionCall(Operator.BOOLOR, expression, parseLogicalXor());
    }

    return expression;
  }

  /**
   * Parse logical XOR expression
   *
   * <p>LogicalAnd ( XOR LogicalAnd )*
   *
   * @return Expression
   */
  private IExpression parseLogicalXor() throws ExpressionParserException {
    IExpression expression = this.parseLogicalAnd();
    while (next(Id.XOR)) {
      expression = new ExpressionCall(Operator.BOOLXOR, expression, parseLogicalAnd());
    }

    return expression;
  }

  /**
   * Parse logical AND expression
   *
   * <p>LogicalNot ( AND LogicalNot )*
   *
   * @return Expression
   */
  private IExpression parseLogicalAnd() throws ExpressionParserException {
    IExpression expression = this.parseLogicalNot();
    while (next(Id.AND)) {
      expression = new ExpressionCall(Operator.BOOLAND, expression, parseLogicalNot());
    }

    return expression;
  }

  /**
   * Parse logical NOT expression
   *
   * <p>[NOT] RelationalExpression
   */
  private IExpression parseLogicalNot() throws ExpressionParserException {

    if (next(Id.NOT)) {
      return new ExpressionCall(Operator.BOOLNOT, this.parseIs());
    }

    return this.parseIs();
  }

  /**
   * Parse IS expression
   *
   * <p>Basic [NOT] BasicExpression
   */
  private IExpression parseIs() throws ExpressionParserException {
    IExpression expression = this.parseRelational();
    if (next(Id.IS)) {
      boolean not = false;
      if (next(Id.NOT)) {
        not = true;
      }
      IExpression result = new ExpressionCall(Operator.IS, expression, this.parseLiteralBasic());
      if (not) return new ExpressionCall(Operator.BOOLNOT, result);
      return result;
    }
    return expression;
  }

 
  /**
   * ExponentExpression ( (* | / | %) ExponentExpression())*
   *
   * @return Expression
   */
  private IExpression parseFactor() throws ExpressionParserException {
    IExpression expression = this.parseBitwiseNot();

    while (hasNext()) {
      if (next(Id.MULTIPLY)) {
        expression = new ExpressionCall(Operator.MULTIPLY, expression, this.parseBitwiseNot());
      } else if (next(Id.DIVIDE)) {
        expression = new ExpressionCall(Operator.DIVIDE, expression, this.parseBitwiseNot());
      } else if (next(Id.MODULUS)) {
        expression = new ExpressionCall(Operator.MODULUS, expression, this.parseBitwiseNot());
      } else break;
    }

    return expression;
  }

  /** ( UnaryExpression)* */
  private IExpression parseBitwiseNot() throws ExpressionParserException {
    if (next(Id.BITWISE_NOT)) {
      return new ExpressionCall(Operator.BITNOT, this.parseUnary());
    }
    return this.parseUnary();
  }

  /** UnaryExpression ( & UnaryExpression)* */
  private IExpression parseBitwiseAnd() throws ExpressionParserException {
    IExpression expression = this.parseFactor();
    if (next(Id.BITWISE_AND)) {
      return new ExpressionCall(Operator.BITAND, expression, this.parseFactor());
    }
    return expression;
  }

  /** UnaryExpression ( | UnaryExpression)* */
  private IExpression parseBitwiseOr() throws ExpressionParserException {
    IExpression expression = this.parseBitwiseXor();
    if (next(Id.BITWISE_OR)) {
      return new ExpressionCall(Operator.BITOR, expression, this.parseBitwiseXor());
    }
    return expression;
  }

  /** UnaryExpression ( ^ UnaryExpression)* */
  private IExpression parseBitwiseXor() throws ExpressionParserException {
    IExpression expression = this.parseBitwiseAnd();
    if (next(Id.BITWISE_XOR)) {
      return new ExpressionCall(Operator.BITXOR, expression, this.parseBitwiseAnd());
    }
    return expression;
  }

  /** (('+' | '-') PrimaryExpression ) */
  private IExpression parseUnary() throws ExpressionException {

    if (next(Id.MINUS)) {
      return new ExpressionCall(Operator.NEGATIVE, this.parseCast());
    }
    if (next(Id.PLUS)) {
      // Ignore
    }

    return this.parseCast();
  }

  /** Literal TRUE | FALSE | NULL */
  private IExpression parseLiteralBasic() throws ExpressionParserException {

    Token token = next();

    if (token == null)
      throw new ExpressionParserException(
          BaseMessages.getString(PKG, "ExpressionParser.UnexpectedEndOfExpression"),
          source,
          this.getPosition());

    switch (token.id()) {
      case TRUE:
        return Value.of(true);
      case FALSE:
        return Value.of(false);
      case NULL:
        return Value.NULL;
      default:
        throw new ExpressionParserException(" ERROR2 ", source, token.start());
    }
  }

  /** Literal text */
  private IExpression parseLiteralText(Token token) throws ExpressionParserException {
    return Value.of(token.value());
  }

  /** Term = Literal | Identifier | Function | '(' Expression ')' */
  private IExpression parseTerm() throws ExpressionParserException {
    Token token = next();

    if (token != null) {
      switch (token.id()) {
        case CAST:
        case TRUE:
          return Value.TRUE;
        case FALSE:
          return Value.FALSE;
        case NULL:
          return Value.NULL;
        case IDENTIFIER:
          return new ExpressionIdentifier(token.value());
        case LITERAL_STRING:
          return parseLiteralText(token);
        case LITERAL_NUMBER:
          return parseLiteralNumber(token);
        case LITERAL_BINARY_HEX:
          return parseLiteralBinaryHexa(token);
        case LITERAL_BINARY_BIT:
          return parseLiteralBinaryBit(token);
        case DATE:
          return parseLiteralDate();
        case TIME:
          return parseLiteralTime();
        case TIMESTAMP:
          return parseLiteralTimestamp();
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
          throw new ExpressionParserException(
              BaseMessages.getString(PKG, "ExpressionParser.UnbalancedParenthesis"),
              source,
              this.getPosition());
        default:
          // Error
      }
    }
    throw new ExpressionParserException(
        BaseMessages.getString(PKG, "ExpressionParser.UnexpectedEndOfExpression"),
        source,
        this.getPosition());
  }

  /** AdditiveExpression ( Operator AdditiveExpression | InClause | BetweenClause | LikeClause */
  private IExpression parseRelational() throws ExpressionParserException {
    IExpression expression = this.parseAdditive();

    if (next(Id.EQUAL)) {
      return new ExpressionCall(Operator.EQUALS, expression, this.parseAdditive());
    }
    if (next(Id.NOT_EQUAL)) {
      return new ExpressionCall(Operator.NOT_EQUALS, expression, this.parseAdditive());
    }
    if (next(Id.LESS_THAN_OR_GREATER_THAN)) {
      return new ExpressionCall(
          Operator.LESS_THAN_OR_GREATER_THAN, expression, this.parseAdditive());
    }
    if (next(Id.GREATER_THAN)) {
      return new ExpressionCall(Operator.GREATER_THAN, expression, this.parseAdditive());
    }
    if (next(Id.GREATER_THAN_OR_EQUAL)) {
      return new ExpressionCall(Operator.GREATER_THAN_OR_EQUAL, expression, this.parseAdditive());
    }
    if (next(Id.LESS_THAN)) {
      return new ExpressionCall(Operator.LESS_THAN, expression, this.parseAdditive());
    }
    if (next(Id.LESS_THAN_OR_EQUAL)) {
      return new ExpressionCall(Operator.LESS_THAN_OR_EQUAL, expression, this.parseAdditive());
    }

    // Special case NOT before operation: <exp> [NOT] LIKE|IN|BETWEEN <primaryExp>
    boolean not = false;
    if (next(Id.NOT)) {
      not = true;
    }

    if (next(Id.LIKE)) {
      IExpression pattern = this.parseCast();

      if (next(Id.ESCAPE)) {
        IExpression escape = this.parseCast();
        expression = new ExpressionCall(Operator.LIKE, expression, pattern, escape);
      } else expression = new ExpressionCall(Operator.LIKE, expression, pattern);
    } else if (next(Id.ILIKE)) {
      IExpression pattern = this.parseCast();

      if (next(Id.ESCAPE)) {
        IExpression escape = this.parseCast();
        expression = new ExpressionCall(Operator.ILIKE, expression, pattern, escape);
      } else expression = new ExpressionCall(Operator.ILIKE, expression, pattern);
    } else if (next(Id.IN)) {
      expression = new ExpressionCall(Operator.IN, expression, this.parseList());
    } else if (next(Id.BETWEEN)) {
      IExpression begin = this.parseCast();
      if (!next(Id.AND)) {
        throw new ExpressionParserException(
            BaseMessages.getString(PKG, "ExpressionParser.InvalidOperator", Id.BETWEEN),
            source,
            this.getPosition());
      }
      IExpression end = this.parseCast();

      expression = new ExpressionCall(Operator.BETWEEN, expression, begin, end);
    }

    if (not) {
      return new ExpressionCall(Operator.BOOLNOT, expression);
    }

    return expression;
  }

  /** BitwiseExpression ( (+ | - | ||) BitwiseOrExpression )* */
  private IExpression parseAdditive() throws ExpressionParserException {
    IExpression expression = this.parseBitwiseOr();
    while (hasNext()) {
      if (next(Id.PLUS)) {
        expression = new ExpressionCall(Operator.ADD, expression, this.parseBitwiseOr());
      } else if (next(Id.MINUS)) {
        expression = new ExpressionCall(Operator.SUBTRACT, expression, this.parseBitwiseOr());
      } else if (next(Id.CONCAT)) {
        expression = new ExpressionCall(Operator.CONCAT, expression, this.parseBitwiseOr());
      } else break;
    }

    return expression;
  }

  private Value parseLiteralNumber(Token token) throws ExpressionParserException {
    boolean isDecimalPartFound = false;
    boolean isDecimalSeparatorFound = false;
    boolean isExponentSymbolFound = false;

    String text = token.value();

    try {

      int length = text.length();
      int pos = 0;

      while (pos < length && Characters.isDigit(text.charAt(pos))) {
        pos++;
      }

      // Use dot for decimal separator
      if (pos < length && text.charAt(pos) == '.') {
        isDecimalSeparatorFound = true;
        pos++;
      }
      while (pos < length && Characters.isDigit(text.charAt(pos))) {
        pos++;
        isDecimalPartFound = true;
      }
      if (pos < length && Characters.isExponentChar(text.charAt(pos))) {
        pos++;
        isExponentSymbolFound = true;
      }
      if (pos < length
          && (source.charAt(pos) == '+' || text.charAt(pos) == '-')
          && isExponentSymbolFound) {
        pos++;
      }
      while (pos < length && Character.isDigit(source.charAt(pos)) && isExponentSymbolFound) {
        pos++;
      }

      if (length < 18 && isDecimalSeparatorFound == false) {
        return Value.of(Long.parseLong(text, 10));
      }

      return Value.of(new BigDecimal(text));
    } catch (Exception e) {
      throw new ExpressionParserException(
          BaseMessages.getString(PKG, "Expression.InvalidNumeric", text),
          source,
          this.getPosition());
    }
  }

  private Value parseLiteralBinaryHexa(Token token) throws ExpressionParserException {

    String s = token.value();

    if (s.length() % 2 > 0) s = '0' + s;

    byte[] bytes = new byte[s.length() / 2];

    for (int i = 0; i < bytes.length; i++) {
      int index = i * 2;
      bytes[i] = (byte) Integer.parseInt(s.substring(index, index + 2), 16);
    }

    if (bytes.length <= 8) {
      // Value as integer if less than or equals 8 bytes
      long result = 0;
      for (int i = 0; i < bytes.length; i++) {
        result = result << 8;
        result = result | (bytes[i] & 0xFF);
      }
      return Value.of(result);
    }

    return Value.of(bytes);
  }

  private Value parseLiteralBinaryBit(Token token) throws ExpressionParserException {

    String s = token.value();
    BitSet bitset = new BitSet(s.length());

    int length = s.length();
    for (int i = length - 1; i >= 0; i--) {
      if (s.charAt(i) == '1') {
        bitset.set(length - i - 1);
      }
    }

    if (bitset.length() <= 32) {
      // Value as integer if less than or equals 32 bits
      return Value.of(bitset.toLongArray()[0]);
    }

    return Value.of(bitset.toByteArray());
  }

  /**
   * Parses a date literal. The parsing is strict and requires months to be less than 12, days to be
   * less than 31, etc.
   */
  private Value parseLiteralDate() throws ExpressionParserException {

    // The real literal text DATE 'literal'
    Token token = next();

    Instant instant;
    try {
      instant = DateTimeFormat.parse(token.value(), "YYYY-MM-DD");
    } catch (ParseException e) {
      throw new ExpressionParserException(
          BaseMessages.getString(PKG, "Expression.InvalidDate", token.value()),
          source,
          this.getPosition());
    }

    return Value.of(instant);
  }

  /** Parses a time literal. */
  private Value parseLiteralTime() throws ExpressionParserException {

    try {
      // The real literal text TIME 'literal'
      Token token = next();

      DateTimeFormatter format = DateTimeFormatter.ISO_TIME;
      //		if ( token.getLength()==16 )
      //			format = TIME_FORMAT_NANO6;
      //		else if ( token.getLength()==17 )
      //			format = TIME_FORMAT_NANO7;
      //
      LocalTime time = LocalTime.parse(token.value(), format);

      // System.out.println(token.getText() + " parse to " + time.toString());

      LocalDateTime datetime = LocalDateTime.of(LocalDate.of(1900, 1, 1), time);
      return Value.of(datetime.toInstant(ZoneOffset.UTC));
    } catch (Exception e) {
      throw new ExpressionParserException(
          BaseMessages.getString(PKG, "Expression.InvalidTime"), source, this.getPosition());
    }
  }

  /**
   * Parses a date literal. The parsing is strict and requires months to be less than 12, days to be
   * less than 31, etc.
   */
  private Value parseLiteralTimestamp() throws ExpressionParserException {

    try {
      // The real literal text TIMESTAMP 'literal'
      Token token = next();

      DateTimeFormatter format = TIMESTAMP_FORMAT;
      if (token.value().length() == 26) format = TIMESTAMP_FORMAT_NANO6;

      LocalDateTime datetime = LocalDateTime.parse(token.value(), format);

      // System.out.println(token.getText()+" parse to "+datetime.toString() );

      return Value.of(datetime.toInstant(ZoneOffset.UTC));
    } catch (Exception e) {
      throw new ExpressionParserException(
          BaseMessages.getString(PKG, "Expression.InvalidTimestamp"), source, this.getPosition());
    }
  }

  /** Parses a list of expressions separated by commas. */
  private ExpressionList parseList() throws ExpressionParserException {

    // Token token = tokenizer.next();

    List<IExpression> list = new ArrayList<>();

    // System.out.println("Parse expression list: " + token);
    if (next(Id.LPARENTHESIS)) {

      Token token;
      do {
        list.add(parseCast());

        token = next();
        if (token == null)
          throw new ExpressionParserException(
              BaseMessages.getString(PKG, "ExpressionParser.MissingRightParenthesis"),
              source,
              this.getPosition());
        if (token.is(Id.COMMA)) continue;

        if (token.is(Id.RPARENTHESIS)) return new ExpressionList(list);

      } while (token != null);
    }

    throw new ExpressionParserException(
        BaseMessages.getString(PKG, "ExpressionParser.InvalidExpressionList"),
        source,
        this.getPosition());
  }

  /** Case When Then Else End ) */
  private IExpression parseCaseWhen() throws ExpressionException {
    IExpression valueExpression = null;
    IExpression elseExpression = null;
    List<IExpression> whenList = new ArrayList<>();
    List<IExpression> thenList = new ArrayList<>();

    // Form Switch value
    if (!is(Id.WHEN)) {
      valueExpression = this.parseLogicalOr();
    }

    // Form mutli boolean condition
    while (next(Id.WHEN)) {
      whenList.add(this.parseLogicalOr());
      if (!next(Id.THEN)) {
        throw new ExpressionParserException(
            BaseMessages.getString(PKG, "ExpressionParser.InvalidOperator", Id.CASE),
            source,
            this.getPosition());
      }
      thenList.add(this.parseLogicalOr());
    }

    if (!next(Id.ELSE)) {
      throw new ExpressionParserException(
          BaseMessages.getString(PKG, "ExpressionParser.InvalidOperator", Id.CASE),
          source,
          this.getPosition());
    }
    elseExpression = this.parseLogicalOr();

    if (!next(Id.END)) {
      throw new ExpressionParserException(
          BaseMessages.getString(PKG, "ExpressionParser.InvalidOperator", Id.CASE),
          source,
          this.getPosition());
    }

    return new ExpressionCall(
        Operator.CASE,
        valueExpression,
        new ExpressionList(whenList),
        new ExpressionList(thenList),
        elseExpression);
  }

  /**
   * Cast operator :: 
   *
   */
  private IExpression parseCast() throws ExpressionParserException {
    IExpression expression = this.parseTerm();
    if (next(Id.CAST)) {
      DataType type = parseDataType();
      
      // Use Enum.ordinal as argument for evaluate performance
      Value targetType = Value.of(type.ordinal());
      
      IExpression result = new ExpressionCall(Operator.CAST, expression, targetType);
     
      return result;
    }
    return expression;
  }
  
  
  /** Function */
  private IExpression parseFunction(Token token) throws ExpressionParserException {

    Function function = Function.getFunction(token.value());
    List<IExpression> operands = new ArrayList<>();

    if (is(Id.LPARENTHESIS)) token = next();
    else {
      throw new ExpressionParserException(
          BaseMessages.getString(PKG, "ExpressionParser.MissingLeftParenthesis"),
          source,
          this.getPosition());
    }

    /** Cast(expression AS dataType FORMAT pattern) */
    if (Kind.CAST == function.kind) {

      operands.add(this.parseLogicalOr());

      if (!next(Id.AS)) {
        throw new ExpressionParserException(
            BaseMessages.getString(PKG, "ExpressionParser.InvalidFunctionCastAs"),
            source,
            this.getPosition());
      }

      DataType type = parseDataType();
      
      // Use Enum.ordinal as argument for evaluate performance
      operands.add(Value.of(type.ordinal()));

      if (is(Id.FORMAT)) {
        next();
        operands.add(this.parseLiteralText(next()));
      }
    }

    /** Extract(datePart FROM expression) */
    else if (Kind.EXTRACT == function.kind) {
      
      DatePart part = DatePart.of(next().value());

      // Replace EXTRACT with the corresponding function
      switch (part) {
        case YEAR:
        case MONTH:
        case QUARTER:
        case DAY:
        case HOUR:
        case MINUTE:
        case SECOND:
        case WEEK:
        case DAYOFYEAR:
        case DAYOFWEEK:
          function = Function.getFunction(part.name());
          break;
        default:
          function = Function.getFunction(Kind.EXTRACT);
          // Use Enum.ordinal as argument for evaluate performance
          operands.add(Value.of(part.ordinal()));
          break;
      }

      if (!next(Id.FROM)) {
        throw new ExpressionParserException(
            BaseMessages.getString(PKG, "ExpressionParser.InvalidFunctionExtractFrom"),
            source,
            this.getPosition());
      }

      operands.add(this.parseLogicalOr());

    } else {

      // No param function
      if (is(Id.RPARENTHESIS)) {
        next();

        if (!function.checkNumberOfArguments(operands.size())) {
          throw new ExpressionParserException(
              BaseMessages.getString(
                  PKG, "ExpressionParser.InvalidNumberOfArguments", function.getName()),
              source,
              token.start());
        }

        return new ExpressionCall(function, operands);
      }

      operands.add(this.parseLogicalOr());

      while (is(Id.COMMA)) {
        token = next();
        operands.add(this.parseLogicalOr());
      }
    }

    if (is(Id.RPARENTHESIS)) {
      token = next();
    } else {
      throw new ExpressionParserException(
          BaseMessages.getString(PKG, "ExpressionParser.MissingRightParenthesis"),
          source,
          token.start());
    }

    if (!function.checkNumberOfArguments(operands.size())) {
      throw new ExpressionParserException(
          BaseMessages.getString(
              PKG, "ExpressionParser.InvalidNumberOfArguments", function.getName()),
          source,
          token.start());
    }

    return new ExpressionCall(function, operands);
  }

  private DataType parseDataType() {
    Token token = next();
    if ( token==null) {
      throw new ExpressionParserException(
          BaseMessages.getString(PKG, "ExpressionParser.MissingDataType"),
          source,
          this.getPosition());
    }
    
    try {
      return DataType.of(token.value());
    } catch (RuntimeException e) {
      throw new ExpressionParserException(
          BaseMessages.getString(PKG, "ExpressionParser.InvalidDataType", token.value()),
          source,
          token.start());
    }
  }
}
