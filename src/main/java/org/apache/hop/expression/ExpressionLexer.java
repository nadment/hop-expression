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

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import lombok.Getter;
import org.apache.hop.expression.Token.Id;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.util.Characters;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Lazy expression lexer.
 *
 * <p>The tokens are read from the source only when the parser needs them, and kept so that the
 * parser can look at the next token without reading it, or go back with {@link #previous()}.
 *
 * <p>The methods follow the vocabulary of a recursive descent parser: {@code isNext} looks at the
 * next token, {@code accept} reads it only if it matches, {@code expect} reads it or raises an
 * error.
 */
@NullMarked
public class ExpressionLexer {

  @Getter private final String source;
  private final List<Token> tokens;

  /** Position in the source */
  private int position = 0;

  /** Index of the current token */
  private int index = 0;

  /**
   * Creates a lexer for the source of an expression.
   *
   * @param source the source of the expression
   * @throws ParseExpressionException if the source is null
   */
  public ExpressionLexer(@Nullable String source) {
    if (source == null) throw new ParseExpressionException(0, ErrorCode.NULL_SOURCE_ERROR);
    this.source = source;
    this.tokens = new ArrayList<>();
  }

  /**
   * Gets the position in the source of the next token, used to locate a syntax error.
   *
   * @return the start of the next token, or the length of the source if all the tokens are read
   */
  public int getPosition() {
    if (index >= 0 && index < tokens.size()) return tokens.get(index).start();
    return source.length();
  }

  /** Gets the next token without reading it, the token is created from the source if needed. */
  private @Nullable Token peek() throws ExpressionException {
    while (index >= tokens.size()) {
      Token token = scanToken();
      if (token == null) return null;
      tokens.add(token);
    }
    return tokens.get(index);
  }

  /**
   * Returns whether there is a next token.
   *
   * @return true if the end of the source is not reached
   * @throws ExpressionException if the next token is not valid
   */
  public boolean hasNext() throws ExpressionException {
    return peek() != null;
  }

  /**
   * Raises an error if there is no next token, the next token is not read.
   *
   * @param errorCode the error raised at the end of the source
   * @param values the values of the error message
   * @throws ExpressionException if the end of the source is reached or the next token is not valid
   */
  public void expectNext(ErrorCode errorCode, Object... values) throws ExpressionException {
    Token token = peek();
    if (token == null) {
      throw new ParseExpressionException(position, errorCode, values);
    }
  }

  /**
   * Reads the next token.
   *
   * @return the next token
   * @throws ExpressionException if the end of the source is reached, which is an internal error
   *     because the caller checks {@link #hasNext()} first, or if the next token is not valid
   * @see #nextOrThrows(ErrorCode, Object...)
   */
  public Token next() throws ExpressionException {
    Token token = peek();
    if (token == null) {
      throw new ParseExpressionException(
          getPosition(), ErrorCode.INTERNAL_ERROR, "Unexpected end of expression");
    }

    index++;
    return token;
  }

  /**
   * Goes back to the token read last, it becomes the next token again.
   *
   * @return the token read last
   */
  public Token previous() {
    return tokens.get(--index);
  }

  /**
   * Returns whether the next token is of the specified type, the next token is not read.
   *
   * @param id the type of token
   * @return true if there is a next token of this type
   * @throws ExpressionException if the next token is not valid
   */
  public boolean isNext(Id id) throws ExpressionException {
    Token token = peek();
    return token != null && token.is(id);
  }

  /**
   * Reads the next token only if it is of the specified type.
   *
   * @param id the type of token
   * @return true if the token is read, false if the next token is of another type or there is no
   *     next token
   * @throws ExpressionException if the next token is not valid
   */
  public boolean accept(Id id) throws ExpressionException {
    if (isNext(id)) {
      index++;
      return true;
    }
    return false;
  }

  /**
   * Reads the next token only if it is of the specified type, like {@link #accept(Id)}, for a token
   * that must be followed by another one, like an operator before its operand.
   *
   * @param id the type of token
   * @return true if the token is read, false if the next token is of another type or there is no
   *     next token
   * @throws ExpressionException if the token is read but is the last one of the source, the error
   *     {@link ErrorCode#SYNTAX_ERROR} is raised at the position of the token
   */
  public boolean acceptAndNotEnd(Id id) throws ExpressionException {
    Token token = peek();
    if (token != null && token.is(id)) {
      index++;
      if (!hasNext()) {
        throw new ParseExpressionException(token.start(), ErrorCode.SYNTAX_ERROR, id);
      }
      return true;
    }
    return false;
  }

  /**
   * Reads the next token whatever its type, like {@link #next()} but with the error of the caller
   * at the end of the source.
   *
   * @param errorCode the error raised at the end of the source
   * @param values the values of the error message
   * @return the next token
   * @throws ExpressionException if the end of the source is reached or the next token is not valid
   */
  public Token nextOrThrows(ErrorCode errorCode, Object... values) throws ExpressionException {
    Token token = peek();
    if (token == null) {
      throw new ParseExpressionException(getPosition(), errorCode, values);
    }
    index++;
    return token;
  }

  /**
   * Reads the next token, it must be of the specified type.
   *
   * @param id the type of token
   * @param errorCode the error raised if the next token is of another type or there is no next
   *     token
   * @param values the values of the error message
   * @throws ExpressionException if the next token is not of this type, the error is raised at the
   *     position of the next token, or at the end of the source
   */
  public void expect(Id id, ErrorCode errorCode, Object... values) throws ExpressionException {
    Token token = peek();
    if (token != null && token.is(id)) {
      index++;
      return;
    }
    throw new ParseExpressionException(token != null ? token.start() : position, errorCode, values);
  }

  /**
   * Returns whether the next token is the reserved word (ignore case), the next token is not read.
   *
   * <p>A word registered by a function has the generic token {@link Id#KEYWORD}, a core word of the
   * grammar has its own token. A quoted identifier with the same text is not the reserved word.
   *
   * @param word the reserved word
   * @return true if the next token is the reserved word
   * @throws ExpressionException if the next token is not valid
   */
  public boolean isNextKeyword(String word) throws ExpressionException {
    Token token = peek();
    if (token == null) return false;
    String upper = word.toUpperCase(Locale.ROOT);
    if (token.is(Id.KEYWORD)) return token.text().equals(upper);
    return Keyword.isCore(upper) && token.id().name().equals(upper);
  }

  /**
   * Reads the next token only if it is the reserved word (ignore case).
   *
   * @param word the reserved word
   * @return true if the token is read, false if the next token is another one or there is no next
   *     token
   * @throws ExpressionException if the next token is not valid
   * @see #isNextKeyword(String)
   */
  public boolean acceptKeyword(String word) throws ExpressionException {
    if (isNextKeyword(word)) {
      index++;
      return true;
    }
    return false;
  }

  /**
   * Reads the next token, it must be the reserved word (ignore case).
   *
   * @param word the reserved word
   * @param errorCode the error raised if the next token is another one or there is no next token
   * @param values the values of the error message
   * @throws ExpressionException if the next token is not the reserved word, the error is raised at
   *     the position of the next token, or at the end of the source
   * @see #isNextKeyword(String)
   */
  public void expectKeyword(String word, ErrorCode errorCode, Object... values)
      throws ExpressionException {
    if (!acceptKeyword(word)) {
      Token token = peek();
      throw new ParseExpressionException(
          token != null ? token.start() : position, errorCode, values);
    }
  }

  /**
   * Tokenize the source string.
   *
   * <p>The tokens are read from the current position in the source, they are not kept for the other
   * methods. Call it on a new lexer, not on a lexer used by a parser.
   *
   * @return The list of tokens.
   * @throws ExpressionException if a token is not valid
   */
  public List<Token> getTokens() throws ExpressionException {
    List<Token> list = new ArrayList<>();
    for (Token token = scanToken(); token != null; token = scanToken()) {
      list.add(token);
    }
    return list;
  }

  /**
   * Returns the next token.
   *
   * @return The next token, or null if the end of the source.
   */
  private @Nullable Token scanToken() throws ExpressionException {
    skipSpacesAndComments();
    if (position >= source.length()) {
      return null;
    }

    return switch (source.charAt(position)) {
      case ',' -> new Token(Id.COMMA, position++);
      case '(' -> new Token(Id.LPARENTHESIS, position++);
      case ')' -> new Token(Id.RPARENTHESIS, position++);
      case '[' -> new Token(Id.LBRACKET, position++);
      case ']' -> new Token(Id.RBRACKET, position++);
      case '=' -> new Token(Id.EQUAL, position++);
      case '+' -> new Token(Id.PLUS, position++);
        // Minus sign, a comment -- is already skipped
      case '-' -> new Token(Id.MINUS, position++);
      case '*' -> new Token(Id.STAR, position++);
      case '%' -> new Token(Id.PERCENT, position++);
      case '~' -> new Token(Id.TILDE, position++);
      case '&' -> new Token(Id.AMPERSAND, position++);
      case '^' -> new Token(Id.CARET, position++);
      case '<' -> scanLessThan();
      case '>' -> scanOperator(Id.GT, '=', Id.GTE);
        // Bitwise OR operator or concat symbol
      case '|' -> scanOperator(Id.PIPE, '|', Id.CONCAT);
        // A comment /* or // is already skipped
      case '/' -> new Token(Id.SLASH, position++);
        // Bang equal symbol "!="
      case '!' -> scanPair('=', Id.NOT_EQUAL);
        // Cast operator
      case ':' -> scanPair(':', Id.CAST);
        // Single-quoted literal text
      case '\'' -> scanQuotedString();
        // Quoted identifier matching reserved words or with white space
      case '"' -> scanQuotedIdentifier();
        // Number, also without zero .1
      case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.' -> scanNumber();
      default -> scanWord();
    };
  }

  /** Skips the white spaces, the line comments -- and //, and the block comments. */
  private void skipSpacesAndComments() throws ExpressionException {
    while (position < source.length()) {
      char c = source.charAt(position);
      if (Characters.isSpace(c) || c == '\n' || c == '\r') {
        position++;
      } else if ((c == '-' || c == '/') && isChar(position + 1, c)) {
        skipLineComment();
      } else if (c == '/' && isChar(position + 1, '*')) {
        skipBlockComment();
      } else {
        return;
      }
    }
  }

  /**
   * Returns whether the source has the specified character at the specified position.
   *
   * @param index the position in the source, may be after the end
   * @param ch the character
   * @return true if the source has this character at this position
   */
  private boolean isChar(int index, char ch) {
    return index < source.length() && source.charAt(index) == ch;
  }

  /**
   * Reads the specified character only if it is at the current position.
   *
   * @param ch the character
   * @return true if the character is read
   */
  private boolean acceptChar(char ch) {
    if (isChar(position, ch)) {
      position++;
      return true;
    }
    return false;
  }

  /** Skips a line comment, up to the end of the line. */
  private void skipLineComment() {
    position += 2;
    while (position < source.length()) {
      char c = source.charAt(position);
      if (c == '\r' || c == '\n') break;
      position++;
    }
  }

  /** Skips a block comment, which can be nested. */
  private void skipBlockComment() throws ExpressionException {
    int start = position++;
    int level = 1;

    while (level > 0) {
      int end = source.indexOf('*', position + 1);
      if (end <= 0 || end >= source.length() - 1) {
        throw new ParseExpressionException(start, ErrorCode.MISSING_END_BLOCK_COMMENT);
      }
      // nested block comment
      if (source.charAt(end - 1) == '/') {
        level++;
        position = end;
      } else if (source.charAt(end + 1) == '/') {
        level--;
        position = end + 2;
      } else {
        position++;
      }
    }
  }

  /** Parses the less symbol, the less or equal symbol "<=" or the not equal symbol "<>". */
  private Token scanLessThan() {
    int start = position++;
    if (acceptChar('=')) {
      return new Token(Id.LTE, start);
    }
    if (acceptChar('>')) {
      return new Token(Id.NOT_EQUAL, start);
    }
    return new Token(Id.LT, start);
  }

  /**
   * Parses an operator of one character, or of two characters if followed by the specified one.
   *
   * @param id the type of the operator of one character
   * @param next the second character of the operator of two characters
   * @param longId the type of the operator of two characters
   * @return the token of the operator
   */
  private Token scanOperator(Id id, char next, Id longId) {
    int start = position++;
    return new Token(acceptChar(next) ? longId : id, start);
  }

  /**
   * Parses an operator of two characters whose first character is not valid alone.
   *
   * @param next the second character of the operator
   * @param id the type of the operator
   * @return the token of the operator
   * @throws ExpressionException if the first character is not followed by the second one
   */
  private Token scanPair(char next, Id id) throws ExpressionException {
    int start = position++;
    if (acceptChar(next)) {
      return new Token(id, start);
    }
    throw new ParseExpressionException(start, ErrorCode.UNEXPECTED_CHARACTER, source.charAt(start));
  }

  /** Parses a single-quoted literal text, two consecutive single-quotes are one single-quote. */
  private Token scanQuotedString() throws ExpressionException {
    StringBuilder text = new StringBuilder();
    int start = position++;
    while (position < source.length()) {
      char c = source.charAt(position++);
      // A single-quote ends the text, unless followed by another one
      if (c == '\'' && !acceptChar('\'')) {
        return new Token(Id.LITERAL_STRING, start, position, text.toString());
      }
      text.append(c);
    }

    // The end of the source is reached without the end single-quote
    throw new ParseExpressionException(start, ErrorCode.MISSING_END_SINGLE_QUOTED_STRING);
  }

  /** Parses a double-quoted identifier, matching reserved words or with white space. */
  private Token scanQuotedIdentifier() throws ExpressionException {
    int start = position;
    int end = source.indexOf('"', start + 1);
    if (end < 0) {
      throw new ParseExpressionException(start, ErrorCode.MISSING_END_DOUBLE_QUOTED_STRING);
    }
    position = end + 1;
    String value = source.substring(start + 1, end).toUpperCase(Locale.ROOT);
    return new Token(Id.IDENTIFIER, start, position, value);
  }

  /** Parses a literal number, decimal or with a prefix 0x, 0b or 0o. */
  private Token scanNumber() throws ExpressionException {
    if (source.charAt(position) == '0' && position + 1 < source.length()) {
      switch (source.charAt(position + 1)) {
          // Literal hexadecimal numeric 0xABC_DEF
        case 'x', 'X' -> {
          return scanRadixNumber(Id.LITERAL_NUMERIC_HEXA, Characters::isHexDigit);
        }
          // Literal binary numeric 0b0110_1011
        case 'b', 'B' -> {
          return scanRadixNumber(Id.LITERAL_NUMERIC_BINARY, Characters::isBitDigit);
        }
          // Literal octal numeric 0o1234567
        case 'o', 'O' -> {
          return scanRadixNumber(Id.LITERAL_NUMERIC_OCTAL, Characters::isOctDigit);
        }
        default -> {
          // A decimal number starting with zero
        }
      }
    }
    return scanDecimalNumber();
  }

  /** A test on a character, without boxing it. */
  @FunctionalInterface
  private interface CharPredicate {
    boolean test(char ch);
  }

  /**
   * Parses a literal number with a prefix of two characters, like 0x.
   *
   * @param id the type of the literal number
   * @param isDigit the test of a digit of the number
   * @return the token of the number, without the underscores
   * @throws ExpressionException if there is no digit, two consecutive underscores or a trailing
   *     underscore
   */
  private Token scanRadixNumber(Id id, CharPredicate isDigit) throws ExpressionException {
    int start = position;
    position += 2;
    char previous = source.charAt(start + 1);
    boolean error = false;
    while (position < source.length()) {
      char c = source.charAt(position);
      if (!isDigit.test(c) && c != '_') break;
      if (c == '_' && previous == '_') error = true;
      previous = c;
      position++;
    }

    String str = source.substring(start, position);
    // Empty, consecutive underscore or last char is underscore
    if (str.length() == 2 || previous == '_' || error) {
      throw new ParseExpressionException(position, ErrorCode.INVALID_NUMBER, str);
    }

    return new Token(id, start, position, str.replace("_", ""));
  }

  /** Parses a literal decimal number, with an optional decimal part and exponent. */
  private Token scanDecimalNumber() throws ExpressionException {
    int start = position;

    // Integer part, empty for a number without zero .1
    boolean valid = skipDigits(false);

    // Use dot for decimal separator. The &= scans the whole number before raising the error.
    if (acceptChar('.')) {
      valid &= skipDigits(true);
    }

    // Exponentiation part
    if (position < source.length() && Characters.isExponent(source.charAt(position))) {
      position++;
      if (!acceptChar('+')) {
        acceptChar('-');
      }
      valid &= skipDigits(true);
    }

    String str = source.substring(start, position);
    // Empty, consecutive underscore or last char is underscore
    if (str.isEmpty() || !valid) {
      throw new ParseExpressionException(position, ErrorCode.INVALID_NUMBER, str);
    }

    // Literal decimal number
    return new Token(Id.LITERAL_NUMERIC_DECIMAL, start, position, str.replace("_", ""));
  }

  /**
   * Skips decimal digits separated by single underscores.
   *
   * @param required true if at least one digit is required, then an underscore is not allowed first
   * @return false if there are two consecutive underscores, a trailing underscore, or no digit when
   *     required
   */
  private boolean skipDigits(boolean required) {
    char previous = required ? '_' : '0';
    boolean valid = true;
    while (position < source.length()) {
      char c = source.charAt(position);
      if (!Characters.isDigit(c) && c != '_') break;
      if (c == '_' && previous == '_') valid = false;
      previous = c;
      position++;
    }
    return valid && previous != '_';
  }

  /** Parses a function name, a reserved word, a data type, a time unit or an identifier. */
  private Token scanWord() {
    // Probably a letter or digit. Start an identifier.
    // Other characters, e.g. *, ! are also included
    // in identifiers.
    int start = position++;
    while (position < source.length()) {
      char c = source.charAt(position);
      if (Characters.isSpace(c) || "()/*%,^&><=~+-.!|$:[]\n\r".indexOf(c) >= 0) break;

      position++;
    }

    String identifier = source.substring(start, position);
    String name = identifier.toUpperCase(Locale.ROOT);

    // Function is follow by a parenthesis
    if (isChar(position, '(')) {
      return new Token(Id.FUNCTION, start, position, name);
    }

    // Reserved words: AS, AND, LIKE, NOT, TRUE, FALSE, OR
    if (Keyword.isCore(name)) {
      return new Token(Id.valueOf(name), start, position, name);
    }

    // Reserved words registered by the functions with an extended syntax
    if (Keyword.is(name)) {
      return new Token(Id.KEYWORD, start, position, name);
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
