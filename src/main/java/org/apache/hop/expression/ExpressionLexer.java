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
import java.util.Set;
import lombok.Getter;
import org.apache.hop.expression.Token.Id;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.util.Characters;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** Lazy expression lexer. */
@NullMarked
public class ExpressionLexer {

  private static final Set<String> RESERVED_WORDS =
      Set.of(
          "AND",
          "AS",
          "ASYMMETRIC",
          "AT",
          "BETWEEN",
          "BINARY",
          "CASE",
          "DATE",
          "DISTINCT",
          "ELSE",
          "END",
          "ESCAPE",
          "FALSE",
          "FORMAT",
          "FROM",
          "IGNORE",
          "ILIKE",
          "IN",
          "INET",
          "INTERVAL",
          "IS",
          "JSON",
          "KEY",
          "LIKE",
          "NOT",
          "NULL",
          "NULLS",
          "OR",
          "RESPECT",
          "RETURNING",
          "RLIKE",
          "SIMILAR",
          "SYMMETRIC",
          "THEN",
          "TIME",
          "TIMESTAMP",
          "TO",
          "TRUE",
          "VALUE",
          "WHEN",
          "XOR",
          "ZONE");

  @Getter private final String source;
  private final List<Token> tokens;

  /** Position in the source */
  private int position = 0;

  /** Index of the current token */
  private int index = 0;

  public ExpressionLexer(@Nullable String source) {
    if (source == null) throw new ExpressionParseException(0, ErrorCode.NULL_SOURCE_ERROR);
    this.source = source;
    this.tokens = new ArrayList<>();
  }

  public int getPosition() {
    if (index >= 0 && index < tokens.size()) return tokens.get(index).start();
    return source.length();
  }

  private @Nullable Token peekToken() throws ExpressionException {
    while (index >= tokens.size()) {
      Token token = nextToken();
      if (token == null) return null;
      tokens.add(token);
    }
    return tokens.get(index);
  }

  public boolean hasNext() throws ExpressionException {
    return peekToken() != null;
  }

  public void hasNextOrThrows(ErrorCode errorCode, Object... values) throws ExpressionException {
    Token token = peekToken();
    if (token == null) {
      throw new ExpressionParseException(position, errorCode, values);
    }
  }

  public Token next() throws ExpressionException {
    Token token = peekToken();
    if (token == null) {
      throw new ExpressionParseException(
          getPosition(), ErrorCode.INTERNAL_ERROR, "Unexpected end of expression");
    }

    index++;
    return token;
  }

  public Token previous() {
    return tokens.get(--index);
  }

  public boolean is(Id id) throws ExpressionException {
    Token token = peekToken();
    return token != null && token.is(id);
  }

  public boolean isThenNext(Id id) throws ExpressionException {
    if (is(id)) {
      index++;
      return true;
    }
    return false;
  }

  public boolean isThenNextAndNotEnd(Id id) throws ExpressionException {
    Token token = peekToken();
    if (token != null && token.is(id)) {
      index++;
      if (!hasNext()) {
        throw new ExpressionParseException(token.start(), ErrorCode.SYNTAX_ERROR, id);
      }
      return true;
    }
    return false;
  }

  public void nextOrThrows(Id id, ErrorCode errorCode, Object... values)
      throws ExpressionException {
    if (is(id)) {
      index++;
      return;
    }
    Token token = peekToken();
    throw new ExpressionParseException(token != null ? token.start() : position, errorCode, values);
  }

  public static Set<String> getReservedWords() {
    return RESERVED_WORDS;
  }

  public static boolean isReservedWord(@Nullable String name) {
    if (name == null) return false;
    return RESERVED_WORDS.contains(name.toUpperCase());
  }

  /**
   * Tokenize the source string.
   *
   * @return The list of tokens.
   */
  public List<Token> getTokens() throws ExpressionException {
    List<Token> tokens = new ArrayList<>();
    for (Token token = nextToken(); token != null; token = nextToken()) {
      tokens.add(token);
    }
    return tokens;
  }

  /**
   * Returns the next token.
   *
   * @return The next token, or null if end of source.
   */
  @Nullable
  protected Token nextToken() throws ExpressionException {

    while (position < source.length()) {
      char c = source.charAt(position);

      switch (c) {
        case ',':
          return new Token(Id.COMMA, position++);

        case '(':
          return new Token(Id.LPARENTHESIS, position++);

        case ')':
          return new Token(Id.RPARENTHESIS, position++);

        case '[':
          return new Token(Id.LBRACKET, position++);

        case ']':
          return new Token(Id.RBRACKET, position++);

          // Single-quoted literal text.
        case '\'':
          {
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
              throw new ExpressionParseException(start, ErrorCode.MISSING_END_SINGLE_QUOTED_STRING);
            }

            return new Token(Id.LITERAL_STRING, start, position, text.toString());
          }

        case '=':
          return new Token(Id.EQUAL, position++);

        case '+':
          return new Token(Id.PLUS, position++);

        case '-':
          {
            // Single line comment --
            if (position + 1 < source.length() && source.charAt(position + 1) == '-') {
              int start = position;
              position++;
              while (position < source.length()) {
                c = source.charAt(position);
                if (c == '\r' || c == '\n') break;
                position++;
              }
              // return new Token(Id.COMMENT, start, position, source.substring(start, position));
              // Ignore comment
              continue;
            }

            // Minus sign
            return new Token(Id.MINUS, position++);
          }

        case '*':
          return new Token(Id.STAR, position++);

        case '%':
          return new Token(Id.PERCENT, position++);

        case '<':
          {
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
        case '>':
          {
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
        case '!':
          {
            int start = position++;
            if (position < source.length()) {
              c = source.charAt(position);
              if (c == '=') {
                position++;
                return new Token(Id.NOT_EQUAL, start);
              }
            }
            throw new ExpressionParseException(start, ErrorCode.UNEXPECTED_CHARACTER, '!');
          }

          // cast operator
        case ':':
          {
            int start = position++;
            if (position < source.length()) {
              c = source.charAt(position);
              if (c == ':') {
                position++;
                return new Token(Id.CAST, start);
              }
            }
            throw new ExpressionParseException(start, ErrorCode.UNEXPECTED_CHARACTER, ':');
          }

          // possible start of '/*' or '//' comment
        case '/':
          {
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
                    } else position++;
                  } else {
                    throw new ExpressionParseException(start, ErrorCode.MISSING_END_BLOCK_COMMENT);
                  }
                }
                // Ignore comment
                continue;
                // return new Token(Id.COMMENT, start, position, source.substring(start, position));
              }
              // Line comment
              if (c1 == '/') {
                position++;

                while (position < source.length()) {
                  c = source.charAt(position);
                  if (c == '\r' || c == '\n') break;
                  position++;
                }

                // Ignore comment
                continue;
                // return new Token(Id.COMMENT, start, position, source.substring(start, position));
              }
            }
            return new Token(Id.SLASH, start);
          }

        case '~':
          return new Token(Id.TILDE, position++);

        case '&':
          return new Token(Id.AMPERSAND, position++);

        case '^':
          return new Token(Id.CARET, position++);

          // Bitwise OR operator or concat symbol
        case '|':
          {
            int start = position++;
            if (position < source.length()) {
              c = source.charAt(position);
              if (c == '|') {
                position++;
                return new Token(Id.CONCAT, start);
              }
            }
            return new Token(Id.PIPE, start);
          }

          // Quoted identifier matching reserved words or with white space
        case '"':
          {
            int start = position++;
            while (position < source.length()) {
              c = source.charAt(position++);
              if (c == '"') {
                String value = source.substring(start + 1, position - 1).toUpperCase();
                return new Token(Id.IDENTIFIER, start, position, value);
              }
            }
            throw new ExpressionParseException(start, ErrorCode.MISSING_END_DOUBLE_QUOTED_STRING);
          }

        case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.': // Number without zero .1
          {
            int start = position;
            char previous;
            boolean error = false;

            if (c == '0' && position + 1 < source.length()) {
              c = source.charAt(position + 1);

              // Literal hexadecimal numeric 0xABC_DEF
              if (c == 'x' || c == 'X') {
                position += 2;
                previous = c;
                while (position < source.length()) {
                  c = source.charAt(position);
                  if (Characters.isHexDigit(c) || c == '_') {
                    position++;
                    if (c == '_' && c == previous) error = true;
                    previous = c;
                  } else {
                    break;
                  }
                }

                String str = source.substring(start, position);
                // Empty, consecutive underscore or last char is underscore
                if (str.length() == 2 || previous == '_' || error) {
                  throw new ExpressionParseException(position, ErrorCode.INVALID_NUMBER, str);
                }

                return new Token(Id.LITERAL_NUMERIC_HEXA, start, position, str.replace("_", ""));
              }

              // Literal binary numeric 0b0110_1011
              if (source.charAt(position + 1) == 'b' || source.charAt(position + 1) == 'B') {
                position += 2;
                previous = c;
                while (position < source.length()) {
                  c = source.charAt(position);
                  if (Characters.isBitDigit(c) || c == '_') {
                    position++;
                    if (c == '_' && c == previous) error = true;
                    previous = c;
                  } else {
                    break;
                  }
                }

                String str = source.substring(start, position);
                // Empty, consecutive underscore or last char is underscore
                if (str.length() == 2 || previous == '_' || error) {
                  throw new ExpressionParseException(position, ErrorCode.INVALID_NUMBER, str);
                }

                return new Token(Id.LITERAL_NUMERIC_BINARY, start, position, str.replace("_", ""));
              }

              // Literal octal numeric 0o1234567
              if (source.charAt(position + 1) == 'o' || source.charAt(position + 1) == 'O') {
                position += 2;
                previous = c;
                while (position < source.length()) {
                  c = source.charAt(position);
                  if (Characters.isOctDigit(c) || c == '_') {
                    position++;
                    if (c == '_' && c == previous) error = true;
                    previous = c;
                  } else {
                    break;
                  }
                }

                String str = source.substring(start, position);
                // Empty, consecutive underscore or last char is underscore
                if (str.length() == 2 || previous == '_' || error) {
                  throw new ExpressionParseException(position, ErrorCode.INVALID_NUMBER, str);
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
                if (c == '_' && c == previous) error = true;
                previous = c;
              } else {
                break;
              }
            }

            // Last char should not be an underscore
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
                if (c == '_' && c == previous) error = true;
                previous = c;
              } else {
                break;
              }
            }

            // Last char should not be an underscore
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
                  if (Characters.isDigit(c)) digit++;
                  if (c == '_' && c == previous) error = true;
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
            if (str.isEmpty() || previous == '_' || error) {
              throw new ExpressionParseException(position, ErrorCode.INVALID_NUMBER, str);
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
            if (Characters.isSpace(c) || "()/*%,^&><=~+-.!|$:[]\n\r".indexOf(c) >= 0) break;

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
