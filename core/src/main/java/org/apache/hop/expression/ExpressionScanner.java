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

import java.util.Arrays;
import java.util.Set;
import java.util.TreeSet;

import org.apache.hop.expression.Token.Id;
import org.apache.hop.expression.util.Characters;
import org.apache.hop.i18n.BaseMessages;

/** Parses an expression string to return the individual tokens. */
public class ExpressionScanner {

  private static final Class<?> PKG = IExpression.class; // for i18n purposes

  // TODO: Java 9 use unmodifiable Set.of()
  private static final Set<String> RESERVED_WORDS =
      new TreeSet<>(
          Arrays.asList(
              "AS",
              "AND",
              "BETWEEN",
              "CASE",
              "DATE",
              "ELSE",
              "END",
              "ESCAPE",
              "FALSE",
              "FORMAT",
              "FROM",
              "ILIKE",
              "IN",
              "IS",
              "LIKE",
              "NOT",
              "NULL",
              "OR",
              "THEN",
              "TIME",
              "TIMESTAMP",
              "TRUE",
              "WHEN",
              "XOR"));

  public static Set<String> getReservedWords() {
    return RESERVED_WORDS;
  }

  private String source;

  private int pos = 0;

  public ExpressionScanner(String text) {
    this.source = text;
  }

  public Token tokenize() throws ExpressionParserException {

    while (pos < source.length()) {
      char c = source.charAt(pos);

      switch (c) {
        case ',':
          return new Token(Id.COMMA, pos++);

        case '(':
          return new Token(Id.LPARENTHESIS, pos++);

        case ')':
          return new Token(Id.RPARENTHESIS, pos++);

          // Double-quoted literal text.
          //			case '"': {
          //				StringBuilder text = new StringBuilder();
          //				int start = pos++;
          //				while (pos < source.length()) {
          //					c = source.charAt(pos++);
          //					if (c == '"') {
          //						if (pos < source.length()) {
          //							char c2 = source.charAt(pos);
          //							// encountered consecutive double-quotes
          //							if (c2 == '"') {
          //								++pos;
          //								text.append(c);
          //								continue;
          //							}
          //						}
          //						break;
          //					}
          //					text.append(c);
          //				}
          //
          //				if (c != '"')
          //					throw new
          // ExpressionParserException("ExpressionException.MissingEndDoubleQuotedString", source,
          //							pos);
          //
          //				return new ExpressionToken(Id.LITERAL_TEXT, start, text.toString());
          //			}

          // Single-quoted literal text.
        case '\'':
          {
            StringBuilder text = new StringBuilder();
            int index = pos++;
            while (pos < source.length()) {
              c = source.charAt(pos++);
              if (c == '\'') {
                if (pos < source.length()) {
                  char c2 = source.charAt(pos);
                  // encountered consecutive single-quotes
                  if (c2 == '\'') {
                    ++pos;
                    text.append(c);
                    continue;
                  }
                }
                break;
              }
              text.append(c);
            }

            if (c != '\'')
              throw new ExpressionParserException(
                  BaseMessages.getString(PKG, "ExpressionParser.MissingEndSingleQuotedString"),
                  source,
                  pos);

            return new Token(Id.LITERAL_STRING, index, text.toString());
          }

        case '=':
          {
            int index = pos++;
            if (pos < source.length()) {
              c = source.charAt(pos);
              if (c == '~') {
                pos++;
                return new Token(Id.CONTAINS, index);
              }
            }
            return new Token(Id.EQUAL, index);
          }

        case '+':
          return new Token(Id.PLUS, pos++);

        case '-':
          return new Token(Id.MINUS, pos++);

        case '*':
          return new Token(Id.MULTIPLY, pos++);

        case '%':
          return new Token(Id.MODULUS, pos++);

        case '<':
          {
            // parse less symbol
            int index = pos++;
            if (pos < source.length()) {
              c = source.charAt(pos);
              if (c == '=') {
                pos++;
                return new Token(Id.LESS_THAN_OR_EQUAL, index);
              }
              if (c == '>') {
                pos++;
                return new Token(Id.LESS_THAN_OR_GREATER_THAN, index);
              }
            }
            return new Token(Id.LESS_THAN, index);
          }

          // parse greater symbol
        case '>':
          {
            int index = pos++;
            if (pos < source.length()) {
              c = source.charAt(pos);
              if (c == '=') {
                pos++;
                return new Token(Id.GREATER_THAN_OR_EQUAL, index);
              }
            }
            return new Token(Id.GREATER_THAN, index);
          }

          // parse not equal symbol
        case '!':
          {
            int start = pos++;
            if (pos < source.length()) {
              c = source.charAt(pos);
              if (c == '=') {
                pos++;
                return new Token(Id.NOT_EQUAL, start);
              }
            }
            throw new ExpressionParserException(
                BaseMessages.getString(PKG, "ExpressionParser.UnexpectedCharacter"), source, start);
          }

          // possible start of '/*' or '//' comment
        case '/':
          {
            int start = pos++;
            if (pos < source.length()) {
              char c1 = source.charAt(pos);
              // Block comment
              if (c1 == '*') {
                int level = 1;

                while (level > 0) {
                  int end = source.indexOf('*', pos + 1);
                  if (end > 0 && end < source.length() - 1) {
                    // nested block comment
                    if (source.charAt(end - 1) == '/') {
                      level++;
                      pos = end;
                      continue;
                    }
                    if (source.charAt(end + 1) == '/') {
                      level--;
                      pos = end + 2;
                    } else pos++;
                  } else {
                    throw new ExpressionParserException(
                        BaseMessages.getString(PKG, "ExpressionParser.MissingEndBlockComment"),
                        source,
                        pos);
                  }
                }

                //						int end = source.indexOf("*/", pos + 1);
                //						if (end < 0) {
                //							end = source.length();
                //						} else {
                //							end += "*/".length();
                //						}
                //						pos = end;
                return new Token(Id.COMMENT, start);
              }
              // Line comment
              if (c1 == '/') {
                pos++;

                while (pos < source.length()) {
                  c = source.charAt(pos);
                  if (c == '\r' || c == '\n') break;
                  pos++;
                }

                return new Token(Id.COMMENT, start);
              }
            }
            return new Token(Id.DIVIDE, start);
          }

        case '~':
          return new Token(Id.BITWISE_NOT, pos++);

        case '&':
          return new Token(Id.BITWISE_AND, pos++);

        case '^':
          return new Token(Id.BITWISE_XOR, pos++);

          // Bitwise OR operator or concat symbol
        case '|':
          {
            int start = pos++;
            if (pos < source.length()) {
              c = source.charAt(pos);
              if (c == '|') {
                pos++;
                return new Token(Id.CONCAT, start);
              }
            }
            return new Token(Id.BITWISE_OR, start);
          }

          // Escape field name matching reserved words
        case '[':
          {
            int start = pos++;
            while (pos < source.length()) {
              c = source.charAt(pos++);
              if (c == ']') {
                String name = source.substring(start + 1, pos - 1).toUpperCase();
                return new Token(Id.IDENTIFIER, start, name);
              }
            }
            // FIXME: End of bracket
            throw new ExpressionParserException(
                BaseMessages.getString(PKG, "ExpressionParser.SyntaxError"), source, pos);
          }

          // Variable
          //			case '$': {
          //				int start = pos++;
          //				if (pos < source.length()) {
          //
          //					c = source.charAt(pos++);
          //					if (c != '{')
          //						throw new ExpressionParserException("ExpressionException.SyntaxError", source,
          // pos);
          //
          //					while (pos < source.length()) {
          //						c = source.charAt(++pos);
          //						if (!Characters.isAlphaOrDigit(c)) {
          //							break;
          //						}
          //					}
          //
          //					if (c != '}')
          //						throw new ExpressionParserException("ExpressionException.SyntaxError", source,
          // pos);
          //
          //					String name = source.substring(start + 2, pos++).toUpperCase();
          //					return new ExpressionToken(Id.VARIABLE, start, pos, name);
          //				}
          //				throw new ExpressionParserException("ExpressionException.SyntaxError", source, pos);
          //			}

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
          {
            int index = pos++;

            // Hexadecimal number 0xABCDEF
            if (c == '0' && pos < source.length() && (source.charAt(pos) == 'x')) {
              do {
                pos++;
              } while (pos < source.length() && Characters.isHexDigit(source.charAt(pos)));

              return new Token(Id.LITERAL_BINARY_HEX, index, source.substring(index + 2, pos));
            }

            // Binary number 0b01101011
            if (c == '0' && pos < source.length() && (source.charAt(pos) == 'b')) {
              do {
                pos++;
              } while (pos < source.length()
                  && (source.charAt(pos) == '0' || source.charAt(pos) == '1'));

              return new Token(Id.LITERAL_BINARY_BIT, index, source.substring(index + 2, pos));
            }

            // Integer part
            while (pos < source.length() && Characters.isDigit(source.charAt(pos))) {
              pos++;
            }

            // Use dot for decimal separator
            if (pos < source.length() && source.charAt(pos) == '.') {
              pos++;
            }

            // Decimal part
            while (pos < source.length() && Characters.isDigit(source.charAt(pos))) {
              pos++;
            }

            // Exponentiation part
            if (pos < source.length() && Characters.isExponentChar(source.charAt(pos))) {
              pos++;

              if (pos < source.length()
                  && (source.charAt(pos) == '+' || source.charAt(pos) == '-')) {
                pos++;
              }
              while (pos < source.length() && Character.isDigit(source.charAt(pos))) {
                pos++;
              }
            }

            return new Token(Id.LITERAL_NUMBER, index, source.substring(index, pos));
          }

        default:
          if (Character.isWhitespace(c)) {
            ++pos;
            continue;
          }

          // Probably a letter or digit. Start an identifier.
          // Other characters, e.g. *, ! are also included
          // in identifiers.
          int index = pos++;
          boolean isFunction = false;
          loop:
          while (pos < source.length()) {
            c = source.charAt(pos);
            switch (c) {
              case '(':
                isFunction = true;
              case ')':
              case '/':
              case '*':
              case '%':
              case ',':
              case '^':
              case '&':
              case '>':
              case '<':
              case '=':
              case '~':
              case '+':
              case '-':
              case '!':
              case '|':
              case '$':
              case '[':
              case ']':
                break loop;

              default:
                if (Character.isWhitespace(c)) {
                  break loop;
                } else {
                  ++pos;
                }
            }
          }
          String identifier = source.substring(index, pos);
          String name = identifier.toUpperCase();

          if (isFunction && Function.getFunction(name) != null) {
            return new Token(Id.FUNCTION, index, name);
          }

          // Reserved words: AS, AND, LIKE, NOT, TRUE, FALSE, OR
          if (RESERVED_WORDS.contains(name)) {
            return new Token(Id.valueOf(name), index, name);
          }

          DataType type = DataType.of(name);
          if (type != null) {
            return new Token(Id.DATATYPE, index, name);
          }

          DatePart part = DatePart.of(name);
          if (part != null) {
            return new Token(Id.DATEPART, index, name);
          }

          return new Token(Id.IDENTIFIER, index, identifier);
      }
    }
    return null;
  }
}
