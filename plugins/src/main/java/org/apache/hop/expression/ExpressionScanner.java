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
import org.apache.hop.i18n.BaseMessages;
import java.text.ParseException;
import java.util.Arrays;
import java.util.Set;
import java.util.TreeSet;

/** Parses an expression string to return the individual tokens. */
public class ExpressionScanner {

  private static final Class<?> PKG = ExpressionScanner.class; // for i18n purposes

  private static final Set<String> RESERVED_WORDS =
      new TreeSet<>(Arrays.asList("AS", "AND", "AT", "BETWEEN", "CASE", "DATE", "ELSE", "END",
          "ESCAPE", "FALSE", "FORMAT", "FROM", "ILIKE", "IN", "IS", "LIKE", "NOT", "NULL", "OR", "SYMMETRY",
          "THEN", "TIME", "TIMESTAMP", "TRUE", "WHEN", "XOR", "ZONE"));
  
  public static Set<String> getReservedWords() {
    return RESERVED_WORDS;
  }

  public static boolean isReservedWord(String name) {
    return RESERVED_WORDS.contains(name.toUpperCase());
  }
  
  private String source;

  private int index = 0;

  public ExpressionScanner(String text) {
    this.source = text;
  }

  public Token tokenize() throws ParseException {

    while (index < source.length()) {
      char c = source.charAt(index);

      switch (c) {
        case ',':
          return new Token(Id.COMMA, index++);

        case '(':
          return new Token(Id.LPARENTHESIS, index++);

        case ')':
          return new Token(Id.RPARENTHESIS, index++);

        // Single-quoted literal text.
        case '\'': {
          StringBuilder text = new StringBuilder();
          int start = index++;
          while (index < source.length()) {
            c = source.charAt(index++);
            if (c == '\'') {
              if (index < source.length()) {
                char c2 = source.charAt(index);
                // encountered consecutive single-quotes
                if (c2 == '\'') {
                  ++index;
                  text.append(c);
                  continue;
                }
              }
              break;
            }
            text.append(c);
          }

          if (c != '\'')
            throw new ParseException(
                BaseMessages.getString(PKG, "Expression.MissingEndSingleQuotedString"),

                index);

          return new Token(Id.LITERAL_STRING, start, index, text.toString());
        }

        case '=':
          return new Token(Id.EQUAL, index++);

        case '+':
          return new Token(Id.PLUS, index++);

        case '-':
          return new Token(Id.MINUS, index++);

        case '*':
          return new Token(Id.MULTIPLY, index++);

        case '%':
          return new Token(Id.MODULUS, index++);

        case '<': {
          // parse less symbol
          int start = index++;
          if (index < source.length()) {
            c = source.charAt(index);
            if (c == '=') {
              index++;
              return new Token(Id.LESS_THAN_OR_EQUAL, start);
            }
            if (c == '>') {
              index++;
              return new Token(Id.LESS_THAN_OR_GREATER_THAN, start);
            }
          }
          return new Token(Id.LESS_THAN, start);
        }

        // parse greater symbol
        case '>': {
          int start = index++;
          if (index < source.length()) {
            c = source.charAt(index);
            if (c == '=') {
              index++;
              return new Token(Id.GREATER_THAN_OR_EQUAL, start);
            }
          }
          return new Token(Id.GREATER_THAN, start);
        }

        // parse not equal symbol
        case '!': {
          int start = index++;
          if (index < source.length()) {
            c = source.charAt(index);
            if (c == '=') {
              index++;
              return new Token(Id.NOT_EQUAL, start);
            }
          }
          throw new ParseException(BaseMessages.getString(PKG, "Expression.UnexpectedCharacter"),
              start);
        }


        // cast operator
        case ':': {
          int start = index++;
          if (index < source.length()) {
            c = source.charAt(index);
            if (c == ':') {
              index++;
              return new Token(Id.CAST, start);
            }
          }
          throw new ParseException(BaseMessages.getString(PKG, "Expression.UnexpectedCharacter"),
              start);
        }

        // possible start of '/*' or '//' comment
        case '/': {
          int start = index++;
          if (index < source.length()) {
            char c1 = source.charAt(index);
            // Block comment
            if (c1 == '*') {
              int level = 1;

              while (level > 0) {
                int end = source.indexOf('*', index + 1);
                if (end > 0 && end < source.length() - 1) {
                  // nested block comment
                  if (source.charAt(end - 1) == '/') {
                    level++;
                    index = end;
                    continue;
                  }
                  if (source.charAt(end + 1) == '/') {
                    level--;
                    index = end + 2;
                  } else
                    index++;
                } else {
                  throw new ParseException(
                      BaseMessages.getString(PKG, "Expression.MissingEndBlockComment"),

                      index);
                }
              }

              return new Token(Id.COMMENT, start, index, source.substring(start, index));
            }
            // Line comment
            if (c1 == '/') {
              index++;

              while (index < source.length()) {
                c = source.charAt(index);
                if (c == '\r' || c == '\n')
                  break;
                index++;
              }

              return new Token(Id.COMMENT, start, index, source.substring(start, index));
            }
          }
          return new Token(Id.DIVIDE, start);
        }

        case '~':
          return new Token(Id.BITWISE_NOT, index++);

        case '&':
          return new Token(Id.BITWISE_AND, index++);

        case '^':
          return new Token(Id.BITWISE_XOR, index++);

        // Bitwise OR operator or concat symbol
        case '|': {
          int start = index++;
          if (index < source.length()) {
            c = source.charAt(index);
            if (c == '|') {
              index++;
              return new Token(Id.CONCAT, start);
            }
          }
          return new Token(Id.BITWISE_OR, start);
        }

        // Escape field name matching reserved words or with white space
        case '[': {
          int start = index++;
          while (index < source.length()) {
            c = source.charAt(index++);
            if (c == ']') {
              String value = source.substring(start + 1, index - 1).toUpperCase();
              return new Token(Id.IDENTIFIER, start, index, value);
            }
          }
          throw new ParseException(BaseMessages.getString(PKG, "Expression.UnexpectedCharacter"),
              index);
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
          int start = index++;

          // Hexadecimal number 0xABCDEF
          if (c == '0' && index < source.length() && (source.charAt(index) == 'x')) {
            do {
              index++;
            } while (index < source.length() && Characters.isHexDigit(source.charAt(index)));

            return new Token(Id.LITERAL_BINARY_HEX, start, index,
                source.substring(start + 2, index));
          }

          // Binary number 0b01101011
          if (c == '0' && index < source.length() && (source.charAt(index) == 'b')) {
            do {
              index++;
            } while (index < source.length()
                && (source.charAt(index) == '0' || source.charAt(index) == '1'));

            return new Token(Id.LITERAL_BINARY_BIT, start, index,
                source.substring(start + 2, index));
          }

          // Integer part
          while (index < source.length() && Characters.isDigit(source.charAt(index))) {
            index++;
          }

          // Use dot for decimal separator
          if (index < source.length() && source.charAt(index) == '.') {
            index++;
          }

          // Decimal part
          while (index < source.length() && Characters.isDigit(source.charAt(index))) {
            index++;
          }

          // Exponentiation part
          if (index < source.length() && Characters.isExponent(source.charAt(index))) {
            index++;

            if (index < source.length()
                && (source.charAt(index) == '+' || source.charAt(index) == '-')) {
              index++;
            }
            while (index < source.length() && Characters.isDigit(source.charAt(index))) {
              index++;
            }
          }

          return new Token(Id.LITERAL_NUMBER, start, index, source.substring(start, index));
        }

        default:
          if (Characters.isSpace(c)) {
            ++index;
            continue;
          }

          // Probably a letter or digit. Start an identifier.
          // Other characters, e.g. *, ! are also included
          // in identifiers.
          int start = index++;
          while (index < source.length()) {
            c = source.charAt(index);
            if (Characters.isSpace(c) || "()/*%,^&><=~+-.!|$:[]".indexOf(c) >= 0)
              break;

            index++;
          }

          OperatorRegistry registry = OperatorRegistry.getInstance();
          String identifier = source.substring(start, index);
          String name = identifier.toUpperCase();

          if (c == '(' && registry.isFunctionName(name)) {
            return new Token(Id.FUNCTION, start, index, name);
          }

          // Reserved words: AS, AND, LIKE, NOT, TRUE, FALSE, OR
          if (isReservedWord(name)) {
            return new Token(Id.valueOf(name), start, index, name);
          }

          if (Type.exist(name)) {
            return new Token(Id.DATATYPE, start, index, name);
          }

          if (DatePart.exist(name)) {
            return new Token(Id.DATEPART, start, index, name);
          }

          return new Token(Id.IDENTIFIER, start, index, identifier);
      }
    }
    return null;
  }
}
