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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import org.apache.hop.expression.Token.Id;
import org.junit.jupiter.api.Test;

class ExpressionLexerTest {
  @Test
  void testNullSource() {
    assertThrows(ExpressionException.class, () -> new ExpressionLexer(null));
  }

  @Test
  void testOperators() throws Exception {
    ExpressionLexer lexer =
        new ExpressionLexer(", ( ) [ ] = + - * % < > <= <> >= != = = :: / ~ & ^ || |");

    assertEquals(Id.COMMA, lexer.next().id());
    assertEquals(Id.LPARENTHESIS, lexer.next().id());
    assertEquals(Id.RPARENTHESIS, lexer.next().id());
    assertEquals(Id.LBRACKET, lexer.next().id());
    assertEquals(Id.RBRACKET, lexer.next().id());
    assertEquals(Id.EQUAL, lexer.next().id());
    assertEquals(Id.PLUS, lexer.next().id());
    assertEquals(Id.MINUS, lexer.next().id());
    assertEquals(Id.STAR, lexer.next().id());
    assertEquals(Id.PERCENT, lexer.next().id());
    assertEquals(Id.LT, lexer.next().id());
    assertEquals(Id.GT, lexer.next().id());
    assertEquals(Id.LTE, lexer.next().id());
    assertEquals(Id.NOT_EQUAL, lexer.next().id());
    assertEquals(Id.GTE, lexer.next().id());
    assertEquals(Id.NOT_EQUAL, lexer.next().id());
    assertEquals(Id.EQUAL, lexer.next().id());
    assertEquals(Id.EQUAL, lexer.next().id());
    assertEquals(Id.CAST, lexer.next().id());
    assertEquals(Id.SLASH, lexer.next().id());
    assertEquals(Id.TILDE, lexer.next().id());
    assertEquals(Id.AMPERSAND, lexer.next().id());
    assertEquals(Id.CARET, lexer.next().id());
    assertEquals(Id.CONCAT, lexer.next().id());
    assertEquals(Id.PIPE, lexer.next().id());
  }

  @Test
  void testStrings() throws Exception {
    ExpressionLexer lexer = new ExpressionLexer("'hello' 'it''s me'");

    Token token = lexer.next();
    assertEquals(Id.LITERAL_STRING, token.id());
    assertEquals("hello", token.text());

    token = lexer.next();
    assertEquals(Id.LITERAL_STRING, token.id());
    assertEquals("it's me", token.text());
  }

  @Test
  void testIdentifiers() throws Exception {
    ExpressionLexer lexer = new ExpressionLexer("field1 \"quoted field\" \"CASE\"");
    List<Token> tokens = lexer.getTokens();

    assertEquals(Id.IDENTIFIER, tokens.get(0).id());
    assertEquals("field1", tokens.get(0).text());
    assertEquals(Id.IDENTIFIER, tokens.get(1).id());
    assertEquals("QUOTED FIELD", tokens.get(1).text());
    assertEquals(Id.IDENTIFIER, tokens.get(2).id());
    assertEquals("CASE", tokens.get(2).text());
  }

  @Test
  void testIsReservedWords() {
    assertTrue(ExpressionLexer.isReservedWord("BETWEEN"));
    assertFalse(ExpressionLexer.isReservedWord("XXX"));
    assertFalse(ExpressionLexer.isReservedWord(null));
  }

  @Test
  void testReservedWords() throws Exception {
    ExpressionLexer lexer =
        new ExpressionLexer("CASE WHEN THEN ELSE END AND OR NOT XOR IS NULL TRUE FALSE");
    List<Token> tokens = lexer.getTokens();

    assertEquals(Id.CASE, tokens.get(0).id());
    assertEquals(Id.WHEN, tokens.get(1).id());
    assertEquals(Id.THEN, tokens.get(2).id());
    assertEquals(Id.ELSE, tokens.get(3).id());
    assertEquals(Id.END, tokens.get(4).id());
    assertEquals(Id.AND, tokens.get(5).id());
    assertEquals(Id.OR, tokens.get(6).id());
    assertEquals(Id.NOT, tokens.get(7).id());
    assertEquals(Id.XOR, tokens.get(8).id());
    assertEquals(Id.IS, tokens.get(9).id());
    assertEquals(Id.NULL, tokens.get(10).id());
    assertEquals(Id.TRUE, tokens.get(11).id());
    assertEquals(Id.FALSE, tokens.get(12).id());

    assertEquals(42, ExpressionLexer.getReservedWords().size());
  }

  @Test
  void testNumbers() throws Exception {
    ExpressionLexer lexer =
        new ExpressionLexer("123 123_456 12.34 .5 1e10 1.2E-5 0xABC 0b101 0o777");
    List<Token> tokens = lexer.getTokens();

    assertEquals(Id.LITERAL_NUMERIC_DECIMAL, tokens.get(0).id());
    assertEquals("123", tokens.get(0).text());
    assertEquals(Id.LITERAL_NUMERIC_DECIMAL, tokens.get(1).id());
    assertEquals("123456", tokens.get(1).text());
    assertEquals(Id.LITERAL_NUMERIC_DECIMAL, tokens.get(2).id());
    assertEquals("12.34", tokens.get(2).text());
    assertEquals(Id.LITERAL_NUMERIC_DECIMAL, tokens.get(3).id());
    assertEquals(".5", tokens.get(3).text());
    assertEquals(Id.LITERAL_NUMERIC_DECIMAL, tokens.get(4).id());
    assertEquals("1e10", tokens.get(4).text());
    assertEquals(Id.LITERAL_NUMERIC_DECIMAL, tokens.get(5).id());
    assertEquals("1.2E-5", tokens.get(5).text());
    assertEquals(Id.LITERAL_NUMERIC_HEXA, tokens.get(6).id());
    assertEquals("0xABC", tokens.get(6).text());
    assertEquals(Id.LITERAL_NUMERIC_BINARY, tokens.get(7).id());
    assertEquals("0b101", tokens.get(7).text());
    assertEquals(Id.LITERAL_NUMERIC_OCTAL, tokens.get(8).id());
    assertEquals("0o777", tokens.get(8).text());
  }

  @Test
  void testComments() throws Exception {
    // getTokens() should ignore comments
    ExpressionLexer lexer =
        new ExpressionLexer(
            "1 -- line comment\n2 /* block \n comment */ 3 // another line comment\n4 /* nested /* block */ comment */ 5");
    List<Token> tokens = lexer.getTokens();

    assertEquals(5, tokens.size());
    assertEquals("1", tokens.get(0).text());
    assertEquals("2", tokens.get(1).text());
    assertEquals("3", tokens.get(2).text());
    assertEquals("4", tokens.get(3).text());
    assertEquals("5", tokens.get(4).text());
  }

  @Test
  void testFunctions() throws Exception {
    ExpressionLexer lexer = new ExpressionLexer("ABS(x) UPPER('a') NOT(TRUE)");
    List<Token> tokens = lexer.getTokens();

    assertEquals(Id.FUNCTION, tokens.get(0).id());
    assertEquals("ABS", tokens.get(0).text());
    assertEquals(Id.LPARENTHESIS, tokens.get(1).id());

    // Jump to next function
    assertEquals(Id.FUNCTION, tokens.get(4).id());
    assertEquals("UPPER", tokens.get(4).text());

    // NOT(TRUE) is special case in lexer
    assertEquals(Id.NOT, tokens.get(8).id());
  }

  @Test
  void testDataTypesAndUnits() throws Exception {
    ExpressionLexer lexer = new ExpressionLexer("INTEGER STRING BOOLEAN DAY MONTH YEAR");
    List<Token> tokens = lexer.getTokens();

    assertEquals(Id.LITERAL_DATATYPE, tokens.get(0).id());
    assertEquals("INTEGER", tokens.get(0).text());
    assertEquals(Id.LITERAL_DATATYPE, tokens.get(1).id());
    assertEquals("STRING", tokens.get(1).text());
    assertEquals(Id.LITERAL_DATATYPE, tokens.get(2).id());
    assertEquals("BOOLEAN", tokens.get(2).text());

    assertEquals(Id.LITERAL_TIMEUNIT, tokens.get(3).id());
    assertEquals("DAY", tokens.get(3).text());
    assertEquals(Id.LITERAL_TIMEUNIT, tokens.get(4).id());
    assertEquals("MONTH", tokens.get(4).text());
    assertEquals(Id.LITERAL_TIMEUNIT, tokens.get(5).id());
    assertEquals("YEAR", tokens.get(5).text());
  }

  @Test
  void testErrors() {
    assertThrows(
        ExpressionParseException.class, () -> new ExpressionLexer("'unclosed").getTokens());
    assertThrows(
        ExpressionParseException.class, () -> new ExpressionLexer("\"unclosed").getTokens());
    assertThrows(
        ExpressionParseException.class, () -> new ExpressionLexer("/* unclosed").getTokens());
    assertThrows(ExpressionParseException.class, () -> new ExpressionLexer("0x__123").getTokens());
    assertThrows(ExpressionParseException.class, () -> new ExpressionLexer("123_").getTokens());
    assertThrows(ExpressionParseException.class, () -> new ExpressionLexer("!").getTokens());
    assertThrows(ExpressionParseException.class, () -> new ExpressionLexer(":").getTokens());
  }
}
