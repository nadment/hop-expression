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
import java.util.Set;
import org.apache.hop.expression.operator.CoalesceFunction;
import org.junit.jupiter.api.Test;

class KeywordTest {

  @Test
  void testIs() {
    assertTrue(Keyword.is("AND"));
    assertTrue(Keyword.is("BETWEEN"));
    assertTrue(Keyword.is("THEN"));
    assertFalse(Keyword.is("XXX"));
    assertFalse(Keyword.is(null));
  }

  @Test
  void testGet() throws Exception {
    // The registry is shared, other tests can register more words
    assertEquals(33, Keyword.get().stream().filter(Keyword::isCore).count());
    assertThrows(UnsupportedOperationException.class, () -> Keyword.get().add("XXX"));
  }

  @Test
  void eachKeywordIsAToken() {
    // The lexer creates the token of a core word with Token.Id.valueOf
    for (String word : Keyword.get()) {
      if (Keyword.isCore(word)) {
        assertEquals(word, Token.Id.valueOf(word).name());
      }
    }
  }

  @Test
  void register() {
    assertFalse(Keyword.is("KEYWORD_TEST_REGISTER"));
    Keyword.register("Keyword_Test_Register");

    // The words are case-insensitive, they are not core words
    assertTrue(Keyword.is("KEYWORD_TEST_REGISTER"));
    assertTrue(Keyword.is("keyword_test_register"));
    assertFalse(Keyword.isCore("KEYWORD_TEST_REGISTER"));
    assertTrue(Keyword.get().contains("KEYWORD_TEST_REGISTER"));

    // Registering a word again or a core word has no effect
    Keyword.register("KEYWORD_TEST_REGISTER");
    Keyword.register("from");
    assertTrue(Keyword.isCore("FROM"));
  }

  @Test
  void registerInvalidWord() {
    assertThrows(IllegalArgumentException.class, () -> Keyword.register(null));
    assertThrows(IllegalArgumentException.class, () -> Keyword.register(""));
    assertThrows(IllegalArgumentException.class, () -> Keyword.register("TWO WORDS"));
    assertThrows(IllegalArgumentException.class, () -> Keyword.register("A1"));
    assertThrows(IllegalArgumentException.class, () -> Keyword.register("A-B"));
    // A data type or a time unit would no longer be recognized by the lexer
    assertThrows(IllegalArgumentException.class, () -> Keyword.register("NUMBER"));
    assertThrows(IllegalArgumentException.class, () -> Keyword.register("year"));
  }

  @Test
  void lexerCreatesKeywordToken() throws Exception {
    Keyword.register("KEYWORD_TEST_LEXER");

    List<Token> tokens = new ExpressionLexer("1 keyword_test_lexer FROM").getTokens();
    assertEquals(Token.Id.KEYWORD, tokens.get(1).id());
    assertEquals("KEYWORD_TEST_LEXER", tokens.get(1).text());
    // A core word keeps its own token
    assertEquals(Token.Id.FROM, tokens.get(2).id());
    // A registered word followed by a parenthesis is still a function
    assertEquals(
        Token.Id.FUNCTION, new ExpressionLexer("KEYWORD_TEST_LEXER(1)").getTokens().get(0).id());
  }

  @Test
  void identifierWithRegisteredWordIsQuoted() {
    Keyword.register("KEYWORD_TEST_QUOTE");
    assertEquals("\"KEYWORD_TEST_QUOTE\"", Identifier.quoteIfNeeded("KEYWORD_TEST_QUOTE"));
  }

  @Test
  void functionRegistersItsKeywords() {
    Function function =
        new CoalesceFunction() {
          @Override
          public Set<String> getKeywords() {
            return Set.of("KEYWORD_TEST_FUNCTION");
          }
        };
    FunctionRegistry.register("KEYWORD_TEST_FUNC", function);
    try {
      assertTrue(Keyword.is("KEYWORD_TEST_FUNCTION"));
    } finally {
      FunctionRegistry.unregister("KEYWORD_TEST_FUNC");
    }
  }

  @Test
  void functionWithInvalidKeywordIsNotRegistered() {
    Function function =
        new CoalesceFunction() {
          @Override
          public Set<String> getKeywords() {
            return Set.of("NOT A WORD");
          }
        };
    assertThrows(
        IllegalArgumentException.class,
        () -> FunctionRegistry.register("KEYWORD_TEST_INVALID", function));
    assertFalse(FunctionRegistry.isFunction("KEYWORD_TEST_INVALID"));
  }
}
