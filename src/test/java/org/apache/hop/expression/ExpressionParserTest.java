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
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.apache.hop.expression.operator.BoolAndOperator;
import org.apache.hop.expression.operator.BoolOrOperator;
import org.apache.hop.expression.operator.BoolXorOperator;
import org.apache.hop.expression.operator.ConcatFunction;
import org.apache.hop.expression.operator.EqualOperator;
import org.apache.hop.expression.operator.GreaterThanOperator;
import org.apache.hop.expression.operator.GreaterThanOrEqualOperator;
import org.apache.hop.expression.operator.InOperator;
import org.apache.hop.expression.operator.IsDistinctFromOperator;
import org.apache.hop.expression.operator.IsFalseOperator;
import org.apache.hop.expression.operator.IsNotDistinctFromOperator;
import org.apache.hop.expression.operator.IsNotFalseOperator;
import org.apache.hop.expression.operator.IsNotNullOperator;
import org.apache.hop.expression.operator.IsNotTrueOperator;
import org.apache.hop.expression.operator.IsNullOperator;
import org.apache.hop.expression.operator.IsTrueOperator;
import org.apache.hop.expression.operator.LessThanOperator;
import org.apache.hop.expression.operator.LessThanOrEqualOperator;
import org.apache.hop.expression.operator.MultiplyOperator;
import org.apache.hop.expression.operator.NotEqualOperator;
import org.apache.hop.expression.operator.NotInOperator;
import org.apache.hop.expression.operator.NotSimilarToOperator;
import org.apache.hop.expression.operator.SimilarToOperator;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.StringType;
import org.junit.jupiter.api.Test;

public class ExpressionParserTest extends ExpressionTest {

  @Test
  void reservedWords() {
    assertTrue(ExpressionParser.isReservedWord("BETWEEN"));
    assertFalse(ExpressionParser.isReservedWord("XXX"));
    assertFalse(ExpressionParser.isReservedWord(null));
    assertEquals(42, ExpressionParser.getReservedWords().size());
  }

  @Test
  void sourceNullSource() {
    ExpressionParser parser = new ExpressionParser(null);
    assertNull(parser.getSource());
  }

  @Test
  void sourceEmptyOrNull() throws Exception {
    // Empty source return NULL
    evalNull("");
    evalNull(" ");
    evalNull("\t");
    evalNull("\n");

    evalFails(null, ErrorCode.NULL_SOURCE_ERROR);
  }

  @Test
  void sourceComment() throws Exception {
    evalTrue(" // Test line comment \n  true ");
    evalTrue(" /* Test block comment */  true ");
    evalTrue(" true /* Test block comment */");
    evalTrue("/*\n * Comment on multi line\n *\n */ True");
    evalTrue(
        "/*\n * Comment on multi line \n  with nesting: /* nested block comment */ *\n */   True");

    // Single line comment
    evalNull("//");
    evalNull("// ");
    evalNull("//\n");
    evalTrue("//\n\rTrue");
    evalTrue("// Single line comment\nTrue");

    evalNull("--");
    evalNull("-- ");
    evalNull("--\n");
    evalTrue("--\n\rTrue");
    evalTrue("-- Single line comment\nTrue");
    evalTrue("-- Single line comment\rTrue");

    // Multi line comment
    evalTrue("/* Line 1\n * Line 2 */ True");

    // Empty
    evalNull("-- Single line comment\n");

    // Syntax error
    evalFails(" /", ErrorCode.SYNTAX_ERROR);
    evalFails("/   True", ErrorCode.SYNTAX_ERROR);
    evalFails("/*   True", ErrorCode.MISSING_END_BLOCK_COMMENT);
    evalFails("/*   True/*", ErrorCode.MISSING_END_BLOCK_COMMENT);
    evalFails("/* /* nested block comment */    True", ErrorCode.MISSING_END_BLOCK_COMMENT);
  }

  @Test
  void sourceTab() throws Exception {
    evalTrue(" \n\tTrue");
    evalTrue(" \nTrue\t\r");
  }

  @Test
  void sourceCarriageReturnAndLineFeed() throws Exception {
    evalTrue(" \rTrue");
    evalTrue(" \n\tTrue");
    evalTrue(" \nTrue\n\r");
  }

  @Test
  void array() throws Exception {
    // Empty array
    optimize("[]");

    // Simple array values
    optimize("[1]");
    optimize("[1,2.5,3+2]", "[1,2.5,5]");

    // Multi-dimensional array values
    optimize("[[1,2],[3,4]]");

    evalFails("[1", ErrorCode.MISSING_RIGHT_BRACKET);
    evalFails("[1,", ErrorCode.SYNTAX_ERROR);
    evalFails("[1,]", ErrorCode.SYNTAX_ERROR);
  }

  @Test
  void arrayElementAt() throws Exception {
    evalEquals("['A','B','C'][1]", "A").returnType(StringType.of(1));
    evalEquals("[1,4,8][2]", 4L).returnType(IntegerType.of(1));
  }

  @Test
  void operatorComparator() {
    // Primary operator first and alias last
    OperatorComparator comparator = new OperatorComparator();
    assertTrue(comparator.compare(ConcatFunction.INSTANCE, new ConcatFunction()) > 0);
  }

  @Test
  void operator() {
    assertEquals("Mathematical", MultiplyOperator.INSTANCE.getCategory());
    assertEquals(IsTrueOperator.INSTANCE, new IsTrueOperator());
    assertEquals(ConcatFunction.INSTANCE, new ConcatFunction());
    assertEquals(51, MultiplyOperator.INSTANCE.getLeftPrec());
    assertEquals(50, MultiplyOperator.INSTANCE.getRightPrec());
    assertEquals("CONCAT", ConcatFunction.INSTANCE.toString());
    assertNotEquals(ConcatFunction.INSTANCE, EqualOperator.INSTANCE);
    assertTrue(ConcatFunction.INSTANCE.is(FunctionRegistry.getFunction("CONCAT")));
    assertFalse(ConcatFunction.INSTANCE.is(null));
    assertFalse(ConcatFunction.INSTANCE.isAggregate());
    assertTrue(ConcatFunction.INSTANCE.isDeterministic());
    assertNotNull(ConcatFunction.INSTANCE.getDescription());
    assertNotEquals(ConcatFunction.INSTANCE, null);
    // assertNotNull(ConcatFunction.INSTANCE.getDocumentation());
    assertNotNull(ConcatFunction.INSTANCE.getDocumentationUrl());
    assertTrue(FunctionRegistry.getFunction("TRUNCATE").is(FunctionRegistry.getFunction("TRUNC")));
    assertTrue(FunctionRegistry.getFunction("COUNT").isAggregate());
    assertNotEquals(InOperator.INSTANCE, NotInOperator.INSTANCE);
    assertNotEquals(SimilarToOperator.INSTANCE, NotSimilarToOperator.INSTANCE);
  }

  @Test
  void operatorNot() {
    assertEquals(IsNotNullOperator.INSTANCE, IsNullOperator.INSTANCE.not());
    assertEquals(IsNullOperator.INSTANCE, IsNotNullOperator.INSTANCE.not());
    assertEquals(IsNotTrueOperator.INSTANCE, IsTrueOperator.INSTANCE.not());
    assertEquals(IsTrueOperator.INSTANCE, IsNotTrueOperator.INSTANCE.not());
    assertEquals(IsNotFalseOperator.INSTANCE, IsFalseOperator.INSTANCE.not());
    assertEquals(IsFalseOperator.INSTANCE, IsNotFalseOperator.INSTANCE.not());
    assertEquals(EqualOperator.INSTANCE, NotEqualOperator.INSTANCE.not());
    assertEquals(NotEqualOperator.INSTANCE, EqualOperator.INSTANCE.not());
    assertEquals(LessThanOperator.INSTANCE, GreaterThanOrEqualOperator.INSTANCE.not());
    assertEquals(LessThanOrEqualOperator.INSTANCE, GreaterThanOperator.INSTANCE.not());
    assertEquals(GreaterThanOperator.INSTANCE, LessThanOrEqualOperator.INSTANCE.not());
    assertEquals(GreaterThanOrEqualOperator.INSTANCE, LessThanOperator.INSTANCE.not());
    assertEquals(IsDistinctFromOperator.INSTANCE, IsNotDistinctFromOperator.INSTANCE.not());
    assertEquals(IsNotDistinctFromOperator.INSTANCE, IsDistinctFromOperator.INSTANCE.not());
    assertEquals(SimilarToOperator.INSTANCE, NotSimilarToOperator.INSTANCE.not());
    assertEquals(NotSimilarToOperator.INSTANCE, SimilarToOperator.INSTANCE.not());
  }

  @Test
  void operatorReverse() {
    assertEquals(EqualOperator.INSTANCE, EqualOperator.INSTANCE.reverse());
    assertEquals(NotEqualOperator.INSTANCE, NotEqualOperator.INSTANCE.reverse());
    assertEquals(LessThanOrEqualOperator.INSTANCE, GreaterThanOrEqualOperator.INSTANCE.reverse());
    assertEquals(LessThanOperator.INSTANCE, GreaterThanOperator.INSTANCE.reverse());
    assertEquals(GreaterThanOrEqualOperator.INSTANCE, LessThanOrEqualOperator.INSTANCE.reverse());
    assertEquals(GreaterThanOperator.INSTANCE, LessThanOperator.INSTANCE.reverse());
    assertEquals(IsDistinctFromOperator.INSTANCE, IsDistinctFromOperator.INSTANCE.reverse());
    assertEquals(IsNotDistinctFromOperator.INSTANCE, IsNotDistinctFromOperator.INSTANCE.reverse());
    assertEquals(MultiplyOperator.INSTANCE, MultiplyOperator.INSTANCE.reverse());
    assertEquals(BoolAndOperator.INSTANCE, BoolAndOperator.INSTANCE.reverse());
    assertEquals(BoolOrOperator.INSTANCE, BoolOrOperator.INSTANCE.reverse());
    assertEquals(BoolXorOperator.INSTANCE, BoolXorOperator.INSTANCE.reverse());
    assertNull(ConcatFunction.INSTANCE.reverse());
  }

  @Test
  void operatorSymmetrical() {
    assertTrue(BoolAndOperator.INSTANCE.isSymmetrical());
    assertTrue(BoolOrOperator.INSTANCE.isSymmetrical());
    assertTrue(BoolXorOperator.INSTANCE.isSymmetrical());
    assertTrue(MultiplyOperator.INSTANCE.isSymmetrical());
    assertTrue(EqualOperator.INSTANCE.isSymmetrical());
    assertTrue(NotEqualOperator.INSTANCE.isSymmetrical());
    assertTrue(IsDistinctFromOperator.INSTANCE.isSymmetrical());
    assertTrue(IsNotDistinctFromOperator.INSTANCE.isSymmetrical());
    assertFalse(GreaterThanOperator.INSTANCE.isSymmetrical());
    assertTrue(FunctionRegistry.getFunction("EQUAL_NULL").isSymmetrical());
    assertTrue(FunctionRegistry.getFunction("BIT_AND").isSymmetrical());
    assertTrue(FunctionRegistry.getFunction("BIT_OR").isSymmetrical());
    assertTrue(FunctionRegistry.getFunction("BIT_XOR").isSymmetrical());
    assertTrue(FunctionRegistry.getFunction("GREATEST").isSymmetrical());
    assertTrue(FunctionRegistry.getFunction("LEAST").isSymmetrical());
  }

  @Test
  void operatorPrecedenceAndAssociativity() throws Exception {

    // Arithmetic
    evalEquals("1 + 2 * 3 * 4 + 5", 1 + 2 * 3 * 4 + 5L);
    evalEquals("1-2+3*4/5/6-7", 1 - 2 + 3 * 4d / 5d / 6d - 7);
    evalEquals("10*2+1", 21L);
    evalEquals("8+5-2*8", 8 + 5 - 2 * 8L);
    evalEquals("8+(5-2)*8", 8L + (5 - 2) * 8L);
    evalEquals("1+10*2", 1 + 10 * 2L);
    evalEquals("10*(2+1)", 30L);
    evalEquals("30/(5+5)", 3D);
    evalEquals("42%(3+2)", 2D);
    evalEquals("1-2+3*4/5/6-7", (((1d - 2d) + (((3d * 4d) / 5d) / 6d)) - 7d));
    evalEquals("FIELD_INTEGER-(10+3*10+50-2*25)", 0L);

    // Operators with the same precedence and adjacent, respect associativity without parenthesis
    evalEquals("3*5/2", 3 * 5 / 2d);
    evalEquals("9/3*3", 9L / 3L * 3d);
    evalEquals("-10*2/4+1", -10L * 2L / 4L + 1d);

    // NOT has higher precedence than AND, which has higher precedence than OR
    evalTrue("NOT false AND NOT false");
    evalTrue("NOT 5 = 5 OR NOT 'Test' = 'X' AND NOT 5 = 4");
    // AND has higher precedence than XOR
    evalTrue("true AND false XOR true");
    // XOR has higher precedence than OR
    evalTrue("false OR false XOR true");

    // Equals (=) has higher precedence than NOT "NOT (1=1)"
    evalTrue("NOT 2 = 1");

    // IS NULL has higher precedence than NOT
    evalFalse("NOT NULL_BOOLEAN IS NULL");

    // IS NULL has lower precedence than comparison (1 = 1) IS NULL
    evalFalse("1 = 1 is null");
    evalTrue(" 3 > 5 IS FALSE");

    // TODO: BETWEEN, IN, LIKE have higher precedence than comparison
    // evalFalse("5 between 4>=4 and 6<=6");

    // The cast operator has higher precedence than the unary minus (negation) operator,
    // so the statement is interpreted as -(0.0::NUMBER::BOOLEAN)
    evalFails("-0.0::NUMBER::BOOLEAN", ErrorCode.UNEXPECTED_CHARACTER);
    evalFalse("(-0.0::NUMBER)::BOOLEAN");
  }

  @Test
  void unparse() throws Exception {
    optimize("4*(2+FIELD_INTEGER)", "4*(2+FIELD_INTEGER)");
    optimize("-FIELD_INTEGER::NUMBER", "-CAST(FIELD_INTEGER AS NUMBER)");
    optimize("-2*(FIELD_INTEGER-4)*(FIELD_INTEGER/2)", "-2*(FIELD_INTEGER-4)*FIELD_INTEGER/2");
    optimize("((FIELD_INTEGER > 5) AND FALSE) OR (FIELD_INTEGER < 10)", "FIELD_INTEGER<10");
  }

  @Test
  void syntaxError() throws Exception {

    // Single quote for string
    evalFails("'T'||'T", ErrorCode.MISSING_END_SINGLE_QUOTED_STRING);
    evalFails("'missing end", ErrorCode.MISSING_END_SINGLE_QUOTED_STRING);

    // Double quote for identifier
    evalFails("\"T\"||\"T", ErrorCode.MISSING_END_DOUBLE_QUOTED_STRING);
    evalFails("\"", ErrorCode.MISSING_END_DOUBLE_QUOTED_STRING);
    evalFails(" \" ", ErrorCode.MISSING_END_DOUBLE_QUOTED_STRING);
    evalFails(" \"", ErrorCode.MISSING_END_DOUBLE_QUOTED_STRING);

    // Empty identifier
    evalFails("\"\"", ErrorCode.UNRESOLVED_IDENTIFIER);

    // Unbalanced parenthesis
    evalFails("(9+7", ErrorCode.MISSING_RIGHT_PARENTHESIS);
    evalFails("9+(", ErrorCode.MISSING_RIGHT_PARENTHESIS);
    evalFails("3*(1+2", ErrorCode.MISSING_RIGHT_PARENTHESIS);
    evalFails("Year(2020", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    evalFails("9+()", ErrorCode.SYNTAX_ERROR);
    evalFails("9+*(", ErrorCode.SYNTAX_ERROR);
    evalFails("*9", ErrorCode.SYNTAX_ERROR);
    // TODO: evalFails("DATE '2023-01-01'||'X'", ErrorCode.ILLEGAL_ARGUMENT);

    evalFails(")+1", ErrorCode.SYNTAX_ERROR);
    evalFails("Year(", ErrorCode.SYNTAX_ERROR_FUNCTION);

    evalFails("Year()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Year(()", ErrorCode.SYNTAX_ERROR);
    evalFails("Year+3", ErrorCode.ILLEGAL_ARGUMENT);

    evalFails("Today())", ErrorCode.UNEXPECTED_CHARACTER);
    evalFails("Year)", ErrorCode.UNEXPECTED_CHARACTER);
    evalFails("9!7", ErrorCode.UNEXPECTED_CHARACTER);
    evalFails("9: ", ErrorCode.UNEXPECTED_CHARACTER);

    evalFails("Date '2022-05-01' AT TIME", ErrorCode.SYNTAX_ERROR);
    evalFails("Date '2022-05-01' AT TIME ZONE", ErrorCode.SYNTAX_ERROR);
    evalFails("Date '2022-05-01' AT TIME ZONE 'XYZ'", ErrorCode.INVALID_TIMEZONE);
    evalFails("Year(1,2)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("TRUE AND", ErrorCode.SYNTAX_ERROR);
    evalFails("FIELD_BOOLEAN_TRUE IS ", ErrorCode.SYNTAX_ERROR);
    evalFails("FIELD_BOOLEAN_TRUE IS MONTH", ErrorCode.SYNTAX_ERROR);
    evalFails("FIELD_BOOLEAN_TRUE NOT IS NULL", ErrorCode.SYNTAX_ERROR);
    evalFails("IS ", ErrorCode.SYNTAX_ERROR);
    evalFails("IS AND", ErrorCode.SYNTAX_ERROR);
    evalFails("FIELD IS ", ErrorCode.SYNTAX_ERROR);

    evalFails("case when 1=1 then 1 else 0", ErrorCode.SYNTAX_ERROR_CASE_STATEMENT);
    evalFails("case when 1=1 then 1 else  end ", ErrorCode.SYNTAX_ERROR);
    evalFails("case 1 when 1  else 0 end", ErrorCode.SYNTAX_ERROR_CASE_STATEMENT);

    evalFails("BOOLEAN ", ErrorCode.RETURN_TYPE_UNKNOWN);
    evalFails("DATE ", ErrorCode.SYNTAX_ERROR);
    evalFails("TIME", ErrorCode.SYNTAX_ERROR);
    evalFails("TIMESTAMP", ErrorCode.SYNTAX_ERROR);
    evalFails("BINARY", ErrorCode.SYNTAX_ERROR);
    evalFails("JSON ", ErrorCode.SYNTAX_ERROR);

    evalFails("DATE FIELD_STRING", ErrorCode.INVALID_DATE);
    evalFails("TIMESTAMP FIELD_STRING", ErrorCode.INVALID_TIMESTAMP);

    evalFails("INTEGER", ErrorCode.RETURN_TYPE_UNKNOWN);
    evalFails("NUMBER", ErrorCode.RETURN_TYPE_UNKNOWN);
  }

  @Test
  void dataType() throws Exception {
    evalEquals("Cast(123 as InTeGeR)", 123L);
    evalEquals("Cast(123 as STRING(3))", "123");

    evalFails("Cast(12 as STRING(4", ErrorCode.MISSING_RIGHT_PARENTHESIS);
    evalFails("Cast(123 as STRING(3)", ErrorCode.MISSING_RIGHT_PARENTHESIS);
    evalFails("Cast(123 as STRING 3))", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    evalFails("Cast('TRUE' as BOOLEAN(4)", ErrorCode.INVALID_TYPE);
    evalFails("Cast('2023-12-31' as DATE(10)", ErrorCode.INVALID_TYPE);
  }
}
