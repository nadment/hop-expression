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

import org.apache.hop.expression.operator.AddOperator;
import org.apache.hop.expression.operator.BetweenAsymmetricOperator;
import org.apache.hop.expression.operator.BoolAndOperator;
import org.apache.hop.expression.operator.BoolOrOperator;
import org.apache.hop.expression.operator.ConcatFunction;
import org.apache.hop.expression.operator.DivOperator;
import org.apache.hop.expression.operator.EqualOperator;
import org.apache.hop.expression.operator.IsNullOperator;
import org.apache.hop.expression.operator.LikeOperator;
import org.apache.hop.expression.operator.ModFunction;
import org.apache.hop.expression.operator.MultiplyOperator;
import org.apache.hop.expression.operator.NegativeOperator;
import org.apache.hop.expression.operator.PositiveOperator;
import org.apache.hop.expression.operator.SubtractOperator;
import org.apache.hop.junit.rules.RestoreHopEnvironmentExtension;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Checks that unparsing adds parentheses only where the precedence or the associativity of the
 * operators requires it, so that the unparsed expression is parsed back to the same tree.
 */
// The functions are registered to parse the calls to functions
@ExtendWith({RestoreHopEnvironmentExtension.class, ExpressionExtension.class})
class UnparseTest {
  private static final IExpression A = new Identifier("A");
  private static final IExpression B = new Identifier("B");
  private static final IExpression C = new Identifier("C");
  private static final IExpression D = new Identifier("D");

  private static Call call(Operator operator, IExpression... operands) {
    return new Call(operator, operands);
  }

  private static IExpression parse(String source) {
    return new ExpressionParser(new ExpressionLexer(source)).parse();
  }

  /** Asserts the unparsed tree and that it is parsed back to the same tree. */
  private static void assertUnparse(String expected, IExpression expression) {
    assertEquals(expected, expression.toString());
    assertEquals(expression, parse(expected));
  }

  /** Asserts that the source is parsed, unparsed and parsed back to the same tree. */
  private static void assertRoundTrip(String expected, String source) {
    IExpression expression = parse(source);
    assertEquals(expected, expression.toString());
    assertEquals(expression, parse(expected));
  }

  @Test
  void precedence() {
    assertUnparse("A*(B+C)", call(MultiplyOperator.INSTANCE, A, call(AddOperator.INSTANCE, B, C)));
    assertUnparse("(A+B)*C", call(MultiplyOperator.INSTANCE, call(AddOperator.INSTANCE, A, B), C));
    assertUnparse("A+B*C", call(AddOperator.INSTANCE, A, call(MultiplyOperator.INSTANCE, B, C)));
    assertUnparse("A*B+C", call(AddOperator.INSTANCE, call(MultiplyOperator.INSTANCE, A, B), C));
    assertUnparse("A+B=C", call(EqualOperator.INSTANCE, call(AddOperator.INSTANCE, A, B), C));
    assertUnparse(
        "A OR B AND C", call(BoolOrOperator.INSTANCE, A, call(BoolAndOperator.INSTANCE, B, C)));
    assertUnparse(
        "A AND (B OR C)", call(BoolAndOperator.INSTANCE, A, call(BoolOrOperator.INSTANCE, B, C)));
    assertUnparse(
        "(A OR B) AND C", call(BoolAndOperator.INSTANCE, call(BoolOrOperator.INSTANCE, A, B), C));
  }

  @Test
  void leftAssociativity() {
    // Operators of the same precedence on the left side don't need parentheses
    assertUnparse(
        "A-B-C", call(SubtractOperator.INSTANCE, call(SubtractOperator.INSTANCE, A, B), C));
    assertUnparse("A-B+C", call(AddOperator.INSTANCE, call(SubtractOperator.INSTANCE, A, B), C));
    assertUnparse("A/B*C", call(MultiplyOperator.INSTANCE, call(DivOperator.INSTANCE, A, B), C));
    assertUnparse(
        "A AND B AND C", call(BoolAndOperator.INSTANCE, call(BoolAndOperator.INSTANCE, A, B), C));

    // Operators of the same precedence on the right side need parentheses
    assertUnparse(
        "A-(B-C)", call(SubtractOperator.INSTANCE, A, call(SubtractOperator.INSTANCE, B, C)));
    assertUnparse("A-(B+C)", call(SubtractOperator.INSTANCE, A, call(AddOperator.INSTANCE, B, C)));
    assertUnparse("A+(B-C)", call(AddOperator.INSTANCE, A, call(SubtractOperator.INSTANCE, B, C)));
    assertUnparse("A/(B*C)", call(DivOperator.INSTANCE, A, call(MultiplyOperator.INSTANCE, B, C)));
    assertUnparse(
        "A AND (B AND C)", call(BoolAndOperator.INSTANCE, A, call(BoolAndOperator.INSTANCE, B, C)));
  }

  @Test
  void infixFunctions() {
    // The modulus has the precedence of the multiplication
    assertUnparse("A%(B*C)", call(ModFunction.INSTANCE, A, call(MultiplyOperator.INSTANCE, B, C)));
    assertUnparse("A*(B%C)", call(MultiplyOperator.INSTANCE, A, call(ModFunction.INSTANCE, B, C)));
    assertUnparse("A*B%C", call(ModFunction.INSTANCE, call(MultiplyOperator.INSTANCE, A, B), C));

    // The concatenation has the precedence of the addition
    assertUnparse("A||(B+C)", call(ConcatFunction.INSTANCE, A, call(AddOperator.INSTANCE, B, C)));
    assertUnparse("A+B||C", call(ConcatFunction.INSTANCE, call(AddOperator.INSTANCE, A, B), C));
    assertUnparse(
        "(A||B)*C", call(MultiplyOperator.INSTANCE, call(ConcatFunction.INSTANCE, A, B), C));
    assertEquals("A||B||C", call(ConcatFunction.INSTANCE, A, B, C).toString());
  }

  @Test
  void nonAssociative() {
    // Comparisons can't be chained
    assertUnparse("A=(B=C)", call(EqualOperator.INSTANCE, A, call(EqualOperator.INSTANCE, B, C)));
    assertUnparse("(A=B)=C", call(EqualOperator.INSTANCE, call(EqualOperator.INSTANCE, A, B), C));

    // IS NULL can't be chained
    assertUnparse(
        "(A IS NULL) IS NULL", call(IsNullOperator.INSTANCE, call(IsNullOperator.INSTANCE, A)));
    assertUnparse("A=B IS NULL", call(IsNullOperator.INSTANCE, call(EqualOperator.INSTANCE, A, B)));
    assertUnparse(
        "(A IS NULL)=B", call(EqualOperator.INSTANCE, call(IsNullOperator.INSTANCE, A), B));

    // LIKE and BETWEEN can't be chained
    assertUnparse(
        "(A LIKE B) LIKE C", call(LikeOperator.INSTANCE, call(LikeOperator.INSTANCE, A, B), C));
    assertUnparse(
        "A BETWEEN B+C AND (C AND D)",
        call(
            BetweenAsymmetricOperator.INSTANCE,
            A,
            call(AddOperator.INSTANCE, B, C),
            call(BoolAndOperator.INSTANCE, C, D)));
  }

  @Test
  void unaryMinus() {
    assertUnparse("-A*B", call(MultiplyOperator.INSTANCE, call(NegativeOperator.INSTANCE, A), B));
    assertUnparse("A*-B", call(MultiplyOperator.INSTANCE, A, call(NegativeOperator.INSTANCE, B)));
    assertUnparse("A+-B", call(AddOperator.INSTANCE, A, call(NegativeOperator.INSTANCE, B)));
    assertUnparse("-(A+B)", call(NegativeOperator.INSTANCE, call(AddOperator.INSTANCE, A, B)));

    // Unary operators are right-associative, but a unary operand is enclosed for readability
    assertUnparse("+(-A)", call(PositiveOperator.INSTANCE, call(NegativeOperator.INSTANCE, A)));
    assertUnparse("-(+A)", call(NegativeOperator.INSTANCE, call(PositiveOperator.INSTANCE, A)));
    assertUnparse("-(-A)", call(NegativeOperator.INSTANCE, call(NegativeOperator.INSTANCE, A)));

    // Never generate "--" which starts a single line comment
    assertUnparse("A-(-B)", call(SubtractOperator.INSTANCE, A, call(NegativeOperator.INSTANCE, B)));
    assertUnparse(
        "-(-1)", call(NegativeOperator.INSTANCE, call(NegativeOperator.INSTANCE, Literal.of(1L))));
    assertUnparse(
        "A-(-1)",
        call(SubtractOperator.INSTANCE, A, call(NegativeOperator.INSTANCE, Literal.of(1L))));

    // A negative literal is parsed back as a negation, so only the text is checked
    assertEquals("-(-1)", call(NegativeOperator.INSTANCE, Literal.of(-1L)).toString());
    assertEquals("A-(-1)", call(SubtractOperator.INSTANCE, A, Literal.of(-1L)).toString());
    assertEquals("+(-1)", call(PositiveOperator.INSTANCE, Literal.of(-1L)).toString());
  }

  @Test
  void roundTrip() {
    assertRoundTrip("(A+B)::INTEGER", "(A+B)::INTEGER");
    assertRoundTrip("A::STRING::INTEGER", "(A::STRING)::INTEGER");
    // The cast binds more tightly than the array element selection
    assertRoundTrip("A::STRING[1]", "(A::STRING)[1]");
    assertRoundTrip("(A[1])::INTEGER", "A[1]::INTEGER");
    assertRoundTrip("A::DATE AT TIME ZONE 'UTC'", "(A::DATE) AT TIME ZONE 'UTC'");
    assertRoundTrip("A AT TIME ZONE B::STRING", "A AT TIME ZONE (B::STRING)");
    assertRoundTrip("-A AT TIME ZONE B", "-(A AT TIME ZONE B)");
    assertRoundTrip("(-A) AT TIME ZONE B", "(-A) AT TIME ZONE B");
    assertRoundTrip("(-A)::INTEGER", "(-A)::INTEGER");
    assertRoundTrip("-A::INTEGER", "-A::INTEGER");
    assertRoundTrip("(A||B)[1]", "(A||B)[1]");
    assertRoundTrip("A[B+1]", "A[B+1]");
    assertRoundTrip("A[1][2]", "(A[1])[2]");
    assertRoundTrip("(A+B) AT TIME ZONE 'UTC'", "(A+B) AT TIME ZONE 'UTC'");
    assertRoundTrip("A-B-C-D", "((A-B)-C)-D");
    assertRoundTrip("A-(B-(C-D))", "A-(B-(C-D))");
    assertRoundTrip("A*(B/C)%D", "(A*(B/C))%D");
    assertRoundTrip("A=B AND C!=D", "(A=B) AND (C<>D)");
    assertRoundTrip("(A=B)!=(C=D)", "(A=B)<>(C=D)");
    assertRoundTrip("A IN (1,2) AND B", "(A IN (1,2)) AND B");
    assertRoundTrip("A IN (1,2)=B", "(A IN (1,2))=B");
    assertRoundTrip("A+1 IN (1,2)", "(A+1) IN (1,2)");
    assertRoundTrip("A IS DISTINCT FROM B OR C", "(A IS DISTINCT FROM B) OR C");
    assertRoundTrip("A OR B AND C XOR D", "A OR ((B AND C) XOR D)");
  }

  @Test
  void lambda() {
    // The body extends as far as possible, no parentheses are needed
    assertRoundTrip("TRANSFORM(A,x->x*2+B)", "TRANSFORM(A, x -> (x*2)+B)");
    assertRoundTrip("FILTER(A,x->x>0 AND x<B)", "FILTER(A, x -> (x > 0) AND (x < B))");
    // Nested lambdas, the inner parameter hides the outer one
    assertRoundTrip("TRANSFORM(A,x->FILTER(B,y->x=y))", "TRANSFORM(A, x -> FILTER(B, y -> x = y))");
    assertRoundTrip("TRANSFORM(A,x->FILTER(x,x->x>0))", "TRANSFORM(A, x -> FILTER(x, x -> x > 0))");
    // A parameter named like a reserved word is quoted
    assertRoundTrip("TRANSFORM(A,\"YEAR\"->\"YEAR\"+1)", "TRANSFORM(A, \"YEAR\" -> \"YEAR\" + 1)");

    // Several parameters are enclosed in parentheses, a single one is not
    assertRoundTrip("TRANSFORM(A,(x,y)->x*y+B)", "TRANSFORM(A, (x, y) -> x * y + B)");
    assertRoundTrip("TRANSFORM(A,x->x+1)", "TRANSFORM(A, (x) -> x + 1)");
    assertRoundTrip(
        "TRANSFORM(A,(x,y)->FILTER(B,z->x=z OR y=z))",
        "TRANSFORM(A, (x, y) -> FILTER(B, z -> x = z OR y = z))");

    // A parenthesis that doesn't start the parameters of a lambda
    assertRoundTrip("A*2", "(A)*2");
    assertRoundTrip("TRANSFORM(A,x->(x+1)*2)", "TRANSFORM(A, x -> (x + 1) * 2)");
    assertRoundTrip("TRANSFORM(A,x->x IN (B,C))", "TRANSFORM(A, x -> x IN (B, C))");
  }
}
