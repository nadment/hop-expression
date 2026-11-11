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
 * or implied. See the License for the specific language governing permissions and limitations
 * under the License.
 */
package org.apache.hop.ui.expression;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.util.List;
import org.apache.hop.expression.ExpressionExtension;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.operator.AddOperator;
import org.apache.hop.expression.operator.BetweenAsymmetricOperator;
import org.apache.hop.expression.operator.BoolNotFunction;
import org.apache.hop.expression.operator.CaseSearchOperator;
import org.apache.hop.expression.operator.InListOperator;
import org.apache.hop.expression.operator.IsNullOperator;
import org.apache.hop.expression.operator.LessThanOperator;
import org.apache.hop.expression.operator.LessThanOrEqualOperator;
import org.apache.hop.expression.operator.SubtractOperator;
import org.apache.hop.junit.rules.RestoreHopEnvironmentExtension;
import org.eclipse.jface.text.Document;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith({RestoreHopEnvironmentExtension.class, ExpressionExtension.class})
class OperatorFinderTest {

  private static final Operator NOT = new BoolNotFunction("NOT");

  private static final List<Operator> OPERATORS =
      List.of(
          AddOperator.INSTANCE,
          SubtractOperator.INSTANCE,
          LessThanOperator.INSTANCE,
          LessThanOrEqualOperator.INSTANCE,
          BetweenAsymmetricOperator.INSTANCE,
          InListOperator.INSTANCE,
          IsNullOperator.INSTANCE,
          CaseSearchOperator.INSTANCE,
          NOT);

  private static Operator find(String source, int offset) {
    return OperatorFinder.find(OPERATORS, new Document(source), offset);
  }

  @Test
  void symbol() {
    // The caret before or after the symbol
    assertSame(AddOperator.INSTANCE, find("A + B", 2));
    assertSame(AddOperator.INSTANCE, find("A + B", 3));
    assertSame(AddOperator.INSTANCE, find("A+B", 1));
    assertNull(find("A + B", 0));
    assertNull(find("A + B", 5));

    // The longest symbol
    assertSame(LessThanOperator.INSTANCE, find("A < B", 2));
    assertSame(LessThanOrEqualOperator.INSTANCE, find("A <= B", 2));
    assertSame(LessThanOrEqualOperator.INSTANCE, find("A <= B", 3));
    assertSame(LessThanOrEqualOperator.INSTANCE, find("A <= B", 4));

    // The arrow of a lambda is not an operator
    assertNull(find("x -> x", 2));
    assertNull(find("x -> x", 3));
    assertSame(SubtractOperator.INSTANCE, find("x - 1", 2));
  }

  @Test
  void keyword() {
    assertSame(BetweenAsymmetricOperator.INSTANCE, find("A BETWEEN 1 AND 2", 4));
    assertSame(BetweenAsymmetricOperator.INSTANCE, find("A between 1 AND 2", 9));
    assertSame(InListOperator.INSTANCE, find("A IN (1,2)", 3));
    assertSame(CaseSearchOperator.INSTANCE, find("CASE WHEN A THEN 1 END", 0));
    assertSame(NOT, find("NOT A", 1));

    // A keyword in a longer word is not an operator
    assertNull(find("BEGIN", 4));
    assertNull(find("INDEX", 1));
    assertNull(find("NOTE", 1));
  }

  @Test
  void keywords() {
    assertSame(IsNullOperator.INSTANCE, find("A IS NULL", 3));
    assertSame(IsNullOperator.INSTANCE, find("A IS NULL", 7));
    assertSame(IsNullOperator.INSTANCE, find("A IS   null", 9));

    // With NOT, the longest name wins
    assertSame(IsNullOperator.INSTANCE, find("A IS NOT NULL", 3));
    assertSame(IsNullOperator.INSTANCE, find("A IS NOT NULL", 6));
    assertSame(IsNullOperator.INSTANCE, find("A IS NOT NULL", 11));
    assertSame(NOT, find("A NOT IN (1,2)", 3));
    assertSame(InListOperator.INSTANCE, find("A NOT IN (1,2)", 7));
  }
}
