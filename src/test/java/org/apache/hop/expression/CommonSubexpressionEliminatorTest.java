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
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.util.BitSet;
import org.junit.jupiter.api.Test;

class CommonSubexpressionEliminatorTest extends ExpressionTest {

  /** The index of FIELD_STRING in the row. */
  private static final int FIELD_STRING = 0;

  @Test
  void shareInExpression() throws Exception {
    IRowExpressionContext context = createExpressionContext();
    String source = "UPPER(FIELD_STRING)||LOWER(FIELD_STRING)||UPPER(FIELD_STRING)";
    IExpression expression = IExpression.of(context, source);

    Call call = (Call) expression;
    assertInstanceOf(SharedExpression.class, call.getOperand(0));
    assertSame(call.getOperand(0), call.getOperand(2));
    assertFalse(call.getOperand(1) instanceof SharedExpression);
    assertEquals(source, expression.toString());
    assertEquals("TESTtestTEST", expression.getValue());

    // The value is computed again for a new row
    Object[] row = context.getRow().clone();
    row[FIELD_STRING] = "Other";
    context.setRow(row);
    assertEquals("OTHERotherOTHER", expression.getValue());
  }

  @Test
  void shareOnlyOutermostCall() throws Exception {
    IExpression expression =
        IExpression.of(
            createExpressionContext(),
            "REVERSE(UPPER(FIELD_STRING))||'-'||REVERSE(UPPER(FIELD_STRING))");

    Call call = (Call) expression;
    SharedExpression shared = assertInstanceOf(SharedExpression.class, call.getOperand(0));
    assertSame(shared, call.getOperand(2));

    // UPPER is only found in the shared call, so it's already evaluated once
    Call reverse = (Call) shared.getExpression();
    assertFalse(reverse.getOperand(0) instanceof SharedExpression);
    assertEquals("TSET-TSET", expression.getValue());
  }

  @Test
  void doNotShareNonDeterministicCall() throws Exception {
    IExpression expression =
        IExpression.of(
            createExpressionContext(), "RANDOM()*FIELD_INTEGER||'-'||RANDOM()*FIELD_INTEGER");

    Call call = (Call) expression;
    assertFalse(call.getOperand(0) instanceof SharedExpression);
    assertNotSame(call.getOperand(0), call.getOperand(2));
  }

  @Test
  void keepLazyEvaluation() throws Exception {
    // The shared division by zero is in a branch that is not evaluated
    IExpression expression =
        IExpression.of(
            createExpressionContext(),
            "CASE WHEN FIELD_INTEGER=40 THEN 'A' "
                + "ELSE TO_CHAR(1/(FIELD_INTEGER-40))||TO_CHAR(1/(FIELD_INTEGER-40)) END");

    assertEquals("A", expression.getValue());
  }

  @Test
  void shareBetweenExpressions() throws Exception {
    IRowExpressionContext context = createExpressionContext();
    IExpression[] expressions = {
      IExpression.of(context, "UPPER(FIELD_STRING)"),
      null,
      IExpression.of(context, "UPPER(FIELD_STRING)||UPPER(FIELD_STRING)")
    };

    CommonSubexpressionEliminator.eliminate(context, expressions, new BitSet());

    SharedExpression shared = assertInstanceOf(SharedExpression.class, expressions[0]);
    Call call = (Call) expressions[2];
    assertSame(shared, call.getOperand(0));
    assertSame(shared, call.getOperand(1));
    assertEquals("TEST", expressions[0].getValue());
    assertEquals("TESTTEST", expressions[2].getValue());
  }

  @Test
  void doNotShareCallReadingModifiedField() throws Exception {
    IRowExpressionContext context = createExpressionContext();
    IExpression[] expressions = {
      IExpression.of(context, "UPPER(FIELD_STRING)"),
      IExpression.of(context, "UPPER(FIELD_STRING)||UPPER(FIELD_STRING)")
    };
    BitSet modifiedOrdinals = new BitSet();
    modifiedOrdinals.set(FIELD_STRING);

    CommonSubexpressionEliminator.eliminate(context, expressions, modifiedOrdinals);

    // The calls shared when the second expression was compiled are no longer shared
    assertFalse(expressions[0] instanceof SharedExpression);
    Call call = (Call) expressions[1];
    assertFalse(call.getOperand(0) instanceof SharedExpression);
    assertFalse(call.getOperand(1) instanceof SharedExpression);
    assertEquals("TESTTEST", expressions[1].getValue());
  }
}
