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
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import org.apache.hop.expression.operator.AddOperator;
import org.apache.hop.expression.operator.InListOperator;
import org.apache.hop.expression.type.Types;
import org.junit.jupiter.api.Test;

class CallTest extends ExpressionTest {

  @Test
  void testCall() {
    Call call1 = new Call(3, AddOperator.INSTANCE, Literal.of(3), Literal.of(5));
    Call call2 = new Call(AddOperator.INSTANCE, List.of(Literal.of(3), Literal.of(5)));

    Call call3 = new Call(AddOperator.INSTANCE, Literal.of(3), Literal.of(6));

    Call call4 =
        new Call(
            AddOperator.INSTANCE,
            Literal.of(3),
            new Call(AddOperator.INSTANCE, Literal.of(3), new Identifier("Field")));
    Call call5 =
        new Call(
            AddOperator.INSTANCE,
            Literal.of(3),
            new Call(AddOperator.INSTANCE, Literal.of(3), new Identifier("Field")));

    Call call6 =
        new Call(
            InListOperator.INSTANCE,
            new Identifier("Field"),
            new Array(Literal.of(1), Literal.of(2)));
    Call call7 =
        new Call(
            InListOperator.INSTANCE,
            new Identifier("Field"),
            new Array(Literal.of(1), Literal.of(2)));
    Call call8 = new Call(FunctionRegistry.getFunction("RANDOM"));
    assertEquals(Kind.CALL, call1.getKind());
    assertEquals(call1, call2);
    assertTrue(call1.is(Kind.CALL));
    assertTrue(call1.isOperator(AddOperator.INSTANCE));
    assertTrue(call1.isConstant());
    assertFalse(call4.isConstant());
    assertFalse(call8.isConstant());
    // assertEquals(call1.hashCode(), call2.hashCode());
    assertEquals(AddOperator.INSTANCE, call1.getOperator());
    assertEquals(2, call1.getOperandCount());
    assertEquals(3, call1.getPosition());
    assertEquals(7, call1.getCost());
    assertEquals(14, call4.getCost());
    assertEquals(11, call6.getCost());

    // Data type is unknown before validation
    assertEquals(Types.UNKNOWN, call1.getType());
    assertEquals(Types.UNKNOWN, call3.getType());
    assertNotEquals(null, call1);
    assertEquals("3+5", call1.toString());

    assertEquals(call1, call2);
    assertEquals(call4, call5);
    assertEquals(call6, call7);
    assertNotEquals(call1, call3);
  }

  @Test
  void testCoercion() throws Exception {

    // Coercion to boolean
    // evalTrue("FIELD_STRING_BOOLEAN_TRUE::BOOLEAN IS TRUE");
    // evalTrue("FIELD_INTEGER::BOOLEAN IS TRUE");
    // evalTrue("FIELD_NUMBER IS TRUE");
    // evalTrue("Cast(Month(FIELD_DATE) as BOOLEAN)");
    // evalTrue("Cast(FIELD_NUMBER::NUMBER as BOOLEAN)");

    // Coercion to string
    //    evalEquals("Upper(FIELD_BOOLEAN_TRUE::BOOLEAN)", "TRUE").returnType(Types.STRING);
    //    evalEquals("Upper(FIELD_BOOLEAN_TRUE::STRING)", "TRUE");
    //    evalEquals("Upper(FIELD_BOOLEAN_TRUE::INTEGER)", "1");
    //    evalEquals("Upper(FIELD_BOOLEAN_TRUE::NUMBER)", "1");
    //    evalEquals("Lower(FIELD_INTEGER::BOOLEAN)", "true");
    //    evalEquals("Lower(FIELD_NUMBER::BOOLEAN)", "true");
    //    evalEquals("Upper(FIELD_NUMBER::BOOLEAN)", "TRUE");
    //    evalEquals("Upper(FIELD_BOOLEAN_TRUE::INTEGER)", "1");
    //    evalEquals("Upper(FIELD_INTEGER::INTEGER)", "40");

    // Coercion boolean to integer
    evalEquals("Abs(FIELD_BOOLEAN_TRUE)", 1L);
    // evalEquals("Abs(FIELD_STRING_NUMBER)", 12.56D);
  }

  @Test
  void normalizeReversibleOperator() throws Exception {

    // Normalize identifiers by name
    optimize(
        "FIELD_STRING_NUMBER<FIELD_STRING_INTEGER", "FIELD_STRING_INTEGER>FIELD_STRING_NUMBER");
    optimize(
        "FIELD_STRING_NUMBER<=FIELD_STRING_INTEGER", "FIELD_STRING_INTEGER>=FIELD_STRING_NUMBER");
  }

  @Test
  void normalizeSymmetricalOperator() throws Exception {

    // Normalize identifiers by name
    optimize("FIELD_NUMBER+FIELD_INTEGER", "FIELD_INTEGER+FIELD_NUMBER");
    optimize(
        "FIELD_BOOLEAN_TRUE AND FIELD_BOOLEAN_FALSE", "FIELD_BOOLEAN_FALSE AND FIELD_BOOLEAN_TRUE");
    optimize(
        "FIELD_BOOLEAN_TRUE OR FIELD_BOOLEAN_FALSE", "FIELD_BOOLEAN_FALSE OR FIELD_BOOLEAN_TRUE");
    optimize("FIELD_BOOLEAN_TRUE = FIELD_BOOLEAN_FALSE", "FIELD_BOOLEAN_FALSE=FIELD_BOOLEAN_TRUE");
    optimize(
        "EQUAL_NULL(FIELD_BOOLEAN_TRUE,FIELD_BOOLEAN_FALSE)",
        "EQUAL_NULL(FIELD_BOOLEAN_FALSE,FIELD_BOOLEAN_TRUE)");

    // Normalize operands by cost
    optimize("1+FIELD_NUMBER+3", "4+FIELD_NUMBER");
    optimize("FIELD_NUMBER>3 AND TRUE", "TRUE AND FIELD_NUMBER>3");
    optimize("2::INTEGER*FIELD_NUMBER*3::NUMBER", "6*FIELD_NUMBER");
  }
}
