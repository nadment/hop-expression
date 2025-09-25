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

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import org.apache.hop.expression.operator.AddOperator;
import org.apache.hop.expression.operator.InListOperator;
import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.UnknownType;
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
    assertEquals(UnknownType.UNKNOWN, call1.getType());
    assertEquals(UnknownType.UNKNOWN, call3.getType());
    assertNotEquals(null, call1);
    assertEquals("3+5", call1.toString());

    assertEquals(call1, call2);
    assertEquals(call4, call5);
    assertEquals(call6, call7);
    assertNotEquals(call1, call3);
  }

  @Test
  void testImplicitCoercionToBoolean() throws Exception {
    evalTrue("FIELD_INTEGER IS TRUE");
    evalTrue("FIELD_NUMBER IS TRUE");
  }

  @Test
  void testImplicitCoercionToInteger() throws Exception {
    evalEquals("Abs(FIELD_BOOLEAN_TRUE)", 1L).returnType(IntegerType.INTEGER);
    evalEquals("1+FALSE", 1L);
    evalEquals("TRUE+1", 2L);
  }

  @Test
  void testImplicitCoercionToNumber() throws Exception {
    evalEquals("Cos(FIELD_BOOLEAN_FALSE)", BigDecimal.ONE).returnType(NumberType.NUMBER);
    evalEquals("Cos(FIELD_INTEGER)", new BigDecimal("-0.6669380616522618443840927819337"))
        .returnType(NumberType.NUMBER);
  }

  @Test
  void testImplicitCoercionToDate() throws Exception {
    evalEquals("FIRST_DAY(FIELD_TIMESTAMP)", LocalDate.of(2023, 2, 1)).returnType(DateType.DATE);
  }

  @Test
  void testImplicitCoercionToString() throws Exception {
    evalEquals("Upper(FIELD_BOOLEAN_TRUE)", "TRUE").returnType(StringType.STRING);
    evalEquals("Upper(FIELD_INTEGER)", "40").returnType(StringType.STRING);
    evalEquals("Upper(FIELD_NUMBER)", "-5.12").returnType(StringType.STRING);
    evalEquals("Upper(FIELD_DATE)", "1981-06-23 21:44:58").returnType(StringType.STRING);
    evalEquals("Upper(FIELD_TIMESTAMP)", "2023-02-28 22:11:01").returnType(StringType.STRING);
    evalEquals("Upper(FIELD_INET)", "10.10.10.1").returnType(StringType.STRING);
    evalEquals("Upper(FIELD_BINARY)", "TEST").returnType(StringType.STRING);
    evalEquals(
            "Upper(FIELD_JSON)",
            "{\"STORE\":{\"BOOK\":[{\"CATEGORY\":\"REFERENCE\",\"AUTHOR\":\"NIGEL REES\",\"TITLE\":\"SAYINGS OF THE CENTURY\",\"PRICE\":8.95},{\"CATEGORY\":\"FICTION\",\"AUTHOR\":\"EVELYN WAUGH\",\"TITLE\":\"SWORD OF HONOUR\",\"PRICE\":12.99},{\"CATEGORY\":\"FICTION\",\"AUTHOR\":\"HERMAN MELVILLE\",\"TITLE\":\"MOBY DICK\",\"ISBN\":\"0-553-21311-3\",\"PRICE\":8.99},{\"CATEGORY\":\"FICTION\",\"AUTHOR\":\"J. R. R. TOLKIEN\",\"TITLE\":\"THE LORD OF THE RINGS\",\"ISBN\":\"0-395-19395-8\",\"PRICE\":22.99}],\"BICYCLE\":{\"COLOR\":\"RED\",\"PRICE\":19.95}}}")
        .returnType(StringType.STRING);
  }

  @Test
  void testImplicitCoercionToBinary() throws Exception {}

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
