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
package org.apache.hop.core.expression;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.Identifier;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.Tuple;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.UnknownType;
import org.junit.Test;
import java.util.List;

public class CallTest extends ExpressionTest {
 
  @Test
  public void testCall() throws Exception {
    Call call1 = new Call(3, Operators.ADD, Literal.of(3), Literal.of(5));
    Call call2 = new Call(Operators.ADD, List.of(Literal.of(3), Literal.of(5)));
    
    Call call3 = new Call(Operators.ADD, Literal.of(3), Literal.of(6));
    
    Call call4 = new Call(Operators.ADD, Literal.of(3), new Call(Operators.ADD, Literal.of(3), new Identifier("Field")));
    Call call5 = new Call(Operators.ADD, Literal.of(3), new Call(Operators.ADD, Literal.of(3), new Identifier("Field")));
    
    Call call6 = new Call(Operators.IN, new Identifier("Field"), new Tuple(Literal.of(1), Literal.of(2)));
    Call call7 = new Call(Operators.IN, new Identifier("Field"), new Tuple(Literal.of(1), Literal.of(2)));
    Call call8 = new Call(FunctionRegistry.getFunction("RANDOM"));
       assertEquals(Kind.CALL, call1.getKind());
    assertEquals(call1, call2);
    assertTrue(call1.is(Kind.CALL));
    assertTrue(call1.is(Operators.ADD));
    assertTrue(call1.isConstant());
    assertFalse(call4.isConstant());
    assertFalse(call8.isConstant());
    //assertEquals(call1.hashCode(), call2.hashCode()); 
    assertEquals(Operators.ADD, call1.getOperator());
    assertEquals(2, call1.getOperandCount());
    assertEquals(3, call1.getPosition());
    assertEquals(7, call1.getCost());
    assertEquals(14, call4.getCost());
    assertEquals(11, call6.getCost());
    // Data type is unknown before validation
    assertEquals(UnknownType.UNKNOWN, call1.getType());
    assertEquals(UnknownType.UNKNOWN, call3.getType());
    assertNotEquals(call1, null);   
    assertEquals("3+5", call1.toString());

    assertEquals(call1, call2);
    assertEquals(call4, call5);
    assertEquals(call6, call7);
    assertNotEquals(call1, call3); 
  }

  @Test
  public void coercion() throws Exception {
    
    // Coercion to boolean
    evalTrue("FIELD_STRING_BOOLEAN_TRUE IS TRUE");
    evalTrue("FIELD_INTEGER IS TRUE");
    evalTrue("FIELD_NUMBER IS TRUE");
    evalTrue("Cast(Month(FIELD_DATE) as BOOLEAN)");
    evalTrue("Cast(FIELD_NUMBER::NUMBER as BOOLEAN)");
    
    // Coercion to string
    evalEquals("Upper(FIELD_BOOLEAN_TRUE::BOOLEAN)", "TRUE").returnType(StringType.STRING);
    evalEquals("Upper(FIELD_BOOLEAN_TRUE::STRING)", "TRUE");
    evalEquals("Upper(FIELD_BOOLEAN_TRUE::INTEGER)", "1");
    evalEquals("Upper(FIELD_BOOLEAN_TRUE::NUMBER)", "1");
    evalEquals("Lower(FIELD_INTEGER::BOOLEAN)", "true");
    evalEquals("Lower(FIELD_NUMBER::BOOLEAN)", "true");
    evalEquals("Upper(FIELD_NUMBER::BOOLEAN)", "TRUE");
    evalEquals("Upper(FIELD_BOOLEAN_TRUE::INTEGER)", "1");
    evalEquals("Upper(FIELD_INTEGER::INTEGER)", "40");
    
    // Coercion 
    evalEquals("Abs(FIELD_STRING_INTEGER)", 25L);    
    evalEquals("Abs(FIELD_STRING_NUMBER)", 12.56D);
  }
  
  @Test
  public void orderIdentifierByName() throws Exception {

    // Order identifiers by name with symmetrical operator
    optimize("FIELD_STRING_NUMBER+FIELD_STRING_INTEGER", "FIELD_STRING_INTEGER+FIELD_STRING_NUMBER");
    optimize("FIELD_STRING_NUMBER*FIELD_STRING_INTEGER", "FIELD_STRING_INTEGER*FIELD_STRING_NUMBER");
    optimize("FIELD_BOOLEAN_TRUE AND FIELD_BOOLEAN_FALSE", "FIELD_BOOLEAN_FALSE AND FIELD_BOOLEAN_TRUE");
    optimize("FIELD_BOOLEAN_TRUE OR FIELD_BOOLEAN_FALSE", "FIELD_BOOLEAN_FALSE OR FIELD_BOOLEAN_TRUE");
    optimize("FIELD_BOOLEAN_TRUE = FIELD_BOOLEAN_FALSE", "FIELD_BOOLEAN_FALSE=FIELD_BOOLEAN_TRUE");
    optimize("EQUAL_NULL(FIELD_BOOLEAN_TRUE,FIELD_BOOLEAN_FALSE)", "EQUAL_NULL(FIELD_BOOLEAN_FALSE,FIELD_BOOLEAN_TRUE)");
  }

  @Test
  public void orderOperandByCost() throws Exception {

    // Order operands by cost with symmetrical operator
    optimize("FIELD_STRING_NUMBER+3", "3+FIELD_STRING_NUMBER");
    optimize("FIELD_STRING_NUMBER*3", "3*FIELD_STRING_NUMBER");
  }
}