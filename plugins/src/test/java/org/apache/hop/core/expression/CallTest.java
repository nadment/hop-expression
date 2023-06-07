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
import org.apache.hop.expression.Identifier;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.Tuple;
import org.apache.hop.expression.type.UnknownDataType;
import org.junit.Test;

public class CallTest extends ExpressionTest {
 
  @Test
  public void test() throws Exception {
    Call call1 = new Call(Operators.ADD, Literal.of(3), Literal.of(5));
    Call call2 = new Call(Operators.ADD, Literal.of(3), Literal.of(5));
    Call call3 = new Call(Operators.ADD, Literal.of(3), Literal.of(6));
    
    Call call4 = new Call(Operators.ADD, Literal.of(3), new Call(Operators.ADD, Literal.of(3), new Identifier("Field")));
    Call call5 = new Call(Operators.ADD, Literal.of(3), new Call(Operators.ADD, Literal.of(3), new Identifier("Field")));
    
    Call call6 = new Call(Operators.IN, new Identifier("Field"), new Tuple(Literal.of(1), Literal.of(2)));
    Call call7 = new Call(Operators.IN, new Identifier("Field"), new Tuple(Literal.of(1), Literal.of(2)));
    
    assertEquals(Kind.CALL, call1.getKind());
    assertEquals(call1, call2);
    assertTrue(call1.is(Kind.CALL));
    assertTrue(call1.is(Operators.ADD));
    assertTrue(call1.isConstant());
    assertFalse(call4.isConstant());
    //assertEquals(call1.hashCode(), call2.hashCode()); 
    assertEquals(2, call1.getOperandCount());
    // Data type is unknown before validation
    assertEquals(UnknownDataType.UNKNOWN, call1.getType());
    assertEquals(UnknownDataType.UNKNOWN, call3.getType());
    assertNotEquals(call1, null);   
    assertEquals("3+5", call1.toString());
    assertEquals(call1, call2);
    assertEquals(call4, call5);
    assertEquals(call6, call7);
    assertNotEquals(call1, call3); 
  }
}