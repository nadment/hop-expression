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
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.type.DataTypeName;
import org.junit.Test;
import java.util.List;

public class CallTest extends ExpressionTest {
 
  @Test
  public void test() throws Exception {
    Call call1 = new Call(Operators.ADD, List.of(Literal.of(3), Literal.of(5)));
    Call call2 = new Call(Operators.ADD, List.of(Literal.of(3), Literal.of(5)));
    Call call3 = new Call(DataTypeName.INTEGER, Operators.ADD, List.of(Literal.of(3), Literal.of(5)));
    assertEquals(Kind.CALL, call1.getKind());
    assertEquals(call1, call2);
    assertTrue(call1.is(Kind.CALL));
    assertTrue(call1.is(Operators.ADD));
    //assertEquals(call1.hashCode(), call2.hashCode()); 
    assertEquals(2, call1.getOperandCount());
    // Unknown before optimization
    assertEquals(DataTypeName.UNKNOWN, call1.getType());
    assertEquals(DataTypeName.INTEGER, call3.getType());
    assertNotEquals(call1, null);   
    assertEquals("3+5", call1.toString());    
  }
}