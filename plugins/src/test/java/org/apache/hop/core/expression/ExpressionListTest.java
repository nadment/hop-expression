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
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionList;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.junit.Test;

public class ExpressionListTest extends BaseExpressionTest {
 
  @Test
  public void test() throws Exception {
    ExpressionList list0 = new ExpressionList();
    ExpressionList list1 = new ExpressionList(Literal.ONE, Literal.ZERO, Literal.NULL);
    ExpressionList list2 = new ExpressionList(Literal.ONE, Literal.ZERO, Literal.NULL);

    assertEquals(Kind.LIST, list1.getKind());
    assertTrue(list0.isEmpty());
    assertFalse(list1.isEmpty());
    assertEquals(list1, list2);
    assertNotEquals(list1, null);
    assertEquals(list1.hashCode(), list2.hashCode());    
    assertEquals("(1,0,NULL)", list1.toString());
    
    // Not evaluable alone
    assertThrows(ExpressionException.class, () -> list1.eval(createExpressionContext()));      
  }
}