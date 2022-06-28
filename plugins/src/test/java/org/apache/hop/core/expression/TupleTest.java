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
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Tuple;
import org.junit.Test;

public class TupleTest extends BaseExpressionTest {
 
  @Test
  public void test() throws Exception {
    Tuple tuple0 = new Tuple();
    Tuple tuple1 = new Tuple(Literal.ONE, Literal.ZERO, Literal.NULL);
    Tuple tuple2 = new Tuple(Literal.ONE, Literal.ZERO, Literal.NULL);

    assertEquals(Kind.TUPLE, tuple1.getKind());
    assertTrue(tuple0.isEmpty());
    assertFalse(tuple1.isEmpty());
    assertEquals(tuple1, tuple2);
    assertNotEquals(tuple1, null);
    assertEquals(tuple1.hashCode(), tuple2.hashCode());    
    assertEquals("(1,0,NULL)", tuple1.toString());
    
    // Not evaluable alone
    assertThrows(ExpressionException.class, () -> tuple1.eval(createExpressionContext()));      
  }
}