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
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.apache.hop.expression.type.ArrayType;
import org.apache.hop.expression.type.Types;
import org.junit.jupiter.api.Test;

class ArrayTest extends ExpressionTest {

  @Test
  void array() {
    Array array0 = new Array();
    Array array1 = new Array(Literal.ONE, Literal.ZERO, Literal.NULL);
    Array array2 = new Array(Literal.ONE, Literal.ZERO, Literal.NULL);
    Array array3 = new Array(Literal.NULL, Literal.NULL, Literal.NULL);
    Array array4 = new Array(ArrayType.of(Types.STRING), Literal.of("A"), new Identifier("B"));

    assertEquals(Kind.ARRAY, array1.getKind());
    assertEquals(Types.ARRAY, array3.getType());
    assertEquals(ArrayType.of(Types.STRING), array4.getType());

    assertEquals(0, array0.size());
    assertEquals(3, array1.size());
    assertTrue(array0.isEmpty());
    assertFalse(array1.isEmpty());
    assertTrue(array1.isConstant());
    assertFalse(array4.isConstant());
    assertEquals(array1, array2);
    assertEquals(
        new Array(Literal.TRUE, Literal.FALSE, Literal.NULL),
        new Array(Literal.TRUE, Literal.FALSE, Literal.NULL));
    assertEquals(
        new Array(Literal.of("A"), Literal.of("B")), new Array(Literal.of("A"), Literal.of("B")));
    assertNotEquals(null, array1);
    assertNotEquals(array1, array3);
    assertNotEquals(
        new Array(Literal.of("B"), Literal.of("A")), new Array(Literal.of("A"), Literal.of("B")));
    assertEquals(array4.hashCode(), array4.hashCode());
    assertEquals(array1.hashCode(), array2.hashCode());
    assertEquals("[1,0,NULL]", array1.toString());

    // Not evaluable alone
    assertThrows(ConversionException.class, () -> array1.getValue(String.class));
  }
}
