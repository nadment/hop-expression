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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Set;
import org.apache.hop.expression.operator.CoalesceFunction;
import org.apache.hop.expression.operator.TrimFunction;
import org.junit.jupiter.api.Test;

class FunctionRegistryTest extends ExpressionTest {

  @Test
  void registry() {
    assertNotNull(FunctionRegistry.getFunctions());
    assertNotNull(FunctionRegistry.getFunctionNames());
    assertNull(FunctionRegistry.getFunction(null));
    assertEquals(
        FunctionRegistry.getFunction("TRUNC").getId(),
        FunctionRegistry.getFunction("TRUNCATE").getId());
    assertEquals(
        FunctionRegistry.getFunction("TRUNC").getName(),
        FunctionRegistry.getFunction("TRUNCATE").getName());
    assertEquals(FunctionRegistry.getFunction("TRUNC"), FunctionRegistry.getFunction("TRUNCATE"));
    // An alias is registered with the same instance
    assertSame(FunctionRegistry.getFunction("TRUNC"), FunctionRegistry.getFunction("TRUNCATE"));

    // Names ignore case
    assertNotNull(FunctionRegistry.getFunction("UPPER"));
    assertSame(FunctionRegistry.getFunction("UPPER"), FunctionRegistry.getFunction("upper"));
    assertSame(FunctionRegistry.getFunction("UPPER"), FunctionRegistry.getFunction("Upper"));

    // Unknown function
    assertNull(FunctionRegistry.getFunction("NOT_A_FUNCTION"));
  }

  @Test
  void isFunction() {
    assertTrue(FunctionRegistry.isFunction("UPPER"));
    assertTrue(FunctionRegistry.isFunction("TRUNCATE"));
    assertFalse(FunctionRegistry.isFunction("NOT_A_FUNCTION"));
  }

  @Test
  void functionNames() {
    Set<String> names = FunctionRegistry.getFunctionNames();
    assertTrue(names.contains("UPPER"));

    // An unmodifiable copy, the registry cannot be changed through it
    assertThrows(UnsupportedOperationException.class, () -> names.remove("UPPER"));
    assertNotNull(FunctionRegistry.getFunction("UPPER"));
  }

  @Test
  void unregister() throws Exception {
    Function trim = FunctionRegistry.getFunction("TRIM");
    try {
      // Names ignore case
      assertEquals(TrimFunction.INSTANCE, FunctionRegistry.unregister("trim"));
      assertNull(FunctionRegistry.getFunction("TRIM"));
      assertFalse(FunctionRegistry.isFunction("TRIM"));

      // Not registered
      assertNull(FunctionRegistry.unregister("TRIM"));
      assertNull(FunctionRegistry.unregister("NOT_A_FUNCTION"));
      assertNull(FunctionRegistry.unregister(null));
    } finally {
      // The registry is shared by the tests, restore the function
      FunctionRegistry.register("TRIM", trim);
    }
    assertSame(trim, FunctionRegistry.getFunction("TRIM"));
  }

  @Test
  void registerAlreadyRegisteredFunction() throws Exception {
    Function coalesce = FunctionRegistry.getFunction("COALESCE");

    // The function already registered is kept, whatever the case of the name
    FunctionRegistry.register("COALESCE", new CoalesceFunction());
    FunctionRegistry.register("coalesce", new CoalesceFunction());
    assertSame(coalesce, FunctionRegistry.getFunction("COALESCE"));
  }

  @Test
  void registerWithoutName() throws Exception {
    int count = FunctionRegistry.getFunctionNames().size();

    // A function without name raises an error and is not registered
    assertThrows(
        IllegalArgumentException.class,
        () -> FunctionRegistry.register(null, new CoalesceFunction()));
    assertThrows(
        IllegalArgumentException.class,
        () -> FunctionRegistry.register("", new CoalesceFunction()));
    assertThrows(
        IllegalArgumentException.class,
        () -> FunctionRegistry.register("  ", new CoalesceFunction()));
    assertEquals(count, FunctionRegistry.getFunctionNames().size());
  }
}
