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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

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
  }

  @Test
  void unregistrer() throws Exception {
    assertEquals(TrimFunction.INSTANCE, FunctionRegistry.unregister("TRIM"));
  }

  @Test
  void failOnAlreadyRegistredFunction() throws Exception {
    FunctionRegistry.register("COALESCE", new CoalesceFunction());
  }
}
