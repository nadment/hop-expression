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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.operator.CoalesceFunction;
import org.junit.jupiter.api.Test;

public class FunctionRegistryTest extends ExpressionTest {

  @Test
  public void test() throws Exception {
    assertNotNull(FunctionRegistry.getFunctions());
    assertNotNull(FunctionRegistry.getFunctionNames());
    assertNull(FunctionRegistry.getFunction(null));
    assertEquals(
        FunctionRegistry.getFunction("CEIL").getId(),
        FunctionRegistry.getFunction("CEILING").getId());
    assertEquals(
        FunctionRegistry.getFunction("CEIL").getName(),
        FunctionRegistry.getFunction("CEILING").getName());
    assertEquals(FunctionRegistry.getFunction("CEIL"), FunctionRegistry.getFunction("CEILING"));
  }

  @Test
  public void failOnAlreadyRegistredFucntion() throws Exception {
    FunctionRegistry.register("COALESCE", new CoalesceFunction());
  }
}
