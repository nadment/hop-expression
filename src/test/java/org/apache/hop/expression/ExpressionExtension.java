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

import org.apache.hop.core.HopEnvironment;
import org.junit.jupiter.api.extension.AfterAllCallback;
import org.junit.jupiter.api.extension.BeforeAllCallback;
import org.junit.jupiter.api.extension.ExtensionContext;

/**
 * Registers the functions for the tests of a class, and leaves the Hop environment ready for the
 * next tests. It is declared after {@code RestoreHopEnvironmentExtension}, so its {@code afterAll}
 * runs before the reset of the client environment.
 */
public class ExpressionExtension implements BeforeAllCallback, AfterAllCallback {
  @Override
  public void beforeAll(ExtensionContext context) throws Exception {
    FunctionRegistry.init();
  }

  @Override
  public void afterAll(ExtensionContext context) throws Exception {
    // Remove the functions registered by the tests, like the user-defined functions, and register
    // the built-in functions again, which are registered by the first initialization of the
    // environment only
    FunctionRegistry.reset();
    FunctionRegistry.init();

    // RestoreHopEnvironmentExtension resets the client environment (log store, plugins) but not
    // HopEnvironment, the next HopEnvironment.init() must initialize everything again
    HopEnvironment.reset();
  }
}
