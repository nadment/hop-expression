/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.apache.hop.resolvers.expression;

import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.gui.plugin.GuiPlugin;
import org.apache.hop.core.logging.LogChannel;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.resolver.IVariableResolver;
import org.apache.hop.core.variables.resolver.VariableResolverPlugin;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.IExpression;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

@NullMarked
@GuiPlugin
@VariableResolverPlugin(
    id = ExpressionVariableResolver.ID,
    name = ExpressionVariableResolver.NAME,
    description = "Evaluate value",
    documentationUrl = "/variables/resolvers/expression.html")
public class ExpressionVariableResolver implements IVariableResolver {

  /** The plugin id, also used to serialize the resolver in the metadata. */
  public static final String ID = "Expression-Variable-Resolver";

  /** The plugin name. */
  public static final String NAME = "Expression Variable Resolver";

  @Override
  public @Nullable String resolve(String source, IVariables variables) throws HopException {
    try {
      // Compile expression
      IExpression expression = IExpression.of(new ExpressionContext(variables), source);
      return expression.getValue(String.class);
    } catch (Exception e) {
      LogChannel.GENERAL.logError("Error evaluating expression: " + source, e);
      return null;
    }
  }

  @Override
  public void init() {
    // Not used today
  }

  @Override
  public String getPluginId() {
    return ID;
  }

  @Override
  public String getPluginName() {
    return NAME;
  }
}
