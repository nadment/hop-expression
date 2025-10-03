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

import lombok.Getter;
import lombok.Setter;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.gui.plugin.GuiPlugin;
import org.apache.hop.core.logging.LogChannel;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.resolver.IVariableResolver;
import org.apache.hop.core.variables.resolver.VariableResolverPlugin;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionFactory;
import org.apache.hop.expression.IExpression;

@Getter
@Setter
@GuiPlugin
@VariableResolverPlugin(
    id = "Expression-Variable-Resolver",
    name = "Expression Variable Resolver",
    description = "Evaluate value",
    documentationUrl = "/variables/resolvers/expression.html")
public class ExpressionVariableResolver implements IVariableResolver {

  @Override
  public String resolve(String source, IVariables variables) throws HopException {
    try {
      // Compile expression
      IExpression expression = ExpressionFactory.create(new ExpressionContext(variables), source);
      return expression.getValue(String.class);
    } catch (Exception e) {
      LogChannel.GENERAL.logError("Error evaluate expression: " + source, e);
      return null;
    }
  }

  @Override
  public void setPluginId() {
    // Nothing to set
  }

  @Override
  public void init() {
    // Not used today
  }

  @Override
  public String getPluginId() {
    return "Expression-Variable-Resolver";
  }

  @Override
  public void setPluginName(String pluginName) {
    // Nothing to set
  }

  @Override
  public String getPluginName() {
    return "Expression Variable Resolver";
  }
}
