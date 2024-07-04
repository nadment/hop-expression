/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression;

import org.apache.hop.core.variables.IVariables;

public interface IExpressionContext extends IVariables {

  public IExpression createExpression(String source) throws ExpressionException;

  /**
   * Gets the value of an attribute.
   *
   * @param name The name of the attribute to retrieve.
   * @return The value of the given attribute or <code>null</code>.
   * @throws IllegalArgumentException if the name is empty.
   * @throws NullPointerException if the name is null.
   */
  public Object getAttribute(String name);

  /**
   * Gets the value of an attribute.
   *
   * @param name The name of the attribute to set
   * @param value The value of the attribute
   * @throws IllegalArgumentException if the name is empty or if the scope is invalid.
   * @throws NullPointerException if the name is null.
   */
  // public void setAttribute(String name, Object value);

  /**
   * Get the value of a variable with a default in case the variable is not found.
   *
   * @param name The name of the variable
   * @param defaultValue The default value in case the variable could not be found
   * @return the String value of a variable
   */
  String getVariable(String name, String defaultValue);

  /**
   * Get the value of a variable.
   *
   * @param name The name of the variable
   * @return the String value of a variable or null in case the variable could not be found.
   */
  String getVariable(String name);

  /**
   * Substitute the string using the current variable variables.
   *
   * @param string The string to substitute.
   * @return The substituted string.
   */
  String resolve(String string);
}
