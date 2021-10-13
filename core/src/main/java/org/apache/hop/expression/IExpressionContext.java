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

import java.time.ZoneId;
import java.util.Locale;

public interface IExpressionContext {

  /**
   * Resolves the given reference. How the name is interpreted by the outside system is an
   * implementation detail. Resolve is case sensitive.
   *
   * @param name the name that identifies the reference.
   * @return the resolved value.
   * @throws ExpressionException if an error occurs.
   */
  public Object resolve(String name) throws ExpressionException;

  public Locale getLocale();

  public ZoneId getZone();

  /**
   * Gets the value of an attribute in a given scope.
   *
   * @param name The name of the attribute to retrieve.
   * @return The value of the attribute. Returns <code>null</code> is the name
   *         does not exist.
   *
   * @throws IllegalArgumentException if the name is empty.
   * @throws NullPointerException if the name is null.
   */
  public Object getAttribute(String name);
}
