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

import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

@NullMarked
public interface IExpressionContext {

  /**
   * Gets the value of an attribute.
   *
   * @param name The name of the attribute to retrieve.
   * @return The value of the given attribute or <code>null</code>.
   */
  @Nullable Object getAttribute(String name);

  /**
   * Returns a version changed each time the state of the context changes, like when a row or an
   * attribute is set. It is used to detect that the values computed for the previous state are no
   * longer valid.
   *
   * @return the version of the context
   */
  long getContextVersion();
}
