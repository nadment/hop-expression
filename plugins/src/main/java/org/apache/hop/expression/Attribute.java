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

import java.time.ZonedDateTime;
import java.util.Random;

public enum Attribute {
  CURRENT_TIMEZONE("timezone", String.class), CURRENT_TIMESTAMP("current_timestamp",
      ZonedDateTime.class), CURRENT_DATE("current_data",
          ZonedDateTime.class), RANDOM("random", Random.class);

  public final String id;
  public final Class<?> clazz;

  Attribute(String name, Class<?> clazz) {
    this.id = name;
    this.clazz = clazz;
  }

  /** Returns the value of this attribute in a given context. */
  public <T> T get(IExpressionContext context) {
    // noinspection unchecked
    return (T) clazz.cast(context.getAttribute(id));
  }
}
