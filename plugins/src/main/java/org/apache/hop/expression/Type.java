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

import java.math.BigDecimal;
import java.time.Instant;

/**
 * Enumeration of the data type which can be used to construct a expression.
 *
 * <p>
 * The order of enum declaration is important to be usable with <code>compareTo</code> method.
 *
 * <p>
 * If values need to be converted to match the other operands data type, the value with the lower
 * order is converted to the value with the higher order.
 */
public enum Type {
  /** A unknown type */
  NONE(Void.class),

  /** Unlimited length text */
  STRING(String.class),

  /** Boolean value (true or false) */
  BOOLEAN(Boolean.class),

  /** Signed long (64-bit) integer */
  INTEGER(Long.class),

  /** Double precision floating point number */
  NUMBER(Double.class),

  /** Unlimited precision number */
  BIGNUMBER(BigDecimal.class),

  /** Date-time value with nanosecond precision */
  DATE(Instant.class),

  /** A binary type can be images, sounds, videos, and other types of binary data */
  BINARY(byte[].class);

  private final Class<?> javaClass;

  private Type(Class<?> javaClass) {
    this.javaClass = javaClass;
  }

  public Class<?> javaClass() {
    return javaClass;
  }

  public static Type of(final String name) {
    for (Type type : Type.values()) {
      if (type.name().equalsIgnoreCase(name)) {
        return type;
      }
    }
    throw new IllegalArgumentException("Invalid type: " + name);
  }

  public static Type fromJava(final Object object) {
    if (object == null)
      return NONE;
    if (object instanceof Boolean)
      return BOOLEAN;
    if (object instanceof String)
      return STRING;
    if (object instanceof BigDecimal)
      return BIGNUMBER;
    if (object instanceof Double)
      return NUMBER;
    if (object instanceof Long)
      return INTEGER;
    if (object instanceof Instant)
      return DATE;
    if (object instanceof byte[])
      return BINARY;

    throw new IllegalArgumentException("Unknown type for: " + object);
  }



  /**
   * Check if type exist.
   * 
   * @param str the name to check
   * @return
   */
  public static boolean exist(final String name) {
    for (Type type : Type.values()) {
      if (type.name().equalsIgnoreCase(name)) {
        return true;
      }
    }
    return false;
  }
}
