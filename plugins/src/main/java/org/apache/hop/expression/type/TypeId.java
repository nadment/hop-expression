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
package org.apache.hop.expression.type;

import java.math.BigDecimal;
import java.time.ZonedDateTime;
import java.util.Set;
import com.fasterxml.jackson.databind.JsonNode;

/**
 * Enumeration of the data type identifier which can be used to construct an expression.
 *
 * <p>
 * The order of enum declaration is important to be usable with <code>compareTo</code> method.
 *
 * <p>
 * If values need to be converted to match the other operands data type, the value with the lower
 * order is converted to the value with the higher order.
 */
public enum TypeId {

  /** A unknown type */
  UNKNOWN(TypeFamily.NONE, false, false, -1, -1, -1, -1, Void.class),

  ANY(TypeFamily.ANY, false, false, -1, -1, -1, -1, Object.class),

  /** Unlimited length text */
  STRING(TypeFamily.STRING, true, false, 16_777_216, 1, 0, 0, String.class),

  /** Boolean (true or false) */
  BOOLEAN(TypeFamily.BOOLEAN, false, false, 1, 0, 0, 0, Boolean.class),

  /** Signed integer (64-bit) */
  INTEGER(TypeFamily.NUMERIC, true, false, 19, 1, 0, 0, Long.class),

  /** Unlimited precision number */
  NUMBER(TypeFamily.NUMERIC, true, true, 38, 1, 37, 0, BigDecimal.class),

  /** Date-time value with nanosecond precision and time zone 
   * TODO: add precision for nanoseconds
   * */
  DATE(TypeFamily.TEMPORAL, false, false, -1, -1, -1, -1, ZonedDateTime.class),

  /** A Json type */
  JSON(TypeFamily.JSON, false, false, -1, -1, -1, -1, JsonNode.class),

  /** A binary type can be images, sounds, videos, and other types of binary data */
  BINARY(TypeFamily.BINARY, true, false, 16_777_216, 1, 0, 0, byte[].class),

  /** A interval type for years to months 
   * TODO: add precision for nanoseconds
   */  
  INTERVAL(TypeFamily.INTERVAL, false, false, -1, -1, -1, -1, Interval.class);
  

  protected static final Set<TypeId> STRING_TYPES = Set.of(STRING);
  protected static final Set<TypeId> BINARY_TYPES = Set.of(BINARY);
  protected static final Set<TypeId> BOOLEAN_TYPES = Set.of(BOOLEAN);
  protected static final Set<TypeId> NUMERIC_TYPES = Set.of(INTEGER, NUMBER);
  protected static final Set<TypeId> TEMPORAL_TYPES = Set.of(DATE);
  protected static final Set<TypeId> JSON_TYPES = Set.of(JSON);
  protected static final Set<TypeId> INTERVAL_TYPES = Set.of(INTERVAL);

  protected static final Set<TypeId> ALL_TYPES = Set.of(STRING, BOOLEAN, INTEGER, NUMBER, DATE, INTERVAL, BINARY, JSON);
  
  /**
   * If the precision parameter is supported.
   */
  private boolean supportsPrecision;

  /**
   * If the scale parameter is supported.
   */
  private boolean supportsScale;

  /**
   * The minimum supported precision.
   */
  private int minPrecision;

  /**
   * The maximum supported precision.
   */
  private int maxPrecision;

  /**
   * The lowest possible scale.
   */
  private int minScale;

  /**
   * The highest possible scale.
   */
  private int maxScale;
  
  private final TypeFamily family;

  private final Class<?> javaClass;

  public static final Set<String> ALL_NAMES =
      Set.of("Binary", "Boolean", "Date", "Integer", "Number", "Json", "String", "Interval");

  private TypeId(TypeFamily family, boolean supportsPrecision, boolean supportsScale, int maxPrecision, int minPrecision, int maxScale, int minScale, Class<?> javaClass) {
    this.family = family;
    this.supportsPrecision = supportsPrecision;
    this.supportsScale = supportsScale; 
    this.maxPrecision = maxPrecision;
    this.minPrecision = minPrecision;
    this.maxScale = maxScale;
    this.minScale = minScale;
    this.javaClass = javaClass;
  }

  public Class<?> getJavaClass() {
    return javaClass;
  }

  /**
   * Gets the {@link TypeFamily} containing this {@link TypeId}.
   */
  public TypeFamily getFamily() {
    return family;
  }

  /**
   * Returns whether type are in same type family.
   */
  public boolean isFamily(TypeFamily family) {
    return this.family.isFamily(family);
  }

  /**
   * Returns whether this {@link TypeId} support implicit coercion to the specified {@link TypeFamily}.
   */
  public boolean isCoercible(TypeFamily family) {
    return this.family.isCoercible(family);
  }
 
  public boolean supportsPrecision() {
    return this.supportsPrecision;
  }
  
  public boolean supportsScale() {
    return this.supportsScale;
  }
  
  /**
   * Returns the minimum precision (or length) allowed for this type, or -1 if
   * precision/length are not applicable for this type.
   *
   * @return Minimum allowed precision
   */
  public int getMinPrecision() {
    return minPrecision;
  }

  /**
   * Returns the maximum precision (or length) allowed for this type, or -1 if
   * precision/length are not applicable for this type.
   *
   * @return Maximum allowed precision
   */
  public int getMaxPrecision() {
    return maxPrecision;
  }

  public int getMaxScale() {
    return maxScale;
  }

  public int getMinScale() {
    return minScale;
  }
  
  public int getDefaultScale() {
    switch (this) {      
      case NUMBER:
        return 9;
      case BOOLEAN:
        return 0;
      default:
        return -1;
    }
  }
  
  /**
   * Returns a {@link TypeId} with a given name (ignore case).
   *
   * @param name The name of the data name
   * @return data name, or null if not valid
   */
  public static TypeId of(final String name) {
    for (TypeId type : TypeId.values()) {
      if (type.name().equalsIgnoreCase(name)) {
        return type;
      }
    }
    return null;
  }

  /**
   * Search a data type identifier from a java class.
   *
   * @return The {@link TypeId} or 'UNKNOWN' if not found
   */
  public static TypeId fromJavaClass(final Class<?> clazz) {
    if (clazz == null)
      return UNKNOWN;

    for (TypeId id : values()) {

      // Ignore ANY
      if (id.equals(ANY))
        continue;

      if (id.getJavaClass().isAssignableFrom(clazz)) {
        return id;
      }
    }
    return UNKNOWN;
  }
  
  /**
   * Search a data type identifier from a value.
   * 
   * @return The type id or 'UNKNOWN' if not found
   */
  public static TypeId fromValue(final Object value) {
    if (value == null)
      return UNKNOWN;

    if (value instanceof Integer) {
      return INTEGER;
    }
    if (value instanceof Double) {
      return NUMBER;
    }
    
    return fromJavaClass(value.getClass());
  }
}
