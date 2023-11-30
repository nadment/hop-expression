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

import org.apache.hop.expression.Interval;
import java.math.BigDecimal;
import java.time.ZonedDateTime;
import java.util.Set;
import com.fasterxml.jackson.databind.JsonNode;

/**
 * Enumeration of the data type which can be used to construct an expression.
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
  UNKNOWN(TypeFamily.NONE, PrecScale.NO_NO, Void.class),

  SYMBOL(TypeFamily.SYMBOL, PrecScale.NO_NO, Void.class),

  ANY(TypeFamily.ANY, PrecScale.NO_NO | PrecScale.YES_NO | PrecScale.YES_YES, Object.class),

  /** Unlimited length text */
  STRING(TypeFamily.STRING, PrecScale.NO_NO | PrecScale.YES_NO, String.class),

  /** Boolean (true or false) */
  BOOLEAN(TypeFamily.BOOLEAN, PrecScale.NO_NO, Boolean.class),

  /** Signed integer (64-bit) */
  INTEGER(TypeFamily.NUMERIC, PrecScale.NO_NO, Long.class),

  /** Unlimited precision number */
  NUMBER(TypeFamily.NUMERIC, PrecScale.NO_NO | PrecScale.YES_NO | PrecScale.YES_YES,
      BigDecimal.class),

  /** Date-time value with nanosecond precision and time zone */
  DATE(TypeFamily.TEMPORAL, PrecScale.NO_NO, ZonedDateTime.class),

  JSON(TypeFamily.JSON, PrecScale.NO_NO, JsonNode.class),

  /** A binary type can be images, sounds, videos, and other types of binary data */
  BINARY(TypeFamily.BINARY, PrecScale.NO_NO | PrecScale.YES_NO, byte[].class),

  /** A interval type for years to months */
  INTERVAL(TypeFamily.INTERVAL, PrecScale.NO_NO, Interval.class);

  /**
   * Flags indicating precision/scale combinations.
   */
  private interface PrecScale {
    int NO_NO = 1;
    int YES_NO = 2;
    int YES_YES = 4;
  }

  public static final int MAX_INTERVAL_FRACTIONAL_SECOND_PRECISION = 9;

  protected static final Set<TypeId> STRING_TYPES = Set.of(STRING);
  protected static final Set<TypeId> BINARY_TYPES = Set.of(BINARY);
  protected static final Set<TypeId> BOOLEAN_TYPES = Set.of(BOOLEAN);
  protected static final Set<TypeId> NUMERIC_TYPES = Set.of(INTEGER, NUMBER);
  protected static final Set<TypeId> TEMPORAL_TYPES = Set.of(DATE);
  protected static final Set<TypeId> JSON_TYPES = Set.of(JSON);
  protected static final Set<TypeId> INTERVAL_TYPES = Set.of(INTERVAL);

  protected static final Set<TypeId> ALL_TYPES =
      Set.of(STRING, BOOLEAN, INTEGER, NUMBER, DATE, BINARY, JSON);

  /**
   * Indicating allowable precision/scale combinations.
   */
  private final int signature;

  private final TypeFamily family;

  private final Class<?> javaClass;

  public static final Set<String> ALL_NAMES =
      Set.of("Binary", "Boolean", "Date", "Integer", "Number", "Json", "String");

  private TypeId(TypeFamily family, int signature, Class<?> javaClass) {
    this.family = family;
    this.signature = signature;
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
   * Returns whether type are in same type family.
   */
  public boolean isCompatibleWithCoercion(TypeFamily family) {
    return this.family.isCompatibleWithCoercion(family);
  }

  public boolean allowsNoPrecNoScale() {
    return (signature & PrecScale.NO_NO) != 0;
  }

  public boolean allowsPrecNoScale() {
    return (signature & PrecScale.YES_NO) != 0;
  }

  public boolean allowsPrec() {
    return allowsPrecScale(true, true) || allowsPrecScale(true, false);
  }

  public boolean allowsScale() {
    return allowsPrecScale(true, true);
  }

  public boolean allowsPrecScale(boolean precision, boolean scale) {
    int mask =
        precision ? (scale ? PrecScale.YES_YES : PrecScale.YES_NO) : (scale ? 0 : PrecScale.NO_NO);
    return (signature & mask) != 0;
  }

  /**
   * Returns the minimum precision (or length) allowed for this type, or -1 if
   * precision/length are not applicable for this type.
   *
   * @return Minimum allowed precision
   */
  public int getMinPrecision() {
    switch (this) {
      case STRING:
      case BINARY:
      case DATE:
      case INTEGER:
      case NUMBER:
        return 1;
      case INTERVAL:
        return 6;
      default:
        return -1;
    }
  }

  /**
   * Returns the maximum precision (or length) allowed for this type, or -1 if
   * precision/length are not applicable for this type.
   *
   * @return Maximum allowed precision
   */
  public int getMaxPrecision() {
    switch (this) {
      case STRING:
      case BINARY:
        return 16_777_216;
      case INTEGER:
        return 19;
      case NUMBER:
        return 38;
      case DATE:
      case INTERVAL:
        return 9;
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
   * Search a data type identifier for java class.
   *
   * @return The {@link TypeId}, 'UNKNOWN' if not found
   */
  public static TypeId fromJavaClass(final Class<?> clazz) {
    if (clazz == null)
      return TypeId.UNKNOWN;

    for (TypeId id : TypeId.values()) {

      // Ignore ANY
      if (id.equals(TypeId.ANY))
        continue;

      if (id.getJavaClass().isAssignableFrom(clazz)) {
        return id;
      }
    }
    return TypeId.UNKNOWN;
  }
}
