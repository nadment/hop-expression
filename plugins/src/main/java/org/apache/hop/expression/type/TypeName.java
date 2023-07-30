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

import org.apache.hop.expression.TimeUnit;
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
public enum TypeName {

  
  /** A unknown type */
  UNKNOWN(TypeFamily.NONE, PrecScale.NO_NO, -1, Void.class),
  
  ANY(TypeFamily.ANY, PrecScale.NO_NO | PrecScale.YES_NO | PrecScale.YES_YES, -1, Object.class),
  
  /** Unlimited length text */
  STRING(TypeFamily.STRING, PrecScale.NO_NO | PrecScale.YES_NO, 16777216, String.class),
  
  /** Boolean (true or false) */
  BOOLEAN(TypeFamily.BOOLEAN, PrecScale.NO_NO, -1, Boolean.class),

  /** Signed long (64-bit) integer */
  INTEGER(TypeFamily.NUMERIC, PrecScale.NO_NO, 19, Long.class),

  /** Unlimited precision number */
  NUMBER(TypeFamily.NUMERIC, PrecScale.NO_NO | PrecScale.YES_NO | PrecScale.YES_YES, 38, BigDecimal.class),

  /** Date-time value with nanosecond precision and time zone */
  DATE(TypeFamily.TEMPORAL, PrecScale.NO_NO, -1, ZonedDateTime.class),
  
  JSON(TypeFamily.JSON, PrecScale.NO_NO, -1, JsonNode.class),

  /** A binary type can be images, sounds, videos, and other types of binary data */
  BINARY(TypeFamily.BINARY, PrecScale.NO_NO | PrecScale.YES_NO, 16777216, byte[].class);

  protected static final Set<TypeName> STRING_TYPES = Set.of(STRING);
  protected static final Set<TypeName> BINARY_TYPES = Set.of(BINARY);
  protected static final Set<TypeName> BOOLEAN_TYPES = Set.of(BOOLEAN);
  protected static final Set<TypeName> NUMERIC_TYPES = Set.of(INTEGER, NUMBER);
  protected static final Set<TypeName> TEMPORAL_TYPES = Set.of(DATE);
  protected static final Set<TypeName> JSON_TYPES = Set.of(JSON);
  protected static final Set<TypeName> ALL_TYPES = Set.of(STRING, BOOLEAN, INTEGER, NUMBER, DATE, BINARY, JSON);

  /**
   * Indicating allowable precision/scale combinations.
   */
  private final int signature;

  private final TypeFamily family;

  private final Class<?> javaClass;
  
  private final int precisionMax;

  public static final Set<String> ALL_NAMES = Set.of("Binary", "Boolean", "Date", "Integer", "Number", "Json", "String");

  private TypeName(TypeFamily family, int signature, int precisionMax,Class<?> javaClass) {
    this.family = family;
    this.signature = signature;
    this.precisionMax = precisionMax;
    this.javaClass = javaClass;
  }

  public Class<?> getJavaClass() {
    return javaClass;
  }

  /**
   * Gets the {@link TypeFamily} containing this {@link TypeName}.
   */
  public TypeFamily getFamily() {
    return family;
  }

  /**
   * Returns whether type are in same type family.
   */
  public boolean isSameFamily(TypeFamily family) {
    return this.family.isSameFamily(family);
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
   * Returns a {@link TypeName} with a given name (ignore case).
   *
   * @param name The name of the data name
   * @return data name, or null if not valid
   */
  public static TypeName of(final String name) {
    for (TypeName type : TypeName.values()) {
      if (type.name().equalsIgnoreCase(name)) {
        return type;
      }
    }
    return null;
  }

  
  /**
   * Search a data name for a value or a java class.
   *
   * @return The {@link TypeName}, 'UNKNOWN' if not found
   */
  public static TypeName from(final Object value) {
    if (value == null)
      return UNKNOWN;
    
    Class<?> clazz = value.getClass();
    
    if ( value instanceof Class) {
      clazz = (Class<?>) value;
    }
    
    for (TypeName name : TypeName.values()) {
      
      // Ignore ANY
      if ( name.equals(ANY) ) continue;
      
      if (name.javaClass.isAssignableFrom(clazz)) {
        return name;
      }
    }
    return UNKNOWN;
  }

  public int getMaxPrecision() {
    return precisionMax;
  }
}
