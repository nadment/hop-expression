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

import org.apache.hop.expression.ExpressionError;
import java.math.BigDecimal;
import java.time.ZonedDateTime;
import java.util.List;
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
public enum DataTypeName {

  /** Unlimited length text */
  STRING(DataTypeFamily.STRING, PrecScale.NO_NO | PrecScale.YES_NO, String.class),

  /** Boolean (true or false) */
  BOOLEAN(DataTypeFamily.BOOLEAN, PrecScale.NO_NO, Boolean.class),

  /** Signed long (64-bit) integer */
  INTEGER(DataTypeFamily.NUMERIC, PrecScale.NO_NO, Long.class),

  /** Double precision floating point number */
  NUMBER(DataTypeFamily.NUMERIC, PrecScale.NO_NO, Double.class),

  /** Unlimited precision number */
  BIGNUMBER(DataTypeFamily.NUMERIC, PrecScale.NO_NO | PrecScale.YES_NO | PrecScale.YES_YES, BigDecimal.class),

  /** Date-time value with nanosecond precision */
  DATE(DataTypeFamily.DATE, PrecScale.NO_NO, ZonedDateTime.class),

  JSON(DataTypeFamily.JSON, PrecScale.NO_NO, JsonNode.class),

  /** A binary type can be images, sounds, videos, and other types of binary data */
  BINARY(DataTypeFamily.BINARY, PrecScale.NO_NO | PrecScale.YES_NO, byte[].class),

  /** A unknown type */
  UNKNOWN(DataTypeFamily.ANY, PrecScale.NO_NO, Void.class),
  
  ANY(DataTypeFamily.ANY, PrecScale.NO_NO | PrecScale.YES_NO | PrecScale.YES_YES, Object.class)
  ;

  
  public static final List<DataTypeName> STRING_TYPES = List.of(STRING);
  public static final List<DataTypeName> BINARY_TYPES = List.of(BINARY);
  public static final List<DataTypeName> BOOLEAN_TYPES = List.of(BOOLEAN);
  public static final List<DataTypeName> NUMERIC_TYPES = List.of(INTEGER, NUMBER, BIGNUMBER);
  public static final List<DataTypeName> DATE_TYPES = List.of(DATE);
  public static final List<DataTypeName> JSON_TYPES = List.of(JSON);
  public static final List<DataTypeName> ALL_TYPES = List.of(STRING,BOOLEAN,INTEGER, NUMBER, BIGNUMBER,DATE,BINARY,JSON);

  
  /**
   * Flags indicating precision/scale combinations.
   */
  private interface PrecScale {
    int NO_NO = 1;
    int YES_NO = 2;
    int YES_YES = 4;
  }

  /**
   * Indicating allowable precision/scale combinations.
   */
  private final int signature;

  private final DataTypeFamily family;

  private final Class<?> javaClass;

  public static final Set<String> ALL_NAMES =
      Set.of("BigNumber", "Binary", "Boolean", "Date", "Integer", "Number", "Json", "String");

  private DataTypeName(DataTypeFamily family, int signature, Class<?> javaClass) {
    this.family = family;
    this.signature = signature;
    this.javaClass = javaClass;
  }

  public Class<?> getJavaClass() {
    return javaClass;
  }

  /**
   * Gets the {@link DataTypeFamily} containing this DataTypeName.
   */
  public DataTypeFamily getFamily() {
    return family;
  }

  public boolean isSameFamily(DataTypeName type) {
    return this.family.is(type.getFamily());
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

  public static DataTypeName of(final String name) {
    for (DataTypeName type : DataTypeName.values()) {
      if (type.name().equalsIgnoreCase(name)) {
        return type;
      }
    }
    throw new IllegalArgumentException(ExpressionError.INVALID_DATATYPE.message(name));
  }

  public static DataTypeName from(final Object value) {
    if (value == null)
      return UNKNOWN;
    if (value instanceof Boolean)
      return BOOLEAN;
    if (value instanceof String)
      return STRING;
    if (value instanceof BigDecimal)
      return BIGNUMBER;
    if (value instanceof Double)
      return NUMBER;
    if (value instanceof Long)
      return INTEGER;
    if (value instanceof ZonedDateTime)
      return DATE;
    if (value instanceof JsonNode)
      return JSON;
    if (value instanceof byte[])
      return BINARY;

    throw new IllegalArgumentException(ExpressionError.UNKNOWN_DATATYPE.message(value.getClass()));
  }

  /**
   * Check if data type exist.
   * 
   * @param name the name to check
   * @return
   */
  public static boolean exist(final String name) {
    for (DataTypeName type : DataTypeName.values()) {
      if (type.name().equalsIgnoreCase(name)) {
        return true;
      }
    }
    return false;
  }
}
