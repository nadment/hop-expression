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

import org.apache.hop.i18n.BaseMessages;
import java.math.BigDecimal;
import java.time.ZonedDateTime;
import java.util.Set;
import com.fasterxml.jackson.databind.JsonNode;

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
public enum DataType {
  /** A unknown type */
  UNKNOWN(Void.class),

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
  DATE(ZonedDateTime.class),

  JSON(JsonNode.class),
  
  /** A binary type can be images, sounds, videos, and other types of binary data */
  BINARY(byte[].class);

  private final Class<?> javaClass;
 
  private static final String[] names = Set.of("BigNumber","Binary","Boolean","Date","Integer","Number", "Json", "String").toArray(new String[0]);
  
  private DataType(Class<?> javaClass) {
    this.javaClass = javaClass;
  }

  public Class<?> getJavaClass() {
    return javaClass;
  }

  public static DataType of(final String name) {
    for (DataType type : DataType.values()) {
      if (type.name().equalsIgnoreCase(name)) {
        return type;
      }
    }
    throw new IllegalArgumentException(ExpressionError.INVALID_DATATYPE.message(name));
  }

  public static DataType from(final Object value) {
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

    throw new IllegalArgumentException(
        BaseMessages.getString(IExpression.class, "Expression.UnknownDataType", value.getClass()));
  }
  
  public static String name(final Object value) {
    if (value == null)
      return UNKNOWN.name();
    if (value instanceof Boolean)
      return BOOLEAN.name();
    if (value instanceof String)
      return STRING.name();
    if (value instanceof BigDecimal)
      return BIGNUMBER.name();
    if (value instanceof Double)
      return NUMBER.name();
    if (value instanceof Long)
      return INTEGER.name();
    if (value instanceof ZonedDateTime)
      return DATE.name();
    if (value instanceof JsonNode)
      return JSON.name();
    if (value instanceof byte[])
      return BINARY.name();

    return UNKNOWN.name();
  }
  
  /**
   * Check if data type exist.
   * 
   * @param name the name to check
   * @return
   */
  public static boolean exist(final String name) {
    for (DataType type : DataType.values()) {
      if (type.name().equalsIgnoreCase(name)) {
        return true;
      }
    }
    return false;
  }
  
  public static String[] getDisplayNames() {
    return names;
  }
}
