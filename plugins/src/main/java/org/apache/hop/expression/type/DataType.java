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
import java.util.Objects;
import com.fasterxml.jackson.databind.JsonNode;

public class DataType {

  public static final int SCALE_NOT_SPECIFIED = -1;
  public static final int PRECISION_NOT_SPECIFIED = -1;
  
  /**
   * BOOLEAN type.
   */
  public static final DataType BOOLEAN;

  /**
   * Default BINARY type with default parameters.
   */
  public static final DataType BINARY;
  
  /**
   * Default DATE type with parameters.
   */
  public static final DataType DATE;

  /**
   * Default STRING type with max precision.
   */
  public static final DataType STRING;

  /**
   * JSON type.
   */
  public static final DataType JSON;

  /**
   * Default INTEGER type with max precision.
   */
  public static final DataType INTEGER;
  public static final DataType NUMBER;
  public static final DataType BIGNUMBER;
  public static final DataType TIMEUNIT;
  public static final DataType UNKNOWN;

  static {
    UNKNOWN = new DataType(DataName.UNKNOWN);
    BOOLEAN = new DataType(DataName.BOOLEAN, DataName.BOOLEAN.getMaxPrecision(), 0);
    BINARY = new DataType(DataName.BINARY);
    DATE = new DataType(DataName.DATE, PRECISION_NOT_SPECIFIED, 9);
    JSON = new DataType(DataName.JSON);
    STRING = new DataType(DataName.STRING, DataName.STRING.getMaxPrecision(), 0);
    INTEGER = new DataType(DataName.INTEGER, DataName.INTEGER.getMaxPrecision(), 0);
    NUMBER = new DataType(DataName.NUMBER, DataName.NUMBER.getMaxPrecision(), 0);
    BIGNUMBER = new DataType(DataName.BIGNUMBER, DataName.BIGNUMBER.getMaxPrecision(), 0);
    TIMEUNIT = new DataType(DataName.TIMEUNIT);
  }
  
  private final DataName name;
  private final int precision;
  private final int scale;
  private String digest;

  private DataType(final DataName name) {
    this(name, PRECISION_NOT_SPECIFIED, SCALE_NOT_SPECIFIED);
  }

  public DataType(final DataName name, final int precision) {
    this(name, precision, SCALE_NOT_SPECIFIED);
  }
  
  public DataType(final DataName name, final int precision, final int scale) {
    this.name = Objects.requireNonNull(name);
    this.precision = precision;
    this.scale = scale;
    this.digest = generate();
  }

  /**
   * Gets the {@link DataName} of this type.
   *
   * @return name, never null
   */
  public DataName getName() {
    return name;
  }

  /**
   * Gets the {@link DataFamily} of this type.
   *
   * @return family, never null
   */
  public DataFamily getFamily() {
    return name.getFamily();
  }
  
  public boolean isSameFamily(DataFamily family) {
    return name.getFamily().isSameFamily(family);
  }

  public boolean isSameFamily(DataType type) {
    return name.getFamily().isSameFamily(type.getFamily());
  }
  
  /**
   * Gets the precision of this type.
   *
   * <p>
   * Returns {@link #PRECISION_NOT_SPECIFIED} (-1) if precision is not
   * applicable for this type.
   * </p>
   *
   * @return number of decimal digits for exact numeric types;
   *         number of decimal digits in mantissa for approximate numeric types;
   *         number of decimal digits for fractional seconds of datetime types;
   *         length in characters for String types;
   *         length in bytes for Binary types;
   *         1 for BOOLEAN;
   *         -1 if precision is not valid for this type
   */
  public int getPrecision() {
    return precision;
  }

  /**
   * Gets the scale of this type. 
   * Returns {@link #SCALE_NOT_SPECIFIED} (-1) if scale is not valid for this type.
   *
   * @return number of digits of scale
   */
  public int getScale() {
    return scale;
  }
  
  @Override
  public boolean equals(Object obj) {
    return this == obj
        || obj instanceof DataType
          && Objects.equals(this.digest, ((DataType) obj).digest);
  }

  @Override
  public int hashCode() {
    return Objects.hashCode(digest);
  }
  
  /**
   * Generates a string representation of this type.
   *
   * @return The string representation
   */
  private String generate() {
    if (this.precision == PRECISION_NOT_SPECIFIED && this.scale == SCALE_NOT_SPECIFIED) {
      return name.name();
    }

    StringBuilder builder = new StringBuilder();
    builder.append(name.toString());
    if (precision >= 0 && precision != name.getMaxPrecision()) {
      builder.append('(');
      builder.append(this.precision);
      if (scale >= 0 && name!=DataName.INTEGER) {
        builder.append(',');
        builder.append(this.scale);
      }
      builder.append(')');
    }

    return builder.toString();
  }
  
  @Override
  public String toString() {
    return digest;
  }
  
  /**
   * Return a data type for a value.
   *
   * @return The data type or 'UNKNOWN' if not found
   */
  public static DataType of(final Object value) {
    if (value == null)
      return UNKNOWN;
    if (value instanceof Boolean)
      return BOOLEAN;
    if (value instanceof String)
      return STRING;
    if (value instanceof BigDecimal) {
      BigDecimal number = (BigDecimal) value; 
      return new DataType(DataName.BIGNUMBER, number.precision(), number.scale());
    }
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
    if (value instanceof TimeUnit)
      return TIMEUNIT;
    
    return UNKNOWN;
  }
}

