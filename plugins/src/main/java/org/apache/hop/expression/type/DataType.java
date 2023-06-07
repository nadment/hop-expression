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
import java.util.Objects;
import com.fasterxml.jackson.databind.JsonNode;

public abstract class DataType {

  public static final int SCALE_NOT_SPECIFIED = -1;
  public static final int PRECISION_NOT_SPECIFIED = -1;
    
  protected final DataName name;
  protected final int precision;
  protected final int scale;
  private String signature;

  protected DataType(final DataName name) {
    this(name, PRECISION_NOT_SPECIFIED, SCALE_NOT_SPECIFIED);
  }

  protected DataType(final DataName name, final int precision) {
    this(name, precision, SCALE_NOT_SPECIFIED);
  }
  
  protected DataType(final DataName name, final int precision, final int scale) {
    this.name = Objects.requireNonNull(name);
    this.precision = precision;
    this.scale = scale;
    this.signature = generate();
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
          && Objects.equals(this.signature, ((DataType) obj).signature);
  }

  @Override
  public int hashCode() {
    return Objects.hashCode(signature);
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
  
  
  /**
   * Convert a value to the specified type {@link DataType} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *        numeric, or null if none
   * @return the converted value
   */
  public Object cast(final Object value, String pattern) {
    throw new RuntimeException(ExpressionError.INTERNAL_ERROR.message());
  }
  
  @Override
  public String toString() {
    return signature;
  }
  
  /**
   * Return a default data type for a value.
   *
   * @return The data type or 'UNKNOWN' if not found
   */
  public static DataType of(final Object value) {
    if (value == null)
      return UnknownDataType.UNKNOWN;
    if (value instanceof Boolean)
      return BooleanDataType.BOOLEAN;
    if (value instanceof String)
      return StringDataType.STRING;
    if (value instanceof BigDecimal) {
      BigDecimal number = (BigDecimal) value; 
      return new NumberDataType(number.precision(), number.scale());
    }
    if (value instanceof Double)
      return NumberDataType.NUMBER;
    if (value instanceof Long)
      return IntegerDataType.INTEGER;
    if (value instanceof ZonedDateTime)
      return DateDataType.DATE;
    if (value instanceof JsonNode)
      return JsonDataType.JSON;
    if (value instanceof byte[])
      return BinaryDataType.BINARY;
    
    return UnknownDataType.UNKNOWN;
  }
}

