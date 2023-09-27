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

import static java.util.Objects.requireNonNull;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.exception.ConversionException;
import java.math.BigDecimal;
import java.time.ZonedDateTime;
import java.util.Objects;
import com.fasterxml.jackson.databind.JsonNode;

public abstract class Type {

  public static final int SCALE_NOT_SPECIFIED = -1;
  public static final int PRECISION_NOT_SPECIFIED = -1;

  protected final TypeName name;
  protected final int precision;
  protected final int scale;
  private final String signature;

  protected Type(final TypeName name) {
    this(name, PRECISION_NOT_SPECIFIED, SCALE_NOT_SPECIFIED);
  }

  protected Type(final TypeName name, final int precision) {
    this(name, precision, SCALE_NOT_SPECIFIED);
  }

  protected Type(final TypeName name, int precision, int scale) {
    this.name = requireNonNull(name);
    this.precision = precision;
    this.scale = scale;
    this.signature = generate();

    if (precision > name.getMaxPrecision()) {
      throw new IllegalArgumentException("Precision out of range");
    }
  }

  /**
   * Gets the {@link TypeName} of this type.
   *
   * @return name, never null
   */
  public TypeName getName() {
    return name;
  }

  /**
   * Gets the {@link TypeFamily} of this type.
   *
   * @return family, never null
   */
  public TypeFamily getFamily() {
    return name.getFamily();
  }

  public boolean is(TypeName typeName) {
    return name == typeName;
  }

  public boolean isSameFamily(TypeFamily family) {
    return name.getFamily().isSameFamily(family);
  }

  public boolean isSameFamily(Type type) {
    return name.getFamily().isSameFamily(type.getFamily());
  }

  public boolean isCompatibleWithCoercion(Type type) {
    return name.getFamily().isCompatibleWithCoercion(type.getFamily());
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
        || obj instanceof Type && Objects.equals(this.signature, ((Type) obj).signature);
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
    StringBuilder builder = new StringBuilder();
    builder.append(name.toString());
    if (precision > 0 && (precision != name.getMaxPrecision() || scale > 0)) {
      builder.append('(');
      builder.append(this.precision);
      if (scale > 0) {
        builder.append(',');
        builder.append(this.scale);
      }
      builder.append(')');
    }

    return builder.toString();
  }

  /**
   * Convert a value to the specified {@link Type}.
   *
   * @param value the value to convert
   * @return the converted value
   * @throws ConversionException if the casting fail
   */
  public abstract Object cast(final Object value) throws ConversionException;

  /**
   * Convert a value to the specified {@link Type} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *        numeric, or null if none
   * @return the converted value
   * @throws ConversionException if the casting fail
   */
  public abstract Object cast(final Object value, final String pattern) throws ConversionException;

  @Override
  public String toString() {
    return signature;
  }

  /**
   * Return a default {@link Type} for a value.
   *
   * @return The type or 'UNKNOWN' if not found
   */
  public static Type of(final Object value) {
    if (value == null)
      return UnknownType.UNKNOWN;
    if (value instanceof Boolean)
      return BooleanType.BOOLEAN;
    if (value instanceof String)
      return StringType.STRING;
    if (value instanceof BigDecimal) {
      BigDecimal number = (BigDecimal) value;
      return new NumberType(number.precision(), number.scale());
    }
    if (value instanceof Double)
      return NumberType.NUMBER;
    if (value instanceof Long)
      return IntegerType.INTEGER;
    if (value instanceof ZonedDateTime)
      return DateType.DATE;
    if (value instanceof JsonNode)
      return JsonType.JSON;
    if (value instanceof TimeUnit)
      return UnknownType.SYMBOL;
    if (value instanceof byte[])
      return BinaryType.BINARY;
    if (value instanceof Interval)
      return IntervalType.INTERVAL;

    return UnknownType.UNKNOWN;
  }
}

