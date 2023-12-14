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

import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.exception.ConversionException;
import java.math.BigDecimal;
import java.time.ZonedDateTime;
import java.util.Objects;
import com.fasterxml.jackson.databind.JsonNode;

public abstract class Type {

  public static final int SCALE_NOT_SPECIFIED = -1;
  public static final int PRECISION_NOT_SPECIFIED = -1;
  
  protected final int precision;
  protected final int scale;
  protected final boolean nullable;
  private final String signature;

  protected Type(int precision, int scale, boolean nullable) {
    this.precision = precision;
    this.scale = scale;
    this.nullable = nullable;

    // Generates a string representation of this type.
    TypeId id = getId();
    StringBuilder builder = new StringBuilder();
    builder.append(id.name());
    if (precision != id.getMaxPrecision() || ( scale>0 && scale!=id.getDefaultScale() ) ) {
      builder.append('(');
      builder.append(precision);
      if (scale > 0) {
        builder.append(',');
        builder.append(scale);
      }
      builder.append(')');
    }
    this.signature = builder.toString();

    // Check precision range
    if (id.supportsPrecision() && ( precision < id.getMinPrecision() ||  precision > id.getMaxPrecision()) ) {
      throw new IllegalArgumentException(ErrorCode.PRECISION_OUT_OF_RANGE.message(signature,id.getMinPrecision(),id.getMaxPrecision()));
    }
    // Check scale range
    if (id.supportsScale() && ( scale < id.getMinScale() ||  scale > id.getMaxScale()) ) {
      throw new IllegalArgumentException(ErrorCode.SCALE_OUT_OF_RANGE.message(signature,id.getMinScale(),id.getMaxScale()));
    }
    if (scale>precision ) {
      throw new IllegalArgumentException(ErrorCode.SCALE_GREATER_THAN_PRECISION.message(signature));
    }
  }
  
  /**
   * Gets the {@link TypeId} of this type.
   *
   * @return name, never null
   */
  public abstract TypeId getId();

  /**
   * Gets the {@link TypeFamily} of this type.
   *
   * @return family, never null
   */
  public TypeFamily getFamily() {
    return getId().getFamily();
  }

  public boolean is(final TypeId id) {
    return getId() == id;
  }

  public boolean isFamily(final TypeFamily family) {
    return getId().isFamily(family);
  }

  public boolean isFamily(final Type type) {
    return getId().isFamily(type.getFamily());
  }

  public boolean isCompatibleWithCoercion(final Type type) {
    return getId().isCompatibleWithCoercion(type.getFamily());
  }

  /**
   * Queries whether this type allows null values.
   *
   * @return whether type allows null values
   */
  public boolean isNullable() {
    return nullable;
  }

  public abstract Type withNullability(final boolean nullable);

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
  public final int getPrecision() {
    return precision;
  }

  /**
   * Gets the scale of this type.
   * Returns {@link #SCALE_NOT_SPECIFIED} (-1) if scale is not valid for this type.
   *
   * @return number of digits of scale
   */
  public final int getScale() {
    return scale;
  }

  @Override
  public boolean equals(Object obj) {
    return this == obj
        || obj instanceof Type && Objects.equals(this.signature, ((Type) obj).signature)
            && nullable == ((Type) obj).nullable;
  }

  @Override
  public int hashCode() {
    return Objects.hash(signature, nullable);
  }

  /**
   * Convert a value from this data type to the specified Java type.
   *
   * @param value the value to convert
   * @param clazz Desired Java type
   * @return the converted value
   * @throws ConversionException if the casting fail
   */
  public <T> T convert(final Object value, Class<T> clazz) throws ConversionException {
    throw new ConversionException(ErrorCode.UNSUPPORTED_COERCION, value, Type.valueOf(value),
        TypeId.fromJavaClass(clazz));
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
   * Return a default {@link Type} from a value.
   * 
   * @return The type or 'UNKNOWN' if not found
   */
  public static Type valueOf(final Object value) {
    if (value == null) {
      return UnknownType.UNKNOWN;
    }
    if (value instanceof Boolean) {
      return BooleanType.BOOLEAN;
    }
    if (value instanceof String) {
      return StringType.from((String) value);
    }
    if (value instanceof BigDecimal) {
      return NumberType.from((BigDecimal) value);
    }
    if (value instanceof Double) {
      return NumberType.from(BigDecimal.valueOf((Double) value));
    }
    if (value instanceof Long) {
      return IntegerType.from((Long) value);
    }
    if (value instanceof Integer) {
      return IntegerType.from((Integer) value);
    }
    if (value instanceof byte[]) {
      return BinaryType.from((byte[]) value);
    }
    if (value instanceof ZonedDateTime) {
      return DateType.DATE;
    }
    if (value instanceof JsonNode) {
      return JsonType.JSON;
    }
    if (value instanceof Interval) {
      return IntervalType.INTERVAL;
    }

    return UnknownType.UNKNOWN;
  }
}

