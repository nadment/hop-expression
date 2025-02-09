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

import java.util.Objects;
import org.apache.hop.expression.ConversionException;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;

/**
 * Data type
 *
 * <p>Identity is based upon the {@link #signature} field, which each derived class should set
 * during construction.
 */
public abstract class Type {

  public static final int SCALE_NOT_SPECIFIED = -1;
  public static final int PRECISION_NOT_SPECIFIED = -1;

  protected final int precision;
  protected final int scale;
  protected final boolean nullable;
  protected String signature;

  protected Type(int precision, int scale, boolean nullable) {
    this.precision = precision;
    this.scale = scale;
    this.nullable = nullable;
  }

  /**
   * Generates a string representation of this type.
   *
   * @return string
   */
  protected String generateSignature() {
    StringBuilder builder = new StringBuilder();
    TypeName id = getName();
    builder.append(id.name());
    if (precision != id.getMaxPrecision() || (scale > 0 && scale != id.getDefaultScale())) {
      builder.append('(');
      builder.append(precision);
      if (scale > 0) {
        builder.append(',');
        builder.append(scale);
      }
      builder.append(')');
    }
    return builder.toString();
  }

  /** Check precision and scale. */
  protected void checkPrecisionAndScale() {
    TypeName id = getName();
    if (id.supportsPrecision()
        && (precision < id.getMinPrecision() || precision > id.getMaxPrecision())) {
      throw new ExpressionException(
          ErrorCode.PRECISION_OUT_OF_RANGE, signature, id.getMinPrecision(), id.getMaxPrecision());
    }
    if (id.supportsScale() && (scale < id.getMinScale() || scale > id.getMaxScale())) {
      throw new ExpressionException(
          ErrorCode.SCALE_OUT_OF_RANGE, signature, id.getMinScale(), id.getMaxScale());
    }
    if (scale > precision) {
      throw new ExpressionException(ErrorCode.SCALE_GREATER_THAN_PRECISION, signature);
    }
  }

  /**
   * Gets the {@link TypeName} of this type.
   *
   * @return name, never null
   */
  public abstract TypeName getName();

  /**
   * Gets the {@link TypeFamily} of this type.
   *
   * @return family, never null
   */
  public TypeFamily getFamily() {
    return getName().getFamily();
  }

  public boolean is(final TypeName name) {
    return getName() == name;
  }

  public boolean isFamily(final TypeFamily family) {
    return getName().isFamily(family);
  }

  /** Returns whether this {@link Type} support implicit coercion to the specified {@link Type}. */
  public boolean isCoercible(final Type type) {
    return getName().isCoercible(type.getName());
  }

  /** Returns whether a type is atomic (date, numeric, string or BOOLEAN). */
  public boolean isAtomic() {
    TypeName name = getName();
    return name == TypeName.STRING
        || name == TypeName.DATE
        || name == TypeName.INTEGER
        || name == TypeName.NUMBER
        || name == TypeName.BOOLEAN
        || name == TypeName.BINARY
        || name == TypeName.INET;
  }

  /**
   * Gets the {@link TypeComparability} of this type used by comparison operators.
   *
   * @return comparability, never null
   */
  public abstract TypeComparability getComparability();

  public boolean compareEqual(final Object left, final Object right) {
    throw new ExpressionException(ErrorCode.INTERNAL_ERROR, "Equals error");
  }

  public boolean compareEqualNull(final Object left, final Object right) {
    if (left == null) {
      return right == null;
    }
    if (right == null) return false;

    return compareEqual(left, right);
  }

  public int compare(final Object left, final Object right) {
    throw new ExpressionException(ErrorCode.INTERNAL_ERROR, "Compare error");
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
   * <p>Returns {@link #PRECISION_NOT_SPECIFIED} (-1) if precision is not applicable for this type.
   *
   * @return number of decimal digits for exact numeric types; number of decimal digits in mantissa
   *     for approximate numeric types; number of decimal digits for fractional seconds of datetime
   *     types; length in characters for String types; length in bytes for Binary types; 1 for
   *     BOOLEAN; -1 if precision is not valid for this type
   */
  public final int getPrecision() {
    return precision;
  }

  /**
   * Gets the scale of this type. Returns {@link #SCALE_NOT_SPECIFIED} (-1) if scale is not valid
   * for this type.
   *
   * @return number of digits of scale
   */
  public final int getScale() {
    return scale;
  }

  /**
   * Gets the element type if this type is a collection, otherwise null.
   *
   * @return type the element type
   */
  public Type getElementType() {
    return null;
  }

  /** Indicates whether that type are equal with each other by ignoring the nullability. */
  public boolean equalsIgnoreNullability(final Type type) {
    if (type == null) return false;
    return this.signature.equals(type.signature);
  }

  @Override
  public final boolean equals(Object obj) {
    return this == obj
        || obj instanceof Type type
            && Objects.equals(this.signature, type.signature)
            && nullable == type.nullable;
  }

  @Override
  public final int hashCode() {
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
    throw new ConversionException(
        ErrorCode.UNSUPPORTED_CONVERSION,
        value,
        TypeName.fromValue(value),
        TypeName.fromClass(clazz));
  }

  /**
   * Convert a value to the specified {@link Type}.
   *
   * @param value the value to convert
   * @return the converted value
   * @throws ConversionException if the casting fail
   */
  public Object cast(final Object value) throws ConversionException {
    throw new ConversionException(ErrorCode.INTERNAL_ERROR);
  }

  /**
   * Convert a value to the specified {@link Type} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *     numeric, or null if none
   * @return the converted value
   * @throws ConversionException if the casting fail
   */
  public Object cast(final Object value, final String pattern) throws ConversionException {
    throw new ConversionException(ErrorCode.INTERNAL_ERROR);
  }

  @Override
  public String toString() {
    return signature;
  }
}
