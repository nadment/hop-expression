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
import java.math.RoundingMode;
import java.time.ZonedDateTime;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.util.NumberConversion;
import org.apache.hop.expression.util.NumberFormat;
import org.apache.hop.expression.util.StringConversion;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** Number type with optional precision and scale: */
@NullMarked
public final class NumberType extends Type {
  /** Default NUMBER type with max precision and no fixed scale, a value keeps its decimals. */
  public static final NumberType NUMBER =
      new NumberType(TypeName.NUMBER.getMaxPrecision(), SCALE_NOT_SPECIFIED, true);

  /** Default NUMBER NOT NULL type with max precision and no fixed scale. */
  public static final NumberType NUMBER_NOT_NULL =
      new NumberType(TypeName.NUMBER.getMaxPrecision(), SCALE_NOT_SPECIFIED, false);

  NumberType(int precision, int scale, boolean nullable) {
    super(precision, scale, nullable);
  }

  public static NumberType from(final BigDecimal number) {
    int precision = number.precision();
    int scale = number.scale();

    // A negative scale, like 1E30, counts the zeros of the integer part as digits
    if (scale < 0) {
      precision -= scale;
      scale = 0;
    }

    if (precision == scale) precision = scale + 1;
    else if (precision < scale) precision += scale;

    // A value can exceed the limits of the type, like 1E50 or 1E-50, the type is then the widest
    precision = Math.min(precision, TypeName.NUMBER.getMaxPrecision());
    scale = Math.min(scale, TypeName.NUMBER.getMaxScale());

    return new NumberType(precision, scale, false);
  }

  public static NumberType of(int precision) {
    return of(precision, 0, true);
  }

  public static NumberType of(int precision, int scale) {
    return of(precision, scale, true);
  }

  /**
   * Create a number of data type
   *
   * @param precision Total number of digits allowed.
   * @param scale Number of digits allowed to the right of the decimal point.
   * @param nullable
   * @return
   */
  public static NumberType of(int precision, int scale, boolean nullable) {
    // NUMBER without precision and scale has no fixed scale
    if (precision == PRECISION_NOT_SPECIFIED) {
      if (scale == SCALE_NOT_SPECIFIED) {
        return nullable ? NUMBER : NUMBER_NOT_NULL;
      }
      precision = TypeName.NUMBER.getMaxPrecision();
    }

    // NUMBER(p) has a scale of 0
    if (scale == SCALE_NOT_SPECIFIED) scale = 0;

    return new NumberType(precision, scale, nullable);
  }

  @Override
  public NumberType withNullability(boolean nullable) {
    if (nullable == this.isNullable()) {
      return this;
    }
    return new NumberType(precision, scale, nullable);
  }

  @Override
  public TypeName getName() {
    return TypeName.NUMBER;
  }

  @Override
  protected <T> T convertValue(final Object value, final Class<T> clazz)
      throws ExpressionException {
    if (clazz == Boolean.class) {
      // The sign avoids allocating the unscaled value
      return clazz.cast(((BigDecimal) value).signum() != 0);
    }
    if (clazz == Long.class) {
      // Truncate the decimal part, a value out of the integer range raises an error instead of
      // keeping only the low-order 64 bits
      try {
        return clazz.cast(((BigDecimal) value).toBigInteger().longValueExact());
      } catch (ArithmeticException e) {
        throw new ExpressionException(ErrorCode.INTEGER_CONVERSION_OVERFLOW, value);
      }
    }
    if (clazz == String.class) {
      return clazz.cast(StringConversion.convert((BigDecimal) value));
    }

    return super.convertValue(value, clazz);
  }

  /**
   * Convert a value to the specified type {@link NumberType} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when the value is date or
   *     numeric, or null if none
   * @return the converted value
   */
  @Override
  public @Nullable BigDecimal cast(final @Nullable Object value, final @Nullable String pattern)
      throws ExpressionException {

    if (value == null) {
      return null;
    }

    if (value instanceof BigDecimal number) {
      return checkOverflow(round(number));
    }
    if (value instanceof Boolean bool) {
      return checkOverflow((bool) ? BigDecimal.ONE : BigDecimal.ZERO);
    }
    if (value instanceof Long l) {
      return checkOverflow(BigDecimal.valueOf(l));
    }
    if (value instanceof String str) {
      // A string is parsed with the format if any, then rounded like a number
      return checkOverflow(
          round(
              (pattern == null)
                  ? NumberConversion.convert(str)
                  : NumberFormat.of(pattern).parse(str)));
    }
    if (value instanceof ZonedDateTime dt) {
      // The fractions of seconds are rounded like a number
      return checkOverflow(round(NumberConversion.convert(dt)));
    }

    throw new ExpressionException(
        ErrorCode.UNSUPPORTED_CONVERSION, value, TypeName.fromValue(value), this);
  }

  /**
   * Rounds the number to the scale of this type like a cast to INTEGER, halfway values are rounded
   * away from zero. A number without fixed scale keeps its decimals.
   */
  private BigDecimal round(BigDecimal number) {
    if (scale == SCALE_NOT_SPECIFIED) {
      return number;
    }
    return number.setScale(scale, RoundingMode.HALF_UP);
  }

  /** Raises an error if the integer part of the number exceeds the precision minus the scale. */
  private BigDecimal checkOverflow(BigDecimal number) throws ExpressionException {
    // The integer part of zero has no digit, so zero fits in a type like NUMBER(1,1)
    int integerDigits = (number.signum() == 0) ? 0 : number.precision() - number.scale();
    if (precision != PRECISION_NOT_SPECIFIED && integerDigits > precision - Math.max(scale, 0)) {
      throw new ExpressionException(ErrorCode.NUMBER_CONVERSION_OVERFLOW, number, this);
    }
    return number;
  }

  @Override
  public boolean compareEqual(Object left, Object right) {
    if (left instanceof BigDecimal l && right instanceof BigDecimal r) {
      return l.compareTo(r) == 0;
    }
    return super.compareEqual(left, right);
  }

  @Override
  public int compare(Object left, Object right) {
    if (left instanceof BigDecimal l && right instanceof BigDecimal r) {
      return l.compareTo(r);
    }
    return super.compare(left, right);
  }
}
