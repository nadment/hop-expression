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
import java.math.BigInteger;
import java.math.RoundingMode;
import java.time.ZonedDateTime;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.util.NumberConversion;
import org.apache.hop.expression.util.NumberFormat;

/** Number type with an optional precision and scale: */
public final class NumberType extends Type {
  /** Default NUMBER(38,9) type with max precision and default scale. */
  public static final NumberType NUMBER =
      new NumberType(TypeName.NUMBER.getMaxPrecision(), TypeName.NUMBER.getDefaultScale(), true);

  /** Default NUMBER(38,9) NOT NULL type with max precision and default scale. */
  public static final NumberType NUMBER_NOT_NULL =
      new NumberType(TypeName.NUMBER.getMaxPrecision(), TypeName.NUMBER.getDefaultScale(), false);

  NumberType(int precision, int scale, boolean nullable) {
    super(precision, scale, nullable);
    this.signature = generateSignature();
    this.checkPrecisionAndScale();
  }

  public static NumberType from(final BigDecimal number) {
    int precision = number.precision();
    int scale = number.scale();

    if (precision == scale) precision = scale + 1;
    else if (precision < scale) precision += scale;

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
    if (precision == PRECISION_NOT_SPECIFIED) precision = TypeName.NUMBER.getMaxPrecision();
    if (scale == SCALE_NOT_SPECIFIED) scale = 0;

    if (precision == TypeName.NUMBER.getMaxPrecision()
        && scale == TypeName.NUMBER.getDefaultScale()
        && nullable) return NumberType.NUMBER;

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
  public TypeComparability getComparability() {
    return TypeComparability.ALL;
  }

  @Override
  public <T> T convert(final Object value, final Class<T> clazz) throws ExpressionException {

    if (value == null) {
      return null;
    }
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }
    if (clazz == Boolean.class) {
      return clazz.cast(!BigInteger.ZERO.equals(((BigDecimal) value).unscaledValue()));
    }
    if (clazz == Long.class) {
      return clazz.cast(((BigDecimal) value).longValue());
    }
    if (clazz == String.class) {
      return clazz.cast(NumberFormat.of("TM").format((BigDecimal) value));
    }

    return super.convert(value, clazz);
  }

  @Override
  public BigDecimal cast(final Object value) throws ExpressionException {
    return cast(value, null);
  }

  /**
   * Convert a value to the specified type {@link NumberType} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *     numeric, or null if none
   * @return the converted value
   */
  @Override
  public BigDecimal cast(final Object value, final String pattern) throws ExpressionException {

    if (value == null) {
      return null;
    }

    if (value instanceof BigDecimal number) {
      if (this.scale != SCALE_NOT_SPECIFIED) {
        number = number.setScale(scale, RoundingMode.DOWN);
      }
      return number;
    }
    if (value instanceof Boolean bool) {
      return (bool) ? BigDecimal.ONE : BigDecimal.ZERO;
    }
    if (value instanceof Long l) {
      if (l == 0L) return BigDecimal.ZERO;
      if (l == 1L) return BigDecimal.ONE;
      return BigDecimal.valueOf(l);
    }
    if (value instanceof String str) {
      return NumberConversion.convert(str);
    }
    if (value instanceof ZonedDateTime dt) {
      return NumberConversion.convert(dt);
    }

    throw new ExpressionException(
        ErrorCode.UNSUPPORTED_CONVERSION, value, TypeName.fromValue(value), this);
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
