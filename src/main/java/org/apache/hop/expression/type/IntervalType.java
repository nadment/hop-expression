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
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Interval;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

@NullMarked
public final class IntervalType extends Type {
  /** Default INTERVAL type with default parameters, the fractional seconds have 9 digits. */
  public static final IntervalType INTERVAL =
      new IntervalType(TypeName.INTERVAL.getMaxPrecision(), true);

  /** Default INTERVAL NOT NULL type with default parameters. */
  public static final IntervalType INTERVAL_NOT_NULL =
      new IntervalType(TypeName.INTERVAL.getMaxPrecision(), false);

  IntervalType(int precision, boolean nullable) {
    super(precision, SCALE_NOT_SPECIFIED, nullable);
  }

  /**
   * Returns a nullable INTERVAL type.
   *
   * @param precision the number of digits of the fractional seconds, from 0 to 9, or {@link
   *     #PRECISION_NOT_SPECIFIED} for 9
   * @return the type
   */
  public static IntervalType of(int precision) {
    return of(precision, true);
  }

  /**
   * Returns an INTERVAL type.
   *
   * @param precision the number of digits of the fractional seconds, from 0 to 9, or {@link
   *     #PRECISION_NOT_SPECIFIED} for 9
   * @param nullable whether the type allows null values
   * @return the type
   */
  public static IntervalType of(int precision, boolean nullable) {
    if (precision == PRECISION_NOT_SPECIFIED) precision = TypeName.INTERVAL.getMaxPrecision();
    if (precision == TypeName.INTERVAL.getMaxPrecision()) {
      return nullable ? INTERVAL : INTERVAL_NOT_NULL;
    }
    return new IntervalType(precision, nullable);
  }

  /**
   * Convert String value to Interval.
   *
   * @param str the string to convert
   * @return Interval
   */
  public static @Nullable Interval convert(final @Nullable String str) throws ExpressionException {
    if (str == null) return null;
    return Interval.of(str);
  }

  @Override
  public IntervalType withNullability(boolean nullable) {
    if (nullable == this.isNullable()) {
      return this;
    }
    return new IntervalType(precision, nullable);
  }

  @Override
  public TypeName getName() {
    return TypeName.INTERVAL;
  }

  @Override
  protected <T> T convertValue(final Object value, final Class<T> clazz)
      throws ExpressionException {
    if (clazz == String.class) {
      return clazz.cast(value.toString());
    }

    return super.convertValue(value, clazz);
  }

  @Override
  public @Nullable Interval cast(final @Nullable Object value) throws ExpressionException {
    return cast(value, null);
  }

  /** {@inheritDoc} The fractional seconds are rounded to the precision of the type. */
  @Override
  public @Nullable Interval cast(final @Nullable Object value, final @Nullable String pattern)
      throws ExpressionException {
    Interval interval =
        switch (value) {
          case null -> null;
          case Interval i -> i;
          case String str -> Interval.of(str);
          default ->
              throw new ExpressionException(
                  ErrorCode.UNSUPPORTED_CONVERSION, value, TypeName.fromValue(value), this);
        };
    if (interval == null) {
      return null;
    }
    try {
      return interval.round(precision);
    } catch (ArithmeticException e) {
      throw new ExpressionException(ErrorCode.INTERVAL_OVERFLOW, "CAST");
    }
  }

  @Override
  public boolean compareEqual(Object left, Object right) {
    if (left instanceof Interval l && right instanceof Interval r) {
      return l.compareTo(r) == 0;
    }
    return super.compareEqual(left, right);
  }

  @Override
  public int compare(Object left, Object right) {
    if (left instanceof Interval l && right instanceof Interval r) {
      return l.compareTo(r);
    }
    return super.compare(left, right);
  }
}
