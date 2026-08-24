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
import java.time.ZonedDateTime;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.util.IntegerConversion;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

@NullMarked
public final class IntegerType extends Type {
  /** Default INTEGER type with maximum precision. */
  public static final IntegerType INTEGER =
      new IntegerType(TypeName.INTEGER.getMaxPrecision(), true);

  /** Default INTEGER NOT NULL type with maximum precision. */
  public static final IntegerType INTEGER_NOT_NULL =
      new IntegerType(TypeName.INTEGER.getMaxPrecision(), false);

  IntegerType(int precision, boolean nullable) {
    super(precision, 0, nullable);
  }

  public static IntegerType from(Long value) {
    return of(numberOfDigit(value), false);
  }

  public static IntegerType from(Integer value) {
    return of(numberOfDigit(value), false);
  }

  public static IntegerType of(int precision) {
    return of(precision, true);
  }

  public static IntegerType of(int precision, boolean nullable) {
    if (precision == PRECISION_NOT_SPECIFIED) precision = TypeName.INTEGER.getMaxPrecision();

    if (precision == TypeName.INTEGER.getMaxPrecision() && nullable) return IntegerType.INTEGER;

    return new IntegerType(precision, nullable);
  }

  private static int numberOfDigit(long number) {
    if (number == 0) return 1;
    int count = 0;
    while (number != 0) {
      number = number / 10;
      ++count;
    }
    return count;
  }

  @Override
  public Type withNullability(boolean nullable) {
    if (nullable == this.isNullable()) {
      return this;
    }
    return new IntegerType(precision, nullable);
  }

  @Override
  public TypeName getName() {
    return TypeName.INTEGER;
  }

  @Override
  public @Nullable <T> T convert(final @Nullable Object value, final Class<T> clazz)
      throws ExpressionException {

    if (value == null) {
      return null;
    }
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }
    if (clazz == Boolean.class) {
      return clazz.cast(((Long) value) != 0);
    }
    if (clazz == BigDecimal.class) {
      return clazz.cast(BigDecimal.valueOf((Long) value));
    }
    if (clazz == String.class) {
      return clazz.cast(String.valueOf(value));
    }

    return super.convert(value, clazz);
  }

  @Override
  public @Nullable Long cast(final @Nullable Object value) throws ExpressionException {
    return cast(value, null);
  }

  @Override
  public @Nullable Long cast(final @Nullable Object value, final @Nullable String pattern)
      throws ExpressionException {
    return switch (value) {
      case null -> null;
      case Long integer -> integer;
      case BigDecimal number -> IntegerConversion.convert(number);
      case Boolean bool -> (bool) ? 1L : 0L;
      case String str -> IntegerConversion.convert(str);
      case ZonedDateTime datetime -> IntegerConversion.convert(datetime);
      default ->
          throw new ExpressionException(
              ErrorCode.UNSUPPORTED_CONVERSION, value, TypeName.fromValue(value), this);
    };
  }

  @Override
  public boolean compareEqual(@Nullable Object left, @Nullable Object right) {
    if (left instanceof Long l && right instanceof Long r) {
      return l.equals(r);
    }
    return super.compareEqual(left, right);
  }

  @Override
  public int compare(@Nullable Object left, @Nullable Object right) {
    if (left instanceof Long l && right instanceof Long r) {
      return l.compareTo(r);
    }
    return super.compare(left, right);
  }
}
