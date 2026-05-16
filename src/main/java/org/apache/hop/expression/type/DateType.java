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
import org.apache.hop.expression.util.DateTimeConversion;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.IntegerConversion;
import org.apache.hop.expression.util.NumberConversion;
import org.apache.hop.expression.util.StringConversion;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

@NullMarked
public final class DateType extends Type {

  /** Default DATE type with default parameters. */
  public static final DateType DATE = new DateType(true);

  /** Default DATE NOT NULL type with default parameters. */
  public static final DateType DATE_NOT_NULL = new DateType(false);

  DateType(boolean nullable) {
    super(PRECISION_NOT_SPECIFIED, SCALE_NOT_SPECIFIED, nullable);
    this.signature = generateSignature();
    this.checkPrecisionAndScale();
  }

  @Override
  public DateType withNullability(boolean nullable) {
    if (nullable == this.isNullable()) {
      return this;
    }
    return new DateType(nullable);
  }

  @Override
  public TypeName getName() {
    return TypeName.DATE;
  }

  @Override
  public TypeComparability getComparability() {
    return TypeComparability.ALL;
  }

  @Override
  public @Nullable <T> T convert(@Nullable Object value, Class<T> clazz)
      throws ExpressionException {
    if (value == null) {
      return null;
    }
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }
    if (clazz == String.class) {
      return clazz.cast(StringConversion.convert((ZonedDateTime) value));
    }
    if (clazz == Long.class) {
      return clazz.cast(IntegerConversion.convert((ZonedDateTime) value));
    }
    if (clazz == BigDecimal.class) {
      return clazz.cast(NumberConversion.convert((ZonedDateTime) value));
    }
    return super.convert(value, clazz);
  }

  @Override
  public @Nullable ZonedDateTime cast(final @Nullable Object value) throws ExpressionException {
    return cast(value, "FXYYYY-MM-DD");
  }

  /**
   * Convert a value to the specified type {@link DateType} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when the value is date or
   *     numeric, or null if none
   * @return the converted value
   */
  @Override
  public @Nullable ZonedDateTime cast(final @Nullable Object value, final @Nullable String pattern)
      throws ExpressionException {
    return switch (value) {
      case null -> null;
      case ZonedDateTime datetime -> datetime;
      case String str -> DateTimeFormat.of(pattern).parse(str);
      case Long number -> DateTimeConversion.convert(number);
      case BigDecimal number -> DateTimeConversion.convert(number);
      default ->
          throw new ExpressionException(
              ErrorCode.UNSUPPORTED_CONVERSION, value, TypeName.fromValue(value), this);
    };
  }

  @Override
  public boolean compareEqual(@Nullable Object left, @Nullable Object right) {
    if (left instanceof ZonedDateTime l && right instanceof ZonedDateTime r) {
      return l.isEqual(r);
    }
    return super.compareEqual(left, right);
  }

  @Override
  public int compare(@Nullable Object left, @Nullable Object right) {
    if (left instanceof ZonedDateTime l && right instanceof ZonedDateTime r) {
      // Two timestamps are equal if they represent the same moment in time:
      // Timestamp '2019-01-01 8:00:00 -8:00' = Timestamp '2019-01-01 11:00:00 -5:00'
      return l.compareTo(r);
    }
    return super.compare(left, right);
  }
}
