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
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.util.DateConversion;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.IntegerConversion;
import org.apache.hop.expression.util.NumberConversion;
import org.apache.hop.expression.util.StringConversion;
import org.apache.hop.expression.util.TimestampConversion;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

@NullMarked
public final class DateType extends Type {

  /** Default DATE type with default parameters. */
  public static final DateType DATE = new DateType(true);

  /** Default DATE NOT NULL type with default parameters. */
  public static final DateType DATE_NOT_NULL = new DateType(false);

  /** The format used to cast a string to a date without pattern. */
  private static final DateTimeFormat DEFAULT_FORMAT = DateTimeFormat.of("AUTO");

  DateType(boolean nullable) {
    super(PRECISION_NOT_SPECIFIED, SCALE_NOT_SPECIFIED, nullable);
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

  /**
   * {@inheritDoc}
   *
   * <p>A date is a {@link LocalDateTime}, a date without time zone. A {@link ZonedDateTime}
   * returned by a date function is also accepted. A date without time zone is in the system time
   * zone.
   */
  @Override
  protected <T> T convertValue(final Object value, final Class<T> clazz)
      throws ExpressionException {
    if (clazz == LocalDateTime.class) {
      return clazz.cast(DateConversion.toLocal(value));
    }
    ZonedDateTime datetime = TimestampConversion.toZoned(value);
    if (clazz == ZonedDateTime.class) {
      return clazz.cast(datetime);
    }
    if (clazz == String.class) {
      return clazz.cast(StringConversion.convert(datetime));
    }
    // The epoch of a date without time zone is nominal
    if (clazz == Long.class) {
      return clazz.cast(IntegerConversion.convert(DateConversion.toLocal(value)));
    }
    if (clazz == BigDecimal.class) {
      return clazz.cast(NumberConversion.convert(DateConversion.toLocal(value)));
    }
    return super.convertValue(value, clazz);
  }

  @Override
  public @Nullable LocalDateTime cast(final @Nullable Object value) throws ExpressionException {
    return cast(value, "FXYYYY-MM-DD");
  }

  /**
   * Convert a value to the specified type {@link DateType} with a pattern. A date with time zone
   * keeps its local date and time.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when the value is date or
   *     numeric, or null if none
   * @return the converted value
   */
  @Override
  public @Nullable LocalDateTime cast(final @Nullable Object value, final @Nullable String pattern)
      throws ExpressionException {
    return switch (value) {
      case null -> null;
      case LocalDateTime datetime -> datetime;
      case ZonedDateTime datetime -> datetime.toLocalDateTime();
      case String str ->
          ((pattern == null) ? DEFAULT_FORMAT : DateTimeFormat.of(pattern))
              .parse(str)
              .toLocalDateTime();
      case Long number -> DateConversion.convert(number);
      case BigDecimal number -> DateConversion.convert(number);
      default ->
          throw new ExpressionException(
              ErrorCode.UNSUPPORTED_CONVERSION, value, TypeName.fromValue(value), this);
    };
  }

  /** {@inheritDoc} A date without time zone is compared in the system time zone. */
  @Override
  public boolean compareEqual(Object left, Object right) {
    if (isDateTime(left) && isDateTime(right)) {
      return TimestampConversion.toZoned(left).isEqual(TimestampConversion.toZoned(right));
    }
    return super.compareEqual(left, right);
  }

  /** {@inheritDoc} A date without time zone is compared in the system time zone. */
  @Override
  public int compare(Object left, Object right) {
    if (isDateTime(left) && isDateTime(right)) {
      // Two timestamps are equal if they represent the same moment in time:
      // Timestamp '2019-01-01 8:00:00 -8:00' = Timestamp '2019-01-01 11:00:00 -5:00'
      return TimestampConversion.toZoned(left).compareTo(TimestampConversion.toZoned(right));
    }
    return super.compare(left, right);
  }

  /** Returns whether the value is a date-time, without or with time zone. */
  public static boolean isDateTime(Object value) {
    return value instanceof LocalDateTime || value instanceof ZonedDateTime;
  }
}
