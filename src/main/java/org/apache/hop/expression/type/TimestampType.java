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
import org.apache.hop.expression.util.FractionalSeconds;
import org.apache.hop.expression.util.StringConversion;
import org.apache.hop.expression.util.TimestampConversion;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * The TIMESTAMP type, a date-time with a time zone, an offset like +02:00 or a region like
 * Europe/Paris that is kept with the value.
 *
 * <p>Two values are equal if they represent the same instant, whatever their time zone.
 */
@NullMarked
public final class TimestampType extends Type {

  /** Default TIMESTAMP type, the fractional seconds have 9 digits. */
  public static final TimestampType TIMESTAMP =
      new TimestampType(TypeName.TIMESTAMP.getMaxPrecision(), true);

  /** Default TIMESTAMP NOT NULL type. */
  public static final TimestampType TIMESTAMP_NOT_NULL =
      new TimestampType(TypeName.TIMESTAMP.getMaxPrecision(), false);

  /** The format used to cast a string without pattern, a time zone is optional. */
  private static final DateTimeFormat DEFAULT_FORMAT = DateTimeFormat.of("AUTO");

  TimestampType(int precision, boolean nullable) {
    super(precision, SCALE_NOT_SPECIFIED, nullable);
  }

  /**
   * Returns a nullable TIMESTAMP type.
   *
   * @param precision the number of digits of the fractional seconds, from 0 to 9, or {@link
   *     #PRECISION_NOT_SPECIFIED} for 9
   * @return the type
   */
  public static TimestampType of(int precision) {
    return of(precision, true);
  }

  /**
   * Returns a TIMESTAMP type.
   *
   * @param precision the number of digits of the fractional seconds, from 0 to 9, or {@link
   *     #PRECISION_NOT_SPECIFIED} for 9
   * @param nullable whether the type allows null values
   * @return the type
   */
  public static TimestampType of(int precision, boolean nullable) {
    if (precision == PRECISION_NOT_SPECIFIED) precision = TypeName.TIMESTAMP.getMaxPrecision();
    if (precision == TypeName.TIMESTAMP.getMaxPrecision()) {
      return nullable ? TIMESTAMP : TIMESTAMP_NOT_NULL;
    }
    return new TimestampType(precision, nullable);
  }

  @Override
  public TimestampType withNullability(boolean nullable) {
    if (nullable == this.isNullable()) {
      return this;
    }
    return new TimestampType(precision, nullable);
  }

  @Override
  public TypeName getName() {
    return TypeName.TIMESTAMP;
  }

  /**
   * {@inheritDoc}
   *
   * <p>A date without time zone keeps its local date and time.
   */
  @Override
  protected <T> T convertValue(final Object value, final Class<T> clazz)
      throws ExpressionException {
    if (clazz == ZonedDateTime.class) {
      return clazz.cast(TimestampConversion.toZoned(value));
    }
    if (clazz == LocalDateTime.class) {
      return clazz.cast(DateConversion.toLocal(value));
    }
    if (clazz == String.class) {
      // The fractional seconds have the digits of the precision of the type
      return clazz.cast(StringConversion.convert(TimestampConversion.toZoned(value), precision));
    }
    return super.convertValue(value, clazz);
  }

  @Override
  public @Nullable ZonedDateTime cast(final @Nullable Object value) throws ExpressionException {
    return cast(value, null);
  }

  /**
   * Convert a value to a TIMESTAMP with a pattern. A string or a date without time zone is in the
   * system time zone. The fractional seconds are rounded to the precision of the type.
   *
   * @param value the value to convert, a date-time, a string or an epoch time value
   * @param pattern the optional pattern of a string, or null for the ISO formats
   * @return the converted value
   */
  @Override
  public @Nullable ZonedDateTime cast(final @Nullable Object value, final @Nullable String pattern)
      throws ExpressionException {
    return FractionalSeconds.round(convert(value, pattern), precision);
  }

  private @Nullable ZonedDateTime convert(
      final @Nullable Object value, final @Nullable String pattern) throws ExpressionException {
    return switch (value) {
      case null -> null;
      case ZonedDateTime datetime -> datetime;
      case LocalDateTime datetime -> TimestampConversion.toZoned(datetime);
      case String str ->
          ((pattern == null) ? DEFAULT_FORMAT : DateTimeFormat.of(pattern)).parse(str);
        // An epoch time value is the instant, in UTC
      case Long number -> TimestampConversion.convert(number);
      case BigDecimal number -> TimestampConversion.convert(number);
      default ->
          throw new ExpressionException(
              ErrorCode.UNSUPPORTED_CONVERSION, value, TypeName.fromValue(value), this);
    };
  }

  /** {@inheritDoc} A date without time zone is compared in the system time zone. */
  @Override
  public boolean compareEqual(Object left, Object right) {
    if (DateType.isDateTime(left) && DateType.isDateTime(right)) {
      return TimestampConversion.toZoned(left).isEqual(TimestampConversion.toZoned(right));
    }
    return super.compareEqual(left, right);
  }

  /** {@inheritDoc} A date without time zone is compared in the system time zone. */
  @Override
  public int compare(Object left, Object right) {
    if (DateType.isDateTime(left) && DateType.isDateTime(right)) {
      // Two timestamps are equal if they represent the same moment in time
      return TimestampConversion.toZoned(left).compareTo(TimestampConversion.toZoned(right));
    }
    return super.compare(left, right);
  }
}
