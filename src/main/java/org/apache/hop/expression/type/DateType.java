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
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import org.apache.hop.expression.ConversionException;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.util.DateTimeFormat;

public final class DateType extends Type {

  DateType(boolean nullable) {
    super(PRECISION_NOT_SPECIFIED, SCALE_NOT_SPECIFIED, nullable);
    this.signature = generateSignature();
    this.checkPrecisionAndScale();
  }

  @Override
  public DateType withNullability(final boolean nullable) {
    return new DateType(nullable);
  }

  @Override
  public TypeId getId() {
    return TypeId.DATE;
  }

  @Override
  public TypeComparability getComparability() {
    return TypeComparability.ALL;
  }

  @Override
  public <T> T convert(Object value, Class<T> clazz) throws ConversionException {
    if (value == null) {
      return null;
    }
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }
    if (clazz == String.class) {
      return clazz.cast(StringType.convertToString((ZonedDateTime) value));
    }
    if (clazz == Long.class) {
      return clazz.cast(IntegerType.convertToInteger((ZonedDateTime) value));
    }
    if (clazz == BigDecimal.class) {
      return clazz.cast(NumberType.convertToNumber((ZonedDateTime) value));
    }
    return super.convert(value, clazz);
  }

  @Override
  public ZonedDateTime cast(final Object value) throws ConversionException {
    return cast(value, "FXYYYY-MM-DD");
  }

  /**
   * Convert a value to the specified type {@link DateType} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *     numeric, or null if none
   * @return the converted value
   */
  @Override
  public ZonedDateTime cast(final Object value, final String pattern) throws ConversionException {

    if (value == null) {
      return null;
    }

    if (value instanceof ZonedDateTime) {
      return (ZonedDateTime) value;
    }
    if (value instanceof String) {
      return DateTimeFormat.of(pattern).parse((String) value);
    }
    if (value instanceof Long) {
      return convertToDate((Long) value);
    }
    if (value instanceof BigDecimal) {
      return convertToDate((BigDecimal) value);
    }

    throw new ConversionException(
        ErrorCode.UNSUPPORTED_CONVERSION, value, TypeId.fromValue(value), this);
  }

  /**
   * Coerce value to data type {@link DateType}.
   *
   * @param value the value to coerce
   * @return ZonedDateTime
   */
  public static final ZonedDateTime coerce(final Object value) throws ConversionException {
    if (value == null) {
      return null;
    }

    if (value instanceof ZonedDateTime datetime) {
      return datetime;
    }

    throw new ConversionException(
        ErrorCode.UNSUPPORTED_COERCION, value, TypeId.fromValue(value), TypeId.DATE);
  }

  public static final ZonedDateTime convertToDate(final String value) throws ConversionException {
    return DateTimeFormat.of("FXYYY-MM-DD").parse(value);
  }

  /**
   * Convert the epoch time value to a timestamp with UTC time zone.
   *
   * @param seconds number of seconds that have elapsed since the epoch (00:00:00 UTC on January 1,
   *     1970)
   * @return
   */
  public static final ZonedDateTime convertToDate(final Long seconds) {
    Instant instant = Instant.ofEpochSecond(seconds);
    return ZonedDateTime.ofInstant(instant, ZoneOffset.UTC);
  }

  public static final ZonedDateTime convertToDate(final BigDecimal number) {
    long nanos = number.remainder(BigDecimal.ONE).movePointRight(9).abs().longValue();
    Instant instant = Instant.ofEpochSecond(number.longValue(), nanos);
    return ZonedDateTime.ofInstant(instant, ZoneOffset.UTC);
  }
}
