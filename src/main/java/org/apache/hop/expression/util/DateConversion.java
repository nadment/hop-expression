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

package org.apache.hop.expression.util;

import java.math.BigDecimal;
import java.time.DateTimeException;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Date;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.type.TypeName;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Conversions to and from a DATE, a date-time without time zone held by a {@link LocalDateTime}.
 */
@NullMarked
public final class DateConversion {

  private static final DateTimeFormat FORMAT = DateTimeFormat.of("FXYYY-MM-DD");

  private DateConversion() {
    // Utility class
  }

  /**
   * Converts a string in the format YYYY-MM-DD to a date, a string with a time zone keeps its local
   * date and time.
   *
   * @param value the string
   * @return the date without time zone
   */
  public static LocalDateTime convert(final String value) {
    return FORMAT.parse(value).toLocalDateTime();
  }

  /**
   * Convert the epoch time value to a date without time zone, see {@link
   * #toEpochSecond(LocalDateTime)}.
   *
   * @param seconds number of seconds that have elapsed since the epoch (00:00:00 UTC on January 1,
   *     1970)
   */
  public static @Nullable LocalDateTime convert(final @Nullable Long seconds) {
    if (seconds == null) {
      return null;
    }
    try {
      return ofEpoch(Instant.ofEpochSecond(seconds));
    } catch (DateTimeException e) {
      throw createConversionError(seconds);
    }
  }

  /**
   * Convert the epoch time value with the fractions of seconds to a date without time zone, see
   * {@link #toEpochSecond(LocalDateTime)}.
   *
   * @param number number of seconds that have elapsed since the epoch, the fractional part gives
   *     the nanoseconds
   */
  public static @Nullable LocalDateTime convert(final @Nullable BigDecimal number) {
    if (number == null) {
      return null;
    }
    try {
      long nanos = number.remainder(BigDecimal.ONE).movePointRight(9).longValue();
      // A number out of the range of epoch seconds raises an error instead of wrapping around
      return ofEpoch(Instant.ofEpochSecond(number.toBigInteger().longValueExact(), nanos));
    } catch (ArithmeticException | DateTimeException e) {
      throw createConversionError(number);
    }
  }

  /**
   * Converts a date or a timestamp of the row to a date without time zone, the local date-time it
   * shows in the system time zone, with the nanoseconds of a timestamp.
   *
   * @param date the date or the timestamp of the row
   * @return the date without time zone
   */
  public static LocalDateTime convert(final Date date) {
    return LocalDateTime.ofInstant(date.toInstant(), ZoneId.systemDefault());
  }

  /**
   * Returns the date without time zone of an epoch instant, its local date-time in UTC, the reverse
   * of {@link #toEpochSecond(LocalDateTime)}: 0 is 1970-01-01 00:00:00.
   */
  private static LocalDateTime ofEpoch(final Instant instant) {
    return LocalDateTime.ofInstant(instant, ZoneOffset.UTC);
  }

  /**
   * Returns the nominal number of seconds since the epoch 1970-01-01 00:00:00 of a date without
   * time zone, its local date-time read in UTC without regard to the time zone and the daylight
   * saving time: DATE '1970-01-01' is 0. The fraction of second is ignored.
   *
   * @param datetime the date without time zone
   * @return the number of seconds since the epoch
   */
  public static long toEpochSecond(final LocalDateTime datetime) {
    return datetime.toEpochSecond(ZoneOffset.UTC);
  }

  /**
   * Returns a date-time without time zone: a date-time with time zone keeps its local date-time,
   * the time zone is dropped.
   *
   * @param value a {@link LocalDateTime} or a {@link ZonedDateTime}
   * @return the date-time without time zone
   * @throws IllegalArgumentException if the value is not a date-time
   */
  public static LocalDateTime toLocal(final Object value) {
    return switch (value) {
      case LocalDateTime datetime -> datetime;
      case ZonedDateTime datetime -> datetime.toLocalDateTime();
      default -> throw new IllegalArgumentException("Not a date-time: " + value);
    };
  }

  private static ExpressionException createConversionError(final Object value) {
    return new ExpressionException(
        ErrorCode.CONVERSION_ERROR, TypeName.fromValue(value), TypeName.DATE, value);
  }
}
