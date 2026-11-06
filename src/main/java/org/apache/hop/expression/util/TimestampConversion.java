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
import java.sql.Timestamp;
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
 * Conversions to and from a TIMESTAMP, a date-time with a time zone held by a {@link
 * ZonedDateTime}.
 */
@NullMarked
public final class TimestampConversion {

  private static final DateTimeFormat FORMAT = DateTimeFormat.of("FXYYY-MM-DD");

  private TimestampConversion() {
    // Utility class
  }

  /**
   * Converts a string in the format YYYY-MM-DD to a timestamp, in the system time zone if it has no
   * time zone.
   *
   * @param value the string
   * @return the timestamp
   */
  public static ZonedDateTime convert(final String value) {
    return FORMAT.parse(value);
  }

  /**
   * Convert the epoch time value to a timestamp at its instant, in the UTC time zone: 0 is
   * 1970-01-01 00:00:00 +00:00.
   *
   * @param seconds number of seconds that have elapsed since the epoch (00:00:00 UTC on January 1,
   *     1970)
   */
  public static @Nullable ZonedDateTime convert(final @Nullable Long seconds) {
    if (seconds == null) {
      return null;
    }
    try {
      return ZonedDateTime.ofInstant(Instant.ofEpochSecond(seconds), ZoneOffset.UTC);
    } catch (DateTimeException e) {
      throw createConversionError(seconds);
    }
  }

  /**
   * Convert the epoch time value with the fractions of seconds to a timestamp at its instant, in
   * the UTC time zone.
   *
   * @param number number of seconds that have elapsed since the epoch, the fractional part gives
   *     the nanoseconds
   */
  public static @Nullable ZonedDateTime convert(final @Nullable BigDecimal number) {
    if (number == null) {
      return null;
    }
    try {
      long nanos = number.remainder(BigDecimal.ONE).movePointRight(9).longValue();
      // A number out of the range of epoch seconds raises an error instead of wrapping around
      Instant instant = Instant.ofEpochSecond(number.toBigInteger().longValueExact(), nanos);
      return ZonedDateTime.ofInstant(instant, ZoneOffset.UTC);
    } catch (ArithmeticException | DateTimeException e) {
      throw createConversionError(number);
    }
  }

  private static ExpressionException createConversionError(final Object value) {
    return new ExpressionException(
        ErrorCode.CONVERSION_ERROR, TypeName.fromValue(value), TypeName.TIMESTAMP, value);
  }

  /**
   * Returns a date-time with time zone: a date without time zone is in the system time zone.
   *
   * @param value a {@link LocalDateTime} or a {@link ZonedDateTime}
   * @return the date-time with time zone
   * @throws IllegalArgumentException if the value is not a date-time
   */
  public static ZonedDateTime toZoned(final Object value) {
    return switch (value) {
      case ZonedDateTime datetime -> datetime;
      case LocalDateTime datetime -> datetime.atZone(ZoneId.systemDefault());
      default -> throw new IllegalArgumentException("Not a date-time: " + value);
    };
  }

  /**
   * Converts a date-time to a date of the row, at the same instant.
   *
   * @param datetime the date-time
   * @return the date of the row
   */
  public static Date toDate(final ZonedDateTime datetime) {
    return Date.from(datetime.toInstant());
  }

  /**
   * Converts a date-time to a timestamp of the row, at the same instant with the nanoseconds.
   *
   * @param datetime the date-time
   * @return the timestamp of the row
   */
  public static Timestamp toTimestamp(final ZonedDateTime datetime) {
    return Timestamp.from(datetime.toInstant());
  }
}
