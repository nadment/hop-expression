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

@NullMarked
public final class DateTimeConversion {

  private static final DateTimeFormat FORMAT = DateTimeFormat.of("FXYYY-MM-DD");

  private DateTimeConversion() {
    // Utility class
  }

  public static ZonedDateTime convert(final String value) {
    return FORMAT.parse(value);
  }

  /**
   * Convert the epoch time value to a date-time in the system time zone, see {@link
   * #toEpochSecond(ZonedDateTime)}.
   *
   * @param seconds number of seconds that have elapsed since the epoch (00:00:00 UTC on January 1,
   *     1970)
   */
  public static @Nullable ZonedDateTime convert(final @Nullable Long seconds) {
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
   * Convert the epoch time value with the fractions of seconds to a date-time in the system time
   * zone, see {@link #toEpochSecond(ZonedDateTime)}.
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
      return ofEpoch(Instant.ofEpochSecond(number.toBigInteger().longValueExact(), nanos));
    } catch (ArithmeticException | DateTimeException e) {
      throw createConversionError(number);
    }
  }

  /**
   * Returns the date-time in the system time zone with the local date-time of an epoch instant in
   * UTC, the reverse of {@link #toEpochSecond(ZonedDateTime)}: 0 is 1970-01-01 00:00:00.
   */
  private static ZonedDateTime ofEpoch(final Instant instant) {
    return LocalDateTime.ofInstant(instant, ZoneOffset.UTC).atZone(ZoneId.systemDefault());
  }

  /**
   * Returns the number of seconds since the epoch 1970-01-01 00:00:00, the fraction of second is
   * ignored.
   *
   * <p>A date-time in the system time zone is a date without time zone, like a date literal or a
   * date field: the number of seconds is nominal, its local date-time is read in UTC without regard
   * to the time zone and the daylight saving time, DATE '1970-01-01' is 0. A date-time with another
   * time zone gives the number of seconds of its instant, TIMESTAMP '1970-01-01 02:00:00 +02:00' is
   * 0.
   *
   * @param datetime the date-time
   * @return the number of seconds since the epoch
   */
  public static long toEpochSecond(final ZonedDateTime datetime) {
    if (datetime.getZone().equals(ZoneId.systemDefault())) {
      return datetime.toLocalDateTime().toEpochSecond(ZoneOffset.UTC);
    }
    return datetime.toEpochSecond();
  }

  /**
   * Converts a date or a timestamp of the row to a date-time in the system time zone, like the date
   * literals and the date functions. The instant is kept, with the nanoseconds of a timestamp.
   *
   * @param date the date or the timestamp of the row
   * @return the date-time in the system time zone
   */
  public static ZonedDateTime convert(final Date date) {
    return ZonedDateTime.ofInstant(date.toInstant(), ZoneId.systemDefault());
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

  private static ExpressionException createConversionError(final Object value) {
    return new ExpressionException(
        ErrorCode.CONVERSION_ERROR, TypeName.fromValue(value), TypeName.DATE, value);
  }
}
