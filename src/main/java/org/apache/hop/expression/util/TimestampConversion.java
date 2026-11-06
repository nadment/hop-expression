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

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;
import org.jspecify.annotations.NullMarked;

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
   * Returns the number of seconds since the epoch 1970-01-01 00:00:00, the fraction of second is
   * ignored.
   *
   * <p>A date-time in the system time zone is a date without time zone, like a date returned by a
   * date function: the number of seconds is nominal, see {@link
   * DateConversion#toEpochSecond(LocalDateTime)}. A date-time with another time zone gives the
   * number of seconds of its instant, TIMESTAMP '1970-01-01 02:00:00 +02:00' is 0.
   *
   * @param datetime the date-time
   * @return the number of seconds since the epoch
   */
  public static long toEpochSecond(final ZonedDateTime datetime) {
    if (datetime.getZone().equals(ZoneId.systemDefault())) {
      return DateConversion.toEpochSecond(datetime.toLocalDateTime());
    }
    return datetime.toEpochSecond();
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
