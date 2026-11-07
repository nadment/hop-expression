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

import java.time.DateTimeException;
import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.time.temporal.Temporal;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Rounds the fractional seconds to the precision of a DATE, TIMESTAMP or INTERVAL type, the number
 * of digits from 0 to 9. Like a number, halfway values are rounded away from zero, for example
 * {@code 10:00:00.5} to the precision 0 gives {@code 10:00:01}.
 */
@NullMarked
public final class FractionalSeconds {

  /** The maximum precision, the nanoseconds. */
  public static final int MAX_PRECISION = 9;

  /** The number of nanoseconds of the last digit kept, by precision. */
  private static final long[] UNITS = {
    1_000_000_000L, 100_000_000L, 10_000_000L, 1_000_000L, 100_000L, 10_000L, 1_000L, 100L, 10L, 1L
  };

  private FractionalSeconds() {
    // Utility class
  }

  /**
   * Rounds a signed number of nanoseconds to a precision, halfway values away from zero.
   *
   * <p>For example, 123_456_789 nanoseconds to the precision 3 gives 123_000_000, and 999_600_000
   * to the precision 3 gives 1_000_000_000, a whole second.
   *
   * @param nanos the signed number of nanoseconds
   * @param precision the number of digits of the fractional seconds, from 0 to 9
   * @return the rounded number of nanoseconds, which can be a whole second
   */
  public static long round(long nanos, int precision) {
    if (precision >= MAX_PRECISION) {
      return nanos;
    }
    long unit = UNITS[precision];
    long rounded = (Math.abs(nanos) + unit / 2) / unit * unit;
    return (nanos < 0) ? -rounded : rounded;
  }

  /**
   * Rounds the fractional seconds of a date-time to a precision, halfway values away from zero. A
   * rounded value can be the next second, minute or day, for example {@code 23:59:59.5} to the
   * precision 0 gives the next day at midnight.
   *
   * @param value the date-time, with or without time zone, or {@code null}
   * @param precision the number of digits of the fractional seconds, from 0 to 9
   * @return the rounded date-time, or {@code null} if the value is {@code null}
   * @throws ExpressionException if the rounded date-time is out of the supported range
   */
  @SuppressWarnings("unchecked")
  public static <T extends Temporal> @Nullable T round(@Nullable T value, int precision) {
    if (value == null || precision >= MAX_PRECISION) {
      return value;
    }
    int nanos = value.get(ChronoField.NANO_OF_SECOND);
    long rounded = round(nanos, precision);
    if (rounded == nanos) {
      return value;
    }
    try {
      return (T) value.with(ChronoField.NANO_OF_SECOND, 0).plus(rounded, ChronoUnit.NANOS);
    } catch (DateTimeException e) {
      throw new ExpressionException(ErrorCode.TEMPORAL_OVERFLOW, "CAST");
    }
  }
}
