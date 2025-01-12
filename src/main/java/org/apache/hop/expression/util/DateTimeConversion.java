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
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import org.apache.hop.expression.ConversionException;
import org.apache.hop.expression.type.TypeName;

public final class DateTimeConversion extends Conversion<ZonedDateTime> {

  private DateTimeConversion() {
    // Utility class
  }

  @Override
  public Class<ZonedDateTime> getConvertedType() {
    return ZonedDateTime.class;
  }

  @Override
  public TypeName getTypeName() {
    return TypeName.DATE;
  }

  public static final ZonedDateTime convert(final String value) throws ConversionException {
    return DateTimeFormat.of("FXYYY-MM-DD").parse(value);
  }

  /**
   * Convert the epoch time value to a timestamp with UTC time zone.
   *
   * @param seconds number of seconds that have elapsed since the epoch (00:00:00 UTC on January 1,
   *     1970)
   * @return
   */
  public static final ZonedDateTime convert(final Long seconds) {
    if (seconds == null) {
      return null;
    }
    Instant instant = Instant.ofEpochSecond(seconds);
    return ZonedDateTime.ofInstant(instant, ZoneOffset.UTC);
  }

  public static final ZonedDateTime convert(final BigDecimal number) {
    if (number == null) {
      return null;
    }
    long nanos = number.remainder(BigDecimal.ONE).movePointRight(9).abs().longValue();
    Instant instant = Instant.ofEpochSecond(number.longValue(), nanos);
    return ZonedDateTime.ofInstant(instant, ZoneOffset.UTC);
  }
}
