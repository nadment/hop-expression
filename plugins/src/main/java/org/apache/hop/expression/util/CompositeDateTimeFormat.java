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

import org.apache.hop.expression.ErrorCode;
import java.time.ZonedDateTime;
import java.time.format.DateTimeParseException;

public class CompositeDateTimeFormat extends DateTimeFormat {
  private final String pattern;
  private final ZonedDateTimeFormat[] formats;

  public CompositeDateTimeFormat(String pattern, ZonedDateTimeFormat[] formats) {
    this.pattern = pattern;
    this.formats = formats;
  }

  @Override
  public String format(ZonedDateTime value) {
    return formats[0].format(value);
  }

  @Override
  public ZonedDateTime parse(String text) {
    for (DateTimeFormat format : formats) {
      try {
        return format.parse(text);
      } catch (Exception e) {
        // Ignore, try an other format
      }
    }

    throw new DateTimeParseException(
        ErrorCode.UNPARSABLE_DATE_WITH_FORMAT.message(text, pattern), text, 0);
  }

  @Override
  public void setTwoDigitYearStart(int year) {
    for (ZonedDateTimeFormat format : formats) {
      format.setTwoDigitYearStart(year);
    }
  }
}
