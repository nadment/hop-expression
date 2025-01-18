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

import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import org.apache.hop.expression.ErrorCode;

public class CompositeDateTimeFormat extends DateTimeFormat {
  private final String pattern;
  private final List<DateTimeFormat> formats;

  public CompositeDateTimeFormat(final String pattern) {
    this.pattern = pattern;
    this.formats = new ArrayList<>();

    for (String p : pattern.split("\\|")) {
      // Text minimal format
      if (p.equalsIgnoreCase("AUTO")) {
        formats.add(new AutoDateTimeFormat());
      } else {
        formats.add(new PatternDateTimeFormat(p));
      }
    }
  }

  @Override
  public String format(final ZonedDateTime value) {
    return formats.get(0).format(value);
  }

  @Override
  public ZonedDateTime parse(final String text) {
    for (DateTimeFormat format : formats) {
      try {
        return format.parse(text);
      } catch (Exception e) {
        // Ignore, try another format
      }
    }

    throw new FormatParseException(ErrorCode.UNPARSABLE_DATE_WITH_FORMAT, text, pattern);
  }

  @Override
  public void setTwoDigitYearStart(int year) {
    for (DateTimeFormat format : formats) {
      format.setTwoDigitYearStart(year);
    }
  }

  @Override
  public String toString() {
    return pattern;
  }
}
