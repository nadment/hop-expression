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
import org.apache.hop.expression.ParseNumberException;
import java.math.BigDecimal;

public class CompositeNumberFormat extends NumberFormat {
  private final String pattern;
  private final SimpleNumberFormat[] formats;

  public CompositeNumberFormat(String pattern, SimpleNumberFormat[] formats) {
    this.pattern = pattern;
    this.formats = formats;
  }

  @Override
  public String format(BigDecimal value) {
    return formats[0].format(value);
  }

  @Override
  public BigDecimal parse(String text) throws ParseNumberException {
    for (SimpleNumberFormat format : formats) {
      try {
        return format.parse(text);
      } catch (Exception e) {
        // Ignore try next format
      }
    }

    throw new ParseNumberException(ErrorCode.UNPARSABLE_NUMBER_WITH_FORMAT, text, pattern, 0);
  }

  @Override
  public String toString() {
    return pattern;
  }
}
