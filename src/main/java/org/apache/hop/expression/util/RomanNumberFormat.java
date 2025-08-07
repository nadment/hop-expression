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
import org.apache.commons.lang3.StringUtils;

final class RomanNumberFormat extends NumberFormat {

  // Original format
  private final String format;
  private final boolean lowerCase;
  // Fill mode suppress padding blanks.
  private boolean fillMode = true;

  public RomanNumberFormat(final String pattern) {

    this.format = pattern;

    int index = 0;
    if (startsWithIgnoreCase(pattern, index, "FM")) {
      this.fillMode = false;
      index += 2;
    }

    this.lowerCase = pattern.charAt(index) == 'r';
  }

  @Override
  public String format(final BigDecimal value) {
    String rn = RomanNumeral.format(value.intValue(), lowerCase);
    if (this.fillMode) {
      rn = StringUtils.leftPad(rn, 15, " ");
    }
    return rn;
  }

  @Override
  public BigDecimal parse(String str) throws FormatParseException {
    long value = RomanNumeral.parse(str);
    return BigDecimal.valueOf(value);
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (this == obj) {
      return true;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    RomanNumberFormat other = (RomanNumberFormat) obj;
    return format.equals(other.format);
  }

  @Override
  public int hashCode() {
    return format.hashCode();
  }

  @Override
  public String toString() {
    return format;
  }
}
