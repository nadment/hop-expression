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
import java.math.RoundingMode;
import org.apache.commons.lang3.StringUtils;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;

final class RomanNumberFormat extends NumberFormat {

  private static final BigDecimal MAX_VALUE = BigDecimal.valueOf(3999);

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
    // A value with a fractional part is rounded, the range is checked before the conversion to an
    // int to not keep only the low-order bits of a large value
    BigDecimal rounded = value.setScale(0, RoundingMode.HALF_UP);
    if (rounded.signum() <= 0 || rounded.compareTo(MAX_VALUE) > 0) {
      throw new ExpressionException(ErrorCode.ARGUMENT_OUT_OF_RANGE, 1, value.stripTrailingZeros());
    }
    String rn = RomanNumeral.format(rounded.intValue(), lowerCase);
    if (this.fillMode) {
      rn = StringUtils.leftPad(rn, 15, " ");
    }
    return rn;
  }

  @Override
  public BigDecimal parse(String str) throws ExpressionException {
    try {
      // The formatted value is padded with blanks
      long value = RomanNumeral.parse(str.strip());
      return BigDecimal.valueOf(value);
    } catch (IllegalArgumentException e) {
      throw createUnparsableNumber(format, str, 0);
    }
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
