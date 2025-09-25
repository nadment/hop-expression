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
import java.math.BigInteger;
import java.math.RoundingMode;
import java.time.ZonedDateTime;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.type.TypeName;

public final class IntegerConversion extends Conversion<Long> {

  /** BigInteger equal to Long.MIN_VALUE. */
  private static final BigInteger LONGMIN = BigInteger.valueOf(Long.MIN_VALUE);

  /** BigInteger equal to Long.MAX_VALUE. */
  private static final BigInteger LONGMAX = BigInteger.valueOf(Long.MAX_VALUE);

  private static final NumberFormat numberFormat = NumberFormat.of("TM");

  private IntegerConversion() {
    // Utility class
  }

  public static Long convert(final BigDecimal number) throws ExpressionException {
    BigInteger integer = number.setScale(0, RoundingMode.HALF_UP).toBigInteger();
    if (integer.compareTo(LONGMIN) < 0 || integer.compareTo(LONGMAX) > 0)
      throw new ExpressionException(ErrorCode.CONVERSION_OVERFLOW, number);

    return integer.longValue();
  }

  public static Long convert(final String str) throws ExpressionException {
    try {
      BigDecimal number = numberFormat.parse(str);
      return convert(number);
    } catch (Exception e) {
      throw new ExpressionException(
          ErrorCode.CONVERSION_ERROR, TypeName.STRING, TypeName.INTEGER, str);
    }
  }

  public static Long convert(final byte[] bytes) throws ExpressionException {
    if (bytes.length > 8)
      throw new ExpressionException(
          ErrorCode.CONVERSION_ERROR, TypeName.BINARY, TypeName.INTEGER, bytes);
    long result = 0;
    for (byte aByte : bytes) {
      result <<= Byte.SIZE;
      result |= (aByte & 0xFF);
    }
    return result;
  }

  public static Long convert(final ZonedDateTime datetime) throws ExpressionException {
    return datetime.toEpochSecond();
  }

  @Override
  public Class<Long> getConvertedType() {
    return Long.class;
  }

  @Override
  public TypeName getTypeName() {
    return TypeName.INTEGER;
  }
}
