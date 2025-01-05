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
import java.time.ZonedDateTime;
import org.apache.hop.expression.ConversionException;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.type.TypeName;

public final class IntegerConverter {

  /** BigInteger equal to Long.MIN_VALUE. */
  private static final BigInteger LONGMIN = BigInteger.valueOf(Long.MIN_VALUE);

  /** BigInteger equal to Long.MAX_VALUE. */
  private static final BigInteger LONGMAX = BigInteger.valueOf(Long.MAX_VALUE);

  private static NumberFormat numberFormat = NumberFormat.of("TM");

  private IntegerConverter() {
    // Utility class
  }

  public static final Long convert(final BigDecimal number) throws ConversionException {
    try {
      BigInteger integer = number.toBigInteger();
      if (integer.compareTo(LONGMIN) < 0 || integer.compareTo(LONGMAX) > 0)
        throw new ConversionException(ErrorCode.ARITHMETIC_OVERFLOW, "CONVERT");
      return number.longValue();
    } catch (Exception e) {
      throw new ConversionException(ErrorCode.ARITHMETIC_OVERFLOW, "CONVERT");
    }
  }

  public static final Long convert(final String str) throws ConversionException {
    try {
      BigDecimal number = numberFormat.parse(str);
      return convert(number);
    } catch (Exception e) {
      throw new ConversionException(ErrorCode.CONVERSION_ERROR_TO_INTEGER, TypeName.STRING, str);
    }
  }

  public static final Long convert(final byte[] bytes) throws ConversionException {
    if (bytes.length > 8)
      throw new ConversionException(ErrorCode.CONVERSION_ERROR_TO_INTEGER, TypeName.BINARY, bytes);
    long result = 0;
    for (int i = 0; i < bytes.length; i++) {
      result <<= Byte.SIZE;
      result |= (bytes[i] & 0xFF);
    }
    return result;
  }

  public static final Long convert(final ZonedDateTime datetime) throws ConversionException {
    return datetime.toEpochSecond();
  }
}
