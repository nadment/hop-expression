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
import java.time.ZonedDateTime;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.type.TypeName;

public final class NumberConversion extends Conversion<BigDecimal> {

  private static final NumberFormat FORMAT = NumberFormat.of("TM");

  private NumberConversion() {
    // Utility class
  }

  /**
   * Convert String value to BigDecimal.
   *
   * @param str the string to convert
   * @return BigDecimal
   * @throws ExpressionException - When the conversion is not possible
   */
  public static BigDecimal convert(final String str) throws ExpressionException {
    try {
      return FORMAT.parse(str);
    } catch (ExpressionException e) {
      throw new ExpressionException(
          ErrorCode.CONVERSION_ERROR, TypeName.STRING, TypeName.NUMBER, str);
    }
  }

  public static BigDecimal convert(final byte[] bytes) throws ExpressionException {
    if (bytes.length > 8)
      throw new ExpressionException(
          ErrorCode.CONVERSION_ERROR, TypeName.BINARY, TypeName.NUMBER, bytes);
    long result = 0;
    for (byte aByte : bytes) {
      result <<= Byte.SIZE;
      result |= (aByte & 0xFF);
    }
    return new BigDecimal(result);
  }

  /**
   * Convert String value to BigDecimal.
   *
   * @param datetime the datetime to convert
   * @return BigDecimal
   * @throws ExpressionException - When the conversion is not possible
   */
  public static BigDecimal convert(final ZonedDateTime datetime) throws ExpressionException {

    BigDecimal result = new BigDecimal(datetime.toEpochSecond());
    int nanos = datetime.getNano();
    if (nanos != 0) {
      BigDecimal fraction = BigDecimal.valueOf(nanos).movePointLeft(9);
      result = result.add(fraction).stripTrailingZeros();
    }
    return result;
  }

  @Override
  public Class<BigDecimal> getConvertedType() {
    return BigDecimal.class;
  }

  @Override
  public TypeName getTypeName() {
    return TypeName.NUMBER;
  }
}
