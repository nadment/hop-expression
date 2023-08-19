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

package org.apache.hop.expression.type;

import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.util.NumberFormat;
import java.math.BigDecimal;
import java.math.RoundingMode;

public final class NumberType extends Type {

  private static final NumberFormat FORMAT = NumberFormat.of("TM");
  
  /**
   * Default NUMBER type with max precision.
   */
  public static final NumberType NUMBER =
      new NumberType(TypeName.NUMBER.getMaxPrecision(), SCALE_NOT_SPECIFIED);

  public NumberType() {
    super(TypeName.NUMBER, TypeName.NUMBER.getMaxPrecision(), 0);
  }

  public NumberType(int precision) {
    super(TypeName.NUMBER, precision, 0);
  }

  public NumberType(int precision, int scale) {
    super(TypeName.NUMBER, precision, scale);
  }

  @Override
  public BigDecimal cast(final Object value) {
    return cast(value, null);
  }

  /**
   * Convert a value to the specified type {@link NumberType} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *        numeric, or null if none
   * @return the converted value
   */
  @Override
  public BigDecimal cast(final Object value, final String pattern) {

    if (value == null) {
      return null;
    }

    if (value instanceof BigDecimal) {
      BigDecimal number = (BigDecimal) value;
      if (this.scale != SCALE_NOT_SPECIFIED) {
        number = number.setScale(scale, RoundingMode.DOWN);
      }
      return number;
    }
    if (value instanceof Boolean) {
      return ((boolean) value) ? BigDecimal.ONE : BigDecimal.ZERO;
    }
    if (value instanceof Long) {
      long v = (long) value;
      if (v == 0L)
        return BigDecimal.ZERO;
      if (v == 1L)
        return BigDecimal.ONE;
      return BigDecimal.valueOf(v);
    }
    if (value instanceof Double) {
      double v = (double) value;
      if (v == 0D)
        return BigDecimal.ZERO;
      if (v == 1D)
        return BigDecimal.ONE;
      return BigDecimal.valueOf(v);
    }
    if (value instanceof String) {
      return convert((String) value);

    }
    if (value instanceof byte[]) {
      return convert((byte[]) value);
    }

    throw new IllegalArgumentException(
        ExpressionError.UNSUPPORTED_CONVERSION.message(value, TypeName.from(value), this));
  }

  /**
   * Coerce value to data type BIGNUMBER
   * 
   * @param value the value to coerce
   * @return BigDecimal
   */
  public static final BigDecimal coerce(final Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof BigDecimal) {
      return (BigDecimal) value;
    }
    // if (value instanceof Boolean) {
    // return ((boolean) value) ? BigDecimal.ONE : BigDecimal.ZERO;
    // }
    if (value instanceof Long) {
      long v = (long) value;
      if (v == 0L)
        return BigDecimal.ZERO;
      if (v == 1L)
        return BigDecimal.ONE;
      return BigDecimal.valueOf(v);
    }
    if (value instanceof Double) {
      double v = (double) value;
      if (v == 0D)
        return BigDecimal.ZERO;
      if (v == 1D)
        return BigDecimal.ONE;
      return BigDecimal.valueOf(v);
    }
    if (value instanceof String) {
      return convert((String) value);
    }
    throw new IllegalArgumentException(
        ExpressionError.UNSUPPORTED_COERCION.message(value, TypeName.from(value), TypeName.NUMBER));
  }

  public static final BigDecimal convert(final String str) {
    return FORMAT.parse(str);
  }

  public static BigDecimal convert(final byte[] bytes) {
    if (bytes.length > 8)
      throw new IllegalArgumentException(
          ExpressionError.CONVERSION_ERROR.message(TypeName.BINARY, bytes, TypeName.NUMBER));
    long result = 0;
    for (int i = 0; i < bytes.length; i++) {
      result <<= Byte.SIZE;
      result |= (bytes[i] & 0xFF);
    }
    return new BigDecimal(result);
  }
}
