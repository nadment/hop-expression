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

import org.apache.commons.lang3.StringUtils;
import org.apache.hop.expression.ExpressionError;
import java.math.BigDecimal;
import java.math.RoundingMode;

public final class NumberDataType extends DataType {

  /**
   * Default NUMBER type with max precision.
   */
  public static final NumberDataType NUMBER = new NumberDataType(DataName.NUMBER.getMaxPrecision(), SCALE_NOT_SPECIFIED);
  
  public NumberDataType() {
    super(DataName.NUMBER);
  }
  
  public NumberDataType(int precision) {
    super(DataName.NUMBER, precision);
  }
  
  public NumberDataType(int precision, int scale) {
    super(DataName.NUMBER, precision, scale);
  }
  
  public BigDecimal cast(final Object value) {
    return cast(value, null);
  }
  
  /**
   * Convert a value to the specified type {@link NumberDataType} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *        numeric, or null if none
   * @return the converted value
   */
  @Override
  public BigDecimal cast(final Object value, String pattern) {

    if (value == null) {
      return null;
    }
    
    if (value instanceof BigDecimal) {
      BigDecimal number = (BigDecimal) value;

//      if ( this.precision!=PRECISION_NOT_SPECIFIED ) {
//        number = number.setScale(precision, RoundingMode.DOWN);
//      }

      if ( this.scale!=SCALE_NOT_SPECIFIED ) {
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
        ExpressionError.UNSUPPORTED_CONVERSION.message(value, DataName.from(value), this));
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
    throw new IllegalArgumentException(ExpressionError.UNSUPPORTED_COERCION.message(value,
        DataName.from(value), DataName.NUMBER));
  }
  
  public static final BigDecimal convert(final String str) {
    try {
      return new BigDecimal(StringUtils.trim(str));
    } catch (NumberFormatException e) {
      throw new NumberFormatException(ExpressionError.INVALID_BIGNUMBER.message(str));
    }
  }

  public static BigDecimal convert(final byte[] bytes) {
    if (bytes.length > 8)
      throw new IllegalArgumentException(ExpressionError.CONVERSION_ERROR
          .message(DataName.BINARY, bytes, DataName.NUMBER));
    long result = 0;
    for (int i = 0; i < bytes.length; i++) {
      result <<= Byte.SIZE;
      result |= (bytes[i] & 0xFF);
    }
    return new BigDecimal(result);
  }
}