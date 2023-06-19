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
import java.math.BigDecimal;

public final class IntegerType extends Type {
  public static final IntegerType INTEGER = new IntegerType(TypeName.INTEGER.getMaxPrecision());

  public IntegerType() {
    super(TypeName.INTEGER, TypeName.INTEGER.getMaxPrecision(), 0);
  }
  
  protected IntegerType(int precision) {
    super(TypeName.INTEGER, precision, 0);
  }
    
  @Override
  public Long cast(final Object value) {
    return cast(value, null);
  }
  
  /**
   * Convert a value to the specified type {@link IntegerType} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *        numeric, or null if none
   * @return the converted value
   */
  @Override
  public Long cast(final Object value, String pattern) {

    if (value == null) {
      return null;
    }
    
    if (value instanceof Long) {
    return (Long) value;
    }
    if (value instanceof BigDecimal) {
      BigDecimal number = (BigDecimal) value;
     return number.longValue();
   }
   if (value instanceof Boolean) {
     return ((boolean) value) ? 1L : 0L;
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
   * Coerce value to data type INTEGER
   * 
   * @param value the value to coerce
   * @return Long
   */
  public static final Long coerce(final Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof Long) {
      return (Long) value;
    }
    if (value instanceof Number) {
      return ((Number) value).longValue();
    }
    if (value instanceof String) {
      return IntegerType.convert((String) value);
    }
    // if (value instanceof Boolean) {
    // return ((boolean) value) ? 1L : 0L;
    // }
    // if (value instanceof byte[]) {
    // return toInteger((byte[]) value);
    // }

    throw new IllegalArgumentException(ExpressionError.UNSUPPORTED_COERCION.message(value,
        TypeName.from(value), TypeName.INTEGER));
  }
  
  public static final Long convert(final String str) {
    try {
      Double number = Double.parseDouble(str);
      return number.longValue();
    } catch (NumberFormatException e) {
      throw new NumberFormatException(ExpressionError.INVALID_INTEGER.message(str));
    }
  }
  
  public static Long convert(final byte[] bytes) {
    if (bytes.length > 8)
      throw new IllegalArgumentException(ExpressionError.CONVERSION_ERROR
          .message(TypeName.BINARY, bytes, TypeName.INTEGER));
    long result = 0;
    for (int i = 0; i < bytes.length; i++) {
      result <<= Byte.SIZE;
      result |= (bytes[i] & 0xFF);
    }
    return result;
  }
}
