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
import org.apache.hop.expression.exception.ConversionException;
import java.math.BigDecimal;

public final class IntegerType extends Type {
  public static final IntegerType INTEGER = new IntegerType(TypeName.INTEGER.getMaxPrecision());

  public IntegerType() {
    super(TypeName.INTEGER, TypeName.INTEGER.getMaxPrecision(), 0);
  }

  protected IntegerType(int precision) {
    super(TypeName.INTEGER, precision, 0);
  }

  /**
   * Coerce value to data type INTEGER
   * 
   * @param value the value to coerce
   * @return Long
   */
  public  static final Long coerce(final Object value) throws ConversionException {   
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
      return IntegerType.convertStringToInteger((String) value);
    }
    // if (value instanceof Boolean) {
    // return ((boolean) value) ? 1L : 0L;
    // }
    // if (value instanceof byte[]) {
    // return toInteger((byte[]) value);
    // }

    throw new ConversionException(ExpressionError.UNSUPPORTED_COERCION, value,
        TypeName.from(value), TypeName.INTEGER);
  }
  
  @Override
  public <T> T convert(final Object value, final Class<T> clazz) throws ConversionException {

    if (value == null) {
      return null;
    }
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }
    if (clazz == Boolean.class) {
      return clazz.cast(((Long) value) != 0);
    }
    if (clazz == BigDecimal.class) {
      return clazz.cast(BigDecimal.valueOf((Long) value));
    }
    if (clazz == String.class) {
      return clazz.cast(String.valueOf(value));
    }
    
    return super.convert(value, clazz);
  }
  
  @Override
  public Long cast(final Object value) throws ConversionException {
    return cast(value, null);
  }

  @Override
  public Long cast(final Object value, String pattern) throws ConversionException {

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
      return convertStringToInteger((String) value);
    }
    if (value instanceof byte[]) {
      return convertBinaryToInteger((byte[]) value);
    }

    throw new ConversionException(
        ExpressionError.UNSUPPORTED_CONVERSION, value, TypeName.from(value), this);
  }

  public static final Long convertStringToInteger(final String str) throws ConversionException {
    try {
      Double number = Double.parseDouble(str);
      return number.longValue();
    } catch (Exception e) {
      throw new ConversionException(ExpressionError.INVALID_INTEGER, str);
    }
  }

  public static final Long convertBinaryToInteger(final byte[] bytes) throws ConversionException {
    if (bytes.length > 8)
      throw new ConversionException(
          ExpressionError.CONVERSION_ERROR, TypeName.BINARY, bytes, TypeName.INTEGER);
    long result = 0;
    for (int i = 0; i < bytes.length; i++) {
      result <<= Byte.SIZE;
      result |= (bytes[i] & 0xFF);
    }
    return result;
  }
}
