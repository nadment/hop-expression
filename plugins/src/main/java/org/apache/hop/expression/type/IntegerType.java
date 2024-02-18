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

import org.apache.hop.expression.ConversionException;
import org.apache.hop.expression.ErrorCode;
import java.math.BigDecimal;
import java.time.ZonedDateTime;

public final class IntegerType extends Type {

  public static IntegerType from(final Long value) {
    return of(numberOfDigit(value), false);
  }

  public static IntegerType from(final Integer value) {
    return of(numberOfDigit(value), false);
  }

  public static IntegerType of(int precision) {
    return of(precision, true);
  }
  
  public static IntegerType of(int precision, boolean nullable) {
    if ( precision==PRECISION_NOT_SPECIFIED )
      precision = TypeId.INTEGER.getMaxPrecision();
    
    if ( precision==TypeId.INTEGER.getMaxPrecision() && nullable)
      return Types.INTEGER;

    return new IntegerType(precision, nullable);
  }

  IntegerType(int precision, boolean nullable) {
    super(precision, 0, nullable);
    this.signature = generateSignature();
    this.checkPrecisionAndScale();
  }

  @Override
  public Type withNullability(boolean nullable) {
    return new IntegerType(precision, nullable);
  }

  @Override
  public TypeId getId() {
    return TypeId.INTEGER;
  }
  
  @Override
  public TypeComparability getComparability() {
    return TypeComparability.ALL;
  }

  /**
   * Coerce value to data type INTEGER
   * 
   * @param value the value to coerce
   * @return Long
   */
  public static final Long coerce(final Object value) throws ConversionException {
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
      return IntegerType.convertToInteger((String) value);
    }

    throw new ConversionException(ErrorCode.UNSUPPORTED_COERCION, value, TypeId.fromValue(value),
        TypeId.INTEGER);
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
      return convertToInteger((String) value);
    }
    if (value instanceof byte[]) {
      return convertToInteger((byte[]) value);
    }
    if (value instanceof ZonedDateTime) {
      return convertToInteger((ZonedDateTime) value);
    }
    
    throw new ConversionException(ErrorCode.UNSUPPORTED_CONVERSION, value,
        TypeId.fromValue(value), this);
  }

  public static final Long convertToInteger(final String str) throws ConversionException {
    try {
      Double number = Double.parseDouble(str);
      return number.longValue();
    } catch (Exception e) {
      throw new ConversionException(ErrorCode.INVALID_INTEGER, str);
    }
  }

  public static final Long convertToInteger(final byte[] bytes) throws ConversionException {
    if (bytes.length > 8)
      throw new ConversionException(ErrorCode.CONVERSION_ERROR, TypeId.BINARY, bytes,
          TypeId.INTEGER);
    long result = 0;
    for (int i = 0; i < bytes.length; i++) {
      result <<= Byte.SIZE;
      result |= (bytes[i] & 0xFF);
    }
    return result;
  }

  public static final Long convertToInteger(final ZonedDateTime datetime) throws ConversionException {
    return datetime.toEpochSecond();
  }
  
  protected static int numberOfDigit(int number) {
    if (number < 100000) {
      if (number < 100) {
        if (number < 10) {
          return 1;
        } else {
          return 2;
        }
      } else {
        if (number < 1000) {
          return 3;
        } else {
          if (number < 10000) {
            return 4;
          } else {
            return 5;
          }
        }
      }
    } else {
      if (number < 10000000) {
        if (number < 1000000) {
          return 6;
        } else {
          return 7;
        }
      } else {
        if (number < 100000000) {
          return 8;
        } else {
          if (number < 1000000000) {
            return 9;
          } else {
            return 10;
          }
        }
      }
    }
  }

  protected static int numberOfDigit(long number) {
    int count = 0;
    while (number != 0) {
      number = number / 10;
      ++count;
    }
    return count;
  }
}
