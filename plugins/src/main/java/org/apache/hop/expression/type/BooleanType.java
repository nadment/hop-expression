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

public final class BooleanType extends Type {

  BooleanType(boolean nullable) {
    super(1, 0, nullable);
    this.signature = generateSignature();
  }

  @Override
  public BooleanType withNullability(final boolean nullable) {   
    return (nullable) ? Types.BOOLEAN:Types.BOOLEAN_NOT_NULL;
  }

  @Override
  public TypeId getId() {
    return TypeId.BOOLEAN;
  }
  
  @Override
  public TypeComparability getComparability() {
    return TypeComparability.ALL;
  }
  
  /**
   * Coerce value to data type BOOLEAN
   * 
   * @param value the value to coerce
   * @return Boolean
   */
  public static final Boolean coerce(final Object value) throws ConversionException {
    if (value == null) {
      return null;
    }
    if (value instanceof Boolean) {
      return (Boolean) value;
    }
    if (value instanceof Number) {
      return ((Number) value).intValue() != 0;
    }
    throw new ConversionException(ErrorCode.UNSUPPORTED_COERCION, value, TypeId.fromValue(value),
        TypeId.BOOLEAN);
  }

  @Override
  public <T> T convert(final Object value, final Class<T> clazz) throws ConversionException {

    if (value == null)
      return null;
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }
    if (clazz == String.class) {
      return clazz.cast((boolean) value ? "TRUE" : "FALSE");
    }
    if (clazz == Long.class) {
      return clazz.cast(((boolean) value) ? 1L : 0L);
    }
    if (clazz == BigDecimal.class) {
      return clazz.cast(((boolean) value) ? BigDecimal.ONE : BigDecimal.ZERO);
    }

    return super.convert(value, clazz);
  }

  @Override
  public Boolean cast(final Object value) throws ConversionException {
    return cast(value, null);
  }

  /**
   * Convert a value to the specified type {@link BooleanType} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *        numeric, or null if none
   * @return the converted value
   */
  @Override
  public Boolean cast(final Object value, final String pattern) throws ConversionException {

    if (value == null) {
      return null;
    }
    if (value instanceof Boolean) {
      return (Boolean) value;
    }
    if (value instanceof Long) {
      Long number = (Long) value;
      return number != 0;
    }
    if (value instanceof BigDecimal) {
      BigDecimal number = (BigDecimal) value;
      return number.signum() != 0;
    }
    if (value instanceof String) {
      return convertToBoolean((String) value);
    }

    throw new ConversionException(ErrorCode.UNSUPPORTED_CONVERSION, value,
        TypeId.fromValue(value), this);
  }

  public static final Boolean convertToBoolean(final String str) throws ConversionException {
    switch (str.length()) {
      case 1:
        if (str.equals("1") || str.equalsIgnoreCase("t") || str.equalsIgnoreCase("y")) {
          return true;
        }
        if (str.equals("0") || str.equalsIgnoreCase("f") || str.equalsIgnoreCase("n")) {
          return false;
        }
        break;
      case 2:
        if (str.equalsIgnoreCase("on")) {
          return true;
        }
        if (str.equalsIgnoreCase("no")) {
          return false;
        }
        break;
      case 3:
        if (str.equalsIgnoreCase("yes")) {
          return true;
        }
        if (str.equalsIgnoreCase("off")) {
          return false;
        }
        break;
      case 4:
        if (str.equalsIgnoreCase("true")) {
          return true;
        }
        break;
      case 5:
        if (str.equalsIgnoreCase("false")) {
          return false;
        }
        break;
      default:
        break;
    }

    throw new ConversionException(ErrorCode.INVALID_BOOLEAN, str);
  }
}
