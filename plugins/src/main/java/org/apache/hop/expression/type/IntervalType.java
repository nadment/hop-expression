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

import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.exception.ConversionException;

public final class IntervalType extends Type {

  IntervalType(boolean nullable) {
    super(PRECISION_NOT_SPECIFIED, SCALE_NOT_SPECIFIED, nullable);
  }

  @Override
  public IntervalType withNullability(boolean nullable) {
    return new IntervalType(nullable);
  }

  @Override
  public TypeId getId() {
    return TypeId.INTERVAL;
  }

  @Override
  public TypeComparability getComparability() {
    return TypeComparability.ALL;
  }

  @Override
  public <T> T convert(Object value, Class<T> clazz) throws ConversionException {
    if (value == null) {
      return null;
    }
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }

    return super.convert(value, clazz);
  }

  @Override
  public Object cast(final Object value) throws ConversionException {
    return cast(value, null);
  }

  @Override
  public Object cast(final Object value, final String pattern) throws ConversionException {

    if (value == null) {
      return null;
    }

    if (value instanceof String) {
      return Interval.valueOf((String) value);
    }

    throw new ConversionException(ErrorCode.UNSUPPORTED_CONVERSION, value,
        TypeId.fromValue(value), this);
  }

  /**
   * Convert String value to Interval.
   * 
   * @param str the string to convert
   * @return Interval
   */
  public static Interval convertStringToInterval(final String str) throws ConversionException {
    if (str == null)
      return null;
    Interval value = Interval.valueOf(str);
    if (value == null)
      throw new ConversionException(ErrorCode.INVALID_INTERVAL, str);
    return value;
  }
}
