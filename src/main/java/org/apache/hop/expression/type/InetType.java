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

import java.net.InetAddress;
import org.apache.hop.core.util.Utils;
import org.apache.hop.expression.ConversionException;
import org.apache.hop.expression.ErrorCode;

public final class InetType extends Type {

  InetType(boolean nullable) {
    super(PRECISION_NOT_SPECIFIED, SCALE_NOT_SPECIFIED, nullable);
    this.signature = generateSignature();
  }

  @Override
  public InetType withNullability(boolean nullable) {
    return new InetType(nullable);
  }

  @Override
  public TypeName getName() {
    return TypeName.INET;
  }

  @Override
  public TypeComparability getComparability() {
    return TypeComparability.UNORDERED;
  }

  /**
   * Coerce value to data type INET
   *
   * @param value the value to coerce
   * @return String
   */
  public static final InetAddress coerce(final Object value) throws ConversionException {
    if (value == null) {
      return null;
    }
    if (value instanceof InetAddress inet) {
      return inet;
    }

    throw new ConversionException(
        ErrorCode.UNSUPPORTED_COERCION, value, TypeName.fromValue(value), TypeName.JSON);
  }

  @Override
  public <T> T convert(final Object value, final Class<T> clazz) throws ConversionException {

    if (value == null) {
      return null;
    }
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }
    if (clazz == String.class) {
      return clazz.cast(StringType.convert((InetAddress) value));
    }

    return super.convert(value, clazz);
  }

  @Override
  public InetAddress cast(final Object value) throws ConversionException {
    return cast(value, null);
  }

  /**
   * Convert a value to the specified type {@link InetType} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *     numeric, or null if none
   * @return the converted value
   */
  @Override
  public InetAddress cast(final Object value, String pattern) throws ConversionException {

    if (value == null) {
      return null;
    }

    if (value instanceof InetAddress inet) {
      return inet;
    }

    if (value instanceof String str) {
      return convert(str);
    }

    throw new ConversionException(
        ErrorCode.UNSUPPORTED_CONVERSION, value, TypeName.fromValue(value), this);
  }

  /**
   * Convert String value to Inet.
   *
   * @param str the string to convert
   * @return InetAddress
   */
  public static InetAddress convert(final String str) throws ConversionException {
    if (str == null || Utils.isEmpty(str)) return null;

    try {
      return InetAddress.getByName(str);
    } catch (Exception e) {
      throw new ConversionException(ErrorCode.CONVERSION_ERROR_TO_INET, TypeName.STRING, str);
    }
  }
}
