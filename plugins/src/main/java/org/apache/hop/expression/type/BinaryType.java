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

import java.nio.charset.StandardCharsets;
import org.apache.hop.expression.ConversionException;
import org.apache.hop.expression.ErrorCode;

public final class BinaryType extends Type {

  public static BinaryType from(final byte[] value) {
    int precision = value.length;
    // Empty binary array should return 1
    if (precision < 1) precision = 1;
    return new BinaryType(precision, false);
  }

  public static BinaryType of(int precision) {
    return of(precision, true);
  }

  public static BinaryType of(int precision, boolean nullable) {
    if (precision == PRECISION_NOT_SPECIFIED && nullable) return Types.BINARY;
    return new BinaryType(precision, nullable);
  }

  BinaryType(int precision, boolean nullable) {
    super(precision, 0, nullable);
    this.signature = generateSignature();
    this.checkPrecisionAndScale();
  }

  @Override
  public Type withNullability(boolean nullable) {
    return new BinaryType(precision, nullable);
  }

  @Override
  public TypeId getId() {
    return TypeId.BINARY;
  }

  @Override
  public TypeComparability getComparability() {
    return TypeComparability.ALL;
  }

  /**
   * Coerce value to data type BINARY
   *
   * @param value the value to coerce
   * @return bytes array
   */
  public static final byte[] coerce(final Object value) throws ConversionException {
    if (value == null) {
      return null;
    }
    if (value instanceof byte[]) {
      return (byte[]) value;
    }

    throw new ConversionException(
        ErrorCode.UNSUPPORTED_COERCION, value, TypeId.fromValue(value), TypeId.BINARY);
  }

  @Override
  public <T> T convert(Object value, Class<T> clazz) throws ConversionException {
    if (value == null) return null;
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }
    if (clazz == String.class) {
      return clazz.cast(StringType.convertToString((byte[]) value));
    }

    return super.convert(value, clazz);
  }

  @Override
  public byte[] cast(final Object value) throws ConversionException {
    return cast(value, null);
  }

  /**
   * Convert a value to the specified type {@link BinaryType} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *     numeric, or null if none
   * @return the converted value
   */
  @Override
  public byte[] cast(final Object value, String pattern) throws ConversionException {

    if (value == null) {
      return null;
    }

    if (value instanceof byte[]) {
      return (byte[]) value;
    }
    if (value instanceof String) {
      return ((String) value).getBytes(StandardCharsets.UTF_8);
    }

    throw new ConversionException(
        ErrorCode.UNSUPPORTED_CONVERSION, value, TypeId.fromValue(value), this);
  }

  public static byte[] convertToBinary(Long number) throws ConversionException {
    byte[] result = new byte[Long.BYTES];
    for (int i = Long.BYTES - 1; i >= 0; i--) {
      result[i] = (byte) (number & 0xFF);
      number >>= Byte.SIZE;
    }
    return result;
  }

  public static byte[] convertToBinary(final String str) throws ConversionException {
    return str.getBytes(StandardCharsets.UTF_8);
  }
}
