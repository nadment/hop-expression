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
import java.nio.charset.StandardCharsets;

public final class BinaryType extends Type {
  /**
   * Default BINARY type with default parameters.
   */
  public static final BinaryType BINARY = new BinaryType();

  public BinaryType() {
    super(TypeName.BINARY, TypeName.BINARY.getMaxPrecision());
  }

  public BinaryType(int precision) {
    super(TypeName.BINARY, precision);
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
   *        numeric, or null if none
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
        ExpressionError.UNSUPPORTED_CONVERSION, value, TypeName.from(value), this);
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
    if (value instanceof String) {
      return ((String) value).getBytes(StandardCharsets.UTF_8);
    }

    throw new ConversionException(
        ExpressionError.UNSUPPORTED_COERCION, value, TypeName.from(value), TypeName.BINARY);
  }

  public static byte[] convert(Long number) throws ConversionException {
    byte[] result = new byte[Long.BYTES];
    for (int i = Long.BYTES - 1; i >= 0; i--) {
      result[i] = (byte) (number & 0xFF);
      number >>= Byte.SIZE;
    }
    return result;
  }

  public static byte[] convert(final String str) throws ConversionException {
    return str.getBytes(StandardCharsets.UTF_8);
  }
}
