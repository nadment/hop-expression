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

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import org.apache.hop.expression.ConversionException;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.util.StringConversion;

public final class BinaryType extends Type {

  BinaryType(int precision, boolean nullable) {
    super(precision, 0, nullable);
    this.signature = generateSignature();
    this.checkPrecisionAndScale();
  }

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

  @Override
  public Type withNullability(boolean nullable) {
    if (nullable == this.isNullable()) {
      return this;
    }
    return new BinaryType(precision, nullable);
  }

  @Override
  public TypeName getName() {
    return TypeName.BINARY;
  }

  @Override
  public TypeComparability getComparability() {
    return TypeComparability.ALL;
  }

  @Override
  public <T> T convert(Object value, Class<T> clazz) throws ConversionException {
    if (value == null) return null;
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }
    if (clazz == String.class) {
      return clazz.cast(StringConversion.convert((byte[]) value));
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
    if (value instanceof byte[] bytes) {
      return bytes;
    }
    if (value instanceof String str) {
      return str.getBytes(StandardCharsets.UTF_8);
    }

    throw new ConversionException(
        ErrorCode.UNSUPPORTED_CONVERSION, value, TypeName.fromValue(value), this);
  }

  @Override
  public boolean compareEqual(Object left, Object right) {
    if (left instanceof byte[] l && right instanceof byte[] r) {
      if (l.length != r.length) return false;
      for (int i = 0; i < l.length; i++) {
        int compare = l[i] - r[i];
        if (compare != 0) {
          return false;
        }
      }
      return true;
    }
    return super.compareEqual(left, right);
  }

  @Override
  public int compare(Object left, Object right) {
    if (left instanceof byte[] l && right instanceof byte[] r) {
      return ByteBuffer.wrap(l).compareTo(ByteBuffer.wrap(r));
    }
    return super.compare(left, right);
  }
}
