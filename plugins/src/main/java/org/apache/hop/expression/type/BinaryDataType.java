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
import java.nio.charset.StandardCharsets;

public final class BinaryDataType extends DataType {
  /**
   * Default BINARY type with default parameters.
   */
  public static final BinaryDataType BINARY = new BinaryDataType();
  
  public BinaryDataType() {
    super(DataName.BINARY, DataName.BINARY.getMaxPrecision());
  } 
    
  public BinaryDataType(int precision) {
    super(DataName.BINARY, precision);
  }
  
  
  public byte[] cast(final Object value) {
    return cast(value, null);
  }
  
  /**
   * Convert a value to the specified type {@link BinaryDataType} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *        numeric, or null if none
   * @return the converted value
   */
  @Override
  public byte[] cast(final Object value, String pattern) {

    if (value == null) {
      return null;
    }
    
    if (value instanceof byte[]) {
      return (byte[]) value;
    }
    if (value instanceof String) {
      return ((String) value).getBytes(StandardCharsets.UTF_8);
    }
    // if (value instanceof Long) {
    // return toBinary((Long) value);
    // }

    throw new IllegalArgumentException(
        ExpressionError.UNSUPPORTED_CONVERSION.message(value, DataName.from(value), this));
  }
  
  /**
   * Coerce value to data type BINARY
   * 
   * @param value the value to coerce
   * @return bytes array
   */
  public static final byte[] coerce(final Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof byte[]) {
      return (byte[]) value;
    }
    if (value instanceof String) {
      return ((String) value).getBytes(StandardCharsets.UTF_8);
    }

    throw new IllegalArgumentException(ExpressionError.UNSUPPORTED_COERCION.message(value,
        DataName.from(value), DataName.BINARY));
  }
  
  public static byte[] convert(Long number) {
    byte[] result = new byte[Long.BYTES];
    for (int i = Long.BYTES - 1; i >= 0; i--) {
      result[i] = (byte) (number & 0xFF);
      number >>= Byte.SIZE;
    }
    return result;
  }

  public static byte[] convert(final String str) {
    return str.getBytes(StandardCharsets.UTF_8);
  }
  
}
