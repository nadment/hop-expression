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
import org.apache.hop.expression.exception.ParseNumberException;
import org.apache.hop.expression.util.NumberFormat;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

public final class NumberType extends Type {

  private static final NumberFormat FORMAT = NumberFormat.of("TM");

  protected final int precision;
  protected final int scale;

  /**
   * Default NUMBER type with max precision.
   */
  public static final NumberType NUMBER =
      new NumberType(TypeId.NUMBER.getMaxPrecision(), SCALE_NOT_SPECIFIED, true);

  public static NumberType from(final BigDecimal number) {
    return new NumberType(number.precision(), number.scale(), true);
  }

  public NumberType() {
    this(TypeId.NUMBER.getMaxPrecision(), 0, true);
  }

  public NumberType(int precision) {
    this(precision, 0, true);
  }

  public NumberType(int precision, int scale) {
    this(precision, scale, true);
  }

  public NumberType(int precision, int scale, boolean nullable) {
    super(precision, scale, nullable);
    this.precision = precision;
    this.scale = scale;
  }

  @Override
  public NumberType withNullability(boolean nullable) {
    return new NumberType(precision, scale, nullable);
  }

  @Override
  public int getPrecision() {
    return precision;
  }

  @Override
  public int getScale() {
    return scale;
  }

  @Override
  public TypeId getId() {
    return TypeId.NUMBER;
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
      return clazz.cast(((BigDecimal) value).unscaledValue() != BigInteger.ZERO);
    }
    if (clazz == Long.class) {
      return clazz.cast(((BigDecimal) value).longValue());
    }
    if (clazz == String.class) {
      return clazz.cast(NumberFormat.of("TM").format((BigDecimal) value));
    }

    return super.convert(value, clazz);
  }

  @Override
  public BigDecimal cast(final Object value) throws ConversionException {
    return cast(value, null);
  }

  /**
   * Convert a value to the specified type {@link NumberType} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *        numeric, or null if none
   * @return the converted value
   */
  @Override
  public BigDecimal cast(final Object value, final String pattern) throws ConversionException {

    if (value == null) {
      return null;
    }

    if (value instanceof BigDecimal) {
      BigDecimal number = (BigDecimal) value;
      if (this.scale != SCALE_NOT_SPECIFIED) {
        number = number.setScale(scale, RoundingMode.DOWN);
      }
      return number;
    }
    if (value instanceof Boolean) {
      return ((boolean) value) ? BigDecimal.ONE : BigDecimal.ZERO;
    }
    if (value instanceof Long) {
      long v = (long) value;
      if (v == 0L)
        return BigDecimal.ZERO;
      if (v == 1L)
        return BigDecimal.ONE;
      return BigDecimal.valueOf(v);
    }
    if (value instanceof Double) {
      double v = (double) value;
      if (v == 0D)
        return BigDecimal.ZERO;
      if (v == 1D)
        return BigDecimal.ONE;
      return BigDecimal.valueOf(v);
    }
    if (value instanceof String) {
      return convertStringToNumber((String) value);

    }
    if (value instanceof byte[]) {
      return convertBinaryToNumber((byte[]) value);
    }

    throw new ConversionException(ExpressionError.UNSUPPORTED_CONVERSION, value,
        Type.valueOf(value), this);
  }

  /**
   * Coerce value to data type BIGNUMBER
   * 
   * @param value the value to coerce
   * @return BigDecimal
   */
  public static final BigDecimal coerce(final Object value) throws ConversionException {
    if (value == null) {
      return null;
    }
    if (value instanceof BigDecimal) {
      return (BigDecimal) value;
    }
    // if (value instanceof Boolean) {
    // return ((boolean) value) ? BigDecimal.ONE : BigDecimal.ZERO;
    // }
    if (value instanceof Long) {
      long v = (long) value;
      if (v == 0L)
        return BigDecimal.ZERO;
      if (v == 1L)
        return BigDecimal.ONE;
      return BigDecimal.valueOf(v);
    }
    if (value instanceof Double) {
      double v = (double) value;
      if (v == 0D)
        return BigDecimal.ZERO;
      if (v == 1D)
        return BigDecimal.ONE;
      return BigDecimal.valueOf(v);
    }
    if (value instanceof String) {
      return convertStringToNumber((String) value);
    }
    throw new ConversionException(ExpressionError.UNSUPPORTED_COERCION, value, Type.valueOf(value),
        NumberType.NUMBER);
  }

  public static final BigDecimal convertStringToNumber(final String str)
      throws ConversionException {
    try {
      return FORMAT.parse(str);
    } catch (ParseNumberException e) {
      throw new ConversionException(ExpressionError.UNSUPPORTED_COERCION, str, StringType.STRING,
          NumberType.NUMBER);
    }
  }

  public static BigDecimal convertBinaryToNumber(final byte[] bytes) throws ConversionException {
    if (bytes.length > 8)
      throw new ConversionException(ExpressionError.CONVERSION_ERROR, bytes, BinaryType.BINARY,
          NumberType.NUMBER);
    long result = 0;
    for (int i = 0; i < bytes.length; i++) {
      result <<= Byte.SIZE;
      result |= (bytes[i] & 0xFF);
    }
    return new BigDecimal(result);
  }
}
