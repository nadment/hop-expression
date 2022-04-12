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
package org.apache.hop.expression;

import org.apache.commons.lang3.StringUtils;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.NumberFormat;
import org.apache.hop.i18n.BaseMessages;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.ZonedDateTime;

/**
 * Enumeration of the data type which can be used to construct a expression.
 *
 * <p>
 * The order of enum declaration is important to be usable with <code>compareTo</code> method.
 *
 * <p>
 * If values need to be converted to match the other operands data type, the value with the lower
 * order is converted to the value with the higher order.
 */
public enum DataType {
  /** A unknown type */
  NONE(Void.class),

  /** Unlimited length text */
  STRING(String.class),

  /** Boolean value (true or false) */
  BOOLEAN(Boolean.class),

  /** Signed long (64-bit) integer */
  INTEGER(Long.class),

  /** Double precision floating point number */
  NUMBER(Double.class),

  /** Unlimited precision number */
  BIGNUMBER(BigDecimal.class),

  /** Date-time value with nanosecond precision */
  DATE(ZonedDateTime.class),

  /** A binary type can be images, sounds, videos, and other types of binary data */
  BINARY(byte[].class);

  private final Class<?> javaClass;

  private DataType(Class<?> javaClass) {
    this.javaClass = javaClass;
  }

  public Class<?> javaClass() {
    return javaClass;
  }

  public boolean isInstance(Object value) {
    return javaClass.isInstance(value);
  }

  public static DataType of(final String name) {
    for (DataType type : DataType.values()) {
      if (type.name().equalsIgnoreCase(name)) {
        return type;
      }
    }
    throw new IllegalArgumentException(Error.INVALID_DATATYPE.message(name));
  }

  public static DataType from(final Object value) {
    if (value == null)
      return NONE;
    if (value instanceof Boolean)
      return BOOLEAN;
    if (value instanceof String)
      return STRING;
    if (value instanceof BigDecimal)
      return BIGNUMBER;
    if (value instanceof Double)
      return NUMBER;
    if (value instanceof Long)
      return INTEGER;
    if (value instanceof ZonedDateTime)
      return DATE;
    if (value instanceof byte[])
      return BINARY;

    throw new IllegalArgumentException(
        BaseMessages.getString(IExpression.class, "Expression.UnknownDataType", value.getClass()));
  }

  /**
   * Check if data type exist.
   * 
   * @param name the name to check
   * @return
   */
  public static boolean exist(final String name) {
    for (DataType type : DataType.values()) {
      if (type.name().equalsIgnoreCase(name)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Check if predicat is true
   * 
   * @param value the value to coerce
   * @return Boolean
   */

  public static final boolean isPredicatTrue(final Object value) throws ExpressionException {
    if (value == null) {
      return false;
    }
    if (value instanceof Boolean) {
      return ((Boolean) value).booleanValue();
    }
    if (value instanceof Number) {
      return ((Number) value).intValue() != 0;
    }
    throw new ExpressionException(Error.UNSUPPORTED_CONVERSION, value, from(value) ,DataType.BOOLEAN);
  }
  
  /**
   * Coerce value to data type BOOLEAN
   * 
   * @param value the value to coerce
   * @return Boolean
   */

  public static final Boolean toBoolean(final Object value) throws ExpressionException {
    if (value == null) {
      return null;
    }
    if (value instanceof Boolean) {
      return (Boolean) value;
    }
    if (value instanceof Number) {
      return ((Number) value).intValue() != 0;
    }
    throw new ExpressionException(Error.UNSUPPORTED_CONVERSION, value, from(value) ,DataType.BOOLEAN);
  }

  /**
   * Coerce value to data type DATE
   * 
   * @param value the value to coerce
   * @return ZonedDateTime
   */
  public static final ZonedDateTime toDate(final Object value) throws ExpressionException {
    if (value == null) {
      return null;
    }
    if (value instanceof ZonedDateTime) {
      return (ZonedDateTime) value;
    }
    throw new ExpressionException(Error.UNSUPPORTED_CONVERSION, value, from(value) ,DataType.DATE);
  }

  /**
   * Coerce value to data type STRING
   * 
   * @param value the value to coerce
   * @return String
   */
  public static final String toString(final Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof String) {
      return (String) value;
    }
    if (value instanceof BigDecimal) {
      return NumberFormat.of("TM").format((BigDecimal) value);
    }
    if (value instanceof byte[]) {
      return new String((byte[]) value, StandardCharsets.UTF_8);
    }

    return String.valueOf(value);
  }

  /**
   * Coerce value to data type BINARY
   * 
   * @param value the value to coerce
   * @return bytes array
   */
  public static final byte[] toBinary(final Object value) throws ExpressionException {
    if (value == null) {
      return null;
    }
    if (value instanceof byte[]) {
      return (byte[]) value;
    }
    if (value instanceof String) {
      return ((String) value).getBytes(StandardCharsets.UTF_8);
    }

    throw new ExpressionException(Error.UNSUPPORTED_CONVERSION, value, from(value) ,DataType.BINARY);
  }

  /**
   * Coerce value to data type INTEGER
   * 
   * @param value the value to coerce
   * @return Long
   */
  public static final Long toInteger(final Object value) throws ExpressionException {
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
      return convertToInteger((String) value);
    }
    if (value instanceof Boolean) {
      return ((boolean) value) ? 1L : 0L;
    }
    if (value instanceof byte[]) {
      return convertToInteger((byte[]) value);
    }

    throw new ExpressionException(Error.UNSUPPORTED_CONVERSION, value, from(value) ,DataType.INTEGER);
  }

  /**
   * Coerce value to data type NUMBER
   * 
   * @param value the value to coerce
   * @return Double
   */
  public static Double toNumber(final Object value) throws ExpressionException {
    if (value == null) {
      return null;
    }
    if (value instanceof Double) {
      return (Double) value;
    }
    if (value instanceof Number) {
      return Double.valueOf(((Number) value).doubleValue());
    }
    if (value instanceof String) {
      return convertToNumber((String) value);
    }
    if (value instanceof byte[]) {
      return convertToNumber((byte[]) value);
    }

    throw new ExpressionException(Error.UNSUPPORTED_CONVERSION, value, from(value) ,DataType.NUMBER);
  }

  /**
   * Coerce value to data type BIGNUMBER
   * 
   * @param value the value to coerce
   * @return BigDecimal
   */
  public static final BigDecimal toBigNumber(final Object value) throws ExpressionException {
    if (value == null) {
      return null;
    }
    if (value instanceof BigDecimal) {
      return (BigDecimal) value;
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
      return convertToBigNumber((String) value);
    }
    throw new ExpressionException(Error.UNSUPPORTED_CONVERSION, value, from(value) ,DataType.BIGNUMBER);
  }

  public static Object convertTo(Object value, final DataType type) throws ExpressionException {
    return convertTo(value, type, null);
  }

  /**
   * Convert a value to the specified type.
   *
   * @param value the value to convert
   * @param type the data type of the returned value
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *        numeric, or null if none
   * @return the converted value
   */
  public static final Object convertTo(final Object value, final DataType type, String pattern)
      throws ExpressionException {
    if (value == null) {
      return null;
    }

    if (type.isInstance(value))
      return value;

    switch (type) {
      case BOOLEAN:
        if (value instanceof Number) {
          return ((Number) value).intValue() != 0;
        }
        if (value instanceof String) {
          return convertToBoolean((String) value);
        }
        break;
      case INTEGER:
        if (value instanceof Number) {
          return ((Number) value).longValue();
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
        break;
      case NUMBER:
        if (value instanceof Boolean) {
          return ((boolean) value) ? 1D : 0D;
        }
        if (value instanceof Number) {
          return Double.valueOf(((Number) value).doubleValue());
        }
        if (value instanceof String) {
          return convertToNumber((String) value);
        }
        if (value instanceof byte[]) {
          return convertToNumber((byte[]) value);
        }
        break;
      case BIGNUMBER:
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
          return convertToBigNumber((String) value);
        }
        break;
      case STRING:
        if (value instanceof Boolean) {
          return ((boolean) value) ? "TRUE" : "FALSE";
        }
        if (value instanceof Number) {
          return NumberFormat.of(pattern).format(toBigNumber(value));
        }
        if (value instanceof ZonedDateTime) {
          if (pattern == null)
            pattern = "YYYY-MM-DD";
          return DateTimeFormat.of(pattern).format((ZonedDateTime) value);
        }
        return toString(value);
      case DATE:
        if (value instanceof String) {
          try {
            return DateTimeFormat.of(pattern).parse((String) value);
          } catch (Exception e) {
            throw new ExpressionException(Error.INVALID_DATE, value);
          }
        }
        break;
      case BINARY:
        if (value instanceof String) {
          return ((String) value).getBytes(StandardCharsets.UTF_8);
        }
        if (value instanceof Long) {
          return convertToBinary((Long) value);
        }
        break;
      case NONE:
        return null;
      default:
    }

    throw new ExpressionException(Error.UNSUPPORTED_CONVERSION, value, from(value) , type);
  }

  private static final Long convertToInteger(final String str) throws ExpressionException {
    try {
      Double number = Double.parseDouble(str);
      return number.longValue();
    } catch (NumberFormatException e) {
      throw new ExpressionException(Error.INVALID_INTEGER, str);
    }
  }

  private static final Double convertToNumber(final String str) throws ExpressionException {
    try {
      return Double.parseDouble(str);
    } catch (NumberFormatException e) {
      throw new ExpressionException(Error.INVALID_NUMBER, str);
    }
  }

  private static final BigDecimal convertToBigNumber(final String str) throws ExpressionException {
    try {     
      return new BigDecimal(StringUtils.trim(str));
    } catch (NumberFormatException e) {
      throw new ExpressionException(Error.INVALID_BIGNUMBER, str);
    }
  }

  private static byte[] convertToBinary(Long number) {
    byte[] result = new byte[Long.BYTES];
    for (int i = Long.BYTES - 1; i >= 0; i--) {
      result[i] = (byte) (number & 0xFF);
      number >>= Byte.SIZE;
    }
    return result;
  }

  private static Long convertToInteger(final byte[] bytes) throws ExpressionException {
    if (bytes.length > 8)
      throw new ExpressionException(Error.CONVERSION_ERROR, BINARY, bytes, INTEGER);
    long result = 0;
    for (int i = 0; i < bytes.length; i++) {
      result <<= Byte.SIZE;
      result |= (bytes[i] & 0xFF);
    }
    return result;
  }

  private static Double convertToNumber(final byte[] bytes) throws ExpressionException {
    if (bytes.length > 8)
      throw new ExpressionException(Error.CONVERSION_ERROR, BINARY, bytes, NUMBER);
    long result = 0;
    for (int i = 0; i < bytes.length; i++) {
      result <<= Byte.SIZE;
      result |= (bytes[i] & 0xFF);
    }
    return Double.valueOf(result);
  }

  private static final boolean convertToBoolean(final String str) throws ExpressionException {
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
    
    throw new ExpressionException(Error.UNSUPPORTED_CONVERSION, str, DataType.STRING ,DataType.BOOLEAN);
  }


  /**
   * Compare this value against another value. If values need to be converted to match the other
   * operands data type, the value with the lower order is converted to the value with the higher
   * order.
   *
   * @param value the other value
   * @return 0 if both values are equal, -1 if this value is smaller, and 1 otherwise
   */
  public static final int compareTo(final Object left, final Object right)
      throws ExpressionException {

    if (left == null && right == null)
      return 0;
    if (left == null)
      return -1;
    if (right == null)
      return 1;

    // The lower order data type is converted
    if (left instanceof byte[] || right instanceof byte[]) {
      return compare(toBinary(left), toBinary(right));
    }
    if (left instanceof ZonedDateTime || right instanceof ZonedDateTime) {
      ZonedDateTime dt1 = toDate(left);
      ZonedDateTime dt2 = toDate(right);
      // Two timestamp are equal if they represent the same moment in time:
      // Timestamp '2019-01-01 8:00:00 -8:00' = Timestamp '2019-01-01 11:00:00 -5:00'
      if (dt1.isEqual(dt2)) {
        return 0;
      }
      return dt1.compareTo(dt2);
    }
    if (left instanceof BigDecimal || right instanceof BigDecimal) {
      return toBigNumber(left).compareTo(toBigNumber(right));
    }
    if (left instanceof Double || right instanceof Double) {
      return toNumber(left).compareTo(toNumber(right));
    }
    if (left instanceof Long || right instanceof Long) {
      return toInteger(left).compareTo(toInteger(right));
    }
    if (left instanceof Boolean || right instanceof Boolean) {
      return toBoolean(left).compareTo(toBoolean(right));
    }

    return toString(left).compareTo(toString(right));
  }

  protected static int compare(final byte[] left, final byte[] right) {
    int length = left.length < right.length ? left.length : right.length;

    int compare = left.length - right.length;
    if (compare == 0) {
      for (int i = 0; i < length; i++) {
        compare = left[i] - right[i];
        if (compare != 0) {
          compare = compare < 0 ? -1 : 1;
          break;
        }
      }
    }

    return compare;
  }
}
