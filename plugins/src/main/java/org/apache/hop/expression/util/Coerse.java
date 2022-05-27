/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package org.apache.hop.expression.util;

import org.apache.hop.core.exception.HopValueException;
import org.apache.hop.expression.DataType;
import org.apache.hop.expression.DatePart;
import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.ExpressionException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.ZonedDateTime;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

public class Coerse {
  
  /**
   * Private constructor since this is a utility class.
   */
  private Coerse() {
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

    throw new ExpressionException(ExpressionError.UNSUPPORTED_CONVERSION, value, DataType.name(value) ,DataType.BINARY);
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
    throw new ExpressionException(ExpressionError.UNSUPPORTED_CONVERSION, value, DataType.name(value), DataType.BOOLEAN);
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
      return Converter.toNumber((String) value);
    }
    if (value instanceof byte[]) {
      return Converter.toNumber((byte[]) value);
    }

    throw new ExpressionException(ExpressionError.UNSUPPORTED_CONVERSION, value, DataType.name(value), DataType.NUMBER);
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
      return Converter.toBigNumber((String) value);
    }
    throw new ExpressionException(ExpressionError.UNSUPPORTED_CONVERSION, value, DataType.name(value), DataType.BIGNUMBER);
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
      return Converter.toInteger((String) value);
    }
    if (value instanceof Boolean) {
      return ((boolean) value) ? 1L : 0L;
    }
    if (value instanceof byte[]) {
      return Converter.toInteger((byte[]) value);
    }

    throw new ExpressionException(ExpressionError.UNSUPPORTED_CONVERSION, value, DataType.name(value), DataType.INTEGER);
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
   * Coerce value to data type DATE.
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
    throw new ExpressionException(ExpressionError.UNSUPPORTED_CONVERSION, value, DataType.name(value), DataType.DATE);
  }

  /**
   * Coerce value to data type JSON
   * 
   * @param value the value to coerce
   * @return String
   */
  public static final JsonNode toJson(final Object value) throws ExpressionException {
    if (value == null) {
      return null;
    }
    if (value instanceof JsonNode) {
      return (JsonNode) value;
    }
    if (value instanceof String) {
      return Converter.toJson((String) value);
    }

    throw new ExpressionException(ExpressionError.UNSUPPORTED_CONVERSION, value, DataType.name(value), DataType.JSON);
  }
  
  /**
   * Coerce value to DatePart.
   * 
   * @param value the value to coerce
   * @return DatePart
   */
  public static DatePart toDatePart(Object value) throws ExpressionException {
    if (value instanceof DatePart) {
      return (DatePart) value;
    }
    throw new ExpressionException(ExpressionError.INVALID_DATEPART, value);
  }
  
  /**
   * Check if predicate is true.
   * 
   * @param value the value to coerce
   * @return Boolean
   */

  public static final boolean isTrue(final Object value) throws ExpressionException {
    if (value == null) {
      return false;
    }
    if (value instanceof Boolean) {
      return ((Boolean) value).booleanValue();
    }
    if (value instanceof Number) {
      return ((Number) value).intValue() != 0;
    }
    throw new ExpressionException(ExpressionError.UNSUPPORTED_CONVERSION, value, DataType.name(value), DataType.BOOLEAN);
  }
  
  /**
   * Compare this value against another value. If values need to be converted to match the other
   * operands data type, the value with the lower order is converted to the value with the higher
   * order.
   *
   * @param value the other value
   * @return 0 if both values are equal, -1 if this value is smaller, and 1 otherwise
   */
  public static final int compare(final Object left, final Object right)
      throws ExpressionException {

    if (left == null && right == null)
      return 0;
    if (left == null)
      return -1;
    if (right == null)
      return 1;

    // The lower order data type is converted
    if (left instanceof byte[] || right instanceof byte[]) {
      return compareTo(Coerse.toBinary(left), Coerse.toBinary(right));
    }
    if (left instanceof ZonedDateTime || right instanceof ZonedDateTime) {
      ZonedDateTime dt1 = Coerse.toDate(left);
      ZonedDateTime dt2 = Coerse.toDate(right);
      // Two timestamp are equal if they represent the same moment in time:
      // Timestamp '2019-01-01 8:00:00 -8:00' = Timestamp '2019-01-01 11:00:00 -5:00'
      if (dt1.isEqual(dt2)) {
        return 0;
      }
      return dt1.compareTo(dt2);
    }
    if (left instanceof BigDecimal || right instanceof BigDecimal) {
      return Coerse.toBigNumber(left).compareTo(Coerse.toBigNumber(right));
    }
    if (left instanceof Double || right instanceof Double) {
      return Coerse.toNumber(left).compareTo(Coerse.toNumber(right));
    }
    if (left instanceof Long || right instanceof Long) {
      return Coerse.toInteger(left).compareTo(Coerse.toInteger(right));
    }
    if (left instanceof Boolean || right instanceof Boolean) {
      return Coerse.toBoolean(left).compareTo(Coerse.toBoolean(right));
    }

    return Coerse.toString(left).compareTo(Coerse.toString(right));
  }

  protected static int compareTo(final byte[] left, final byte[] right) {
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
