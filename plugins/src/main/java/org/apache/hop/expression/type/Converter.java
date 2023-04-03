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

package org.apache.hop.expression.type;

import org.apache.commons.lang3.StringUtils;
import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.JsonComparator;
import org.apache.hop.expression.util.NumberFormat;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;
import com.fasterxml.jackson.core.json.JsonReadFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.json.JsonMapper;

public class Converter {

  private static final JsonComparator JSON_COMPARATOR = new JsonComparator();

  /**
   * Private constructor since this is a utility class.
   */
  private Converter() {}

  public static Object cast(Object value, final DataType type) {
    return cast(value, type, null);
  }

  /**
   * Convert a value to the specified type {@link DataType} with a pattern.
   *
   * @param value the value to convert
   * @param type the data type of the returned value
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *        numeric, or null if none
   * @return the converted value
   */
  public static final Object cast(final Object value, final DataType type, String pattern) {

    //Objects.requireNonNull(type);

    if (value == null) {
      return null;
    }

    if (type.getName().getJavaClass().isInstance(value)) {
      return value;
    }

    switch (type.getName()) {
      case BOOLEAN:
        if (value instanceof Number) {
          Number number = (Number) value;
          return number.intValue() != 0;
        }
        if (value instanceof String) {
          return parseBoolean((String) value);
        }
        break;
      case INTEGER:
        if (value instanceof Number) {
          Number number = (Number) value;
          return number.longValue();
        }
        if (value instanceof Boolean) {
          return ((boolean) value) ? 1L : 0L;
        }
        if (value instanceof String) {
          return parseInteger((String) value);
        }
        if (value instanceof byte[]) {
          return toInteger((byte[]) value);
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
          return parseNumber((String) value);
        }
        if (value instanceof byte[]) {
          return toNumber((byte[]) value);
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
          return parseBigNumber((String) value);
        }
        if (value instanceof byte[]) {
          return toBigNumber((byte[]) value);
        }
        break;
      case STRING:
        if (value instanceof Boolean) {
          return toString((boolean) value);
        }
        if (value instanceof Number) {
          if (pattern == null) {
            pattern = "TM";
          }
          return NumberFormat.of(pattern).format(coerceToBigNumber(value));
        }
        if (value instanceof ZonedDateTime) {
          if (pattern == null)
            pattern = "YYYY-MM-DD";
          return DateTimeFormat.of(pattern).format((ZonedDateTime) value);
        }
        return coerceToString(value);
      case DATE:
        if (value instanceof String) {
          try {
            return DateTimeFormat.of(pattern).parse((String) value);
          } catch (ParseException e) {
            throw new IllegalArgumentException(
                ExpressionError.INVALID_DATE.message(e.getMessage()));
          }
        }
        break; 
      case JSON:
        if (value instanceof String) {
          return parseJson((String) value);
        }
        break;
      case BINARY:
        if (value instanceof String) {
          return ((String) value).getBytes(StandardCharsets.UTF_8);
        }
        // if (value instanceof Long) {
        // return toBinary((Long) value);
        // }
        break;
      default:
    }

    throw new IllegalArgumentException(
        ExpressionError.UNSUPPORTED_CONVERSION.message(value, DataName.of(value), type));
  }

  public static final BigDecimal parseBigNumber(final String str) {
    try {
      return new BigDecimal(StringUtils.trim(str));
    } catch (NumberFormatException e) {
      throw new NumberFormatException(ExpressionError.INVALID_BIGNUMBER.message(str));
    }
  }

  public static final Long parseInteger(final String str) {
    try {
      Double number = Double.parseDouble(str);
      return number.longValue();
    } catch (NumberFormatException e) {
      throw new NumberFormatException(ExpressionError.INVALID_INTEGER.message(str));
    }
  }

  public static final Double parseNumber(final String str) {
    try {
      return Double.parseDouble(str);
    } catch (NumberFormatException e) {
      throw new NumberFormatException(ExpressionError.INVALID_NUMBER.message(str));
    }
  }

  public static byte[] toBinary(Long number) {
    byte[] result = new byte[Long.BYTES];
    for (int i = Long.BYTES - 1; i >= 0; i--) {
      result[i] = (byte) (number & 0xFF);
      number >>= Byte.SIZE;
    }
    return result;
  }

  public static Long toInteger(final byte[] bytes) {
    if (bytes.length > 8)
      throw new IllegalArgumentException(ExpressionError.CONVERSION_ERROR
          .message(DataName.BINARY, bytes, DataName.INTEGER));
    long result = 0;
    for (int i = 0; i < bytes.length; i++) {
      result <<= Byte.SIZE;
      result |= (bytes[i] & 0xFF);
    }
    return result;
  }

  public static Double toNumber(final byte[] bytes) {
    if (bytes.length > 8)
      throw new IllegalArgumentException(ExpressionError.CONVERSION_ERROR
          .message(DataName.BINARY, bytes, DataName.NUMBER));
    long result = 0;
    for (int i = 0; i < bytes.length; i++) {
      result <<= Byte.SIZE;
      result |= (bytes[i] & 0xFF);
    }
    return Double.valueOf(result);
  }

  public static BigDecimal toBigNumber(final byte[] bytes) {
    if (bytes.length > 8)
      throw new IllegalArgumentException(ExpressionError.CONVERSION_ERROR
          .message(DataName.BINARY, bytes, DataName.BIGNUMBER));
    long result = 0;
    for (int i = 0; i < bytes.length; i++) {
      result <<= Byte.SIZE;
      result |= (bytes[i] & 0xFF);
    }
    return new BigDecimal(result);
  }

  public static final Boolean parseBoolean(final String str) {
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

    throw new IllegalArgumentException(
        ExpressionError.UNSUPPORTED_COERCION.message(str, DataName.STRING, DataName.BOOLEAN));
  }

  /**
   * Convert String value to Json.
   * 
   * @param str the string to convert
   * @return JsonNode
   */
  public static JsonNode parseJson(final String str) {
    try {
      ObjectMapper objectMapper =
          JsonMapper.builder().enable(JsonReadFeature.ALLOW_UNQUOTED_FIELD_NAMES).build();
      return objectMapper.readTree(str);
    } catch (Exception e) {
      throw new IllegalArgumentException(ExpressionError.INVALID_JSON.message(str));
    }
  }


  public static String toString(final boolean value) {
    return value ? "TRUE" : "FALSE";
  }

  public static String toString(final BigDecimal value) {
    return NumberFormat.of("TM").format((BigDecimal) value);
  }

  /**
   * Convert Json value to String.
   * 
   * @param json the json to convert
   * @return String
   */
  public static String toString(final JsonNode json) {
    try {
      ObjectMapper objectMapper = new ObjectMapper();
      return objectMapper.writeValueAsString(json);
    } catch (Exception e) {
      throw new IllegalArgumentException(ExpressionError.INVALID_JSON.message(json));
    }
  }

  public static ZoneId toZoneId(final String zone) throws Exception {
    try {
      return ZoneId.of(zone);
    } catch (Exception e) {
      throw new ExpressionException(ExpressionError.UNKNOWN_TIMEZONE, zone);
    }
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
      return compareTo(coerceToBinary(left), coerceToBinary(right));
    }

    if (left instanceof JsonNode || right instanceof JsonNode) {

      JsonNode l = coerceToJson(left);
      JsonNode r = coerceToJson(right);

      // Ignores the order of attributes
      return l.equals(JSON_COMPARATOR, r) ? 0 : 1;
    }

    if (left instanceof ZonedDateTime || right instanceof ZonedDateTime) {
      ZonedDateTime dt1 = coerceToDate(left);
      ZonedDateTime dt2 = coerceToDate(right);
      // Two timestamp are equal if they represent the same moment in time:
      // Timestamp '2019-01-01 8:00:00 -8:00' = Timestamp '2019-01-01 11:00:00 -5:00'
      if (dt1.isEqual(dt2)) {
        return 0;
      }
      return dt1.compareTo(dt2);
    }
    if (left instanceof BigDecimal || right instanceof BigDecimal) {
      return coerceToBigNumber(left).compareTo(coerceToBigNumber(right));
    }
    if (left instanceof Double || right instanceof Double) {
      return coerceToNumber(left).compareTo(coerceToNumber(right));
    }
    if (left instanceof Long || right instanceof Long) {
      return coerceToInteger(left).compareTo(coerceToInteger(right));
    }
    if (left instanceof Boolean || right instanceof Boolean) {
      return coerceToBoolean(left).compareTo(coerceToBoolean(right));
    }

    return coerceToString(left).compareTo(coerceToString(right));
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

  /**
   * Coerce value to data type NUMBER
   * 
   * @param value the value to coerce
   * @return Double
   */
  public static Double coerceToNumber(final Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof Double) {
      return (Double) value;
    }
    if (value instanceof Number) {
      return Double.valueOf(((Number) value).doubleValue());
    }
    // if (value instanceof Boolean) {
    // return ((boolean) value) ? 1D : 0D;
    // }
    if (value instanceof String) {
      return parseNumber((String) value);
    }
    throw new IllegalArgumentException(ExpressionError.UNSUPPORTED_COERCION.message(value,
        DataName.of(value), DataName.NUMBER));
  }

  /**
   * Coerce value to data type BIGNUMBER
   * 
   * @param value the value to coerce
   * @return BigDecimal
   */
  public static final BigDecimal coerceToBigNumber(final Object value) {
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
      return parseBigNumber((String) value);
    }
    throw new IllegalArgumentException(ExpressionError.UNSUPPORTED_COERCION.message(value,
        DataName.of(value), DataName.BIGNUMBER));
  }

  /**
   * Coerce value to data type INTEGER
   * 
   * @param value the value to coerce
   * @return Long
   */
  public static final Long coerceToInteger(final Object value) {
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
      return parseInteger((String) value);
    }
    // if (value instanceof Boolean) {
    // return ((boolean) value) ? 1L : 0L;
    // }
    // if (value instanceof byte[]) {
    // return toInteger((byte[]) value);
    // }

    throw new IllegalArgumentException(ExpressionError.UNSUPPORTED_COERCION.message(value,
        DataName.of(value), DataName.INTEGER));
  }

  /**
   * Coerce value to data type STRING
   * 
   * @param value the value to coerce
   * @return String
   */
  public static final String coerceToString(final Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof String) {
      return (String) value;
    }
    if (value instanceof Boolean) {
      return toString((boolean) value);
    }
    if (value instanceof BigDecimal) {
      return toString((BigDecimal) value);
    }
    if (value instanceof byte[]) {
      return new String((byte[]) value, StandardCharsets.UTF_8);
    }

    return String.valueOf(value);
  }

  /**
   * Coerce value to data type TIMESTAMP.
   * 
   * @param value the value to coerce
   * @return ZonedDateTime
   */
  public static final ZonedDateTime coerceToDate(final Object value) {
    if (value == null) {
      return null;
    }
    
    if (value instanceof ZonedDateTime) {
      return (ZonedDateTime) value;
    }

    throw new IllegalArgumentException(ExpressionError.UNSUPPORTED_COERCION.message(value,
        DataName.of(value), DataName.DATE));
  }
  
  
  public static final Date convertToDate(final Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof ZonedDateTime) {
      return Date.from(((ZonedDateTime) value).toInstant());
    }
    throw new IllegalArgumentException(ExpressionError.UNSUPPORTED_COERCION.message(value,
        DataName.of(value), DataName.DATE));
  }

  /**
   * Coerce value to data type BOOLEAN
   * 
   * @param value the value to coerce
   * @return Boolean
   */
  public static final Boolean coerceToBoolean(final Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof Boolean) {
      return (Boolean) value;
    }
    if (value instanceof Number) {
      return ((Number) value).intValue() != 0;
    }
    throw new IllegalArgumentException(ExpressionError.UNSUPPORTED_COERCION.message(value,
        DataName.of(value), DataName.BOOLEAN));
  }

  /**
   * Coerce value to data type BINARY
   * 
   * @param value the value to coerce
   * @return bytes array
   */
  public static final byte[] coerceToBinary(final Object value) {
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
        DataName.of(value), DataName.BINARY));
  }

  /**
   * Coerce value to data type JSON
   * 
   * @param value the value to coerce
   * @return String
   */
  public static final JsonNode coerceToJson(final Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof JsonNode) {
      return (JsonNode) value;
    }
    if (value instanceof String) {
      return Converter.parseJson((String) value);
    }

    throw new IllegalArgumentException(ExpressionError.UNSUPPORTED_COERCION.message(value,
        DataName.of(value), DataName.JSON));
  }

}
