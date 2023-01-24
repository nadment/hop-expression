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
import org.apache.hop.expression.util.NumberFormat;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.ZonedDateTime;
import java.util.Objects;
import com.fasterxml.jackson.core.json.JsonReadFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.json.JsonMapper;

public class Converter {

  /**
   * Private constructor since this is a utility class.
   */
  private Converter() {}

  public static Object to(Object value, final DataTypeName type) throws ExpressionException {
    return to(value, type, null);
  }

  /**
   * Convert a value to the specified type {@link DataTypeName} with a pattern.
   *
   * @param value the value to convert
   * @param type the data type of the returned value
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *        numeric, or null if none
   * @return the converted value
   */
  public static final Object to(final Object value, final DataTypeName type, String pattern)
      throws ExpressionException {

    Objects.requireNonNull(type);

    // Special date type, can't be converted to
    // if ( type.getFamily()==DataTypeFamily.UNKNOWN ) {
    // throw new ExpressionException(ExpressionError.ILLEGAL_ARGUMENT, type);
    // }

    if (value == null) {
      return null;
    }

    if (type.getJavaClass().isInstance(value)) {
      return value;
    }

    switch (type) {
      case BOOLEAN:
        if (value instanceof Number) {
          Number number = (Number) value;
          return number.intValue() != 0;
        }
        if (value instanceof String) {
          return toBoolean((String) value);
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
          return toInteger((String) value);
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
          return toNumber((String) value);
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
          return toBigNumber((String) value);
        }
        break;
      case STRING:
        if (value instanceof Boolean) {
          return ((boolean) value) ? "TRUE" : "FALSE";
        }
        if (value instanceof Number) {
          if (pattern == null) {
            pattern = "TM";
          }
          return NumberFormat.of(pattern).format(Coerce.toBigNumber(value));
        }
        if (value instanceof ZonedDateTime) {
          if (pattern == null)
            pattern = "YYYY-MM-DD";
          return DateTimeFormat.of(pattern).format((ZonedDateTime) value);
        }
        return Coerce.toString(value);
      case DATE:
        if (value instanceof String) {
          try {
            return DateTimeFormat.of(pattern).parse((String) value);
          } catch (Exception e) {
            throw new ExpressionException(ExpressionError.INVALID_DATE, value);
          }
        }
        break;
      case JSON:
        if (value instanceof String) {
          return toJson((String) value);
        }
        break;
      case BINARY:
        if (value instanceof String) {
          return ((String) value).getBytes(StandardCharsets.UTF_8);
        }
        if (value instanceof Long) {
          return toBinary((Long) value);
        }
        break;
      default:
    }

    throw new ExpressionException(ExpressionError.UNSUPPORTED_CONVERSION, value,
        DataTypeName.from(value), type);
  }

  public static final BigDecimal toBigNumber(final String str) throws ExpressionException {
    try {
      return new BigDecimal(StringUtils.trim(str));
    } catch (NumberFormatException e) {
      throw new ExpressionException(ExpressionError.INVALID_BIGNUMBER, str);
    }
  }

  public static final Long toInteger(final String str) throws ExpressionException {
    try {
      Double number = Double.parseDouble(str);
      return number.longValue();
    } catch (NumberFormatException e) {
      throw new ExpressionException(ExpressionError.INVALID_INTEGER, str);
    }
  }

  public static final Double toNumber(final String str) throws ExpressionException {
    try {
      return Double.parseDouble(str);
    } catch (NumberFormatException e) {
      throw new ExpressionException(ExpressionError.INVALID_NUMBER, str);
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

  public static Long toInteger(final byte[] bytes) throws ExpressionException {
    if (bytes.length > 8)
      throw new ExpressionException(ExpressionError.CONVERSION_ERROR, DataTypeName.BINARY, bytes,
          DataTypeName.INTEGER);
    long result = 0;
    for (int i = 0; i < bytes.length; i++) {
      result <<= Byte.SIZE;
      result |= (bytes[i] & 0xFF);
    }
    return result;
  }

  public static Double toNumber(final byte[] bytes) throws ExpressionException {
    if (bytes.length > 8)
      throw new ExpressionException(ExpressionError.CONVERSION_ERROR, DataTypeName.BINARY, bytes,
          DataTypeName.NUMBER);
    long result = 0;
    for (int i = 0; i < bytes.length; i++) {
      result <<= Byte.SIZE;
      result |= (bytes[i] & 0xFF);
    }
    return Double.valueOf(result);
  }

  public static final boolean toBoolean(final String str) throws ExpressionException {
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

    throw new ExpressionException(ExpressionError.UNSUPPORTED_CONVERSION, str, DataTypeName.STRING,
        DataTypeName.BOOLEAN);
  }


  /**
   * Convert String value to Json.
   * 
   * @param str the string to convert
   * @return JsonNode
   */
  public static JsonNode toJson(final String str) throws ExpressionException {
    try {
      ObjectMapper objectMapper =
          JsonMapper.builder().enable(JsonReadFeature.ALLOW_UNQUOTED_FIELD_NAMES).build();
      return objectMapper.readTree(str);
    } catch (Exception e) {
      throw new ExpressionException(ExpressionError.INVALID_JSON, str);
    }
  }

  /**
   * Convert Json value to String.
   * 
   * @param json the json to convert
   * @return String
   */
  public static String toString(final JsonNode json) throws ExpressionException {
    try {
      ObjectMapper objectMapper = new ObjectMapper();
      return objectMapper.writeValueAsString(json);
    } catch (Exception e) {
      throw new ExpressionException(ExpressionError.INVALID_JSON, json);
    }
  }

}
