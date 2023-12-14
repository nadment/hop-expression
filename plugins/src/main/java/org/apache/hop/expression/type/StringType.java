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

import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.exception.ConversionException;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.NumberFormat;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.ZonedDateTime;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

public final class StringType extends Type {
  /**
   * Default STRING type with maximum precision.
   */
  public static final StringType STRING = new StringType(TypeId.STRING.getMaxPrecision(), true);


  public static StringType of(int precision) {
    return of(precision, true);
  }
  
  public static StringType of(int precision, boolean nullable) {
    if ( precision==PRECISION_NOT_SPECIFIED )
      precision = TypeId.STRING.getMaxPrecision();
    
    if ( precision==TypeId.STRING.getMaxPrecision() && nullable==true)
      return STRING;  
    
    return new StringType(precision, nullable);
  }
  
  public static StringType from(final String value) {
    int precision = value.length();
    // Empty string should return 1
    if ( precision<1 ) precision = 1;
    return StringType.of(precision).withNullability(false);
  }
  
  private StringType(int precision, boolean nullable) {
    super(precision, SCALE_NOT_SPECIFIED, nullable);
  }

  public StringType withNullability(final boolean nullable) {
    return new StringType(precision, nullable);
  }

  @Override
  public TypeId getId() {
    return TypeId.STRING;
  }

  @Override
  public <T> T convert(Object value, Class<T> clazz) throws ConversionException {
    if (value == null) {
      return null;
    }
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }
    if (clazz == Boolean.class) {
      return clazz.cast(BooleanType.convertStringToBoolean((String) value));
    }
    if (clazz == Long.class) {
      return clazz.cast(IntegerType.convertStringToInteger((String) value));
    }
    if (clazz == BigDecimal.class) {
      return clazz.cast(NumberType.convertStringToNumber((String) value));
    }
    if (clazz == byte[].class) {
      return clazz.cast(BinaryType.convertStringToBinary((String) value));
    }
    if (clazz == JsonNode.class) {
      return clazz.cast(JsonType.convertStringToJson((String) value));
    }

    return super.convert(value, clazz);
  }

  @Override
  public String cast(final Object value) throws ConversionException {
    return cast(value, null);
  }

  /**
   * Convert a value to the specified type {@link StringType} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *        numeric, or null if none
   * @return the converted value
   */
  @Override
  public String cast(final Object value, String pattern) throws ConversionException {

    if (value == null) {
      return null;
    }

    if (value instanceof String) {
      String str = (String) value;

      // adjust length
      if (this.precision < str.length()) {
        str = str.substring(0, this.precision);
      }

      return str;
    }
    if (value instanceof Boolean) {
      String result = convertBooleanToString((boolean) value);
      if (checkPrecision(result)) {
        return result;
      }

      throw new ConversionException(ErrorCode.CONVERSION_ERROR, BooleanType.BOOLEAN, value,
          this);
    } else if (value instanceof Number) {
      if (pattern == null) {
        pattern = "TM";
      }
      BigDecimal number = NumberType.coerce(value);
      String result = NumberFormat.of(pattern).format(number);
      if (checkPrecision(result)) {
        return result;
      }
      throw new ConversionException(ErrorCode.CONVERSION_ERROR, NumberType.NUMBER, value,
          this);
    }
    if (value instanceof ZonedDateTime) {
      if (pattern == null)
        pattern = "YYYY-MM-DD";
      String result = DateTimeFormat.of(pattern).format((ZonedDateTime) value);

      if (checkPrecision(result)) {
        return result;
      }
      throw new ConversionException(ErrorCode.CONVERSION_ERROR, DateType.DATE, value, this);
    }

    if (value instanceof byte[]) {
      return new String((byte[]) value, StandardCharsets.UTF_8);
    }

    return String.valueOf(value);
  }


  protected boolean checkPrecision(final String result) {
    if (result == null)
      return true;

    return this.precision < 0 || this.precision >= result.length();
  }

  /**
   * Coerce value to data type STRING
   * 
   * @param value the value to coerce
   * @return String
   */
  public static final String coerce(final Object value) throws ConversionException {
    if (value == null) {
      return null;
    }
    if (value instanceof String) {
      return (String) value;
    }
    if (value instanceof Boolean) {
      return convertBooleanToString((boolean) value);
    }
    if (value instanceof BigDecimal) {
      return convertNumberToString((BigDecimal) value);
    }
    if (value instanceof byte[]) {
      return new String((byte[]) value, StandardCharsets.UTF_8);
    }

    return String.valueOf(value);
  }

  public static String convertBooleanToString(final boolean value) {
    return value ? "TRUE" : "FALSE";
  }

  public static String convertNumberToString(final BigDecimal value) throws ConversionException {
    return NumberFormat.of("TM").format(value);
  }

  public static String convertBinaryToString(final byte[] bytes) {
    return new String(bytes, StandardCharsets.UTF_8);
  }

  /**
   * Convert Json value to String.
   * 
   * @param json the json to convert
   * @return String
   */
  public static String convertJsonToString(final JsonNode json) throws ConversionException {
    try {
      ObjectMapper objectMapper = new ObjectMapper();
      return objectMapper.writeValueAsString(json);
    } catch (Exception e) {
      throw new ConversionException(ErrorCode.INVALID_JSON, json);
    }
  }
}
