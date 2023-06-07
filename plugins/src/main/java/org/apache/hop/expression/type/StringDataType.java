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
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.NumberFormat;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.ZonedDateTime;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

public final class StringDataType extends DataType {

  /**
   * Default STRING type with max precision.
   */
  public static final StringDataType STRING = new StringDataType(PRECISION_NOT_SPECIFIED);   
  
  public StringDataType(int precision) {
    super(DataName.STRING, precision);
  }
  
  public StringDataType(int precision, int scale) {
    super(DataName.STRING, precision, scale);
  }
  
  public String cast(final Object value) {
    return cast(value, null);
  }
  
  /**
   * Convert a value to the specified type {@link StringDataType} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *        numeric, or null if none
   * @return the converted value
   */
  public String cast(final Object value, String pattern) {

    if (value == null) {
      return null;
    }
    
    if (value instanceof String) {
      String str = (String) value;

      // adjust length
      if ( this.precision<str.length()) {
        str = str.substring(0,this.precision);
      }
      
      return str;
    }
    if (value instanceof Boolean) {
      return convert((boolean) value);
    }
    if (value instanceof Number) {
      if (pattern == null) {
        pattern = "TM";
      }
      return NumberFormat.of(pattern).format(NumberDataType.coerce(value));
    }
    if (value instanceof ZonedDateTime) {
      if (pattern == null)
        pattern = "YYYY-MM-DD";
      return DateTimeFormat.of(pattern).format((ZonedDateTime) value);
    }
    
    if (value instanceof byte[]) {
      return new String((byte[]) value, StandardCharsets.UTF_8);
    }

    return String.valueOf(value);    
  }
  
  /**
   * Coerce value to data type STRING
   * 
   * @param value the value to coerce
   * @return String
   */
  public static final String coerce(final Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof String) {
      return (String) value;
    }
    if (value instanceof Boolean) {
      return convert((boolean) value);
    }
    if (value instanceof BigDecimal) {
      return convert((BigDecimal) value);
    }
    if (value instanceof byte[]) {
      return new String((byte[]) value, StandardCharsets.UTF_8);
    }

    return String.valueOf(value);
  }
  
  public static String convert(final boolean value) {
    return value ? "TRUE" : "FALSE";
  }

  public static String convert(final BigDecimal value) {
    return NumberFormat.of("TM").format(value);
  }

  public static String convert(final byte[] bytes) {
    return new String(bytes, StandardCharsets.UTF_8);
  }
  
  /**
   * Convert Json value to String.
   * 
   * @param json the json to convert
   * @return String
   */
  public static String convert(final JsonNode json) {
    try {
      ObjectMapper objectMapper = new ObjectMapper();
      return objectMapper.writeValueAsString(json);
    } catch (Exception e) {
      throw new IllegalArgumentException(ExpressionError.INVALID_JSON.message(json));
    }
  }
}