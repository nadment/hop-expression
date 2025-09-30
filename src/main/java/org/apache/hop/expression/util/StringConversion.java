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

package org.apache.hop.expression.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.fasterxml.jackson.databind.util.StdDateFormat;
import java.math.BigDecimal;
import java.net.InetAddress;
import java.nio.charset.StandardCharsets;
import java.time.ZonedDateTime;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.type.TypeName;

public final class StringConversion extends Conversion<String> {

  // JsonMapper is thread-safe after its configuration
  private static final JsonMapper JSON_WRITER = JsonMapper.builder().disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS).build();
  {
      // StdDateFormat is ISO8601
      JSON_WRITER.setDateFormat(new StdDateFormat().withColonInTimeZone(true));
  }

  private StringConversion() {
    // Utility class
  }

  public static String convert(final boolean value) {
    return value ? "TRUE" : "FALSE";
  }

  public static String convert(final Long value) {
    return value.toString();
  }

  public static String convert(final BigDecimal value) {
    return NumberFormat.of("TM").format(value);
  }

  public static String convert(final byte[] bytes) {
    return new String(bytes, StandardCharsets.UTF_8);
  }

  public static String convert(final ZonedDateTime value) {
    String pattern = "YYYY-MM-DD";
    if (value.getNano() > 0) {
      pattern = "YYYY-MM-DD HH24:MI:SS.FFFFFFFFF";
    }
    if (value.getHour() > 0 || value.getMinute() > 0 || value.getSecond() > 0) {
      pattern = "YYYY-MM-DD HH24:MI:SS";
    }

    return DateTimeFormat.of(pattern).format(value);
  }

  public static String convert(final InetAddress value) {
    return value.getHostAddress();
  }

  /**
   * Convert JSON value to String.
   *
   * @param json the JSON to convert
   * @return String
   * @throws ExpressionException - When the conversion is not possible
   */
  public static String convert(final JsonNode json) throws ExpressionException {
    try {
      return JSON_WRITER.writeValueAsString(json);
    } catch (JsonProcessingException e) {
      throw new ExpressionException(
          ErrorCode.CONVERSION_ERROR, TypeName.JSON, TypeName.STRING, json);
    }
  }

  @Override
  public Class<String> getConvertedType() {
    return String.class;
  }

  @Override
  public TypeName getTypeName() {
    return TypeName.STRING;
  }
}
