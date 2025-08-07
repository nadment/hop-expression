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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.math.BigDecimal;
import java.net.InetAddress;
import java.nio.charset.StandardCharsets;
import java.time.ZonedDateTime;
import org.apache.hop.expression.ConversionException;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.type.TypeName;

public final class StringConversion extends Conversion<String> {

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
    return DateTimeFormat.of("YYYY-MM-DD").format(value);
  }

  public static String convert(final InetAddress value) {
    return value.getHostAddress();
  }

  /**
   * Convert Json value to String.
   *
   * @param json the json to convert
   * @return String
   */
  public static String convert(final JsonNode json) throws ConversionException {
    try {
      ObjectMapper objectMapper = new ObjectMapper();
      return objectMapper.writeValueAsString(json);
    } catch (Exception e) {
      throw new ConversionException(ErrorCode.CONVERSION_ERROR_TO_STRING, TypeName.JSON, json);
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
