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

import org.apache.hop.expression.ConversionException;
import org.apache.hop.expression.ErrorCode;
import com.fasterxml.jackson.core.json.JsonReadFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.json.JsonMapper;

public final class JsonType extends Type {

  
  JsonType(boolean nullable) {
    super(PRECISION_NOT_SPECIFIED, SCALE_NOT_SPECIFIED, nullable);
  }

  @Override
  public JsonType withNullability(boolean nullable) {
    return new JsonType(nullable);
  }

  @Override
  public TypeId getId() {
    return TypeId.JSON;
  }

  @Override
  public TypeComparability getComparability() {
    return TypeComparability.UNORDERED;
  }

  /**
   * Coerce value to data type JSON
   * 
   * @param value the value to coerce
   * @return String
   */
  public static final JsonNode coerce(final Object value) throws ConversionException {
    if (value == null) {
      return null;
    }
    if (value instanceof JsonNode) {
      return (JsonNode) value;
    }
//    if (value instanceof String) {
//      return JsonType.convertStringToJson((String) value);
//    }

    throw new ConversionException(ErrorCode.UNSUPPORTED_COERCION, value, TypeId.fromValue(value),
        TypeId.JSON);
  }


  @Override
  public <T> T convert(final Object value, final Class<T> clazz) throws ConversionException {

    if (value == null) {
      return null;
    }
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }
    if (clazz == String.class) {
      return clazz.cast(StringType.convertJsonToString((JsonNode) value));
    }

    return super.convert(value, clazz);
  }

  @Override
  public JsonNode cast(final Object value) throws ConversionException {
    return cast(value, null);
  }

  /**
   * Convert a value to the specified type {@link JsonType} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *        numeric, or null if none
   * @return the converted value
   */
  @Override
  public JsonNode cast(final Object value, String pattern) throws ConversionException {

    if (value == null) {
      return null;
    }

    if (value instanceof JsonNode) {
      return (JsonNode) value;
    }

    if (value instanceof String) {
      return convertStringToJson((String) value);
    }

    throw new ConversionException(ErrorCode.UNSUPPORTED_CONVERSION, value,
        TypeId.fromValue(value), this);
  }

  /**
   * Convert String value to Json.
   * 
   * @param str the string to convert
   * @return JsonNode
   */
  public static JsonNode convertStringToJson(final String str) throws ConversionException {
    if (str == null)
      return null;
    try {
      ObjectMapper objectMapper =
          JsonMapper.builder().enable(JsonReadFeature.ALLOW_UNQUOTED_FIELD_NAMES).build();
      return objectMapper.readTree(str);
    } catch (Exception e) {
      throw new ConversionException(ErrorCode.INVALID_JSON, str);
    }
  }
}
