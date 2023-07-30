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
import com.fasterxml.jackson.core.json.JsonReadFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.json.JsonMapper;

public final class JsonType extends Type {

  /**
   * Default JSON type.
   */
  public static final JsonType JSON = new JsonType();

  public JsonType() {
    super(TypeName.JSON);
  }

  @Override
  public JsonNode cast(final Object value) {
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
  public JsonNode cast(final Object value, String pattern) {

    if (value == null) {
      return null;
    }

    if (value instanceof JsonNode) {
      return (JsonNode) value;
    }

    if (value instanceof String) {
      return convert((String) value);
    }

    throw new IllegalArgumentException(
        ExpressionError.UNSUPPORTED_CONVERSION.message(value, TypeName.from(value), this));
  }


  /**
   * Coerce value to data type JSON
   * 
   * @param value the value to coerce
   * @return String
   */
  public static final JsonNode coerce(final Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof JsonNode) {
      return (JsonNode) value;
    }
    if (value instanceof String) {
      return JsonType.convert((String) value);
    }

    throw new IllegalArgumentException(
        ExpressionError.UNSUPPORTED_COERCION.message(value, TypeName.from(value), TypeName.JSON));
  }

  /**
   * Convert String value to Json.
   * 
   * @param str the string to convert
   * @return JsonNode
   */
  public static JsonNode convert(final String str) {
    try {
      ObjectMapper objectMapper =
          JsonMapper.builder().enable(JsonReadFeature.ALLOW_UNQUOTED_FIELD_NAMES).build();
      return objectMapper.readTree(str);
    } catch (Exception e) {
      throw new IllegalArgumentException(ExpressionError.INVALID_JSON.message(str));
    }
  }
}
