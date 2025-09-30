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
import com.fasterxml.jackson.core.json.JsonReadFeature;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.json.JsonMapper;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.type.TypeName;

public final class JsonConversion extends Conversion<JsonNode> {

  // JsonMapper is thread-safe after its configuration
  private static final JsonMapper JSON_READER =
      JsonMapper.builder().enable(JsonReadFeature.ALLOW_UNQUOTED_FIELD_NAMES).build();

  private JsonConversion() {
    // Utility class
  }

  /**
   * Convert String value to JSON.
   *
   * @param str the string to convert
   * @return JsonNode
   * @throws ExpressionException - When the conversion is not possible
   */
  public static JsonNode convert(final String str) throws ExpressionException {
    if (str == null) return null;
    try {
      return JSON_READER.readTree(str);
    } catch (JsonProcessingException e) {
      throw new ExpressionException(
          ErrorCode.CONVERSION_ERROR, TypeName.STRING, TypeName.JSON, str);
    }
  }

  @Override
  public Class<JsonNode> getConvertedType() {
    return JsonNode.class;
  }

  @Override
  public TypeName getTypeName() {
    return TypeName.JSON;
  }
}
