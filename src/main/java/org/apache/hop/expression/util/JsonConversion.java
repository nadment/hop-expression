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
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.jayway.jsonpath.JsonPath;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.type.TypeName;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

@NullMarked
public final class JsonConversion {

  // JsonMapper is thread-safe after its configuration.
  // Content after the JSON value is rejected, otherwise it would be silently ignored.
  private static final JsonMapper JSON_READER =
      JsonMapper.builder()
          .enable(JsonReadFeature.ALLOW_UNQUOTED_FIELD_NAMES)
          .enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS)
          .build();

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
  public static @Nullable JsonNode convert(final @Nullable String str) throws ExpressionException {
    if (str == null) return null;
    try {
      JsonNode node = JSON_READER.readTree(str);
      // An empty or blank string has no content and is read as a missing node, not as JSON
      if (!node.isMissingNode()) return node;
    } catch (JsonProcessingException e) {
      // Fall through to the conversion error
    }
    throw new ExpressionException(ErrorCode.CONVERSION_ERROR, TypeName.STRING, TypeName.JSON, str);
  }

  /**
   * Compiles the JSON path of a call ahead of evaluation when it is constant, so it is not compiled
   * again for each row. A compiled {@link JsonPath} is immutable and can be shared between threads.
   *
   * <p>A null or invalid path is not compiled, it is handled at evaluation which raises the error.
   *
   * @param call the call of the JSON function
   * @param index the index of the path operand
   * @return the compiled path, or {@code null} if it must be compiled at evaluation
   */
  public static @Nullable JsonPath compileConstantPath(final Call call, int index) {
    if (call.getOperandCount() <= index) {
      return null;
    }
    IExpression operand = call.getOperand(index);
    if (!operand.isConstant()) {
      return null;
    }

    try {
      String path = operand.getValue(String.class);
      if (path == null) {
        return null;
      }
      return JsonPath.compile(path);
    } catch (RuntimeException e) {
      return null;
    }
  }
}
