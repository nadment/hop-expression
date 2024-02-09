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
package org.apache.hop.expression.operator;

import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.TypeId;
import java.math.BigDecimal;
import com.fasterxml.jackson.core.json.JsonReadFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.PathNotFoundException;
import com.jayway.jsonpath.spi.json.JacksonJsonNodeJsonProvider;
import com.jayway.jsonpath.spi.mapper.JacksonMappingProvider;

/**
 * Extracts a scalar value from a JSON string.
 */
@FunctionPlugin
public class JsonValueFunction extends Function {

  public static final Configuration JSONPATH_CONFIGURATION = Configuration.builder()
      .mappingProvider(new JacksonMappingProvider(
          JsonMapper.builder().enable(JsonReadFeature.ALLOW_UNQUOTED_FIELD_NAMES).build()))
      .jsonProvider(new JacksonJsonNodeJsonProvider()).build();


  public JsonValueFunction() {
    super("JSON_VALUE", ReturnTypes.ANY, OperandTypes.JSON_STRING.or(OperandTypes.STRING_STRING),
        OperatorCategory.JSON, "/docs/json_value.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    JsonNode jsonNode = operands[0].getValue(JsonNode.class);
    if (jsonNode == null)
      return null;

    String path = operands[1].getValue(String.class);
    if (path == null)
      throw new ExpressionException(ErrorCode.JSON_PATH_IS_NULL);

    Object value = null;
    try {
      JsonPath jsonPath = JsonPath.compile(path);
      value = jsonPath.read(jsonNode, JSONPATH_CONFIGURATION);
      if (value instanceof JsonNode) {
        JsonNode result = (JsonNode) value;
        if (result.isNull())
          return null;
        if (result.isTextual())
          return result.textValue();
        if (result.isNumber())
          return result.decimalValue();
        if (result.isBoolean())
          return result.booleanValue();
        if (result.isArray())
          throw new ExpressionException(ErrorCode.UNSUPPORTED_ARRAY_TYPE, path);
      }
      if (value instanceof Integer) {
        return Long.valueOf((Integer) value);
      }
      if (value instanceof Double) {
        return BigDecimal.valueOf((Double) value);
      }

      return value;    
    } catch (PathNotFoundException e) {
      // Return NULL if path not found
      return null;
    } catch (ClassCastException e) {
      throw new ExpressionException(ErrorCode.CONVERSION_ERROR, TypeId.fromValue(value), value, TypeId.ANY);
    } catch (Exception e) {
      throw new ExpressionException(ErrorCode.INVALID_JSON_PATH, path);
    }
  }
}
