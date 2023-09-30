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

import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import com.fasterxml.jackson.core.json.JsonReadFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.spi.json.JacksonJsonNodeJsonProvider;
import com.jayway.jsonpath.spi.mapper.JacksonMappingProvider;

/**
 * Extracts a JSON fragment from a JSON object or string representing a Json.
 * <code>JSON_QUERY( expression [, path] )</code>
 */
@FunctionPlugin
public class JsonQueryFunction extends Function {

  public static final Configuration JSONPATH_CONFIGURATION = Configuration.builder()
      .mappingProvider(new JacksonMappingProvider(
          JsonMapper.builder().enable(JsonReadFeature.ALLOW_UNQUOTED_FIELD_NAMES).build()))
      .jsonProvider(new JacksonJsonNodeJsonProvider()).build();


  public JsonQueryFunction() {
    super("JSON_QUERY", ReturnTypes.JSON, OperandTypes.JSON.or(OperandTypes.JSON_STRING),
        "i18n::Operator.Category.Json", "/docs/json_query.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    JsonNode jsonNode = operands[0].getValue(JsonNode.class);
    if (jsonNode == null)
      return null;

    if ( operands.length==1)
      return jsonNode;
    
    String path = operands[1].getValue(String.class);
    if (path == null)
      throw new ExpressionException(ExpressionError.JSON_PATH_IS_NULL);

    try {
      JsonPath jsonPath = JsonPath.compile(path);
      JsonNode result = (JsonNode) jsonPath.read(jsonNode, JSONPATH_CONFIGURATION);
      return result;
    } catch (Exception e) {
      throw new ExpressionException(ExpressionError.INVALID_JSON_PATH, path);
    }
  }
}
