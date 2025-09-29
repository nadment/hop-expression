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

import com.fasterxml.jackson.core.json.JsonReadFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.InvalidPathException;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.PathNotFoundException;
import com.jayway.jsonpath.spi.json.JacksonJsonNodeJsonProvider;
import com.jayway.jsonpath.spi.mapper.JacksonMappingProvider;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.ArrayType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.StringType;

/**
 * Extracts unique JSON keys from a JSON expression.
 *
 * <p><code>JSON_KEYS( expression [, path] )</code>
 */
@FunctionPlugin
public class JsonKeysFunction extends Function {

  public static final Configuration JSONPATH_CONFIGURATION =
      Configuration.builder()
          .mappingProvider(
              new JacksonMappingProvider(
                  JsonMapper.builder().enable(JsonReadFeature.ALLOW_UNQUOTED_FIELD_NAMES).build()))
          .jsonProvider(new JacksonJsonNodeJsonProvider())
          .build();

  public JsonKeysFunction() {
    super(
        "JSON_KEYS",
        ReturnTypes.ARRAY_OF_STRING,
        OperandTypes.JSON.or(OperandTypes.JSON_STRING),
        OperatorCategory.JSON,
        "/docs/json_keys.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    JsonNode jsonNode = operands[0].getValue(JsonNode.class);
    if (jsonNode == null) return null;

    if (operands.length == 2) {
      String path = operands[1].getValue(String.class);
      if (path == null) return null;
      try {
        JsonPath jsonPath = JsonPath.compile(path);
        jsonNode = jsonPath.read(jsonNode, JSONPATH_CONFIGURATION);
      } catch (PathNotFoundException e) {
        // Returns NULL if the path does not locate an object
        return null;
      } catch (InvalidPathException e) {
        throw new ExpressionException(ErrorCode.INVALID_JSON_PATH, path);
      }
    }

    Set<IExpression> keys = new HashSet<>();
    Iterator<String> iterator = jsonNode.fieldNames();
    iterator.forEachRemaining(key -> keys.add(Literal.of(key)));

    return new Array(ArrayType.of(StringType.STRING), keys);
  }
}
