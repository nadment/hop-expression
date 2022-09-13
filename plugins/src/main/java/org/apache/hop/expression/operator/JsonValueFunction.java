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
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Coerse;
import com.fasterxml.jackson.core.json.JsonReadFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.PathNotFoundException;
import com.jayway.jsonpath.spi.json.JacksonJsonNodeJsonProvider;
import com.jayway.jsonpath.spi.mapper.JacksonMappingProvider;


@FunctionPlugin
public class JsonValueFunction extends Function {

  public static final Configuration JSONPATH_CONFIGURATION = Configuration.builder()
      .mappingProvider(new JacksonMappingProvider(
          JsonMapper.builder().enable(JsonReadFeature.ALLOW_UNQUOTED_FIELD_NAMES).build()))
      .jsonProvider(new JacksonJsonNodeJsonProvider()).build();


  public JsonValueFunction() {
    super("JSON_VALUE", true, ReturnTypes.STRING, OperandTypes.JSON_STRING_OR_STRING_STRING, "i18n::Operator.Category.Json", "/docs/json_value.html");
  }
  
  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].getValue(context);
    if (v0 == null)
      return null;

    Object v1 = operands[1].getValue(context);
    if (v1 == null)
      return null;

    JsonNode jsonNode = Coerse.toJson(v0);
    JsonPath jsonPath;
    String path = Coerse.toString(v1);
    try {
      jsonPath = JsonPath.compile(Coerse.toString(v1));

      JsonNode result = (JsonNode) jsonPath.read(jsonNode, JSONPATH_CONFIGURATION);

      if (result.isNull())
        return null;
      if (result.isNumber())
        return result.decimalValue();
      if (result.isBoolean())
        return result.booleanValue();
      if (result.isTextual())
        return result.textValue();
      return result;
    } catch (PathNotFoundException e) {
      throw new ExpressionException(ExpressionError.JSON_PATH_NOT_FOUND,path);
    } catch (Exception e) {
      throw new ExpressionException(ExpressionError.INVALID_JSON_PATH,path);
    }
  }
}
