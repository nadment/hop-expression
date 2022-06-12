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

import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.ScalarFunction;
import org.apache.hop.expression.util.Coerse;
import java.io.StringWriter;
import com.fasterxml.jackson.core.json.JsonReadFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.spi.json.JacksonJsonNodeJsonProvider;
import com.jayway.jsonpath.spi.mapper.JacksonMappingProvider;

/**
 * Build a JSON object from a list of key=values pairs.
 * 
 * JSON_OBJECT([KEY] <key> VALUE <expression> [, [KEY] <key> VALUE <expression>]...)
 */
public class JsonValue extends Operator {

//public static final Configuration JSONPATH_CONFIGURATION =
//Configuration.builder().mappingProvider(new JacksonMappingProvider())
//  .jsonProvider(new JacksonJsonProvider()).build();

public static final Configuration JSONPATH_CONFIGURATION =
Configuration.builder().mappingProvider(new JacksonMappingProvider(JsonMapper.builder().enable(JsonReadFeature.ALLOW_UNQUOTED_FIELD_NAMES).build()))
  .jsonProvider(new JacksonJsonNodeJsonProvider()).build();


  public JsonValue() {
    super("JSON_VALUE", 10, true, true, "i18n::Operator.Category.Json", "/docs/json_value.html");
  }

  @ScalarFunction(id = "JSON_VALUE", category = "i18n::Operator.Category.Json", minArgs = 2,
      maxArgs = 2, documentationUrl = "/docs/json_value.html")

  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;

    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    JsonNode jsonNode = Coerse.toJson(v0);
    JsonPath jsonPath = JsonPath.compile(Coerse.toString(v1));

    JsonNode result = (JsonNode) jsonPath.read(jsonNode, JSONPATH_CONFIGURATION);
    if ( result.isNull()) return null;
    if ( result.isNumber() ) return result.decimalValue();
    if ( result.isBoolean()) return result.booleanValue();
    if ( result.isTextual()) return result.textValue();
   
    return result;
  }
  
  @Override
  public void unparse(final StringWriter writer, IExpression[] operands) {
    writer.append(this.getName());
    writer.append('(');
    boolean first = true;
    for (IExpression operand : operands) {
      if (!first)
        writer.append(',');
      else
        first = false;
      operand.unparse(writer);
    }
    writer.append(')');
  }
}
