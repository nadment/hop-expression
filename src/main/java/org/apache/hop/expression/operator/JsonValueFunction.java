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
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.JsonPathException;
import com.jayway.jsonpath.PathNotFoundException;
import com.jayway.jsonpath.spi.json.JacksonJsonNodeJsonProvider;
import com.jayway.jsonpath.spi.mapper.JacksonMappingProvider;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.Set;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;

/** Extracts a scalar value from a JSON string. */
@FunctionPlugin
public class JsonValueFunction extends Function {

  public static final Configuration JSONPATH_CONFIGURATION =
      Configuration.builder()
          .mappingProvider(
              new JacksonMappingProvider(
                  JsonMapper.builder().enable(JsonReadFeature.ALLOW_UNQUOTED_FIELD_NAMES).build()))
          .jsonProvider(new JacksonJsonNodeJsonProvider())
          .build();
  private static final Set<TypeName> SUPPORTED_TYPES =
      Set.of(TypeName.STRING, TypeName.BOOLEAN, TypeName.INTEGER, TypeName.NUMBER, TypeName.BINARY);

  public JsonValueFunction() {
    super(
        "JSON_VALUE",
        ReturnTypes.JSON_VALUE,
        OperandTypes.JSON_VALUE,
        OperatorCategory.JSON,
        "/docs/json_value.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    if (call.getOperandCount() == 2) {
      // Add default returning type arguments
      return new Call(
          call.getOperator(),
          call.getOperand(0),
          call.getOperand(1),
          Literal.of(StringType.STRING));
    }

    // Check if the returning type is supported
    Type type = call.getOperand(2).getValue(Type.class);
    if (!SUPPORTED_TYPES.contains(type.getName())) {
      throw new ExpressionException(ErrorCode.UNEXPECTED_DATA_TYPE, type, getName());
    }

    return call;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    JsonNode jsonNode = operands[0].getValue(JsonNode.class);
    if (jsonNode == null) return null;

    String path = operands[1].getValue(String.class);
    if (path == null) throw new ExpressionException(ErrorCode.JSON_PATH_IS_NULL);

    Object value = null;
    try {
      JsonPath jsonPath = JsonPath.compile(path);
      value = jsonPath.read(jsonNode, JSONPATH_CONFIGURATION);
      if (value instanceof JsonNode result) {
        if (result.isNull()) return null;
        Type type = operands[2].getValue(Type.class);
        if (type.is(TypeName.STRING)) {
          return result.asText();
        }
        if (type.is(TypeName.BOOLEAN)) {
          return result.asBoolean();
        }
        if (type.is(TypeName.INTEGER)) {
          return result.asLong();
        }
        if (type.is(TypeName.NUMBER)) {
          return result.decimalValue();
        }
        if (type.is(TypeName.BINARY)) {
          try {
            return result.binaryValue();
          } catch (IOException e) {
            throw new RuntimeException(e);
          }
        }

        return result;
      }

      // Json function can return integer
      else if (value instanceof Integer integer) {
        return Long.valueOf(integer);
      } else if (value instanceof Double number) {
        return BigDecimal.valueOf(number);
      }

      return value;
    } catch (PathNotFoundException e) {
      // Return NULL if path not found
      return null;
    } catch (ClassCastException e) {
      throw new ExpressionException(
          ErrorCode.CONVERSION_ERROR, TypeName.fromValue(value), TypeName.ANY, value);
    } catch (JsonPathException e) {
      throw new ExpressionException(ErrorCode.INVALID_JSON_PATH, path);
    }
  }
}
