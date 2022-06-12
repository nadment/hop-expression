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
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.ScalarFunction;
import org.apache.hop.expression.util.Coerse;
import java.io.StringWriter;
import java.math.BigDecimal;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * Build a JSON object from a list of key=values pairs.
 * 
 * JSON_OBJECT([KEY] <key> VALUE <expression> [, [KEY] <key> VALUE <expression>]...)
 */
public class JsonObject extends Operator {

  public JsonObject() {
    super("JSON_OBJECT", 10, true, true, "i18n::Operator.Category.Json", "/docs/json_object.html");
  }

  @ScalarFunction(id = "JSON_OBJECT", category = "i18n::Operator.Category.Json", minArgs = 2,
      maxArgs = 100, documentationUrl = "/docs/json_object.html")
  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {

    ObjectNode node = JsonNodeFactory.instance.objectNode();
    
    // TODO: JSON_OBJECT(*)
    // if ( operands.length==0) {
    // for ( IValueMeta meta : context.getRowMeta().getValueMetaList() ) {
    // Object value = context.getRow()
    // node.put(meta.getName(), value);
    // }
    // }

    for (int i = 0; i < operands.length; i += 2) {
      String key = Coerse.toString(operands[i].eval(context));
      Object value = operands[i + 1].eval(context);
      if (value == null) {
        node.put(key, "null");
      } else if (value instanceof String) {
        node.put(key, (String) value);
      } else if (value instanceof Boolean) {
        node.put(key, (Boolean) value);
      } else if (value instanceof Long) {
        node.put(key, (Long) value);
      } else if (value instanceof BigDecimal) {
        node.put(key, (BigDecimal) value);
      }
      // TODO: Support Json Date
      else {
        throw new ExpressionException(ExpressionError.UNSUPPORTED_JSON_TYPE);
      }
    }

    return node;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append("JSON_OBJECT(");
    for (int i = 0; i < operands.length; i += 2) {
      if (i > 0) {
        writer.append(',');
      }
      writer.append("KEY ");
      operands[i].unparse(writer);
      writer.append(" VALUE ");
      operands[i + 1].unparse(writer);
    }
    writer.append(')');
  }
}
