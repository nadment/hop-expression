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

import org.apache.hop.expression.Call;
import org.apache.hop.expression.Category;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.IOperandCountRange;
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.OperandCountRange;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.TypeFamily;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.util.EnumSet;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * Build a JSON object from a list of key=values pairs.
 * 
 * <code>JSON_OBJECT([KEY] <key> VALUE <expression> [, [KEY] <key> VALUE <expression>]...)</code>
 */
@FunctionPlugin
public class JsonObjectFunction extends Function {

  public static final IOperandTypeChecker OTC = new JsonObjectFunctionOperandTypeChecker();

  private static final EnumSet<TypeFamily> VALUE_TYPES =
      EnumSet.of(TypeFamily.STRING, TypeFamily.BOOLEAN, TypeFamily.NUMERIC);

  public static class JsonObjectFunctionOperandTypeChecker implements IOperandTypeChecker {

    public JsonObjectFunctionOperandTypeChecker() {}

    @Override
    public boolean checkOperandTypes(Call call) {
      for (int i = 0; i < call.getOperandCount();) {
        // Key should be string
        if (call.getOperand(i++).getType().getFamily() != TypeFamily.STRING) {
          return false;
        }

        IExpression value = call.getOperand(i++);
        if (value.isNull())
          continue;
        if (!VALUE_TYPES.contains(value.getType().getFamily())) {
          return false;
        }
      }

      return true;
    }

    @Override
    public IOperandCountRange getOperandCountRange() {
      return OperandCountRange.between(2, Integer.MAX_VALUE);
    }
  }


  public JsonObjectFunction() {
    super("JSON_OBJECT", ReturnTypes.JSON, OTC, Category.JSON, "/docs/json_object.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {

    ObjectNode node = JsonNodeFactory.instance.objectNode();

    for (int i = 0; i < operands.length; i += 2) {
      String key = operands[i].getValue(String.class);
      Object value = operands[i + 1].getValue();
      if (value == null) {
        node.putNull(key);
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
        throw new ExpressionException(ErrorCode.UNSUPPORTED_JSON_TYPE);
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
