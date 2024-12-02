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
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.type.JsonType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.TypeId;

/** Check if a string is a valid JSON. */
@FunctionPlugin
public class IsJsonFunction extends Function {

  public IsJsonFunction() {
    super(
        "IS_JSON",
        ReturnTypes.BOOLEAN_NOT_NULL,
        OperandTypes.ANY,
        OperatorCategory.COMPARISON,
        "/docs/is_json.html");
  }

  @Override
  public IExpression compile(final IExpressionContext context, final Call call)
      throws ExpressionException {

    if (call.getOperand(0).getType().is(TypeId.STRING)) {
      return call;
    }

    // Optimize "IS_JSON(json)" to "json IS NOT NULL"
    if (call.getOperand(0).getType().is(TypeId.JSON)) {
      return new Call(Operators.IS_NOT_NULL, call.getOperand(0));
    }

    // Other data type are always false
    return Literal.FALSE;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);

    // Return FALSE if a value is NULL.
    if (value == null) return Boolean.FALSE;

    try {
      JsonType.convert(value);
      return Boolean.TRUE;
    } catch (Exception e) {
      return Boolean.FALSE;
    }
  }
}
