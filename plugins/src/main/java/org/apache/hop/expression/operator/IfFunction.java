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
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Single-level if-then-else expression. Similar to CASE, but only allows a single condition.
 */
@FunctionPlugin
public class IfFunction extends Function {

  public IfFunction() {
    super("IF", ReturnTypes.IF_FUNCTION,
        OperandTypes.BOOLEAN_ANY.or(OperandTypes.BOOLEAN_SAME_SAME), Category.CONDITIONAL,
        "/docs/if.html");
  }

  @Override
  public IExpression compile(final IExpressionContext context, final Call call)
      throws ExpressionException {

    if (call.getOperandCount() == 2) {
      return new Call(call.getOperator(), call.getOperand(0), call.getOperand(1), Literal.NULL);
    }

    IExpression condition = call.getOperand(0);

    if (condition.is(Operators.IS_NULL)) {
      // IF(x IS NULL,y,x) → IFNULL(x, y)
      if (call.getOperand(2).equals(condition.asCall().getOperand(0))) {
        return new Call(Operators.IFNULL, call.getOperand(2), call.getOperand(1));
      }

      // IF(x IS NULL,y,z) → NVL2(x, z, y)
      return new Call(Operators.NVL2, condition.asCall().getOperand(0), call.getOperand(2),
          call.getOperand(1));
    }

    if (condition.is(Operators.IS_NOT_NULL)) {
      // IF(x IS NOT NULL,y,z) → NVL2(x,y,z)
      return new Call(Operators.NVL2, condition.asCall().getOperand(0), call.getOperand(1),
          call.getOperand(2));
    }

    if (condition.is(Operators.EQUAL) && call.getOperand(1) == Literal.NULL) {

      // IF(x=y,NULL,x) → NULLIF(x, y)
      if (condition.asCall().getOperand(0).equals(call.getOperand(2))) {
        return new Call(Operators.NULLIF, call.getOperand(2), condition.asCall().getOperand(1));
      }

      // IF(x=y,NULL,y) → NULLIF(y, x)
      if (condition.asCall().getOperand(1).equals(call.getOperand(2))) {
        return new Call(Operators.NULLIF, call.getOperand(2), condition.asCall().getOperand(0));
      }
    }

    return call;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Boolean value = operands[0].getValue(Boolean.class);
    if (value == null)
      value = Boolean.FALSE;

    return operands[value ? 1 : 2].getValue();
  }
}
