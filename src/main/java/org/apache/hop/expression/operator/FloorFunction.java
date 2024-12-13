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

import java.math.BigDecimal;
import java.math.RoundingMode;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Returns the values rounded to the nearest equal or smaller integer.
 *
 * @see {@link CeilingOperator}, {@link RoundOperator}, {@link TruncateOperator}
 */
@FunctionPlugin
public class FloorFunction extends Function {

  public FloorFunction() {
    super(
        "FLOOR",
        ReturnTypes.CEIL_FLOOR_FUNCTION,
        OperandTypes.NUMBER,
        OperatorCategory.MATHEMATICAL,
        "/docs/floor.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    BigDecimal value = operands[0].getValue(BigDecimal.class);
    if (value == null) return null;

    return value.setScale(0, RoundingMode.FLOOR);
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Idempotent function repetition
    // FLOOR(FLOOR(x)) → FLOOR(x)
    if (call.getOperand(0).isOperator(call.getOperator())) {
      return call.getOperand(0);
    }
    // FLOOR(CEIL(x)) → CEIL(x)
    if (call.getOperand(0).isOperator(FunctionRegistry.getFunction("CEIL"))) {
      return call.getOperand(0);
    }

    return call;
  }
}
