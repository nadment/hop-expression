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
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Types;

/**
 * Returns the values rounded to the nearest equal or larger integer.
 *
 * @see FloorFunction
 * @see RoundFunction
 * @see TruncateFunction
 */
@FunctionPlugin
public class CeilFunction extends Function {

  public static final Function INSTANCE = new CeilFunction();

  public CeilFunction() {
    super(
        "CEIL",
        ReturnTypes.CEIL_FLOOR_FUNCTION,
        OperandTypes.NUMBER,
        OperatorCategory.MATHEMATICAL,
        "/docs/ceil.html");
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coerceOperandType(call, call.getType(), 0);
  }

  @Override
  public Object eval(final IExpression[] operands) {
    BigDecimal value = operands[0].getValue(BigDecimal.class);
    if (value == null) return null;

    return value.setScale(0, RoundingMode.CEILING);
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Idempotent function repetition
    // CEIL(CEIL(x)) → CEIL(x)
    if (call.getOperand(0).isOperator(call.getOperator())) {
      return call.getOperand(0);
    }
    // CEIL(FLOOR(x)) → FLOOR(x)
    if (call.getOperand(0).isOperator(FloorFunction.INSTANCE)) {
      return call.getOperand(0);
    }

    return call;
  }
}
