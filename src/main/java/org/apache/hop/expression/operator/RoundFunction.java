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
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Returns the values rounded to the nearest integer or decimal.
 *
 * @see CeilFunction
 * @see FloorFunction
 * @see TruncateFunction
 */
@FunctionPlugin
@NullMarked
public class RoundFunction extends Function {

  public RoundFunction() {
    super(
        "ROUND",
        ReturnTypes.NUMBER_NULLABLE,
        OperandTypes.NUMBER.or(OperandTypes.NUMBER_NUMBER),
        OperatorCategory.MATHEMATICAL,
        "/docs/hop-user-manual/expression/round.html");
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coerceOperandType(call, call.getType(), 0);
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    BigDecimal value = operands[0].getValue(BigDecimal.class);
    if (value == null) return null;

    int scale = 0;
    if (operands.length == 2) {
      Long l = operands[1].getValue(Long.class);
      if (l == null) return null;
      // Clamp the scale, the conversion to int would wrap a scale out of the int range. A scale
      // before the first integer digit returns 0, like any smaller scale, and a scale beyond the
      // decimal digits returns the value, like any greater scale.
      long lower = -Math.max(value.precision() - value.scale(), 0) - 1L;
      scale = (int) Math.clamp(l, lower, Integer.MAX_VALUE);
    }

    if (scale > value.scale()) {
      scale = value.scale();
    }

    if (scale == 0) {
      return value.setScale(0, RoundingMode.HALF_UP);
    }
    return value.movePointRight(scale).setScale(0, RoundingMode.HALF_UP).movePointLeft(scale);
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Idempotent function repetition, only if the inner call has no scale ROUND(ROUND(x)) →
    // ROUND(x)
    if (call.getOperandCount() == 1
        && call.getOperand(0).isOperator(call.getOperator())
        && call(call.getOperand(0)).getOperandCount() == 1) {
      return call.getOperand(0);
    }

    return call;
  }
}
