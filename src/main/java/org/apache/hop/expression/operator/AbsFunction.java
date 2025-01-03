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
import org.apache.commons.math3.util.FastMath;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;

/** Returns the absolute (positive) value of the numeric or interval value. */
@FunctionPlugin
public class AbsFunction extends Function {

  public AbsFunction() {
    super(
        "ABS",
        ReturnTypes.ABS_FUNCTION,
        OperandTypes.NUMERIC.or(OperandTypes.INTERVAL),
        OperatorCategory.MATHEMATICAL,
        "/docs/abs.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    // Idempotent function repetition ABS(ABS(x)) → ABS(x)
    if (call.getOperand(0).isOperator(call.getOperator())) {
      return call.getOperand(0);
    }

    Type type = call.getOperand(0).getType();

    if (Types.isInterval(type)) {
      return new Call(IntervalAbsFunction.INSTANCE, call.getOperands());
    }

    if (Types.isInteger(type)) {
      return new Call(IntegerAbsFunction.INSTANCE, call.getOperands());
    }

    // If type Number or String
    return new Call(NumberAbsFunction.INSTANCE, call.getOperands());
  }

  /** Returns the absolute value of the integer value. */
  private static final class IntegerAbsFunction extends AbsFunction {
    public static final IntegerAbsFunction INSTANCE = new IntegerAbsFunction();

    @Override
    public Object eval(final IExpression[] operands) {
      Long value = operands[0].getValue(Long.class);
      if (value == null) return value;

      return FastMath.abs(value);
    }
  }

  /** Returns the absolute value of the number value. */
  private static final class NumberAbsFunction extends AbsFunction {
    public static final NumberAbsFunction INSTANCE = new NumberAbsFunction();

    @Override
    public Object eval(final IExpression[] operands) {
      BigDecimal value = operands[0].getValue(BigDecimal.class);
      if (value == null) return value;

      return value.abs();
    }
  }

  /** Returns the absolute value of the interval value. */
  private static final class IntervalAbsFunction extends AbsFunction {
    public static final IntervalAbsFunction INSTANCE = new IntervalAbsFunction();

    @Override
    public Object eval(final IExpression[] operands) {
      Interval value = operands[0].getValue(Interval.class);
      if (value == null) return value;

      return value.abs();
    }
  }
}
