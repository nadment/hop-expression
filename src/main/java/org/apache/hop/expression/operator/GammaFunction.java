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

import ch.obermuhlner.math.big.BigDecimalMath;
import java.math.BigDecimal;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Returns the value of the gamma function Γ(x), which extends the factorial to real numbers: Γ(n) =
 * (n-1)! for a positive integer n.
 */
@FunctionPlugin
@NullMarked
public class GammaFunction extends Function {

  /**
   * The smallest argument whose result overflows the maximum precision of a number: Γ(35) = 34! has
   * 39 digits. The gamma function increases beyond 2, the result is not computed for a larger
   * argument, which would be slow.
   */
  private static final BigDecimal OVERFLOW_ARGUMENT = BigDecimal.valueOf(35);

  public GammaFunction() {
    super(
        "GAMMA",
        ReturnTypes.NUMBER_NULLABLE,
        OperandTypes.NUMBER,
        OperatorCategory.MATHEMATICAL,
        "/docs/hop-user-manual/expression/gamma.html");
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coerceOperandType(call, call.getType(), 0);
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    BigDecimal value = operands[0].getValue(BigDecimal.class);
    if (value == null) return null;

    // The gamma function is undefined for zero and the negative integers, its poles
    if (value.signum() <= 0 && value.stripTrailingZeros().scale() <= 0) {
      throw new ExpressionException(ErrorCode.ARGUMENT_OUT_OF_RANGE, 1, value.stripTrailingZeros());
    }

    if (value.compareTo(OVERFLOW_ARGUMENT) >= 0) {
      throw new ExpressionException(ErrorCode.ARITHMETIC_OVERFLOW, getName());
    }

    // A positive integer returns the exact factorial, a value near a pole can still overflow
    return checkNumberOverflow(BigDecimalMath.gamma(value, MATH_CONTEXT));
  }
}
