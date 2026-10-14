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
import java.math.MathContext;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** Calculates the specified base logarithm of a numeric value. */
@FunctionPlugin
@NullMarked
public class LogFunction extends Function {

  /**
   * Extra digits for the logarithms and their division, rounded at the end, so that an exact result
   * like LOG(2,8) does not end with an error of the last digit.
   */
  private static final MathContext EXTENDED_CONTEXT =
      new MathContext(MATH_CONTEXT.getPrecision() + 10, MATH_CONTEXT.getRoundingMode());

  public LogFunction() {
    super(
        "LOG",
        ReturnTypes.NUMBER_NULLABLE,
        OperandTypes.NUMBER_NUMBER,
        OperatorCategory.TRIGONOMETRY,
        "/docs/log.html");
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    BigDecimal base = operands[0].getValue(BigDecimal.class);
    if (base == null) return null;

    // The base must be positive and different from 1, whose logarithm is 0
    if (base.signum() <= 0 || base.compareTo(BigDecimal.ONE) == 0) {
      throw new ExpressionException(ErrorCode.ARGUMENT_OUT_OF_RANGE, 1, base.stripTrailingZeros());
    }

    BigDecimal value = operands[1].getValue(BigDecimal.class);
    if (value == null) return null;

    if (value.signum() <= 0) {
      throw new ExpressionException(ErrorCode.ARGUMENT_OUT_OF_RANGE, 2, value.stripTrailingZeros());
    }

    return BigDecimalMath.log(value, EXTENDED_CONTEXT)
        .divide(BigDecimalMath.log(base, EXTENDED_CONTEXT), EXTENDED_CONTEXT)
        .round(MATH_CONTEXT);
  }
}
