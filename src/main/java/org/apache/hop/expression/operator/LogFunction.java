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
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/** Calculates the specified base logarithm of a numeric value. */
@FunctionPlugin
public class LogFunction extends Function {

  public LogFunction() {
    super(
        "LOG",
        ReturnTypes.NUMBER_NULLABLE,
        OperandTypes.NUMBER_NUMBER,
        OperatorCategory.TRIGONOMETRY,
        "/docs/log.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    BigDecimal base = operands[0].getValue(BigDecimal.class);
    if (base == null) return null;

    BigDecimal value = operands[1].getValue(BigDecimal.class);
    if (value == null) return null;

    if (value.signum() <= 0)
      throw new IllegalArgumentException(ErrorCode.ARGUMENT_OUT_OF_RANGE.message(2, value));

    return BigDecimalMath.log(value, MATH_CONTEXT)
        .divide(BigDecimalMath.log(base, MATH_CONTEXT), MATH_CONTEXT);
  }
}
