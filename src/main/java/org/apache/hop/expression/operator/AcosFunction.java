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
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Types;

/**
 * Calculates the inverse cosine (arc cosine) of a number in radians; the result is a number in the
 * interval [0, pi].
 */
@FunctionPlugin
public class AcosFunction extends Function {

  public AcosFunction() {
    super(
        "ACOS",
        ReturnTypes.NUMBER_NULLABLE,
        OperandTypes.NUMBER,
        OperatorCategory.TRIGONOMETRY,
        "/docs/acos.html");
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coerceOperandType(call, call.getType(), 0);
  }

  @Override
  public Object eval(final IExpression[] operands) {
    BigDecimal number = operands[0].getValue(BigDecimal.class);
    if (number == null) return null;

    double value = number.doubleValue();
    if (value < -1.0 || value > 1.0) {
      throw new IllegalArgumentException(ErrorCode.ARGUMENT_OUT_OF_RANGE.message(1, value));
    }
    // FIXME: Use BigDecimalMath when bug are fixed
    // https://github.com/eobermuhlner/big-math/issues/66
    return BigDecimal.valueOf(FastMath.acos(value));
  }
}
