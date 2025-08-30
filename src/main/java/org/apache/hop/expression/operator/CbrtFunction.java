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
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Types;

/** Returns the cubic root of a numeric expression. @See {@link SqrtFunction} */
@FunctionPlugin
public class CbrtFunction extends Function {

  private static final BigDecimal TREE = BigDecimal.valueOf(3);

  public CbrtFunction() {
    super(
        "CBRT",
        ReturnTypes.NUMBER_NULLABLE,
        OperandTypes.NUMBER,
        OperatorCategory.MATHEMATICAL,
        "/docs/cbrt.html");
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coerceOperandType(call, call.getType(), 0);
  }

  @Override
  public Object eval(final IExpression[] operands) {
    BigDecimal value = operands[0].getValue(BigDecimal.class);
    if (value == null) return null;

    // If negative value
    if (value.signum() < 0) {
      BigDecimal result = BigDecimalMath.root(value.abs(), TREE, MATH_CONTEXT);
      return result.negate();
    }

    return BigDecimalMath.root(value, TREE, MATH_CONTEXT);
  }
}
