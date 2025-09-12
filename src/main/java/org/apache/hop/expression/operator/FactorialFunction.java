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
import java.math.BigInteger;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Types;

/** Calculates the factorial value of a numeric expression. */
@FunctionPlugin
public class FactorialFunction extends Function {

  public FactorialFunction() {
    super(
        "FACTORIAL",
        ReturnTypes.NUMBER_NULLABLE,
        OperandTypes.INTEGER,
        OperatorCategory.MATHEMATICAL,
        "/docs/factorial.html");
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coerceOperandType(call, IntegerType.INTEGER, 0);
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Long value = operands[0].getValue(Long.class);
    if (value == null) return null;

    int n = value.intValue();

    if (n < 0) throw new ExpressionException(ErrorCode.ARGUMENT_OUT_OF_RANGE, 1, value);

    if (n > 20) {
      BigInteger result = BigInteger.ONE;
      for (int i = 2; i <= n; i++) {
        result = result.multiply(BigInteger.valueOf(i));
      }
      return new BigDecimal(result);
    }

    long result = 1;
    for (int i = 2; i <= n; i++) {
      result = result * i;
    }
    return new BigDecimal(result);
  }
}
