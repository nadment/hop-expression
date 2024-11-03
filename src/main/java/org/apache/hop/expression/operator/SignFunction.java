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
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/** Returns the sign of a number. */
@FunctionPlugin
public class SignFunction extends Function {

  public SignFunction() {
    super(
        "SIGN",
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.NUMERIC,
        OperatorCategory.MATHEMATICAL,
        "/docs/sign.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    BigDecimal value = operands[0].getValue(BigDecimal.class);
    if (value == null) return value;

    if (value.signum() == 0) return 0L;
    return (value.signum() > 0L) ? 1L : -1L;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    // Idempotent function repetition
    if (call.getOperand(0).isOperator(call.getOperator())) {
      return call.getOperand(0);
    }

    return call;
  }
}
