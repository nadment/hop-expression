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

import org.apache.hop.expression.Call;
import org.apache.hop.expression.Category;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * Returns the values rounded to the nearest integer.
 */
@FunctionPlugin
public class RoundFunctionFunction extends Function {

  public RoundFunctionFunction() {
    super("ROUND", ReturnTypes.NUMBER_NULLABLE, OperandTypes.NUMERIC, Category.MATHEMATICAL,
        "/docs/round.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    BigDecimal value = operands[0].getValue(BigDecimal.class);
    if (value == null)
      return value;
    return value.setScale(0, RoundingMode.HALF_UP);
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Idempotent function repetition
    if (call.getOperand(0).is(call.getOperator())) {
      return call.getOperand(0);
    }

    return call;
  }
}
