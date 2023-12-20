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
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.time.ZonedDateTime;

/**
 * Adds or subtracts a specified number of months to a date or timestamp
 */
@FunctionPlugin
public class AddMonthsFunction extends Function {
  public static final Function INSTANCE = new AddMonthsFunction();

  public AddMonthsFunction() {
    super("ADD_MONTHS", ReturnTypes.DATE_NULLABLE, OperandTypes.TEMPORAL_NUMERIC, OperatorCategory.DATE,
        "/docs/add_months.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Simplify arithmetic ADD_MONTHS(A,0) → A
    if (Literal.ZERO.equals(call.getOperand(1))) {
      return call.getOperand(0);
    }

    return call;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    ZonedDateTime value = operands[0].getValue(ZonedDateTime.class);
    if (value == null)
      return null;

    Long months = operands[1].getValue(Long.class);
    if (months == null)
      return null;

    return value.plusMonths(months);
  }
}
