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
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Adds or subtracts a specified number of time unit to a date or timestamp
 *
 * @see AddDaysFunction
 * @see AddWeeksFunction
 * @see AddQuartersFunction
 * @see AddMonthsFunction
 * @see AddYearsFunction
 * @see AddHoursFunction
 * @see AddMinutesFunction
 * @see AddSecondsFunction
 * @see AddNanosecondsFunction
 */
@FunctionPlugin
public class DateAddFunction extends Function {

  public DateAddFunction() {
    super(
        "DATE_ADD",
        ReturnTypes.DATE_NULLABLE,
        OperandTypes.TIMEUNIT_INTEGER_DATE,
        OperatorCategory.DATE,
        "/docs/date_add.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    TimeUnit unit = call.getOperand(0).getValue(TimeUnit.class);
    return switch (unit) {
      case YEAR -> new Call(AddYearsFunction.INSTANCE, call.getOperand(2), call.getOperand(1));
      case QUARTER ->
          new Call(AddQuartersFunction.INSTANCE, call.getOperand(2), call.getOperand(1));
      case MONTH -> new Call(AddMonthsFunction.INSTANCE, call.getOperand(2), call.getOperand(1));
      case WEEK -> new Call(AddWeeksFunction.INSTANCE, call.getOperand(2), call.getOperand(1));
      case DAY -> new Call(AddDaysFunction.INSTANCE, call.getOperand(2), call.getOperand(1));
      case HOUR -> new Call(AddHoursFunction.INSTANCE, call.getOperand(2), call.getOperand(1));
      case MINUTE -> new Call(AddMinutesFunction.INSTANCE, call.getOperand(2), call.getOperand(1));
      case SECOND -> new Call(AddSecondsFunction.INSTANCE, call.getOperand(2), call.getOperand(1));
      case NANOSECOND ->
          new Call(AddNanosecondsFunction.INSTANCE, call.getOperand(2), call.getOperand(1));
      default -> throw new ExpressionException(ErrorCode.INVALID_ARGUMENT, unit);
    };
  }
}
