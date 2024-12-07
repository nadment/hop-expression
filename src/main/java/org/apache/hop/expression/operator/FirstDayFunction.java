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

import java.time.DayOfWeek;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalAdjuster;
import java.time.temporal.TemporalAdjusters;
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
import org.apache.hop.expression.util.FirstDayOfQuarter;

/**
 * Returns the first day of the time unit.
 *
 * <p>The supported units of a time are {@link TimeUnit#YEAR}, {@link TimeoUnit#MONTH}, {@link
 * TimeoUnit#QUARTER}, {@link TimeoUnit#WEEK} and {@link TimeUnit#DAY}.
 *
 * @see {@link LastDayFunction}
 */
@FunctionPlugin
public class FirstDayFunction extends Function {
  private static final FirstDayOfQuarter FirstDayOfQuarter = new FirstDayOfQuarter();

  public FirstDayFunction() {
    super(
        "FIRST_DAY",
        ReturnTypes.DATE_NULLABLE,
        OperandTypes.TEMPORAL.or(OperandTypes.TEMPORAL_TIMEUNIT),
        OperatorCategory.DATE,
        "/docs/first_day.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Validate time unit
    if (call.getOperandCount() == 2) {
      TimeUnit unit = call.getOperand(1).getValue(TimeUnit.class);
      switch (unit) {
        case YEAR, QUARTER, MONTH, WEEK:
          break;
        default:
          throw new ExpressionException(ErrorCode.UNSUPPORTED_TIME_UNIT, unit);
      }
    }

    return call;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    ZonedDateTime value = operands[0].getValue(ZonedDateTime.class);
    if (value == null) return null;

    // Default to first day of month
    TemporalAdjuster adjuster = TemporalAdjusters.firstDayOfMonth();

    if (operands.length == 2) {
      TimeUnit unit = operands[1].getValue(TimeUnit.class);

      switch (unit) {
        case YEAR:
          adjuster = TemporalAdjusters.firstDayOfYear();
          break;
        case QUARTER:
          adjuster = FirstDayOfQuarter;
          break;
        case MONTH:
          adjuster = TemporalAdjusters.firstDayOfMonth();
          break;
        case WEEK:
          adjuster = TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY);
          break;
        default:
          throw new ExpressionException(ErrorCode.UNSUPPORTED_TIME_UNIT, unit);
      }
    }

    // Remove time and adjust
    return value.truncatedTo(ChronoUnit.DAYS).with(adjuster);
  }
}
