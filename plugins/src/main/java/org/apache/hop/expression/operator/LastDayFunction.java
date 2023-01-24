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

import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.Coerce;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.LastDayOfQuarter;
import org.apache.hop.expression.util.TimeUnit;
import java.time.DayOfWeek;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalAdjuster;
import java.time.temporal.TemporalAdjusters;

/**
 * Returns the last day of the time unit.
 */
@FunctionPlugin
public class LastDayFunction extends Function {

  private static final LastDayOfQuarter LastDayOfQuarter = new LastDayOfQuarter();

  public LastDayFunction() {
    super("LAST_DAY", true, ReturnTypes.DATE, OperandTypes.DATE_OPTIONAL_TIMEUNIT,
        OperatorCategory.DATE, "/docs/last_day.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    Object value = operands[0].getValue(context);
    if (value == null)
      return null;

    // Default to last day of month
    TemporalAdjuster adjuster = TemporalAdjusters.lastDayOfMonth();
    if (operands.length == 2) {
      Object v1 = operands[1].getValue(context);
      if (v1 == null)
        return null;
      TimeUnit unit = Coerce.toTimeUnit(v1);
      switch (unit) {
        case YEAR:
          adjuster = TemporalAdjusters.lastDayOfYear();
          break;
        case QUARTER:
          adjuster = LastDayOfQuarter;
          break;
        case MONTH:
          adjuster = TemporalAdjusters.lastDayOfMonth();
          break;
        case WEEK:
          adjuster = TemporalAdjusters.nextOrSame(DayOfWeek.SUNDAY);
          break;
        default:
          throw new ExpressionException(ExpressionError.ILLEGAL_ARGUMENT, unit);
      }
    }

    // Remove time and adjust
    return Coerce.toDateTime(value).truncatedTo(ChronoUnit.DAYS).with(adjuster);
  }
}
