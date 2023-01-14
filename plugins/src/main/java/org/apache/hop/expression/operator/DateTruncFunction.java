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
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Coerse;
import org.apache.hop.expression.util.TimeUnit;
import java.time.DayOfWeek;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalAdjusters;

/**
 * Truncates a date or timestamp to the specified time unit.
 */
@FunctionPlugin
public class DateTruncFunction extends Function {

  public DateTruncFunction() {
    super("DATE_TRUNC", true, ReturnTypes.DATE, OperandTypes.DATE_TIMEUNIT,
        "i18n::Operator.Category.Date", "/docs/date_trunc.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {

    TimeUnit unit = Coerse.toTimeUnit(operands[0].getValue(context));

    Object v1 = operands[1].getValue(context);
    if (v1 == null)
      return null;

    ZonedDateTime datetime = Coerse.toDateTime(v1);

    switch (unit) {
      case MILLENNIUM:
        return datetime.withDayOfYear(1).minusYears(datetime.getYear() % 1000);
      case CENTURY:
        return datetime.withDayOfYear(1).minusYears(datetime.getYear() % 100);
      case DECADE:
        return datetime.withDayOfYear(1).minusYears(datetime.getYear() % 10);
      // First day of the year
      case YEAR:
        return datetime.withDayOfYear(1);
      // First day of the month
      case MONTH:
        return datetime.withDayOfMonth(1);
      // First day of the quarter
      case QUARTER:
        int month = (datetime.getMonthValue() / 3) * 3 + 1;
        return datetime.withMonth(month).withDayOfMonth(1);
      // First day of the week (the week starts on Monday)
      case WEEK:
        // TODO: DayOfWeek dow = DayOfWeek.of(Integer.parseInt((String)
        // context.getAttribute("NLS_FIRST_DAY_OF_WEEK")));
        return datetime.with(TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY));
      case DAY:
        return datetime.truncatedTo(ChronoUnit.DAYS);
      case HOUR:
        return datetime.truncatedTo(ChronoUnit.HOURS);
      case MINUTE:
        return datetime.truncatedTo(ChronoUnit.MINUTES);
      case SECOND:
        return datetime.truncatedTo(ChronoUnit.SECONDS);
      case MILLISECOND:
        return datetime.truncatedTo(ChronoUnit.MILLIS);
      case MICROSECOND:
        return datetime.truncatedTo(ChronoUnit.MICROS);
      case NANOSECOND:
        return datetime.truncatedTo(ChronoUnit.NANOS);
      default:
        throw new ExpressionException(ExpressionError.ILLEGAL_ARGUMENT, unit);
    }
  }
}
