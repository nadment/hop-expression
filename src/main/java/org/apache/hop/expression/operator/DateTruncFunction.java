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
import java.time.temporal.TemporalAdjusters;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/** Truncates a date or timestamp to the specified time unit. */
@FunctionPlugin
public class DateTruncFunction extends Function {

  public DateTruncFunction() {
    super(
        "DATE_TRUNC",
        ReturnTypes.DATE_NULLABLE,
        OperandTypes.TIMEUNIT_DATE,
        OperatorCategory.DATE,
        "/docs/date_trunc.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {

    TimeUnit unit = operands[0].getValue(TimeUnit.class);

    ZonedDateTime datetime = operands[1].getValue(ZonedDateTime.class);
    if (datetime == null) return null;

    return switch (unit) {
      case MILLENNIUM -> datetime.withDayOfYear(1).minusYears(datetime.getYear() % 1000);
      case CENTURY -> datetime.withDayOfYear(1).minusYears(datetime.getYear() % 100);
      case DECADE -> datetime.withDayOfYear(1).minusYears(datetime.getYear() % 10);
      case YEAR ->
          // First day of the year
          datetime.withDayOfYear(1);
      case MONTH ->
          // First day of the month
          datetime.withDayOfMonth(1);
      case QUARTER -> {
        // First day of the quarter
        int month = (datetime.getMonthValue() / 3) * 3 + 1;
        yield datetime.withMonth(month).withDayOfMonth(1);
      }
      case WEEK ->
          // First day of the week (the week starts on Monday)
          datetime.with(TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY));
      case DAY -> datetime.truncatedTo(ChronoUnit.DAYS);
      case HOUR -> datetime.truncatedTo(ChronoUnit.HOURS);
      case MINUTE -> datetime.truncatedTo(ChronoUnit.MINUTES);
      case SECOND -> datetime.truncatedTo(ChronoUnit.SECONDS);
      case MILLISECOND -> datetime.truncatedTo(ChronoUnit.MILLIS);
      case MICROSECOND -> datetime.truncatedTo(ChronoUnit.MICROS);
      case NANOSECOND -> datetime.truncatedTo(ChronoUnit.NANOS);
      default -> throw new ExpressionException(ErrorCode.INVALID_ARGUMENT, unit);
    };
  }
}
