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

import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Returns the difference in complete time unit between two date or timestamp.
 *
 * @see DaysBetweenFunction
 * @see WeeksBetweenFunction
 * @see MonthsBetweenFunction
 * @see YearsBetweenFunction
 * @see HoursBetweenFunction
 * @see MinutesBetweenFunction
 * @see SecondsBetweenFunction
 */
@FunctionPlugin
public class DateDiffFunction extends Function {

  public DateDiffFunction() {
    super(
        "DATE_DIFF",
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.TIMEUNIT_DATE_DATE,
        OperatorCategory.DATE,
        "/docs/date_diff.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {

    TimeUnit unit = operands[0].getValue(TimeUnit.class);

    ZonedDateTime startDateTime = operands[1].getValue(ZonedDateTime.class);
    if (startDateTime == null) return null;
    ZonedDateTime endDateTime = operands[2].getValue(ZonedDateTime.class);
    if (endDateTime == null) return null;

    return switch (unit) {
      case MILLENNIUM -> startDateTime.until(endDateTime, ChronoUnit.MILLENNIA);
      case CENTURY -> startDateTime.until(endDateTime, ChronoUnit.CENTURIES);
      case DECADE -> startDateTime.until(endDateTime, ChronoUnit.DECADES);
      case YEAR -> startDateTime.until(endDateTime, ChronoUnit.YEARS);
      case MONTH -> startDateTime.until(endDateTime, ChronoUnit.MONTHS);
      case QUARTER -> {
        long months = startDateTime.until(endDateTime, ChronoUnit.MONTHS);
        yield months / 3L;
      }
      case WEEK -> startDateTime.until(endDateTime, ChronoUnit.WEEKS);
      case DAY -> startDateTime.until(endDateTime, ChronoUnit.DAYS);
      case HOUR -> startDateTime.until(endDateTime, ChronoUnit.HOURS);
      case MINUTE -> startDateTime.until(endDateTime, ChronoUnit.MINUTES);
      case SECOND -> startDateTime.until(endDateTime, ChronoUnit.SECONDS);
      case MILLISECOND -> startDateTime.until(endDateTime, ChronoUnit.MILLIS);
      case MICROSECOND -> startDateTime.until(endDateTime, ChronoUnit.MICROS);
      case NANOSECOND -> startDateTime.until(endDateTime, ChronoUnit.NANOS);
      default -> throw new ExpressionException(ErrorCode.INVALID_ARGUMENT, unit);
    };
  }
}
