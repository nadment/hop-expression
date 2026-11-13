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

import java.time.temporal.ChronoUnit;
import java.time.temporal.Temporal;
import java.util.EnumSet;
import java.util.Set;
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
import org.apache.hop.expression.util.DateConversion;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

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
@NullMarked
public class DateDiffFunction extends Function {

  private static final Set<TimeUnit> SUPPORTED_TIME_UNITS =
      EnumSet.of(
          TimeUnit.MILLENNIUM,
          TimeUnit.CENTURY,
          TimeUnit.DECADE,
          TimeUnit.YEAR,
          TimeUnit.QUARTER,
          TimeUnit.MONTH,
          TimeUnit.WEEK,
          TimeUnit.DAY,
          TimeUnit.HOUR,
          TimeUnit.MINUTE,
          TimeUnit.SECOND,
          TimeUnit.MILLISECOND,
          TimeUnit.MICROSECOND,
          TimeUnit.NANOSECOND);

  public DateDiffFunction() {
    super(
        "DATE_DIFF",
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.TIMEUNIT_DATE_DATE,
        OperatorCategory.DATE,
        "/docs/hop-user-manual/expression/date_diff.html");
  }

  /**
   * {@inheritDoc}
   *
   * <p>A time unit that is not supported, like ISOWEEK, is rejected when the call is validated, not
   * when a row is evaluated.
   */
  @Override
  protected void validateOperands(IExpressionContext context, Call call)
      throws ExpressionException {
    super.validateOperands(context, call);
    validateTimeUnit(call, 0, SUPPORTED_TIME_UNITS, ErrorCode.INVALID_ARGUMENT);
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {

    TimeUnit unit = operands[0].getValue(TimeUnit.class);

    Temporal startDateTime = operands[1].getValue(Temporal.class);
    if (startDateTime == null) return null;
    Temporal endDateTime = operands[2].getValue(Temporal.class);
    if (endDateTime == null) return null;

    try {
      return switch (unit) {
        case MILLENNIUM -> DateConversion.between(startDateTime, endDateTime, ChronoUnit.MILLENNIA);
        case CENTURY -> DateConversion.between(startDateTime, endDateTime, ChronoUnit.CENTURIES);
        case DECADE -> DateConversion.between(startDateTime, endDateTime, ChronoUnit.DECADES);
        case YEAR -> DateConversion.between(startDateTime, endDateTime, ChronoUnit.YEARS);
        case MONTH -> DateConversion.between(startDateTime, endDateTime, ChronoUnit.MONTHS);
        case QUARTER -> {
          long months = DateConversion.between(startDateTime, endDateTime, ChronoUnit.MONTHS);
          yield months / 3L;
        }
        case WEEK -> DateConversion.between(startDateTime, endDateTime, ChronoUnit.WEEKS);
        case DAY -> DateConversion.between(startDateTime, endDateTime, ChronoUnit.DAYS);
        case HOUR -> DateConversion.between(startDateTime, endDateTime, ChronoUnit.HOURS);
        case MINUTE -> DateConversion.between(startDateTime, endDateTime, ChronoUnit.MINUTES);
        case SECOND -> DateConversion.between(startDateTime, endDateTime, ChronoUnit.SECONDS);
        case MILLISECOND -> DateConversion.between(startDateTime, endDateTime, ChronoUnit.MILLIS);
        case MICROSECOND -> DateConversion.between(startDateTime, endDateTime, ChronoUnit.MICROS);
        case NANOSECOND -> DateConversion.between(startDateTime, endDateTime, ChronoUnit.NANOS);
        default -> throw new ExpressionException(ErrorCode.INVALID_ARGUMENT, unit);
      };
    } catch (ArithmeticException e) {
      // The number of small units, like nanoseconds, exceeds the integer range
      throw new ExpressionException(ErrorCode.ARITHMETIC_OVERFLOW, getName());
    }
  }
}
