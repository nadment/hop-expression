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

import java.time.DateTimeException;
import java.time.DayOfWeek;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.time.temporal.Temporal;
import java.time.temporal.TemporalAdjusters;
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
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** Truncates a date or timestamp to the specified time unit. */
@FunctionPlugin
@NullMarked
public class DateTruncFunction extends Function {

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

  public DateTruncFunction() {
    super(
        "DATE_TRUNC",
        ReturnTypes.TEMPORAL_NULLABLE,
        OperandTypes.TIMEUNIT_DATE,
        OperatorCategory.DATE,
        "/docs/hop-user-manual/expression/date_trunc.html");
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

    Temporal temporal = operands[1].getValue(Temporal.class);
    if (temporal == null) return null;

    try {
      return switch (unit) {
        case HOUR -> truncatedTo(temporal, ChronoUnit.HOURS);
        case MINUTE -> truncatedTo(temporal, ChronoUnit.MINUTES);
        case SECOND -> truncatedTo(temporal, ChronoUnit.SECONDS);
        case MILLISECOND -> truncatedTo(temporal, ChronoUnit.MILLIS);
        case MICROSECOND -> truncatedTo(temporal, ChronoUnit.MICROS);
        case NANOSECOND -> truncatedTo(temporal, ChronoUnit.NANOS);
        default -> truncateToDayOrMore(temporal, unit);
      };
    } catch (DateTimeException e) {
      // The first year of the decade can be before the first supported year
      throw new ExpressionException(ErrorCode.TEMPORAL_OVERFLOW, getName());
    }
  }

  /**
   * Truncates a date-time, without or with time zone, to a unit. The result is of the same class.
   */
  private static Temporal truncatedTo(Temporal temporal, ChronoUnit unit) {
    return switch (temporal) {
      case LocalDateTime local -> local.truncatedTo(unit);
      case ZonedDateTime zoned -> zoned.truncatedTo(unit);
      default -> throw new IllegalArgumentException("Not a date-time: " + temporal);
    };
  }

  /** Truncates to a unit of a day or more, so the time is always reset to midnight. */
  private static Temporal truncateToDayOrMore(Temporal temporal, TimeUnit unit) {
    Temporal day = truncatedTo(temporal, ChronoUnit.DAYS);
    int year = day.get(ChronoField.YEAR);

    return switch (unit) {
      case MILLENNIUM ->
          day.with(ChronoField.DAY_OF_YEAR, 1).with(ChronoField.YEAR, firstYear(year, 1000));
      case CENTURY ->
          day.with(ChronoField.DAY_OF_YEAR, 1).with(ChronoField.YEAR, firstYear(year, 100));
      case DECADE ->
          day.with(ChronoField.DAY_OF_YEAR, 1).with(ChronoField.YEAR, firstYearOfDecade(year));
      case YEAR ->
          // First day of the year
          day.with(ChronoField.DAY_OF_YEAR, 1);
      case MONTH ->
          // First day of the month
          day.with(ChronoField.DAY_OF_MONTH, 1);
      case QUARTER -> {
        // First day of the quarter (month value is 1-based, so shift to 0-based before dividing)
        int month = ((day.get(ChronoField.MONTH_OF_YEAR) - 1) / 3) * 3 + 1;
        yield day.with(ChronoField.MONTH_OF_YEAR, month).with(ChronoField.DAY_OF_MONTH, 1);
      }
      case WEEK ->
          // First day of the week (the week starts on Monday)
          day.with(TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY));
      case DAY -> day;
      default -> throw new ExpressionException(ErrorCode.INVALID_ARGUMENT, unit);
    };
  }

  /**
   * Returns the first year of the century or millennium that contains a year. As with EXTRACT, the
   * first century and millennium start in year 1, so the 21st century starts in 2001. Before year
   * 1, the century 0 goes from year -99 to year 0, the century -1 from year -199 to year -100.
   *
   * @param year the year
   * @param size the number of years, 100 for a century or 1000 for a millennium
   * @return the first year
   */
  static int firstYear(int year, int size) {
    return (year > 0) ? (year - 1) / size * size + 1 : year / size * size - size + 1;
  }

  /**
   * Returns the first year of the decade that contains a year. As with EXTRACT, the decade is the
   * year divided by 10 rounded down, so the decade -1 goes from year -10 to year -1.
   *
   * @param year the year
   * @return the first year
   */
  static int firstYearOfDecade(int year) {
    return Math.floorDiv(year, 10) * 10;
  }
}
