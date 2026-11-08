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

import static java.util.Objects.requireNonNull;

import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.time.temporal.Temporal;
import org.apache.hop.expression.Attribute;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.TimestampConversion;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Returns the interval between two dates, in years, months and days rather than in days only.
 *
 * <p>{@code AGE(date1, date2)} subtracts date2 from date1, {@code AGE(date)} subtracts the date
 * from the current date at midnight.
 *
 * <pre>{@code
 * AGE(DATE '2024-03-15', DATE '1980-06-23') → 43 years 8 months 22 days
 * }</pre>
 */
@FunctionPlugin
@NullMarked
public class AgeFunction extends Function {

  public AgeFunction() {
    super(
        "AGE",
        ReturnTypes.INTERVAL_NULLABLE,
        OperandTypes.DATE.or(OperandTypes.DATE_DATE),
        OperatorCategory.DATE,
        "/docs/hop-user-manual/expression/age.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    // AGE(date) → AGE(CURRENT_DATE, date), the current date at midnight, always set by the context
    if (call.getOperandCount() == 1) {
      IExpression currentDate = Literal.of(requireNonNull(Attribute.CURRENT_DATE.getDate(context)));
      return new Call(call.getPosition(), this, currentDate, call.getOperand(0));
    }
    return call;
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    Temporal value1 = operands[0].getValue(Temporal.class);
    if (value1 == null) return null;
    Temporal value2 = operands[1].getValue(Temporal.class);
    if (value2 == null) return null;

    // Two dates without time zone are compared with their local fields, otherwise the fields are
    // in the time zone of the first operand, a date without time zone is in the system time zone
    LocalDateTime end;
    LocalDateTime start;
    if (value1 instanceof LocalDateTime date1 && value2 instanceof LocalDateTime date2) {
      end = date1;
      start = date2;
    } else {
      ZonedDateTime zoned1 = TimestampConversion.toZoned(value1);
      end = zoned1.toLocalDateTime();
      start =
          TimestampConversion.toZoned(value2)
              .withZoneSameInstant(zoned1.getZone())
              .toLocalDateTime();
    }

    return age(end, start);
  }

  /**
   * Subtracts the fields of the start from the fields of the end, with a borrow from the next field
   * when a field is negative. The days are borrowed from the month of the earlier date.
   *
   * @param end the date to subtract from
   * @param start the date to subtract
   * @return the interval, negative if the end is before the start
   */
  static Interval age(LocalDateTime end, LocalDateTime start) {
    boolean negative = end.isBefore(start);
    LocalDateTime later = negative ? start : end;
    LocalDateTime earlier = negative ? end : start;

    int nanos = later.getNano() - earlier.getNano();
    int seconds = later.getSecond() - earlier.getSecond();
    int minutes = later.getMinute() - earlier.getMinute();
    int hours = later.getHour() - earlier.getHour();
    int days = later.getDayOfMonth() - earlier.getDayOfMonth();
    int months = later.getMonthValue() - earlier.getMonthValue();
    int years = later.getYear() - earlier.getYear();

    if (nanos < 0) {
      nanos += 1_000_000_000;
      seconds--;
    }
    if (seconds < 0) {
      seconds += 60;
      minutes--;
    }
    if (minutes < 0) {
      minutes += 60;
      hours--;
    }
    if (hours < 0) {
      hours += 24;
      days--;
    }
    // Borrow the number of days of the month of the earlier date, the day of the earlier date is
    // at most this number of days, so the days are no longer negative
    if (days < 0) {
      days += earlier.toLocalDate().lengthOfMonth();
      months--;
    }
    if (months < 0) {
      months += 12;
      years--;
    }

    Interval interval = Interval.of(years, months, days, hours, minutes, seconds, nanos);
    return negative ? interval.negate() : interval;
  }
}
