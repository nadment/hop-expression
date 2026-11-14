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
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.type.IReturnTypeInference;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

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
@NullMarked
public class DateAddFunction extends Function {

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

  /**
   * The result type is TIMESTAMP if the date is a TIMESTAMP, otherwise DATE. With a unit smaller
   * than a second, the fractional seconds have 9 digits, the added units can have more digits than
   * the date.
   */
  private static final IReturnTypeInference RETURN_TYPE =
      ReturnTypes.TEMPORAL_NULLABLE.andThen(
          (call, type) ->
              call.getOperand(0) instanceof Literal literal
                      && (literal.getValue() == TimeUnit.MILLISECOND
                          || literal.getValue() == TimeUnit.MICROSECOND)
                  ? Types.withFractionalPrecision(
                      type, TypeName.TIMESTAMP.getMaxPrecision(), type.isNullable())
                  : type);

  public DateAddFunction() {
    super(
        "DATE_ADD",
        RETURN_TYPE,
        OperandTypes.TIMEUNIT_INTEGER_DATE,
        OperatorCategory.DATE,
        "/docs/hop-user-manual/expression/date_add.html");
  }

  /**
   * {@inheritDoc}
   *
   * <p>A time unit that is not supported, like ISOWEEK, is rejected with the position of the call.
   */
  @Override
  protected void validateOperands(IExpressionContext context, Call call)
      throws ExpressionException {
    super.validateOperands(context, call);
    validateTimeUnit(call, 0, SUPPORTED_TIME_UNITS, ErrorCode.INVALID_ARGUMENT);
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    TimeUnit unit = call.getOperand(0).getValue(TimeUnit.class);
    return switch (unit) {
        // Units without their own function, evaluated by this function
      case MILLENNIUM, CENTURY, DECADE, MILLISECOND, MICROSECOND ->
          // Simplify arithmetic DATE_ADD(DECADE,0,A) → A
          Literal.isZero(call.getOperand(1)) ? call.getOperand(2) : call;
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
      case null, default -> throw new ExpressionException(ErrorCode.INVALID_ARGUMENT, unit);
    };
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    Long value = operands[1].getValue(Long.class);
    if (value == null) return null;
    Temporal temporal = operands[2].getValue(Temporal.class);
    if (temporal == null) return null;

    // The other units are compiled to their own function, like ADD_YEARS
    TimeUnit unit = operands[0].getValue(TimeUnit.class);
    ChronoUnit chronoUnit =
        switch (unit) {
          case MILLENNIUM -> ChronoUnit.MILLENNIA;
          case CENTURY -> ChronoUnit.CENTURIES;
          case DECADE -> ChronoUnit.DECADES;
          case MILLISECOND -> ChronoUnit.MILLIS;
          case MICROSECOND -> ChronoUnit.MICROS;
          case null, default -> throw new ExpressionException(ErrorCode.INVALID_ARGUMENT, unit);
        };
    try {
      return temporal.plus(value, chronoUnit);
    } catch (DateTimeException | ArithmeticException e) {
      throw new ExpressionException(ErrorCode.TEMPORAL_OVERFLOW, getName());
    }
  }
}
