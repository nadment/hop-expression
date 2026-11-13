/*
 *
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

import java.io.StringWriter;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.time.temporal.Temporal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.IExpressionParser;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.Token;
import org.apache.hop.expression.Token.Id;
import org.apache.hop.expression.UserDefinedFunction;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Extracts the specified time unit from a date, timestamp or interval.
 *
 * <p>Time unit: DECADE | YEAR | MONTH | WEEK | DAY | HOUR | MINUTE | SECOND...
 */
@FunctionPlugin(names = "DATE_PART")
@NullMarked
public class ExtractFunction extends Function {

  /** The time units that are a part of an interval, the only ones extracted from an interval. */
  public static final Set<TimeUnit> INTERVAL_TIME_UNITS =
      Collections.unmodifiableSet(
          EnumSet.of(
              TimeUnit.MILLENNIUM,
              TimeUnit.CENTURY,
              TimeUnit.DECADE,
              TimeUnit.YEAR,
              TimeUnit.MONTH,
              TimeUnit.DAY,
              TimeUnit.HOUR,
              TimeUnit.MINUTE,
              TimeUnit.SECOND,
              TimeUnit.MILLISECOND,
              TimeUnit.MICROSECOND,
              TimeUnit.NANOSECOND));

  /** The time units extracted from a date without time zone, all but the time zone parts. */
  private static final Set<TimeUnit> DATE_TIME_UNITS =
      Collections.unmodifiableSet(
          EnumSet.complementOf(
              EnumSet.of(TimeUnit.TIMEZONE_HOUR, TimeUnit.TIMEZONE_MINUTE, TimeUnit.TIMEZONE)));

  public ExtractFunction() {
    super(
        "EXTRACT",
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.TIMEUNIT_DATE.or(OperandTypes.TIMEUNIT_INTERVAL),
        OperatorCategory.DATE,
        "/docs/hop-user-manual/expression/extract.html");
  }

  @Override
  public Set<String> getKeywords() {
    return Set.of("FROM");
  }

  /**
   * Parses <code>EXTRACT(part FROM expression)</code>, the alias DATE_PART has the syntax of a
   * conventional function <code>DATE_PART(part, expression)</code>.
   */
  @Override
  public IExpression parse(IExpressionParser parser, Token token) throws ExpressionException {
    if (!"EXTRACT".equals(token.text())) {
      return super.parse(parser, token);
    }

    List<IExpression> operands = new ArrayList<>();

    // Check argument will detect an error later
    if (parser.accept(Id.RPARENTHESIS)) {
      return new Call(token.start(), this, operands);
    }
    parser.expectNext(ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());

    operands.add(parser.parseLiteralTimeUnit(parser.next()));

    // Check argument will detect an error later
    if (parser.accept(Id.RPARENTHESIS)) {
      return new Call(token.start(), this, operands);
    }

    parser.expectKeyword("FROM", ErrorCode.SYNTAX_ERROR_FUNCTION, this.getName());
    parser.expectNext(ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());

    operands.add(parser.parseExpression());

    parser.expect(Id.RPARENTHESIS, ErrorCode.MISSING_RIGHT_PARENTHESIS);

    return new Call(token.start(), this, operands);
  }

  /**
   * {@inheritDoc}
   *
   * <p>A time unit that is not a part of an interval, like EPOCH or WEEK, or a time zone part of a
   * date without time zone, is rejected when the call is validated, not when a row is evaluated.
   *
   * @throws ExpressionException with {@link ErrorCode#INVALID_ARGUMENT} if the time unit is not a
   *     part of the interval or of the date
   */
  @Override
  protected void validateOperands(IExpressionContext context, Call call)
      throws ExpressionException {
    super.validateOperands(context, call);

    if (call.getOperandCount() == 2) {
      Type type = call.getOperand(1).getType();
      if (Types.isInterval(type)) {
        validateTimeUnit(call, 0, INTERVAL_TIME_UNITS, ErrorCode.INVALID_ARGUMENT);
      } else if (type.isName(TypeName.DATE)) {
        // A date without time zone has no time zone part
        validateTimeUnit(call, 0, DATE_TIME_UNITS, ErrorCode.INVALID_ARGUMENT);
      }
    }
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append("EXTRACT(");
    operands[0].unparse(writer, 0, 0);
    writer.append(" FROM ");
    operands[1].unparse(writer, 0, 0);
    writer.append(')');
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    Type type = call.getOperand(1).getType();
    if (Types.isInterval(type)) {
      return new Call(IntervalExtractFunction.INSTANCE, call.getOperands());
    }
    return new Call(DateExtractFunction.INSTANCE, call.getOperands());
  }

  /** Extracts the specified date or time part from a date, time, or timestamp. */
  public static final class DateExtractFunction extends ExtractFunction {
    public static final DateExtractFunction INSTANCE = new DateExtractFunction();

    @Override
    public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
      // Replace EXTRACT from date with the corresponding function YEAR, DAY, HOUR... only if
      // without a time zone
      TimeUnit unit = call.getOperand(0).getValue(TimeUnit.class);
      if (unit == null) {
        throw new ExpressionException(ErrorCode.INVALID_TIME_UNIT, call.getOperand(0));
      }
      Function function = FunctionRegistry.getFunction(unit.name());
      if (function != null && !(function instanceof UserDefinedFunction)) {
        return new Call(function, call.getOperand(1));
      }

      return call;
    }

    @Override
    public @Nullable Object eval(final IExpression[] operands) {
      TimeUnit unit = operands[0].getValue(TimeUnit.class);
      Temporal temporal = operands[1].getValue(Temporal.class);
      if (temporal == null) return null;

      // A date without time zone has no time zone part
      return switch (temporal) {
        case ZonedDateTime datetime -> unit.extract(datetime);
        case LocalDateTime datetime -> unit.extract(datetime);
        default -> throw new IllegalArgumentException("Not a date-time: " + temporal);
      };
    }
  }

  /** Extracts the specified time unit from an interval. */
  public static final class IntervalExtractFunction extends ExtractFunction {
    public static final IntervalExtractFunction INSTANCE = new IntervalExtractFunction();

    @Override
    public @Nullable Object eval(final IExpression[] operands) {
      TimeUnit unit = operands[0].getValue(TimeUnit.class);
      Interval interval = operands[1].getValue(Interval.class);
      if (interval == null) return null;

      return unit.extract(interval);
    }
  }
}
