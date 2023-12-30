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

import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.UserDefinedFunction;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.Interval;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeFamily;
import java.io.StringWriter;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoField;
import java.time.temporal.IsoFields;

/**
 * Extracts the specified time unit from a date, timestamp or interval.
 * 
 * Time unit: DECADE | YEAR | MONTH | WEEK | DAY | HOUR | MINUTE | SECOND...
 */
@FunctionPlugin(names = "DATE_PART")
public class ExtractFunction extends Function {
  public static final ExtractFunction ExtractDateFunction = new ExtractDate();
  public static final ExtractInterval ExtractIntervalFunction = new ExtractInterval();

  public ExtractFunction() {
    super("EXTRACT", ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.TIMEUNIT_TEMPORAL.or(OperandTypes.TIMEUNIT_INTERVAL), OperatorCategory.DATE,
        "/docs/extract.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    Type type = call.getOperand(1).getType();
    if (type.isFamily(TypeFamily.INTERVAL)) {
      return new Call(ExtractIntervalFunction, call.getOperands());
    }
    return new Call(ExtractDateFunction, call.getOperands());
  }

  protected static int millennium(int year) {
    return year > 0 ? (year + 999) / 1000 : year / 1000;
  }

  protected static int century(int year) {
    return year > 0 ? (year + 99) / 100 : year / 100;
  }

  protected static int decade(int year) {
    return year >= 0 ? year / 10 : (year - 9) / 10;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append("EXTRACT(");
    operands[0].unparse(writer);
    writer.append(" FROM ");
    operands[1].unparse(writer);
    writer.append(')');
  }

  /**
   * Extracts the specified date or time part from a date, time, or timestamp.
   */
  private static final class ExtractDate extends ExtractFunction {

    @Override
    public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
      // Replace EXTRACT from date with the corresponding function YEAR, DAY, HOUR... only if
      // without time zone
      TimeUnit unit = call.getOperand(0).getValue(TimeUnit.class);
      Function function = FunctionRegistry.getFunction(unit.name());
      if (function != null && !(function instanceof UserDefinedFunction)) {
        return new Call(function, call.getOperand(1));
      }

      return call;
    }

    @Override
    public Object eval(final IExpression[] operands) {

      TimeUnit unit = operands[0].getValue(TimeUnit.class);

      ZonedDateTime datetime = operands[1].getValue(ZonedDateTime.class);
      if (datetime == null)
        return null;

      switch (unit) {
        case DAY:
          return Long.valueOf(datetime.getDayOfMonth());
        case DAYOFYEAR:
          return Long.valueOf(datetime.getDayOfYear());
        case DAYOFWEEK:
          int dow = datetime.getDayOfWeek().getValue() + 1;
          if (dow == 8)
            dow = 1;
          return Long.valueOf(dow);
        case ISODAYOFWEEK:
          return Long.valueOf(datetime.getDayOfWeek().getValue());
        case WEEK:
          return Long.valueOf(datetime.get(ChronoField.ALIGNED_WEEK_OF_YEAR));
        case ISOWEEK:
          return Long.valueOf(datetime.get(IsoFields.WEEK_OF_WEEK_BASED_YEAR));
        case WEEKOFMONTH:
          return Long.valueOf(datetime.get(ChronoField.ALIGNED_WEEK_OF_MONTH));
        case MONTH:
          return Long.valueOf(datetime.getMonthValue());
        case QUARTER:
          return Long.valueOf(datetime.get(IsoFields.QUARTER_OF_YEAR));
        case YEAR:
          return Long.valueOf(datetime.getYear());
        case ISOYEAR:
          return Long.valueOf(datetime.get(IsoFields.WEEK_BASED_YEAR));
        case DECADE:
          return Long.valueOf(decade(datetime.getYear()));
        case CENTURY:
          return Long.valueOf(century(datetime.getYear()));
        case MILLENNIUM:
          return Long.valueOf(millennium(datetime.getYear()));
        case HOUR:
          return Long.valueOf(datetime.getHour());
        case MINUTE:
          return Long.valueOf(datetime.getMinute());
        case SECOND:
          return Long.valueOf(datetime.getSecond());
        case MILLISECOND:
          return Long.valueOf(datetime.get(ChronoField.MILLI_OF_SECOND));
        case MICROSECOND:
          return Long.valueOf(datetime.get(ChronoField.MICRO_OF_SECOND));
        case NANOSECOND:
          return Long.valueOf(datetime.getNano());
        case EPOCH:
          return datetime.toEpochSecond();
        case TIMEZONE_HOUR:
          return Long.valueOf(datetime.getOffset().getTotalSeconds() / (60 * 60));
        case TIMEZONE_MINUTE:
          return Long.valueOf((datetime.getOffset().getTotalSeconds() / 60) % 60);
        default:
          throw new IllegalArgumentException(ErrorCode.ILLEGAL_ARGUMENT.message(unit));
      }
    }
  }

  /**
   * Extracts the specified time unit from a interval.
   */
  private static final class ExtractInterval extends ExtractFunction {

    @Override
    public Object eval(final IExpression[] operands) {

      TimeUnit unit = operands[0].getValue(TimeUnit.class);

      Interval interval = operands[1].getValue(Interval.class);
      if (interval == null)
        return null;

      switch (unit) {
        case DAY:
          return interval.getSign() * interval.getDays();
        case MONTH:
          return interval.getSign() * interval.getMonths();
        case YEAR:
          return interval.getSign() * interval.getYears();
        case DECADE:
          return interval.getSign() * interval.getYears() / 10;
        case CENTURY:
          return interval.getSign() * interval.getYears() / 100;
        case MILLENNIUM:
          return interval.getSign() * interval.getYears() / 1000;
        case HOUR:
          return interval.getSign() * interval.getHours();
        case MINUTE:
          return interval.getSign() * interval.getMinutes();
        case SECOND:
          return interval.getSign() * interval.getSeconds();
        case MILLISECOND:
          return interval.getSign() * interval.getMilliseconds();
        case MICROSECOND:
          return interval.getSign() * interval.getMicroseconds();
        case NANOSECOND:
          return interval.getSign() * interval.getNanoseconds();
        default:
          throw new IllegalArgumentException(ErrorCode.ILLEGAL_ARGUMENT.message(unit));
      }
    }
  }

}
