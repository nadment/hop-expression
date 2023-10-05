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
import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.UserDefinedFunction;
import org.apache.hop.expression.exception.ExpressionException;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoField;
import java.time.temporal.IsoFields;

/**
 * Extracts the specified date or time part from a date, time, or timestamp.
 * 
 * Time unit: DECADE | YEAR | MONTH | WEEK | DAY | HOUR | MINUTE | SECOND...
 */

public class ExtractDateFunction extends ExtractFunction {
  public static final ExtractDateFunction INSTANCE = new ExtractDateFunction();
  
  public ExtractDateFunction() {
    super();
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    // Replace EXTRACT from date with the corresponding function YEAR, DAY, HOUR... only if without time zone
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
        throw new IllegalArgumentException(ExpressionError.ILLEGAL_ARGUMENT.message(unit));
    }
  }
}