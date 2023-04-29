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
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;

/**
 *  Returns the difference in time unit between two date or timestamp. 
 */
@FunctionPlugin
public class DateDiffFunction extends Function {

  public DateDiffFunction() {
    super("DATE_DIFF", ReturnTypes.INTEGER, OperandTypes.TIMEUNIT_DATE_DATE, OperatorCategory.DATE,
        "/docs/date_diff.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {

    TimeUnit unit = operands[0].getValue(context, TimeUnit.class);

    ZonedDateTime startDateTime = operands[1].getValue(context, ZonedDateTime.class);
    if (startDateTime == null)
      return null;
    ZonedDateTime endDateTime = operands[2].getValue(context, ZonedDateTime.class);
    if (endDateTime == null)
      return null;

    switch (unit) {
      case MILLENNIUM:
        return startDateTime.until(endDateTime, ChronoUnit.MILLENNIA);
      case CENTURY:
        return startDateTime.until(endDateTime, ChronoUnit.CENTURIES);
      case DECADE:
        return startDateTime.until(endDateTime, ChronoUnit.DECADES);
      case YEAR:
          return startDateTime.until(endDateTime, ChronoUnit.YEARS);
      case MONTH:
        return startDateTime.until(endDateTime, ChronoUnit.MONTHS);
      case QUARTER:
        long months = startDateTime.until(endDateTime, ChronoUnit.MONTHS);        
        return Long.valueOf(months / 3);
      case WEEK:
        return startDateTime.until(endDateTime, ChronoUnit.WEEKS);
      case DAY:
       return startDateTime.until(endDateTime, ChronoUnit.DAYS);
      case HOUR:
        return startDateTime.until(endDateTime, ChronoUnit.HOURS);
      case MINUTE:
        return startDateTime.until(endDateTime, ChronoUnit.MINUTES);
      case SECOND:
        return startDateTime.until(endDateTime, ChronoUnit.SECONDS);
      case MILLISECOND:
        return startDateTime.until(endDateTime, ChronoUnit.MILLIS);
      case MICROSECOND:
        return startDateTime.until(endDateTime, ChronoUnit.MICROS);
      case NANOSECOND:
        return startDateTime.until(endDateTime, ChronoUnit.NANOS);
      default:
        throw new ExpressionException(ExpressionError.ILLEGAL_ARGUMENT, unit);
    }
  }
}
