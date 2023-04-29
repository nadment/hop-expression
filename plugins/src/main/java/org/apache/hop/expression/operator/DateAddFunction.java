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

/**
 * Adds or subtracts a specified number of time unit to a date or timestamp
 */
@FunctionPlugin
public class DateAddFunction extends Function {

  public DateAddFunction() {
    super("DATE_ADD", ReturnTypes.DATE, OperandTypes.TIMEUNIT_NUMERIC_DATE, OperatorCategory.DATE,
        "/docs/date_add.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {

    TimeUnit unit = operands[0].getValue(context, TimeUnit.class);

    Long value = operands[1].getValue(context, Long.class);
    if (value == null)
      return null;

    ZonedDateTime datetime = operands[2].getValue(context, ZonedDateTime.class);
    if (datetime == null)
      return null;

    switch (unit) {
      case YEAR:
        return datetime.plusYears(value);
      case MONTH:
        return datetime.plusMonths(value);
      case WEEK:
        return datetime.plusWeeks(value);
      case DAY:
        return datetime.plusDays(value);
      case HOUR:
        return datetime.plusHours(value);
      case MINUTE:
        return datetime.plusMinutes(value);
      case SECOND:
        return datetime.plusSeconds(value);
      case NANOSECOND:
        return datetime.plusNanos(value);
      default:
        throw new ExpressionException(ExpressionError.ILLEGAL_ARGUMENT, unit);
    }
  }
}
