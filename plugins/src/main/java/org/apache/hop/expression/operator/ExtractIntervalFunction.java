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

import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.TimeUnit;

/**
 * Extracts the specified time unit from a interval.
 * 
 * Time unit: YEAR | MONTH | WEEK | DAY | HOUR | MINUTE | SECOND...
 */

public class ExtractIntervalFunction extends ExtractFunction {
  public static final ExtractIntervalFunction INSTANCE = new ExtractIntervalFunction();

  public ExtractIntervalFunction() {
    super();
  }

  @Override
  public Object eval(final IExpression[] operands) {

    TimeUnit unit = operands[0].getValue(TimeUnit.class);

    Interval interval = operands[1].getValue(Interval.class);
    if (interval == null)
      return null;

    switch (unit) {
      case DAY:
        return interval.getSign()*interval.getDays();
      case MONTH:
        return interval.getSign()*interval.getMonths();
      case YEAR:
        return interval.getSign()*interval.getYears();
      case DECADE:
        return interval.getSign()*interval.getYears() / 10;
      case CENTURY:
        return interval.getSign()*interval.getYears() / 100;
      case MILLENNIUM:
        return interval.getSign()*interval.getYears() / 1000;
      case HOUR:
        return interval.getSign()*interval.getHours();
      case MINUTE:
        return interval.getSign()*interval.getMinutes();
      case SECOND:
        return interval.getSign()*interval.getSeconds();
      case MILLISECOND:
        return interval.getSign()*interval.getMilliseconds();
      case MICROSECOND:
        return interval.getSign()*interval.getMicroseconds();
      case NANOSECOND:
        return interval.getSign()*interval.getNanoseconds();
      default:
        throw new IllegalArgumentException(ExpressionError.ILLEGAL_ARGUMENT.message(unit));
    }
  }
}
