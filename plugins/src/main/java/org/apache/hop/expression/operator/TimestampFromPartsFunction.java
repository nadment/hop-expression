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

import org.apache.hop.expression.Category;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.time.LocalDateTime;
import java.time.ZoneId;

/**
 * Build TIMESTAMP_FROM_PARTS(year, month, day, hour, minute, second [,nanosecond]) function
 */
@FunctionPlugin
public class TimestampFromPartsFunction extends Function {

  public TimestampFromPartsFunction() {
    super("TIMESTAMP_FROM_PARTS", ReturnTypes.DATE,
        OperandTypes.NUMERIC_NUMERIC_NUMERIC_NUMERIC_NUMERIC_NUMERIC_OPTIONAL_NUMERIC,
        Category.DATE, "/docs/timestamp_from_parts.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Long v0 = operands[0].getValue(Long.class);
    if (v0 == null)
      return null;

    Long v1 = operands[1].getValue(Long.class);
    if (v1 == null)
      return null;

    Long v2 = operands[2].getValue(Long.class);
    if (v2 == null)
      return null;

    Long v3 = operands[3].getValue(Long.class);
    if (v3 == null)
      return null;

    Long v4 = operands[4].getValue(Long.class);
    if (v4 == null)
      return null;

    Long v5 = operands[5].getValue(Long.class);
    if (v5 == null)
      return null;

    int year = v0.intValue();
    int month = v1.intValue();
    int day = v2.intValue();
    int hour = v3.intValue();
    int minute = v4.intValue();
    int second = v5.intValue();
    int nano = 0;

    if (operands.length == 7) {
      Long v6 = operands[6].getValue(Long.class);
      if (v6 == null)
        return null;
      nano = v6.intValue();
    }

    int monthsToAdd = 0;
    if (month < 1 || month > 12) {
      monthsToAdd = month - 1;
      month = 1;
    }

    int daysToAdd = 0;
    if (day < 1 || day > 31) {
      daysToAdd = day - 1;
      day = 1;
    }

    int hoursToAdd = 0;
    if (hour < 0 || hour > 23) {
      hoursToAdd = hour;
      hour = 0;
    }

    int minutesToAdd = 0;
    if (minute < 0 || minute > 59) {
      minutesToAdd = minute;
      minute = 0;
    }

    int secondsToAdd = 0;
    if (second < 0 || second > 59) {
      secondsToAdd = second;
      second = 0;
    }

    LocalDateTime datetime = LocalDateTime.of(year, month, day, hour, minute, second, nano);

    if (monthsToAdd != 0)
      datetime = datetime.plusMonths(monthsToAdd);
    if (daysToAdd != 0)
      datetime = datetime.plusDays(daysToAdd);
    if (hoursToAdd != 0)
      datetime = datetime.plusHours(hoursToAdd);
    if (minutesToAdd != 0)
      datetime = datetime.plusMinutes(minutesToAdd);
    if (secondsToAdd != 0)
      datetime = datetime.plusSeconds(secondsToAdd);

    return datetime.atZone(ZoneId.systemDefault());
  }
}
