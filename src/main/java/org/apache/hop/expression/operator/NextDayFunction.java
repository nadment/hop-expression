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

import java.time.DayOfWeek;
import java.time.ZonedDateTime;
import java.time.temporal.TemporalAdjusters;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/** Returns the date of the first specified day of week that occurs after the input date. */
@FunctionPlugin
public class NextDayFunction extends Function {

  public NextDayFunction() {
    super(
        "NEXT_DAY",
        ReturnTypes.DATE_NULLABLE,
        OperandTypes.DATE_STRING,
        OperatorCategory.DATE,
        "/docs/next_day.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    ZonedDateTime value = operands[0].getValue(ZonedDateTime.class);
    if (value == null) return null;
    String dow = operands[1].getValue(String.class);
    if (dow == null) return null;

    DayOfWeek dayofweek;
    try {
      dayofweek = DayOfWeek.valueOf(dow.toUpperCase());
    } catch (Exception e) {
      throw new ExpressionException(ErrorCode.INVALID_ARGUMENT, dow);
    }

    return value.with(TemporalAdjusters.next(dayofweek));
  }
}
