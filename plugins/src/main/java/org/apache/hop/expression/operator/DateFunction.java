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

import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Coerse;
import java.time.LocalDate;
import java.time.ZoneId;

/**
 * Build DATE(YYYY,MM,DD) function
 */
@FunctionPlugin
public class DateFunction extends Function {

  public DateFunction() {
    super("DATE", true, ReturnTypes.DATE, OperandTypes.NUMERIC_NUMERIC_NUMERIC,
        "i18n::Operator.Category.Date", "/docs/date.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    Object v0 = operands[0].getValue(context);
    if (v0 == null)
      return null;

    Object v1 = operands[1].getValue(context);
    if (v1 == null)
      return null;

    Object v2 = operands[2].getValue(context);
    if (v2 == null)
      return null;

    int year = Coerse.toInteger(v0).intValue();
    int month = Coerse.toInteger(v1).intValue();
    int day = Coerse.toInteger(v2).intValue();

    int monthsToAdd = 0;
    if (month < 1) {
      monthsToAdd = month;
      month = 1;
    } else if (month > 12) {
      monthsToAdd = month - 1;
      month = 1;
    }

    int daysToAdd = 0;
    if (day < 1 || day > 31) {
      daysToAdd = day;
      day = 1;
    }

    LocalDate localDate = LocalDate.of(year, month, day);
    if (monthsToAdd != 0)
      localDate = localDate.plusMonths(monthsToAdd);
    if (daysToAdd != 0)
      localDate = localDate.plusDays(daysToAdd);

    return localDate.atStartOfDay().atZone(ZoneId.systemDefault());
  }

}
