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

import java.time.LocalDate;
import java.time.ZoneOffset;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/** Build a date from its separate year, month and day fields. */
@FunctionPlugin
public class MakeDateFunction extends Function {

  public MakeDateFunction() {
    super(
        "MAKE_DATE",
        ReturnTypes.DATE_NULLABLE,
        OperandTypes.INTEGER_INTEGER_INTEGER,
        OperatorCategory.DATE,
        "/docs/make_date.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Long v0 = operands[0].getValue(Long.class);
    if (v0 == null) return null;

    Long v1 = operands[1].getValue(Long.class);
    if (v1 == null) return null;

    Long v2 = operands[2].getValue(Long.class);
    if (v2 == null) return null;

    int year = v0.intValue();
    int month = v1.intValue();
    int day = v2.intValue();

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

    LocalDate date = LocalDate.of(year, month, day);
    if (monthsToAdd != 0) date = date.plusMonths(monthsToAdd);
    if (daysToAdd != 0) date = date.plusDays(daysToAdd);

    return date.atStartOfDay().atZone(ZoneOffset.UTC);
  }
}
