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

import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.ZoneOffset;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** Build a date from its separate year, month and day fields. */
@FunctionPlugin
@NullMarked
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
  public @Nullable Object eval(final IExpression[] operands) {
    Long v0 = operands[0].getValue(Long.class);
    if (v0 == null) return null;

    Long v1 = operands[1].getValue(Long.class);
    if (v1 == null) return null;

    Long v2 = operands[2].getValue(Long.class);
    if (v2 == null) return null;

    // Add the months and the days to the first day of the year, so that values out of the
    // normal ranges overflow to the next or previous months and years, e.g. February 30
    try {
      LocalDate date =
          LocalDate.of(Math.toIntExact(v0), 1, 1)
              .plusMonths(Math.subtractExact(v1, 1))
              .plusDays(Math.subtractExact(v2, 1));

      return date.atStartOfDay().atZone(ZoneOffset.UTC);
    } catch (DateTimeException | ArithmeticException e) {
      throw new ExpressionException(ErrorCode.DATE_OVERFLOW, getName());
    }
  }
}
