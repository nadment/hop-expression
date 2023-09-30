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
import java.math.BigDecimal;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;

/**
 * Returns number of months between two date.
 * 
 * The difference is calculated based on 31 days per month.
 */
@FunctionPlugin
public class MonthsBetweenFunction extends Function {

  public MonthsBetweenFunction() {
    super("MONTHS_BETWEEN", ReturnTypes.INTEGER, OperandTypes.TEMPORAL_TEMPORAL, Category.DATE,
        "/docs/months_between.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    ZonedDateTime startDateTime = operands[0].getValue(ZonedDateTime.class);
    if (startDateTime == null)
      return null;
    ZonedDateTime endDateTime = operands[1].getValue(ZonedDateTime.class);
    if (endDateTime == null)
      return null;

    long days = startDateTime.until(endDateTime, ChronoUnit.DAYS);

    // All months have 31 days
    return BigDecimal.valueOf(days / 31d);
  }
}
