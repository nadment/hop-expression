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

import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/** Returns number of weeks between two date. */
@FunctionPlugin
public class WeeksBetweenFunction extends Function {

  public WeeksBetweenFunction() {
    super(
        "WEEKS_BETWEEN",
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.DATE_DATE,
        OperatorCategory.DATE,
        "/docs/weeks_between.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    ZonedDateTime startDateTime = operands[0].getValue(ZonedDateTime.class);
    if (startDateTime == null) return null;
    ZonedDateTime endDateTime = operands[1].getValue(ZonedDateTime.class);
    if (endDateTime == null) return null;

    return startDateTime.until(endDateTime, ChronoUnit.WEEKS);
  }
}
