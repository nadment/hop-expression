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
import java.time.ZonedDateTime;

/**
 * Adds or subtracts a specified number of hours to a date or timestamp
 */
@FunctionPlugin
public class AddHoursFunction extends Function {

  public AddHoursFunction() {
    super("ADD_HOURS", true, ReturnTypes.DATE, OperandTypes.DATE_NUMERIC,
        "i18n::Operator.Category.Date", "/docs/add_hours.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    ZonedDateTime value = Coerse.toDateTime(operands[0].getValue(context));
    if (value == null)
      return value;
    Object hours = operands[1].getValue(context);
    if (hours == null)
      return null;

    return value.plusHours(Coerse.toInteger(hours));
  }
}
