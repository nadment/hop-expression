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
import java.time.temporal.IsoFields;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Week of the year ISO semantics (number from 1-53).
 *
 * @see WeekFunction
 */
@FunctionPlugin
public class IsoWeekFunction extends Function {

  public IsoWeekFunction() {
    super(
        "ISOWEEK",
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.DATE,
        OperatorCategory.DATE,
        "/docs/isoweek.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    ZonedDateTime value = operands[0].getValue(ZonedDateTime.class);
    if (value == null) return null;
    return Long.valueOf(value.get(IsoFields.WEEK_OF_WEEK_BASED_YEAR));
  }
}
