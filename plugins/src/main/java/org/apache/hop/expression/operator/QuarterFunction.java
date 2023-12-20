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
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.time.ZonedDateTime;
import java.time.temporal.IsoFields;

/**
 * Quarter of the year (number from 1-4).
 */
@FunctionPlugin
public class QuarterFunction extends Function {

  public QuarterFunction() {
    super("QUARTER", ReturnTypes.INTEGER_NULLABLE, OperandTypes.TEMPORAL, OperatorCategory.DATE,
        "/docs/quarter.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    ZonedDateTime value = operands[0].getValue(ZonedDateTime.class);
    if (value == null)
      return null;
    return Long.valueOf(value.get(IsoFields.QUARTER_OF_YEAR));
  }
}
