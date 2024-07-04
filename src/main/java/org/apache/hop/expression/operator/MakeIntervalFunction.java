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

import java.math.BigDecimal;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.Interval;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Build a interval from its separate year, month, day, hour, minute, second[.fractional]) fields.
 */
@FunctionPlugin
public class MakeIntervalFunction extends Function {

  public MakeIntervalFunction() {
    super(
        "MAKE_INTERVAL",
        ReturnTypes.INTERVAL_NULLABLE,
        OperandTypes.NUMERIC_NUMERIC_NUMERIC_NUMERIC_NUMERIC_NUMERIC,
        OperatorCategory.DATE,
        "/docs/make_interval.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Long v0 = operands[0].getValue(Long.class);
    if (v0 == null) return null;

    Long v1 = operands[1].getValue(Long.class);
    if (v1 == null) return null;

    Long v2 = operands[2].getValue(Long.class);
    if (v2 == null) return null;

    Long v3 = operands[3].getValue(Long.class);
    if (v3 == null) return null;

    Long v4 = operands[4].getValue(Long.class);
    if (v4 == null) return null;

    BigDecimal v5 = operands[5].getValue(BigDecimal.class);
    if (v5 == null) return null;

    int years = v0.intValue();
    int months = v1.intValue();
    int days = v2.intValue();
    int hours = v3.intValue();
    int minutes = v4.intValue();
    int seconds = v5.intValue();
    int nanos = v5.remainder(BigDecimal.ONE).movePointRight(9).intValue();

    return Interval.of(years, months, days, hours, minutes, seconds, nanos);
  }
}
