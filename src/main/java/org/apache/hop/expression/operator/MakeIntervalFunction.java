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
import java.math.RoundingMode;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Build an interval from its separate year, month, day, hour, minute, second[.fractional] fields.
 */
@FunctionPlugin
@NullMarked
public class MakeIntervalFunction extends Function {

  public MakeIntervalFunction() {
    super(
        "MAKE_INTERVAL",
        ReturnTypes.INTERVAL_NULLABLE,
        OperandTypes.INTEGER_INTEGER_INTEGER_INTEGER_INTEGER_NUMBER,
        OperatorCategory.DATE,
        "/docs/make_interval.html");
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
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

    try {
      // Signed months, days and time parts, each value can be negative like PostgreSQL
      long months = Math.addExact(Math.multiplyExact(v0, 12), v1);
      long seconds =
          Math.addExact(
              Math.addExact(Math.multiplyExact(v3, 3600), Math.multiplyExact(v4, 60)),
              v5.setScale(0, RoundingMode.DOWN).longValueExact());
      long nanos =
          v5.remainder(BigDecimal.ONE).movePointRight(9).setScale(0, RoundingMode.DOWN).longValue();

      return Interval.ofSigned(months, v2, seconds, nanos);
    } catch (ArithmeticException e) {
      throw new ExpressionException(ErrorCode.INTERVAL_OVERFLOW, getName());
    }
  }
}
