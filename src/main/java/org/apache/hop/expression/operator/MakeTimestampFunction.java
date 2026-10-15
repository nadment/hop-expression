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
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Build a timestamp from its separate year, month, day, hour, minute, second[.fractional] fields.
 */
@FunctionPlugin
@NullMarked
public class MakeTimestampFunction extends Function {

  public MakeTimestampFunction() {
    super(
        "MAKE_TIMESTAMP",
        ReturnTypes.DATE_NULLABLE,
        OperandTypes.INTEGER_INTEGER_INTEGER_INTEGER_INTEGER_NUMBER,
        OperatorCategory.DATE,
        "/docs/make_timestamp.html");
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

    // The fractional part of the seconds, with the sign of the seconds
    long seconds = v5.longValue();
    long nanos = v5.remainder(BigDecimal.ONE).movePointRight(9).longValue();

    // Add each component to the start of the year, so that values out of the normal ranges
    // overflow to the next or previous units, e.g. February 30 or -0.5 second
    LocalDateTime datetime =
        LocalDateTime.of(v0.intValue(), 1, 1, 0, 0)
            .plusMonths(v1 - 1)
            .plusDays(v2 - 1)
            .plusHours(v3)
            .plusMinutes(v4)
            .plusSeconds(seconds)
            .plusNanos(nanos);

    return datetime.atZone(ZoneOffset.UTC);
  }
}
