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

import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.time.temporal.Temporal;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.DateConversion;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * The number of seconds since 1970-01-01.
 *
 * @see ExtractFunction
 */
@FunctionPlugin
@NullMarked
public class EpochFunction extends Function {

  public EpochFunction() {
    super(
        "EPOCH",
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.DATE,
        OperatorCategory.DATE,
        "/docs/hop-user-manual/expression/epoch.html");
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    Temporal temporal = operands[0].getValue(Temporal.class);
    if (temporal == null) return null;
    // The epoch of a timestamp is its instant, the epoch of a date is nominal
    return switch (temporal) {
      case ZonedDateTime datetime -> datetime.toEpochSecond();
      case LocalDateTime datetime -> DateConversion.toEpochSecond(datetime);
      default -> throw new IllegalArgumentException("Not a temporal: " + temporal);
    };
  }
}
