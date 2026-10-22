/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.apache.hop.expression.operator;

import java.io.StringWriter;
import java.util.Objects;
import org.apache.hop.expression.AggregateFunction;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IAggregateProcessor;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Returns the nth value over a group of rows.
 *
 * <p><code>NTH_VALUE(expression, offset) [ IGNORE NULLS | RESPECT NULLS ]</code>
 *
 * <p>The default is RESPECT NULLS.
 */
@FunctionPlugin
@NullMarked
public class NthValueFunction extends AggregateFunction {
  public static final NthValueFunction NTH_VALUE_RESPECT_NULLS = new NthValueFunction(false);
  public static final NthValueFunction NTH_VALUE_IGNORE_NULLS = new NthValueFunction(true);

  private final boolean ignoreNulls;

  public NthValueFunction() {
    this(false);
  }

  public NthValueFunction(boolean ignoreNulls) {
    super("NTH_VALUE", ReturnTypes.ARG0, OperandTypes.ANY_INTEGER, "/docs/nth_value.html");

    this.ignoreNulls = ignoreNulls;
  }

  /** {@inheritDoc} The null treatment is not an operand, it distinguishes the functions. */
  @Override
  public boolean equals(@Nullable Object other) {
    return other instanceof NthValueFunction function
        && ignoreNulls == function.ignoreNulls
        && super.equals(other);
  }

  @Override
  public int hashCode() {
    return Objects.hash(super.hashCode(), ignoreNulls);
  }

  @Override
  public IAggregateProcessor createProcessor(IExpressionContext context, IExpression[] operands) {
    Long offset = operands[1].getValue(Long.class);
    // The offset is mandatory and starts at 1
    if (offset == null || offset <= 0) {
      throw new ExpressionException(ErrorCode.ARGUMENT_OUT_OF_RANGE, 2, operands[1]);
    }
    return (ignoreNulls)
        ? new NthValueIgnoreNullsProcessor(offset)
        : new NthValueRespectNullsProcessor(offset);
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    super.unparse(writer, operands);
    if (ignoreNulls) {
      writer.append(" IGNORE NULLS");
    }
  }
}
