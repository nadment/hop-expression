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

import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeFamily;
import org.apache.hop.expression.type.Types;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.NumberFormat;

/**
 * Converts a value of one data type into another data type <code>
 * TRY_CAST(value AS type [FORMAT format])</code>.
 *
 * @see CastFunction
 * @see CastOperator
 */
@FunctionPlugin
public class TryCastFunction extends CastFunction {

  public TryCastFunction() {
    super("TRY_CAST");
  }

  @Override
  public IExpression compile(final IExpressionContext context, Call call)
      throws ExpressionException {

    // Cast null value
    if (call.getOperand(0).isNull()) {
      return new Literal(null, call.getType());
    }

    // When cast without format
    if (call.getOperandCount() == 2) {

      // Cast constant value
      if (call.getOperand(0).isConstant()) {
        return new Literal(call.getValue(), call.getType());
      }

      // Remove loss-less cast
      if (Types.isLosslessCast(call.getOperand(0).getType(), call.getType())) {
        return call.getOperand(0);
      }
    } else {

      // Compile format to check it
      String pattern = call.getOperand(2).getValue(String.class);

      if (call.getType().isFamily(TypeFamily.TEMPORAL)) {
        DateTimeFormat.of(pattern);
      } else if (call.getType().isFamily(TypeFamily.NUMERIC)) {
        NumberFormat.of(pattern);
      }
    }

    return call;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Object value = operands[0].getValue();
    if (value == null) return null;

    Type type = operands[1].getValue(Type.class);

    String format = null;
    if (operands.length == 3) {
      format = operands[2].getValue(String.class);
    }

    try {
      return type.cast(value, format);
    } catch (ExpressionException e) {
      return null;
    }
  }
}
