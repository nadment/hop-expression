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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Types;

/**
 * The IFNULL function replace the null with value (Alias NVL).
 *
 * @see CoalesceFunction
 */
@FunctionPlugin(names = "NVL")
public class IfNullFunction extends Function {

  public static final Function INSTANCE = new IfNullFunction();

  public IfNullFunction() {
    super(
        "IFNULL",
        ReturnTypes.LEAST_RESTRICTIVE,
        OperandTypes.SAME_SAME,
        OperatorCategory.CONDITIONAL,
        "/docs/ifnull.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    // Remove null and duplicate but keep order
    final List<IExpression> operands = new ArrayList<>();
    for (IExpression operand : call.getOperands()) {
      if (operand.isNull() || operands.contains(operand)) {
        continue;
      }

      // Flatten chained COALESCE or IFNULL but keep order
      if (operand.isOperator(IfNullFunction.INSTANCE)
          || operand.isOperator(CoalesceFunction.INSTANCE)) {
        operands.addAll(Arrays.asList(call(operand).getOperands()));
      } else {
        operands.add(operand);
      }
    }

    return switch (operands.size()) {
      case 0 -> // Nothing to coalesce
          new Literal(null, call.getType());
      case 1 -> // COALESCE(X) → X
          operands.get(0);
      case 2 -> // COALESCE(X,Y) → IFNULL(X,Y)
          new Call(IfNullFunction.INSTANCE, operands);
      default -> {
        // First is literal COALESCE(1, a, b) → 1
        if (operands.get(0).isConstant()) {
          yield operands.get(0);
        }

        yield new Call(CoalesceFunction.INSTANCE, operands);
      }
    };
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Object value = operands[0].getValue();
    if (value == null) return operands[1].getValue();
    return value;
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coerceOperandsType(call, call.getType());
  }
}
