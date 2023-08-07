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
import org.apache.hop.expression.Category;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * The COALESCE function returns the first of its arguments that is not null. Null is returned
 * only if all arguments are null.
 */
@FunctionPlugin
public class CoalesceFunction extends Function {

  public CoalesceFunction() {
    super("COALESCE", ReturnTypes.LEAST_RESTRICTIVE, OperandTypes.AT_LEAST_ONE_SAME_VARIADIC,
        Category.CONDITIONAL, "/docs/coalesce.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    for (IExpression operand : operands) {
      Object value = operand.getValue();
      if (value != null)
        return value;
    }

    return null;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Remove null and duplicate but keep order
    final List<IExpression> operands = new ArrayList<>();
    for (IExpression operand : call.getOperands()) {
      if (operand.isNull() || operands.contains(operand)) {
        continue;
      }

      // Flatten chained coalesce
      if (operand.is(call.getOperator())) {
        operands.addAll(Arrays.asList(operand.asCall().getOperands()));
      } else {
        operands.add(operand);
      }
    }

    switch (operands.size()) {
      case 0: // Nothing to coalesce
        return Literal.NULL;
      case 1: // Coalesce(X) => X
        return operands.get(0);
      default:
        return new Call(call.getOperator(), operands);
    }
  }
}
