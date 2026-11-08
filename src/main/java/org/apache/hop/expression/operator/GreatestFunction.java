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
import java.util.List;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * The function returns the largest value that is not NULL, or NULL if all values are NULL.
 *
 * @see LeastFunction
 */
@FunctionPlugin
@NullMarked
public class GreatestFunction extends Function {

  public GreatestFunction() {
    super(
        "GREATEST",
        ReturnTypes.LEAST_RESTRICTIVE,
        OperandTypes.COMPARABLE_ORDERED_VARIADIC,
        OperatorCategory.CONDITIONAL,
        "/docs/hop-user-manual/expression/greatest.html");
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    return removeDuplicateOperands(call);
  }

  /**
   * Removes the operands repeated in a call of GREATEST or LEAST, the result is the same without
   * them, and returns the operand alone if it is the only one.
   *
   * <p>GREATEST(x,y,x) → GREATEST(x,y), GREATEST(x) → x
   *
   * @param call the call of GREATEST or LEAST
   * @return the simplified expression, or the call if nothing changed
   */
  static IExpression removeDuplicateOperands(Call call) {
    List<IExpression> operands = new ArrayList<>();
    for (IExpression operand : call.getOperands()) {
      if (operands.stream().noneMatch(previous -> isSameExpression(previous, operand))) {
        operands.add(operand);
      }
    }

    if (operands.size() == 1) {
      return operands.getFirst();
    }
    if (operands.size() < call.getOperandCount()) {
      return new Call(call.getOperator(), operands);
    }
    return call;
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coercionComparisonOperator(call);
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    Object result = null;
    Type type = operands[0].getType();
    for (IExpression operand : operands) {
      Object value = operand.getValue();

      // Null is never greater
      if (value == null) continue;
      if (result == null || type.compare(result, value) < 0) result = value;
    }

    return result;
  }
}
