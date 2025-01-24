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
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;

/** Comparison greater than operator '<code>&gt;</code>'. */
public class GreaterThanOperator extends BinaryOperator {
  public static final GreaterThanOperator INSTANCE = new GreaterThanOperator();

  public GreaterThanOperator() {
    super(
        "GREATER_THAN",
        ">",
        130,
        true,
        ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.COMPARABLE_ORDERED_COMPARABLE_ORDERED,
        OperatorCategory.COMPARISON,
        "/docs/greater_than.html");
  }

  @Override
  public Operator not() {
    return LessThanOrEqualOperator.INSTANCE;
  }

  @Override
  public Operator reverse() {
    return LessThanOperator.INSTANCE;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Object left = operands[0].getValue();
    if (left == null) {
      return null;
    }
    Object right = operands[1].getValue();
    if (right == null) {
      return null;
    }
    Type type = operands[0].getType();
    return type.compare(left, right) > 0;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Normalize
    call = normalizeReversiblePredicate(call);

    // If the operator has been changed
    if (!call.getOperator().equals(this)) return call;

    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Simplify x>NULL → NULL
    if (left.isNull() || right.isNull()) {
      return Literal.NULL_BOOLEAN;
    }
    // Simplify x>x → NULL AND x IS NULL
    if (left.equals(right)) {
      return new Call(
          BoolAndOperator.INSTANCE, Literal.NULL_BOOLEAN, new Call(IsNullOperator.INSTANCE, left));
    }

    // Simplify 3>X+1 → 3-1>X
    if (left.isConstant()
        && right.isOperator(AddOperator.INSTANCE)
        && call(right).getOperand(0).isConstant()) {
      return new Call(
          call.getOperator(),
          new Call(SubtractOperator.INSTANCE, left, call(right).getOperand(0)),
          call(right).getOperand(1));
    }

    return call;
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coercionComparisonOperator(call);
  }
}
