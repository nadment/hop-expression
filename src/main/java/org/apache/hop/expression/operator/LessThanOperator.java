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
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.type.Comparison;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Types;

/**
 * Comparison less than operator. <br>
 * <strong>Syntax:</strong> <code>x &lt; y</code>
 */
public class LessThanOperator extends BinaryOperator {

  public LessThanOperator() {
    super(
        "LESS_THAN",
        "<",
        130,
        true,
        ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.COMPARABLE_ORDERED_COMPARABLE_ORDERED,
        OperatorCategory.COMPARISON,
        "/docs/less_than.html");
  }

  @Override
  public Operator not() {
    return Operators.GREATER_THAN_OR_EQUAL;
  }

  @Override
  public Operator reverse() {
    return Operators.GREATER_THAN;
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

    return Comparison.compare(left, right) < 0;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Normalize
    call = normalizeReversiblePredicate(call);

    // If the operator has been changed
    if (!call.getOperator().equals(this)) return call;

    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Simplify x<NULL → NULL
    if (left.isNull() || right.isNull()) {
      return Literal.NULL_BOOLEAN;
    }

    // Simplify x<x → NULL AND x IS NULL
    if (left.equals(right)) {
      return new Call(Operators.BOOLAND, Literal.NULL_BOOLEAN, new Call(Operators.IS_NULL, left));
    }

    // Simplify 3<X+1 → 3-1<X
    if (left.isConstant()
        && right.isOperator(Operators.ADD)
        && call(right).getOperand(0).isConstant()) {
      return new Call(
          call.getOperator(),
          new Call(Operators.SUBTRACT, left, call(right).getOperand(0)),
          call(right).getOperand(1));
    }

    return call;
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coercionComparisonOperator(call);
  }
}
