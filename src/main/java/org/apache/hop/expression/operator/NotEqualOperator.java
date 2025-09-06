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

/** Comparison not equals operator '<code>!=</code>' or '<code><></code>'. */
public class NotEqualOperator extends BinaryOperator {
  public static final NotEqualOperator INSTANCE = new NotEqualOperator("!=");

  public NotEqualOperator(final String name) {
    super(
        "NOT_EQUAL",
        name,
        90,
        Associativity.LEFT,
        ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.COMPARABLE_UNORDERED_COMPARABLE_UNORDERED,
        OperatorCategory.COMPARISON,
        "/docs/not_equal.html");
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public Operator not() {
    return EqualOperator.INSTANCE;
  }

  @Override
  public Operator reverse() {
    return NotEqualOperator.INSTANCE;
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
    return !type.compareEqual(left, right);
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Normalize
    call = normalizeSymmetricalPredicate(call);

    // If the operator has been changed
    if (!call.getOperator().equals(this)) return call;

    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Simplify comparison when operands is of boolean type
    // x<>TRUE → x
    // x<>FALSE → NOT x
    if (Types.isBoolean(left.getType())) {
      if (right == Literal.TRUE) {
        return new Call(BoolNotOperator.INSTANCE, left);
      }
      if (right == Literal.FALSE) {
        return left;
      }
    }

    // Simplify x!=NULL → NULL
    if (left.isNull() || right.isNull()) {
      return Literal.NULL_BOOLEAN;
    }

    // Simplify x!=x → NULL AND x IS NULL
    if (left.equals(right)) {
      return new Call(
          BoolAndOperator.INSTANCE, Literal.NULL_BOOLEAN, new Call(IsNullOperator.INSTANCE, left));
    }

    // Simplify 3!=X+1 → 3-1!=X
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
