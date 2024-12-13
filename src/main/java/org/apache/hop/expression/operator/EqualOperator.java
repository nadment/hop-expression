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
 * Comparison equals operator. <br>
 * <strong>Syntax:</strong> <code>x = y</code>
 *
 * <p>NULL is not equal ( = ) to anything—not even to another NULL.
 */
public class EqualOperator extends BinaryOperator {

  public EqualOperator() {
    super(
        "EQUAL",
        "=",
        130,
        true,
        ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.COMPARABLE_UNORDERED_COMPARABLE_UNORDERED,
        OperatorCategory.COMPARISON,
        "/docs/equal.html");
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public Operator not() {
    return Operators.NOT_EQUAL;
  }

  @Override
  public Operator reverse() {
    return Operators.EQUAL;
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
    return Comparison.equals(left, right);
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Normalize
    call = normalizeSymmetricalPredicate(call);

    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Simplify comparison when operands is of boolean type
    // x=TRUE → x
    // x=FALSE → NOT x
    if (Types.isBoolean(left.getType())) {
      if (right == Literal.TRUE) {
        return left;
      }
      if (right == Literal.FALSE) {
        return new Call(Operators.BOOLNOT, left);
      }
    }

    // Simplify x=x → TRUE (if x is not nullable)
    if (left.equals(right) && !left.getType().isNullable()) {
      return Literal.TRUE;
    }

    // Simplify x=NULL → NULL
    if (left.isNull() || right.isNull()) {
      return Literal.NULL_BOOLEAN;
    }

    // Simplify 3=X+1 → 3-1=X
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
