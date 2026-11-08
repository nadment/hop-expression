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

import java.io.StringWriter;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.IReturnTypeInference;
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** Binary operator, as in "x + y". */
@NullMarked
public abstract class BinaryOperator extends Operator {

  protected BinaryOperator(
      String id,
      String name,
      int precedence,
      Associativity associativity,
      IReturnTypeInference returnTypeInference,
      IOperandTypeChecker operandTypeChecker,
      String category,
      String documentationUrl) {
    super(
        id,
        name,
        precedence,
        associativity,
        returnTypeInference,
        operandTypeChecker,
        category,
        documentationUrl);
  }

  /**
   * Moves the constant of an addition, a subtraction or a negation to the constant side of a
   * comparison, the constants are then computed once.
   *
   * <p>3=X+1 → 3-1=X, 3=X-1 → 3+1=X, 3&gt;-X → -3&lt;X
   *
   * @param call the comparison, with the constant on the left like a normalized comparison
   * @return the simplified comparison, or null if the constant can't be moved
   */
  protected static @Nullable Call moveConstant(Call call) {
    IExpression left = call.getOperand(0);
    if (!left.isConstant() || !(call.getOperand(1) instanceof Call operation)) {
      return null;
    }

    // 3=X+1 → 3-1=X
    if (operation.isOperator(AddOperator.INSTANCE) && operation.getOperand(0).isConstant()) {
      return new Call(
          call.getOperator(),
          new Call(SubtractOperator.INSTANCE, left, operation.getOperand(0)),
          operation.getOperand(1));
    }

    // Only for numbers, adding a month to a date is not the reverse of subtracting it
    if (!Types.isNumeric(operation.getType())) {
      return null;
    }

    // 3=X-1 → 3+1=X
    if (operation.isOperator(SubtractOperator.INSTANCE) && operation.getOperand(1).isConstant()) {
      return new Call(
          call.getOperator(),
          new Call(AddOperator.INSTANCE, left, operation.getOperand(1)),
          operation.getOperand(0));
    }

    // 3>-X → -3<X, the comparison is reversed like the sign
    Operator reverse = call.getOperator().reverse();
    if (operation.isOperator(NegativeOperator.INSTANCE) && reverse != null) {
      return new Call(reverse, new Call(NegativeOperator.INSTANCE, left), operation.getOperand(0));
    }

    return null;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, 0, getLeftPrecedence());
    writer.append(getName());
    operands[1].unparse(writer, getRightPrecedence(), 0);
  }
}
