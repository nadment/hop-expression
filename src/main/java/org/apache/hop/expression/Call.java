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
package org.apache.hop.expression;

import static java.util.Objects.requireNonNull;

import java.io.StringWriter;
import java.util.Arrays;
import java.util.Collection;
import java.util.Deque;
import java.util.LinkedList;
import java.util.Objects;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;

/**
 * An expression formed by a call to an {@link Operator} with zero or more expressions as operands.
 */
public class Call implements IExpression {

  // The position of this expression in the source before compilation else 0.
  protected final int position;
  // The operator of this call
  protected Operator operator;
  // The operands of this call
  protected IExpression[] operands;
  // The return data type
  protected Type type = Types.UNKNOWN;

  public Call(Operator operator, IExpression... operands) {
    this(0, operator, operands);
  }

  public Call(Operator operator, Collection<IExpression> operands) {
    this(0, operator, operands);
  }

  public Call(int position, Operator operator, IExpression... operands) {
    this.operator = requireNonNull(operator, "operator");
    this.operands = requireNonNull(operands, "operands");
    this.position = position;
  }

  public Call(int position, Operator operator, Collection<IExpression> operands) {
    this.operator = requireNonNull(operator, "operator");
    this.operands = requireNonNull(operands, "operands").toArray(new IExpression[0]);
    this.position = position;
  }

  @Override
  public Kind getKind() {
    return Kind.CALL;
  }

  @Override
  public int getCost() {
    int cost = operands.length + 3;
    for (IExpression operand : operands) {
      cost += operand.getCost();
    }
    return cost;
  }

  /** Data type is unknown before validation. */
  @Override
  public Type getType() {
    return type;
  }

  /**
   * Returns the position of this expression in the source before compilation else 0.
   *
   * @return position
   */
  public int getPosition() {
    return position;
  }

  /**
   * Get the operator
   *
   * @return the operator
   */
  public Operator getOperator() {
    return operator;
  }

  /**
   * Change the operator, be very careful with this method!
   *
   * @param operator The operator to set
   */
  public void setOperator(Operator operator) {
    this.operator = requireNonNull(operator, "operator");
  }

  /**
   * Get array of operands. An empty array is returned if no operands
   *
   * @return the operands
   */
  public IExpression[] getOperands() {
    return operands;
  }

  public IExpression getOperand(int index) {
    return operands[index];
  }

  /**
   * Change the value of an operand.
   *
   * @param index The operand index
   * @param operand The operand to set
   */
  public void setOperand(int index, IExpression operand) {
    if (index < 0 || index >= operands.length) {
      return;
    }

    operands[index] = requireNonNull(operand, "operand");
  }

  /** Returns a count of operands of this expression. */
  public int getOperandCount() {
    return operands.length;
  }

  @Override
  public Object getValue() {
    try {
      return operator.eval(operands);
    } catch (ExpressionException e) {
      throw e;
    } catch (Throwable e) {
      ErrorCode error =
          (operator instanceof Function)
              ? ErrorCode.CALL_FUNCTION_ERROR
              : ErrorCode.CALL_OPERATOR_ERROR;
      throw new ExpressionException(error, operator, e.getMessage());
    }
  }

  @Override
  public <T> T getValue(final Class<T> clazz) {
    try {
      Object value = operator.eval(operands);
      return type.convert(value, clazz);
    } catch (ExpressionException e) {
      throw e;
    } catch (Throwable e) {
      ErrorCode error =
          (operator instanceof Function)
              ? ErrorCode.CALL_FUNCTION_ERROR
              : ErrorCode.CALL_OPERATOR_ERROR;
      throw new ExpressionException(error, operator, e.getMessage());
    }
  }

  /**
   * Validates the operands of the call, inferring the return type.
   *
   * @param context The context against which the expression will be validated.
   */
  @Override
  public void validate(final IExpressionContext context) throws ExpressionException {

    // Validates the operands of a call
    for (IExpression operand : operands) {
      operand.validate(context);
    }

    // Check the number of operands expected
    operator.checkOperandCount(this);

    // Check operand types expected
    operator.checkOperandTypes(this, true);

    // Inferring the return type
    type = operator.inferReturnType(this);
  }

  /** Inferring the return type. */
  public Type inferReturnType() {
    type = operator.inferReturnType(this);
    return type;
  }

  @Override
  public <E> E accept(IExpressionVisitor<E> visitor) {
    return visitor.visitCall(this);
  }

  @Override
  public boolean isOperator(final Operator other) {
    return operator.is(other);
  }

  @Override
  public Call asCall() {
    return this;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;

    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    Call call = (Call) o;
    return this.operator.equals(call.operator)
        && this.type.equals(call.type)
        && Arrays.equals(this.operands, call.operands);
  }

  @Override
  public int hashCode() {
    return Objects.hash(type, operator, operands);
  }

  @Override
  public void unparse(final StringWriter writer, int leftPrec, int rightPrec) {
    if ((leftPrec < operator.getLeftPrec() && (leftPrec != 0))
        || (operator.getRightPrec() > rightPrec && (rightPrec != 0))) {
      writer.append('(');
      operator.unparse(writer, operands);
      writer.append(')');
    } else {
      operator.unparse(writer, operands);
    }
  }

  @Override
  public String toString() {
    StringWriter writer = new StringWriter();
    unparse(writer, 0, 0);
    return writer.toString();
  }

  @Override
  public boolean isConstant() {
    // A call is constant if the operator is deterministic and all operands are constant
    if (operator.isDeterministic()) {
      for (IExpression operand : operands) {
        if (!operand.isConstant()) {
          return false;
        }
      }
      return true;
    }
    return false;
  }

  /**
   * Normalize reversible operator
   *
   * <ul>
   *   <li>swap term to identifier operator literal 1>A → A<1
   *   <li>ordering identifiers by name with case sensitive B>A → A<B
   *   <li>ordering the low-cost operand to the left
   * </ul>
   *
   * @param call The call to normalize
   * @return normalized call
   */
  public static Call normalizeReversiblePredicate(Call call) {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Nothing to normalize
    if (left.is(Kind.IDENTIFIER) && (right.is(Kind.LITERAL) || right.is(Kind.CALL))) {
      return call;
    }

    // Normalize identifier to the left
    if (right.is(Kind.IDENTIFIER)) {
      // Swap terms and reverse operator 1>A → A<1
      if (left.is(Kind.LITERAL)) {
        return call.reverse();
      }
      // Swap terms and reverse operator B>A → A<B
      if (left.is(Kind.IDENTIFIER)
          && left.asIdentifier().getName().compareTo(right.asIdentifier().getName()) > 0) {
        return call.reverse();
      }
    }

    // Normalize operator by moving the low-cost operand to the left
    if (left.getCost() > right.getCost()) {
      return call.reverse();
    }

    // If same cost order with textual representation
    //    if (left.toString().compareTo(right.toString()) > 0) {
    //      System.out.println("Reverse term by textual");
    //      return reverse(call);
    //    }

    return call;
  }

  /**
   * Swap the first and second operands, reverse the operator and preserve type.
   *
   * @param call
   * @return call
   */
  protected Call reverse() {
    Call reverse = new Call(operator.reverse(), getOperand(1), getOperand(0));
    reverse.inferReturnType();
    return reverse;
  }

  /**
   * Normalize symmetrical operator
   *
   * <ul>
   *   <li>swap term to identifier operator literal 1=A → A=1
   *   <li>ordering identifiers by name (case sensitive) B=A → A=B
   *   <li>ordering the low-cost operand to the left
   * </ul>
   *
   * @param call The call to normalize
   * @return normalized call
   */
  public static Call normalizeSymmetricalPredicate(Call call) {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Nothing to normalize
    if (left.is(Kind.IDENTIFIER) && (right.is(Kind.LITERAL) || right.is(Kind.CALL))) {
      return call;
    }

    if (right.is(Kind.IDENTIFIER)) {

      // Swap terms 1=A → A=1
      if (left.is(Kind.LITERAL)) {
        return call.swap();
      }
      // Swap terms B=A → A=B
      if (left.is(Kind.IDENTIFIER)
          && left.asIdentifier().getName().compareTo(right.asIdentifier().getName()) > 0) {
        return call.swap();
      }
    }

    // Normalize operator by moving the low-cost operand to the left
    if (left.getCost() > right.getCost()) {
      return call.swap();
    }

    // If same cost order with textual representation
    //    if (left.toString().compareTo(right.toString()) > 0) {
    //      System.out.println("Swap term by textual");
    //      return swap(call);
    //    }

    return call;
  }

  /**
   * Swap the first and second operands and preserve type.
   *
   * @param call
   * @return call
   */
  protected Call swap() {
    Call swaped = new Call(operator, getOperand(1), getOperand(0));
    swaped.inferReturnType();
    return swaped;
  }

  public Deque<IExpression> getChainedOperands(boolean allowDuplicate) {
    return getChainedOperands(this, new LinkedList<>(), allowDuplicate);
  }

  private Deque<IExpression> getChainedOperands(
      Call call, Deque<IExpression> operands, boolean allowDuplicate) {
    for (IExpression operand : call.getOperands()) {
      if (operand.isOperator(call.getOperator())) {
        getChainedOperands(operand.asCall(), operands, allowDuplicate);
      } else if (allowDuplicate || !operands.contains(operand)) {
        operands.push(operand);
      }
    }

    return operands;
  }
}
