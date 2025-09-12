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
import org.apache.hop.expression.type.UnknownType;

/**
 * An expression formed by a call to an {@link Operator} with zero or more expressions as operands.
 */
public class Call implements IExpression {

  /** The position of this expression in the source before compilation else 0. */
  protected final int position;

  /** The operator of this call. */
  protected final Operator operator;

  /** The operands of this call. */
  protected final IExpression[] operands;

  /** The return type. The type is unknown before validation. */
  protected Type type = UnknownType.UNKNOWN;

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
    } catch (Exception e) {
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
    } catch (Exception e) {
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
  public void inferReturnType() {
    for (IExpression operand : operands) {
      if (operand instanceof Call call) {
        call.inferReturnType();
      }
    }
    type = operator.inferReturnType(this);
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
    return Objects.hash(type, operator, Arrays.hashCode(operands));
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
   * Swap the first and second operands, reverse the operator and preserve type.
   *
   * @return call
   */
  protected Call reverse() {
    return new Call(operator.reverse(), getOperand(1), getOperand(0));
  }

  /**
   * Swap the first and second operands and preserve type.
   *
   * @return call
   */
  protected Call swap() {
    return new Call(operator, getOperand(1), getOperand(0));
  }

  public Deque<IExpression> getChainedOperands(boolean allowDuplicate) {
    return getChainedOperands(this, new LinkedList<>(), allowDuplicate);
  }

  private Deque<IExpression> getChainedOperands(
      Call call, Deque<IExpression> operands, boolean allowDuplicate) {
    for (IExpression operand : call.getOperands()) {
      if (operand.isOperator(call.getOperator())) {
        getChainedOperands((Call) operand, operands, allowDuplicate);
      } else if (allowDuplicate || !operands.contains(operand)) {
        operands.push(operand);
      }
    }

    return operands;
  }
}
