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
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.Collection;
import java.util.Objects;

/**
 * An expression formed by a call to an {@link Operator} with zero or more expressions as operands.
 */
public class Call implements IExpression {

  // The position of this expression in the source before compilation else 0.
  protected final int position;
  // The operator of this call
  protected final Operator operator;
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

  /**
   * Data type is unknown before validation.
   */
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
   * Get array of operands.
   * An empty array is returned if no operands
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
   * Changes the value of an operand.
   *
   * @param i Operand index
   * @param operand Operand value
   */
  public void setOperand(int index, IExpression operand) {
    if (index < 0 || index >= operands.length) {
      return;
    }

    operands[index] = operand;
  }

  /**
   * Returns a count of operands of this expression.
   */
  public int getOperandCount() {
    return operands.length;
  }

  @Override
  public Object getValue() {
    try {
      return operator.eval(operands);
    } catch (Exception e) {
      throw new ExpressionException(ErrorCode.OPERATOR_ERROR, operator, e.getMessage());
    }
  }

  @Override
  public <T> T getValue(final Class<T> clazz) {
    try {
      Object value = operator.eval(operands);
      return type.convert(value, clazz);
    } catch (Exception e) {
      throw new ExpressionException(ErrorCode.OPERATOR_ERROR, operator, e.getMessage());
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

  /**
   * Inferring the return type.
   */
  public Type inferReturnType() {
    type = operator.inferReturnType(this);
    return type;
  }

  @Override
  public <E> E accept(IExpressionVisitor<E> visitor) {
    return visitor.visitCall(this);
  }

  @Override
  public boolean is(final Operator other) {
    return operator.is(other);
  }

  @Override
  public Call asCall() {
    return this;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o)
      return true;

    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    Call call = (Call) o;
    return this.type.equals(call.type) && this.operator.equals(call.operator)
        && Arrays.equals(this.operands, call.operands);
  }

  @Override
  public int hashCode() {
    return Objects.hash(type, operator, operands);
  }

  @Override
  public void unparse(final StringWriter writer) {
    operator.unparse(writer, operands);
  }

  @Override
  public String toString() {
    StringWriter writer = new StringWriter();
    unparse(writer);
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
}
