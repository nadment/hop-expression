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
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.IOperandCountRange;
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeId;
import org.apache.hop.expression.type.UnknownType;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.Collection;
import java.util.Objects;

/**
 * An expression formed by a call to an {@link Operator} with zero or more expressions as operands.
 */
public final class Call implements IExpression {

  // The return data type
  protected Type type = UnknownType.UNKNOWN;

  // The position of this expression in the source before compilation else 0.
  protected final int position;

  protected final Operator operator;
  protected final IExpression[] operands;

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
   * Validate a call.
   * 
   * @param context The context against which the expression will be validated.
   */
  @Override
  public void validate(final IExpressionContext context) throws ExpressionException {

    // Validate all operands
    for (IExpression operand : operands) {
      operand.validate(context);
    }

    // Check the number of operands expected
    IOperandCountRange operandCountRange = operator.getOperandCountRange();
    if (!operandCountRange.isValid(this.getOperandCount())) {
      if (getOperandCount() < operandCountRange.getMin()) {
        throw new ExpressionException(position, ErrorCode.NOT_ENOUGH_ARGUMENT, operator);
      }
      if (getOperandCount() > operandCountRange.getMax()) {
        throw new ExpressionException(position, ErrorCode.TOO_MANY_ARGUMENT, operator);
      }
    }

    // Check operand types expected
    IOperandTypeChecker operandTypeChecker = operator.getOperandTypeChecker();
    if (!operandTypeChecker.checkOperandTypes(this)) {
      throw new ExpressionException(position, ErrorCode.ILLEGAL_ARGUMENT_TYPE, operator);
    }

    // Inference type
    this.type = operator.getReturnTypeInference().inferReturnType(this);
  }

  /**
   * Compile and optimize the call in the context
   * 
   * @param context The context against which the expression will be compiled.
   * @return the compiled expression
   */
  public IExpression compile(final IExpressionContext context) throws ExpressionException {

    // Compile all operands
    IExpression[] compiledOperands = new IExpression[this.getOperandCount()];
    for (int i = 0; i < this.getOperandCount(); i++) {
      compiledOperands[i] = this.getOperand(i).compile(context);
    }

    // Create a new call with compiled operands and infer return type
    Call call = new Call(this.getOperator(), compiledOperands).inferReturnType();

    // Compile with operator
    IExpression expression = call.getOperator().compile(context, call);

    if (expression.is(Kind.CALL)) {
      // Infer return type
      expression = expression.asCall().inferReturnType();
    }

    // Evaluate if constant
    if (expression.isConstant()) {
      try {
        Object value = expression.getValue();
        Type valueType = expression.getType();

        // Some operator don't known return type like JSON_VALUE.
        if (TypeId.ANY.equals(valueType.getId())) {
          return Literal.of(value);
        } else {
          // TODO: Remove this cast when operands are coerced to the least restrictive data type 
          // value = valueType.cast(value);
        }

        // For CAST operator, it's important to return type
        return new Literal(value, valueType);
      } catch (Exception e) {
        // Ignore error like division by zero "X IN (1,3/0)" and continue
      }
    }

    return expression;
  }

  public Call inferReturnType() {
    this.type = this.operator.getReturnTypeInference().inferReturnType(this);
    return this;
  }

  @Override
  public <E> E accept(IExpressionContext context, IExpressionVisitor<E> visitor) {
    return visitor.apply(context, this);
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
  public boolean equals(Object other) {
    if (other instanceof Call) {
      Call call = (Call) other;
      return this.type.equals(call.type) && this.operator.equals(call.operator)
          && Arrays.equals(this.operands, call.operands);
    }
    return false;
  }

  @Override
  public int hashCode() {
    return Objects.hash(type, operator, operands);
  }

  @Override
  public void unparse(StringWriter writer) {
    //writer.append('(');
    operator.unparse(writer, operands);
    //writer.append(')');
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
