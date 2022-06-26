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

import java.io.StringWriter;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/** 
 * A <code>Call</code> is a call to an {@link Operator}.
 */
public class Call implements IExpression {

  protected final Operator operator;
  protected final IExpression[] operands;

  public Call(Operator operator, IExpression... operands) {
    super();
    this.operator = Objects.requireNonNull(operator);
    this.operands = Objects.requireNonNull(operands);
    
    operator.checkNumberOfArguments(operands);
  }

  public Call(Operator operator, List<IExpression> operands) {
    super();
    this.operator = Objects.requireNonNull(operator);
    this.operands = operands.toArray(new IExpression[0]);
    
    operator.checkNumberOfArguments(this.operands);
  }

  public Object eval(IExpressionContext context) throws ExpressionException {
    return operator.eval(context, operands);
  }
  
  @Override
  public <E> E visit(IExpressionContext context, IExpressionVisitor<E> visitor) {
    return visitor.apply(context, this);    
  }
  
  @Override
  public boolean is(final Operator other) {
    if (other == null)
      return false;
    
    return operator.is(other);
  }

  @Override
  public Kind getKind() {
    return Kind.CALL;
  }

  @Override
  public int getCost() {
    int cost = 3;
    for (IExpression operand : operands) {
      cost += operand.getCost();
    }
    return cost;
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
  public boolean isNull() {
    return false;
  }
  
  @Override
  public boolean equals(Object other) {
    if (other == null)
      return false;

    if (other instanceof Call) {
      Call call = (Call) other;
      return this.operator.equals(call.operator) && Arrays.equals(this.operands,call.operands);
    }
    return false;
  }

  @Override
  public int hashCode() {
    return Objects.hash(operator,operands);
  }

  @Override
  public void unparse(StringWriter writer) {
    operator.unparse(writer, operands);
  }

  @Override
  public String toString() {
    StringWriter writer = new StringWriter();
    unparse(writer);
    return writer.toString();
  }
}
