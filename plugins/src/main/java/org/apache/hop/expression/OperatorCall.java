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
import java.util.List;
import java.util.Objects;

/** A <code>OperatorCall</code> is a call to an {@link Operator operator}. */
public class OperatorCall implements IExpression {

  protected final Operator operator;
  protected final IExpression[] operands;

  public OperatorCall(Operator operator, IExpression... operands) throws ExpressionException {
    super();
    this.operator = Objects.requireNonNull(operator);
    this.operands = operands;
  }

  public OperatorCall(Operator operator, List<IExpression> operands) throws ExpressionException {
    super();
    this.operator = Objects.requireNonNull(operator);
    this.operands = operands.toArray(new IExpression[0]);
  }

  public Object eval(IExpressionContext context) throws ExpressionException {
    return operator.eval(context, operands);
  }

  @Override
  public Kind getKind() {
    return Kind.OPERATOR;
  }

  @Override
  public int getCost() {
    int cost = 1;
    for (IExpression operand : operands) {
      cost += operand.getCost();
    }

    return cost;
  }

  /**
   * Get to the operator
   *
   * @return the operator
   */
  public Operator getOperator() {
    return operator;
  }

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

  /**
   * Creates a new call to the same operator with different operands.
   *
   * @param operands Operands to call
   * @return New call
   * @throws EXpressionException
   */
  public OperatorCall clone(IExpression... operands) {
    return new OperatorCall(operator, operands);
  }

  /**
   * Creates a new call to the same operator with different operands.
   *
   * @param operands Operands to call
   * @return New call
   * @throws EXpressionException
   */
  public OperatorCall clone(List<IExpression> operands) {
    return new OperatorCall(operator, operands);
  }

  @Override
  public void write(StringWriter writer) {
    operator.write(writer, operands);
  }

  @Override
  public String toString() {
    StringWriter writer = new StringWriter();
    write(writer);
    return writer.toString();
  }



  // @Override
  // public void write(StringWriter writer, int leftPrec, int rightPrec) {
  //
  // if ((leftPrec < operator.getLeftPrecedence() && (rightPrec != 0))
  // || (operator.getRightPrecedence() >= rightPrec && (rightPrec != 0))) {
  // writer.append('(');
  // operator.write(writer, this, 0, 0);
  // writer.append(')');
  // } else {
  // operator.write(writer, this, leftPrec, rightPrec);
  // }
  // }

}
