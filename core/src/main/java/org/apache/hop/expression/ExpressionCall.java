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

/** A <code>ExpressionCall</code> is a call to an {@link Operator operator}. */
public class ExpressionCall implements IExpression {
  protected static final Class<?> PKG = IExpression.class; // for i18n purposes

  private final Operator operator;
  private final IExpression[] operands;

  public ExpressionCall(Operator operator, IExpression... operands) throws ExpressionException {
    super();
    this.operator = Objects.requireNonNull(operator);
    this.operands = operands;

    operator.checkNumberOfArguments(operands.length);
  }

  public ExpressionCall(Operator operator, List<IExpression> operands) throws ExpressionException {
    super();
    this.operator = Objects.requireNonNull(operator);
    this.operands = operands.toArray(new IExpression[0]);

    operator.checkNumberOfArguments(this.operands.length);
  }

  @Override
  public Object eval(IExpressionContext context) throws ExpressionException {
      return operator.eval(context, operands);
  }

  @Override
  public Kind getKind() {
    return operator.kind;
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
   */
  public ExpressionCall clone(List<IExpression> operands) {
    return new ExpressionCall(operator, operands);
  }

  /**
   * Creates a new call to the same operator with different operands.
   *
   * @param operands Operands to call
   * @return New call
   */
  public ExpressionCall clone(IExpression... operands) {
    return new ExpressionCall(operator, operands);
  }

  @Override
  public String toString() {
    StringWriter writer = new StringWriter();
    write(writer, 0, 0);
    return writer.toString();
  }

  @Override
  public void write(StringWriter writer, int leftPrec, int rightPrec) {

    if ((leftPrec < operator.getLeftPrecedence() && (rightPrec != 0))
        || (operator.getRightPrecedence() >= rightPrec && (rightPrec != 0))) {
      writer.append('(');
      operator.write(writer, this, 0, 0);
      writer.append(')');
    } else {
      operator.write(writer, this, leftPrec, rightPrec);
    }
  }
}
