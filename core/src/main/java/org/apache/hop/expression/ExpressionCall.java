/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
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
public class ExpressionCall extends Expression {

  private final Operator operator;
  private final Expression[] operands;

  public ExpressionCall(Operator operator, Expression... operands) throws ExpressionException {
    super();
    this.operator = Objects.requireNonNull(operator);
    this.operands = operands;
  }

  public ExpressionCall(Operator operator, List<Expression> operands) throws ExpressionException {
    super();
    this.operator = Objects.requireNonNull(operator);
    this.operands = operands.toArray(new Expression[0]);
  }

  @Override
  public Value eval(IExpressionContext context) throws ExpressionException {
    return operator.eval(context, operands);
  }

  @Override
  public Expression optimize(IExpressionContext context) throws ExpressionException {
    return operator.optimize(context, operands);
  }

  @Override
  public Kind getKind() {
    return operator.kind;
  }

  @Override
  public int getCost() {
    int cost = 1;
    for (Expression operand : operands) {
      cost += operand.getCost();
    }

    return cost;
  }

  /**
   * Accessor to the operator
   *
   * @return the operator
   */
  public Operator getOperator() {
    return operator;
  }

  public Expression[] getOperands() {
    return operands;
  }

  /**
   * Returns a count of operands of this expression. In real life there are unary (count == 1),
   * binary (count == 2) and ternary (count == 3) expressions.
   */
  public int getOperandCount() {
    return operands.length;
  }

  @Override
  public void unparse(StringWriter writer, int leftPrec, int rightPrec) {

    final Operator operator = this.getOperator();

    if (leftPrec < operator.getLeftPrecedence()
        || (operator.getRightPrecedence() >= rightPrec && (rightPrec != 0))) {
      writer.append('(');
      operator.unparse(writer, this, 0, 0);
      writer.append(')');
    } else {
      operator.unparse(writer, this, leftPrec, rightPrec);
    }
  }
}
