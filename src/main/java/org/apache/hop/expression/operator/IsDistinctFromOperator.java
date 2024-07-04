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

/**
 * Comparison <code>IS DISTINCT FROM</code> operator. <br>
 * <strong>Syntax:</strong> <code>x IS DISTINCT FROM y</code>
 *
 * @see {@link IsNotDistinctFromOperator}
 */
public class IsDistinctFromOperator extends BinaryOperator {

  public IsDistinctFromOperator() {
    super(
        "IS DISTINCT FROM",
        "IS DISTINCT FROM",
        10,
        true,
        ReturnTypes.BOOLEAN_NOT_NULL,
        OperandTypes.ANY_ANY,
        OperatorCategory.COMPARISON,
        "/docs/is-distinct-from.html");
  }

  @Override
  public Operator not() {
    return Operators.IS_NOT_DISTINCT_FROM;
  }

  @Override
  public Operator reverse() {
    return this;
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Object left = operands[0].getValue();
    Object right = operands[1].getValue();

    return !Comparison.equals(left, right);
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Simplify same expressions.
    if (left.equals(right)) {
      return Literal.FALSE;
    }

    // The DISTINCT predicate is a verbose way of NULL safe comparisons.
    // If one of the operands is NULL, then it can be simplified to the NULL predicate.
    if (left.isNull()) {
      return new Call(Operators.IS_NOT_NULL, right);
    }
    if (right.isNull()) {
      return new Call(Operators.IS_NOT_NULL, left);
    }

    return call;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, getLeftPrec(), getRightPrec());
    writer.append(" IS DISTINCT FROM ");
    operands[1].unparse(writer, getLeftPrec(), getRightPrec());
  }
}
