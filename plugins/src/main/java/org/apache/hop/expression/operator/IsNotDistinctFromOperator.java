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

import org.apache.hop.expression.Call;
import org.apache.hop.expression.Category;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.Comparison;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.StringWriter;

/**
 * Comparison IS NOT DISTINCT FROM operator.
 * <br>
 * <strong>Syntax:</strong> <code>x IS NOT DISTINCT FROM y</code>
 */
public class IsNotDistinctFromOperator extends Operator {

  public IsNotDistinctFromOperator() {
    super("IS NOT DISTINCT FROM", 10, true, ReturnTypes.BOOLEAN, OperandTypes.ANY_ANY,
        Category.COMPARISON, "/docs/is-distinct-from.html");
  }

  @Override
    public Object eval(final IExpression[] operands) {
    Object v0 = operands[0].getValue();
    Object v1 = operands[1].getValue();

    return Comparison.compare(v0, v1) == 0;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Simplify same expressions.
    if (left.equals(right)) {
      return Literal.TRUE;
    }

    // The DISTINCT predicate is a verbose way of NULL safe comparisons.
    // If one of the operands is NULL, then it can be simplified to the IS NULL predicate.
    if (left == Literal.NULL) {
      return new Call(Operators.IS_NULL, right);
    }
    if (right == Literal.NULL) {
      return new Call(Operators.IS_NULL, left);
    }

    return call;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append(" IS NOT DISTINCT FROM ");
    operands[1].unparse(writer);
  }
}
