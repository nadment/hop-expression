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
import java.io.StringWriter;

/**
 * Comparison equals operator.
 * <br>
 * <strong>Syntax:</strong> <code>x = y</code>
 */
public class EqualOperator extends Operator {

  public EqualOperator() {
    super("EQUAL", "=", 130, true, ReturnTypes.BOOLEAN, OperandTypes.ANY_ANY,
        OperatorCategory.COMPARISON, "/docs/equal.html");
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands) throws Exception {
    // Treats NULLs as unknown values
    // NULL is not equal ( = ) to anything—not even to another NULL.
    Object left = operands[0].getValue(context);
    if (left == null) {
      return null;
    }
    Object right = operands[1].getValue(context);
    if (right == null) {
      return null;
    }
    return Comparison.compare(left, right) == 0;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Simplify "x = x" to "NULL OR x IS NOT NULL"
    if (left.equals(right)) {
      return new Call(Operators.BOOLOR, Literal.NULL, new Call(Operators.IS_NOT_NULL, left));
    }

    // Simplify "x = NULL" to "NULL"
    if (left.equals(Literal.NULL) || right.equals(Literal.NULL)) {
      return Literal.NULL;
    }    
    // Simplify "x = TRUE" to "x IS TRUE"
    if (left.equals(Literal.TRUE)) {
      return new Call(Operators.IS_TRUE, right);
    }
    if (right.equals(Literal.TRUE)) {
      return new Call(Operators.IS_TRUE, left);
    }
    // Simplify "x = FALSE" to "x IS FALSE"
    if (left.equals(Literal.FALSE)) {
      return new Call(Operators.IS_FALSE, right);
    }
    if (right.equals(Literal.FALSE)) {
      return new Call(Operators.IS_FALSE, left);
    }

    // Simplify "3 = X+1" to "3-1 = X"
    if (left.isConstant() && right.is(Operators.ADD)
        && right.asCall().getOperand(0).isConstant()) {
      return new Call(call.getOperator(),
          new Call(Operators.SUBTRACT, left, right.asCall().getOperand(0)),
          right.asCall().getOperand(1));
    }

    return call;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append('=');
    operands[1].unparse(writer);
  }
}
