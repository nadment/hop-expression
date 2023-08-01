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
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.StringWriter;
import java.math.BigDecimal;

/**
 * Arithmetic multiply operator.
 * <br>
 * <strong>Syntax:</strong> <code>x * y</code>
 */
public class MultiplyOperator extends Operator {

  public MultiplyOperator() {
    super("MULTIPLY", "*", 50, true, ReturnTypes.MULTIPLY_OPERATOR, OperandTypes.NUMERIC_NUMERIC,
        Category.MATHEMATICAL, "/docs/multiply.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Simplify arithmetic "1 * A" to "A"
    if (Literal.ONE.equals(left)) {
      return right;
    }

    // Simplify arithmetic "(-A) * (-B)" to "A * B"
    if (left.is(Operators.NEGATIVE) && right.is(Operators.NEGATIVE)) {
      return new Call(Operators.MULTIPLY, left.asCall().getOperand(0),
          right.asCall().getOperand(0));
    }

    // Simplify arithmetic "A * A" to "SQUARE(A)"
    if (left.equals(right)) {
      return new Call(SquareFunction.INSTANCE, left);
    }

    // Pull up literal "1 * (1 * A)" to "(1 * 1) * A
    if (left.isConstant() && right.is(Operators.MULTIPLY)
        && right.asCall().getOperand(0).isConstant()) {
      IExpression operation = new Call(Operators.MULTIPLY, left, right.asCall().getOperand(0));
      return new Call(Operators.MULTIPLY, operation, right.asCall().getOperand(1));
    }

    return call;
  }

  @Override
  public Object eval(IExpression[] operands) {
    BigDecimal left = operands[0].getValue(BigDecimal.class);
    if (left == null)
      return null;
    BigDecimal right = operands[1].getValue(BigDecimal.class);
    if (right == null)
      return null;

    return left.multiply(right, DECIMAL128);
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append('*');
    operands[1].unparse(writer);
  }
}
