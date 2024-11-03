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
import java.util.PriorityQueue;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionComparator;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Bitwise OR operator. <br>
 * <strong>Syntax:</strong> <code>x | y</code>
 */
@FunctionPlugin
public class BitOrFunction extends Function {

  public BitOrFunction() {
    super(
        "BIT_OR",
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.NUMERIC_NUMERIC,
        OperatorCategory.BITWISE,
        "/docs/bit_or.html");
  }

  public BitOrFunction(String name) {
    super(
        "BIT_OR",
        name,
        90,
        true,
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.NUMERIC_NUMERIC,
        OperatorCategory.BITWISE,
        "/docs/bit_or.html");
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    // Reorder chained symmetric operator and simplify A | (..A..) --> (..A..)
    PriorityQueue<IExpression> operands = new PriorityQueue<>(new ExpressionComparator());
    operands.addAll(call.getChainedOperands(true));
    IExpression operand = operands.poll();
    while (!operands.isEmpty()) {
      call = new Call(this, operand, operands.poll());
      call.inferReturnType();
      operand = call;
    }
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Simplify NULL | A → NULL
    if (left.isNull()) {
      return Literal.NULL_INTEGER;
    }

    // Simplify 0 | A → A (even if A is null)
    if (Literal.ZERO.equals(left)) {
      return right;
    }

    // TODO: Simplify A | !A → -1 (if A is not nullable)

    return call;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Long left = operands[0].getValue(Long.class);
    if (left == null) return null;
    Long right = operands[1].getValue(Long.class);
    if (right == null) return null;
    return left | right;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, getLeftPrec(), getRightPrec());
    writer.append('|');
    operands[1].unparse(writer, getLeftPrec(), getRightPrec());
  }
}
