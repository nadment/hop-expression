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
import org.apache.hop.expression.type.Types;

/**
 * Bitwise AND operator. <br>
 * <strong>Syntax:</strong> <code>x &amp; y</code>
 */
@FunctionPlugin
public class BitAndFunction extends Function {

  public BitAndFunction() {
    super(
        "BIT_AND",
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.NUMERIC_NUMERIC,
        OperatorCategory.BITWISE,
        "/docs/bit_and.html");
  }

  public BitAndFunction(String name) {
    super(
        "BIT_AND",
        name,
        70,
        true,
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.NUMERIC_NUMERIC,
        OperatorCategory.BITWISE,
        "/docs/bit_and.html");
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    // Reorder chained symmetric operator and simplify A & (..A..) --> (..A..)
    PriorityQueue<IExpression> operands = new PriorityQueue<>(new ExpressionComparator());
    operands.addAll(this.getChainedOperands(call, true));
    IExpression operand = operands.poll();
    while (!operands.isEmpty()) {
      call = new Call(this, operand, operands.poll());
      call.inferReturnType();
      operand = call;
    }
    call = operand.asCall();
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Simplify NULL & A → NULL
    if (left.isNull()) {
      return new Literal(null, Types.INTEGER);
    }

    // Simplify 0 & A -> 0 (if A not nullable)
    if (Literal.ZERO.equals(left) && !right.getType().isNullable()) {
      return Literal.ZERO;
    }

    return call;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Long left = operands[0].getValue(Long.class);
    if (left == null) return null;
    Long right = operands[1].getValue(Long.class);
    if (right == null) return null;
    return left & right;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, 0, 0);
    writer.append('&');
    operands[1].unparse(writer, 0, 0);
  }
}
