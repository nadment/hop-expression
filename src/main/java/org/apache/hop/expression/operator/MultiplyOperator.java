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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.PriorityQueue;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionComparator;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.TypeId;
import org.apache.hop.expression.type.Types;

/**
 * Arithmetic multiply operator. <br>
 * <strong>Syntax:</strong> <code>x * y</code>
 */
public class MultiplyOperator extends BinaryOperator {

  public MultiplyOperator() {
    super(
        "MULTIPLY",
        "*",
        50,
        true,
        ReturnTypes.MULTIPLY_OPERATOR,
        OperandTypes.NUMERIC_NUMERIC,
        OperatorCategory.MATHEMATICAL,
        "/docs/multiply.html");
  }

  @Override
  public Operator reverse() {
    return this;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    // Reorder chained symmetric operator
    PriorityQueue<IExpression> operands = new PriorityQueue<>(new ExpressionComparator());
    operands.addAll(call.getChainedOperands(true));

    final List<IExpression> zeroTerms = new ArrayList<>();
    final List<IExpression> oneTerms = new ArrayList<>();
    final List<IExpression> constantTerms = new ArrayList<>();
    final List<IExpression> nullableTerms = new ArrayList<>();
    final List<Call> divideTerms = new ArrayList<>();
    final List<Call> negateTerms = new ArrayList<>();

    for (IExpression operand : operands) {

      // NULL as soon any operand is NULL
      if (operand.isNull()) {
        return Literal.NULL;
      }
      if (Literal.ZERO.equals(operand)) {
        zeroTerms.add(operand);
      }
      if (Literal.ONE.equals(operand)) {
        oneTerms.add(operand);
      }
      if (operand.isConstant()) {
        constantTerms.add(operand);
      }
      if (operand.getType().isNullable()) {
        nullableTerms.add(operand);
      }
      if (operand.isOperator(Operators.NEGATE)) {
        negateTerms.add(operand.asCall());
      }
      if (operand.isOperator(Operators.DIVIDE)) {
        divideTerms.add(operand.asCall());
      }
    }

    // Simplify arithmetic 0 * A → 0 (if A is not nullable)
    if (!zeroTerms.isEmpty() && nullableTerms.isEmpty()) {
      return Literal.ZERO;
    }

    // Simplify arithmetic 1 * A → A
    if (!oneTerms.isEmpty()) {
      // operands.removeAll(oneTerms);
      for (IExpression term : oneTerms) {
        if (operands.size() > 1) {
          operands.remove(term);
        }
      }
    }

    // Simplify arithmetic (-A) * (-B) → A * B
    int negateCount = negateTerms.size();
    if (negateCount > 1) {
      // Remove all negate if pair else Keep one negate
      int i = negateCount % 2;
      for (Call term : negateTerms) {
        if (i <= 0) {
          operands.add(term.getOperand(0));
          operands.remove(term);
          i--;
        }
      }
    }

    // Simplify arithmetic A * (1 / B) → A / B
    for (Call term : divideTerms) {
      if (Literal.ONE.equals(term.getOperand(0))) {
        operands.remove(term);
        operands.add(term.getOperand(1));
      }
    }

    // Rebuild chain
    if (operands.size() == 1) {
      return operands.peek();
    }

    IExpression operand = operands.poll();
    while (!operands.isEmpty()) {
      call = new Call(Operators.MULTIPLY, operand, operands.poll());
      call.inferReturnType();

      // Optimize data type
      if (call.getType().is(TypeId.INTEGER)) {
        call.setOperator(MultiplyInteger.INSTANCE);
      } else {
        call.setOperator(MultiplyNumber.INSTANCE);
      }

      operand = call;
    }
    call = operand.asCall();

    return call;
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coercionMultiplyOperator(call);
  }

  private static final class MultiplyInteger extends MultiplyOperator {
    private static final MultiplyOperator INSTANCE = new MultiplyInteger();

    @Override
    public Object eval(final IExpression[] operands) {
      Long left = operands[0].getValue(Long.class);
      if (left == null) return null;
      Long right = operands[1].getValue(Long.class);
      if (right == null) return null;

      return left * right;
    }
  }

  private static final class MultiplyNumber extends MultiplyOperator {
    private static final MultiplyOperator INSTANCE = new MultiplyNumber();

    @Override
    public Object eval(final IExpression[] operands) {
      BigDecimal left = operands[0].getValue(BigDecimal.class);
      if (left == null) return null;
      BigDecimal right = operands[1].getValue(BigDecimal.class);
      if (right == null) return null;

      return left.multiply(right, MATH_CONTEXT);
    }
  }
}
