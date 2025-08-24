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
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionComparator;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;

/**
 * Arithmetic multiply operator. <br>
 * <strong>Syntax:</strong> <code>x * y</code>
 */
public class MultiplyOperator extends BinaryOperator {
  public static final MultiplyOperator INSTANCE = new MultiplyOperator();
  private static final Literal MINUS_ONE = Literal.of(new BigDecimal(-1L));

  public MultiplyOperator() {
    super(
        "MULTIPLY",
        "*",
        50,
        Associativity.LEFT,
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

    // Simplify arithmetic -1 * A → -A
    //    if (MINUS_ONE.equals(call.getOperand(0))) {
    //      return new Call(NegateOperator.INSTANCE, call.getOperand(1));
    //    }

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

      // NULL as soon any left is NULL
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
      if (operand.isOperator(NegateOperator.INSTANCE)) {
        negateTerms.add((Call) operand);
      }
      if (operand.isOperator(DivOperator.INSTANCE)) {
        divideTerms.add((Call) operand);
      }
    }

    // Simplify arithmetic 0 * A → 0 (if A is not nullable)
    if (!zeroTerms.isEmpty() && nullableTerms.isEmpty()) {
      return Literal.ZERO;
    }

    // Simplify arithmetic 1 * A → A
    if (!oneTerms.isEmpty()) {
      for (IExpression term : oneTerms) {
        if (operands.size() > 1) {
          operands.remove(term);
        }
      }
    }

    // Simplify arithmetic (-A) * (-B) → A * B
    int negateCount = negateTerms.size();
    if (negateCount > 1) {
      // Remove all pairs of negate operators else Keep one negate
      int i = negateCount % 2;
      for (Call term : negateTerms) {
        if (i <= 0) {
          operands.add(term.getOperand(0));
          operands.remove(term);
          i--;
        }
      }
    }

    // Simplify arithmetic only if one negate const * (-A) → -const * A
    else if (negateCount == 1 && !constantTerms.isEmpty()) {
      Call term = negateTerms.get(0);
      // operands.remove(term);
      operands.add(term.getOperand(0));
      IExpression constant = constantTerms.get(0);
      operands.remove(constant);
      term.setOperand(0, constant);
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

    IExpression left = operands.poll();
    while (!operands.isEmpty()) {
      IExpression right = operands.poll();
      Type type = ReturnTypes.deriveMultiplyType(left.getType(), right.getType());
      Operator operator =
          (Types.isInteger(type))
              ? IntegerMultiplyOperator.INSTANCE
              : NumberMultiplyOperator.INSTANCE;
      call = new Call(operator, left, right);
      call.inferReturnType();
      left = call;
    }
    return left;
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coercionMultiplyOperator(call);
  }

  public static final class IntegerMultiplyOperator extends MultiplyOperator {
    public static final MultiplyOperator INSTANCE = new IntegerMultiplyOperator();

    @Override
    public Object eval(final IExpression[] operands) {
      Long left = operands[0].getValue(Long.class);
      if (left == null) return null;
      Long right = operands[1].getValue(Long.class);
      if (right == null) return null;

      try {
        return Math.multiplyExact(left, right);
      } catch (ArithmeticException e) {
        throw new ExpressionException(ErrorCode.ARITHMETIC_OVERFLOW, getName());
      }
    }
  }

  public static final class NumberMultiplyOperator extends MultiplyOperator {
    public static final MultiplyOperator INSTANCE = new NumberMultiplyOperator();

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
