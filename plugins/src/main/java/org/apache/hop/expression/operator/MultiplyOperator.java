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
import java.io.StringWriter;
import java.math.BigDecimal;
import java.util.PriorityQueue;

/**
 * Arithmetic multiply operator.
 * <br>
 * <strong>Syntax:</strong> <code>x * y</code>
 */
public class MultiplyOperator extends Operator {
    
  private static final MultiplyOperator IntegerMultiplyOperator = new IntegerMultiplyOperator();
  private static final MultiplyOperator NumberMultiplyOperator = new NumberMultiplyOperator();
  
  public MultiplyOperator() {
    super("MULTIPLY", "*", 50, true, ReturnTypes.MULTIPLY_OPERATOR, OperandTypes.NUMERIC_NUMERIC,
        OperatorCategory.MATHEMATICAL, "/docs/multiply.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Reorder chained symmetric operator 
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

    // Simplify arithmetic 0*A → 0
    if (Literal.ZERO.equals(left)) {
      return Literal.ZERO;
    }
    
    // Simplify arithmetic 1*A → A
    if (Literal.ONE.equals(left)) {
      return right;
    }
    
    // Simplify arithmetic (-A)*(-B) → A*B
    if (left.is(Operators.NEGATE) && right.is(Operators.NEGATE)) {
      return new Call(Operators.MULTIPLY, left.asCall().getOperand(0),
          right.asCall().getOperand(0));
    }

    // Simplify arithmetic A*(1/B) → A/B
    if (right.is(Operators.DIVIDE) && Literal.ONE.equals(right.asCall().getOperand(0)) ) {
      return new Call(Operators.DIVIDE, left, right.asCall().getOperand(1));
    }
    
    // Simplify arithmetic A*A → SQUARE(A)
    if (left.equals(right)) {
      return new Call(SquareFunction.INSTANCE, left);
    }
    
    // Optimize data type
    if (call.getType().is(TypeId.INTEGER) ) {
      return new Call(IntegerMultiplyOperator, call.getOperands());
    }
    
    return new Call(NumberMultiplyOperator, call.getOperands());
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coercionArithmeticOperator(call);    
  }
  
  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append('*');
    operands[1].unparse(writer);
  }
  
  private static final class IntegerMultiplyOperator extends MultiplyOperator {
    @Override
    public Object eval(final IExpression[] operands) {
      Long left = operands[0].getValue(Long.class);
      if (left == null)
        return null;
      Long right = operands[1].getValue(Long.class);
      if (right == null)
        return null;

      return left*right;
    }
  }

  private static final class NumberMultiplyOperator extends MultiplyOperator {
    @Override
    public Object eval(final IExpression[] operands) {
      BigDecimal left = operands[0].getValue(BigDecimal.class);
      if (left == null)
        return null;
      BigDecimal right = operands[1].getValue(BigDecimal.class);
      if (right == null)
        return null;

      return left.multiply(right, MATH_CONTEXT);
    }
  }
}
