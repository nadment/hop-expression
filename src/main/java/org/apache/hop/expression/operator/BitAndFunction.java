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
import org.apache.hop.expression.ErrorCode;
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
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeTransforms;
import org.apache.hop.expression.type.Types;

/**
 * Bitwise AND operator.<br>
 * <strong>Syntax:</strong> <code>x &amp; y</code>
 */
@FunctionPlugin
public class BitAndFunction extends Function {

  public static final BitAndFunction INSTANCE = new BitAndFunction("&");

  public BitAndFunction() {
    super(
        "BIT_AND",
        ReturnTypes.LEAST_RESTRICTIVE.andThen(TypeTransforms.TO_NULLABLE),
        OperandTypes.INTEGER_INTEGER.or(OperandTypes.BINARY_BINARY),
        OperatorCategory.BITWISE,
        "/docs/bit_and.html");
  }

  public BitAndFunction(String name) {
    super(
        "BIT_AND",
        name,
        70,
        Associativity.LEFT,
        ReturnTypes.LEAST_RESTRICTIVE.andThen(TypeTransforms.TO_NULLABLE),
        OperandTypes.INTEGER_INTEGER.or(OperandTypes.BINARY_BINARY),
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
    operands.addAll(call.getChainedOperands(true));
    IExpression operand = operands.poll();
    while (!operands.isEmpty()) {
      call = new Call(this, operand, operands.poll());
      call.inferReturnType();
      operand = call;
    }
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Simplify A & NULL → NULL
    // Simplify NULL & A → NULL
    if (left.isNull() || right.isNull()) {
      return Literal.NULL_INTEGER;
    }

    Type type = left.getType();
    if (Types.isBinary(type)) {
      return new Call(BinaryBitAndFunction.INSTANCE, call.getOperands());
    }

    return new Call(IntegerBitAndFunction.INSTANCE, call.getOperands());
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, 0, 0);
    writer.append('&');
    operands[1].unparse(writer, 0, 0);
  }

  public static final class IntegerBitAndFunction extends BitAndFunction {
    public static final IntegerBitAndFunction INSTANCE = new IntegerBitAndFunction();

    @Override
    public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
      IExpression left = call.getOperand(0);
      IExpression right = call.getOperand(1);

      // Simplify 0 & A -> 0 (if A not nullable)
      if (Literal.ZERO.equals(left) && !right.getType().isNullable()) {
        return Literal.ZERO;
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
      return left & right;
    }
  }

  public static final class BinaryBitAndFunction extends BitAndFunction {
    public static final BinaryBitAndFunction INSTANCE = new BinaryBitAndFunction();

    @Override
    public Object eval(final IExpression[] operands) {
      byte[] left = operands[0].getValue(byte[].class);
      if (left == null) return null;
      byte[] right = operands[1].getValue(byte[].class);
      if (right == null) return null;

      if (left.length != right.length) {
        throw new ExpressionException(ErrorCode.INVALID_BITWISE_OPERANDS_SIZE);
      }

      final byte[] result = new byte[left.length];
      for (int i = result.length - 1; i >= 0; i--) {
        result[i] = (byte) (left[i] & right[i]);
      }

      return result;
    }
  }
}
