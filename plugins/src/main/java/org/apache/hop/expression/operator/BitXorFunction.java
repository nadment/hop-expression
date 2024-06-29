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
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Bitwise XOR operator. <br>
 * <strong>Syntax:</strong> <code>x ^ y</code>
 */
@FunctionPlugin
public class BitXorFunction extends Function {

  public BitXorFunction() {
    super(
        "BIT_XOR",
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.NUMERIC_NUMERIC,
        OperatorCategory.BITWISE,
        "/docs/bit_xor.html");
  }

  public BitXorFunction(String name) {
    super(
        "BIT_XOR",
        name,
        80,
        true,
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.NUMERIC_NUMERIC,
        OperatorCategory.BITWISE,
        "/docs/bit_xor.html");
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Simplify A ^ NULL → NULL
    // Simplify NULL ^ A → NULL
    if (left.isNull() || right.isNull()) {
      return Literal.NULL_INTEGER;
    }

    // Simplify 0 ^ A → A (even if A is not nullable)
    if (Literal.ZERO.equals(left)) {
      return right;
    }
    if (Literal.ZERO.equals(right)) {
      return left;
    }

    // TODO: Simplify A ^ (..A..) → (the expression without A, if number of A is odd, otherwise
    // one A)

    return call;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Long left = operands[0].getValue(Long.class);
    if (left == null) return null;
    Long right = operands[1].getValue(Long.class);
    if (right == null) return null;

    return left ^ right;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, 0, 0);
    writer.append('^');
    operands[1].unparse(writer, 0, 0);
  }
}
