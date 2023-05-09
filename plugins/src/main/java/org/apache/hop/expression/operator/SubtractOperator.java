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
import org.apache.hop.expression.type.Converter;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.StringWriter;
import java.math.BigDecimal;

/**
 * Arithmetic subtraction operator.
 * <br>
 * <strong>Syntax:</strong> <code>x - y</code>
 */
public class SubtractOperator extends Operator {

  protected static final double SECONDS_BY_DAY = 24D * 60 * 60;

  public SubtractOperator() {
    super("SUBTRACT", "-", 100, true, ReturnTypes.LEAST_RESTRICTIVE,
        OperandTypes.NUMERIC_NUMERIC, OperatorCategory.MATHEMATICAL, "/docs/subtract.html");
  }

  /**
   * Simplify arithmetic subtract
   */
  @Override
  public IExpression compile(IExpressionContext context, Call call)
      throws ExpressionException {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Simplify arithmetic A-0 => A
    if (Literal.ZERO.equals(right)) {
      return left;
    }
    
    // Simplify arithmetic 0-A => -A
    if (Literal.ZERO.equals(left)) {
      return new Call(Operators.NEGATIVE, right);
    }

    // Simplify arithmetic A-(-B) => A+B
    if (right.is(Operators.NEGATIVE)) {
      Call negative = (Call) right;
      return new Call(Operators.ADD, left, negative.getOperand(0));
    }

    return call;
  }
  
  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands) throws Exception {
    Object left = operands[0].getValue(context);
    if (left == null)
      return null;
    Object right = operands[1].getValue(context);
    if (right == null)
      return null;

    if (left instanceof BigDecimal || right instanceof BigDecimal) {
      return Converter.coerceToBigNumber(left).subtract(Converter.coerceToBigNumber(right));
    }
    if (left instanceof Double || right instanceof Double) {
      return Converter.coerceToNumber(left) - Converter.coerceToNumber(right);
    }
    if (left instanceof Long || right instanceof Long) {
      return Converter.coerceToInteger(left) - Converter.coerceToInteger(right);
    }

    return Converter.coerceToBigNumber(left).subtract(Converter.coerceToBigNumber(right));
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append('-');
    operands[1].unparse(writer);
  }
}
