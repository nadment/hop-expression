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
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Prefix arithmetic plus (positive) operator '<code>+</code>' for numeric or interval. It only
 * checks the data type of the operand and is removed at compile time.
 */
@NullMarked
public class PositiveOperator extends PrefixUnaryOperator {
  public static final PositiveOperator INSTANCE = new PositiveOperator();

  public PositiveOperator() {
    super(
        "POSITIVE",
        "+",
        40,
        Associativity.RIGHT,
        ReturnTypes.ARG0,
        OperandTypes.INTEGER.or(OperandTypes.NUMBER).or(OperandTypes.INTERVAL),
        OperatorCategory.MATHEMATICAL,
        "/docs/plus.html");
  }

  @Override
  public boolean checkOperandTypes(Call call) {
    // A boolean is not implicitly coerced to a number
    return super.checkOperandTypes(call) && !Types.isBoolean(call.getOperand(0).getType());
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coerceOperandType(call, call.getType(), 0);
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    // Simplify +A → A
    return call.getOperand(0);
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    return operands[0].getValue();
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append('+');
    operands[0].unparse(writer, getLeftPrec(), getRightPrec());
  }
}
