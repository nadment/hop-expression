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
import java.math.BigDecimal;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
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
 * Arithmetic modulus operator. <br>
 * <strong>Syntax:</strong> <code>x % y</code>
 */
@FunctionPlugin
public class ModFunction extends Function {

  public ModFunction() {
    super(
        "MOD",
        ReturnTypes.MOD_OPERATOR,
        OperandTypes.NUMBER_NUMBER,
        OperatorCategory.MATHEMATICAL,
        "/docs/mod.html");
  }

  public ModFunction(String name) {
    super(
        "MOD",
        name,
        50,
        true,
        ReturnTypes.NUMBER_NULLABLE,
        OperandTypes.NUMBER_NUMBER,
        OperatorCategory.MATHEMATICAL,
        "/docs/mod.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Simplify A % NULL → NULL
    if (right.isNull()) {
      return Literal.NULL;
    }

    // Simplify NULL % A → NULL
    if (left.isNull()) {
      return Literal.NULL;
    }

    // Simplify arithmetic A % 1 → A
    if (Literal.ONE.equals(right)) {
      return call.getOperand(0);
    }

    return call;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    BigDecimal value = operands[0].getValue(BigDecimal.class);
    if (value == null) return null;
    BigDecimal divisor = operands[1].getValue(BigDecimal.class);
    if (divisor == null) return null;

    // Prevent a division by zero ..
    if (divisor.signum() == 0) throw new ExpressionException(ErrorCode.DIVISION_BY_ZERO);

    return value.remainder(divisor);
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coercionMultiplyOperator(call);
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, getLeftPrec(), getRightPrec());
    writer.append('%');
    operands[1].unparse(writer, getLeftPrec(), getRightPrec());
  }
}
