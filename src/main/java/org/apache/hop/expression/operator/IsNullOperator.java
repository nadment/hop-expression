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
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * An operator describing the <code>IS NULL</code> operator.
 *
 * @see IsNotNullOperator
 */
public class IsNullOperator extends PostfixUnaryOperator {

  public static final IsNullOperator INSTANCE = new IsNullOperator();

  public IsNullOperator() {
    super(
        "IS NULL",
        100,
        Associativity.LEFT,
        ReturnTypes.BOOLEAN_NOT_NULL,
        OperandTypes.ANY,
        OperatorCategory.COMPARISON,
        "/docs/is-null.html");
  }

  @Override
  public Operator not() {
    return IsNotNullOperator.INSTANCE;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression operand = call.getOperand(0);

    // If the operand is not nullable, always false
    if (!operand.getType().isNullable()) {
      return Literal.FALSE;
    }

    // Simplify CAST(x AS type) IS NULL → x IS NULL
    if (operand.isOperator(CastOperator.INSTANCE)) {
      return new Call(call.getOperator(), call(operand).getOperands());
    }

    return call;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    return operands[0].getValue() == null;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, getLeftPrec(), getRightPrec());
    writer.append(" IS NULL");
  }
}
