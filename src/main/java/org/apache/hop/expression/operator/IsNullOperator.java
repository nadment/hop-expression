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
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;

/**
 * An operator describing the <code>IS NULL</code> operator.
 *
 * @see IsNotNullOperator
 */
@NullMarked
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
    if (isSafeCast(operand)) {
      return new Call(call.getOperator(), call(operand).getOperand(0));
    }

    return call;
  }

  /**
   * Returns whether the expression is a cast that can neither fail nor return NULL for a value that
   * is not NULL, so that the cast can be removed when checking if the value is NULL. A cast to a
   * string without format of an atomic value never fails, nor does a loss-less cast, but a cast
   * like CAST('abc' AS INTEGER) raises an error.
   *
   * @param operand the operand of IS NULL or IS NOT NULL
   * @return true if the cast can be removed
   */
  static boolean isSafeCast(IExpression operand) {
    if (!operand.isOperator(CastOperator.INSTANCE)) {
      return false;
    }
    Call cast = call(operand);
    // A format can fail to apply
    if (cast.getOperandCount() > 2) {
      return false;
    }
    Type source = cast.getOperand(0).getType();
    Type target = cast.getType();
    return (Types.isString(target) && source.isAtomic()) || Types.isLosslessCast(source, target);
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
