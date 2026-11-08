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
        Associativity.NONE,
        ReturnTypes.BOOLEAN_NOT_NULL,
        OperandTypes.ANY,
        OperatorCategory.COMPARISON,
        "/docs/hop-user-manual/expression/is-null.html");
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

    // Simplify CAST(x AS type) IS NULL → x IS NULL, NOT(x) IS NULL → x IS NULL
    if (isNullOnlyIfOperandIsNull(operand)) {
      return new Call(call.getOperator(), call(operand).getOperand(0));
    }

    return call;
  }

  /**
   * Returns whether the expression is NULL only if its operand is NULL, and can't fail, so that it
   * can be removed when checking if the value is NULL, like NOT(x) or CAST(x AS STRING).
   *
   * @param operand the operand of IS NULL or IS NOT NULL
   * @return true if the expression can be replaced by its operand
   */
  static boolean isNullOnlyIfOperandIsNull(IExpression operand) {
    // NOT(x) is NULL only if x is NULL, its operand is a boolean or a number that can't fail to be
    // converted to a boolean
    return operand.isOperator(BoolNotFunction.INSTANCE) || isSafeCast(operand);
  }

  /**
   * Returns whether the expression is a cast that can neither fail nor return NULL for a value that
   * is not NULL, so that the cast can be removed when checking if the value is NULL. A cast to a
   * string without format of an atomic value never fails, nor does a cast of a number to a boolean
   * or a loss-less cast, but a cast like CAST('abc' AS INTEGER) raises an error.
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
    // A number is converted to a boolean without error, 0 is FALSE and any other value is TRUE
    return (Types.isString(target) && source.isAtomic())
        || (Types.isBoolean(target) && Types.isNumeric(source))
        || Types.isLosslessCast(source, target);
  }

  @Override
  public Object eval(final IExpression[] operands) {
    return operands[0].getValue() == null;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, 0, getLeftPrecedence());
    writer.append(" IS NULL");
  }
}
