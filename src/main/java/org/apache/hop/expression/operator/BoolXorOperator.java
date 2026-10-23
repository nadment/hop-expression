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
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Logical exclusive disjunction <code>XOR</code> operator.
 *
 * <ul>
 *   <li>True if one expression is non-zero and the other expression is zero.
 *   <li>False if both expressions are non-zero or both expressions are zero.
 *   <li>NULL if one or both expressions are NULL.
 * </ul>
 */
@NullMarked
public class BoolXorOperator extends BinaryOperator {

  public static final BoolXorOperator INSTANCE = new BoolXorOperator();

  public BoolXorOperator() {
    super(
        "BOOLXOR",
        "XOR",
        130,
        Associativity.LEFT,
        ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.BOOLEAN_BOOLEAN,
        OperatorCategory.LOGICAL,
        "/docs/hop-user-manual/expression/boolxor.html");
  }

  @Override
  public Operator reverse() {
    return this;
  }

  /** Simplifies XOR expressions whose answer can be determined without evaluating both sides. */
  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Coerce numeric operands to boolean before simplification, x XOR FALSE → x must be a boolean
    Types.coerceOperandsToBoolean(call);

    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // x XOR NULL → NULL
    if (left.isNull() || right.isNull()) {
      return Literal.NULL_BOOLEAN;
    }

    // x XOR FALSE → x
    if (Literal.FALSE.equals(right)) {
      return left;
    }
    if (Literal.FALSE.equals(left)) {
      return right;
    }

    // x XOR TRUE → NOT x
    if (Literal.TRUE.equals(right)) {
      return not(left);
    }
    if (Literal.TRUE.equals(left)) {
      return not(right);
    }

    // x XOR x → NVL2(x,FALSE,NULL), FALSE if x is not NULL, but NULL and not FALSE if x is NULL
    if (isSameExpression(left, right)) {
      Call predicate = new Call(Nvl2Function.INSTANCE, left, Literal.FALSE, Literal.NULL_BOOLEAN);
      predicate.inferReturnType();
      return predicate;
    }

    return call;
  }

  private static IExpression not(IExpression operand) {
    Call predicate = new Call(BoolNotFunction.INSTANCE, operand);
    predicate.inferReturnType();
    return predicate;
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coerceOperandsToBoolean(call);
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    Boolean left = operands[0].getValue(Boolean.class);
    if (left == null) {
      return null;
    }

    Boolean right = operands[1].getValue(Boolean.class);
    if (right == null) {
      return null;
    }
    return Boolean.logicalXor(left, right);
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, getLeftPrec(), getRightPrec());
    writer.append(" XOR ");
    operands[1].unparse(writer, getLeftPrec(), getRightPrec());
  }
}
