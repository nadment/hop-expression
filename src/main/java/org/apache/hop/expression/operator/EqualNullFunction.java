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
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;

/**
 * Compares whether two expressions are equal.
 *
 * <p>The function is NULL-safe, meaning it treats NULLs as known values for comparing equality.
 * Note that this is different from the EQUAL comparison operator (=), which treats NULLs as unknown
 * values.
 */
@FunctionPlugin
public class EqualNullFunction extends Function {

  public EqualNullFunction() {
    super(
        "EQUAL_NULL",
        ReturnTypes.BOOLEAN_NOT_NULL,
        OperandTypes.ANY_ANY,
        OperatorCategory.COMPARISON,
        "/docs/equal_null.html");
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Type type = operands[0].getType();
    Object left = operands[0].getValue();
    Object right = operands[1].getValue();
    return type.compareEqualNull(left, right);
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Normalize
    call = normalizeSymmetricalPredicate(call);

    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Simplify same expressions.
    if (left.equals(right)) {
      return Literal.TRUE;
    }

    // Simplify if one of the operands is NULL, then it can be simplified to the IS NULL predicate.
    if (left.isNull()) {
      return new Call(IsNullOperator.INSTANCE, right);
    }
    if (right.isNull()) {
      return new Call(IsNullOperator.INSTANCE, left);
    }

    // Simplify EQUAL_NULL(x,TRUE) → x IS TRUE
    if (right.equals(Literal.TRUE)) {
      return new Call(IsTrueOperator.INSTANCE, left);
    }
    // Simplify EQUAL_NULL(x,FALSE) → x IS FALSE
    if (right.equals(Literal.FALSE)) {
      return new Call(IsFalseOperator.INSTANCE, left);
    }

    return call;
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coercionComparisonOperator(call);
  }
}
