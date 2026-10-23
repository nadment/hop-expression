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
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Returns the second argument when the first argument is not NULL. If the first argument is NULL,
 * the third argument is returned.
 */
@FunctionPlugin
@NullMarked
public class Nvl2Function extends Function {

  public static final Function INSTANCE = new Nvl2Function();

  public Nvl2Function() {
    super(
        "NVL2",
        ReturnTypes.NVL2_FUNCTION,
        OperandTypes.ANY_SAME_SAME,
        OperatorCategory.CONDITIONAL,
        "/docs/hop-user-manual/expression/nvl2.html");
  }

  @Override
  public IExpression compile(final IExpressionContext context, final Call call)
      throws ExpressionException {
    IExpression ifNotNull = call.getOperand(1);
    IExpression ifNull = call.getOperand(2);

    // NVL2(x,TRUE,FALSE) → x IS NOT NULL
    if (Literal.TRUE.equals(ifNotNull) && Literal.FALSE.equals(ifNull)) {
      return new Call(IsNotNullOperator.INSTANCE, call.getOperand(0));
    }

    // NVL2(x,FALSE,TRUE) → x IS NULL
    if (Literal.FALSE.equals(ifNotNull) && Literal.TRUE.equals(ifNull)) {
      return new Call(IsNullOperator.INSTANCE, call.getOperand(0));
    }

    // NVL2(x,TRUE,NULL) is not simplified, it returns NULL and not FALSE if x is NULL

    return call;
  }

  @Override
  public boolean coerceOperandsType(Call call) {

    boolean coerce = Types.coerceOperandType(call, call.getType(), 1);
    coerce |= Types.coerceOperandType(call, call.getType(), 2);

    return coerce;
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    Object condition = operands[0].getValue();

    if (condition == null) {
      return operands[2].getValue();
    }

    return operands[1].getValue();
  }
}
