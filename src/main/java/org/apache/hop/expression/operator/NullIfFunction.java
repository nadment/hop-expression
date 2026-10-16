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
import org.apache.hop.expression.type.TypeTransforms;
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** The function NULLIF */
@FunctionPlugin
@NullMarked
public class NullIfFunction extends Function {
  public static final Function INSTANCE = new NullIfFunction();

  public NullIfFunction() {
    super(
        "NULLIF",
        // The result is NULL when the two expressions are equal
        ReturnTypes.ARG0.andThen(TypeTransforms.TO_NULLABLE),
        OperandTypes.SAME_SAME,
        OperatorCategory.CONDITIONAL,
        "/docs/nullif.html");
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {

    Object value = operands[0].getValue();
    if (value == null) return null;
    Type type = operands[0].getType();
    Object compare = operands[1].getValue();

    if (compare != null && type.compareEqual(value, compare)) return null;

    return value;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    // Simplify NullIf(NULL,x) → NULL
    if (call.getOperand(0).isNull()) {
      return new Literal(null, call.getType());
    }

    // Simplify NullIf(x,NULL) → x, because x is never equal to NULL
    if (call.getOperand(1).isNull()) {
      return call.getOperand(0);
    }

    return call;
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coercionComparisonOperator(call);
  }
}
