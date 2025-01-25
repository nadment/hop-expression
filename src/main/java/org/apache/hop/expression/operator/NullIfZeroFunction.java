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

import java.math.BigDecimal;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;

/** Returns NULL if the argument evaluates to 0; otherwise, returns the argument. */
@FunctionPlugin
public class NullIfZeroFunction extends Function {

  public NullIfZeroFunction() {
    super(
        "NULLIFZERO",
        ReturnTypes.ARG0,
        OperandTypes.INTEGER.or(OperandTypes.NUMBER),
        OperatorCategory.CONDITIONAL,
        "/docs/nullifzero.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    Type type = call.getOperand(0).getType();
    if (Types.isInteger(type)) {
      return new Call(IntegerNullIfZeroFunction.INSTANCE, call.getOperands());
    }

    return new Call(NumberNullIfZeroFunction.INSTANCE, call.getOperands());
  }

  public static final class NumberNullIfZeroFunction extends NullIfZeroFunction {
    public static final NumberNullIfZeroFunction INSTANCE = new NumberNullIfZeroFunction();

    @Override
    public Object eval(final IExpression[] operands) {
      BigDecimal value = operands[0].getValue(BigDecimal.class);
      if (value == null || value.signum() == 0) {
        return null;
      }
      return value;
    }
  }

  public static final class IntegerNullIfZeroFunction extends NullIfZeroFunction {
    public static final IntegerNullIfZeroFunction INSTANCE = new IntegerNullIfZeroFunction();

    @Override
    public Object eval(final IExpression[] operands) {
      Long value = operands[0].getValue(Long.class);
      if (value == null || value == 0) {
        return null;
      }
      return value;
    }
  }
}
