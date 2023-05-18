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

import org.apache.commons.math3.util.FastMath;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.Converter;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Returns the absolute (positive) value of the numeric value.
 */
@FunctionPlugin
public class AbsFunction extends Function {

  public AbsFunction() {
    super("ABS", ReturnTypes.ARG0, OperandTypes.NUMERIC, OperatorCategory.MATHEMATICAL,
        "/docs/abs.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    Object value = operands[0].getValue(context);
    if (value == null)
      return value;

    if (value instanceof Double) {
      return FastMath.abs((double) value);
    }
    if (value instanceof Long) {
      return FastMath.abs((long) value);
    }

    return Converter.coerceToBigNumber(value).abs();
  }
  
  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    // Idempotent function repetition
    if ( call.getOperand(0).is(call.getOperator())) {
      return call.getOperand(0);
    }
    
    return call;
  }
}
