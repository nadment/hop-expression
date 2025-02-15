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

import java.util.Locale;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Types;

/**
 * The function convert a string value to lower case.
 *
 * @see InitCapFunction
 * @see UpperFunction
 */
@FunctionPlugin
public class LowerFunction extends Function {

  public static final LowerFunction INSTANCE = new LowerFunction();

  public LowerFunction() {
    super(
        "LOWER",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING,
        OperatorCategory.STRING,
        "/docs/lower.html");
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coerceOperandType(call, call.getType(), 0);
  }

  @Override
  public Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null) return null;
    return value.toLowerCase(Locale.getDefault());
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression operand = call.getOperand(0);

    // Repetitions of functions that do not have any effects on the result
    if (operand.isOperator(this)
        || operand.isOperator(UpperFunction.INSTANCE)
        || operand.isOperator(InitCapFunction.INSTANCE)) {
      return new Call(this, call(operand).getOperand(0));
    }

    return call;
  }
}
