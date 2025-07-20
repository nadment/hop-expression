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

import org.apache.commons.lang3.StringUtils;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * The function removes leading characters from a string.
 *
 * @see TrimFunction
 * @see RTrimFunction
 */
@FunctionPlugin
public class LTrimFunction extends Function {
  public static final LTrimFunction INSTANCE = new LTrimFunction();

  public LTrimFunction() {
    super(
        "LTRIM",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING.or(OperandTypes.STRING_STRING),
        OperatorCategory.STRING,
        "/docs/ltrim.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null) return null;

    String stripChars = null;

    if (operands.length == 2) {
      stripChars = operands[1].getValue(String.class);
      if (stripChars == null) return null;
    }

    return StringUtils.stripStart(value, stripChars);
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    if (call.getOperandCount() == 1) {
      IExpression operand = call.getOperand(0);

      // Repetitions of the same function
      if (operand.isOperator(this)) {
        return operand;
      }

      // Repetitions of functions that do not have any effects on the result
      // LTRIM(RTRIM(x)) → TRIM(x)
      // LTRIM(TRIM(x)) → TRIM(x)
      if (operand.isOperator(TrimFunction.INSTANCE) || operand.isOperator(RTrimFunction.INSTANCE)) {
        return new Call(TrimFunction.INSTANCE, call(operand).getOperand(0));
      }
    }
    return call;
  }
}
