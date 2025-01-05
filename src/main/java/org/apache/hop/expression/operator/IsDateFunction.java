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
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.TypeFamily;
import org.apache.hop.expression.type.Types;
import org.apache.hop.expression.util.DateTimeFormat;

/** Check if a value is a valid date. */
@FunctionPlugin
public class IsDateFunction extends Function {

  public IsDateFunction() {
    super(
        "IS_DATE",
        ReturnTypes.BOOLEAN_NOT_NULL,
        OperandTypes.ANY_STRING,
        OperatorCategory.COMPARISON,
        "/docs/is_date.html");
  }

  @Override
  public IExpression compile(final IExpressionContext context, final Call call)
      throws ExpressionException {

    if (Types.isString(call.getOperand(0).getType())) {
      Object value = call.getOperand(1).getValue();
      if (value instanceof DateTimeFormat) {
        // format already compiled
        return call;
      }

      String pattern = StringType.coerce(value);

      // Compile format to check it
      DateTimeFormat format = DateTimeFormat.of(pattern);
      format.setTwoDigitYearStart(
          (int) context.getAttribute(ExpressionContext.EXPRESSION_TWO_DIGIT_YEAR_START));

      return new Call(call.getOperator(), call.getOperand(0), Literal.of(format));
    }

    // Optimize "IS_DATE(d)" to "d IS NOT NULL"
    if (call.getOperand(0).getType().isFamily(TypeFamily.TEMPORAL)) {
      return new Call(Operators.IS_NOT_NULL, call.getOperand(0));
    }

    // Other data type are always false
    return Literal.FALSE;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);

    // Return FALSE if a value is NULL.
    if (value == null) return Boolean.FALSE;

    DateTimeFormat format = operands[1].getValue(DateTimeFormat.class);

    try {
      format.parse(value);
    } catch (Exception e) {
      return Boolean.FALSE;
    }

    return Boolean.TRUE;
  }
}
