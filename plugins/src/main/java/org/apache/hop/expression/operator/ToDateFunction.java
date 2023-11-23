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
import org.apache.hop.expression.Category;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.util.DateTimeFormat;

/**
 * Converts a string expression to a date value with optional format.
 */
@FunctionPlugin
public class ToDateFunction extends Function {

  public ToDateFunction() {
    this("TO_DATE");
  }

  protected ToDateFunction(String id) {
    super(id, ReturnTypes.DATE, OperandTypes.STRING.or(OperandTypes.STRING_TEXT),
        Category.CONVERSION, "/docs/to_date.html");
  }

  @Override
  public IExpression compile(final IExpressionContext context, final Call call)
      throws ExpressionException {
    String pattern = context.getVariable(ExpressionContext.EXPRESSION_DATE_FORMAT);

    // With specified format
    if (call.getOperandCount() == 2) {
      Object value = call.getOperand(1).getValue();
      if (value instanceof DateTimeFormat) {
        // Already compiled
        return call;
      }
      pattern = StringType.coerce(value);
    }

    int twoDigitYearStart = Integer
        .parseInt(context.getVariable(ExpressionContext.EXPRESSION_TWO_DIGIT_YEAR_START, "1970"));

    // Compile format to check it
    DateTimeFormat format = DateTimeFormat.of(pattern);
    format.setTwoDigitYearStart(twoDigitYearStart);

    return new Call(call.getOperator(), call.getOperand(0), Literal.of(format));
  }

  @Override
  public Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null)
      return null;

    DateTimeFormat format = operands[1].getValue(DateTimeFormat.class);

    return format.parse(value);
  }
}
