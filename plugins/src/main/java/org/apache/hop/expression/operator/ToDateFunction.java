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

import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Coerse;
import org.apache.hop.expression.util.DateTimeFormat;

/**
 * Converts a string expression to a date value.
 */
@FunctionPlugin
public class ToDateFunction extends Function {

  public ToDateFunction() {
    super("TO_DATE", true, ReturnTypes.DATE, OperandTypes.STRING_OPTIONAL_STRING, "i18n::Operator.Category.Conversion", "/docs/to_date.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].getValue(context);
    if (v0 == null)
      return null;

    String format = null;
    if (operands.length > 1) {
      Object v1 = operands[1].getValue(context);
      if (v1 != null)
        format = Coerse.toString(v1);
    } else {
      format = context.getVariable(ExpressionContext.EXPRESSION_DATE_FORMAT);
    }

    try {
      int twoDigitYearStart = Integer.parseInt(context.getVariable(ExpressionContext.EXPRESSION_TWO_DIGIT_YEAR_START, "1970"));
      DateTimeFormat formatter = DateTimeFormat.of(format);
      formatter.setTwoDigitYearStart(twoDigitYearStart);
      return formatter.parse(Coerse.toString(v0));
    } catch (Exception e) {
      throw new ExpressionException(ExpressionError.OPERATOR_ERROR, this.getName(),
          e.getMessage());
    }
  }
}
