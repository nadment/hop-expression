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
import org.apache.hop.expression.ExpressionContext;
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
import org.apache.hop.expression.util.DateTimeConversion;
import org.apache.hop.expression.util.DateTimeFormat;

/**
 * Converts a string expression to a date value with optional format or a Unix epoch numeric in
 * second to a date.
 */
@FunctionPlugin
public class ToDateFunction extends Function {

  public ToDateFunction() {
    super(
        "TO_DATE",
        ReturnTypes.DATE_NULLABLE,
        OperandTypes.STRING.or(OperandTypes.STRING_TEXT).or(OperandTypes.NUMBER),
        OperatorCategory.CONVERSION,
        "/docs/to_date.html");
  }

  @Override
  public IExpression compile(final IExpressionContext context, final Call call)
      throws ExpressionException {
    Type type = call.getOperand(0).getType();

    if (Types.isInteger(type)) {
      return new Call(IntegerToDateFunction.INSTANCE, call.getOperands());
    }
    if (Types.isNumber(type)) {
      return new Call(NumberToDateFunction.INSTANCE, call.getOperands());
    }

    // String with specified format
    String pattern =
        (call.getOperandCount() == 1) ? "AUTO" : call.getOperand(1).getValue(String.class);

    // Compile format to check it
    DateTimeFormat format = DateTimeFormat.of(pattern);
    format.setTwoDigitYearStart(
        (int) context.getAttribute(ExpressionContext.EXPRESSION_TWO_DIGIT_YEAR_START));

    return new Call(new StringToDateFunction(format), call.getOperands());
  }

  private static final class StringToDateFunction extends ToDateFunction {
    private final DateTimeFormat format;

    public StringToDateFunction(DateTimeFormat format) {
      super();
      this.format = format;
    }

    @Override
    public Object eval(final IExpression[] operands) {
      String value = operands[0].getValue(String.class);
      if (value == null) return null;
      return format.parse(value);
    }
  }

  private static final class IntegerToDateFunction extends ToDateFunction {
    private static final IntegerToDateFunction INSTANCE = new IntegerToDateFunction();

    private IntegerToDateFunction() {
      super();
    }

    @Override
    public Object eval(final IExpression[] operands) {
      Long value = operands[0].getValue(Long.class);
      if (value == null) return null;
      return DateTimeConversion.convert(value);
    }
  }

  private static final class NumberToDateFunction extends ToDateFunction {
    private static final ToDateFunction INSTANCE = new NumberToDateFunction();

    private NumberToDateFunction() {
      super();
    }

    @Override
    public Object eval(final IExpression[] operands) {
      BigDecimal value = operands[0].getValue(BigDecimal.class);
      if (value == null) return null;
      return DateTimeConversion.convert(value);
    }
  }
}
