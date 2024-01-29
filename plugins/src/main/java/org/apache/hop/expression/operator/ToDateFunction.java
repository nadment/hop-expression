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
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeId;
import org.apache.hop.expression.util.DateTimeFormat;
import java.math.BigDecimal;

/**
 * Converts a string expression to a date value with optional format or a Unix epoch numeric in second to a date.
 */
@FunctionPlugin
public class ToDateFunction extends Function {
  private static final ToDateFunction IntegerToDateFunction = new IntegerToDateFunction();
  private static final ToDateFunction NumberToDateFunction = new NumberToDateFunction();

  public ToDateFunction() {
    super("TO_DATE", ReturnTypes.DATE_NULLABLE, OperandTypes.STRING.or(OperandTypes.STRING_TEXT).or(OperandTypes.NUMERIC),
        OperatorCategory.CONVERSION, "/docs/to_date.html");
  }

  @Override
  public IExpression compile(final IExpressionContext context, final Call call)
      throws ExpressionException {
    Type type = call.getOperand(0).getType();

    if (type.is(TypeId.INTEGER) ) {
      return new Call(IntegerToDateFunction, call.getOperands());
    }
    if (type.is(TypeId.NUMBER) ) {
      return new Call(NumberToDateFunction, call.getOperands());
    }
    
    String pattern = context.getVariable(ExpressionContext.EXPRESSION_DATE_FORMAT);

    // With specified format
    if (call.getOperandCount() == 2) {
      pattern = call.getOperand(1).getValue(String.class);
    }

    int twoDigitYearStart = Integer
        .parseInt(context.getVariable(ExpressionContext.EXPRESSION_TWO_DIGIT_YEAR_START, "1970"));

    // Compile format to check it
    DateTimeFormat format = DateTimeFormat.of(pattern);
    format.setTwoDigitYearStart(twoDigitYearStart);

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
      if (value == null)
        return null;
      return format.parse(value);
    }
  }

  private static final class IntegerToDateFunction extends ToDateFunction {
    @Override
    public Object eval(final IExpression[] operands) {
      Long value = operands[0].getValue(Long.class);
      if (value == null)
        return null;
      return DateType.convertToDate(value);
    }
  }
  
  private static final class NumberToDateFunction extends ToDateFunction {
    @Override
    public Object eval(final IExpression[] operands) {
      BigDecimal value = operands[0].getValue(BigDecimal.class);
      if (value == null)
        return null;
      return DateType.convertToDate(value);
    }
  }
}
