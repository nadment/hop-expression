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
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.TimestampConversion;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Converts a string expression to a timestamp with optional format, the time zone of the string is
 * kept, or a Unix epoch numeric in second to a timestamp at its instant in UTC.
 */
@FunctionPlugin
@NullMarked
public class ToTimestampFunction extends Function {

  public static final ToTimestampFunction INSTANCE = new ToTimestampFunction();

  public ToTimestampFunction() {
    super(
        "TO_TIMESTAMP",
        ReturnTypes.TIMESTAMP_NULLABLE,
        OperandTypes.STRING.or(OperandTypes.STRING_TEXT).or(OperandTypes.NUMBER),
        OperatorCategory.CONVERSION,
        "/docs/hop-user-manual/expression/to_timestamp.html");
  }

  @Override
  public IExpression compile(final IExpressionContext context, final Call call)
      throws ExpressionException {
    Type type = call.getOperand(0).getType();

    if (Types.isInteger(type)) {
      return new Call(IntegerToTimestampFunction.INSTANCE, call.getOperands());
    }
    if (Types.isNumber(type)) {
      return new Call(NumberToTimestampFunction.INSTANCE, call.getOperands());
    }

    // String with specified format
    String pattern =
        (call.getOperandCount() == 1) ? "AUTO" : call.getOperand(1).getValue(String.class);

    // Compile format to check it
    Object start = context.getAttribute(ExpressionContext.EXPRESSION_TWO_DIGIT_YEAR_START);
    DateTimeFormat format =
        DateTimeFormat.of(
            pattern,
            (start instanceof Integer year) ? year : DateTimeFormat.DEFAULT_TWO_DIGIT_YEAR_START);

    return new Call(new StringToTimestampFunction(format), call.getOperands());
  }

  public static final class StringToTimestampFunction extends ToTimestampFunction {
    public final DateTimeFormat format;

    public StringToTimestampFunction(DateTimeFormat format) {
      super();
      this.format = format;
    }

    @Override
    public @Nullable Object eval(final IExpression[] operands) {
      String value = operands[0].getValue(String.class);
      if (value == null) return null;
      // The time zone of the string is kept, without time zone it is the system time zone
      return format.parse(value);
    }
  }

  public static final class IntegerToTimestampFunction extends ToTimestampFunction {
    public static final IntegerToTimestampFunction INSTANCE = new IntegerToTimestampFunction();

    @Override
    public @Nullable Object eval(final IExpression[] operands) {
      Long value = operands[0].getValue(Long.class);
      if (value == null) return null;
      return TimestampConversion.convert(value);
    }
  }

  public static final class NumberToTimestampFunction extends ToTimestampFunction {
    public static final ToTimestampFunction INSTANCE = new NumberToTimestampFunction();

    @Override
    public @Nullable Object eval(final IExpression[] operands) {
      BigDecimal value = operands[0].getValue(BigDecimal.class);
      if (value == null) return null;
      return TimestampConversion.convert(value);
    }
  }
}
