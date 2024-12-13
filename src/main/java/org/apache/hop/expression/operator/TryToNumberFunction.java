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

import java.time.ZonedDateTime;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;
import org.apache.hop.expression.util.FormatParseException;
import org.apache.hop.expression.util.NumberFormat;

/** Converts a string expression to a number value with optional format. */
@FunctionPlugin
public class TryToNumberFunction extends Function {

  public TryToNumberFunction() {
    super(
        "TRY_TO_NUMBER",
        ReturnTypes.NUMBER_NULLABLE,
        OperandTypes.STRING.or(OperandTypes.STRING_TEXT).or(OperandTypes.DATE),
        OperatorCategory.CONVERSION,
        "/docs/to_number.html");
  }

  @Override
  public IExpression compile(final IExpressionContext context, final Call call)
      throws ExpressionException {

    Type type = call.getOperand(0).getType();
    if (Types.isDate(type)) {
      return new Call(TryToNumberDate.INSTANCE, call.getOperands());
    }

    String pattern = "TM";
    // Format specified
    if (call.getOperandCount() == 2) {
      pattern = call.getOperand(1).getValue(String.class);
    }

    // Compile format to check it
    NumberFormat format = NumberFormat.of(pattern);
    return new Call(new TryToNumberString(format), call.getOperands());
  }

  private static final class TryToNumberString extends TryToNumberFunction {
    private final NumberFormat format;

    public TryToNumberString(NumberFormat format) {
      super();
      this.format = format;
    }

    @Override
    public Object eval(final IExpression[] operands) {
      String value = operands[0].getValue(String.class);
      if (value == null) return null;

      try {
        return format.parse(value);
      } catch (FormatParseException e) {
        return null;
      }
    }
  }

  private static final class TryToNumberDate extends TryToNumberFunction {
    private static final TryToNumberFunction INSTANCE = new TryToNumberDate();

    private TryToNumberDate() {
      super();
    }

    @Override
    public Object eval(final IExpression[] operands) {
      ZonedDateTime value = operands[0].getValue(ZonedDateTime.class);
      if (value == null) return null;
      try {
        return NumberType.convert(value);
      } catch (RuntimeException e) {
        return null;
      }
    }
  }
}
