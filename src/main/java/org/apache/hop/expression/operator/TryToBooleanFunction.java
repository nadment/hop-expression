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
import org.apache.hop.expression.ConversionException;
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
import org.apache.hop.expression.util.BooleanConversion;

/** Converts a string or numeric expression to a boolean value. */
@FunctionPlugin
public class TryToBooleanFunction extends Function {

  public static final TryToBooleanFunction INSTANCE = new TryToBooleanFunction();

  public TryToBooleanFunction() {
    super(
        "TRY_TO_BOOLEAN",
        ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.STRING.or(OperandTypes.INTEGER).or(OperandTypes.NUMBER),
        OperatorCategory.CONVERSION,
        "/docs/to_boolean.html");
  }

  @Override
  public IExpression compile(final IExpressionContext context, final Call call)
      throws ExpressionException {
    Type type = call.getOperand(0).getType();
    if (Types.isInteger(type)) {
      return new Call(IntegerTryToBooleanFunction.INSTANCE, call.getOperands());
    }
    if (Types.isNumber(type)) {
      return new Call(NumberTryToBooleanFunction.INSTANCE, call.getOperands());
    }

    return new Call(StringTryToBooleanFunction.INSTANCE, call.getOperands());
  }

  public static final class StringTryToBooleanFunction extends TryToBooleanFunction {
    public static final StringTryToBooleanFunction INSTANCE = new StringTryToBooleanFunction();

    @Override
    public Object eval(final IExpression[] operands) {
      String value = operands[0].getValue(String.class);
      try {
        return BooleanConversion.convert(value);
      } catch (ConversionException e) {
        return null;
      }
    }
  }

  public static final class IntegerTryToBooleanFunction extends TryToBooleanFunction {
    public static final IntegerTryToBooleanFunction INSTANCE = new IntegerTryToBooleanFunction();

    @Override
    public Object eval(final IExpression[] operands) {
      Long value = operands[0].getValue(Long.class);
      return BooleanConversion.convert(value);
    }
  }

  public static final class NumberTryToBooleanFunction extends TryToBooleanFunction {
    public static final NumberTryToBooleanFunction INSTANCE = new NumberTryToBooleanFunction();

    @Override
    public Object eval(final IExpression[] operands) {
      BigDecimal value = operands[0].getValue(BigDecimal.class);
      return BooleanConversion.convert(value);
    }
  }
}
