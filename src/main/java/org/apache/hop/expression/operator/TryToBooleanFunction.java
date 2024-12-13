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
import org.apache.hop.expression.type.BooleanType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;

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
      return new Call(TryToBooleanInteger.INSTANCE, call.getOperands());
    }
    if (Types.isNumber(type)) {
      return new Call(TryToBooleanNumber.INSTANCE, call.getOperands());
    }

    return new Call(TryToBooleanString.INSTANCE, call.getOperands());
  }

  private static final class TryToBooleanString extends TryToBooleanFunction {
    private static final TryToBooleanString INSTANCE = new TryToBooleanString();

    private TryToBooleanString() {
      super();
    }

    @Override
    public Object eval(final IExpression[] operands) {
      String value = operands[0].getValue(String.class);
      try {
        return BooleanType.convert(value);
      } catch (ConversionException e) {
        return null;
      }
    }
  }

  private static final class TryToBooleanInteger extends TryToBooleanFunction {
    private static final TryToBooleanInteger INSTANCE = new TryToBooleanInteger();

    private TryToBooleanInteger() {
      super();
    }

    @Override
    public Object eval(final IExpression[] operands) {
      Long value = operands[0].getValue(Long.class);
      return BooleanType.convert(value);
    }
  }

  private static final class TryToBooleanNumber extends TryToBooleanFunction {
    private static final TryToBooleanNumber INSTANCE = new TryToBooleanNumber();

    private TryToBooleanNumber() {
      super();
    }

    @Override
    public Object eval(final IExpression[] operands) {
      BigDecimal value = operands[0].getValue(BigDecimal.class);
      return BooleanType.convert(value);
    }
  }
}
