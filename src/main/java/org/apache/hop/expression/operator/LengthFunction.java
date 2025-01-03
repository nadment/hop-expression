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

/** The function returns the number of characters of the specified string or binary. */
@FunctionPlugin(names = "LEN")
public class LengthFunction extends Function {

  public LengthFunction() {
    super(
        "LENGTH",
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.STRING.or(OperandTypes.BINARY),
        OperatorCategory.STRING,
        "/docs/length.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Type type = call.getOperand(0).getType();

    // Binary first
    if (Types.isBinary(type)) {
      return new Call(BinaryLengthFunction.INSTANCE, call.getOperands());
    }
    return new Call(StringLengthFunction.INSTANCE, call.getOperands());
  }

  /** The function returns the number of characters of the specified string. */
  private static final class StringLengthFunction extends LengthFunction {
    public static final StringLengthFunction INSTANCE = new StringLengthFunction();

    @Override
    public Object eval(final IExpression[] operands) {
      String value = operands[0].getValue(String.class);
      if (value == null) return value;
      return Long.valueOf(value.length());
    }
  }

  /** The function returns the number of characters of the specified binary. */
  private static final class BinaryLengthFunction extends LengthFunction {
    public static final BinaryLengthFunction INSTANCE = new BinaryLengthFunction();

    @Override
    public Object eval(final IExpression[] operands) {
      byte[] value = operands[0].getValue(byte[].class);
      if (value == null) return value;
      return Long.valueOf(value.length);
    }
  }
}
