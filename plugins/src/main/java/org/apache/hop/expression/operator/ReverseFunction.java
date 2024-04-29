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
import org.apache.hop.expression.type.TypeFamily;

/**
 * The function reverses the order of characters in a string value, or of bytes in a binary value.
 */
@FunctionPlugin
public class ReverseFunction extends Function {
  public static final ReverseFunction StringReverseFunction = new StringReverseFunction();
  public static final ReverseFunction BinaryReverseFunction = new BinaryReverseFunction();

  public ReverseFunction() {
    super(
        "REVERSE",
        ReturnTypes.ARG0,
        OperandTypes.STRING.or(OperandTypes.BINARY),
        OperatorCategory.STRING,
        "/docs/reverse.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Type type = call.getOperand(0).getType();

    if (type.isFamily(TypeFamily.BINARY)) {
      return new Call(BinaryReverseFunction, call.getOperand(0));
    }

    return new Call(StringReverseFunction, call.getOperand(0));
  }

  /** The function reverses the order of characters in a string value. */
  private static final class StringReverseFunction extends ReverseFunction {
    @Override
    public Object eval(final IExpression[] operands) {
      String value = operands[0].getValue(String.class);
      if (value == null) return null;

      StringBuilder builder = new StringBuilder(value);
      return builder.reverse().toString();
    }
  }

  /** The function reverses the order of bytes in a binary value. */
  private static final class BinaryReverseFunction extends ReverseFunction {
    @Override
    public Object eval(final IExpression[] operands) {
      final byte[] value = operands[0].getValue(byte[].class);
      if (value == null) return null;

      final byte[] result = new byte[value.length];
      for (int i = value.length - 1, j = 0; i >= 0; i--, j++) {
        result[j] = value[i];
      }
      return result;
    }
  }
}
