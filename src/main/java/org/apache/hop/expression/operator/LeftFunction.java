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

/**
 * The function extracts a number of characters from a string or bytes from binary starting from
 * left.
 *
 * @see RightFunction
 */
@FunctionPlugin
public class LeftFunction extends Function {

  public LeftFunction() {
    super(
        "LEFT",
        ReturnTypes.ARG0_MAX_PRECISION,
        OperandTypes.STRING_INTEGER.or(OperandTypes.BINARY_INTEGER),
        OperatorCategory.STRING,
        "/docs/left.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Type type = call.getType();
    if (Types.isBinary(type)) {
      return new Call(BinaryLeftFunction.INSTANCE, call.getOperands());
    }

    return new Call(StringLeftFunction.INSTANCE, call.getOperands());
  }

  /** The function extracts a number of characters from a string starting from left. */
  private static final class StringLeftFunction extends LeftFunction {
    public static final StringLeftFunction INSTANCE = new StringLeftFunction();

    @Override
    public Object eval(final IExpression[] operands) {
      String str = operands[0].getValue(String.class);
      if (str == null) return null;

      Long v1 = operands[1].getValue(Long.class);
      if (v1 == null) return null;
      int length = v1.intValue();
      if (length < 0) {
        length = 0;
      }

      if (str.length() <= length) {
        return str;
      }
      return str.substring(0, length);
    }
  }

  /** The function extracts a number of characters from a binary starting from left. */
  private static final class BinaryLeftFunction extends LeftFunction {
    public static final BinaryLeftFunction INSTANCE = new BinaryLeftFunction();

    @Override
    public Object eval(final IExpression[] operands) {
      byte[] bytes = operands[0].getValue(byte[].class);
      if (bytes == null) return null;

      Long v1 = operands[1].getValue(Long.class);
      if (v1 == null) return null;
      int length = v1.intValue();
      if (length < 0) {
        length = 0;
      }

      if (bytes.length <= length) {
        return bytes;
      }

      byte[] result = new byte[length];
      System.arraycopy(bytes, 0, result, 0, length);
      return result;
    }
  }
}
