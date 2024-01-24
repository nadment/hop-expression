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
import java.io.ByteArrayOutputStream;
import java.io.IOException;

/**
 * The function repeats a string or binary as many times as specified.
 */
@FunctionPlugin
public class RepeatFunction extends Function {

  public static final RepeatFunction RepeatStringFunction = new RepeatStringFunction();
  public static final RepeatFunction RepeatBinaryFunction = new RepeatBinaryFunction();

  public RepeatFunction() {
    super("REPEAT", ReturnTypes.ARG0_MAX_PRECISION, OperandTypes.STRING_NUMERIC.or(OperandTypes.BINARY_NUMERIC),
        OperatorCategory.STRING, "/docs/repeat.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Type type = call.getOperand(0).getType();
    if (type.isFamily(TypeFamily.BINARY)) {
      return new Call(RepeatBinaryFunction, call.getOperands());
    }

    return new Call(RepeatStringFunction, call.getOperands());
  }

  /**
   * The function repeats a string as many times as specified.
   */
  private static final class RepeatStringFunction extends RepeatFunction {
    @Override
    public Object eval(final IExpression[] operands) {
      String value = operands[0].getValue(String.class);
      if (value == null)
        return null;
      Long repeat = operands[1].getValue(Long.class);
      if (repeat == null)
        return null;
      int count = repeat.intValue();

      StringBuilder builder = new StringBuilder(value.length() * count);
      while (count-- > 0) {
        builder.append(value);
      }
      return builder.toString();
    }
  }

  /**
   * The function repeats a binary as many times as specified.
   */
  private static final class RepeatBinaryFunction extends RepeatFunction {
    @Override
    public Object eval(final IExpression[] operands) {
      byte[] value = operands[0].getValue(byte[].class);
      if (value == null)
        return null;
      Long repeat = operands[1].getValue(Long.class);
      if (repeat == null)
        return null;
      int count = repeat.intValue();

      try {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream(value.length * count);
        for (int i = 0; i < count; i++) {
          buffer.write(value);
        }
        return buffer.toByteArray();
      } catch (IOException e) {
        return null;
      }
    }
  }
}
