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
import org.apache.hop.expression.Category;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeFamily;
import java.io.StringWriter;
import java.util.ArrayList;


/**
 * String or binary concatenation operator '<code>||</code>'
 */
@FunctionPlugin
public class ConcatFunction extends Function {
  public static final ConcatFunction ConcatStringFunction = new ConcatString();
  public static final ConcatFunction ConcatBinaryFunction = new ConcatBinary();

  // Function
  public ConcatFunction() {
    super("CONCAT", ReturnTypes.FIRST_KNOWN,
        OperandTypes.or(OperandTypes.STRING_VARIADIC, OperandTypes.BINARY_VARIADIC),
        Category.STRING, "/docs/concat.html");
  }

  // Operator
  public ConcatFunction(String name) {
    super("CONCAT", name, 110, true, ReturnTypes.FIRST_KNOWN,
        OperandTypes.or(OperandTypes.STRING_VARIADIC, OperandTypes.BINARY_VARIADIC),
        Category.STRING, "/docs/concat.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    Type type = null;
    // Combine chained CONCAT operator and remove NULL
    ArrayList<IExpression> operands = new ArrayList<>();
    for (IExpression operand : getChainedOperands(call, true)) {
      if (operand.isNull())
        continue;
      if (type == null)
        type = operand.getType();
      operands.add(0, operand);
    }

    switch (operands.size()) {
      case 0: // Nothing to concat
        return new Literal(null, call.getType());
      case 1: // Concat(X) => X
        return operands.get(0);
      default:
        Operator operator = ConcatStringFunction;
        if (type != null && type.isFamily(TypeFamily.BINARY)) {
          operator = ConcatBinaryFunction;
        }
        return new Call(operator, operands);
    }
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    boolean concatFirst = true;
    for (IExpression operand : operands) {
      if (concatFirst)
        concatFirst = false;
      else
        writer.append("||");
      operand.unparse(writer);
    }
  }

  /**
   * String concatenation
   */
  private static final class ConcatString extends ConcatFunction {
    @Override
    public Object eval(final IExpression[] operands) {

      String firstNotNull = null;
      String[] values = new String[operands.length];
      int i = 0;
      for (IExpression operand : operands) {
        String value = operand.getValue(String.class);
        if (firstNotNull == null && value != null)
          firstNotNull = value;
        values[i++] = value;
      }

      if (firstNotNull == null)
        return null;

      StringBuilder builder = new StringBuilder();
      for (IExpression operand : operands) {
        String value = operand.getValue(String.class);
        if (value != null)
          builder.append(value);
      }

      return builder.toString();
    }
  }

  /**
   * Binary concatenation
   */
  private static final class ConcatBinary extends ConcatFunction {
    @Override
    public Object eval(final IExpression[] operands) {
      byte[][] values = new byte[operands.length][];
      int i = 0;
      int length = 0;
      for (IExpression operand : operands) {
        byte[] value = operand.getValue(byte[].class);
        values[i++] = value;
        if (value != null) {
          length += value.length;
        }
      }

      if (length == 0)
        return null;

      final byte[] result = new byte[length];
      int index = 0;
      for (byte[] value : values) {
        if (value != null) {
          System.arraycopy(value, 0, result, index, value.length);
          index += value.length;
        }
      }
      return result;
    }
  }
}
