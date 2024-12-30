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

import java.io.StringWriter;
import java.util.ArrayList;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;

/** String or binary concatenation operator '<code>||</code>' */
@FunctionPlugin
public class ConcatFunction extends Function {

  // Function
  public ConcatFunction() {
    super(
        "CONCAT",
        ReturnTypes.CONCAT_FUNCTION,
        OperandTypes.STRING_VARIADIC
            .or(OperandTypes.BINARY_VARIADIC)
            .or(OperandTypes.ARRAY_VARIADIC),
        OperatorCategory.STRING,
        "/docs/concat.html");
  }

  // Operator
  public ConcatFunction(String name) {
    super(
        "CONCAT",
        name,
        110,
        true,
        ReturnTypes.FIRST_KNOWN,
        OperandTypes.or(
            OperandTypes.STRING_VARIADIC,
            OperandTypes.BINARY_VARIADIC,
            OperandTypes.ARRAY_VARIADIC),
        OperatorCategory.STRING,
        "/docs/concat.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    Type type = null;
    // Combine chained CONCAT operator and remove NULL
    // This is especially useful for the || operator
    ArrayList<IExpression> operands = new ArrayList<>();
    for (IExpression operand : call.getChainedOperands(true)) {
      if (operand.isNull()) continue;
      if (type == null) type = operand.getType();
      operands.add(0, operand);
    }

    switch (operands.size()) {
      case 0: // Nothing to concat
        return new Literal(null, call.getType());
      case 1: // Concat(X) → X
        return operands.get(0);
      default:
        Operator operator = ConcatString.INSTANCE;
        if (type != null) {
          if (type.is(TypeName.ARRAY)) {
            operator = ConcatArray.INSTANCE;
          } else if (type.is(TypeName.BINARY)) {
            operator = ConcatBinary.INSTANCE;
          }
        }
        return new Call(operator, operands);
    }
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    boolean concatFirst = true;
    for (IExpression operand : operands) {
      if (concatFirst) concatFirst = false;
      else writer.append("||");
      operand.unparse(writer, 0, 0);
    }
  }

  /** String concatenation */
  private static final class ConcatString extends ConcatFunction {
    public static final ConcatFunction INSTANCE = new ConcatString();

    @Override
    public Object eval(final IExpression[] operands) {

      String firstNotNull = null;
      String[] values = new String[operands.length];
      int i = 0;
      for (IExpression operand : operands) {
        String value = operand.getValue(String.class);
        if (firstNotNull == null && value != null) firstNotNull = value;
        values[i++] = value;
      }

      if (firstNotNull == null) return null;

      StringBuilder builder = new StringBuilder();
      for (IExpression operand : operands) {
        String value = operand.getValue(String.class);
        if (value != null) builder.append(value);
      }

      return builder.toString();
    }
  }

  /** Binary concatenation */
  private static final class ConcatBinary extends ConcatFunction {
    public static final ConcatFunction INSTANCE = new ConcatBinary();

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

      if (length == 0) return null;

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

  /** Array to array concatenation concatenation */
  private static final class ConcatArray extends ConcatFunction {
    public static final ConcatFunction INSTANCE = new ConcatArray();

    @Override
    public Object eval(final IExpression[] operands) {
      Array array0 = operands[0].getValue(Array.class);
      Array array1 = operands[1].getValue(Array.class);

      int size0 = array0.size();
      int size1 = array1.size();

      IExpression[] values = new IExpression[size0 + size1];
      int i = 0;
      for (; i < size0; i++) {
        values[i] = array0.get(i);
      }
      for (int j = 0; j < size1; i++, j++) {
        values[i] = array1.get(j);
      }

      return new Array(array0.getType(), values);
    }
  }
}
