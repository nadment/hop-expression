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
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.ArrayType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.type.Types;

/** String or binary concatenation operator '<code>||</code>' */
@FunctionPlugin
public class ConcatFunction extends Function {

  public static final ConcatFunction INSTANCE = new ConcatFunction();

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
        Associativity.LEFT,
        ReturnTypes.FIRST_KNOWN,
        OperandTypes.or(
            OperandTypes.STRING_VARIADIC,
            OperandTypes.BINARY_VARIADIC,
            OperandTypes.ARRAY_VARIADIC),
        OperatorCategory.STRING,
        "/docs/concat.html");
  }

  @Override
  public boolean coerceOperandsType(Call call) {

    Type type = call.getType();
    if (type instanceof ArrayType arrayType) {
      type = arrayType.getElementType();
    }

    return Types.coerceOperandsType(call, type);
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    call.getOperand(0).is(Kind.ARRAY);

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
        Operator operator = StringConcatFunction.INSTANCE;
        if (type != null) {
          if (type.is(TypeName.ARRAY)) {
            operator = ArrayConcatFunction.INSTANCE;
          } else if (type.is(TypeName.BINARY)) {
            operator = BinaryConcatFunction.INSTANCE;
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
  public static final class StringConcatFunction extends ConcatFunction {
    public static final ConcatFunction INSTANCE = new StringConcatFunction();

    @Override
    public Object eval(final IExpression[] operands) {

      boolean nulls = true;
      StringBuilder builder = new StringBuilder();
      for (IExpression operand : operands) {
        String value = operand.getValue(String.class);
        if (value != null) {
          builder.append(value);
          nulls = false;
        }
      }
      if (nulls) return null;
      return builder.toString();
    }
  }

  /** Binary concatenation */
  public static final class BinaryConcatFunction extends ConcatFunction {
    public static final ConcatFunction INSTANCE = new BinaryConcatFunction();

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
}
