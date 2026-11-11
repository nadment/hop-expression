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
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** String or binary concatenation operator '<code>||</code>' */
@FunctionPlugin
@NullMarked
public class ConcatFunction extends Function {

  public static final ConcatFunction INSTANCE = new ConcatFunction();

  // Function, always unparsed with the operator "||" so with the same precedence
  public ConcatFunction() {
    super(
        "CONCAT",
        "CONCAT",
        70,
        Associativity.LEFT,
        ReturnTypes.CONCAT_FUNCTION,
        OperandTypes.STRING_VARIADIC.or(OperandTypes.BINARY_VARIADIC),
        OperatorCategory.STRING,
        "/docs/hop-user-manual/expression/concat.html");
  }

  // Operator
  public ConcatFunction(String name) {
    super(
        "CONCAT",
        name,
        70,
        Associativity.LEFT,
        ReturnTypes.CONCAT_FUNCTION,
        OperandTypes.or(OperandTypes.STRING_VARIADIC, OperandTypes.BINARY_VARIADIC),
        OperatorCategory.STRING,
        "/docs/hop-user-manual/expression/concat.html");
  }

  @Override
  protected boolean checkOperandTypes(Call call) {
    // An array is not concatenated, see ARRAY_CONCAT, ARRAY_APPEND and ARRAY_PREPEND
    for (IExpression operand : call.getOperands()) {
      if (operand.getType().isName(TypeName.ARRAY)) {
        return false;
      }
    }
    return super.checkOperandTypes(call) && checkBinaryOperands(call);
  }

  /**
   * Checks the operands of a concatenation with a binary operand: the result is a binary, so every
   * operand must be a binary or a string.
   *
   * @param call the call to check
   * @return false if a binary operand is mixed with an operand that is not a binary or a string
   */
  static boolean checkBinaryOperands(Call call) {
    boolean binary = false;
    boolean other = false;
    for (IExpression operand : call.getOperands()) {
      Type type = operand.getType();
      if (type.isName(TypeName.BINARY)) {
        binary = true;
      } else if (!type.isName(TypeName.STRING) && !operand.isNull()) {
        other = true;
      }
    }
    return !(binary && other);
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coerceOperandsType(call, call.getType());
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
      operands.addFirst(operand);
    }

    switch (operands.size()) {
      case 0: // Nothing to concat
        return new Literal(null, call.getType());
      case 1: // Concat(X) → X
        // Cast X to the type of the concatenation, like NULL||1 → CAST(1 AS STRING)
        if (operands.getFirst().getType().getName() != call.getType().getName()) {
          return Types.cast(operands.getFirst(), call.getType());
        }
        return operands.getFirst();
      default:
        Operator operator = StringConcatFunction.INSTANCE;
        if (call.getType().isName(TypeName.BINARY)) {
          operator = BinaryConcatFunction.INSTANCE;
        }
        return new Call(operator, operands);
    }
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    int last = operands.length - 1;
    for (int i = 0; i <= last; i++) {
      if (i > 0) writer.append("||");
      operands[i].unparse(
          writer, (i == 0) ? 0 : getRightPrecedence(), (i == last) ? 0 : getLeftPrecedence());
    }
  }

  /** String concatenation */
  public static final class StringConcatFunction extends ConcatFunction {
    public static final ConcatFunction INSTANCE = new StringConcatFunction();

    @Override
    public @Nullable Object eval(final IExpression[] operands) {

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
    public @Nullable Object eval(final IExpression[] operands) {
      byte[][] values = new byte[operands.length][];
      int i = 0;
      int length = 0;
      boolean nulls = true;
      for (IExpression operand : operands) {
        byte[] value = operand.getValue(byte[].class);
        values[i++] = value;
        if (value != null) {
          length += value.length;
          nulls = false;
        }
      }

      // Returns NULL only if all values are NULL, empty binaries give an empty binary
      if (nulls) return null;

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
