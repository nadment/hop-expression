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

import java.util.ArrayList;
import java.util.List;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.ArrayType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Returns an array concatenating two or more arrays.
 *
 * @see ArrayAppendFunction
 * @see ArrayPrependFunction
 */
@FunctionPlugin
@NullMarked
public class ArrayConcatFunction extends Function {

  public static final Function INSTANCE = new ArrayConcatFunction();

  public ArrayConcatFunction() {
    super(
        "ARRAY_CONCAT",
        ReturnTypes.ARRAY_CONCAT,
        OperandTypes.ARRAY_VARIADIC,
        OperatorCategory.ARRAY,
        "/docs/hop-user-manual/expression/array_concat.html");
  }

  @Override
  protected boolean checkOperandTypes(Call call) {
    if (!super.checkOperandTypes(call)) {
      return false;
    }

    // The elements of every array must be coercible to the most general type of the elements
    Type elementType = null;
    for (IExpression operand : call.getOperands()) {
      // A NULL literal is not an array
      Type type = operand.getType().getElementType();
      if (type != null) {
        elementType = Types.getLeastRestrictive(elementType, type);
      }
    }

    if (elementType == null) {
      return true;
    }

    for (IExpression operand : call.getOperands()) {
      Type type = operand.getType().getElementType();
      if (type != null && !type.isCoercible(elementType)) {
        return false;
      }
    }
    return true;
  }

  @Override
  public boolean coerceOperandsType(Call call) {

    // The elements of a multi-dimensional array are coerced, not its nested arrays
    Type type = call.getType();
    while (type instanceof ArrayType arrayType) {
      type = arrayType.getElementType();
    }

    return Types.coerceOperandsType(call, type);
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Combine chained ARRAY_CONCAT and remove NULL
    List<IExpression> operands = new ArrayList<>();
    boolean constructors = true;
    for (IExpression operand : call.getChainedOperands(true)) {
      if (operand.isNull()) continue;
      constructors &= operand instanceof Array;
      operands.addFirst(operand);
    }

    switch (operands.size()) {
      case 0: // Nothing to concat
        return new Literal(null, call.getType());
      case 1: // ARRAY_CONCAT(X) → X
        return operands.getFirst();
      default:
        IExpression[] arrays = operands.toArray(new IExpression[0]);
        // The array constructors are joined, like ARRAY_CONCAT([1,A],[B]) → [1,A,B]
        if (constructors && eval(arrays) instanceof Array array) {
          return array;
        }
        if (arrays.length == call.getOperandCount()) {
          return call;
        }
        return new Call(call.getPosition(), this, arrays);
    }
  }

  @Override
  public @Nullable Object eval(IExpression[] operands) {
    List<Array> arrays = new ArrayList<>();
    Type elementType = null;
    for (IExpression operand : operands) {
      // NULL values are ignored
      Array array = operand.getValue(Array.class);
      if (array == null) continue;

      arrays.add(array);
      elementType = Types.getLeastRestrictive(elementType, array.getType().getElementType());
    }

    if (arrays.isEmpty()) return null;
    if (arrays.size() == 1 || elementType == null) return arrays.getFirst();

    // Every element is converted to the most general type of the elements of all the arrays
    Array result = new Array(ArrayType.of(elementType, false));
    for (Array array : arrays) {
      result = result.concat(array);
    }
    return result;
  }
}
