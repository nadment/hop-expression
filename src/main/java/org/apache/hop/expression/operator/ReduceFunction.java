/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations
 * under the License.
 */
package org.apache.hop.expression.operator;

import java.util.Iterator;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Lambda;
import org.apache.hop.expression.type.ArrayType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.type.TypeTransforms;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Reduces the elements of an array to a single value with a lambda {@code (accumulator, element) ->
 * expression}, evaluated for each element with the result of the previous element.
 *
 * <p>{@code REDUCE([1,2,3], (acc, x) -> acc + x)} returns {@code 6}.
 *
 * <p>The accumulator starts with the optional initial value, which gives its type, otherwise with
 * the first element of the array.
 *
 * @see TransformFunction
 */
@FunctionPlugin
@NullMarked
public class ReduceFunction extends ArrayLambdaFunction {

  public static final Function INSTANCE = new ReduceFunction();

  public ReduceFunction() {
    super(
        "REDUCE",
        ReduceFunction::accumulatorType,
        OperandTypes.sequence(OperandTypes.ARRAY, OperandTypes.lambda(2, TypeName.ANY))
            .or(
                OperandTypes.sequence(
                    OperandTypes.ARRAY, OperandTypes.lambda(2, TypeName.ANY), OperandTypes.ANY)),
        "/docs/hop-user-manual/expression/reduce.html",
        2);
  }

  /**
   * The accumulator has the type of the initial value, or of the elements of the array, with the
   * maximum precision: the precision of the result can't be inferred from the first value.
   */
  @Override
  protected Type[] lambdaParameterTypes(Call call, ArrayType arrayType) {
    Type elementType = arrayType.getElementType();
    Type accumulatorType =
        (call.getOperandCount() == 3) ? call.getOperand(2).getType() : elementType;
    // The accumulator can be NULL, the result of the expression for the previous element
    return new Type[] {
      TypeTransforms.TO_MAX_PRECISION.transformType(call, accumulatorType).withNullability(true),
      elementType
    };
  }

  /**
   * The result of the lambda is the next value of the accumulator, so it must be coercible to the
   * type of the accumulator.
   */
  @Override
  protected boolean checkOperandTypes(Call call) {
    return super.checkOperandTypes(call)
        && call.getOperand(1).getType().isCoercible(accumulatorType(call));
  }

  /** The type of the accumulator is the type of the result. */
  private static Type accumulatorType(Call call) {
    return ((Lambda) call.getOperand(1)).getParameters().getFirst().getType();
  }

  @Override
  public @Nullable Object eval(IExpression[] operands) {
    Array array = operands[0].getValue(Array.class);
    if (array == null) return null;

    Lambda lambda = (Lambda) operands[1];
    Class<?> accumulatorClass = javaClass(lambda, 0);
    Class<?> elementClass = javaClass(lambda, 1);

    Iterator<IExpression> elements = array.iterator();
    Object accumulator;
    if (operands.length == 3) {
      accumulator = operands[2].getValue(accumulatorClass);
    } else if (elements.hasNext()) {
      accumulator = elements.next().getValue(accumulatorClass);
    } else {
      // Nothing to reduce
      return null;
    }

    while (elements.hasNext()) {
      Object element = elements.next().getValue(elementClass);
      accumulator = lambda.apply(accumulatorClass, accumulator, element);
    }

    return accumulator;
  }

  private static Class<?> javaClass(Lambda lambda, int index) {
    return lambda.getParameters().get(index).getType().getName().getJavaClass();
  }
}
