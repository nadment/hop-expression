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

import java.util.ArrayList;
import java.util.List;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Lambda;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.type.ArrayType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.Type;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Returns an array of the results of a lambda applied to each element of an array.
 *
 * <p>{@code TRANSFORM([1,2,3], x -> x * 2)} returns {@code [2,4,6]}.
 *
 * @see FilterFunction
 */
@FunctionPlugin
@NullMarked
public class TransformFunction extends ArrayLambdaFunction {

  public static final Function INSTANCE = new TransformFunction();

  public TransformFunction() {
    super(
        "TRANSFORM",
        call -> ArrayType.of(call.getOperand(1).getType()),
        OperandTypes.ARRAY_LAMBDA_ANY,
        "/docs/hop-user-manual/expression/transform.html");
  }

  @Override
  public @Nullable Object eval(IExpression[] operands) {
    Array array = operands[0].getValue(Array.class);
    if (array == null) return null;

    Lambda lambda = (Lambda) operands[1];
    Type type = lambda.getType();
    Class<?> clazz = type.getName().getJavaClass();

    List<IExpression> elements = new ArrayList<>(array.size());
    for (IExpression element : array) {
      Object value = lambda.apply(clazz, argument(lambda, element));
      // A nested array is an expression, not a literal value
      elements.add(value instanceof Array result ? result : new Literal(value, type));
    }

    return new Array(ArrayType.of(type), elements);
  }
}
