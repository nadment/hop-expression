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
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Returns an array of the elements of an array for which a lambda condition is TRUE.
 *
 * <p>{@code FILTER([1,-2,3], x -> x > 0)} returns {@code [1,3]}.
 *
 * @see TransformFunction
 */
@FunctionPlugin
@NullMarked
public class FilterFunction extends ArrayLambdaFunction {

  public static final Function INSTANCE = new FilterFunction();

  public FilterFunction() {
    super(
        "FILTER",
        ReturnTypes.ARG0,
        OperandTypes.ARRAY_LAMBDA_BOOLEAN,
        "/docs/hop-user-manual/expression/filter.html");
  }

  @Override
  public @Nullable Object eval(IExpression[] operands) {
    Array array = operands[0].getValue(Array.class);
    if (array == null) return null;

    Lambda lambda = (Lambda) operands[1];

    List<IExpression> elements = new ArrayList<>();
    for (IExpression element : array) {
      // An element is kept only if the condition is TRUE, not FALSE or NULL
      if (Boolean.TRUE.equals(lambda.apply(Boolean.class, argument(lambda, element)))) {
        elements.add(element);
      }
    }

    return new Array(array.getType(), elements);
  }
}
