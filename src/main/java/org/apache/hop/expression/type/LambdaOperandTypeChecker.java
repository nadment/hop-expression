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
package org.apache.hop.expression.type;

import java.util.function.Predicate;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Lambda;
import org.jspecify.annotations.NullMarked;

/**
 * Operand type-checking strategy which checks the operand is a {@link Lambda} with a number of
 * parameters, whose expression returns an accepted type, like a type coercible to BOOLEAN or a type
 * of the NUMERIC family.
 *
 * <p>The type of the expression is known only if the function gave the type of the parameters when
 * it validated the lambda, see {@link
 * org.apache.hop.expression.Operator#validateOperands(org.apache.hop.expression.IExpressionContext,
 * org.apache.hop.expression.Call)}.
 */
@NullMarked
public class LambdaOperandTypeChecker implements ISingleOperandTypeChecker {
  private final int parameterCount;
  private final Predicate<TypeName> returnType;

  /**
   * Creates a checker for a lambda.
   *
   * @param parameterCount the number of parameters expected
   * @param returnType the test of the type returned by the expression of the lambda
   * @see OperandTypes#lambda(int, TypeName)
   * @see OperandTypes#lambda(int, TypeFamily)
   */
  public LambdaOperandTypeChecker(int parameterCount, Predicate<TypeName> returnType) {
    this.parameterCount = parameterCount;
    this.returnType = returnType;
  }

  /**
   * {@inheritDoc}
   *
   * <p>Like the other single-operand checkers, returns {@code true} if the operand is not valid.
   */
  @Override
  public boolean checkSingleOperandType(IExpression operand) {
    if (operand instanceof Lambda lambda) {
      return lambda.getParameterCount() != parameterCount
          || !returnType.test(lambda.getType().getName());
    }
    return true;
  }
}
