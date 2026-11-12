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

import java.util.Arrays;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Lambda;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.ParseExpressionException;
import org.apache.hop.expression.type.ArrayType;
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.IReturnTypeInference;
import org.apache.hop.expression.type.Type;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * A higher-order function whose first operand is an array and the second can be a {@link Lambda}
 * evaluated with elements of the array, like {@code TRANSFORM(array, x -> x * 2)} or {@code
 * ARRAY_SORT(array, (p1, p2) -> p2 - p1)}.
 *
 * <p>Each parameter of the lambda has the type of the elements of the array.
 */
@NullMarked
public abstract class ArrayLambdaFunction extends Function {

  /** The number of parameters of the lambda, the number of elements it is evaluated with. */
  private final int lambdaParameterCount;

  /**
   * Creates a function whose lambda has a single parameter, the element.
   *
   * @param id the name of the function
   * @param returnTypeInference the strategy to infer the return type
   * @param operandTypeChecker the strategy to check the operand types, which checks the lambda
   * @param documentationUrl the URL of the documentation
   */
  protected ArrayLambdaFunction(
      String id,
      IReturnTypeInference returnTypeInference,
      IOperandTypeChecker operandTypeChecker,
      String documentationUrl) {
    this(id, returnTypeInference, operandTypeChecker, documentationUrl, 1);
  }

  /**
   * Creates a function whose lambda has a number of parameters.
   *
   * @param id the name of the function
   * @param returnTypeInference the strategy to infer the return type
   * @param operandTypeChecker the strategy to check the operand types, which checks the lambda
   * @param documentationUrl the URL of the documentation
   * @param lambdaParameterCount the number of parameters of the lambda
   */
  protected ArrayLambdaFunction(
      String id,
      IReturnTypeInference returnTypeInference,
      IOperandTypeChecker operandTypeChecker,
      String documentationUrl,
      int lambdaParameterCount) {
    super(id, returnTypeInference, operandTypeChecker, OperatorCategory.ARRAY, documentationUrl);
    this.lambdaParameterCount = lambdaParameterCount;
  }

  /**
   * Validates the array, then the lambda with the type of the elements of the array as the type of
   * its parameters. An operand that is not a lambda is validated as usual, before the lambda: the
   * type of a parameter can depend on an operand that follows the lambda.
   *
   * <p>A lambda that follows an operand that is not an array, or a NULL literal, is not validated:
   * the call is rejected by {@link #checkOperandTypes(Call)}.
   *
   * @throws ExpressionException with {@link ErrorCode#ILLEGAL_LAMBDA_PARAMETER_COUNT} if the lambda
   *     has not the number of parameters expected
   */
  @Override
  protected void validateOperands(IExpressionContext context, Call call)
      throws ExpressionException {
    IExpression[] operands = call.getOperands();
    Lambda lambda = (operands.length > 1 && operands[1] instanceof Lambda l) ? l : null;
    for (IExpression operand : operands) {
      if (operand != lambda) {
        operand.validate(context);
      }
    }

    if (lambda != null) {
      if (lambda.getParameterCount() != lambdaParameterCount) {
        throw new ParseExpressionException(
            lambda.getPosition(),
            ErrorCode.ILLEGAL_LAMBDA_PARAMETER_COUNT,
            this,
            lambda,
            lambdaParameterCount);
      }
      if (operands[0].getType() instanceof ArrayType arrayType) {
        lambda.validate(context, lambdaParameterTypes(call, arrayType));
      }
    }
  }

  /**
   * {@inheritDoc}
   *
   * <p>A lambda is only accepted with an array: the elements give the type of its parameters. A
   * NULL literal has no type, it must be cast to an array type like {@code CAST(NULL AS
   * INTEGER[])}.
   */
  @Override
  protected boolean checkOperandTypes(Call call) {
    if (call.getOperandCount() > 1
        && call.getOperand(1) instanceof Lambda
        && !(call.getOperand(0).getType() instanceof ArrayType)) {
      return false;
    }
    return super.checkOperandTypes(call);
  }

  /**
   * Returns the types of the parameters of the lambda, by default the type of the elements of the
   * array for each parameter.
   *
   * @param call the call, whose operands other than the lambda are validated
   * @param arrayType the type of the array
   * @return the types of the parameters, in order
   */
  protected Type[] lambdaParameterTypes(Call call, ArrayType arrayType) {
    Type[] types = new Type[lambdaParameterCount];
    Arrays.fill(types, arrayType.getElementType());
    return types;
  }

  /**
   * Evaluates an element of the array, converted to the type of the lambda parameter.
   *
   * @param lambda the lambda
   * @param element the element of the array
   * @return the value to bind to the parameter
   */
  protected static @Nullable Object argument(Lambda lambda, IExpression element) {
    return element.getValue(lambda.getParameters().getFirst().getType().getName().getJavaClass());
  }
}
