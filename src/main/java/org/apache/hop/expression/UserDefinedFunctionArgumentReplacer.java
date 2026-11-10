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
package org.apache.hop.expression;

import java.util.ArrayList;
import java.util.List;
import org.jspecify.annotations.NullMarked;

/**
 * Visitor that replaces, in the source expression of a user-defined function, each argument by the
 * operand of the call at the same position.
 *
 * <p>The source expression must have been validated against the arguments of the function, so that
 * each identifier knows its position.
 */
@NullMarked
final class UserDefinedFunctionArgumentReplacer implements IExpressionVisitor<IExpression> {

  private final IExpression[] operands;

  /**
   * Creates a replacer for a call.
   *
   * @param operands the operands of the call, in the order of the arguments of the function
   */
  UserDefinedFunctionArgumentReplacer(IExpression[] operands) {
    this.operands = operands;
  }

  @Override
  public IExpression visitIdentifier(final Identifier identifier) {
    // The parameter of a lambda is not an argument of the function
    if (identifier instanceof LambdaParameter) {
      return identifier;
    }
    return operands[identifier.getOrdinal()];
  }

  @Override
  public IExpression visitCall(final Call call) {
    List<IExpression> expressions = new ArrayList<>(call.getOperandCount());
    for (IExpression expression : call.getOperands()) {
      expression = expression.accept(this);
      expressions.add(expression);
    }

    Call resolved = new Call(call.getOperator(), expressions);
    resolved.inferReturnType();

    return resolved;
  }

  @Override
  public IExpression visitArray(final Array array) {
    List<IExpression> expressions = new ArrayList<>(array.size());
    for (IExpression expression : array) {
      expression = expression.accept(this);
      expressions.add(expression);
    }
    return new Array(expressions);
  }

  @Override
  public IExpression visitLiteral(final Literal literal) {
    return literal;
  }

  @Override
  public IExpression visitLambda(final Lambda lambda) {
    return new Lambda(
        lambda.getPosition(), lambda.getParameters(), lambda.getExpression().accept(this));
  }
}
