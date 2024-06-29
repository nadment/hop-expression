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

public class UserDefinedFunctionResolver implements IExpressionVisitor<IExpression> {

  private final IExpression[] operands;

  public UserDefinedFunctionResolver(IExpression[] operands) {
    this.operands = operands;
  }

  @Override
  public IExpression visitIdentifier(final Identifier identifier) {
    return operands[identifier.getIndex()];
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
}
