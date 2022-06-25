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

public class UdfResolver implements IExpressionVisitor<IExpression> {

  private final IExpression operands[];

  public UdfResolver(IExpression operands[]) {
    this.operands = operands;
  }

  public IExpression apply(final IExpressionContext context, final Identifier identifier) {
    return operands[identifier.getIndex()];
  }

  @Override
  public IExpression apply(final IExpressionContext context, final Call call) {
    List<IExpression> expressions = new ArrayList<>(call.getOperandCount());
    for (IExpression expression : call.getOperands()) {
      // Some operands can be null
      if ( expression!=null ) {
        expression = expression.visit(context, this);
      }
      expressions.add(expression);
    }
    return new Call(call.getOperator(), expressions);
  }

  @Override
  public IExpression apply(IExpressionContext context, Tuple tuple) {
    List<IExpression> expressions = new ArrayList<>(tuple.size());
    for (IExpression expression : tuple) {
      // Some operands can be null
      if ( expression!=null ) {
        expression = expression.visit(context, this);
      }
      expressions.add(expression);
    }
    return new Tuple(expressions);
  }

  @Override
  public IExpression apply(IExpressionContext context, Literal literal) {
    return literal;
  }
}
