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

import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeId;
import java.util.ArrayList;
import java.util.List;

public class ExpressionCompiler implements IExpressionVisitor<IExpression> {

  private final IExpressionContext context;
  
  public ExpressionCompiler(IExpressionContext context) {
    this.context = context;
  }

  public IExpression compile(IExpression expression)
      throws ExpressionException {
    IExpression original;
    do {
      original = expression;
      expression = expression.accept(this);
    } while (!expression.equals(original));

    return expression;
  }

  @Override
  public IExpression visitIdentifier(final Identifier identifier) {
    return identifier;
  }

  @Override
  public IExpression visitCall(Call call) {

    // Compile the operands of a call
    List<IExpression> expressions = new ArrayList<>(call.getOperandCount());
    for (IExpression expression : call.getOperands()) {
      expression = expression.accept(this);
      expressions.add(expression);
    }
    Operator operator = call.getOperator();
    call = new Call(operator, expressions);
    call.inferReturnType();

    // Compile with operator
    IExpression expression = operator.compile(context, call);
    
    if (expression.is(Kind.CALL)) {
      call = expression.asCall();
      // Inferring return type
      expression = call.getOperator().castType(call);
    }

    // Evaluate if constant
    if (expression.isConstant()) {
      try {
        Object constantValue = expression.getValue();
        Type constantType = expression.getType();

        // Some operator don't known return type like JSON_VALUE.
        if (TypeId.ANY.equals(constantType.getId())) {
          return Literal.of(constantValue);
        }

        // For CAST operator, it's important to return type
        if (expression.is(Operators.CAST)) {
          constantValue = constantType.cast(constantValue);

        }
        return new Literal(constantValue, constantType);
      } catch (Exception e) {
        // Ignore error like division by zero "X IN (1,3/0)" and continue
      }
    }
    return expression;
  }

  @Override
  public IExpression visitTuple(final Tuple tuple) {
    List<IExpression> expressions = new ArrayList<>(tuple.size());
    for (IExpression expression : tuple) {
      expression = expression.accept(this);
      expressions.add(expression);
    }
    return new Tuple(expressions);
  }

  @Override
  public IExpression visitLiteral(final Literal literal) {
    return literal;
  }
}
