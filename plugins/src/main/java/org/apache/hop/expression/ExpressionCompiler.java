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

import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeId;
import java.util.ArrayList;
import java.util.List;

public class ExpressionCompiler implements IExpressionVisitor<IExpression> {

  private final IExpressionContext context;
  private boolean changed;

  public ExpressionCompiler(IExpressionContext context) {
    this.context = context;
  }

  public IExpression compile(IExpression expression) throws ExpressionException {
    do {
      changed = false;
      //System.out.println(expression);
      expression = expression.accept(this);      
    } while (changed);

    return expression;
  }

  @Override
  public IExpression visitIdentifier(final Identifier identifier) {
    return identifier;
  }

  @Override
  public IExpression visitCall(Call call) {

    IExpression original = call;
    
    // Compile the operands of a call
    for (int i=0; i<call.getOperandCount(); i++) {
      call.setOperand(i, call.getOperand(i).accept(this));
    }    
    
    // Compile with operator
    IExpression expression = call.getOperator().compile(context, call);
    
    if (expression.is(Kind.CALL)) {
      call = expression.asCall();

      // Coerce operands data type
      changed |= call.getOperator().coerceOperandsType(call);

      // Inferring return type
      call.inferReturnType();

      // If something changed
      if (!expression.equals(original)) {
          changed = true;
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
          
          changed = true;
          
          return new Literal(constantValue, constantType);
        } catch (Exception e) {
          // Ignore error like division by zero "X IN (1,3/0)" and continue
        }
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
