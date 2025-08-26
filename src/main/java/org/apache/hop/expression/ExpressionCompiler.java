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
import org.apache.hop.expression.operator.CastOperator;
import org.apache.hop.expression.type.ArrayType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;

public class ExpressionCompiler implements IExpressionVisitor<IExpression> {

  private static final int MAX_LOOP_ATTEMPTS = 1000;
  private final IExpressionContext context;
  private boolean hasChanged;

  public ExpressionCompiler(IExpressionContext context) {
    this.context = context;
  }

  public IExpression compile(IExpression expression) throws ExpressionException {
    int attemptCount = 0;
    do {
      hasChanged = false;
      expression = expression.accept(this);
      // Vérification pour éviter une boucle infinie
      if (attemptCount++ > MAX_LOOP_ATTEMPTS) {
        throw new ExpressionException(
            ErrorCode.INTERNAL_ERROR,
            "compilation: Too many attempts for expression %1$s".formatted(expression));
      }
    } while (hasChanged);
    return expression;
  }

  @Override
  public IExpression visitIdentifier(final Identifier identifier) {
    return identifier;
  }

  @Override
  public IExpression visitCall(final Call original) {

    // Compile the operands of a call
    for (int i = 0; i < original.getOperandCount(); i++) {
      original.setOperand(i, original.getOperand(i).accept(this));
    }

    // Compile with operator
    IExpression expression = original.getOperator().compile(context, original);

    if (expression instanceof Call call) {

      // Inferring the return type
      call.inferReturnType();
      hasChanged |= call.getOperator().coerceOperandsType(call);

      // Coerce operands data type
      hasChanged |= call.getOperator().coerceOperandsType(call);

      // If something changed
      if (!call.equals(original)) {
        hasChanged = true;
      }

      // Evaluate if the call is constant
      if (expression.isConstant()) {
        try {
          Type type = expression.getType();
          Object value = expression.getValue();

          if (value instanceof Array array) {
            return array;
          }

          // For CAST operator, it's important to return type
          else if (expression.isOperator(CastOperator.INSTANCE)) {
            value = type.cast(value);
          }

          hasChanged = true;

          // If the value is null, change the nullability of the type.
          if (value == null) {
            type = type.withNullability(true);
          }

          return new Literal(value, type);
        } catch (Exception e) {
          // Ignore error like division by zero "X IN (1,3/0)" and continue
          return call;
        }
      }
    }

    return expression;
  }

  @Override
  public IExpression visitArray(final Array array) {
    int size = array.size();
    List<IExpression> expressions = new ArrayList<>(size);
    List<Type> types = new ArrayList<>(size);

    for (IExpression expression : array) {
      expression = expression.accept(this);
      expressions.add(expression);
      types.add(expression.getType());
    }

    Type elementType = Types.getLeastRestrictive(types);
    return new Array(ArrayType.of(elementType), expressions);
  }

  @Override
  public IExpression visitLiteral(final Literal literal) {
    return literal;
  }
}
