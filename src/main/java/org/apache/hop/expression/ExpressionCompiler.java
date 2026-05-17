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
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.expression.operator.CastOperator;
import org.apache.hop.expression.type.ArrayType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;

@NullMarked
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
      // Avoid infinite loop
      if (attemptCount++ > MAX_LOOP_ATTEMPTS) {
        throw new ExpressionException(
            ErrorCode.INTERNAL_ERROR,
            "compilation: Too many attempts for expression %1$s".formatted(expression));
      }
    } while (hasChanged);
    return expression;
  }

  @Override
  public IExpression visitIdentifier(Identifier identifier) {
    // Check if already compiled
    if (identifier instanceof Field) {
      return identifier;
    }

    if (context instanceof IRowExpressionContext rowContext) {
      IRowMeta rowMeta = rowContext.getRowMeta();
      int ordinal = rowMeta.indexOfValue(identifier.getName());
      if (ordinal >= 0) {
        hasChanged = true;
        return new Field(rowContext, identifier.getType(), rowMeta.getValueMeta(ordinal), ordinal);
      }
    }

    throw new ExpressionParseException(
        identifier.getPosition(), ErrorCode.UNRESOLVED_IDENTIFIER, identifier.getName());
  }

  @Override
  public IExpression visitCall(Call original) {

    // Compile the operands of a call
    for (int i = 0; i < original.getOperandCount(); i++) {
      original.setOperand(i, original.getOperand(i).accept(this));
    }

    // Compile with operator
    IExpression expression = original.getOperator().compile(context, original);

    if (expression instanceof Call call) {

      // Inferring the return type
      call.inferReturnType();

      // Coerce operands data type
      hasChanged |= call.getOperator().coerceOperandsType(call);

      // If something changed
      if (!call.equals(original)) {
        hasChanged = true;
      }

      // Evaluate if the call is constant
      if (call.isConstant()) {
        try {
          Object value = call.getValue();

          if (value instanceof Array array) {
            return array;
          }

          Type type = call.getType();

          // For CAST operator, it's important to return type
          if (call.isOperator(CastOperator.INSTANCE)) {
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
        }
      }
    }

    return expression;
  }

  @Override
  public IExpression visitArray(Array array) {
    boolean changed = false;
    int size = array.size();
    List<IExpression> expressions = new ArrayList<>(size);
    List<Type> types = new ArrayList<>(size);

    for (IExpression expression : array) {
      IExpression compiled = expression.accept(this);
      if (compiled != expression) {
        changed = true;
      }
      expressions.add(compiled);
      types.add(compiled.getType());
    }

    if (changed) {
      hasChanged = true;
      Type elementType = Types.getLeastRestrictive(types);
      return new Array(ArrayType.of(elementType), expressions);
    }

    return array;
  }

  @Override
  public IExpression visitLiteral(Literal literal) {
    return literal;
  }
}
