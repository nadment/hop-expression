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

import com.fasterxml.jackson.databind.JsonNode;
import java.math.BigDecimal;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import org.apache.hop.expression.type.ArrayType;
import org.apache.hop.expression.type.Interval;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeId;
import org.apache.hop.expression.type.Types;

public class ExpressionCompiler implements IExpressionVisitor<IExpression> {

  private final IExpressionContext context;
  private boolean changed;

  public ExpressionCompiler(IExpressionContext context) {
    this.context = context;
  }

  public IExpression compile(IExpression expression) throws ExpressionException {

    int loop = 0;
    do {
      changed = false;
      // System.out.println(expression);
      expression = expression.accept(this);

      // Check a maximum loop to avoid infinite loop
      if (loop++ > 1000) {
        throw new ExpressionException(
            ErrorCode.INTERNAL_ERROR,
            "compilation: Too many try for expression %1$s".formatted(expression));
      }
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
    for (int i = 0; i < call.getOperandCount(); i++) {
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
          Type constantType = expression.getType();
          Object value = expression.getValue();

          if (value instanceof Array array) {
            return array;
          }

          // Some operator don't known return type like JSON_VALUE.
          if (TypeId.ANY.equals(constantType.getId())) {
            if (value instanceof Boolean bool) {
              return Literal.of(bool);
            }
            if (value instanceof Long number) {
              return Literal.of(number);
            }
            if (value instanceof BigDecimal number) {
              return Literal.of(number);
            }
            if (value instanceof String str) {
              return Literal.of(str);
            }
            if (value instanceof ZonedDateTime datetime) {
              return Literal.of(datetime);
            }
            if (value instanceof Interval interval) {
              return Literal.of(interval);
            }
            if (value instanceof JsonNode json) {
              return Literal.of(json);
            }
            if (value instanceof byte[] bytes) {
              return Literal.of(bytes);
            }

            return call;
          } else
          // For CAST operator, it's important to return type
          if (expression.isOperator(Operators.CAST)) {
            value = constantType.cast(value);
          }

          changed = true;

          return new Literal(value, constantType);
        } catch (Exception e) {
          // Ignore error like division by zero "X IN (1,3/0)" and continue
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

    Type type = Types.getLeastRestrictive(types);

    return new Array(ArrayType.of(type), expressions);
  }

  @Override
  public IExpression visitLiteral(final Literal literal) {
    return literal;
  }
}
