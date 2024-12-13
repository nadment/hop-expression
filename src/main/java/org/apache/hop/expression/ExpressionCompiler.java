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
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;
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
  public IExpression visitCall(final Call original) {

    // Compile the operands of a call
    for (int i = 0; i < original.getOperandCount(); i++) {
      original.setOperand(i, original.getOperand(i).accept(this));
    }

    // Compile with operator
    IExpression expression = original.getOperator().compile(context, original);

    if (expression instanceof Call call) {

      // Coerce operands data type
      changed |= call.getOperator().coerceOperandsType(call);

      // Inferring return type
      call.inferReturnType();

      // If something changed
      if (!call.equals(original)) {
        changed = true;
      }

      // Evaluate if the call is constant
      if (expression.isConstant()) {
        try {
          Type type = expression.getType();
          Object value = expression.getValue();

          if (value instanceof Array array) {
            return array;
          }

          // Some operator don't known return type like JSON_VALUE.
          if (TypeName.ANY.equals(type.getName())) {
            if (value == null) {
              type = Types.STRING;
            } else if (value instanceof Boolean) {
              type = Types.BOOLEAN;
            } else if (value instanceof Long integer) {
              type = IntegerType.from(integer);
            } else if (value instanceof BigDecimal number) {
              type = NumberType.from(number);
            } else if (value instanceof String string) {
              type = StringType.from(string);
            } else if (value instanceof ZonedDateTime) {
              type = Types.DATE;
            } else if (value instanceof Interval) {
              type = Types.INTERVAL;
            } else if (value instanceof JsonNode) {
              type = Types.JSON;
            } else if (value instanceof byte[] bytes) {
              type = BinaryType.from(bytes);
            } else {
              return call;
            }
          }
          // For CAST operator, it's important to return type
          else if (expression.isOperator(Operators.CAST)) {
            value = type.cast(value);
          }

          changed = true;

          // If the value is null, change nullability of the type.
          if (value == null) {
            type = type.withNullability(true);
          }

          return new Literal(value, type);
        } catch (Exception e) {
          // Ignore error like division by zero "X IN (1,3/0)" and continue
          expression = call;
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
