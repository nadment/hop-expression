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
import java.util.Arrays;
import java.util.List;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.expression.type.ArrayType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;
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

    // Evaluate the common sub-expressions once per state of the context
    return CommonExpressionEliminator.eliminate(context, expression);
  }

  @Override
  public IExpression visitIdentifier(Identifier identifier) {
    // Check if already compiled, a lambda parameter is not read from the row
    if (identifier instanceof Field || identifier instanceof LambdaParameter) {
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

    throw new ParseExpressionException(
        identifier.getPosition(), ErrorCode.UNRESOLVED_IDENTIFIER, identifier.getName());
  }

  @Override
  public IExpression visitCall(Call original) {

    // Compile the operands of a call
    for (int i = 0; i < original.getOperandCount(); i++) {
      original.setOperand(i, original.getOperand(i).accept(this));
    }

    // Compile with operator, the operands of a symmetrical operator in a normal order
    IExpression expression =
        original.getOperator().compile(context, normalizeSymmetrical(original));

    // A call simplified to NULL, like "x / NULL" or "NULL LIKE x", keeps the type of the call
    // rather than the type of the NULL operand it returns. Not when the type of the call is not
    // known, like a user defined function whose replacing expression gives the type.
    if (expression.isNull()
        && !original.getType().isName(TypeName.UNKNOWN)
        && !original.getType().isName(TypeName.ANY)
        && !expression.getType().equals(original.getType().withNullability(true))) {
      hasChanged = true;
      return new Literal(null, original.getType().withNullability(true));
    }

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

  /**
   * Puts the operands of a call to a symmetrical operator in a normal order, so the same
   * expressions are recognized whatever the order of their operands, like {@code A IS DISTINCT FROM
   * B AND B IS DISTINCT FROM A}. The operands are ordered like the operands of a chain of {@code
   * AND}, the simplification rules of the symmetrical operators expect this order.
   *
   * @param call the call
   * @return the call with its operands in a normal order, or the call itself
   */
  private static Call normalizeSymmetrical(Call call) {
    Operator operator = call.getOperator();
    if (!operator.isSymmetrical()) {
      return call;
    }
    IExpression[] operands = call.getOperands().clone();
    Arrays.sort(operands, new ExpressionComparator());
    if (Arrays.equals(operands, call.getOperands())) {
      return call;
    }
    Call normalized = new Call(call.getPosition(), operator, operands);
    normalized.inferReturnType();
    return normalized;
  }

  @Override
  public IExpression visitArray(Array array) {
    boolean changed = false;
    boolean replaced = false;
    int size = array.size();
    List<IExpression> expressions = new ArrayList<>(size);
    List<Type> types = new ArrayList<>(size);

    for (IExpression expression : array) {
      IExpression compiled = expression.accept(this);
      if (!compiled.equals(expression)) {
        changed = true;
      }
      // An element can be replaced by an equal expression that must be kept, like the generic
      // CONCAT replaced by its string, binary or array implementation.
      if (compiled != expression) {
        replaced = true;
      }
      expressions.add(compiled);
      types.add(compiled.getType());
    }

    if (replaced) {
      hasChanged |= changed;
      Type elementType = Types.getLeastRestrictive(types);
      return new Array(ArrayType.of(elementType), expressions);
    }

    return array;
  }

  @Override
  public IExpression visitLiteral(Literal literal) {
    return literal;
  }

  /** Compiles the body of a lambda, the parameter is kept. */
  @Override
  public IExpression visitLambda(Lambda lambda) {
    IExpression expression = lambda.getExpression().accept(this);
    if (expression == lambda.getExpression()) {
      return lambda;
    }
    return new Lambda(lambda.getPosition(), lambda.getParameters(), expression);
  }
}
