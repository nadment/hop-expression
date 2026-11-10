/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations
 * under the License.
 */
package org.apache.hop.expression;

import static java.util.Objects.requireNonNull;

import java.io.StringWriter;
import java.util.List;
import java.util.Objects;
import lombok.Getter;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * An anonymous function <code>parameter -&gt; expression</code> or <code>(parameter1, parameter2)
 * -&gt; expression</code>, the argument of a higher-order function like {@code TRANSFORM} or {@code
 * FILTER} which evaluates the expression for each element of an array.
 *
 * <p>The expression can reference the parameters and the fields of the row. A lambda is only valid
 * as the argument of a function that gives the type of its parameters, see {@link
 * #validate(IExpressionContext, Type...)}.
 *
 * <pre>{@code
 * Lambda lambda = (Lambda) call.getOperand(1);
 * Object result = lambda.apply(element);
 * }</pre>
 */
@NullMarked
public final class Lambda implements IExpression {

  /** The position of the lambda in the expression source. */
  @Getter private final int position;

  /** The parameters of the lambda, at least one, with distinct names. */
  @Getter private final List<LambdaParameter> parameters;

  /** The expression evaluated for each set of values bound to the parameters. */
  @Getter private final IExpression expression;

  /**
   * Creates a lambda.
   *
   * @param position the position in the expression source
   * @param parameters the parameters, referenced in the expression
   * @param expression the expression evaluated for each set of values of the parameters
   */
  public Lambda(int position, List<LambdaParameter> parameters, IExpression expression) {
    this.position = position;
    this.parameters = List.copyOf(parameters);
    this.expression = requireNonNull(expression, "expression");
  }

  /**
   * Returns the number of parameters, checked by the function calling the lambda.
   *
   * @return the number of parameters
   */
  public int getParameterCount() {
    return parameters.size();
  }

  @Override
  public Kind getKind() {
    return Kind.LAMBDA;
  }

  /** Returns the type of the expression, the type of the values returned by the lambda. */
  @Override
  public Type getType() {
    return expression.getType();
  }

  @Override
  public int getCost() {
    return expression.getCost() + 1;
  }

  /**
   * Rejects a lambda whose parameters are not typed by an enclosing function, so used elsewhere
   * than as the argument of a higher-order function.
   *
   * @throws ExpressionException with {@link ErrorCode#LAMBDA_NOT_ALLOWED}
   */
  @Override
  public void validate(final IExpressionContext context) throws ExpressionException {
    for (LambdaParameter parameter : parameters) {
      if (parameter.getType().isName(TypeName.UNKNOWN)) {
        throw new ParseExpressionException(position, ErrorCode.LAMBDA_NOT_ALLOWED, this);
      }
    }
    expression.validate(context);
  }

  /**
   * Validates the lambda as the argument of a higher-order function, which gives the type of the
   * parameters. The function checks the number of parameters first, see {@link
   * #getParameterCount()}.
   *
   * @param context the context against which the expression is validated
   * @param parameterTypes the types of the values bound to the parameters, in order
   * @throws ExpressionException if the expression is not valid
   */
  public void validate(final IExpressionContext context, final Type... parameterTypes)
      throws ExpressionException {
    checkArgumentCount(parameterTypes.length);
    for (int i = 0; i < parameterTypes.length; i++) {
      parameters.get(i).setType(parameterTypes[i]);
    }
    expression.validate(context);
  }

  /**
   * Evaluates the expression with values bound to the parameters.
   *
   * @param arguments the values of the parameters, in order, of the type of each parameter
   * @return the value of the expression
   */
  public @Nullable Object apply(final @Nullable Object... arguments) {
    bind(arguments);
    return expression.getValue();
  }

  /**
   * Evaluates the expression with values bound to the parameters, then converts the result to a
   * Java type.
   *
   * @param clazz the desired Java type
   * @param arguments the values of the parameters, in order, of the type of each parameter
   * @param <T> the value type
   * @return the value of the expression in the desired type
   */
  public @Nullable <T> T apply(final Class<T> clazz, final @Nullable Object... arguments) {
    bind(arguments);
    return expression.getValue(clazz);
  }

  private void bind(final @Nullable Object[] arguments) {
    checkArgumentCount(arguments.length);
    for (int i = 0; i < arguments.length; i++) {
      parameters.get(i).setValue(arguments[i]);
    }
  }

  /** The number of arguments given by the function must match the number of parameters. */
  private void checkArgumentCount(int count) {
    if (count != parameters.size()) {
      throw new ExpressionException(
          ErrorCode.INTERNAL_ERROR, "lambda %s called with %d arguments".formatted(this, count));
    }
  }

  /**
   * A lambda is constant if its expression only depends on the parameters and on constant values,
   * so a call like {@code TRANSFORM([1,2], x -> x * 2)} can be evaluated when compiled.
   */
  @Override
  public boolean isConstant() {
    return isConstant(expression);
  }

  private boolean isConstant(final IExpression expression) {
    // Compare by identity, a parameter of an enclosing lambda can have the same name
    for (LambdaParameter parameter : parameters) {
      if (expression == parameter) {
        return true;
      }
    }
    if (expression instanceof Call call) {
      if (!call.getOperator().isDeterministic()) {
        return false;
      }
      for (IExpression operand : call.getOperands()) {
        if (!isConstant(operand)) {
          return false;
        }
      }
      return true;
    }
    if (expression instanceof Array array) {
      for (IExpression element : array) {
        if (!isConstant(element)) {
          return false;
        }
      }
      return true;
    }
    return expression.isConstant();
  }

  @Override
  public boolean isDeterministic() {
    return expression.isDeterministic();
  }

  @Override
  public <E> E accept(IExpressionVisitor<E> visitor) {
    return visitor.visitLambda(this);
  }

  @Override
  public void unparse(StringWriter writer, int leftPrec, int rightPrec) {
    // A single parameter is written without parentheses
    if (parameters.size() == 1) {
      parameters.get(0).unparse(writer, 0, 0);
    } else {
      writer.append('(');
      for (int i = 0; i < parameters.size(); i++) {
        if (i > 0) writer.append(',');
        parameters.get(i).unparse(writer, 0, 0);
      }
      writer.append(')');
    }
    writer.append("->");
    expression.unparse(writer, 0, 0);
  }

  @Override
  public boolean equals(@Nullable Object other) {
    if (this == other) return true;
    if (other == null || getClass() != other.getClass()) {
      return false;
    }
    Lambda lambda = (Lambda) other;
    return parameters.equals(lambda.parameters) && expression.equals(lambda.expression);
  }

  @Override
  public int hashCode() {
    return Objects.hash(parameters, expression);
  }

  @Override
  public String toString() {
    StringWriter writer = new StringWriter();
    unparse(writer, 0, 0);
    return writer.toString();
  }
}
