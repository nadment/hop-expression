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

import java.io.StringWriter;
import org.apache.hop.expression.type.Type;

/**
 * An expression is a combination of one or more literal, identifiers, list of expressions or a call
 * to an operator that evaluate to a value.
 */
public interface IExpression {

  /**
   * Returns the kind of expression.
   *
   * @return a {@link Kind} value, never null
   * @see #is
   */
  Kind getKind();

  default boolean is(final Kind kind) {
    return getKind() == kind;
  }

  /**
   * Returns the data type of expression.
   *
   * @return a {@link Type} value
   */
  Type getType();

  /** Check if the expression is a call to this operator or an alias of this operator. */
  default boolean isOperator(Operator operator) {
    return false;
  }

  /**
   * Check if this expression will always return the NULL value.
   *
   * @return {@code true} if the expression is constant NULL value
   */
  default boolean isNull() {
    return false;
  }

  /**
   * Check if this expression will always return the same result when invoked and has no side
   * effect.
   *
   * @return {@code true} if this is a constant expression.
   */
  default boolean isConstant() {
    return false;
  }

  /**
   * Estimate the cost to process the expression, used when optimizing the expression.
   *
   * @return the estimated cost
   */
  int getCost();

  /**
   * Evaluates the value of this expression.
   *
   * @return The result of evaluating the expression.
   */
  default Object getValue() {
    throw new UnsupportedOperationException(ErrorCode.INTERNAL_ERROR.message(this));
  }

  /**
   * Evaluates the value of this expressions a given Java type.
   *
   * @param clazz Desired value type
   * @param <T> Value type
   * @return The result of evaluating the expression in desired type
   */
  default <T extends Object> T getValue(Class<T> clazz) {
    throw new UnsupportedOperationException(ErrorCode.INTERNAL_ERROR.message(this));
  }

  /**
   * Validate the expression
   *
   * @param context The context against which the expression will be validated.
   * @throws ExpressionException if an error occurs.
   */
  void validate(IExpressionContext context) throws ExpressionException;

  /**
   * Accepts a visitor and dispatching to the right overloaded {@link IEpressionVisitor#apply}
   * method.
   */
  <E> E accept(IExpressionVisitor<E> visitor);

  /**
   * Appends this expression statement to the specified writer. This may not always be the original
   * expression statement, specially after optimization.
   *
   * <p>The <code>leftPrec</code> and <code>rightPrec</code> parameters give us enough context to
   * decide whether we need to enclose the expression in parentheses. For example, we need
   * parentheses around "2 + 3" if preceded by "5 *". This is because the precedence of the "*"
   * operator is greater than the precedence of the "+" operator.
   *
   * @param writer Target writer
   * @param leftPrec The precedence of the {@link IExpression} immediately preceding
   * @param rightPrec The precedence of the {@link IExpression} immediately
   */
  void unparse(final StringWriter writer, int leftPrec, int rightPrec);
}
