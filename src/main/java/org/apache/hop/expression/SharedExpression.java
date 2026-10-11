/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
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
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * A sub-expression used several times, evaluated at most once per state of the context, so once per
 * row with a {@link IRowExpressionContext}.
 *
 * <p>The value is computed on first use and kept until the state of the context changes, see {@link
 * IExpressionContext#getContextVersion()}. An evaluation error is not kept. As the value is stored
 * in the expression, an expression containing a shared sub-expression must not be evaluated by
 * several threads.
 *
 * @see CommonSubexpressionEliminator
 */
@NullMarked
public final class SharedExpression implements IExpression {

  private final IExpressionContext context;
  private final IExpression expression;

  /** The version of the context for which the value was computed. */
  private long contextVersion = -1;

  private @Nullable Object value;

  /**
   * Creates a shared expression.
   *
   * @param context the context providing the version of its state
   * @param expression the expression to evaluate once per state of the context
   */
  public SharedExpression(IExpressionContext context, IExpression expression) {
    this.context = context;
    this.expression = expression;
  }

  /**
   * Returns the expression evaluated once per state of the context.
   *
   * @return the expression
   */
  public IExpression getExpression() {
    return expression;
  }

  @Override
  public @Nullable Object getValue() {
    long version = context.getContextVersion();
    if (version != contextVersion) {
      value = expression.getValue();
      contextVersion = version;
    }
    return value;
  }

  @Override
  public Kind getKind() {
    return expression.getKind();
  }

  @Override
  public Type getType() {
    return expression.getType();
  }

  @Override
  public int getCost() {
    return expression.getCost();
  }

  @Override
  public void validate(IExpressionContext context) throws ExpressionException {
    expression.validate(context);
  }

  @Override
  public <E> E accept(IExpressionVisitor<E> visitor) {
    return expression.accept(visitor);
  }

  @Override
  public void unparse(StringWriter writer, int leftPrec, int rightPrec) {
    expression.unparse(writer, leftPrec, rightPrec);
  }

  @Override
  public int hashCode() {
    return expression.hashCode();
  }

  @Override
  public boolean equals(@Nullable Object other) {
    return other instanceof SharedExpression shared && expression.equals(shared.expression);
  }

  @Override
  public String toString() {
    return expression.toString();
  }
}
