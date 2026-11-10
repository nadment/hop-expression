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

import org.apache.hop.expression.type.Type;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * The parameter of a {@link Lambda}, referenced by name in the body of the lambda.
 *
 * <p>The parser creates one parameter per lambda and uses the same instance for each reference in
 * the body. The function calling the lambda gives the type of the parameter when it validates the
 * lambda, then binds a value to the parameter before each evaluation of the body.
 *
 * <p>As the value is stored in the parameter, an expression with a lambda must not be evaluated by
 * several threads.
 */
@NullMarked
public final class LambdaParameter extends Identifier {

  /** The value bound to the parameter for the current evaluation of the body. */
  private @Nullable Object value;

  /**
   * Creates a lambda parameter.
   *
   * @param position the position in the expression source
   * @param name the name of the parameter
   */
  public LambdaParameter(int position, String name) {
    super(position, name);
  }

  /**
   * Sets the type of the parameter, given by the function calling the lambda.
   *
   * @param type the type of the values bound to the parameter
   */
  void setType(Type type) {
    this.type = type;
  }

  /**
   * Binds a value to the parameter, used by the next evaluation of the body.
   *
   * @param value the value, of the type of the parameter
   */
  void setValue(@Nullable Object value) {
    this.value = value;
  }

  /** Returns the value bound to the parameter. */
  @Override
  public @Nullable Object getValue() {
    return value;
  }

  /** A parameter is typed by the function calling the lambda, not resolved in the row. */
  @Override
  public void validate(final IExpressionContext context) throws ExpressionException {
    // Nothing to do
  }

  /**
   * Compares by name like an identifier, so the same lambda parsed twice is equal. The bound value
   * is an evaluation state, not compared. Two parameters with the same name are never confused: in
   * the body of an inner lambda, the name always refers to the inner parameter, which hides the
   * outer one.
   */
  @Override
  public boolean equals(@Nullable Object other) {
    return super.equals(other);
  }

  @Override
  public int hashCode() {
    return super.hashCode();
  }
}
