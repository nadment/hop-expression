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


import org.apache.hop.expression.type.DataTypeName;
import java.io.StringWriter;

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
  public Kind getKind();

  public default boolean is(Kind kind) {
    return getKind() == kind;
  }

  /**
   * Returns the data type of expression.
   *
   * @return a {@link DataTypeName} value
   */
  public DataTypeName getType();

  /**
   * Check if the expression is a call to this operator or an alias of this operator.
   */
  public default boolean is(Operator operator) {
    return false;
  }

  /**
   * Check if this expression will always return the NULL value.
   *
   * @return if the expression is constant NULL value
   */
  public default boolean isNull() {
    return false;
  }

  /**
   * Estimate the cost to process the expression, used when optimizing the expression.
   *
   * @return the estimated cost
   */
  public int getCost();

  /**
   * Evaluates the expression.
   *
   * @param context The context against which the expression will be evaluated.
   * 
   * @throws ExpressionException if an error occurs.
   * @throws NullPointerException if context is null.
   * 
   * @return The result of evaluating the expression.
   */
  public Object getValue(IExpressionContext context) throws ExpressionException;

  /**
   * Accepts a visitor and dispatching to the right overloaded {@link IEpressionVisitor#apply}
   * method.
   */
  public abstract <E> E accept(IExpressionContext context, IExpressionVisitor<E> visitor);

  /**
   * Appends this expression statement to the specified writer. This may not always be the original
   * expression statement, specially after optimization.
   */
  public void unparse(StringWriter writer);
}
