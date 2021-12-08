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

/**
 * An expression is a combination of one or more literal, resolvable identifiers, operators and
 * functions that evaluate to a value.
 */
public interface IExpression {

  /**
   * Returns the type of expression.
   *
   * @return a {@link Kind} value, never null
   * @see #isKind
   */
  public Kind getKind();

  public default boolean isKind(Kind kind) {
    return getKind() == kind;
  }

  /** Check if the expression is a call to this operator or a alias of this operator */
  public default boolean isOperator(Operator operator) {
    if (operator == null)
      return false;
    if (this instanceof OperatorCall) {
      OperatorCall call = (OperatorCall) this;

      if (operator.isSame(call.getOperator()))
        return true;
    }

    return false;
  }

  /**
   * Estimate the cost to process the expression. Used when optimizing the query, to optimize the
   * expression with the lowest estimated cost.
   *
   * @return the estimated cost
   */
  public int getCost();

  /**
   * Evaluates the expression.
   *
   * @param context The context against which the expression will be evaluated.
   *
   * @return The result of evaluating the expression.
   */
  public Object eval(IExpressionContext context) throws ExpressionException;

  /**
   * Appends this expression statement to the specified writer. This may not always be the original
   * expression statement, specially after optimization.
   */
  public void write(StringWriter writer);
}
