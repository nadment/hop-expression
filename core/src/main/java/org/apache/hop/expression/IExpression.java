/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
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
 * An expression is a combination of one or more values, resolvable identifiers, operators and
 * functions that evaluate to a value.
 *
 * @author Nicolas ADMENT
 */
public interface IExpression {

  /**
   * Check if this expression will always return the same value.
   *
   * @return if the expression is constant
   */
  public boolean isConstant();


  /**
   * Return the resulting value for the current context.
   *
   * @param context the context
   * @return the result
   */
  public Value eval(IExpressionContext context) throws ExpressionException;
  
  public Kind getKind();
 
  default public boolean is(Kind kind) {
    return getKind() == kind;
  }
  
  /**
   * Estimate the cost to process the expression. Used when optimizing the query, to optimize the
   * expression with the lowest estimated cost.
   *
   * @return the estimated cost
   */
  public int getCost();

  /**
   * Try to optimize the expression.
   *
   * @param context the context
   * @return the optimized expression
   */
  public IExpression optimize(IExpressionContext context) throws ExpressionException;
  

  /**
   * Appends this expression statement to the specified writer. This may not always be the original
   * expression statement, specially after optimization.
   */
  public void write(StringWriter writer, int leftPrec, int rightPrec);
}
