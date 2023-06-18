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


import org.apache.hop.expression.type.DataType;
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
   * @return a {@link DataType} value
   */
  public DataType getType();

  /**
   * Check if the expression is a call to this operator or an alias of this operator.
   */
  public default boolean is(Operator operator) {
    return false;
  }

  /**
   * Check if this expression will always return the NULL value.
   *
   * @return {@code true} if the expression is constant NULL value
   */
  public default boolean isNull() {
    return false;
  }

  /**
   * Check if this expression will always return the same result when invoked and has no side effect.
   * 
   * @return {@code true} if this is a constant expression.  
   */
  public default boolean isConstant() {
    return false;
  }
    
  /**
   * Estimate the cost to process the expression, used when optimizing the expression.
   *
   * @return the estimated cost
   */
  public int getCost();

  /**
   * Evaluates the value of this expression.
   *
   * @throws ExpressionException if an error occurs.
   * @return The result of evaluating the expression.
   */
  public default Object getValue() throws ExpressionException {
    throw new ExpressionException(ExpressionError.INTERNAL_ERROR);
  }
  
  /**
   * Evaluates the value of this expressions a given Java type.
   *
   * @param clazz Desired value type
   * @param <T> Value type
   * @return The result of evaluating the expression in desired type
   */
  public default <T extends Object> T getValue(Class<T> clazz) throws ExpressionException {
    throw new ExpressionException(ExpressionError.INTERNAL_ERROR);
  }

  
  /**
   * Validate the expression
   * 
   * @param context The context against which the expression will be validated.
   * @return 
   * @throws ExpressionException if an error occurs.
   */
  public default void validate(IExpressionContext context) throws ExpressionException {    
  }
  
  /**
   * Compile and optimize the expression
   * 
   * @param context The context against which the expression will be compiled.
   * @return 
   * @throws ExpressionException if an error occurs.
   */
  
  public IExpression compile(IExpressionContext context) throws ExpressionException;
  
  /**
   * Accepts a visitor and dispatching to the right overloaded {@link IEpressionVisitor#apply}
   * method.
   */
  public abstract <E> E accept(IExpressionContext context, IExpressionVisitor<E> visitor);

  /**
   * Casts and returns this expression as a {@link Call} if it is of kind {@code CALL}
   *
   * @return this instance cast to a class
   */  
  public default Call asCall() {
    throw new RuntimeException(ExpressionError.INTERNAL_ERROR.message());
  } 

  /**
   * Casts and returns this expression as a {@link Literal} if it is of kind {@code LITERAL}
   *
   * @return this instance cast to a class
   */
  public default Literal asLiteral() {
    throw new RuntimeException(ExpressionError.INTERNAL_ERROR.message());
  } 

  /**
   * Casts and returns this expression as a {@link Identifier} if it is of kind {@code IDENTIFIER}
   *
   * @return this instance cast to a class
   */  
  public default Identifier asIdentifier() {
    throw new RuntimeException(ExpressionError.INTERNAL_ERROR.message());
  } 
  

  /**
   * Casts and returns this expression as a {@link Tuple} if it is of kind {@code TUPLE}
   *
   * @return this instance cast to a class
   */  
  public default Tuple asTuple() {
    throw new RuntimeException(ExpressionError.INTERNAL_ERROR.message());
  } 
  
  /**
   * Appends this expression statement to the specified writer. This may not always be the original
   * expression statement, specially after optimization.
   */
  public void unparse(StringWriter writer);
}
