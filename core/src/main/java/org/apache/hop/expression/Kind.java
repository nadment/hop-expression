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
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.apache.hop.expression;

import java.util.Collection;

/** Enumerates the possible types of {@link IExpression}. */
public enum Kind {
  LITERAL,

  LIST,

  IDENTIFIER,
  
  JSON,

  // -------------------------------------------------------------
  // Operator Function
  // -------------------------------------------------------------
    
  FUNCTION,

  EXTRACT,
  
  STARTSWITH,
  ENDSWITH,
  
  /**
   * Converts a value of one data type into another data type <code>::</code> or <code>
   * CAST(value AS type FORMAT format)</code>.
   */
  CAST,
  TRY_CAST,
  
  // -------------------------------------------------------------
  // Operator BITWISE
  // -------------------------------------------------------------

  BITGET,

  /** The bitwise AND operation. */
  BITAND,

  /** The bitwise OR operation. */
  BITOR,

  /** The bitwise NOT operation. */
  BITNOT,

  /** The bitwise XOR operation. */
  BITXOR,

  // LSHIFT,

  // RSHIFT,

  // -------------------------------------------------------------
  // LOGICAL
  // -------------------------------------------------------------

  /** The logical <code>AND</code> operator. */
  LOGICAL_AND,

  /** The logical <code>NOT</code> operator. */
  LOGICAL_NOT,

  /** The logical <code>OR</code> operator. */
  LOGICAL_OR,

  /** The logical <code>XOR</code> operator. */
  LOGICAL_XOR,

  // -------------------------------------------------------------
  // COMPARISON
  // -------------------------------------------------------------

  /** Contains function */
  CONTAINS,

  /** The "IN" operator. */
  IN,

  /** The "BETWEEN" operator. */
  BETWEEN,

  /** The less-than operator '&lt;'. */
  LESS_THAN,

  /** The greater-than operator '&gt;'. */
  GREATER_THAN,

  /** The less-than-or-equal operator '&lt;='. */
  LESS_THAN_OR_EQUAL,

  /** The greater-than-or-equal operator '&gt;='. */
  GREATER_THAN_OR_EQUAL,

  /** The equals operator '='. */
  EQUAL,

  /**
   * Compares whether two expressions are equal.
   *
   * <p>
   * The function is NULL-safe, meaning it treats NULLs as known values for comparing equality. Note
   * that this is different from the EQUAL comparison operator (=), which treats NULLs as unknown
   * values.
   */
  EQUAL_NULL,

  /** The not-equals operator "&lt;&gt;". @See {@link #LESS_THAN_OR_GREATER_THAN} */
  NOT_EQUAL,

  /** The not-equals operator '!=' @See {@link #NOT_EQUAL} */
  LESS_THAN_OR_GREATER_THAN,

  /** The IS NULL or <code>IS TRUE</code> operator. */
  IS,

  /** The LIKE operator. */
  LIKE,

  /** The ILIKE case-insensitive operator. */
  ILIKE,

  // -------------------------------------------------------------
  // CONDITIONAL
  // -------------------------------------------------------------

  /** Case when operator */
  CASE_WHEN,

  // -------------------------------------------------------------
  // STRING
  // -------------------------------------------------------------

  /** String concatenation operator '<code>||</code>'. @See {@link #CONCAT} */
  CONCAT,


  // -------------------------------------------------------------
  // MATHEMATICAL
  // -------------------------------------------------------------

  /** The arithmetic division operator '/'. */
  DIVIDE,

  /** The arithmetic multiplication operator '*'. */
  MULTIPLY,

  /** Returns the exponential value of a numeric expression. */
  EXP,

  /** The arithmetic power operator '**' or function. */
  POWER,

  /** The arithmetic remainder operator '%'. The function returns the remainder division. */
  MOD,

  /** The arithmetic unary minus (negative) operator '-'. */
  NEGATIVE,

  /** The arithmetic addition operator '+'. */
  ADD,

  /** The arithmetic subtract operator '-'. */
  SUBTRACT
  ;
  
  public final boolean is(Collection<Kind> category) {
    return category.contains(this);
  }
}
