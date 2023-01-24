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
package org.apache.hop.expression.type;

/**
 * A collection of return-type inference strategies.
 */
public final class ReturnTypes {

  private ReturnTypes() {
    // Utility class
  }

  public static final IReturnTypeInference UNKNOWN =
      new ExplicitReturnTypeInference(DataTypeName.UNKNOWN);

  /**
   * Type-inference strategy whereby the result type of a call is BOOLEAN.
   */
  public static final IReturnTypeInference BOOLEAN =
      new ExplicitReturnTypeInference(DataTypeName.BOOLEAN);

  /**
   * Type-inference strategy whereby the result type of a call is BINARY.
   */
  public static final IReturnTypeInference BINARY =
      new ExplicitReturnTypeInference(DataTypeName.BINARY);

  /**
   * Type-inference strategy whereby the result type of a call is STRING.
   */
  public static final IReturnTypeInference STRING =
      new ExplicitReturnTypeInference(DataTypeName.STRING);

  /**
   * Type-inference strategy whereby the result type of a call is INTEGER.
   */
  public static final IReturnTypeInference INTEGER =
      new ExplicitReturnTypeInference(DataTypeName.INTEGER);

  /**
   * Type-inference strategy whereby the result type of a call is NUMBER.
   */
  public static final IReturnTypeInference NUMBER =
      new ExplicitReturnTypeInference(DataTypeName.NUMBER);

  /**
   * Type-inference strategy whereby the result type of a call is BIGNUMBER.
   */
  public static final IReturnTypeInference BIGNUMBER =
      new ExplicitReturnTypeInference(DataTypeName.BIGNUMBER);

  /**
   * Type-inference strategy whereby the result type of a call is DATE.
   */
  public static final IReturnTypeInference DATE =
      new ExplicitReturnTypeInference(DataTypeName.DATE);

  /**
   * Type-inference strategy whereby the result type of a call is DATE.
   */
  public static final IReturnTypeInference JSON =
      new ExplicitReturnTypeInference(DataTypeName.JSON);

  /**
   * Type-inference strategy whereby the result type of a call is the type of the operand #0.
   */
  public static final IReturnTypeInference ARG0 = new OrdinalReturnTypeInference(0);

  /**
   * Type-inference strategy whereby the result type of a call is the type of
   * the operand #1.
   */
  public static final IReturnTypeInference ARG1 = new OrdinalReturnTypeInference(1);

  /**
   * Type-inference strategy whereby the result type of a call is the type of
   * the operand #2.
   */
  public static final IReturnTypeInference ARG2 = new OrdinalReturnTypeInference(2);

  public static final IReturnTypeInference ARG1_OR_ARG2 =
      chain(new OrdinalReturnTypeInference(1), new OrdinalReturnTypeInference(2));

  public static final IReturnTypeInference FIRST_KNOWN = new FirstKnownReturnTypeInference();

  public static final IReturnTypeInference LEAST_RESTRICTIVE =
      new LeastRestrictiveReturnTypeInference();

  public static final IReturnTypeInference CAST = new CastFunctionReturnTypeInference();

  public static ReturnTypeInferenceChain chain(IReturnTypeInference... rules) {
    return new ReturnTypeInferenceChain(rules);
  }
}
