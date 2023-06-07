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

  /**
   * Creates an inference rule which returns a type with no precision or scale,
   * such as {@code DATE}.
   */
  public static ExplicitReturnTypeInference explicit(DataType type) {
    return  new ExplicitReturnTypeInference(type);
  }

  
  public static final IReturnTypeInference ANY = explicit(UnknownDataType.ANY);
  
  /**
   * Type-inference strategy whereby the result type of a call is BOOLEAN.
   */
  public static final IReturnTypeInference BOOLEAN = explicit(BooleanDataType.BOOLEAN);

  /**
   * Type-inference strategy whereby the result type of a call is BINARY.
   */
  public static final IReturnTypeInference BINARY = explicit(BinaryDataType.BINARY);

  /**
   * Type-inference strategy whereby the result type of a call is STRING.
   */
  public static final IReturnTypeInference STRING = explicit(StringDataType.STRING);

  /**
   * Type-inference strategy whereby the result type of a call is INTEGER.
   */
  public static final IReturnTypeInference INTEGER = explicit(IntegerDataType.INTEGER);

  /**
   * Type-inference strategy whereby the result type of a call is NUMBER.
   */
  public static final IReturnTypeInference NUMBER = explicit(NumberDataType.NUMBER);

  /**
   * Type-inference strategy whereby the result type of a call is DATE.
   */
  public static final IReturnTypeInference DATE = explicit(DateDataType.DATE);

  /**
   * Type-inference strategy whereby the result type of a call is DATE.
   */
  public static final IReturnTypeInference JSON = explicit(JsonDataType.JSON);

  /**
   * Type-inference strategy whereby the result type of a call is the type of the operand #0.
   */
  public static final IReturnTypeInference ARG0 = new OrdinalReturnTypeInference(0);

  /**
   * Type-inference strategy whereby the result type of a call is the type of the operand #1.
   */
  public static final IReturnTypeInference ARG1 = new OrdinalReturnTypeInference(1);

  /**
   * Type-inference strategy whereby the result type of a call is the type of the operand #2.
   */
  public static final IReturnTypeInference ARG2 = new OrdinalReturnTypeInference(2);

  public static final IReturnTypeInference ARG1_OR_ARG2 =
      chain(new OrdinalReturnTypeInference(1), new OrdinalReturnTypeInference(2));

  public static final IReturnTypeInference FIRST_KNOWN = new FirstKnownReturnTypeInference();

  public static final IReturnTypeInference LEAST_RESTRICTIVE =
      new LeastRestrictiveReturnTypeInference();

  
  /**
   * TODO: Type-inference strategy whereby the result type of a call is {@link #NUMBER_SCALE0} with a fallback to {@link #ARG0}.
   * This rule is used for floor, ceiling.
   */
  public static final IReturnTypeInference ARG0_OR_EXACT_NO_SCALE = (context, call) -> {
    DataType type = call.getOperand(0).getType();
    
    if ( type.getScale()>0) {
      return new NumberDataType(type.getPrecision(), 0); 
    }
    
    return IntegerDataType.INTEGER; 
  };

  public static final IReturnTypeInference TRY = new TryReturnTypeInference();
  public static final IReturnTypeInference ADD_OPERATOR = new AddOperatorReturnTypeInference();
  public static final IReturnTypeInference CASE_OPERATOR = new CaseOperatorReturnTypeInference();
  public static final IReturnTypeInference CAST_OPERATOR = new CastOperatorReturnTypeInference();
  public static final IReturnTypeInference IF_FUNCTION = new IfFunctionReturnTypeInference();
  
  public static ReturnTypeInferenceChain chain(IReturnTypeInference... rules) {
    return new ReturnTypeInferenceChain(rules);
  }
}
