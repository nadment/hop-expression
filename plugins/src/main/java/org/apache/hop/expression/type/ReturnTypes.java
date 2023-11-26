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
 * A collection of strategies for return type inference.
 */
public final class ReturnTypes {

  private ReturnTypes() {
    // Utility class
  }

  /**
   * Creates an inference rule which returns a type with no precision or scale,
   * such as {@code DATE}.
   */
  public static ExplicitReturnTypeInference explicit(Type type) {
    return new ExplicitReturnTypeInference(type);
  }

  public static final IReturnTypeInference ANY = explicit(AnyType.ANY);

  /**
   * Type-inference strategy whereby the result type of a call is BOOLEAN.
   */
  public static final IReturnTypeInference BOOLEAN = explicit(BooleanType.BOOLEAN);

  /**
   * Type-inference strategy whereby the result type of a call is BOOLEAN NOT NULL.
   */
  public static final IReturnTypeInference BOOLEAN_NOT_NULL =
      BOOLEAN.andThen(TypeTransforms.TO_NOT_NULLABLE);

  /**
   * Type-inference strategy whereby the result type of a call is BINARY.
   */
  public static final IReturnTypeInference BINARY = explicit(BinaryType.BINARY);

  /**
   * Type-inference strategy whereby the result type of a call is STRING.
   */
  public static final IReturnTypeInference STRING = explicit(StringType.STRING);

  /**
   * Type-inference strategy whereby the result type of a call is STRING NOT NULL.
   */
  public static final IReturnTypeInference STRING_NOT_NULL =
      STRING.andThen(TypeTransforms.TO_NOT_NULLABLE);

  /**
   * Type-inference strategy whereby the result type of a call is INTEGER.
   */
  public static final IReturnTypeInference INTEGER = explicit(IntegerType.INTEGER);

  /**
   * Type-inference strategy whereby the result type of a call is NUMBER.
   */
  public static final IReturnTypeInference NUMBER = explicit(NumberType.NUMBER);

  /**
   * Type-inference strategy whereby the result type of a call is DATE.
   */
  public static final IReturnTypeInference DATE = explicit(DateType.DATE);

  /**
   * Type-inference strategy whereby the result type of a call is DATE NOT NULL.
   */
  public static final IReturnTypeInference DATE_NOT_NULL =
      DATE.andThen(TypeTransforms.TO_NOT_NULLABLE);

  /**
   * Type-inference strategy whereby the result type of a call is DATE.
   */
  public static final IReturnTypeInference JSON = explicit(JsonType.JSON);

  /**
   * Type-inference strategy whereby the result type of a call is INTERVAL.
   */
  public static final IReturnTypeInference INTERVAL = explicit(IntervalType.INTERVAL);

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

  public static final IReturnTypeInference FIRST_KNOWN =
      new FirstKnownReturnTypeInference().andThen(TypeTransforms.TO_MAX_PRECISION);

  public static final IReturnTypeInference LEAST_RESTRICTIVE =
      new LeastRestrictiveReturnTypeInference().andThen(TypeTransforms.TO_MAX_PRECISION);

  public static final IReturnTypeInference ABS_FUNCTION = call -> {
    Type type = call.getOperand(0).getType();

    if (type.is(TypeName.INTEGER))
      return type;

    // By default Number for coercion
    return NumberType.NUMBER;
  };

  public static final IReturnTypeInference ARG0_MAX_PRECISION =
      new OrdinalReturnTypeInference(0).andThen(TypeTransforms.TO_MAX_PRECISION);

  /**
   * TODO: Type-inference strategy whereby the result type of a call is {@link #NUMBER_SCALE0} with
   * a fallback to {@link #ARG0}.
   * This rule is used for floor, ceiling.
   */
  public static final IReturnTypeInference ARG0_OR_EXACT_NO_SCALE = call -> {
    Type type = call.getOperand(0).getType();

    if (type.getScale() > 0) {
      return new NumberType(type.getPrecision(), 0);
    }

    return IntegerType.INTEGER;
  };

  public static final IReturnTypeInference TRY = new TryReturnTypeInference();
  public static final IReturnTypeInference ADDITIVE_OPERATOR =
      new AdditiveOperatorReturnTypeInference();
  public static final IReturnTypeInference MULTIPLY_OPERATOR =
      new MultiplyOperatorReturnTypeInference();
  public static final IReturnTypeInference DIVIDE_OPERATOR =
      new DivideOperatorReturnTypeInference();
  public static final IReturnTypeInference MOD_OPERATOR = new ModOperatorReturnTypeInference();
  public static final IReturnTypeInference CASE_OPERATOR = new CaseOperatorReturnTypeInference();
  public static final IReturnTypeInference CAST_OPERATOR = new CastOperatorReturnTypeInference();
  public static final IReturnTypeInference IF_FUNCTION = new IfFunctionReturnTypeInference();

  public static ReturnTypeInferenceChain chain(IReturnTypeInference... rules) {
    return new ReturnTypeInferenceChain(rules);
  }

  /**
   * Creates a return-type inference that applies a rule then a sequence of transforms.
   **/
  public static ReturnTypeTransformCascade cascade(IReturnTypeInference rule,
      ITypeTransform... transforms) {
    return new ReturnTypeTransformCascade(rule, transforms);
  }
}
