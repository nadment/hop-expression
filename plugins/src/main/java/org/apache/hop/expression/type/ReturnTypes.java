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

import org.apache.hop.expression.IExpression;

/**
 * A collection of strategies for return type inference.
 */
public final class ReturnTypes {

  private ReturnTypes() {
    // Utility class
  }

  /**
   * Creates a return-type inference rule which returns a type with no precision or scale, such as
   * {@code DATE}.
   */
  public static ExplicitReturnTypeInference explicit(Type type) {
    return new ExplicitReturnTypeInference(type);
  }

  /**
   * Creates a return-type inference that applies a rule then a sequence of rules, returning the
   * first non-null result.
   */
  public static ReturnTypeInferenceChain chain(IReturnTypeInference... rules) {
    return new ReturnTypeInferenceChain(rules);
  }

  /**
   * Creates a return-type inference that applies a rule then a sequence of transforms.
   */
  public static ReturnTypeTransformCascade cascade(IReturnTypeInference rule,
      ITypeTransform... transforms) {
    return new ReturnTypeTransformCascade(rule, transforms);
  }


  public static final IReturnTypeInference ANY = explicit(Types.ANY);

  /**
   * Type-inference strategy whereby the result type of a call is BOOLEAN.
   */
  public static final IReturnTypeInference BOOLEAN_NULLABLE = explicit(Types.BOOLEAN);

  /**
   * Type-inference strategy whereby the result type of a call is BOOLEAN NOT NULL.
   */
  public static final IReturnTypeInference BOOLEAN_NOT_NULL = explicit(Types.BOOLEAN_NOT_NULL);

  /**
   * Type-inference strategy whereby the result type of a call is BINARY.
   */
  public static final IReturnTypeInference BINARY_NULLABLE = explicit(Types.BINARY);

  /**
   * Type-inference strategy whereby the result type of a call is BINARY NOT NULL.
   */
  public static final IReturnTypeInference BINARY_NOT_NULL = explicit(Types.BINARY_NOT_NULL);
  
  /**
   * Type-inference strategy whereby the result type of a call is STRING.
   */
  public static final IReturnTypeInference STRING_NULLABLE = explicit(Types.STRING);

  /**
   * Type-inference strategy whereby the result type of a call is STRING NOT NULL.
   */
  public static final IReturnTypeInference STRING_NOT_NULL = explicit(Types.STRING_NOT_NULL);

  /**
   * Type-inference strategy whereby the result type of a call is INTEGER.
   */
  public static final IReturnTypeInference INTEGER_NULLABLE = explicit(Types.INTEGER);

  /**
   * Type-inference strategy whereby the result type of a call is NUMBER.
   */
  public static final IReturnTypeInference NUMBER_NULLABLE = explicit(Types.NUMBER);

  /**
   * Type-inference strategy whereby the result type of a call is NUMBER NOT NULL.
   */
  public static final IReturnTypeInference NUMBER_NOT_NULL = explicit(Types.NUMBER_NOT_NULL);

  /**
   * Type-inference strategy whereby the result type of a call is DATE.
   */
  public static final IReturnTypeInference DATE_NULLABLE = explicit(Types.DATE);

  /**
   * Type-inference strategy whereby the result type of a call is DATE NOT NULL.
   */
  public static final IReturnTypeInference DATE_NOT_NULL = explicit(Types.DATE_NOT_NULL);

  /**
   * Type-inference strategy whereby the result type of a call is DATE.
   */
  public static final IReturnTypeInference JSON_NULLABLE = explicit(Types.JSON);

  /**
   * Type-inference strategy whereby the result type of a call is INTERVAL.
   */
  public static final IReturnTypeInference INTERVAL_NULLABLE = explicit(Types.INTERVAL);

  /**
   * Type-inference strategy whereby the result type of a call is the type of the operand #0.
   */
  public static final IReturnTypeInference ARG0 = new OrdinalReturnTypeInference(0);

  
  public static final IReturnTypeInference ARG0_NOT_NULL = ARG0.andThen(TypeTransforms.TO_NOT_NULLABLE);
  
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

  public static final IReturnTypeInference ARG0_MAX_PRECISION =
      new OrdinalReturnTypeInference(0).andThen(TypeTransforms.TO_MAX_PRECISION);

  /**
   * Type-inference strategy whereby the result type of a call is a number with scale 0 with precision-scale 
   * and nullity than ARG0.
   * 
   * <p>This rule is used for CEILING, FLOOR.
   */
  public static final IReturnTypeInference CEIL_FLOOR_FUNCTION = call -> {
    Type type = call.getOperand(0).getType();

    int precision = TypeId.NUMBER.getMaxPrecision();
    if ( type.is(TypeId.INTEGER) ) {
      precision = type.getPrecision();
    }
    else if ( type.is(TypeId.NUMBER) ) {
      precision = type.getPrecision()-type.getScale();
      if (precision==0) precision=1;
    }
    
    
    return NumberType.of(precision, 0, type.isNullable());    
  };

  //-------------------------------------------------------------
  //                   FUNCTIONS
  //-------------------------------------------------------------
  
  /**
   * Type-inference strategy for ABS function.
   */
  public static final IReturnTypeInference ABS_FUNCTION = call -> {
    Type type = call.getOperand(0).getType();

    // Keep ARG0 type
    if (type.is(TypeId.INTEGER) || type.is(TypeId.INTERVAL))
      return type;

    // By default coerce to Number
    return Types.NUMBER.withNullability(type.isNullable());
  };

  /**
   * Type-inference strategy for concatenation.
   * For example,
   *
   * <p>
   * concat(cast('a' as string(2)), cast('b' as string(3)),cast('c' as string(2)))
   * returns string(7).
   */
  public static final IReturnTypeInference CONCAT_FUNCTION = call -> {
    TypeId id = TypeId.STRING;
    int precision = 0;

    for (IExpression operand : call.getOperands()) {
      Type type = operand.getType();
      precision += type.getPrecision();

      if (type.is(TypeId.BINARY))
        id = TypeId.BINARY;
    }

    if (precision > id.getMaxPrecision()) {
      precision = id.getMaxPrecision();
    }
    if (id == TypeId.BINARY) {
      return BinaryType.of(precision);
    }

    return StringType.of(precision);
  };

  /**
   * Type-inference strategy for concatenation with separator.
   */
  public static final IReturnTypeInference CONCATWS_FUNCTION = call -> {
    Type separatorType = call.getOperand(0).getType();
    TypeId id = separatorType.getId();
    int precision = 0;

    if (separatorType.getPrecision() != -1) {
      for (int i = 1; i < call.getOperandCount(); i++) {
        Type type = call.getOperand(i).getType();


        // Add separator's precision except when encountering null value or for the last argument
        if (type.getPrecision() > 0) {
          precision += type.getPrecision();

          if (i < call.getOperandCount() - 1) {
            precision += separatorType.getPrecision();
          }
        }

        if (type.is(TypeId.BINARY)) {
          id = TypeId.BINARY;
        }
      }
    } else
      precision = id.getMaxPrecision();

    if (precision > id.getMaxPrecision()) {
      precision = id.getMaxPrecision();
    }
    if (id == TypeId.BINARY) {
      return BinaryType.of(precision);
    }

    return StringType.of(precision);
  };
  
  public static final IReturnTypeInference ADDITIVE_OPERATOR = call -> Types.deriveAdditiveType(call.getOperand(0).getType(), call.getOperand(1).getType());  
  public static final IReturnTypeInference MULTIPLY_OPERATOR = call -> Types.deriveMultiplyType(call.getOperand(0).getType(), call.getOperand(1).getType());
  public static final IReturnTypeInference DIVIDE_OPERATOR = call -> Types.deriveDivideType(call.getOperand(0).getType(), call.getOperand(1).getType());
  public static final IReturnTypeInference MOD_OPERATOR = call -> Types.deriveModType(call.getOperand(0).getType(), call.getOperand(1).getType());

  public static final IReturnTypeInference CASE_OPERATOR = new CaseOperatorReturnTypeInference();

  /**
   * Type-inference strategy for CAST operator.
   */
  public static final IReturnTypeInference CAST_OPERATOR = call -> {
    try {
      return call.getOperand(1).getValue(Type.class);
    } catch (Exception e) {
      return Types.UNKNOWN;
    }
  };

  /**
   * Type-inference strategy for IF function.
   */
  public static final IReturnTypeInference IF_FUNCTION = call -> {
    if (call.getOperandCount() < 2)
      return Types.UNKNOWN;

    ITypeTransform transform = TypeTransforms.TO_MAX_PRECISION;

    Type type1 = call.getOperand(1).getType();
    if (call.getOperandCount() == 2) {
      return transform.transformType(type1);
    }
    Type type2 = call.getOperand(2).getType();
    if (type1.getId().ordinal() < type2.getId().ordinal()) {
      return transform.transformType(type2);
    }

    return transform.transformType(type1);
  };

 
}
