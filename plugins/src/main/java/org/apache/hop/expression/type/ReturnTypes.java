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
  public static final IReturnTypeInference BOOLEAN_NOT_NULL = explicit(Types.BOOLEAN.withNullability(false));

  /**
   * Type-inference strategy whereby the result type of a call is BINARY.
   */
  public static final IReturnTypeInference BINARY_NULLABLE = explicit(Types.BINARY);

  /**
   * Type-inference strategy whereby the result type of a call is BINARY NOT NULL.
   */
  public static final IReturnTypeInference BINARY_NOT_NULL = explicit(Types.BINARY.withNullability(false));
  
  /**
   * Type-inference strategy whereby the result type of a call is STRING.
   */
  public static final IReturnTypeInference STRING_NULLABLE = explicit(Types.STRING);

  /**
   * Type-inference strategy whereby the result type of a call is STRING NOT NULL.
   */
  public static final IReturnTypeInference STRING_NOT_NULL = explicit(Types.STRING.withNullability(false));

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
  public static final IReturnTypeInference NUMBER_NOT_NULL = explicit(Types.NUMBER.withNullability(false));

  /**
   * Type-inference strategy whereby the result type of a call is DATE.
   */
  public static final IReturnTypeInference DATE_NULLABLE = explicit(Types.DATE);

  /**
   * Type-inference strategy whereby the result type of a call is DATE NOT NULL.
   */
  public static final IReturnTypeInference DATE_NOT_NULL = explicit(Types.DATE.withNullability(false));

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
   * TODO: Type-inference strategy whereby the result type of a call is {@link #NUMBER_SCALE0} with
   * a fallback to {@link #ARG0}.
   * This rule is used for floor, ceiling.
   */
  public static final IReturnTypeInference ARG0_OR_EXACT_NO_SCALE = call -> {
    Type type = call.getOperand(0).getType();

    if (type.getScale() > 0) {
      return NumberType.of(type.getPrecision(), 0, type.isNullable());
    }

    return Types.INTEGER;
  };

  /**
   * Type-inference strategy for ABS function.
   */
  public static final IReturnTypeInference ABS_FUNCTION = call -> {
    Type type = call.getOperand(0).getType();

    if (type.is(TypeId.INTEGER) || type.is(TypeId.INTERVAL))
      return type;

    // By default coerce to Number
    return Types.NUMBER;
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

  public static final IReturnTypeInference ADDITIVE_OPERATOR = call -> deriveAdditiveType(call.getOperand(0).getType(), call.getOperand(1).getType());  
  public static final IReturnTypeInference MULTIPLY_OPERATOR = call -> deriveMultiplyType(call.getOperand(0).getType(), call.getOperand(1).getType());
  public static final IReturnTypeInference DIVIDE_OPERATOR = call -> deriveDivideType(call.getOperand(0).getType(), call.getOperand(1).getType());
  public static final IReturnTypeInference MOD_OPERATOR = call -> deriveModType(call.getOperand(0).getType(), call.getOperand(1).getType());

  public static final IReturnTypeInference CASE_OPERATOR = new CaseOperatorReturnTypeInference();

  /**
   * Type-inference strategy for CAST operator.
   */
  public static final IReturnTypeInference CAST_OPERATOR = call -> {
    try {
      return call.getOperand(1).getValue(Type.class);
    } catch (Exception e) {
      return UnknownType.UNKNOWN;
    }
  };

  /**
   * Type-inference strategy for IF function.
   */
  public static final IReturnTypeInference IF_FUNCTION = call -> {
    if (call.getOperandCount() < 2)
      return UnknownType.UNKNOWN;

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

  /**
   * Calculate return type precision and scale for x + y and x - y
   * 
   * <ul>
   * <li>Let p1, s1 be the precision and scale of the first operand</li>
   * <li>Let p2, s2 be the precision and scale of the second operand</li>
   * <li>Let p, s be the precision and scale of the result</li>
   * <li>Let d be the number of whole digits in the result</li>
   * <li>Then the result type is a decimal with:
   * <ul>
   * <li>s = max(s1, s2)</li>
   * <li>p = max(p1 - s1, p2 - s2) + s + 1</li>
   * </ul>
   * </li>
   * <li>p and s are capped at their maximum values</li>
   * </ul>
   * 
   */
  protected static Type deriveAdditiveType(final Type type1, final Type type2) {
    int p1 = type1.getPrecision();
    int p2 = type2.getPrecision();
    int s1 = type1.getScale();
    int s2 = type2.getScale();

    if (type1.is(TypeId.DATE) && type2.is(TypeId.INTERVAL)) {
      return type1;
    }
    if (type2.is(TypeId.DATE) && type1.is(TypeId.INTERVAL)) {
      return type2;
    }

    // Return type scale
    int s = Math.max(s1, s2);
    s = Math.min(s, TypeId.NUMBER.getMaxScale());
    
    // Return type precision
    int p = Math.max(p1 - s1, p2 - s2) + s + 1;
    p = Math.min(p, TypeId.NUMBER.getMaxPrecision());

    Type type = NumberType.of(p, s);

    // Optimize to INTEGER type
    if (!(type1.is(TypeId.NUMBER) || type2.is(TypeId.NUMBER)) && p <= TypeId.INTEGER.getMaxPrecision() && s == 0) {      
      return IntegerType.of(p);
    }
    
    return type;
  }

  /**
   * Calculate return type precision and scale for x * y
   * 
   * <ul>
   * <li>Let p1, s1 be the precision and scale of the first operand</li>
   * <li>Let p2, s2 be the precision and scale of the second operand</li>
   * <li>Let p, s be the precision and scale of the result</li>
   * <li>Let d be the number of whole digits in the result</li>
   * <li>Then the result type is a decimal with:
   * <ul>
   * <li>p = p1 + p2</li>
   * <li>s = s1 + s2</li>
   * </ul>
   * </li>
   * <li>p and s are capped at their maximum values</li>
   * </ul>
   * 
   */
  protected static Type deriveMultiplyType(final Type type1, final Type type2) {
    int p1 = type1.getPrecision();
    int p2 = type2.getPrecision();
    int s1 = type1.getScale();
    int s2 = type2.getScale();

    // Return type precision
    int p = Math.min(TypeId.NUMBER.getMaxPrecision(), p1+p2);
    
    // Return type scale
    int s = Math.min(TypeId.NUMBER.getMaxScale(), s1+s2);
    
    // TODO: Remove when operands are properly casted
    if ( s<0 ) s=0;
    
    return NumberType.of(p, s);
  }

  /**
   * Calculate return type precision and scale for x / y
   * 
   * <ul>
   * <li>Let p1, s1 be the precision and scale of the first operand</li>
   * <li>Let p2, s2 be the precision and scale of the second operand</li>
   * <li>Let p, s be the precision and scale of the result</li>
   * <li>Let d be the number of whole digits in the result</li>
   * <li>Then the result type is a decimal with:
   * <ul>
   * <li>d = p1 - s1 + s2</li>
   * <li>s = max(6, s1 + p2 + 1)</li>
   * <li>p = d + s</li>
   * </ul>
   * </li>
   * <li>p and s are capped at their maximum values</li>
   * </ul>
   */
  protected static Type deriveDivideType(final Type type1, final Type type2) {
    int p1 = type1.getPrecision();
    int p2 = type2.getPrecision();
    int s1 = type1.getScale();
    int s2 = type2.getScale();

    int d = p1 - s1 + s2;

    // Return type scale
    int s = Math.max(6, s1 + p2 + 1);
    s = Math.min(TypeId.NUMBER.getMaxScale(), s);
    
    // Return type precision
    int p = Math.min(TypeId.NUMBER.getMaxPrecision(), d + s);
        
    return NumberType.of(p, s);
  }

  /**
   * Calculate return type precision and scale for x % y
   * 
   * <ul>
   * <li>Let p1, s1 be the precision and scale of the first operand</li>
   * <li>Let p2, s2 be the precision and scale of the second operand</li>
   * <li>Let p, s be the precision and scale of the result</li>
   * <li>Let d be the number of whole digits in the result</li>
   * <li>Then the result type is a decimal with:
   * <ul>
   * <li>s = max(s1, s2)</li>
   * <li>p = min(p1 - s1, p2 - s2) + max(s1, s2)</li>
   * </ul>
   * </li>
   * <li>p and s are capped at their maximum values</li>
   * </ul>
   * 
   */
  protected static Type deriveModType(final Type type1, final Type type2) {
    int p1 = type1.getPrecision();
    int p2 = type2.getPrecision();
    int s1 = type1.getScale();
    int s2 = type2.getScale();

    // Return type scale
    int s = Math.max(s1, s2);

    // Return type precision
    int p = Math.min(p1 - s1, p2 - s2) + s;

    p = Math.min(TypeId.NUMBER.getMaxPrecision(), p);

    return NumberType.of(p, s);
  }
}
