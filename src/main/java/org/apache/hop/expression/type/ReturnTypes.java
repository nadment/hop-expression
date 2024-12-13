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

import java.util.ArrayList;
import java.util.List;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.IExpression;

/** A collection of strategies for return type inference. */
public final class ReturnTypes {

  private ReturnTypes() {
    // Utility class
    super();
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

  /** Creates a return-type inference that applies a rule then a sequence of transforms. */
  public static ReturnTypeTransformCascade cascade(
      IReturnTypeInference rule, ITypeTransform... transforms) {
    return new ReturnTypeTransformCascade(rule, transforms);
  }

  public static final IReturnTypeInference ANY = explicit(Types.ANY);

  /** Type-inference strategy whereby the result type of a call is BOOLEAN. */
  public static final IReturnTypeInference BOOLEAN_NULLABLE = explicit(Types.BOOLEAN);

  /** Type-inference strategy whereby the result type of a call is BOOLEAN NOT NULL. */
  public static final IReturnTypeInference BOOLEAN_NOT_NULL = explicit(Types.BOOLEAN_NOT_NULL);

  /** Type-inference strategy whereby the result type of a call is BINARY. */
  public static final IReturnTypeInference BINARY_NULLABLE = explicit(Types.BINARY);

  /** Type-inference strategy whereby the result type of a call is BINARY NOT NULL. */
  public static final IReturnTypeInference BINARY_NOT_NULL = explicit(Types.BINARY_NOT_NULL);

  /** Type-inference strategy whereby the result type of a call is STRING. */
  public static final IReturnTypeInference STRING_NULLABLE = explicit(Types.STRING);

  /** Type-inference strategy whereby the result type of a call is STRING NOT NULL. */
  public static final IReturnTypeInference STRING_NOT_NULL = explicit(Types.STRING_NOT_NULL);

  /** Type-inference strategy whereby the result type of a call is INTEGER. */
  public static final IReturnTypeInference INTEGER_NULLABLE = explicit(Types.INTEGER);

  /** Type-inference strategy whereby the result type of a call is NUMBER. */
  public static final IReturnTypeInference NUMBER_NULLABLE = explicit(Types.NUMBER);

  /** Type-inference strategy whereby the result type of a call is NUMBER NOT NULL. */
  public static final IReturnTypeInference NUMBER_NOT_NULL = explicit(Types.NUMBER_NOT_NULL);

  /** Type-inference strategy whereby the result type of a call is DATE. */
  public static final IReturnTypeInference DATE_NULLABLE = explicit(Types.DATE);

  /** Type-inference strategy whereby the result type of a call is DATE NOT NULL. */
  public static final IReturnTypeInference DATE_NOT_NULL = explicit(Types.DATE_NOT_NULL);

  /** Type-inference strategy whereby the result type of a call is DATE. */
  public static final IReturnTypeInference JSON_NULLABLE = explicit(Types.JSON);

  /** Type-inference strategy whereby the result type of a call is INTERVAL. */
  public static final IReturnTypeInference INTERVAL_NULLABLE = explicit(Types.INTERVAL);

  /** Type-inference strategy whereby the result type of a call is the type of the operand #0. */
  public static final IReturnTypeInference ARG0 = new OrdinalReturnTypeInference(0);

  public static final IReturnTypeInference ARG0_NOT_NULL =
      ARG0.andThen(TypeTransforms.TO_NOT_NULLABLE);

  /** Type-inference strategy whereby the result type of a call is the type of the operand #1. */
  public static final IReturnTypeInference ARG1 = new OrdinalReturnTypeInference(1);

  /** Type-inference strategy whereby the result type of a call is the type of the operand #2. */
  public static final IReturnTypeInference ARG2 = new OrdinalReturnTypeInference(2);

  public static final IReturnTypeInference ARG1_OR_ARG2 =
      chain(new OrdinalReturnTypeInference(1), new OrdinalReturnTypeInference(2));

  public static final IReturnTypeInference FIRST_KNOWN =
      new FirstKnownReturnTypeInference().andThen(TypeTransforms.TO_MAX_PRECISION);

  public static final IReturnTypeInference LEAST_RESTRICTIVE =
      new LeastRestrictiveReturnTypeInference().andThen(TypeTransforms.TO_MAX_PRECISION);

  public static final IReturnTypeInference ARG0_MAX_PRECISION =
      new OrdinalReturnTypeInference(0).andThen(TypeTransforms.TO_MAX_PRECISION);

  /** Type-inference strategy whereby the result type of a call is the element type of the array. */
  public static final IReturnTypeInference ARRAY_ELEMENT =
      call -> {
        Type type = call.getOperand(0).getType();
        if (type.is(TypeName.ARRAY)) {
          ArrayType arrayType = (ArrayType) type;
          return arrayType.getElementType();
        }
        return Types.UNKNOWN;
      };

  public static final IReturnTypeInference ARRAY =
      call -> {
        List<Type> types = new ArrayList<>();
        for (IExpression operand : call.getOperands()) {
          types.add(operand.getType());
        }

        Type type = Types.getLeastRestrictive(types);
        return ArrayType.of(type);
      };

  /**
   * Type-inference strategy whereby the result type of a call is a number with scale 0 with
   * precision-scale and nullity than ARG0.
   *
   * <p>This rule is used for CEILING, FLOOR.
   */
  public static final IReturnTypeInference CEIL_FLOOR_FUNCTION =
      call -> {
        Type type = call.getOperand(0).getType();

        int precision = TypeName.NUMBER.getMaxPrecision();
        if (type.is(TypeName.INTEGER)) {
          precision = type.getPrecision();
        } else if (type.is(TypeName.NUMBER)) {
          precision = type.getPrecision() - type.getScale();
          if (precision == 0) precision = 1;
        }

        return NumberType.of(precision, 0, type.isNullable());
      };

  // -------------------------------------------------------------
  //                   FUNCTIONS
  // -------------------------------------------------------------

  /** Type-inference strategy for ABS function. */
  public static final IReturnTypeInference ABS_FUNCTION =
      call -> {
        Type type = call.getOperand(0).getType();

        // Keep ARG0 type
        if (type.is(TypeName.INTEGER) || type.is(TypeName.INTERVAL)) return type;

        // By default coerce to Number
        return Types.NUMBER.withNullability(type.isNullable());
      };

  /**
   * Type-inference strategy for concatenation. For example,
   *
   * <p>concat(cast('a' as string(2)), cast('b' as string(3)),cast('c' as string(2))) returns
   * string(7).
   */
  public static final IReturnTypeInference CONCAT_FUNCTION =
      call -> {
        TypeName typeId = TypeName.STRING;
        Type elementType = Types.UNKNOWN;
        int precision = 0;

        for (IExpression operand : call.getOperands()) {
          Type type = operand.getType();
          precision += type.getPrecision();
          if (type.is(TypeName.ARRAY)) {
            typeId = TypeName.ARRAY;
            elementType = Types.getLeastRestrictive((Array) operand);
          } else if (type.is(TypeName.BINARY)) {
            typeId = TypeName.BINARY;
          }
        }

        if (typeId == TypeName.ARRAY) {
          return ArrayType.of(elementType);
        }

        if (precision > typeId.getMaxPrecision()) {
          precision = typeId.getMaxPrecision();
        }

        if (typeId == TypeName.BINARY) {
          return BinaryType.of(precision);
        }

        return StringType.of(precision);
      };

  /** Type-inference strategy for concatenation with separator. */
  public static final IReturnTypeInference CONCATWS_FUNCTION =
      call -> {
        Type separatorType = call.getOperand(0).getType();
        TypeName id = separatorType.getName();
        int precision = 0;

        if (separatorType.getPrecision() != -1) {
          for (int i = 1; i < call.getOperandCount(); i++) {
            Type type = call.getOperand(i).getType();

            // Add separator's precision except when encountering null value or for the last
            // argument
            if (type.getPrecision() > 0) {
              precision += type.getPrecision();

              if (i < call.getOperandCount() - 1) {
                precision += separatorType.getPrecision();
              }
            }

            if (type.is(TypeName.BINARY)) {
              id = TypeName.BINARY;
            }
          }
        } else precision = id.getMaxPrecision();

        if (precision > id.getMaxPrecision()) {
          precision = id.getMaxPrecision();
        }
        if (id == TypeName.BINARY) {
          return BinaryType.of(precision);
        }

        return StringType.of(precision);
      };

  public static final IReturnTypeInference ADDITIVE_OPERATOR =
      call -> deriveAdditiveType(call.getOperand(0).getType(), call.getOperand(1).getType());
  public static final IReturnTypeInference MULTIPLY_OPERATOR =
      call -> deriveMultiplyType(call.getOperand(0).getType(), call.getOperand(1).getType());
  public static final IReturnTypeInference DIVIDE_OPERATOR =
      call -> deriveDivideType(call.getOperand(0).getType(), call.getOperand(1).getType());
  public static final IReturnTypeInference MOD_OPERATOR =
      call -> deriveModType(call.getOperand(0).getType(), call.getOperand(1).getType());

  public static final IReturnTypeInference CASE_OPERATOR = new CaseOperatorReturnTypeInference();

  /** Type-inference strategy for CAST operator. */
  public static final IReturnTypeInference CAST_OPERATOR =
      call -> {
        try {
          return call.getOperand(1).getValue(Type.class);
        } catch (Exception e) {
          return Types.UNKNOWN;
        }
      };

  /** Type-inference strategy for IF function. */
  public static final IReturnTypeInference IF_FUNCTION =
      call -> {
        if (call.getOperandCount() < 2) return Types.UNKNOWN;

        ITypeTransform transform = TypeTransforms.TO_MAX_PRECISION;

        Type type1 = call.getOperand(1).getType();
        if (call.getOperandCount() == 2) {
          return transform.transformType(type1);
        }
        Type type2 = call.getOperand(2).getType();
        if (type1.getName().ordinal() < type2.getName().ordinal()) {
          return transform.transformType(type2);
        }

        return transform.transformType(type1);
      };

  /**
   * Calculate return type precision and scale for x + y and x - y
   *
   * <ul>
   *   <li>Let p1, s1 be the precision and scale of the first operand
   *   <li>Let p2, s2 be the precision and scale of the second operand
   *   <li>Let p, s be the precision and scale of the result
   *   <li>Let d be the number of whole digits in the result
   *   <li>Then the result type is a decimal with:
   *       <ul>
   *         <li>s = max(s1, s2)
   *         <li>p = max(p1 - s1, p2 - s2) + s + 1
   *       </ul>
   *   <li>p and s are capped at their maximum values
   * </ul>
   */
  protected static Type deriveAdditiveType(final Type type1, final Type type2) {

    if (type1.is(TypeName.DATE) && type2.is(TypeName.INTERVAL)) {
      return type1;
    }
    if (type1.is(TypeName.INTERVAL) && type2.is(TypeName.DATE)) {
      return type2;
    }
    if (type1.is(TypeName.INTERVAL) && type2.is(TypeName.INTERVAL)) {
      return type1;
    }
    if (type1.is(TypeName.STRING) || type2.is(TypeName.STRING)) {
      return Types.NUMBER;
    }

    int p1 = type1.getPrecision();
    int p2 = type2.getPrecision();

    // Preserve integer type and adjust precision
    if ((type1.is(TypeName.INTEGER) || type1.is(TypeName.BOOLEAN))
        && (type2.is(TypeName.INTEGER) || type2.is(TypeName.BOOLEAN))) {
      int p = Math.min(TypeName.INTEGER.getMaxPrecision(), Math.max(p1, p2) + 1);
      return IntegerType.of(p);
    }

    int s1 = type1.getScale();
    int s2 = type2.getScale();

    // Return type scale
    int s = Math.max(s1, s2);
    s = Math.min(s, TypeName.NUMBER.getMaxScale());

    // Return type precision
    int p = Math.max(p1 - s1, p2 - s2) + s + 1;
    p = Math.min(p, TypeName.NUMBER.getMaxPrecision());

    return NumberType.of(p, s);
  }

  /**
   * Calculate return type precision and scale for x * y
   *
   * <ul>
   *   <li>Let p1, s1 be the precision and scale of the first operand
   *   <li>Let p2, s2 be the precision and scale of the second operand
   *   <li>Let p, s be the precision and scale of the result
   *   <li>Let d be the number of whole digits in the result
   *   <li>Then the result type is a decimal with:
   *       <ul>
   *         <li>p = p1 + p2
   *         <li>s = s1 + s2
   *       </ul>
   *   <li>p and s are capped at their maximum values
   * </ul>
   */
  protected static Type deriveMultiplyType(final Type type1, final Type type2) {
    if (Types.isString(type1) || Types.isString(type2)) {
      return Types.NUMBER;
    }

    int p1 = type1.getPrecision();
    int p2 = type2.getPrecision();

    // Preserve integer type and adjust precision
    if (type1.is(TypeName.INTEGER) && type2.is(TypeName.INTEGER)) {
      int p = Math.min(TypeName.INTEGER.getMaxPrecision(), p1 + p2);
      return IntegerType.of(p);
    }

    // Return type precision
    int p = Math.min(TypeName.NUMBER.getMaxPrecision(), p1 + p2);

    // Return type scale
    int s1 = type1.getScale();
    int s2 = type2.getScale();
    int s = Math.min(TypeName.NUMBER.getMaxScale(), s1 + s2);

    return NumberType.of(p, s);
  }

  /**
   * Calculate return type precision and scale for x / y
   *
   * <ul>
   *   <li>Let p1, s1 be the precision and scale of the first operand
   *   <li>Let p2, s2 be the precision and scale of the second operand
   *   <li>Let p, s be the precision and scale of the result
   *   <li>Let d be the number of whole digits in the result
   *   <li>Then the result type is a decimal with:
   *       <ul>
   *         <li>d = p1 - s1 + s2
   *         <li>s = max(6, s1 + p2 + 1)
   *         <li>p = d + s
   *       </ul>
   *   <li>p and s are capped at their maximum values
   * </ul>
   */
  protected static Type deriveDivideType(final Type type1, final Type type2) {

    if (Types.isString(type1) || Types.isString(type2)) {
      return Types.NUMBER;
    }

    int p1 = type1.getPrecision();
    int p2 = type2.getPrecision();
    int s1 = type1.getScale();
    int s2 = type2.getScale();

    int d = p1 - s1 + s2;

    // Return type scale
    int s = Math.max(6, s1 + p2 + 1);
    s = Math.min(TypeName.NUMBER.getMaxScale(), s);

    // Return type precision
    int p = Math.min(TypeName.NUMBER.getMaxPrecision(), d + s);

    return NumberType.of(p, s);
  }

  /**
   * Calculate return type precision and scale for x % y
   *
   * <ul>
   *   <li>Let p1, s1 be the precision and scale of the first operand
   *   <li>Let p2, s2 be the precision and scale of the second operand
   *   <li>Let p, s be the precision and scale of the result
   *   <li>Let d be the number of whole digits in the result
   *   <li>Then the result type is a decimal with:
   *       <ul>
   *         <li>s = max(s1, s2)
   *         <li>p = min(p1 - s1, p2 - s2) + max(s1, s2)
   *       </ul>
   *   <li>p and s are capped at their maximum values
   * </ul>
   */
  protected static Type deriveModType(final Type type1, final Type type2) {

    if (Types.isString(type1) || Types.isString(type2)) {
      return Types.NUMBER;
    }

    int p1 = type1.getPrecision();
    int p2 = type2.getPrecision();
    int s1 = type1.getScale();
    int s2 = type2.getScale();

    // Return type scale
    int s = Math.max(s1, s2);

    // Return type precision
    int p = Math.min(p1 - s1, p2 - s2) + s;

    p = Math.min(TypeName.NUMBER.getMaxPrecision(), p);

    return NumberType.of(p, s);
  }
}
