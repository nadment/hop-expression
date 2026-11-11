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
import org.apache.hop.expression.Lambda;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * A collection of reusable strategies to infer the return type of an operator call.
 *
 * <p>The {@code *_NULLABLE} strategies return a type that is nullable if at least one operand of
 * the call is nullable, the {@code *_NOT_NULL} strategies a type that is never nullable.
 */
@NullMarked
public final class ReturnTypes {

  /** Type-inference strategy whereby the result type of the call is not known in advance. */
  public static final IReturnTypeInference ANY = explicit(AnyType.ANY);

  /** Type-inference strategy whereby the result type of the call is BOOLEAN. */
  public static final IReturnTypeInference BOOLEAN_NULLABLE =
      explicit(BooleanType.BOOLEAN).andThen(TypeTransforms.TO_NULLABLE);

  /** Type-inference strategy whereby the result type of the call is BOOLEAN NOT NULL. */
  public static final IReturnTypeInference BOOLEAN_NOT_NULL =
      explicit(BooleanType.BOOLEAN_NOT_NULL);

  /** Type-inference strategy whereby the result type of the call is BINARY. */
  public static final IReturnTypeInference BINARY_NULLABLE =
      explicit(BinaryType.BINARY).andThen(TypeTransforms.TO_NULLABLE);

  /** Type-inference strategy whereby the result type of the call is BINARY NOT NULL. */
  public static final IReturnTypeInference BINARY_NOT_NULL = explicit(BinaryType.BINARY_NOT_NULL);

  /** Type-inference strategy whereby the result type of the call is STRING. */
  public static final IReturnTypeInference STRING_NULLABLE =
      explicit(StringType.STRING).andThen(TypeTransforms.TO_NULLABLE);

  /** Type-inference strategy whereby the result type of the call is STRING NOT NULL. */
  public static final IReturnTypeInference STRING_NOT_NULL = explicit(StringType.STRING_NOT_NULL);

  /** Type-inference strategy whereby the result type of the call is INTEGER. */
  public static final IReturnTypeInference INTEGER_NULLABLE =
      explicit(IntegerType.INTEGER).andThen(TypeTransforms.TO_NULLABLE);

  /** Type-inference strategy whereby the result type of the call is INTEGER NOT NULL. */
  public static final IReturnTypeInference INTEGER_NOT_NULL =
      explicit(IntegerType.INTEGER_NOT_NULL);

  /** Type-inference strategy whereby the result type of the call is NUMBER. */
  public static final IReturnTypeInference NUMBER_NULLABLE =
      explicit(NumberType.NUMBER).andThen(TypeTransforms.TO_NULLABLE);

  /** Type-inference strategy whereby the result type of the call is NUMBER NOT NULL. */
  public static final IReturnTypeInference NUMBER_NOT_NULL = explicit(NumberType.NUMBER_NOT_NULL);

  /** Type-inference strategy whereby the result type of the call is DATE. */
  public static final IReturnTypeInference DATE_NULLABLE =
      explicit(DateType.DATE).andThen(TypeTransforms.TO_NULLABLE);

  /** Type-inference strategy whereby the result type of the call is DATE NOT NULL. */
  public static final IReturnTypeInference DATE_NOT_NULL = explicit(DateType.DATE_NOT_NULL);

  /** Type-inference strategy whereby the result type of the call is TIMESTAMP NOT NULL. */
  public static final IReturnTypeInference TIMESTAMP_NOT_NULL =
      explicit(TimestampType.TIMESTAMP_NOT_NULL);

  /**
   * Type-inference strategy whereby the result type of the call is TIMESTAMP, nullable if an
   * operand is nullable.
   */
  public static final IReturnTypeInference TIMESTAMP_NULLABLE =
      explicit(TimestampType.TIMESTAMP).andThen(TypeTransforms.TO_NULLABLE);

  /**
   * Type-inference strategy of a function that adjusts a date: the result type is TIMESTAMP if an
   * operand is a TIMESTAMP, otherwise DATE, nullable if an operand is nullable. The function adds
   * whole units or truncates, it keeps the precision of the fractional seconds of the date.
   */
  public static final IReturnTypeInference TEMPORAL_NULLABLE =
      ((IReturnTypeInference)
              call -> {
                Type result = null;
                for (IExpression operand : call.getOperands()) {
                  if (Types.isTemporal(operand.getType())) {
                    result = Types.getLeastRestrictive(result, operand.getType());
                  }
                }
                return (result == null) ? DateType.DATE : result;
              })
          .andThen(TypeTransforms.TO_NULLABLE);

  /**
   * Type-inference strategy of a function that adds nanoseconds to a date, like {@link
   * #TEMPORAL_NULLABLE} but the fractional seconds have 9 digits, the added nanoseconds can have
   * more digits than the date.
   */
  public static final IReturnTypeInference TEMPORAL_NANOSECONDS_NULLABLE =
      TEMPORAL_NULLABLE.andThen(
          (call, type) ->
              Types.withFractionalPrecision(
                  type, TypeName.TIMESTAMP.getMaxPrecision(), type.isNullable()));

  /** Type-inference strategy whereby the result type of the call is JSON. */
  public static final IReturnTypeInference JSON_NULLABLE =
      explicit(JsonType.JSON).andThen(TypeTransforms.TO_NULLABLE);

  /** Type-inference strategy whereby the result type of the call is INTERVAL. */
  public static final IReturnTypeInference INTERVAL_NULLABLE =
      explicit(IntervalType.INTERVAL).andThen(TypeTransforms.TO_NULLABLE);

  /** Type-inference strategy whereby the result type of the call is the type of the operand #0. */
  public static final IReturnTypeInference ARG0 = new OrdinalReturnTypeInference(0);

  /**
   * Type-inference strategy of a string function that also accepts a binary: the result type of the
   * call is the type of the operand #0 if it is a STRING or a BINARY, otherwise STRING with the
   * nullability of the operand #0, as any other operand is coerced to a string.
   */
  public static final IReturnTypeInference ARG0_OR_STRING =
      call -> {
        Type type = call.getOperand(0).getType();
        if (Types.isString(type) || Types.isBinary(type)) {
          return type;
        }
        return StringType.STRING.withNullability(type.isNullable());
      };

  /** Type-inference strategy whereby the result type of the call is the type of the operand #1. */
  public static final IReturnTypeInference ARG1 = new OrdinalReturnTypeInference(1);

  /** Type-inference strategy whereby the result type of the call is the type of the operand #2. */
  public static final IReturnTypeInference ARG2 = new OrdinalReturnTypeInference(2);

  /**
   * Type-inference strategy whereby the result type of the call is the most general type of the
   * operands with its maximum precision, not nullable as soon as one operand is not nullable.
   *
   * <p>This rule is used for functions that return the first non-null operand, like COALESCE.
   */
  public static final IReturnTypeInference LEAST_RESTRICTIVE =
      new LeastRestrictiveReturnTypeInference()
          .andThen(TypeTransforms.TO_MAX_PRECISION)
          .andThen(TypeTransforms.LEAST_NULLABLE);

  /** Type-inference strategy whereby the result type of the call is an array of STRING. */
  public static final IReturnTypeInference ARRAY_OF_STRING =
      explicit(ArrayType.of(StringType.STRING));

  /**
   * Type-inference strategy whereby the result type of the call is the element type of the array.
   */
  public static final IReturnTypeInference ARRAY_ELEMENT =
      call -> {
        Type type = call.getOperand(0).getType();
        if (type instanceof ArrayType arrayType) {
          // The element is null if the array or the index is null, an array constructor is never
          // null
          Type elementType = arrayType.getElementType();
          boolean nullable =
              elementType.isNullable()
                  || (type.isNullable() && !(call.getOperand(0) instanceof Array))
                  || call.getOperand(1).getType().isNullable();
          return elementType.withNullability(nullable);
        }
        return UnknownType.UNKNOWN;
      };

  /**
   * Type-inference strategy whereby the result type of the call is an array of the most general
   * type of the operands, never NULL.
   */
  public static final IReturnTypeInference ARRAY =
      call -> {
        List<Type> types = new ArrayList<>();
        for (IExpression operand : call.getOperands()) {
          types.add(operand.getType());
        }

        Type type = Types.getLeastRestrictive(types);
        // An array constructor is never NULL
        return ArrayType.of(type, false);
      };

  /**
   * Type-inference strategy whereby the result type of the call is an array of the most general
   * type of the elements of the array, the first operand, and the appended element, the second
   * operand.
   */
  public static final IReturnTypeInference ARRAY_APPEND = arrayWithElement(0, 1);

  /**
   * Type-inference strategy whereby the result type of the call is an array of the most general
   * type of the prepended element, the first operand, and the elements of the array, the second
   * operand.
   */
  public static final IReturnTypeInference ARRAY_PREPEND = arrayWithElement(1, 0);

  /**
   * Type-inference strategy whereby the result type of the call is an array of the most general
   * type of the elements of the arrays, nullable only if all the arrays are nullable.
   */
  public static final IReturnTypeInference ARRAY_CONCAT =
      call -> {
        Type elementType = null;
        boolean nullable = true;
        for (IExpression operand : call.getOperands()) {
          Type type = operand.getType();
          // A NULL literal is not an array
          if (type.getElementType() != null) {
            elementType = Types.getLeastRestrictive(elementType, type.getElementType());
          }
          if (!type.isNullable()) {
            nullable = false;
          }
        }
        return ArrayType.of(elementType == null ? UnknownType.UNKNOWN : elementType)
            .withNullability(nullable);
      };

  /**
   * Type-inference strategy whereby the result type of the call is the type of the source array,
   * the first operand, which is not an array if it is a NULL literal. The result is NULL if the
   * array or a bound of the slice is NULL.
   */
  public static final IReturnTypeInference ARRAY_SLICE =
      ((IReturnTypeInference)
              call ->
                  call.getOperand(0).getType() instanceof ArrayType type ? type : ArrayType.ARRAY)
          .andThen(TypeTransforms.TO_NULLABLE);

  /**
   * Type-inference strategy whereby the result type of the call is the type of the array, the first
   * operand. The result is NULL if the array or the sort order is NULL, a comparator lambda never
   * makes the result NULL.
   */
  public static final IReturnTypeInference ARRAY_SORT =
      call -> {
        Type type = call.getOperand(0).getType();
        if (call.getOperandCount() == 2
            && !(call.getOperand(1) instanceof Lambda)
            && call.getOperand(1).getType().isNullable()) {
          return type.withNullability(true);
        }
        return type;
      };

  private static IReturnTypeInference arrayWithElement(int arrayOrdinal, int elementOrdinal) {
    // The result is NULL only if the array is NULL
    return call ->
        ArrayType.of(
            Types.getLeastRestrictive(
                call.getOperand(arrayOrdinal).getType().getElementType(),
                call.getOperand(elementOrdinal).getType()),
            call.getOperand(arrayOrdinal).getType().isNullable());
  }

  /**
   * Type-inference strategy whereby the result type of the call is a number with scale 0, with the
   * number of whole digits of the operand #0, plus one if it has a fraction, and its nullability.
   *
   * <p>This rule is used for CEILING, FLOOR.
   */
  public static final IReturnTypeInference CEIL_FLOOR_FUNCTION =
      call -> {
        Type type = call.getOperand(0).getType();

        int precision = TypeName.NUMBER.getMaxPrecision();
        if (type.isName(TypeName.INTEGER)) {
          precision = type.getPrecision();
        } else if (type.isName(TypeName.NUMBER) && !Types.hasVariableScale(type)) {
          precision = type.getPrecision() - type.getScale();
          // Rounding a fraction can add a digit, CEIL(9.5) → 10, and zero needs a digit
          if (type.getScale() > 0) precision++;
          precision = Math.min(precision, TypeName.NUMBER.getMaxPrecision());
        }

        return NumberType.of(precision, 0, type.isNullable());
      };

  /**
   * Type-inference strategy for ABS function: INTEGER, NUMBER and INTERVAL are preserved, any other
   * type gives NUMBER, with the nullability of the operand #0.
   */
  public static final IReturnTypeInference ABS_FUNCTION =
      call -> {
        Type type = call.getOperand(0).getType();

        // Keep ARG0 type, the absolute value keeps the precision and the scale of a number
        if (type.isName(TypeName.INTEGER)
            || type.isName(TypeName.NUMBER)
            || type.isName(TypeName.INTERVAL)) return type;

        // By default, coerce to Number
        return NumberType.NUMBER.withNullability(type.isNullable());
      };

  /**
   * Returns the maximum length of a value of the type once converted to a string by a
   * concatenation, or {@link Type#PRECISION_NOT_SPECIFIED} if the length is unknown. The precision
   * of a date or an interval is the number of digits of its fractional seconds, not a length.
   */
  private static int getConcatLength(Type type) {
    return switch (type.getName()) {
      case STRING, BINARY -> type.getPrecision();
        // The longest value is FALSE
      case BOOLEAN -> 5;
        // The digits and the sign
      case INTEGER -> type.getPrecision() + 1;
      default -> Type.PRECISION_NOT_SPECIFIED;
    };
  }

  /**
   * Type-inference strategy for concatenation. For example,
   *
   * <p>concat(cast('a' as string(2)), cast('b' as string(3)),cast('c' as string(2))) returns
   * string(7).
   */
  public static final IReturnTypeInference CONCAT_FUNCTION =
      call -> {
        TypeName name = TypeName.STRING;
        int precision = 0;
        boolean nullable = true;

        for (IExpression operand : call.getOperands()) {
          Type type = operand.getType();
          // A type without length, like a date, has an unknown length once converted
          int length = getConcatLength(type);
          if (length == Type.PRECISION_NOT_SPECIFIED) {
            precision = Integer.MAX_VALUE;
          } else if (precision != Integer.MAX_VALUE) {
            precision += length;
          }
          if (type.isName(TypeName.BINARY)) {
            name = TypeName.BINARY;
          }
          if (!type.isNullable()) {
            nullable = false;
          }
        }

        if (precision > name.getMaxPrecision() || precision < 0) {
          precision = name.getMaxPrecision();
        }

        if (name == TypeName.BINARY) {
          return BinaryType.of(precision, nullable);
        }

        return StringType.of(precision, nullable);
      };

  /**
   * Type-inference strategy for concatenation with separator, where the operand #0 is the
   * separator. The precision is the sum of the precisions of the values and of the separators
   * between them.
   */
  public static final IReturnTypeInference CONCATWS_FUNCTION =
      call -> {
        Type separatorType = call.getOperand(0).getType();
        TypeName typeName = separatorType.getName();
        int precision = 0;
        boolean nullable = separatorType.isNullable();
        boolean unknown = separatorType.getPrecision() == Type.PRECISION_NOT_SPECIFIED;

        for (int i = 1; i < call.getOperandCount(); i++) {
          Type type = call.getOperand(i).getType();

          // A type without length, like a date, has an unknown length once converted
          int length = getConcatLength(type);
          if (length == Type.PRECISION_NOT_SPECIFIED) {
            unknown = true;
          }

          // Add separator's precision except when encountering null value or for the last
          // argument
          if (length > 0 && !unknown) {
            precision += length;

            if (i < call.getOperandCount() - 1) {
              precision += separatorType.getPrecision();
            }
          }

          if (type.isName(TypeName.BINARY)) {
            typeName = TypeName.BINARY;
          }

          if (type.isNullable()) {
            nullable = true;
          }
        }

        if (unknown || precision > typeName.getMaxPrecision()) {
          precision = typeName.getMaxPrecision();
        }
        if (typeName == TypeName.BINARY) {
          return BinaryType.of(precision, nullable);
        }

        return StringType.of(precision, nullable);
      };

  // -------------------------------------------------------------
  //                   OPERATORS
  // -------------------------------------------------------------

  /** Type-inference strategy for the add and subtract operators. */
  public static final IReturnTypeInference ADDITIVE_OPERATOR =
      call -> deriveAdditiveType(call.getOperand(0).getType(), call.getOperand(1).getType());

  /** Type-inference strategy for the multiply operator. */
  public static final IReturnTypeInference MULTIPLY_OPERATOR =
      call -> deriveMultiplyType(call.getOperand(0).getType(), call.getOperand(1).getType());

  /** Type-inference strategy for the divide operator. */
  public static final IReturnTypeInference DIVIDE_OPERATOR =
      call -> deriveDivideType(call.getOperand(0).getType(), call.getOperand(1).getType());

  /** Type-inference strategy for the modulo operator. */
  public static final IReturnTypeInference MOD_OPERATOR =
      call -> deriveModType(call.getOperand(0).getType(), call.getOperand(1).getType());

  /** Type-inference strategy for the CASE operator. */
  public static final IReturnTypeInference CASE_OPERATOR = new CaseOperatorReturnTypeInference();

  /**
   * Type-inference strategy for CAST operator: the target type given by the operand #1, with the
   * nullability of the operand #0.
   */
  public static final IReturnTypeInference CAST_OPERATOR =
      call -> {
        try {
          return call.getOperand(1)
              .getType()
              .withNullability(call.getOperand(0).getType().isNullable());
        } catch (Exception e) {
          return UnknownType.UNKNOWN;
        }
      };

  // -------------------------------------------------------------
  //                   FUNCTIONS
  // -------------------------------------------------------------

  /**
   * Type-inference strategy for IF function: the least restrictive type of the operands #1 and #2,
   * like the CASE operator, nullable if one of them is nullable or if the operand #2 is omitted.
   */
  public static final IReturnTypeInference IF_FUNCTION =
      call -> {
        if (call.getOperandCount() < 2) return UnknownType.UNKNOWN;

        Type type1 = call.getOperand(1).getType();
        if (call.getOperandCount() == 2) {
          // Without else value, the function returns NULL when the condition is false
          return type1.withNullability(true);
        }
        Type type2 = call.getOperand(2).getType();
        boolean nullable = type1.isNullable() || type2.isNullable();
        return Types.getLeastRestrictive(type1, type2).withNullability(nullable);
      };

  /**
   * Type-inference strategy for NVL2 function: the most general type of the operands #1 and #2,
   * nullable if one of the three operands is nullable.
   */
  public static final IReturnTypeInference NVL2_FUNCTION =
      cascade(
          call ->
              Types.getLeastRestrictive(call.getOperand(1).getType(), call.getOperand(2).getType()),
          TypeTransforms.TO_NULLABLE);

  /**
   * Type-inference strategy for JSON_VALUE function: the type given by the operand #2, STRING by
   * default.
   */
  public static final IReturnTypeInference JSON_VALUE =
      call -> {
        try {
          if (call.getOperandCount() == 3) {
            return call.getOperand(2).getType();
          }

          // By default return STRING
          return StringType.STRING;
        } catch (Exception e) {
          return UnknownType.UNKNOWN;
        }
      };

  private ReturnTypes() {
    // Utility class
    super();
  }

  /**
   * Creates a return-type inference rule which always returns the given type, whatever the call.
   *
   * @param type the type to return
   * @return the rule
   */
  public static ExplicitReturnTypeInference explicit(Type type) {
    return new ExplicitReturnTypeInference(type);
  }

  /**
   * Creates a return-type inference that applies a sequence of rules, returning the first result
   * that is not {@link UnknownType#UNKNOWN}.
   *
   * @param rules the rules to apply, in order
   * @return the rule
   */
  public static ReturnTypeInferenceChain chain(IReturnTypeInference... rules) {
    return new ReturnTypeInferenceChain(rules);
  }

  /**
   * Creates a return-type inference that applies a rule then a sequence of transforms.
   *
   * @param rule the rule that infers the type
   * @param transforms the transforms to apply to the inferred type, in order
   * @return the rule
   */
  public static ReturnTypeTransformCascade cascade(
      IReturnTypeInference rule, ITypeTransform... transforms) {
    return new ReturnTypeTransformCascade(rule, transforms);
  }

  /**
   * Calculate return type precision and scale for x + y and x - y
   *
   * <ul>
   *   <li>Let p1, s1 be the precision and scale of the first operand
   *   <li>Let p2, s2 be the precision and scale of the second operand
   *   <li>Let p, s be the precision and scale of the result
   *   <li>Then the result type is a decimal with:
   *       <ul>
   *         <li>s = max(s1, s2)
   *         <li>p = max(p1 - s1, p2 - s2) + s + 1
   *       </ul>
   *   <li>p and s are capped at their maximum values
   * </ul>
   *
   * <p>A date with an interval gives a date, two intervals an interval, two integers an integer.
   *
   * @param type1 the type of the first operand
   * @param type2 the type of the second operand
   * @return the type of the result, nullable if one of the operands is nullable
   */
  public static Type deriveAdditiveType(final Type type1, final Type type2) {
    Type typeWithNull = deriveTypeWithNull(type1, type2);
    if (typeWithNull != null) {
      return typeWithNull;
    }

    boolean nullable = type1.isNullable() || type2.isNullable();

    // A date keeps its type, with or without time zone, the fractional seconds of the interval can
    // add digits: the result has the greater precision
    if (Types.isTemporal(type1) && type2.isName(TypeName.INTERVAL)) {
      return Types.withGreaterFractionalPrecision(type1, type2, nullable);
    }
    if (type1.isName(TypeName.INTERVAL) && Types.isTemporal(type2)) {
      return Types.withGreaterFractionalPrecision(type2, type1, nullable);
    }
    if (type1.isName(TypeName.INTERVAL) && type2.isName(TypeName.INTERVAL)) {
      return Types.withGreaterFractionalPrecision(type1, type2, nullable);
    }
    if (type1.isName(TypeName.STRING) || type2.isName(TypeName.STRING)) {
      return NumberType.NUMBER.withNullability(nullable);
    }

    int p1 = type1.getPrecision();
    int p2 = type2.getPrecision();

    // Preserve integer type and adjust precision
    if (type1.isName(TypeName.INTEGER) && type2.isName(TypeName.INTEGER)) {
      int p = Math.min(TypeName.INTEGER.getMaxPrecision(), Math.max(p1, p2) + 1);
      return IntegerType.of(p, nullable);
    }

    // A NUMBER without fixed scale gives a NUMBER without fixed scale
    if (Types.hasVariableScale(type1) || Types.hasVariableScale(type2)) {
      return NumberType.NUMBER.withNullability(nullable);
    }

    int s1 = type1.getScale();
    int s2 = type2.getScale();

    // Return type scale
    int s = Math.max(s1, s2);
    s = Math.min(s, TypeName.NUMBER.getMaxScale());

    // Return type precision
    int p = Math.max(p1 - s1, p2 - s2) + s + 1;
    p = Math.min(p, TypeName.NUMBER.getMaxPrecision());

    return NumberType.of(p, s, nullable);
  }

  /**
   * Derives the type of an arithmetic operation with the NULL literal, which has no precision and
   * would give an invalid precision: the result is NULL, with the type of the other operand.
   *
   * @param type1 the type of the first operand
   * @param type2 the type of the second operand
   * @return the type of the other operand made nullable, or {@code null} if no operand is the NULL
   *     literal
   */
  private static @Nullable Type deriveTypeWithNull(final Type type1, final Type type2) {
    if (type1.isName(TypeName.UNKNOWN)) {
      return type2.withNullability(true);
    }
    if (type2.isName(TypeName.UNKNOWN)) {
      return type1.withNullability(true);
    }
    return null;
  }

  /**
   * Calculate return type precision and scale for x * y
   *
   * <ul>
   *   <li>Let p1, s1 be the precision and scale of the first operand
   *   <li>Let p2, s2 be the precision and scale of the second operand
   *   <li>Let p, s be the precision and scale of the result
   *   <li>Then the result type is a decimal with:
   *       <ul>
   *         <li>p = p1 + p2
   *         <li>s = s1 + s2
   *       </ul>
   *   <li>p and s are capped at their maximum values
   * </ul>
   *
   * <p>Two integers give an integer.
   *
   * @param type1 the type of the first operand
   * @param type2 the type of the second operand
   * @return the type of the result, nullable if one of the operands is nullable
   */
  public static Type deriveMultiplyType(final Type type1, final Type type2) {
    Type typeWithNull = deriveTypeWithNull(type1, type2);
    if (typeWithNull != null) {
      return typeWithNull;
    }

    boolean nullable = type1.isNullable() || type2.isNullable();

    if (Types.isString(type1) || Types.isString(type2)) {
      return NumberType.NUMBER.withNullability(nullable);
    }

    int p1 = type1.getPrecision();
    int p2 = type2.getPrecision();

    // Preserve integer type and adjust precision
    if (type1.isName(TypeName.INTEGER) && type2.isName(TypeName.INTEGER)) {
      int p = Math.min(TypeName.INTEGER.getMaxPrecision(), p1 + p2);
      return IntegerType.of(p, nullable);
    }

    // A NUMBER without fixed scale gives a NUMBER without fixed scale
    if (Types.hasVariableScale(type1) || Types.hasVariableScale(type2)) {
      return NumberType.NUMBER.withNullability(nullable);
    }

    // Return type precision
    int p = Math.min(TypeName.NUMBER.getMaxPrecision(), p1 + p2);

    // Return type scale
    int s1 = type1.getScale();
    int s2 = type2.getScale();
    int s = Math.min(TypeName.NUMBER.getMaxScale(), s1 + s2);

    return NumberType.of(p, s, nullable);
  }

  /**
   * Returns the type of x / y: a NUMBER without fixed scale. The quotient is rounded to the
   * significant digits of the calculation, not to a scale derived from the operands, so a type with
   * a fixed scale would not describe the value.
   *
   * @param type1 the type of the first operand
   * @param type2 the type of the second operand
   * @return the type of the result, nullable if one of the operands is nullable
   */
  public static Type deriveDivideType(final Type type1, final Type type2) {
    return NumberType.NUMBER.withNullability(type1.isNullable() || type2.isNullable());
  }

  /**
   * Calculate return type precision and scale for x % y
   *
   * <ul>
   *   <li>Let p1, s1 be the precision and scale of the first operand
   *   <li>Let p2, s2 be the precision and scale of the second operand
   *   <li>Let p, s be the precision and scale of the result
   *   <li>Then the result type is a decimal with:
   *       <ul>
   *         <li>s = max(s1, s2)
   *         <li>p = min(p1 - s1, p2 - s2) + max(s1, s2)
   *       </ul>
   *   <li>p and s are capped at their maximum values
   * </ul>
   *
   * @param type1 the type of the first operand
   * @param type2 the type of the second operand
   * @return the type of the result, nullable if one of the operands is nullable
   */
  public static Type deriveModType(final Type type1, final Type type2) {

    boolean nullable = type1.isNullable() || type2.isNullable();

    // A NUMBER without fixed scale gives a NUMBER without fixed scale
    if (Types.isString(type1)
        || Types.isString(type2)
        || Types.hasVariableScale(type1)
        || Types.hasVariableScale(type2)) {
      return NumberType.NUMBER.withNullability(nullable);
    }

    int p1 = type1.getPrecision();
    int p2 = type2.getPrecision();
    int s1 = type1.getScale();
    int s2 = type2.getScale();

    // Return type scale
    int s = Math.max(s1, s2);

    // Return type precision, at least 1 when an operand has no precision like the NULL literal
    int p = Math.max(1, Math.min(p1 - s1, p2 - s2) + s);

    p = Math.min(TypeName.NUMBER.getMaxPrecision(), p);

    return NumberType.of(p, s, nullable);
  }
}
