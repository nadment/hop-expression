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

package org.apache.hop.expression.type;

import com.fasterxml.jackson.databind.JsonNode;
import java.math.BigDecimal;
import java.net.InetAddress;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.value.ValueMetaBigNumber;
import org.apache.hop.core.row.value.ValueMetaBinary;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaInternetAddress;
import org.apache.hop.core.row.value.ValueMetaJson;
import org.apache.hop.core.row.value.ValueMetaNone;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.operator.CastOperator;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

@NullMarked
public final class Types {

  private Types() {
    // Utility class
  }

  /** Returns the most general of a set of types */
  public static Type getLeastRestrictive(List<Type> types) {

    if (types.isEmpty()) return UnknownType.UNKNOWN;

    Type result = types.getFirst();
    for (int i = 1; i < types.size(); i++) {
      result = getLeastRestrictive(result, types.get(i));
    }
    return result;
  }

  /**
   * Returns the most general type of an expression, for an array this is the most general type of
   * its elements.
   *
   * @param expression the expression
   * @return the type, an array without element is of type ANY
   */
  public static Type getLeastRestrictive(IExpression expression) {

    if (expression instanceof Array array) {
      Type result = getLeastRestrictiveElement(array);
      return result == null ? AnyType.ANY : result;
    }

    return expression.getType();
  }

  /** Returns the most general type of the elements of an array, or null if it has no element. */
  private static @Nullable Type getLeastRestrictiveElement(Array array) {
    Type result = null;
    for (IExpression operand : array) {
      Type type = operand.getType();
      if (operand instanceof Array child) {
        type = getLeastRestrictiveElement(child);
      }
      if (type != null) {
        result = getLeastRestrictive(result, type);
      }
    }
    return result;
  }

  /**
   * Returns the most general of two types.
   *
   * @param type1 the first type, or {@code null} if there is no type yet
   * @param type2 the second type
   * @return the most general type
   */
  public static Type getLeastRestrictive(@Nullable Type type1, Type type2) {
    if (type1 == null) return type2;

    // INTEGER and NUMBER -> NUMBER, whatever the precision
    if (isInteger(type1) && isNumber(type2)) {
      return widenNumber(type2, type1);
    }
    if (isNumber(type1) && isInteger(type2)) {
      return widenNumber(type1, type2);
    }

    // A NUMBER without fixed scale holds the values of any other NUMBER
    if (hasVariableScale(type1) && isNumber(type2)) {
      return type1;
    }
    if (isNumber(type1) && hasVariableScale(type2)) {
      return type2;
    }

    if (type1.getName().ordinal() > type2.getName().ordinal()
        || (type1.isFamily(type2.getFamily()) && type1.getPrecision() > type2.getPrecision())) {
      return type1;
    }
    if (type1.isFamily(type2.getFamily())
        && (type1.getPrecision() > type2.getPrecision()
            || (type1.getPrecision() == type2.getPrecision()
                && type1.getScale() > type2.getScale()))) {
      return type1;
    }

    return type2;
  }

  /**
   * Returns a NUMBER type with enough integer digits to hold the values of an INTEGER type.
   *
   * <p>For example, NUMBER(2,1) and INTEGER(3) give NUMBER(4,1).
   */
  private static Type widenNumber(Type number, Type integer) {
    // A NUMBER without fixed scale already has the maximum precision
    if (hasVariableScale(number)) {
      return number;
    }
    int scale = number.getScale();
    int integerDigits = Math.max(number.getPrecision() - scale, integer.getPrecision());
    int precision = Math.min(integerDigits + scale, TypeName.NUMBER.getMaxPrecision());
    if (precision == number.getPrecision()) {
      return number;
    }
    return NumberType.of(precision, scale, number.isNullable());
  }

  /**
   * Coerce all the operands to a common {@code Type}.
   *
   * @param call the call
   * @param type common type to coerce to
   */
  public static boolean coerceOperandsType(Call call, Type type) {
    boolean coerced = false;
    for (int index = 0; index < call.getOperandCount(); index++) {
      coerced |= coerceOperandType(call, type, index);
    }

    // Inferring the return type
    call.inferReturnType();

    return coerced;
  }

  /**
   * Coerces the numeric operands of a logical operator into a boolean, 0 is FALSE and any other
   * value is TRUE.
   */
  public static boolean coerceOperandsToBoolean(Call call) {
    boolean coerced = false;
    for (int index = 0; index < call.getOperandCount(); index++) {
      IExpression operand = call.getOperand(index);
      if (isNumeric(operand.getType())) {
        call.setOperand(index, cast(operand, BooleanType.BOOLEAN));
        coerced = true;
      }
    }

    if (coerced) {
      call.inferReturnType();
    }

    return coerced;
  }

  /**
   * Coerce the operand at the specified index to target {@code Type}.
   *
   * <p>If the operand is a {@code Array}, coercion of all its elements
   */
  public static boolean coerceOperandType(Call call, Type type, int index) {

    IExpression operand = call.getOperand(index);

    // If the operand is an array, coercion of all its elements
    if (operand.isKind(Kind.ARRAY)) {
      Array array = coerceOperandType((Array) operand, type);
      if (!array.equals(operand)) {
        call.setOperand(index, array);
        return true;
      }

      return false;
    }

    // Check it early.
    if (!needToCast(operand, type)) {
      return false;
    }

    call.setOperand(index, cast(operand, type));

    return true;
  }

  public static Array coerceOperandType(Array array, Type type) {
    List<IExpression> list = new ArrayList<>();
    for (IExpression operand : array) {
      if (operand instanceof Array child) {
        operand = coerceOperandType(child, type);
      } else if (needToCast(operand, type)) {
        operand = cast(operand, type);
      }
      list.add(operand);
    }

    return new Array(new ArrayType(type, type.isNullable()), list);
  }

  public static Call cast(IExpression expression, Type type) {
    Call call = new Call(CastOperator.INSTANCE, expression, Literal.of(type));
    call.inferReturnType();
    return call;
  }

  /** Determines a common type for a comparison operator. */
  public static Type getCommonTypeForComparison(Type type1, Type type2) {

    if (type2.isName(TypeName.UNKNOWN)) return type1;

    // NUMBER compare INTEGER -> NUMBER, with enough integer digits to hold the INTEGER values
    if (isNumber(type1) && isInteger(type2)) {
      return widenNumber(type1, type2);
    }
    // INTEGER compare NUMBER -> NUMBER, with enough integer digits to hold the INTEGER values
    if (isInteger(type1) && isNumber(type2)) {
      return widenNumber(type2, type1);
    }
    // DATE compare STRING -> DATE
    if (isDate(type1) && isString(type2)) {
      return type1;
    }
    // STRING compare DATE -> DATE
    if (isString(type1) && isDate(type2)) {
      return type2;
    }
    // STRING compare numeric -> NUMBER
    if (isString(type1) && isNumeric(type2)) {
      return NumberType.NUMBER;
    }
    // Numeric compare STRING -> NUMBER
    if (isNumeric(type1) && isString(type2)) {
      return NumberType.NUMBER;
    }
    // BOOLEAN compare numeric -> NUMBER
    if (isBoolean(type1) && isNumeric(type2)) {
      return isInteger(type2) ? IntegerType.of(1) : NumberType.of(1);
    }
    // Numeric compare BOOLEAN -> NUMBER
    if (isNumeric(type1) && isBoolean(type2)) {
      return isInteger(type1) ? IntegerType.of(1) : NumberType.of(1);
    }

    return getLeastRestrictive(type1, type2);
  }

  /** Returns whether an IExpression should be cast to a target type. */
  public static boolean needToCast(IExpression expression, Type toType) {
    return expression.getType().getName().ordinal() < toType.getName().ordinal();
  }

  /** Coerces the operands of an addition or subtract expression into a numeric type. */
  public static boolean coercionArithmeticOperator(Call call) {

    Type left = call.getOperand(0).getType();
    Type right = call.getOperand(1).getType();

    if (left.getName() == right.getName()) return false;

    // STRING <operator> numeric -> NUMBER
    if (isString(left) && isNumeric(right)) {
      return coerceOperandsType(call, NumberType.NUMBER);
    }
    // Numeric <operator> STRING -> NUMBER
    if (isNumeric(left) && isString(right)) {
      return coerceOperandsType(call, NumberType.NUMBER);
    }

    // NUMBER <operator> INTEGER -> NUMBER
    //    if (Types.isNumber(left) && Types.isInteger(right)) {
    //      return Types.coerceOperandType(call, NumberType.of(right.getPrecision(), 0), 1);
    //    }
    //    // INTEGER <operator> NUMBER -> NUMBER
    //    if (Types.isInteger(left) && Types.isNumber(right)) {
    //      return Types.coerceOperandType(call, NumberType.of(left.getPrecision(), 0), 0);
    //    }

    return false;
  }

  /** Coerces the operands of a multiplication, division or modulo expression into numeric type. */
  public static boolean coercionMultiplyOperator(Call call) {

    Type left = call.getOperand(0).getType();
    Type right = call.getOperand(1).getType();

    if (left.getName() == right.getName()) return false;

    // STRING <operator> numeric -> NUMBER
    if (isString(left) && isNumeric(right)) {
      return Types.coerceOperandType(call, NumberType.NUMBER, 0);
    }
    // Numeric <operator> STRING -> NUMBER
    if (isNumeric(left) && isString(right)) {
      return Types.coerceOperandType(call, NumberType.NUMBER, 1);
    }

    // NUMBER <operator> INTEGER -> NUMBER
    //    if (Types.isNumber(left) && Types.isInteger(right)) {
    //      return Types.coerceOperandType(call, NumberType.of(right.getPrecision(), 0), 1);
    //    }
    // INTEGER <operator> NUMBER -> NUMBER
    //    if (Types.isInteger(left) && Types.isNumber(right)) {
    //      return Types.coerceOperandType(call, NumberType.of(left.getPrecision(), 0), 0);
    //    }

    return false;
  }

  /** Coerces operands in comparison expressions. */
  public static boolean coercionComparisonOperator(Call call) {

    Type type = call.getOperand(0).getType();
    for (int index = 1; index < call.getOperandCount(); index++) {
      type = getCommonTypeForComparison(type, call.getOperand(index).getType());
    }

    return coerceOperandsType(call, type);
  }

  /**
   * Returns whether the conversion from {@code source} to {@code target} type is a 'loss-less'
   * cast, that is, a cast from which the original value of the field can be certainly recovered.
   *
   * @param source source type
   * @param target target type
   * @return true if the conversion is a loss-less cast
   */
  public static boolean isLosslessCast(Type source, Type target) {

    if (source.getName() != target.getName() || source.getPrecision() > target.getPrecision()) {
      return false;
    }

    // A NUMBER without fixed scale keeps all the decimals, but a fixed scale can truncate them
    if (hasVariableScale(target)) {
      return true;
    }
    if (hasVariableScale(source)) {
      return false;
    }

    return source.getScale() <= target.getScale();
  }

  /**
   * Returns whether two types are comparable. They need to be scalar types of the same family, a
   * numeric is also comparable with a boolean. The result is the same whatever the order of the
   * types.
   *
   * @param type1 First type
   * @param type2 Second type
   * @return Whether types are comparable
   */
  public static boolean isComparable(Type type1, Type type2) {

    final TypeFamily family1 = type1.getFamily();
    final TypeFamily family2 = type2.getFamily();

    // If one of the arguments is of type 'ANY', return true.
    if (family1 == TypeFamily.ANY || family2 == TypeFamily.ANY) {
      return true;
    }

    // The comparison of arrays is not supported
    if (family1 == TypeFamily.ARRAY || family2 == TypeFamily.ARRAY) {
      return false;
    }

    if (family1 == family2) {
      return true;
    }

    // A numeric is comparable with a boolean, like 1 = TRUE
    return (family1 == TypeFamily.NUMERIC && family2 == TypeFamily.BOOLEAN)
        || (family1 == TypeFamily.BOOLEAN && family2 == TypeFamily.NUMERIC);

    // If one of the arguments is of type 'NULL', return true.
    // if (family1 == TypeFamily.NULL
    // || family2 == TypeFamily.NULL) {
    // return true;
    // }
  }

  /**
   * Returns whether two expressions are comparable. In addition to {@link #isComparable(Type,
   * Type)}, a string literal is comparable with any scalar type, it is converted to the type of the
   * other expression like an untyped literal in PostgreSQL, for example {@code FIELD_DATE >=
   * '2020-01-01'}.
   *
   * @param expression1 First expression
   * @param expression2 Second expression
   * @return Whether expressions are comparable
   */
  public static boolean isComparable(IExpression expression1, IExpression expression2) {
    Type type1 = expression1.getType();
    Type type2 = expression2.getType();
    if (isComparable(type1, type2)) {
      return true;
    }

    return (isStringLiteral(expression1) && !type2.isFamily(TypeFamily.ARRAY))
        || (isStringLiteral(expression2) && !type1.isFamily(TypeFamily.ARRAY));
  }

  private static boolean isStringLiteral(IExpression expression) {
    return expression.isKind(Kind.LITERAL) && isString(expression.getType());
  }

  public static boolean isBinary(Type type) {
    return type.isName(TypeName.BINARY);
  }

  public static boolean isBoolean(Type type) {
    return type.isName(TypeName.BOOLEAN);
  }

  /**
   * Returns whether one of the operands of a call is a boolean. The arithmetic operators don't
   * coerce a boolean to a number, like PostgreSQL.
   *
   * @param call the call
   * @return true if one of the operands is a boolean
   */
  public static boolean hasBooleanOperand(Call call) {
    for (IExpression operand : call.getOperands()) {
      if (isBoolean(operand.getType())) {
        return true;
      }
    }
    return false;
  }

  public static boolean isNumeric(Type type) {
    return type.isFamily(TypeFamily.NUMERIC);
  }

  public static boolean isInteger(Type type) {
    return type.isName(TypeName.INTEGER);
  }

  public static boolean isNumber(Type type) {
    return type.isName(TypeName.NUMBER);
  }

  /**
   * Returns whether the type is a NUMBER without fixed scale, declared without precision and scale.
   * A value of this type keeps its decimals.
   */
  public static boolean hasVariableScale(Type type) {
    return isNumber(type) && type.getScale() == Type.SCALE_NOT_SPECIFIED;
  }

  public static boolean isString(Type type) {
    return type.isName(TypeName.STRING);
  }

  public static boolean isDate(Type type) {
    return type.isName(TypeName.DATE);
  }

  /**
   * Determines whether the given {@code Type} is of type INTERVAL.
   *
   * @param type the {@code Type} to be checked
   * @return {@code true} if the type is INTERVAL; {@code false} otherwise
   */
  public static boolean isInterval(Type type) {
    return type.isName(TypeName.INTERVAL);
  }

  /**
   * Infers the type of the given value and returns the corresponding {@code Type}.
   *
   * @param value the value from which the type is inferred.
   * @return the inferred {@code Type} corresponding to the value. Returns {@code null} if the type
   *     cannot be determined or the value's type is unsupported.
   */
  public static @Nullable Type inferTypeFromValue(@Nullable Object value) {
    if (value == null) {
      return StringType.STRING;
    } else if (value instanceof Boolean) {
      return BooleanType.BOOLEAN;
    } else if (value instanceof Long integer) {
      return IntegerType.from(integer);
    } else if (value instanceof BigDecimal number) {
      return NumberType.from(number);
    } else if (value instanceof String string) {
      return StringType.from(string);
    } else if (value instanceof ZonedDateTime) {
      return DateType.DATE;
    } else if (value instanceof Interval) {
      return IntervalType.INTERVAL;
    } else if (value instanceof JsonNode) {
      return JsonType.JSON;
    } else if (value instanceof InetAddress) {
      return InetType.INET;
    } else if (value instanceof byte[] bytes) {
      return BinaryType.from(bytes);
    }
    return null;
  }

  public static IValueMeta createValueMeta(final String valueMetaName, final TypeName typeName) {
    // The callers outside the null-marked code can still pass null
    Objects.requireNonNull(valueMetaName, "Value meta name must not be null");
    Objects.requireNonNull(typeName, "Type name must not be null");

    return switch (typeName) {
      case BOOLEAN -> new ValueMetaBoolean(valueMetaName);
      case INTEGER -> new ValueMetaInteger(valueMetaName, 9, 0);
      case NUMBER -> new ValueMetaBigNumber(valueMetaName, -1, -1);
      case STRING -> new ValueMetaString(valueMetaName, -1, -1);
      case DATE -> new ValueMetaDate(valueMetaName, -1, -1);
      case BINARY -> new ValueMetaBinary(valueMetaName, -1, -1);
      case JSON -> new ValueMetaJson(valueMetaName);
      case INET -> new ValueMetaInternetAddress(valueMetaName);
      default -> new ValueMetaNone(valueMetaName);
    };
  }

  public static IValueMeta createValueMeta(final String valueMetaName, final Type type) {
    // The callers outside the null-marked code can still pass null
    Objects.requireNonNull(valueMetaName, "Value meta name must not be null");
    Objects.requireNonNull(type, "Type must not be null");
    return switch (type.getName()) {
      case BOOLEAN -> new ValueMetaBoolean(valueMetaName);
      case INTEGER -> new ValueMetaInteger(valueMetaName);
      case NUMBER -> new ValueMetaBigNumber(valueMetaName, type.getPrecision(), type.getScale());
      case STRING -> new ValueMetaString(valueMetaName, type.getPrecision(), type.getScale());
      case DATE -> new ValueMetaDate(valueMetaName);
      case BINARY -> new ValueMetaBinary(valueMetaName, type.getPrecision(), -1);
      case JSON -> new ValueMetaJson(valueMetaName);
      case INET -> new ValueMetaInternetAddress(valueMetaName);
      default -> new ValueMetaNone(valueMetaName);
    };
  }

  public static Type createType(final IValueMeta meta) {
    return switch (meta.getType()) {
      case IValueMeta.TYPE_BOOLEAN -> BooleanType.BOOLEAN;
      case IValueMeta.TYPE_DATE, IValueMeta.TYPE_TIMESTAMP -> DateType.DATE;
      case IValueMeta.TYPE_STRING -> StringType.of(meta.getLength());
      case IValueMeta.TYPE_INTEGER -> IntegerType.of(meta.getLength());
      case IValueMeta.TYPE_NUMBER, IValueMeta.TYPE_BIGNUMBER ->
          NumberType.of(meta.getLength(), meta.getPrecision());
      case IValueMeta.TYPE_JSON -> JsonType.JSON;
      case IValueMeta.TYPE_INET -> InetType.INET;
      case IValueMeta.TYPE_BINARY -> BinaryType.of(meta.getLength());
      default ->
          throw new ExpressionException(
              ErrorCode.UNSUPPORTED_FIELD_TYPE, meta.getName(), meta.getTypeDesc());
    };
  }
}
