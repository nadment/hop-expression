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

import java.util.ArrayList;
import java.util.List;
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
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operators;

public class Types {

  private Types() {
    // Utility class
  }

  public static final UnknownType UNKNOWN = new UnknownType(true);

  public static final TimeUnitType TIMEUNIT = new TimeUnitType(true);

  public static final ArrayType ARRAY = new ArrayType(UNKNOWN, true);

  public static final AnyType ANY = new AnyType(true);

  /** Default BINARY type with maximum precision. */
  public static final BinaryType BINARY = new BinaryType(TypeName.BINARY.getMaxPrecision(), true);

  /** Default BINARY NOT NULLtype with maximum precision. */
  public static final BinaryType BINARY_NOT_NULL =
      new BinaryType(TypeName.BINARY.getMaxPrecision(), false);

  /** Default BOOLEAN type. */
  public static final BooleanType BOOLEAN = new BooleanType(true);

  /** Default BOOLEAN NOT NULL type. */
  public static final BooleanType BOOLEAN_NOT_NULL = new BooleanType(false);

  /** Default STRING type with maximum precision. */
  public static final StringType STRING = new StringType(TypeName.STRING.getMaxPrecision(), true);

  /** Default STRING NOT NULL type with maximum precision. */
  public static final StringType STRING_NOT_NULL =
      new StringType(TypeName.STRING.getMaxPrecision(), false);

  /** Default INTEGER type with maximum precision. */
  public static final IntegerType INTEGER =
      new IntegerType(TypeName.INTEGER.getMaxPrecision(), true);

  /** Default INTEGER NOT NULL type with maximum precision. */
  public static final IntegerType INTEGER_NOT_NULL =
      new IntegerType(TypeName.INTEGER.getMaxPrecision(), false);

  /** Default NUMBER(38,9) type with max precision and default scale. */
  public static final NumberType NUMBER =
      new NumberType(TypeName.NUMBER.getMaxPrecision(), TypeName.NUMBER.getDefaultScale(), true);

  /** Default NUMBER(38,9) NOT NULL type with max precision and default scale. */
  public static final NumberType NUMBER_NOT_NULL =
      new NumberType(TypeName.NUMBER.getMaxPrecision(), TypeName.NUMBER.getDefaultScale(), false);

  /** Default DATE type with default parameters. */
  public static final DateType DATE = new DateType(true);

  /** Default DATE NOT NULL type with default parameters. */
  public static final DateType DATE_NOT_NULL = new DateType(false);

  /** Default INTERVAL type with default parameters. */
  public static final IntervalType INTERVAL = new IntervalType(true);

  /** Default INTERVAL NOT NULL type with default parameters. */
  public static final IntervalType INTERVAL_NOT_NULL = new IntervalType(false);

  /** Default INET type. */
  public static final InetType INET = new InetType(true);

  /** Default INET NOT NULL type. */
  public static final InetType INET_NOT_NULL = new InetType(false);

  /** Default JSON type. */
  public static final JsonType JSON = new JsonType(true);

  /** Default JSON NOT NULL type. */
  public static final JsonType JSON_NOT_NULL = new JsonType(false);

  /** Returns the most general of a set of types */
  public static Type getLeastRestrictive(List<Type> types) {

    if (types.isEmpty()) return Types.UNKNOWN;

    Type result = null;
    for (Type type : types) {
      result = getLeastRestrictive(result, type);
    }
    return result;
  }

  public static Type getLeastRestrictive(Array array) {
    Type result = null;
    for (IExpression operand : array) {
      Type type = operand.getType();
      if (operand instanceof Array child) {
        type = Types.getLeastRestrictive(child);
      }
      result = Types.getLeastRestrictive(result, type);
    }
    return result;
  }

  public static Type getLeastRestrictive(final Type type1, final Type type2) {
    if (type1 == null) return type2;
    if (type2 == null) return type1;
    if (type1.getName().ordinal() > type2.getName().ordinal()
        || (type1.isFamily(type2.getFamily()) && type1.getPrecision() > type2.getPrecision())) {
      return type1;
    }
    if (type1.isFamily(type2.getFamily())) {
      if (type1.getPrecision() > type2.getPrecision()
          || (type1.getPrecision() == type2.getPrecision()
              && type1.getScale() > type2.getScale())) {
        return type1;
      }
    }

    return type2;
  }

  /**
   * Coerce all the operands to a common {@code Type}.
   *
   * @param call the call
   * @param commonType common type to coerce to
   */
  public static boolean coerceOperandsType(final Call call, final Type type) {
    boolean coerced = false;
    for (int index = 0; index < call.getOperandCount(); index++) {
      coerced |= coerceOperandType(call, type, index);
    }

    // Inferring the return type
    call.inferReturnType();

    return coerced;
  }

  /**
   * Coerce the operand at the specified index to target {@code Type}.
   *
   * <p>If the operand is a {@code Array}, coercion of all its elements
   */
  public static boolean coerceOperandType(final Call call, final Type type, int index) {

    IExpression operand = call.getOperand(index);

    // If the operand is a array, coercion of all its elements
    if (operand.is(Kind.ARRAY)) {
      Array array = coerceOperandType((Array) operand, type);
      if (!array.equals((Array) operand)) {
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

  public static Array coerceOperandType(final Array array, final Type type) {
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
    Call call = new Call(Operators.CAST, expression, Literal.of(type));
    call.inferReturnType();
    return call;
  }

  /** Determines common type for a comparison operator. */
  public static Type getCommonTypeForComparison(final Type type1, final Type type2) {

    if (type1 == null || type2 == null) return null;

    if (type2.is(TypeName.UNKNOWN)) return type1;

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
      return NUMBER;
    }
    // Numeric compare STRING -> NUMBER
    if (isNumeric(type1) && isString(type2)) {
      return NUMBER;
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

  /** Returns whether a IExpression should be cast to a target type. */
  public static boolean needToCast(IExpression expression, Type toType) {
    return expression.getType().getName().ordinal() < toType.getName().ordinal();
  }

  /** Coerces the operands of a addition or subtract expression into numeric type. */
  public static boolean coercionArithmeticOperator(Call call) {

    Type left = call.getOperand(0).getType();
    Type right = call.getOperand(1).getType();

    if (left.getName() == right.getName()) return false;

    // STRING <operator> numeric -> NUMBER
    if (isString(left) && isNumeric(right)) {
      return coerceOperandsType(call, NUMBER);
    }
    // Numeric <operator> STRING -> NUMBER
    if (isNumeric(left) && isString(right)) {
      return coerceOperandsType(call, NUMBER);
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
      return Types.coerceOperandType(call, NUMBER, 0);
    }
    // Numeric <operator> STRING -> NUMBER
    if (isNumeric(left) && isString(right)) {
      return Types.coerceOperandType(call, NUMBER, 1);
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

    if (source.getName() == target.getName()) {
      if (source.getPrecision() <= target.getPrecision()
          && source.getScale() <= target.getScale()) {
        return true;
      }
    }

    // Return FALSE by default
    return false;
  }

  /**
   * Returns whether two types are comparable. They need to be scalar types of the same family
   *
   * @param type1 First type
   * @param type2 Second type
   * @return Whether types are comparable
   */
  public static boolean isComparable(Type type1, Type type2) {

    final TypeFamily family1 = type1.getFamily();
    final TypeFamily family2 = type2.getFamily();
    if (family1 == family2) {
      return true;
    }

    // If one of the arguments is of type 'ANY', return true.
    if (family1 == TypeFamily.ANY || family2 == TypeFamily.ANY) {
      return true;
    }

    // If one of the arguments is of type 'NULL', return true.
    // if (family1 == TypeFamily.NULL
    // || family2 == TypeFamily.NULL) {
    // return true;
    // }

    return false;
  }

  /** Returns whether a type is atomic (date, numeric, string or BOOLEAN). */
  public static boolean isAtomic(final Type type) {
    if (type == null) return false;
    TypeName id = type.getName();
    return id == TypeName.STRING
        || id == TypeName.DATE
        || id == TypeName.INTEGER
        || id == TypeName.NUMBER
        || id == TypeName.BOOLEAN;
  }

  public static boolean isBinary(final Type type) {
    if (type == null) return false;
    return type.is(TypeName.BINARY);
  }

  public static boolean isBoolean(final Type type) {
    if (type == null) return false;
    return type.is(TypeName.BOOLEAN);
  }

  public static boolean isNumeric(final Type type) {
    if (type == null) return false;
    return type.isFamily(TypeFamily.NUMERIC);
  }

  public static boolean isInteger(final Type type) {
    if (type == null) return false;
    return type.is(TypeName.INTEGER);
  }

  public static boolean isNumber(final Type type) {
    if (type == null) return false;
    return type.is(TypeName.NUMBER);
  }

  public static boolean isString(final Type type) {
    if (type == null) return false;
    return type.is(TypeName.STRING);
  }

  public static boolean isDate(final Type type) {
    if (type == null) return false;
    return type.is(TypeName.DATE);
  }

  public static boolean isInterval(final Type type) {
    if (type == null) return false;
    return type.is(TypeName.INTERVAL);
  }

  /** Return the default {@link Type} that belongs to this {@link TypeName}. */
  // public Type getDefaultType(TypeIdTypes.STRINGch (id) {
  // case BOOLEAN:
  // return Types.BOOLEAN;
  // case BINARY:
  // return Types.BINARY;
  // case STRING:
  // return StringType.STRING;
  // case TEMPORAL:
  // return Types.DATE;
  // case INTERVAL:
  // return Types.INTERVAL;
  // case NUMERIC:
  // return Types.NUMBER;
  // case JSON:
  // return Types.JSON;
  // default:
  // return null;
  // }
  // }

  public static IValueMeta createValueMeta(final String name, final TypeName typeId) {

    if (name == null) {
      throw new IllegalArgumentException("Name must not be null");
    }
    if (typeId == null) {
      throw new IllegalArgumentException("TypeId must not be null");
    }

    switch (typeId) {
      case BOOLEAN:
        return new ValueMetaBoolean(name);
      case INTEGER:
        return new ValueMetaInteger(name, 9, 0);
      case NUMBER:
        return new ValueMetaBigNumber(name, -1, -1);
      case STRING:
        return new ValueMetaString(name, -1, -1);
      case DATE:
        return new ValueMetaDate(name, -1, -1);
      case BINARY:
        return new ValueMetaBinary(name, -1, -1);
      case JSON:
        return new ValueMetaJson(name);
      case INET:
        return new ValueMetaInternetAddress(name);
      case UNKNOWN:
      default:
        return new ValueMetaNone(name);
    }
  }

  public static IValueMeta createValueMeta(final String name, final Type type) {
    if (name == null) {
      throw new IllegalArgumentException("Name must not be null");
    }
    if (type == null) {
      throw new IllegalArgumentException("Type must not be null");
    }
    switch (type.getName()) {
      case BOOLEAN:
        return new ValueMetaBoolean(name);
      case INTEGER:
        return new ValueMetaInteger(name);
      case NUMBER:
        return new ValueMetaBigNumber(name, type.getPrecision(), type.getScale());
      case STRING:
        return new ValueMetaString(name, type.getPrecision(), type.getScale());
      case DATE:
        return new ValueMetaDate(name);
      case JSON:
        return new ValueMetaJson(name);
      case INET:
        return new ValueMetaInternetAddress(name);
      case UNKNOWN:
      default:
        return new ValueMetaNone(name);
    }
  }
}
