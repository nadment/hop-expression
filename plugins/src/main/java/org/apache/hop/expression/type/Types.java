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

import org.apache.hop.expression.Call;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operators;

public class Types {

  private Types() {
    // Utility class
  }


  public static final UnknownType UNKNOWN = new UnknownType(true);
  
  public static final AnyType ANY = new AnyType(true);

  /**
   * Default BINARY type with maximum precision.
   */
  public static final BinaryType BINARY = new BinaryType(TypeId.BINARY.getMaxPrecision(), true);

  /**
   * Default BOOLEAN type.
   */
  public static final BooleanType BOOLEAN = new BooleanType(true);
  /**
   * Default BOOLEAN NOT NULL type.
   */
  public static final BooleanType BOOLEAN_NOT_NULL = new BooleanType(false);
  /**
   * Default STRING type with maximum precision.
   */
  public static final StringType STRING = new StringType(TypeId.STRING.getMaxPrecision(), true);
  /**
   * Default INTEGER type with maximum precision.
   */
  public static final IntegerType INTEGER = new IntegerType(TypeId.INTEGER.getMaxPrecision(), true);
  /**
   * Default NUMBER(38,9) type with max precision and default scale.
   */
  public static final NumberType NUMBER =
      new NumberType(TypeId.NUMBER.getMaxPrecision(), TypeId.NUMBER.getDefaultScale(), true);

  /**
   * Default DATE type with default parameters.
   */
  public static final DateType DATE = new DateType(true);

  /**
   * Default DATE NOT NULL type with default parameters.
   */
  public static final DateType DATE_NOT_NULL = new DateType(false);


  public static final IntervalType INTERVAL = new IntervalType(true);

  public static final IntervalType INTERVAL_NOT_NULL = new IntervalType(false);


  /**
   * Default JSON type.
   */
  public static final JsonType JSON = new JsonType(true);

  /**
   * Default JSON NOT NULL type.
   */
  public static final JsonType JSON_NOT_NULL = new JsonType(false);

  /**
   * Coerce all the operands to a common {@code Type}.
   *
   * @param call the call
   * @param commonType common type to coerce to
   */
  protected static boolean coerceOperandsType(Call call, Type type) {
    boolean coerced = false;
    for (int index = 0; index < call.getOperandCount(); index++) {
      coerced = coerceOperandType(call, index, type) || coerced;
    }
    return coerced;
  }

  /**
   * Coerce the operand at the specified index to target {@code Type}.
   * we do this base on the fact that validate happens before type coercion.
   */
  protected static boolean coerceOperandType(Call call, int index, Type type) {

    IExpression operand = call.getOperand(index);

    // Check it early.
    if (!needToCast(operand, type)) {
      return false;
    }

    IExpression desired = new Call(Operators.CAST, operand, Literal.of(type));
    call.setOperand(index, desired);

    return true;
  }



  /**
   * Determines common type for a comparison operator whose operands are STRING
   * type and the other (non STRING) type.
   */
  public static Type commonTypeForBinaryComparison(Type type1, Type type2) {

    if (type1==null || type2==null ) return null;
    
    // DATE compare STRING -> DATE
    if (isDate(type1) && isString(type2)) {
      return type1;
    }

    // STRING compare DATE -> DATE
    if (isString(type1) && isDate(type2)) {
      return type2;
    }

    return type1.getId().ordinal() > type2.getId().ordinal() ? type1 : type2;
  }

  /**
   * Coerces CASE WHEN statement branches to one common type.
   *
   * <p>
   * Rules: Find common type for all the then operands and else operands,
   * then try to coerce the then/else operands to the type if needed.
   */
  // boolean caseWhenCoercion(SqlCallBinding binding);


  /**
   * Returns whether a IExpression should be cast to a target type.
   */
  protected static boolean needToCast(IExpression expression, Type toType) {
    return expression.getType().getId().ordinal() < toType.getId().ordinal();
  }


  /**
   * Coerces operand of arithmetic expressions to Numeric type.
   */
  public boolean aithmeticCoercion(Call call) {
    return true;
  }

  /**
   * Coerces operands in comparison expressions.
   */
  public static boolean comparisonCoercion(Call call) {

    Type commonType = call.getOperand(0).getType();
    for (int index = 1; index < call.getOperandCount(); index++) {
      commonType = commonTypeForBinaryComparison(commonType, call.getOperand(index).getType());
    }

    return coerceOperandsType(call, commonType);
  }

  /**
   * Coerces operands in tuple expressions.
   */
//  public static boolean comparisonCoercion(Tuple tuple, Type commonType) {
//    for (int index = 0; index < tuple.size(); index++) {
//      commonType = commonTypeForBinaryComparison(commonType, tuple.get(index).getType());
//    }
//    return coerceOperandsType(tuple, commonType);
//  }

  /**
   * Coerce all the operands to a common {@code Type}.
   *
   * @param call the call
   * @param commonType common type to coerce to
   */
//  protected static boolean coerceOperandsType(Tuple tuple, Type type) {
//    boolean coerced = false;
//    for (int index = 0; index < call.getOperandCount(); index++) {
//      coerced = coerceOperandType(call, index, type) || coerced;
//    }
//    return coerced;
//  }
  
  
  /**
   * Returns whether the conversion from {@code source} to {@code target} type
   * is a 'loss-less' cast, that is, a cast from which
   * the original value of the field can be certainly recovered.
   *
   * @param source source type
   * @param target target type
   * @return true if the conversion is a loss-less cast
   */
  public static boolean isLosslessCast(Type source, Type target) {

    if (source.getId() == target.getId()) {
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
  public static boolean isAtomic(Type type) {
    if (type == null)
      return false;
    TypeId id = type.getId();
    return id == TypeId.STRING || id == TypeId.DATE || id == TypeId.INTEGER || id == TypeId.NUMBER
        || id == TypeId.BOOLEAN;
  }

  public static boolean isBoolean(final Type type) {
    if (type == null)
      return false;
    return type.is(TypeId.BOOLEAN);
  }

  public static boolean isInteger(final Type type) {
    if (type == null)
      return false;
    return type.is(TypeId.INTEGER);
  }

  public static boolean isNumber(final Type type) {
    if (type == null)
      return false;
    return type.is(TypeId.NUMBER);
  }

  public static boolean isString(final Type type) {
    if (type == null)
      return false;
    return type.is(TypeId.STRING);
  }

  public static boolean isDate(final Type type) {
    if (type == null)
      return false;
    return type.is(TypeId.DATE);
  }

  /** Return the default {@link Type} that belongs to this {@link TypeId}. */
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
}