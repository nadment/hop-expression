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

import java.util.List;
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.type.CompositeOperandTypeChecker.Composition;

/** A collection of strategies for checking operand types. */
public final class OperandTypes {

  /** Operand type-checking strategy type must be a data type. */
  public static final IOperandTypeChecker DATATYPE = literal(Type.class);

  /** Operand type-checking strategy type must be a time unit. */
  public static final IOperandTypeChecker TIMEUNIT = literal(TimeUnit.class);

  /** Operand type-checking strategy type must be a literal string non-NULL. */
  public static final IOperandTypeChecker TEXT = literal(String.class);

  public static final ISingleOperandTypeChecker ANY = explicit(TypeName.ANY);
  public static final IOperandTypeChecker ANY_BOOLEAN = explicit(TypeName.ANY, TypeName.BOOLEAN);
  public static final IOperandTypeChecker ANY_INTEGER = explicit(TypeName.ANY, TypeName.INTEGER);
  public static final IOperandTypeChecker ANY_ANY = explicit(TypeName.ANY, TypeName.ANY);
  public static final IOperandTypeChecker ANY_ANY_ANY =
      explicit(TypeName.ANY, TypeName.ANY, TypeName.ANY);
  public static final IOperandTypeChecker ANY_STRING = explicit(TypeName.ANY, TypeName.STRING);
  public static final IOperandTypeChecker ANY_ARRAY = explicit(TypeName.ANY, TypeName.ARRAY);
  public static final IOperandTypeChecker ANY_SAME_SAME =
      ANY_ANY_ANY.and(new SameOperandTypeChecker(OperandCountRange.of(3), 1));

  /** Operand type-checking strategy for an operator which takes no operands. */
  public static final IOperandTypeChecker NILADIC = explicit();

  /** Operand type-checking strategy where two operands must both be in the same type family. */
  public static final IOperandTypeChecker SAME_SAME =
      new SameOperandTypeChecker(OperandCountRange.of(2));

  /** Operand type-checking strategy where three operands must both be in the same type family. */
  public static final IOperandTypeChecker SAME_SAME_SAME =
      new SameOperandTypeChecker(OperandCountRange.of(3));

  /**
   * Operand type-checking strategy where any number of operands must all be in the same type
   * family.
   */
  public static final IOperandTypeChecker SAME_VARIADIC =
      new SameOperandTypeChecker(OperandCountRange.any());

  /**
   * Operand type-checking strategy where any positive number of operands must all be in the same
   * type family.
   */
  public static final IOperandTypeChecker AT_LEAST_ONE_SAME_VARIADIC =
      new SameOperandTypeChecker(OperandCountRange.from(1));

  public static final IOperandTypeChecker AT_LEAST_TREE_SAME_VARIADIC =
      new SameOperandTypeChecker(OperandCountRange.from(3));

  /** Operand type-checking strategy where any number of operands must allow ordered comparisons. */
  public static final IOperandTypeChecker COMPARABLE_ORDERED_VARIADIC =
      new ComparableOperandTypeChecker(OperandCountRange.between(1, -1), TypeComparability.ALL);

  /**
   * Operand type-checking strategy where operand type must allow ordered comparisons. Used when
   * instance comparisons are made on single operand functions.
   */
  public static final IOperandTypeChecker COMPARABLE_ORDERED =
      new ComparableOperandTypeChecker(OperandCountRange.of(1), TypeComparability.ALL);

  /** Operand type-checking strategy where operand types must allow ordered comparisons. */
  public static final IOperandTypeChecker COMPARABLE_ORDERED_COMPARABLE_ORDERED =
      new ComparableOperandTypeChecker(OperandCountRange.of(2), TypeComparability.ALL);

  /** Operand type-checking strategy where operand types must allow unordered comparisons. */
  public static final IOperandTypeChecker COMPARABLE_UNORDERED_COMPARABLE_UNORDERED =
      new ComparableOperandTypeChecker(OperandCountRange.of(2), TypeComparability.UNORDERED);

  public static final ISingleOperandTypeChecker BOOLEAN = explicit(TypeName.BOOLEAN);
  public static final IOperandTypeChecker BOOLEAN_BOOLEAN =
      explicit(TypeName.BOOLEAN, TypeName.BOOLEAN);
  public static final IOperandTypeChecker BOOLEAN_VARIADIC =
      repeat(OperandCountRange.between(1, -1), BOOLEAN);
  public static final IOperandTypeChecker BOOLEAN_ANY = explicit(TypeName.BOOLEAN, TypeName.ANY);
  public static final IOperandTypeChecker BOOLEAN_ANY_ANY =
      explicit(TypeName.BOOLEAN, TypeName.ANY, TypeName.ANY);
  public static final IOperandTypeChecker BOOLEAN_SAME_SAME =
      BOOLEAN_ANY_ANY.and(new SameOperandTypeChecker(OperandCountRange.of(3), 1));
  public static final ISingleOperandTypeChecker BINARY = explicit(TypeName.BINARY);
  public static final IOperandTypeChecker BINARY_VARIADIC =
      repeat(OperandCountRange.between(1, -1), BINARY);
  public static final IOperandTypeChecker BINARY_BINARY =
      explicit(TypeName.BINARY, TypeName.BINARY);
  public static final IOperandTypeChecker BINARY_BINARY_VARIADIC =
      repeat(OperandCountRange.between(2, -1), BINARY);
  public static final IOperandTypeChecker BINARY_INTEGER =
      explicit(TypeName.BINARY, TypeName.INTEGER);
  public static final IOperandTypeChecker BINARY_INTEGER_BINARY =
      explicit(TypeName.BINARY, TypeName.INTEGER, TypeName.BINARY);
  public static final IOperandTypeChecker BINARY_INTEGER_INTEGER_BINARY =
      explicit(TypeName.BINARY, TypeName.INTEGER, TypeName.INTEGER, TypeName.BINARY);
  public static final IOperandTypeChecker BINARY_TEXT = sequence(BINARY, TEXT);
  public static final ISingleOperandTypeChecker INTEGER = explicit(TypeName.INTEGER);
  public static final ISingleOperandTypeChecker NUMBER = explicit(TypeName.NUMBER);
  public static final IOperandTypeChecker NUMERIC = or(INTEGER, NUMBER);
  public static final IOperandTypeChecker NUMERIC_NUMERIC =
      sequence(INTEGER, INTEGER).or(sequence(NUMBER, NUMBER));
  public static final IOperandTypeChecker INTEGER_INTEGER =
      explicit(TypeName.INTEGER, TypeName.INTEGER);
  public static final IOperandTypeChecker NUMBER_NUMBER =
      explicit(TypeName.NUMBER, TypeName.NUMBER);
  public static final IOperandTypeChecker INTEGER_INTEGER_INTEGER =
      explicit(TypeName.INTEGER, TypeName.INTEGER, TypeName.INTEGER);
  public static final IOperandTypeChecker INTEGER_INTEGER_INTEGER_INTEGER_INTEGER_NUMBER =
      explicit(
          TypeName.INTEGER,
          TypeName.INTEGER,
          TypeName.INTEGER,
          TypeName.INTEGER,
          TypeName.INTEGER,
          TypeName.NUMBER);
  public static final IOperandTypeChecker NUMBER_TEXT = sequence(NUMBER, TEXT);
  public static final IOperandTypeChecker INTEGER_DATE = explicit(TypeName.INTEGER, TypeName.DATE);
  public static final IOperandTypeChecker DATE = explicit(TypeName.DATE);
  public static final IOperandTypeChecker DATE_DATE = explicit(TypeName.DATE, TypeName.DATE);
  public static final IOperandTypeChecker DATE_INTEGER = explicit(TypeName.DATE, TypeName.INTEGER);
  public static final IOperandTypeChecker DATE_STRING = explicit(TypeName.DATE, TypeName.STRING);
  public static final IOperandTypeChecker DATE_INTERVAL =
      explicit(TypeName.DATE, TypeName.INTERVAL);
  public static final ISingleOperandTypeChecker INTERVAL = explicit(TypeName.INTERVAL);
  public static final IOperandTypeChecker INTERVAL_INTERVAL =
      explicit(TypeName.INTERVAL, TypeName.INTERVAL);
  public static final IOperandTypeChecker INTERVAL_DATE =
      explicit(TypeName.INTERVAL, TypeName.DATE);
  public static final IOperandTypeChecker TIMEUNIT_DATE = sequence(TIMEUNIT, DATE);
  public static final IOperandTypeChecker TIMEUNIT_DATE_DATE = sequence(TIMEUNIT, DATE, DATE);
  public static final IOperandTypeChecker TIMEUNIT_INTEGER_DATE = sequence(TIMEUNIT, INTEGER, DATE);
  public static final IOperandTypeChecker TIMEUNIT_INTERVAL = sequence(TIMEUNIT, INTERVAL);
  public static final ISingleOperandTypeChecker STRING = explicit(TypeName.STRING);
  public static final IOperandTypeChecker STRING_STRING =
      explicit(TypeName.STRING, TypeName.STRING);
  public static final IOperandTypeChecker STRING_STRING_STRING =
      explicit(TypeName.STRING, TypeName.STRING, TypeName.STRING);
  public static final IOperandTypeChecker STRING_STRING_INTEGER =
      explicit(TypeName.STRING, TypeName.STRING, TypeName.INTEGER);
  public static final IOperandTypeChecker STRING_STRING_INTEGER_INTEGER =
      explicit(TypeName.STRING, TypeName.STRING, TypeName.INTEGER, TypeName.INTEGER);
  public static final IOperandTypeChecker STRING_STRING_INTEGER_INTEGER_INTEGER =
      explicit(
          TypeName.STRING, TypeName.STRING, TypeName.INTEGER, TypeName.INTEGER, TypeName.INTEGER);
  public static final IOperandTypeChecker STRING_INTEGER =
      explicit(TypeName.STRING, TypeName.INTEGER);
  public static final IOperandTypeChecker STRING_INTEGER_INTEGER =
      explicit(TypeName.STRING, TypeName.INTEGER, TypeName.INTEGER);
  public static final IOperandTypeChecker STRING_INTEGER_INTEGER_STRING =
      explicit(TypeName.STRING, TypeName.INTEGER, TypeName.INTEGER, TypeName.STRING);
  public static final IOperandTypeChecker STRING_INTEGER_STRING =
      explicit(TypeName.STRING, TypeName.INTEGER, TypeName.STRING);
  public static final IOperandTypeChecker STRING_STRING_INTEGER_STRING =
      explicit(TypeName.STRING, TypeName.STRING, TypeName.INTEGER, TypeName.STRING);
  public static final IOperandTypeChecker STRING_STRING_INTEGER_INTEGER_STRING =
      explicit(
          TypeName.STRING, TypeName.STRING, TypeName.INTEGER, TypeName.INTEGER, TypeName.STRING);
  public static final IOperandTypeChecker STRING_STRING_INTEGER_INTEGER_INTEGER_STRING =
      explicit(
          TypeName.STRING,
          TypeName.STRING,
          TypeName.INTEGER,
          TypeName.INTEGER,
          TypeName.INTEGER,
          TypeName.STRING);
  public static final IOperandTypeChecker STRING_DATE = explicit(TypeName.STRING, TypeName.DATE);
  public static final IOperandTypeChecker STRING_STRING_DATE =
      explicit(TypeName.STRING, TypeName.STRING, TypeName.DATE);
  public static final IOperandTypeChecker STRING_VARIADIC =
      repeat(OperandCountRange.between(1, -1), STRING);
  public static final IOperandTypeChecker STRING_STRING_VARIADIC =
      repeat(OperandCountRange.between(2, -1), STRING);
  public static final IOperandTypeChecker DATE_TIMEUNIT = sequence(DATE, TIMEUNIT);
  public static final IOperandTypeChecker DATE_TEXT = sequence(DATE, TEXT);
  public static final IOperandTypeChecker STRING_TEXT = sequence(STRING, TEXT);
  public static final IOperandTypeChecker INET = explicit(TypeName.INET);
  public static final IOperandTypeChecker JSON = explicit(TypeName.JSON);
  public static final IOperandTypeChecker JSON_STRING = explicit(TypeName.JSON, TypeName.STRING);
  public static final ISingleOperandTypeChecker ARRAY = explicit(TypeName.ARRAY);
  public static final IOperandTypeChecker ARRAY_INTEGER =
      explicit(TypeName.ARRAY, TypeName.INTEGER);
  public static final IOperandTypeChecker ARRAY_INTEGER_INTEGER =
      explicit(TypeName.ARRAY, TypeName.INTEGER, TypeName.INTEGER);
  public static final IOperandTypeChecker ARRAY_ANY = explicit(TypeName.ARRAY, TypeName.ANY);
  public static final IOperandTypeChecker ARRAY_ANY_INTEGER =
      explicit(TypeName.ARRAY, TypeName.ANY, TypeName.INTEGER);
  public static final IOperandTypeChecker ARRAY_STRING = explicit(TypeName.ARRAY, TypeName.STRING);
  public static final IOperandTypeChecker ARRAY_VARIADIC =
      repeat(OperandCountRange.between(1, -1), ARRAY);

  /**
   * Operand type-checking strategy for BETWEEN operator where operand types must allow ordered
   * comparisons.
   */
  public static final IOperandTypeChecker BETWEEN =
      new ComparableOperandTypeChecker(OperandCountRange.of(3), TypeComparability.ALL);

  /** Operand type-checking strategy for CAST operator. */
  public static final IOperandTypeChecker CAST =
      sequence(ANY, DATATYPE, TEXT).or(sequence(ANY, DATATYPE));

  /** Operand type-checking strategy for JSON_VALUE function. */
  public static final IOperandTypeChecker JSON_VALUE =
      sequence(JSON, STRING)
          .or(sequence(JSON, STRING, DATATYPE))
          .or(sequence(STRING, STRING))
          .or(sequence(STRING, STRING, DATATYPE));

  private OperandTypes() {
    // Utility class
  }

  /**
   * Creates a checker that passes if each operand is a member of a corresponding {@code TypeId}.
   */
  public static ExplicitOperandTypeChecker explicit(TypeName... names) {
    return new ExplicitOperandTypeChecker(List.of(names), i -> false);
  }

  /** Creates a checker that passes if each operand is a member of a corresponding family. */
  public static FamilyOperandTypeChecker family(TypeFamily... families) {
    return new FamilyOperandTypeChecker(List.of(families), i -> false);
  }

  /** Creates a checker that passes if any one of the rules passes. */
  public static IOperandTypeChecker or(IOperandTypeChecker... rules) {
    return new CompositeOperandTypeChecker(Composition.OR, List.of(rules), null);
  }

  /** Creates a checker that passes if all the rules pass. */
  public static IOperandTypeChecker and(IOperandTypeChecker... rules) {
    return new CompositeOperandTypeChecker(Composition.AND, List.of(rules), null);
  }

  /** Creates an operand checker from a sequence of single-operand checkers. */
  public static IOperandTypeChecker sequence(IOperandTypeChecker... rules) {
    return new CompositeOperandTypeChecker(Composition.SEQUENCE, List.of(rules), null);
  }

  /**
   * Creates a checker that passes if all the rules pass for each operand, using a given operand
   * count strategy.
   */
  public static IOperandTypeChecker repeat(
      IOperandCountRange range, ISingleOperandTypeChecker... rules) {
    return new CompositeOperandTypeChecker(Composition.REPEAT, List.of(rules), range);
  }

  public static ISingleOperandTypeChecker literal(Class<?> javaClass) {
    return new LiteralOperandTypeChecker(javaClass);
  }
}
