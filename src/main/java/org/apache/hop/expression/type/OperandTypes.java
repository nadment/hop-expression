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

  private OperandTypes() {
    // Utility class
  }

  /** Creates a checker that passes if each operand is a member of a corresponding family. */
  public static FamilyOperandTypeChecker family(TypeFamily... families) {
    return new FamilyOperandTypeChecker(List.of(families), i -> false);
  }

  /** Creates a checker that passes if any one of the rules passes. */
  public static IOperandTypeChecker or(IOperandTypeChecker... rules) {
    return new CompositeOperandTypeChecker(Composition.OR, List.of(rules), null);
  }

  /** Creates a checker that passes if all of the rules pass. */
  public static IOperandTypeChecker and(IOperandTypeChecker... rules) {
    return new CompositeOperandTypeChecker(Composition.AND, List.of(rules), null);
  }

  /** Creates an operand checker from a sequence of single-operand checkers. */
  public static IOperandTypeChecker sequence(IOperandTypeChecker... rules) {
    return new CompositeOperandTypeChecker(Composition.SEQUENCE, List.of(rules), null);
  }

  /**
   * Creates a checker that passes if all of the rules pass for each operand, using a given operand
   * count strategy.
   */
  public static IOperandTypeChecker repeat(
      IOperandCountRange range, ISingleOperandTypeChecker... rules) {
    return new CompositeOperandTypeChecker(Composition.REPEAT, List.of(rules), range);
  }

  public static ISingleOperandTypeChecker literal(Class<?> javaClass) {
    return new LiteralOperandTypeChecker(javaClass);
  }

  /** Operand type-checking strategy type must be a data type. */
  public static final IOperandTypeChecker DATATYPE = literal(Type.class);

  /** Operand type-checking strategy type must be a time unit. */
  public static final IOperandTypeChecker TIMEUNIT = literal(TimeUnit.class);

  /** Operand type-checking strategy type must be a literal string non-NULL. */
  public static final IOperandTypeChecker TEXT = literal(String.class);

  public static final ISingleOperandTypeChecker ANY = family(TypeFamily.ANY);
  public static final IOperandTypeChecker ANY_BOOLEAN = family(TypeFamily.ANY, TypeFamily.BOOLEAN);
  public static final IOperandTypeChecker ANY_NUMERIC = family(TypeFamily.ANY, TypeFamily.NUMERIC);
  public static final IOperandTypeChecker ANY_ANY = family(TypeFamily.ANY, TypeFamily.ANY);
  public static final IOperandTypeChecker ANY_ANY_ANY =
      family(TypeFamily.ANY, TypeFamily.ANY, TypeFamily.ANY);
  public static final IOperandTypeChecker ANY_STRING = family(TypeFamily.ANY, TypeFamily.STRING);
  public static final IOperandTypeChecker ANY_SAME_SAME =
      ANY_ANY_ANY.and(new SameOperandTypeChecker(OperandCountRange.of(3), 1));

  /** Operand type-checking strategy for an operator which takes no operands. */
  public static final IOperandTypeChecker NILADIC = family();

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

  public static final ISingleOperandTypeChecker BOOLEAN = family(TypeFamily.BOOLEAN);
  public static final IOperandTypeChecker BOOLEAN_BOOLEAN =
      family(TypeFamily.BOOLEAN, TypeFamily.BOOLEAN);
  public static final IOperandTypeChecker BOOLEAN_VARIADIC =
      repeat(OperandCountRange.between(1, -1), BOOLEAN);

  public static final IOperandTypeChecker BOOLEAN_ANY = family(TypeFamily.BOOLEAN, TypeFamily.ANY);
  public static final IOperandTypeChecker BOOLEAN_ANY_ANY =
      family(TypeFamily.BOOLEAN, TypeFamily.ANY, TypeFamily.ANY);
  public static final IOperandTypeChecker BOOLEAN_SAME_SAME =
      BOOLEAN_ANY_ANY.and(new SameOperandTypeChecker(OperandCountRange.of(3), 1));

  public static final ISingleOperandTypeChecker BINARY = family(TypeFamily.BINARY);
  public static final IOperandTypeChecker BINARY_VARIADIC =
      repeat(OperandCountRange.between(1, -1), BINARY);
  public static final IOperandTypeChecker BINARY_BINARY =
      family(TypeFamily.BINARY, TypeFamily.BINARY);
  public static final IOperandTypeChecker BINARY_BINARY_VARIADIC =
      repeat(OperandCountRange.between(2, -1), BINARY);

  public static final IOperandTypeChecker BINARY_NUMERIC =
      family(TypeFamily.BINARY, TypeFamily.NUMERIC);
  public static final IOperandTypeChecker BINARY_NUMERIC_BINARY =
      family(TypeFamily.BINARY, TypeFamily.NUMERIC, TypeFamily.BINARY);
  public static final IOperandTypeChecker BINARY_NUMERIC_NUMERIC_BINARY =
      family(TypeFamily.BINARY, TypeFamily.NUMERIC, TypeFamily.NUMERIC, TypeFamily.BINARY);

  public static final IOperandTypeChecker BINARY_TEXT = sequence(BINARY, TEXT);

  public static final ISingleOperandTypeChecker NUMERIC = family(TypeFamily.NUMERIC);
  public static final IOperandTypeChecker NUMERIC_NUMERIC =
      family(TypeFamily.NUMERIC, TypeFamily.NUMERIC);
  public static final IOperandTypeChecker NUMERIC_NUMERIC_NUMERIC =
      family(TypeFamily.NUMERIC, TypeFamily.NUMERIC, TypeFamily.NUMERIC);
  public static final IOperandTypeChecker NUMERIC_NUMERIC_NUMERIC_NUMERIC_NUMERIC_NUMERIC =
      family(
          TypeFamily.NUMERIC,
          TypeFamily.NUMERIC,
          TypeFamily.NUMERIC,
          TypeFamily.NUMERIC,
          TypeFamily.NUMERIC,
          TypeFamily.NUMERIC);

  public static final IOperandTypeChecker NUMERIC_TEXT = sequence(NUMERIC, TEXT);

  public static final IOperandTypeChecker TEMPORAL = family(TypeFamily.TEMPORAL);
  public static final IOperandTypeChecker TEMPORAL_TEMPORAL =
      family(TypeFamily.TEMPORAL, TypeFamily.TEMPORAL);
  public static final IOperandTypeChecker TEMPORAL_NUMERIC =
      family(TypeFamily.TEMPORAL, TypeFamily.NUMERIC);
  public static final IOperandTypeChecker TEMPORAL_STRING =
      family(TypeFamily.TEMPORAL, TypeFamily.STRING);
  public static final IOperandTypeChecker TEMPORAL_INTERVAL =
      family(TypeFamily.TEMPORAL, TypeFamily.INTERVAL);

  public static final ISingleOperandTypeChecker INTERVAL = family(TypeFamily.INTERVAL);
  public static final IOperandTypeChecker INTERVAL_INTERVAL =
      family(TypeFamily.INTERVAL, TypeFamily.INTERVAL);
  public static final IOperandTypeChecker INTERVAL_TEMPORAL =
      family(TypeFamily.INTERVAL, TypeFamily.TEMPORAL);

  public static final IOperandTypeChecker TIMEUNIT_TEMPORAL = sequence(TIMEUNIT, TEMPORAL);
  public static final IOperandTypeChecker TIMEUNIT_TEMPORAL_TEMPORAL =
      sequence(TIMEUNIT, TEMPORAL, TEMPORAL);
  public static final IOperandTypeChecker TIMEUNIT_NUMERIC_TEMPORAL =
      sequence(TIMEUNIT, NUMERIC, TEMPORAL);
  public static final IOperandTypeChecker TIMEUNIT_INTERVAL = sequence(TIMEUNIT, INTERVAL);

  public static final ISingleOperandTypeChecker STRING = family(TypeFamily.STRING);
  public static final IOperandTypeChecker STRING_STRING =
      family(TypeFamily.STRING, TypeFamily.STRING);
  public static final IOperandTypeChecker STRING_STRING_STRING =
      family(TypeFamily.STRING, TypeFamily.STRING, TypeFamily.STRING);
  public static final IOperandTypeChecker STRING_STRING_NUMERIC =
      family(TypeFamily.STRING, TypeFamily.STRING, TypeFamily.NUMERIC);
  public static final IOperandTypeChecker STRING_STRING_NUMERIC_NUMERIC =
      family(TypeFamily.STRING, TypeFamily.STRING, TypeFamily.NUMERIC, TypeFamily.NUMERIC);
  public static final IOperandTypeChecker STRING_STRING_NUMERIC_NUMERIC_NUMERIC =
      family(
          TypeFamily.STRING,
          TypeFamily.STRING,
          TypeFamily.NUMERIC,
          TypeFamily.NUMERIC,
          TypeFamily.NUMERIC);
  public static final IOperandTypeChecker STRING_NUMERIC =
      family(TypeFamily.STRING, TypeFamily.NUMERIC);
  public static final IOperandTypeChecker STRING_NUMERIC_NUMERIC =
      family(TypeFamily.STRING, TypeFamily.NUMERIC, TypeFamily.NUMERIC);
  public static final IOperandTypeChecker STRING_NUMERIC_NUMERIC_STRING =
      family(TypeFamily.STRING, TypeFamily.NUMERIC, TypeFamily.NUMERIC, TypeFamily.STRING);
  public static final IOperandTypeChecker STRING_NUMERIC_STRING =
      family(TypeFamily.STRING, TypeFamily.NUMERIC, TypeFamily.STRING);
  public static final IOperandTypeChecker STRING_STRING_NUMERIC_STRING =
      family(TypeFamily.STRING, TypeFamily.STRING, TypeFamily.NUMERIC, TypeFamily.STRING);
  public static final IOperandTypeChecker STRING_STRING_NUMERIC_NUMERIC_STRING =
      family(
          TypeFamily.STRING,
          TypeFamily.STRING,
          TypeFamily.NUMERIC,
          TypeFamily.NUMERIC,
          TypeFamily.STRING);
  public static final IOperandTypeChecker STRING_STRING_NUMERIC_NUMERIC_NUMERIC_STRING =
      family(
          TypeFamily.STRING,
          TypeFamily.STRING,
          TypeFamily.NUMERIC,
          TypeFamily.NUMERIC,
          TypeFamily.NUMERIC,
          TypeFamily.STRING);
  public static final IOperandTypeChecker STRING_DATE =
      family(TypeFamily.STRING, TypeFamily.TEMPORAL);
  public static final IOperandTypeChecker STRING_STRING_TEMPORAL =
      family(TypeFamily.STRING, TypeFamily.STRING, TypeFamily.TEMPORAL);
  public static final IOperandTypeChecker STRING_VARIADIC =
      repeat(OperandCountRange.between(1, -1), STRING);
  public static final IOperandTypeChecker STRING_STRING_VARIADIC =
      repeat(OperandCountRange.between(2, -1), STRING);

  public static final IOperandTypeChecker TEMPORAL_TIMEUNIT = sequence(TEMPORAL, TIMEUNIT);
  public static final IOperandTypeChecker TEMPORAL_TEXT = sequence(TEMPORAL, TEXT);
  public static final IOperandTypeChecker STRING_TEXT = sequence(STRING, TEXT);

  public static final IOperandTypeChecker INET = family(TypeFamily.INET);

  public static final IOperandTypeChecker JSON = family(TypeFamily.JSON);
  public static final IOperandTypeChecker JSON_STRING = family(TypeFamily.JSON, TypeFamily.STRING);

  public static final ISingleOperandTypeChecker ARRAY = family(TypeFamily.ARRAY);
  public static final IOperandTypeChecker ARRAY_NUMERIC =
      family(TypeFamily.ARRAY, TypeFamily.NUMERIC);
  public static final IOperandTypeChecker ARRAY_VARIADIC =
      repeat(OperandCountRange.between(1, -1), ARRAY);

  /**
   * Operand type-checking strategy for BETWWEEN operator where operand types must allow ordered
   * comparisons.
   */
  public static final IOperandTypeChecker BETWEEN =
      new ComparableOperandTypeChecker(OperandCountRange.of(3), TypeComparability.ALL);

  /** Operand type-checking strategy for CAST operator. */
  public static final IOperandTypeChecker CAST =
      sequence(ANY, DATATYPE, TEXT).or(sequence(ANY, DATATYPE));

  public static final IOperandTypeChecker DECODE_FUNCTION = new DecodeFunctionOperandTypeChecker();
}
