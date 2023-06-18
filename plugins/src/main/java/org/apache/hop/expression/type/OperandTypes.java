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

import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.type.CompositeOperandTypeChecker.Composition;
import java.util.List;

/**
 * Strategies for checking operand types.
 */
public final class OperandTypes {

  private OperandTypes() {
    // Utility class
  }

  /**
   * Creates a checker that passes if each operand is a member of a
   * corresponding family.
   */
  public static FamilyOperandTypeChecker family(DataFamily... families) {
    return new FamilyOperandTypeChecker(List.of(families), i -> false);
  }

  /**
   * Creates a checker that passes if all operand is a member of a
   * corresponding family.
   */
  public static FamilyOperandTypeChecker family(DataFamily family, IOperandCountRange range) {
    return new FamilyOperandTypeChecker(family, range);
  }

  /**
   * Creates a checker that passes if any one of the rules passes.
   */
  public static IOperandTypeChecker or(IOperandTypeChecker... rules) {
    return new CompositeOperandTypeChecker(Composition.OR, List.of(rules));
  }

  /**
   * Creates a checker that passes if all of the rules pass.
   */
  public static IOperandTypeChecker and(IOperandTypeChecker... rules) {
    return new CompositeOperandTypeChecker(Composition.AND, List.of(rules));
  }

  /**
   * Creates an operand checker from a sequence of single-operand checkers.
   */
  public static SequenceOperandTypeChecker sequence(IOperandTypeChecker... rules) {
    return new SequenceOperandTypeChecker(List.of(rules));
  }

  public static ISingleOperandTypeChecker literal(Class<?> javaClass) {
    return new LiteralOperandTypeChecker(javaClass);
  }

  /**
   * Operand type-checking strategy that check nothing.
   */
  public static final IOperandTypeChecker NO_CHECK = new NoneOperandTypeChecker();

  /**
   * Operand type-checking strategy type must be a data type.
   */
  public static final IOperandTypeChecker DATATYPE = literal(DataType.class);

  /**
   * Operand type-checking strategy type must be a time unit.
   */
  public static final IOperandTypeChecker TIMEUNIT = literal(TimeUnit.class);

  /**
   * Operand type-checking strategy type must be a literal string non-NULL.
   */
  public static final IOperandTypeChecker TEXT = literal(String.class);

  /**
   * Operand type-checking strategy for an operator which takes no operands.
   */
  public static final IOperandTypeChecker NILADIC = family();

  public static final IOperandTypeChecker ANY = family(DataFamily.ANY);
  public static final IOperandTypeChecker ANY_OPTIONAL_BOOLEAN =
      family(DataFamily.ANY, DataFamily.BOOLEAN).optional(i -> i == 1);
  public static final IOperandTypeChecker ANY_ANY = family(DataFamily.ANY, DataFamily.ANY);
  public static final IOperandTypeChecker ANY_ANY_ANY =
      family(DataFamily.ANY, DataFamily.ANY, DataFamily.ANY);
  public static final IOperandTypeChecker OPTIONAL_ANY =
      family(DataFamily.ANY).optional(i -> i == 0);
  public static final IOperandTypeChecker ANY_SAME_SAME =
      ANY_ANY_ANY.and(new SameOperandTypeChecker(OperandCountRange.of(3), 1));

  /**
   * Operand type-checking strategy where two operands must both be in the
   * same type family.
   */
  public static final IOperandTypeChecker SAME_SAME =
      new SameOperandTypeChecker(OperandCountRange.of(2));

  /**
   * Operand type-checking strategy where three operands must both be in the
   * same type family.
   */
  public static final IOperandTypeChecker SAME_SAME_SAME =
      new SameOperandTypeChecker(OperandCountRange.of(3));
  /**
   * Operand type-checking strategy where any number of operands must all be
   * in the same type family.
   */
  public static final IOperandTypeChecker SAME_VARIADIC =
      new SameOperandTypeChecker(OperandCountRange.any());

  /**
   * Operand type-checking strategy where any positive number of operands must all be
   * in the same type family.
   */
  public static final IOperandTypeChecker AT_LEAST_ONE_SAME_VARIADIC =
      new SameOperandTypeChecker(OperandCountRange.between(1, -1));
  public static final IOperandTypeChecker AT_LEAST_TREE_SAME_VARIADIC =
      new SameOperandTypeChecker(OperandCountRange.between(3, -1));

  public static final IOperandTypeChecker BOOLEAN = family(DataFamily.BOOLEAN);
  public static final IOperandTypeChecker BOOLEAN_VARIADIC =
      family(DataFamily.BOOLEAN, OperandCountRange.between(1, -1));
  public static final IOperandTypeChecker BOOLEAN_BOOLEAN =
      family(DataFamily.BOOLEAN, DataFamily.BOOLEAN);
  public static final IOperandTypeChecker BOOLEAN_ANY =
      family(DataFamily.BOOLEAN, DataFamily.ANY);
  public static final IOperandTypeChecker BOOLEAN_ANY_ANY =
      family(DataFamily.BOOLEAN, DataFamily.ANY, DataFamily.ANY);
  public static final IOperandTypeChecker BOOLEAN_SAME_SAME =
      BOOLEAN_ANY_ANY.and(new SameOperandTypeChecker(OperandCountRange.of(3), 1));

  public static final IOperandTypeChecker BINARY = family(DataFamily.BINARY);
  public static final IOperandTypeChecker BINARY_VARIADIC =
      family(DataFamily.BINARY, OperandCountRange.between(1, -1));
  public static final IOperandTypeChecker BINARY_BINARY_VARIADIC =
      family(DataFamily.BINARY, OperandCountRange.between(2, -1));
  public static final IOperandTypeChecker BINARY_BINARY =
      family(DataFamily.BINARY, DataFamily.BINARY);
  public static final IOperandTypeChecker BINARY_NUMERIC =
      family(DataFamily.BINARY, DataFamily.NUMERIC);
  public static final IOperandTypeChecker BINARY_OPTIONAL_TEXT =
      sequence(OperandTypes.BINARY, OperandTypes.TEXT).optional(i -> i == 1);

  public static final IOperandTypeChecker NUMERIC = family(DataFamily.NUMERIC);
  public static final IOperandTypeChecker NUMERIC_NUMERIC =
      family(DataFamily.NUMERIC, DataFamily.NUMERIC);
  public static final IOperandTypeChecker NUMERIC_NUMERIC_NUMERIC =
      family(DataFamily.NUMERIC, DataFamily.NUMERIC, DataFamily.NUMERIC);
  public static final IOperandTypeChecker NUMERIC_NUMERIC_NUMERIC_NUMERIC_NUMERIC_NUMERIC_OPTIONAL_NUMERIC =
      family(DataFamily.NUMERIC, DataFamily.NUMERIC, DataFamily.NUMERIC, DataFamily.NUMERIC,
          DataFamily.NUMERIC, DataFamily.NUMERIC, DataFamily.NUMERIC).optional(i -> i == 6);
  public static final IOperandTypeChecker NUMERIC_OPTIONAL_STRING =
      family(DataFamily.NUMERIC, DataFamily.STRING).optional(i -> i == 1);
  public static final IOperandTypeChecker NUMERIC_OPTIONAL_NUMERIC =
      family(DataFamily.NUMERIC, DataFamily.NUMERIC).optional(i -> i == 1);
  public static final IOperandTypeChecker NUMERIC_OPTIONAL_TEXT =
      sequence(OperandTypes.NUMERIC, OperandTypes.TEXT).optional(i -> i == 1);
  public static final IOperandTypeChecker OPTIONAL_NUMERIC =
      family(DataFamily.NUMERIC).optional(i -> i == 0);

  public static final IOperandTypeChecker DATE = family(DataFamily.TEMPORAL);
  public static final IOperandTypeChecker DATE_DATE =
      family(DataFamily.TEMPORAL, DataFamily.TEMPORAL);
  public static final IOperandTypeChecker DATE_NUMERIC =
      family(DataFamily.TEMPORAL, DataFamily.NUMERIC);
  public static final IOperandTypeChecker DATE_STRING =
      family(DataFamily.TEMPORAL, DataFamily.STRING);
  public static final IOperandTypeChecker DATE_OPTIONAL_STRING =
      family(DataFamily.TEMPORAL, DataFamily.STRING).optional(i -> i == 1);

  public static final IOperandTypeChecker TIMEUNIT_DATE = sequence(TIMEUNIT, DATE);
  public static final IOperandTypeChecker TIMEUNIT_NUMERIC_DATE = sequence(TIMEUNIT, NUMERIC, DATE);
  public static final IOperandTypeChecker TIMEUNIT_DATE_DATE = sequence(TIMEUNIT, DATE, DATE);

  public static final IOperandTypeChecker STRING = family(DataFamily.STRING);
  public static final IOperandTypeChecker STRING_VARIADIC =
      family(DataFamily.STRING, OperandCountRange.between(1, -1));
  public static final IOperandTypeChecker STRING_STRING_VARIADIC =
      family(DataFamily.STRING, OperandCountRange.between(2, -1));
  public static final IOperandTypeChecker STRING_STRING =
      family(DataFamily.STRING, DataFamily.STRING);
  public static final IOperandTypeChecker STRING_STRING_STRING =
      family(DataFamily.STRING, DataFamily.STRING, DataFamily.STRING);
  public static final IOperandTypeChecker STRING_STRING_NUMERIC =
      family(DataFamily.STRING, DataFamily.STRING, DataFamily.NUMERIC);
  public static final IOperandTypeChecker STRING_STRING_NUMERIC_NUMERIC =
      family(DataFamily.STRING, DataFamily.STRING, DataFamily.NUMERIC, DataFamily.NUMERIC);
  public static final IOperandTypeChecker STRING_NUMERIC =
      family(DataFamily.STRING, DataFamily.NUMERIC);
  public static final IOperandTypeChecker STRING_NUMERIC_NUMERIC =
      family(DataFamily.STRING, DataFamily.NUMERIC, DataFamily.NUMERIC);
  public static final IOperandTypeChecker STRING_OPTIONAL_NUMERIC =
      family(DataFamily.STRING, DataFamily.NUMERIC).optional(i -> i == 1);
  public static final IOperandTypeChecker STRING_OPTIONAL_STRING =
      family(DataFamily.STRING, DataFamily.STRING).optional(i -> i == 1);
  public static final IOperandTypeChecker STRING_NUMERIC_OPTIONAL_NUMERIC =
      family(DataFamily.STRING, DataFamily.NUMERIC, DataFamily.NUMERIC).optional(i -> i == 2);
  public static final IOperandTypeChecker STRING_STRING_OPTIONAL_STRING =
      family(DataFamily.STRING, DataFamily.STRING, DataFamily.STRING).optional(i -> i == 2);
  public static final IOperandTypeChecker STRING_STRING_OPTIONAL_NUMERIC =
      family(DataFamily.STRING, DataFamily.STRING, DataFamily.NUMERIC).optional(i -> i == 2);
  public static final IOperandTypeChecker STRING_STRING_OPTIONAL_NUMERIC_NUMERIC =
      family(DataFamily.STRING, DataFamily.STRING, DataFamily.NUMERIC, DataFamily.NUMERIC)
          .optional(i -> i >= 2);
  public static final IOperandTypeChecker STRING_NUMERIC_OPTIONAL_STRING =
      family(DataFamily.STRING, DataFamily.NUMERIC, DataFamily.STRING).optional(i -> i == 2);
  public static final IOperandTypeChecker STRING_STRING_OPTIONAL_NUMERIC_STRING =
      family(DataFamily.STRING, DataFamily.STRING, DataFamily.NUMERIC, DataFamily.STRING)
          .optional(i -> i >= 2);
  public static final IOperandTypeChecker STRING_STRING_OPTIONAL_NUMERIC_NUMERIC_STRING =
      family(DataFamily.STRING, DataFamily.STRING, DataFamily.NUMERIC, DataFamily.NUMERIC,
          DataFamily.STRING).optional(i -> i >= 2);
  public static final IOperandTypeChecker STRING_STRING_OPTIONAL_NUMERIC_NUMERIC_NUMERIC_STRING =
      family(DataFamily.STRING, DataFamily.STRING, DataFamily.NUMERIC, DataFamily.NUMERIC,
          DataFamily.NUMERIC, DataFamily.STRING).optional(i -> i >= 2);
  public static final IOperandTypeChecker STRING_DATE =
      family(DataFamily.STRING, DataFamily.TEMPORAL);
  public static final IOperandTypeChecker STRING_STRING_DATE =
      family(DataFamily.STRING, DataFamily.STRING, DataFamily.TEMPORAL);

  public static final IOperandTypeChecker DATE_OPTIONAL_TIMEUNIT =
      sequence(DATE, TIMEUNIT).optional(i -> i == 1);
  public static final IOperandTypeChecker DATE_OPTIONAL_TEXT =
      sequence(DATE, TEXT).optional(i -> i == 1);
  public static final IOperandTypeChecker STRING_OPTIONAL_TEXT =
      sequence(STRING, TEXT).optional(i -> i == 1);

  public static final IOperandTypeChecker JSON = family(DataFamily.JSON);
  public static final IOperandTypeChecker JSON_STRING = family(DataFamily.JSON, DataFamily.STRING);

  public static final IOperandTypeChecker CASE_OPERATOR = new CaseOperatorOperandTypeChecker();
  public static final IOperandTypeChecker CAST_OPERATOR = OperandTypes
      .sequence(OperandTypes.ANY, OperandTypes.DATATYPE, OperandTypes.TEXT).optional(i -> i == 2);
  public static final IOperandTypeChecker DECODE_FUNCTION = new DecodeFunctionOperandTypeChecker();
}
