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
 * A collection of strategies for checking operand types.
 */
public final class OperandTypes {

  private OperandTypes() {
    // Utility class
  }

  /**
   * Creates a checker that passes if each operand is a member of a
   * corresponding family.
   */
  public static FamilyOperandTypeChecker family(TypeFamily... families) {
    return new FamilyOperandTypeChecker(List.of(families), i -> false);
  }

  /**
   * Creates a checker that passes if all operand is a member of a
   * corresponding family.
   */
  public static FamilyOperandTypeChecker family(TypeFamily family, IOperandCountRange range) {
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
  public static final IOperandTypeChecker DATATYPE = literal(Type.class);

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

  public static final IOperandTypeChecker ANY = family(TypeFamily.ANY);
  public static final IOperandTypeChecker ANY_OPTIONAL_BOOLEAN =
      family(TypeFamily.ANY, TypeFamily.BOOLEAN).optional(i -> i == 1);
  public static final IOperandTypeChecker ANY_ANY = family(TypeFamily.ANY, TypeFamily.ANY);
  public static final IOperandTypeChecker ANY_ANY_ANY =
      family(TypeFamily.ANY, TypeFamily.ANY, TypeFamily.ANY);
  public static final IOperandTypeChecker OPTIONAL_ANY =
      family(TypeFamily.ANY).optional(i -> i == 0);
  public static final IOperandTypeChecker ANY_STRING = family(TypeFamily.ANY, TypeFamily.STRING);
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
      new SameOperandTypeChecker(OperandCountRange.from(1));
  public static final IOperandTypeChecker AT_LEAST_TREE_SAME_VARIADIC =
      new SameOperandTypeChecker(OperandCountRange.from(3));

  public static final IOperandTypeChecker BOOLEAN = family(TypeFamily.BOOLEAN);
  public static final IOperandTypeChecker BOOLEAN_VARIADIC =
      family(TypeFamily.BOOLEAN, OperandCountRange.from(1));
  public static final IOperandTypeChecker BOOLEAN_BOOLEAN =
      family(TypeFamily.BOOLEAN, TypeFamily.BOOLEAN);
  public static final IOperandTypeChecker BOOLEAN_ANY = family(TypeFamily.BOOLEAN, TypeFamily.ANY);
  public static final IOperandTypeChecker BOOLEAN_ANY_ANY =
      family(TypeFamily.BOOLEAN, TypeFamily.ANY, TypeFamily.ANY);
  public static final IOperandTypeChecker BOOLEAN_SAME_SAME =
      BOOLEAN_ANY_ANY.and(new SameOperandTypeChecker(OperandCountRange.of(3), 1));

  public static final IOperandTypeChecker BINARY = family(TypeFamily.BINARY);
  public static final IOperandTypeChecker BINARY_VARIADIC =
      family(TypeFamily.BINARY, OperandCountRange.between(1, -1));
  public static final IOperandTypeChecker BINARY_BINARY_VARIADIC =
      family(TypeFamily.BINARY, OperandCountRange.between(2, -1));
  public static final IOperandTypeChecker BINARY_BINARY =
      family(TypeFamily.BINARY, TypeFamily.BINARY);
  public static final IOperandTypeChecker BINARY_NUMERIC =
      family(TypeFamily.BINARY, TypeFamily.NUMERIC);
  public static final IOperandTypeChecker BINARY_NUMERIC_BINARY =
      family(TypeFamily.BINARY, TypeFamily.NUMERIC, TypeFamily.BINARY);
  public static final IOperandTypeChecker BINARY_NUMERIC_NUMERIC_BINARY =
      family(TypeFamily.BINARY, TypeFamily.NUMERIC, TypeFamily.NUMERIC, TypeFamily.BINARY);

  public static final IOperandTypeChecker BINARY_OPTIONAL_TEXT =
      sequence(OperandTypes.BINARY, OperandTypes.TEXT).optional(i -> i == 1);

  public static final IOperandTypeChecker NUMERIC = family(TypeFamily.NUMERIC);
  public static final IOperandTypeChecker NUMERIC_NUMERIC =
      family(TypeFamily.NUMERIC, TypeFamily.NUMERIC);
  public static final IOperandTypeChecker NUMERIC_NUMERIC_NUMERIC =
      family(TypeFamily.NUMERIC, TypeFamily.NUMERIC, TypeFamily.NUMERIC);
  public static final IOperandTypeChecker NUMERIC_NUMERIC_NUMERIC_NUMERIC_NUMERIC_NUMERIC = family(TypeFamily.NUMERIC, TypeFamily.NUMERIC, TypeFamily.NUMERIC, TypeFamily.NUMERIC, TypeFamily.NUMERIC, TypeFamily.NUMERIC);

  public static final IOperandTypeChecker NUMERIC_OPTIONAL_STRING =
      family(TypeFamily.NUMERIC, TypeFamily.STRING).optional(i -> i == 1);
  public static final IOperandTypeChecker NUMERIC_OPTIONAL_NUMERIC =
      family(TypeFamily.NUMERIC, TypeFamily.NUMERIC).optional(i -> i == 1);
  public static final IOperandTypeChecker NUMERIC_OPTIONAL_TEXT =
      sequence(OperandTypes.NUMERIC, OperandTypes.TEXT).optional(i -> i == 1);
  public static final IOperandTypeChecker OPTIONAL_NUMERIC =
      family(TypeFamily.NUMERIC).optional(i -> i == 0);

  public static final IOperandTypeChecker TEMPORAL = family(TypeFamily.TEMPORAL);
  public static final IOperandTypeChecker TEMPORAL_TEMPORAL =
      family(TypeFamily.TEMPORAL, TypeFamily.TEMPORAL);
  public static final IOperandTypeChecker TEMPORAL_NUMERIC =
      family(TypeFamily.TEMPORAL, TypeFamily.NUMERIC);
  public static final IOperandTypeChecker TEMPORAL_STRING =
      family(TypeFamily.TEMPORAL, TypeFamily.STRING);
  public static final IOperandTypeChecker TEMPORAL_INTERVAL =
      family(TypeFamily.TEMPORAL, TypeFamily.INTERVAL);
  
  public static final IOperandTypeChecker INTERVAL = family(TypeFamily.INTERVAL);
  public static final IOperandTypeChecker INTERVAL_INTERVAL = family(TypeFamily.INTERVAL, TypeFamily.INTERVAL);
  public static final IOperandTypeChecker INTERVAL_TEMPORAL = family(TypeFamily.INTERVAL, TypeFamily.TEMPORAL);
  
  public static final IOperandTypeChecker TIMEUNIT_TEMPORAL = sequence(TIMEUNIT, TEMPORAL);
  public static final IOperandTypeChecker TIMEUNIT_TEMPORAL_TEMPORAL = sequence(TIMEUNIT, TEMPORAL, TEMPORAL);
  public static final IOperandTypeChecker TIMEUNIT_NUMERIC_TEMPORAL = sequence(TIMEUNIT, NUMERIC, TEMPORAL);
  public static final IOperandTypeChecker TIMEUNIT_INTERVAL = sequence(TIMEUNIT, INTERVAL);
  
  public static final IOperandTypeChecker STRING = family(TypeFamily.STRING);
  public static final IOperandTypeChecker STRING_VARIADIC =
      family(TypeFamily.STRING, OperandCountRange.from(1));
  public static final IOperandTypeChecker STRING_STRING_VARIADIC =
      family(TypeFamily.STRING, OperandCountRange.from(2));
  public static final IOperandTypeChecker STRING_STRING =
      family(TypeFamily.STRING, TypeFamily.STRING);
  public static final IOperandTypeChecker STRING_STRING_STRING =
      family(TypeFamily.STRING, TypeFamily.STRING, TypeFamily.STRING);
  public static final IOperandTypeChecker STRING_STRING_NUMERIC =
      family(TypeFamily.STRING, TypeFamily.STRING, TypeFamily.NUMERIC);
  public static final IOperandTypeChecker STRING_STRING_NUMERIC_NUMERIC =
      family(TypeFamily.STRING, TypeFamily.STRING, TypeFamily.NUMERIC, TypeFamily.NUMERIC);
  public static final IOperandTypeChecker STRING_NUMERIC =
      family(TypeFamily.STRING, TypeFamily.NUMERIC);
  public static final IOperandTypeChecker STRING_NUMERIC_NUMERIC =
      family(TypeFamily.STRING, TypeFamily.NUMERIC, TypeFamily.NUMERIC);
  public static final IOperandTypeChecker STRING_NUMERIC_NUMERIC_STRING =
      family(TypeFamily.STRING, TypeFamily.NUMERIC, TypeFamily.NUMERIC, TypeFamily.STRING);
  public static final IOperandTypeChecker STRING_OPTIONAL_NUMERIC =
      family(TypeFamily.STRING, TypeFamily.NUMERIC).optional(i -> i == 1);
  public static final IOperandTypeChecker STRING_OPTIONAL_STRING =
      family(TypeFamily.STRING, TypeFamily.STRING).optional(i -> i == 1);
  public static final IOperandTypeChecker STRING_NUMERIC_OPTIONAL_NUMERIC =
      family(TypeFamily.STRING, TypeFamily.NUMERIC, TypeFamily.NUMERIC).optional(i -> i == 2);
  public static final IOperandTypeChecker STRING_STRING_OPTIONAL_STRING =
      family(TypeFamily.STRING, TypeFamily.STRING, TypeFamily.STRING).optional(i -> i == 2);
  public static final IOperandTypeChecker STRING_STRING_OPTIONAL_NUMERIC =
      family(TypeFamily.STRING, TypeFamily.STRING, TypeFamily.NUMERIC).optional(i -> i == 2);
  public static final IOperandTypeChecker STRING_STRING_OPTIONAL_NUMERIC_NUMERIC =
      family(TypeFamily.STRING, TypeFamily.STRING, TypeFamily.NUMERIC, TypeFamily.NUMERIC)
          .optional(i -> i >= 2);
  public static final IOperandTypeChecker STRING_NUMERIC_OPTIONAL_STRING =
      family(TypeFamily.STRING, TypeFamily.NUMERIC, TypeFamily.STRING).optional(i -> i == 2);
  public static final IOperandTypeChecker STRING_STRING_OPTIONAL_NUMERIC_STRING =
      family(TypeFamily.STRING, TypeFamily.STRING, TypeFamily.NUMERIC, TypeFamily.STRING)
          .optional(i -> i >= 2);
  public static final IOperandTypeChecker STRING_STRING_OPTIONAL_NUMERIC_NUMERIC_STRING =
      family(TypeFamily.STRING, TypeFamily.STRING, TypeFamily.NUMERIC, TypeFamily.NUMERIC,
          TypeFamily.STRING).optional(i -> i >= 2);
  public static final IOperandTypeChecker STRING_STRING_OPTIONAL_NUMERIC_NUMERIC_NUMERIC_STRING =
      family(TypeFamily.STRING, TypeFamily.STRING, TypeFamily.NUMERIC, TypeFamily.NUMERIC,
          TypeFamily.NUMERIC, TypeFamily.STRING).optional(i -> i >= 2);
  public static final IOperandTypeChecker STRING_DATE =
      family(TypeFamily.STRING, TypeFamily.TEMPORAL);
  public static final IOperandTypeChecker STRING_STRING_TEMPORAL =
      family(TypeFamily.STRING, TypeFamily.STRING, TypeFamily.TEMPORAL);

  public static final IOperandTypeChecker TEMPORAL_TIMEUNIT = sequence(TEMPORAL, TIMEUNIT);
  public static final IOperandTypeChecker TEMPORAL_OPTIONAL_TEXT =
      sequence(TEMPORAL, TEXT).optional(i -> i == 1);
  public static final IOperandTypeChecker STRING_OPTIONAL_TEXT =
      sequence(STRING, TEXT).optional(i -> i == 1);


  
  public static final IOperandTypeChecker JSON = family(TypeFamily.JSON);
  public static final IOperandTypeChecker JSON_STRING = family(TypeFamily.JSON, TypeFamily.STRING);
  
  public static final IOperandTypeChecker CASE_OPERATOR = new CaseOperatorOperandTypeChecker();
  public static final IOperandTypeChecker CAST_OPERATOR = OperandTypes
      .sequence(OperandTypes.ANY, OperandTypes.DATATYPE, OperandTypes.TEXT).optional(i -> i == 2);
  public static final IOperandTypeChecker DECODE_FUNCTION = new DecodeFunctionOperandTypeChecker();
}
