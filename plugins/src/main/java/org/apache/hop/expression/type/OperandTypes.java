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
import java.util.function.Predicate;

/**
 * Strategies for checking operand types.
 */
public final class OperandTypes {
  /**
   * Creates a checker that passes if each operand is a member of a
   * corresponding family.
   */
  public static FamilyOperandTypeChecker family(DataTypeFamily... families) {
    return new FamilyOperandTypeChecker(List.of(families), i -> false);
  }

  /**
   * Creates a checker that passes if each operand is a member of a
   * corresponding family.
   */
  public static FamilyOperandTypeChecker family(List<DataTypeFamily> families) {
    return family(families, i -> false);
  }

  /**
   * Creates a checker that passes if each operand is a member of a
   * corresponding family, and allows specified parameters to be optional.
   */
  public static FamilyOperandTypeChecker family(List<DataTypeFamily> families,
      Predicate<Integer> optional) {
    return new FamilyOperandTypeChecker(families, optional);
  }

  /**
   * Creates a checker that passes if all operand is a member of a
   * corresponding family.
   */
  public static FamilyOperandTypeChecker family(DataTypeFamily family, IOperandCountRange range) {
    return new FamilyOperandTypeChecker(family, range);
  }
  
  /**
   * Creates a checker that passes if any one of the rules passes.
   */
  public static IOperandTypeChecker or(IOperandTypeChecker... rules) {
    return new CompositeOperandTypeChecker(CompositeOperandTypeChecker.Composition.OR,
        List.of(rules));
  }

  /**
   * Creates a checker that passes if all of the rules pass.
   */
  public static IOperandTypeChecker and(IOperandTypeChecker... rules) {
    return new CompositeOperandTypeChecker(CompositeOperandTypeChecker.Composition.AND,
        List.of(rules));
  }

  /**
   * Creates an operand checker from a sequence of single-operand checkers.
   */
  public static IOperandTypeChecker sequence(IOperandTypeChecker... rules) {
    return new CompositeOperandTypeChecker(CompositeOperandTypeChecker.Composition.SEQUENCE,
        List.of(rules));
  }


  /**
   * Operand type-checking strategy for an operator which takes no operands.
   */
  public static final IOperandTypeChecker NILADIC = family();
  /**
   * Operand type-checking strategy type must be a non-NULL literal.
   */
  public static final IOperandTypeChecker LITERAL = new LiteralOperandTypeChecker(false);

  public static final IOperandTypeChecker ANY = family(DataTypeFamily.ANY);
  public static final IOperandTypeChecker ANY_ANY = family(DataTypeFamily.ANY,DataTypeFamily.ANY);
  public static final IOperandTypeChecker ANY_ANY_ANY = family(DataTypeFamily.ANY,DataTypeFamily.ANY,DataTypeFamily.ANY);
  
  /**
   * Operand type-checking strategy where two operands must both be in the
   * same type family.
   */
  public static final IOperandTypeChecker SAME_SAME = new SameOperandTypeChecker(OperandCountRange.of(2));
  
  /**
   * Operand type-checking strategy where three operands must both be in the
   * same type family.
   */  
  public static final IOperandTypeChecker SAME_SAME_SAME = new SameOperandTypeChecker(OperandCountRange.of(3));
  /**
   * Operand type-checking strategy where any number of operands must all be
   * in the same type family.
   */
  public static final IOperandTypeChecker SAME_VARIADIC = new SameOperandTypeChecker(OperandCountRange.any());
  
  /**
   * Operand type-checking strategy where any positive number of operands must all be
   * in the same type family.
   */
  public static final IOperandTypeChecker AT_LEAST_ONE_SAME_VARIADIC = new SameOperandTypeChecker(OperandCountRange.between(1, -1));
  public static final IOperandTypeChecker AT_LEAST_TREE_VARIADIC = new SameOperandTypeChecker(OperandCountRange.between(3, -1));

  public static final IOperandTypeChecker BOOLEAN = family(DataTypeFamily.BOOLEAN);
  public static final IOperandTypeChecker BOOLEAN_BOOLEAN = family(DataTypeFamily.BOOLEAN, DataTypeFamily.BOOLEAN);
  // TODO: Create BOOLEAN_SAME_SAME
  public static final IOperandTypeChecker BOOLEAN_ANY_ANY = family(DataTypeFamily.BOOLEAN, DataTypeFamily.ANY, DataTypeFamily.ANY);
  //public static final IOperandTypeChecker BOOLEAN_SAME_SAME = family(List.of(BOOLEAN, SAME_SAME));
  
  public static final IOperandTypeChecker BINARY = family(DataTypeFamily.BINARY);
  public static final IOperandTypeChecker BINARY_VARIADIC =family(DataTypeFamily.BINARY, OperandCountRange.between(1, -1));  
  public static final IOperandTypeChecker BINARY_BINARY = family(DataTypeFamily.BINARY, DataTypeFamily.BINARY);
  public static final IOperandTypeChecker BINARY_NUMERIC = family(DataTypeFamily.BINARY, DataTypeFamily.NUMERIC);
  
  public static final IOperandTypeChecker NUMERIC = family(DataTypeFamily.NUMERIC);
  public static final IOperandTypeChecker NUMERIC_NUMERIC = family(DataTypeFamily.NUMERIC,DataTypeFamily.NUMERIC);
  public static final IOperandTypeChecker NUMERIC_NUMERIC_NUMERIC = family(DataTypeFamily.NUMERIC,DataTypeFamily.NUMERIC,DataTypeFamily.NUMERIC);
  public static final IOperandTypeChecker NUMERIC_OPTIONAL_STRING = family(List.of(DataTypeFamily.NUMERIC, DataTypeFamily.STRING), i -> i == 1);
  public static final IOperandTypeChecker NUMERIC_OPTIONAL_NUMERIC = family(List.of(DataTypeFamily.NUMERIC, DataTypeFamily.NUMERIC), i -> i == 1);
  public static final IOperandTypeChecker OPTIONAL_NUMERIC = family(List.of(DataTypeFamily.NUMERIC), i -> i == 0);
  
  public static final IOperandTypeChecker DATE = family(DataTypeFamily.DATE);
  public static final IOperandTypeChecker DATE_DATETIME = family(DataTypeFamily.DATE, DataTypeFamily.DATE);
  public static final IOperandTypeChecker DATE_NUMERIC = family(DataTypeFamily.DATE, DataTypeFamily.NUMERIC);
  public static final IOperandTypeChecker DATE_STRING = family(DataTypeFamily.DATE, DataTypeFamily.STRING);
  public static final IOperandTypeChecker DATE_OPTIONAL_STRING = family(List.of(DataTypeFamily.DATE, DataTypeFamily.STRING), i -> i == 1);
  
  // TODO: enforce check to DatePart, not only to literal
  public static final IOperandTypeChecker DATE_DATEPART = sequence(DATE,LITERAL);
  public static final IOperandTypeChecker DATE_OPTIONAL_DATEPART = or(DATE_DATEPART,DATE);
    
 // public static final IOperandTypeChecker INTEGER = family(DataTypeFamily.INTEGER);
 // public static final IOperandTypeChecker INTEGER_INTEGER = family(DataTypeFamily.INTEGER, DataTypeFamily.INTEGER);
 // public static final IOperandTypeChecker INTEGER_INTEGER_INTEGER = family(DataTypeFamily.INTEGER, DataTypeFamily.INTEGER, DataTypeFamily.INTEGER);
 // public static final IOperandTypeChecker OPTIONAL_INTEGER = family(List.of(DataTypeFamily.INTEGER), i -> i == 0);
  
  public static final IOperandTypeChecker STRING = family(DataTypeFamily.STRING);
  public static final IOperandTypeChecker STRING_VARIADIC = family(DataTypeFamily.STRING, OperandCountRange.between(1, -1));  
  public static final IOperandTypeChecker STRING_OR_BINARY = or(family(DataTypeFamily.STRING), family(DataTypeFamily.BINARY));
  public static final IOperandTypeChecker STRING_STRING = family(DataTypeFamily.STRING, DataTypeFamily.STRING);
  public static final IOperandTypeChecker STRING_STRING_STRING = family(DataTypeFamily.STRING, DataTypeFamily.STRING, DataTypeFamily.STRING);
  public static final IOperandTypeChecker STRING_STRING_NUMERIC = family(DataTypeFamily.STRING, DataTypeFamily.STRING, DataTypeFamily.NUMERIC);
  public static final IOperandTypeChecker STRING_STRING_NUMERIC_NUMERIC = family(DataTypeFamily.STRING, DataTypeFamily.STRING, DataTypeFamily.NUMERIC, DataTypeFamily.NUMERIC);
  public static final IOperandTypeChecker STRING_NUMERIC = family(DataTypeFamily.STRING, DataTypeFamily.NUMERIC);
  public static final IOperandTypeChecker STRING_NUMERIC_NUMERIC = family(DataTypeFamily.STRING, DataTypeFamily.NUMERIC, DataTypeFamily.NUMERIC);
  public static final IOperandTypeChecker STRING_OPTIONAL_NUMERIC = family(List.of(DataTypeFamily.STRING, DataTypeFamily.NUMERIC), i -> i == 1);
  public static final IOperandTypeChecker STRING_OPTIONAL_STRING = family(List.of(DataTypeFamily.STRING, DataTypeFamily.STRING), i -> i == 1);
  public static final IOperandTypeChecker STRING_NUMERIC_OPTIONAL_NUMERIC = family(List.of(DataTypeFamily.STRING, DataTypeFamily.NUMERIC, DataTypeFamily.NUMERIC), i -> i == 2); 
  public static final IOperandTypeChecker STRING_NUMERIC_OR_BINARY_NUMERIC = or(STRING_NUMERIC, BINARY_NUMERIC);  
  public static final IOperandTypeChecker STRING_STRING_OPTIONAL_STRING = family(List.of(DataTypeFamily.STRING, DataTypeFamily.STRING, DataTypeFamily.STRING), i -> i == 2);  
  public static final IOperandTypeChecker STRING_STRING_OPTIONAL_NUMERIC = family(List.of(DataTypeFamily.STRING, DataTypeFamily.STRING, DataTypeFamily.NUMERIC), i -> i == 2);
  public static final IOperandTypeChecker STRING_NUMERIC_OPTIONAL_STRING = family(List.of(DataTypeFamily.STRING, DataTypeFamily.NUMERIC, DataTypeFamily.STRING), i -> i == 2);
  public static final IOperandTypeChecker STRING_STRING_OR_BINARY_BINARY = or(STRING_STRING, BINARY_BINARY);
  
  // TO_CHAR
  public static final IOperandTypeChecker NUMERIC_OPTIONAL_STRING_OR_DATETIME_OPTIONAL_STRING = or(NUMERIC_OPTIONAL_STRING, DATE_OPTIONAL_STRING);

  // STRTOK
  public static final IOperandTypeChecker CUSTOM_STRTOK = or(STRING, STRING_STRING, STRING_NUMERIC, STRING_STRING_NUMERIC);

  // REGEXP_REPLACE
  public static final IOperandTypeChecker CUSTOM_REGEXP_REPLACE = family(List.of(DataTypeFamily.STRING, DataTypeFamily.STRING, DataTypeFamily.STRING, DataTypeFamily.NUMERIC, DataTypeFamily.NUMERIC, DataTypeFamily.STRING), i -> i >= 2);

  
  public static final IOperandTypeChecker JSON = family(DataTypeFamily.JSON);
  public static final IOperandTypeChecker JSON_STRING = family(DataTypeFamily.JSON, DataTypeFamily.STRING);
  public static final IOperandTypeChecker JSON_STRING_OR_STRING_STRING = or(JSON_STRING, STRING_STRING);
  
  
  public static final IOperandTypeChecker NO_CHECK = new NoneOperandTypeChecker();
}
