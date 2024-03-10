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
package org.apache.hop.expression;

import org.apache.hop.expression.operator.AddOperator;
import org.apache.hop.expression.operator.AtTimeZoneOperator;
import org.apache.hop.expression.operator.BetweenOperator;
import org.apache.hop.expression.operator.BetweenOperator.Between;
import org.apache.hop.expression.operator.BitAndFunction;
import org.apache.hop.expression.operator.BitNotFunction;
import org.apache.hop.expression.operator.BitOrFunction;
import org.apache.hop.expression.operator.BitXorFunction;
import org.apache.hop.expression.operator.BoolAndOperator;
import org.apache.hop.expression.operator.BoolNotOperator;
import org.apache.hop.expression.operator.BoolOrOperator;
import org.apache.hop.expression.operator.BoolXorOperator;
import org.apache.hop.expression.operator.CaseOperator;
import org.apache.hop.expression.operator.CaseOperator.When;
import org.apache.hop.expression.operator.CastFunction;
import org.apache.hop.expression.operator.CastOperator;
import org.apache.hop.expression.operator.CoalesceFunction;
import org.apache.hop.expression.operator.ConcatFunction;
import org.apache.hop.expression.operator.CountFunction;
import org.apache.hop.expression.operator.CountFunction.Count;
import org.apache.hop.expression.operator.DivOperator;
import org.apache.hop.expression.operator.ElementAtOperator;
import org.apache.hop.expression.operator.EqualOperator;
import org.apache.hop.expression.operator.ExtractFunction;
import org.apache.hop.expression.operator.GreaterThanOperator;
import org.apache.hop.expression.operator.GreaterThanOrEqualOperator;
import org.apache.hop.expression.operator.ILikeOperator;
import org.apache.hop.expression.operator.IfFunction;
import org.apache.hop.expression.operator.IfNullFunction;
import org.apache.hop.expression.operator.InOperator;
import org.apache.hop.expression.operator.IsDistinctFromOperator;
import org.apache.hop.expression.operator.IsFalseOperator;
import org.apache.hop.expression.operator.IsNotDistinctFromOperator;
import org.apache.hop.expression.operator.IsNotFalseOperator;
import org.apache.hop.expression.operator.IsNotNullOperator;
import org.apache.hop.expression.operator.IsNotTrueOperator;
import org.apache.hop.expression.operator.IsNullOperator;
import org.apache.hop.expression.operator.IsTrueOperator;
import org.apache.hop.expression.operator.LessThanOperator;
import org.apache.hop.expression.operator.LessThanOrEqualOperator;
import org.apache.hop.expression.operator.LikeOperator;
import org.apache.hop.expression.operator.ListAggFunction;
import org.apache.hop.expression.operator.ListAggFunction.ListAgg;
import org.apache.hop.expression.operator.ModFunction;
import org.apache.hop.expression.operator.MultiplyOperator;
import org.apache.hop.expression.operator.NegateOperator;
import org.apache.hop.expression.operator.NotEqualOperator;
import org.apache.hop.expression.operator.NullIfFunction;
import org.apache.hop.expression.operator.Nvl2Function;
import org.apache.hop.expression.operator.SimilarToOperator;
import org.apache.hop.expression.operator.SubtractOperator;
import java.util.Set;
import java.util.TreeSet;

public class Operators {
  // -------------------------------------------------------------
  // BITWISE OPERATORS
  // -------------------------------------------------------------
  public static final Operator BITAND = new BitAndFunction("&");
  public static final Operator BITOR = new BitOrFunction("|");
  public static final Operator BITNOT = new BitNotFunction("~");
  public static final Operator BITXOR = new BitXorFunction("^");

  // -------------------------------------------------------------
  // LOGICAL OPERATORS
  // -------------------------------------------------------------
  public static final Operator BOOLNOT = new BoolNotOperator();
  public static final Operator BOOLOR = new BoolOrOperator();
  public static final Operator BOOLXOR = new BoolXorOperator();
  public static final Operator BOOLAND = new BoolAndOperator();

  // -------------------------------------------------------------
  // CONDITIONAL OPERATORS
  // -------------------------------------------------------------
  public static final Operator CASE_SEARCH = new CaseOperator(When.SEARCH);
  public static final Operator CASE_SIMPLE = new CaseOperator(When.SIMPLE);
  public static final Operator COALESCE = new CoalesceFunction();
  public static final Operator IF = new IfFunction();
  public static final Operator IFNULL = new IfNullFunction();
  public static final Operator NULLIF = new NullIfFunction();
  public static final Operator NVL2 = new Nvl2Function();

  // -------------------------------------------------------------
  // COMPARISON OPERATORS
  // -------------------------------------------------------------
  public static final Operator IS_NULL = new IsNullOperator();
  public static final Operator IS_NOT_NULL = new IsNotNullOperator();
  public static final Operator IS_TRUE = new IsTrueOperator();
  public static final Operator IS_NOT_TRUE = new IsNotTrueOperator();
  public static final Operator IS_FALSE = new IsFalseOperator();
  public static final Operator IS_NOT_FALSE = new IsNotFalseOperator();
  public static final Operator IS_DISTINCT_FROM = new IsDistinctFromOperator();
  public static final Operator IS_NOT_DISTINCT_FROM = new IsNotDistinctFromOperator();
  public static final Operator SIMILAR_TO = new SimilarToOperator(false);
  public static final Operator NOT_SIMILAR_TO = new SimilarToOperator(true);
  public static final Operator IN = new InOperator(false);
  public static final Operator NOT_IN = new InOperator(true);
  public static final Operator LIKE = new LikeOperator();
  public static final Operator ILIKE = new ILikeOperator();
  public static final Operator BETWEEN_ASYMMETRIC = new BetweenOperator(Between.ASYMMETRIC);
  public static final Operator BETWEEN_SYMMETRIC = new BetweenOperator(Between.SYMMETRIC);
  public static final Operator EQUAL = new EqualOperator();
  public static final Operator NOT_EQUAL = new NotEqualOperator("!=");
  public static final Operator LESS_THAN_OR_GREATER_THAN = new NotEqualOperator("<>");
  public static final Operator LESS_THAN = new LessThanOperator();
  public static final Operator LESS_THAN_OR_EQUAL = new LessThanOrEqualOperator();
  public static final Operator GREATER_THAN = new GreaterThanOperator();
  public static final Operator GREATER_THAN_OR_EQUAL = new GreaterThanOrEqualOperator();

  // -------------------------------------------------------------
  // ARITHMETIC OPERATORS
  // -------------------------------------------------------------
  public static final Operator ADD = new AddOperator();
  public static final Operator SUBTRACT = new SubtractOperator();
  public static final Operator NEGATE = new NegateOperator();
  public static final Operator MULTIPLY = new MultiplyOperator();
  public static final Operator DIVIDE = new DivOperator();
  public static final Operator MODULUS = new ModFunction("%");  

  // -------------------------------------------------------------
  // DATE OPERATORS
  // -------------------------------------------------------------
  public static final Operator AT_TIME_ZONE = new AtTimeZoneOperator();

  // -------------------------------------------------------------
  // ARRAY OPERATORS
  // -------------------------------------------------------------
  public static final Operator ELEMENT_AT = new ElementAtOperator();

  // -------------------------------------------------------------
  // SPECIAL OPERATORS with custom or alternative syntax, or optimized
  // -------------------------------------------------------------
  public static final Operator CAST = new CastOperator();
  public static final Operator CAST_FUNCTION = new CastFunction();
  public static final Function CONCAT = new ConcatFunction("||");
  public static final Function EXTRACT = new ExtractFunction();

  // -------------------------------------------------------------
  // AGGREGATE FUNCTIONS with custom syntax
  // -------------------------------------------------------------
  public static final AggregateFunction COUNT_DISTINCT = new CountFunction(Count.DISTINCT);
  public static final AggregateFunction COUNT_VALUE = new CountFunction(Count.VALUE);
  public static final AggregateFunction COUNT_ALL = new CountFunction(Count.ALL);
  public static final AggregateFunction LISTAGG_ALL = new ListAggFunction(ListAgg.ALL);
  public static final AggregateFunction LISTAGG_DISTINCT = new ListAggFunction(ListAgg.DISTINCT);

  /** Set of scalar operators without NOT variation (IS_NOT_TRUE, NOT_SIMILAR_TO...). */
  private static final Set<Operator> SET_OPERATORS =
      Set.of(ADD, SUBTRACT, MULTIPLY, DIVIDE, BITAND, BITOR, BITNOT, BITXOR, CAST, MODULUS, ELEMENT_AT, EQUAL,
          GREATER_THAN, GREATER_THAN_OR_EQUAL, ILIKE, LESS_THAN, LESS_THAN_OR_EQUAL,
          LESS_THAN_OR_GREATER_THAN, NOT_EQUAL, BOOLAND, BETWEEN_ASYMMETRIC, CASE_SEARCH, CONCAT, IN,
          IS_DISTINCT_FROM, IS_NULL, SIMILAR_TO, IS_FALSE, IS_TRUE, LIKE, BOOLNOT, BOOLOR, BOOLXOR);

  public static Set<Operator> getOperators() {
    Set<Operator> set = new TreeSet<>(new OperatorComparator());
    set.addAll(SET_OPERATORS);
    set.addAll(FunctionRegistry.getFunctions());
    return set;
  }

  public static boolean is(IExpression expression, Operator... operators) {
    for (Operator o : operators) {
      if (expression.is(o)) {
        return true;
      }
    }
    return false;
  }

  public static boolean isStrong(IExpression expression) {
    return is(expression, EQUAL, NOT_EQUAL, LESS_THAN, LESS_THAN_OR_EQUAL, LESS_THAN_OR_GREATER_THAN, GREATER_THAN, GREATER_THAN_OR_EQUAL);
  }
  
  private Operators() {
    // Utility class    
  }
}
