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

import org.apache.hop.expression.operator.Add;
import org.apache.hop.expression.operator.AtTimeZone;
import org.apache.hop.expression.operator.Avg;
import org.apache.hop.expression.operator.Between;
import org.apache.hop.expression.operator.BetweenSymmetric;
import org.apache.hop.expression.operator.BitAnd;
import org.apache.hop.expression.operator.BitNot;
import org.apache.hop.expression.operator.BitOr;
import org.apache.hop.expression.operator.BitXor;
import org.apache.hop.expression.operator.BoolAnd;
import org.apache.hop.expression.operator.BoolNot;
import org.apache.hop.expression.operator.BoolOr;
import org.apache.hop.expression.operator.BoolXor;
import org.apache.hop.expression.operator.Case;
import org.apache.hop.expression.operator.Cast;
import org.apache.hop.expression.operator.Concat;
import org.apache.hop.expression.operator.Count;
import org.apache.hop.expression.operator.Div;
import org.apache.hop.expression.operator.Equal;
import org.apache.hop.expression.operator.Extract;
import org.apache.hop.expression.operator.GreaterThan;
import org.apache.hop.expression.operator.GreaterThanOrEqual;
import org.apache.hop.expression.operator.ILike;
import org.apache.hop.expression.operator.In;
import org.apache.hop.expression.operator.IsFalse;
import org.apache.hop.expression.operator.IsNotNull;
import org.apache.hop.expression.operator.IsNull;
import org.apache.hop.expression.operator.IsTrue;
import org.apache.hop.expression.operator.JsonObject;
import org.apache.hop.expression.operator.LessThan;
import org.apache.hop.expression.operator.LessThanOrEqual;
import org.apache.hop.expression.operator.LessThanOrGreaterThan;
import org.apache.hop.expression.operator.Like;
import org.apache.hop.expression.operator.Max;
import org.apache.hop.expression.operator.Min;
import org.apache.hop.expression.operator.Mod;
import org.apache.hop.expression.operator.Multiply;
import org.apache.hop.expression.operator.Negate;
import org.apache.hop.expression.operator.NotEqual;
import org.apache.hop.expression.operator.Percentile;
import org.apache.hop.expression.operator.Position;
import org.apache.hop.expression.operator.RLike;
import org.apache.hop.expression.operator.StdDev;
import org.apache.hop.expression.operator.Subtract;
import org.apache.hop.expression.operator.Sum;
import org.apache.hop.expression.operator.Variance;
import java.util.Set;
import java.util.TreeSet;

public class Operators {
  // -------------------------------------------------------------
  // BITWISE OPERATORS
  // -------------------------------------------------------------
  public static final Operator BITAND = new BitAnd();
  public static final Operator BITOR = new BitOr();
  public static final Operator BITNOT = new BitNot();
  public static final Operator BITXOR = new BitXor();

  // -------------------------------------------------------------
  // LOGICAL OPERATORS
  // -------------------------------------------------------------
  public static final Operator BOOLNOT = new BoolNot();
  public static final Operator BOOLOR = new BoolOr();
  public static final Operator BOOLAND = new BoolAnd();
  public static final Operator BOOLXOR = new BoolXor();

  // -------------------------------------------------------------
  // CONDITIONAL OPERATORS
  // -------------------------------------------------------------
  public static final Operator CASE = new Case();

  // -------------------------------------------------------------
  // COMPARISON OPERATORS
  // -------------------------------------------------------------
  public static final Operator IS_NULL = new IsNull();
  public static final Operator IS_NOT_NULL = new IsNotNull();
  public static final Operator IS_TRUE = new IsTrue();
  public static final Operator IS_FALSE = new IsFalse();
  public static final Operator IN = new In();
  public static final Operator LIKE = new Like();
  public static final Operator ILIKE = new ILike();
  public static final Operator RLIKE = new RLike();
  public static final Operator BETWEEN = new Between();
  public static final Operator BETWEEN_SYMMETRIC = new BetweenSymmetric();
  public static final Operator EQUAL = new Equal();
  public static final Operator NOT_EQUAL = new NotEqual();
  public static final Operator LESS_THAN_OR_GREATER_THAN = new LessThanOrGreaterThan();
  public static final Operator LESS_THAN = new LessThan();
  public static final Operator LESS_THAN_OR_EQUAL = new LessThanOrEqual();
  public static final Operator GREATER_THAN = new GreaterThan();
  public static final Operator GREATER_THAN_OR_EQUAL = new GreaterThanOrEqual();

  // -------------------------------------------------------------
  // ARITHMETIC OPERATORS
  // -------------------------------------------------------------
  public static final Operator NEGATIVE = new Negate();
  public static final Operator MULTIPLY = new Multiply();
  public static final Operator DIVIDE = new Div();
  public static final Operator MODULUS = new Mod();
  public static final Operator ADD = new Add();
  public static final Operator SUBTRACT = new Subtract();

  // -------------------------------------------------------------
  // DATE OPERATORS
  // -------------------------------------------------------------
  public static final Operator AT_TIME_ZONE = new AtTimeZone();
  
  // -------------------------------------------------------------
  // SPECIAL OPERATORS with custom syntax
  // -------------------------------------------------------------
  public static final Function CAST = new Cast();  
  public static final Function CONCAT = new Concat();
  public static final Function EXTRACT = new Extract();
  public static final Function POSITION = new Position();
  public static final Function JSON_OBJECT = new JsonObject();

  // -------------------------------------------------------------
  // AGGREGATOR OPERATORS
  // -------------------------------------------------------------
  public static final Aggregator AVG = new Avg();
  public static final Aggregator COUNT = new Count(false, false);
  public static final Aggregator COUNT_DISTINCT = new Count(true, false);
  public static final Aggregator COUNT_ROW = new Count(false, true);
  public static final Aggregator MAX = new Max();
  public static final Aggregator MIN = new Min();
  public static final Aggregator PERCENTILE = new Percentile();
  public static final Aggregator STDDEV = new StdDev();
  public static final Aggregator SUM = new Sum();
  public static final Aggregator VARIANCE = new Variance();

  /** Set of aggregators. */
  protected static final Set<Aggregator> SET_AGGREGATORS = Set.of(AVG, COUNT, MAX, MIN, PERCENTILE, STDDEV, SUM, VARIANCE);
  
  /** Set of scalar operators. */
  private static final Set<Operator> SET_OPERATORS = Set.of(ADD, SUBTRACT, MULTIPLY, DIVIDE, BITAND,
      BITOR, BITNOT, BITXOR, CAST, MODULUS, EQUAL, GREATER_THAN, GREATER_THAN_OR_EQUAL, ILIKE,
      LESS_THAN, LESS_THAN_OR_EQUAL, LESS_THAN_OR_GREATER_THAN, NOT_EQUAL, BOOLAND, BETWEEN, CASE,
      CONCAT, IN, IS_NULL, IS_FALSE, IS_TRUE, LIKE, RLIKE, BOOLNOT, BOOLOR);

  public static Set<Operator> getOperators() {
    Set<Operator> set = new TreeSet<>(SET_OPERATORS);
    set.addAll(FunctionRegistry.getFunctions());
    return set;
  }

  private Operators() {}
}
