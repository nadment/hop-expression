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
package org.apache.hop.expression.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.collections4.multimap.ArrayListValuedHashMap;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Identifier;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.operator.BoolAndOperator;
import org.apache.hop.expression.operator.BoolNotFunction;
import org.apache.hop.expression.operator.EqualOperator;
import org.apache.hop.expression.operator.GreaterThanOperator;
import org.apache.hop.expression.operator.GreaterThanOrEqualOperator;
import org.apache.hop.expression.operator.InListOperator;
import org.apache.hop.expression.operator.IsNotNullOperator;
import org.apache.hop.expression.operator.LessThanOperator;
import org.apache.hop.expression.operator.LessThanOrEqualOperator;
import org.apache.hop.expression.operator.NotEqualOperator;
import org.apache.hop.expression.operator.NotInListOperator;
import org.apache.hop.expression.operator.Nvl2Function;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** The predicates of a disjunction, with their terms grouped by kind to simplify them. */
@NullMarked
public final class Disjunction {
  private final PriorityQueue<IExpression> predicates;
  private final Map<IExpression, Call> notNullTerms = new HashMap<>();
  private final List<Identifier> identifiers = new ArrayList<>();
  private final Map<Pair<IExpression, IExpression>, Call> strongTerms = new HashMap<>();
  private final Map<Pair<IExpression, IExpression>, Call> equalTerms = new HashMap<>();
  private final Map<Pair<IExpression, IExpression>, Call> notEqualTerms = new HashMap<>();
  private final Map<Pair<IExpression, IExpression>, Call> lessThanTerms = new HashMap<>();
  private final Map<Pair<IExpression, IExpression>, Call> greaterThanTerms = new HashMap<>();
  private final MultiValuedMap<IExpression, Pair<Call, IExpression>> inTerms =
      new ArrayListValuedHashMap<>();
  private final MultiValuedMap<IExpression, Pair<Call, IExpression>> notInTerms =
      new ArrayListValuedHashMap<>();
  private final MultiValuedMap<IExpression, ComparisonBound> bounds =
      new ArrayListValuedHashMap<>();

  /**
   * Creates a disjunction that simplifies the given predicates in place.
   *
   * @param predicates the sorted predicates of the disjunction, modified by the simplifications
   */
  public Disjunction(PriorityQueue<IExpression> predicates) {
    this.predicates = predicates;
  }

  /** Registers a predicate in the groups of terms it belongs to. */
  public void add(IExpression predicate) {
    if (predicate instanceof Identifier identifier) {
      identifiers.add(identifier);
    } else if (predicate instanceof Call term) {
      addNotNull(term);
      addComparison(term);
      ComparisonBound bound = ComparisonBound.of(term);
      if (bound != null && bound.reference().isDeterministic()) {
        bounds.put(bound.reference(), bound);
      }
      addList(term);
      if (Strong.isStrong(term)) {
        strongTerms.put(Pair.of(term.getOperand(0), term.getOperand(1)), term);
      }
    }
  }

  private void addNotNull(Call term) {
    if (term.isOperator(IsNotNullOperator.INSTANCE)) {
      notNullTerms.put(term.getOperand(0), term);
    }
    // NVL2(x,TRUE,NULL) is a simplified x IS NOT NULL, it also dominates the strong terms of x
    // x IS NOT NULL OR x>5 OR x<2 → NVL2(x,TRUE,NULL) OR x<2 → NVL2(x,TRUE,NULL)
    if (term.isOperator(Nvl2Function.INSTANCE)
        && Literal.TRUE.equals(term.getOperand(1))
        && term.getOperand(2).isNull()) {
      notNullTerms.put(term.getOperand(0), term);
    }
  }

  private void addComparison(Call term) {
    if (term.isOperator(EqualOperator.INSTANCE)) {
      equalTerms.put(Pair.of(term.getOperand(0), term.getOperand(1)), term);
      if (term.getOperand(1) instanceof Literal) {
        inTerms.put(term.getOperand(0), Pair.of(term, term.getOperand(1)));
      }
    }
    if (term.isOperator(NotEqualOperator.INSTANCE)) {
      notEqualTerms.put(Pair.of(term.getOperand(0), term.getOperand(1)), term);
      notInTerms.put(term.getOperand(0), Pair.of(term, term.getOperand(1)));
    }
    if (term.isOperator(LessThanOperator.INSTANCE)) {
      lessThanTerms.put(Pair.of(term.getOperand(0), term.getOperand(1)), term);
    }
    if (term.isOperator(GreaterThanOperator.INSTANCE)) {
      greaterThanTerms.put(Pair.of(term.getOperand(0), term.getOperand(1)), term);
    }
  }

  private void addList(Call term) {
    if (term.isOperator(InListOperator.INSTANCE)
        && term.getOperand(1) instanceof Array list
        && list.isConstant()) {
      for (IExpression operand : list) {
        inTerms.put(term.getOperand(0), Pair.of(term, operand));
      }
    }
    if (term.isOperator(NotInListOperator.INSTANCE)) {
      notInTerms.put(term.getOperand(0), Pair.of(term, term.getOperand(1)));
    }
  }

  /** Removes the conjunctions absorbed by another predicate of the disjunction. */
  public void simplifyAbsorption() {
    // x OR (x AND y) → x, also if x is NULL: NULL OR (NULL AND y) is NULL
    List<IExpression> absorbed = new ArrayList<>();
    for (IExpression predicate : predicates) {
      if (predicate instanceof Call conjunction
          && conjunction.isOperator(BoolAndOperator.INSTANCE)
          && conjunction.getChainedOperands(false).stream().anyMatch(this::contains)) {
        absorbed.add(predicate);
      }
    }
    absorbed.forEach(predicates::remove);
  }

  /** Replaces a predicate and its negation by NVL2(x,TRUE,NULL). */
  public void simplifyComplement() {
    // x OR NOT x → NVL2(x,TRUE,NULL), TRUE if x is not NULL, but NULL if x is NULL
    for (IExpression predicate : List.copyOf(predicates)) {
      if (predicate instanceof Call negation
          && negation.isOperator(BoolNotFunction.INSTANCE)
          && contains(negation.getOperand(0))) {
        predicates.remove(predicate);
        predicates.remove(negation.getOperand(0));
        predicates.add(trueIfNotNull(negation.getOperand(0)));
      }
    }
  }

  /** Returns whether the disjunction contains a predicate that always evaluates the same. */
  private boolean contains(IExpression expression) {
    return predicates.stream().anyMatch(predicate -> predicate.isSameExpression(expression));
  }

  /** Keeps the least restrictive lower and upper bounds of the range of each expression. */
  public void simplifyRange() {
    // x>5 OR x>=3 → x>=3
    // x>1 OR x<5 → NVL2(x,TRUE,NULL), every value is in the range
    for (IExpression reference : bounds.keySet()) {
      ComparisonBound lower = leastRestrictive(bounds.get(reference), true);
      ComparisonBound upper = leastRestrictive(bounds.get(reference), false);
      if (lower != null && upper != null && lower.isFullRange(upper)) {
        predicates.remove(lower.term());
        predicates.remove(upper.term());
        predicates.add(trueIfNotNull(reference));
      }
    }
  }

  /** Returns the least restrictive bound of a side, the other bounds of this side are removed. */
  private @Nullable ComparisonBound leastRestrictive(
      Collection<ComparisonBound> bounds, boolean lower) {
    ComparisonBound result = null;
    for (ComparisonBound bound : bounds) {
      if (bound.lower() != lower) {
        continue;
      }
      if (result == null || result.isMoreRestrictive(bound)) {
        if (result != null) {
          predicates.remove(result.term());
        }
        result = bound;
      } else {
        predicates.remove(bound.term());
      }
    }
    return result;
  }

  /** Merges the comparisons of the same operands. */
  public void mergeComparisons() {
    // Merge OR comparison
    for (Map.Entry<Pair<IExpression, IExpression>, Call> entry : lessThanTerms.entrySet()) {
      Pair<IExpression, IExpression> pair = entry.getKey();

      // x<a OR x!=a → x!=a
      if (notEqualTerms.containsKey(pair)) {
        predicates.remove(entry.getValue());
      }

      // x<a OR x=a → x<=a
      if (equalTerms.containsKey(pair)) {
        predicates.remove(equalTerms.get(pair));
        predicates.remove(entry.getValue());
        predicates.add(new Call(LessThanOrEqualOperator.INSTANCE, pair.left(), pair.right()));
      }

      // x<a OR x>a → x!=a
      if (greaterThanTerms.containsKey(pair)) {
        predicates.remove(entry.getValue());
        predicates.remove(greaterThanTerms.get(pair));
        predicates.add(new Call(NotEqualOperator.INSTANCE, pair.left(), pair.right()));
      }
    }

    for (Map.Entry<Pair<IExpression, IExpression>, Call> entry : greaterThanTerms.entrySet()) {
      Pair<IExpression, IExpression> pair = entry.getKey();

      // x>a OR x!=a → x!=a
      if (notEqualTerms.containsKey(pair)) {
        predicates.remove(entry.getValue());
      }

      // x>a OR x=a → x>=a
      if (equalTerms.containsKey(pair)) {
        predicates.remove(equalTerms.get(pair));
        predicates.remove(entry.getValue());
        predicates.add(new Call(GreaterThanOrEqualOperator.INSTANCE, pair.left(), pair.right()));
      }
    }
  }

  /** Replaces the NOT IN and != of the same value by the intersection of their values. */
  public void simplifyNotIn() {
    // Simplify intersection NOT IN
    // X NOT IN (1,2,3) OR X NOT IN (2,3,4,5,6) → X NOT IN (2,3)
    for (IExpression reference : notInTerms.keySet()) {
      Collection<Pair<Call, IExpression>> pairs = notInTerms.get(reference);
      Collection<IExpression> values = intersection(pairs);

      // No value is excluded by all predicates, nothing to simplify
      // X NOT IN (1,2) OR X NOT IN (3,4) is TRUE for any value of X, except NULL
      if (values == null || values.isEmpty()) {
        continue;
      }

      for (Pair<Call, IExpression> pair : pairs) {
        predicates.remove(pair.left());
      }

      Call predicate;
      if (values.size() == 1) {
        predicate = new Call(NotEqualOperator.INSTANCE, reference, values.iterator().next());
      } else {
        predicate = new Call(NotInListOperator.INSTANCE, reference, new Array(values));
      }
      predicate.inferReturnType();
      predicates.add(predicate);
    }
  }

  /**
   * Returns the values common to all the terms, a term is a value or a list of values.
   *
   * @param pairs the predicates with their term
   * @return the common values, or null if there is no term
   */
  private static @Nullable Collection<IExpression> intersection(
      Collection<Pair<Call, IExpression>> pairs) {
    Collection<IExpression> values = null;
    for (Pair<Call, IExpression> pair : pairs) {
      IExpression term = pair.right();
      if (values == null) {
        values = new LinkedList<>();
        if (term instanceof Array array) {
          array.forEach(values::add);
        } else values.add(term);
      } else {
        if (term instanceof Array array) {
          values = CollectionUtils.intersection(values, array);
        } else {
          values = CollectionUtils.intersection(values, List.of(term));
        }
      }
    }
    return values;
  }

  /** Replaces the IN and = of the same value by the union of their values. */
  public void simplifyIn() {
    // Simplify union
    // X=1 OR X=2 → X IN (1,2)
    // X=1 OR X IN (2,3) → X IN (1,2,3)
    // Remove the values in the range of a comparison, the disjunction is already TRUE for them
    // X=3 OR X>1 → X>1
    // X IN (1,5) OR X>3 → X=1 OR X>3
    for (IExpression reference : inTerms.keySet()) {
      Collection<Pair<Call, IExpression>> pairs = inTerms.get(reference);
      Collection<ComparisonBound> range = bounds.get(reference);
      List<IExpression> values = new ArrayList<>();
      boolean changed = pairs.size() > 1;
      for (Pair<Call, IExpression> pair : pairs) {
        if (range.stream().anyMatch(bound -> Boolean.TRUE.equals(bound.includes(pair.right())))) {
          changed = true;
        } else {
          values.add(pair.right());
        }
      }
      if (!changed) {
        continue;
      }

      pairs.forEach(pair -> predicates.remove(pair.left()));
      if (values.size() == 1) {
        Call predicate = new Call(EqualOperator.INSTANCE, reference, values.getFirst());
        predicate.inferReturnType();
        predicates.add(predicate);
      } else if (values.size() > 1) {
        Call predicate = new Call(InListOperator.INSTANCE, reference, new Array(values));
        predicate.inferReturnType();
        predicates.add(predicate);
      }
    }
  }

  /** Removes the terms dominated by a x IS NOT NULL term. */
  public void simplifyNotNull() {
    // Simplify x OR x IS NOT NULL → NVL2(x,TRUE,NULL), the result is NULL and not FALSE if x is
    // NULL
    final List<IExpression> dominated = new ArrayList<>();
    for (IExpression identifier : identifiers) {
      if (notNullTerms.containsKey(identifier)) {
        predicates.remove(identifier);
        dominated.add(identifier);
      }
    }

    // Simplify IS NOT NULL(x) OR x<5 → NVL2(x,TRUE,NULL), the result is NULL and not FALSE if x
    // is NULL
    for (Map.Entry<Pair<IExpression, IExpression>, Call> entry : strongTerms.entrySet()) {
      Pair<IExpression, IExpression> pair = entry.getKey();
      if (notNullTerms.containsKey(pair.left())) {
        predicates.remove(entry.getValue());
        dominated.add(pair.left());
      } else if (notNullTerms.containsKey(pair.right())) {
        predicates.remove(entry.getValue());
        dominated.add(pair.right());
      }
    }

    // Replace x IS NOT NULL by NVL2(x,TRUE,NULL) once its dominated terms are removed. Alone, x
    // IS NOT NULL returns FALSE if x is NULL, while the removed terms made the disjunction NULL:
    // x IS NOT NULL OR x<5 is FALSE OR NULL → NULL. A term that is already NVL2(x,TRUE,NULL) is
    // kept as is.
    for (IExpression operand : dominated) {
      Call notNullTerm = notNullTerms.get(operand);
      if (notNullTerm != null
          && notNullTerm.isOperator(IsNotNullOperator.INSTANCE)
          && predicates.remove(notNullTerm)) {
        Call predicate =
            new Call(Nvl2Function.INSTANCE, operand, Literal.TRUE, Literal.NULL_BOOLEAN);
        predicate.inferReturnType();
        predicates.add(predicate);
      }
    }
  }

  /**
   * Returns {@code NVL2(x,TRUE,NULL)}, the value of a disjunction that is TRUE if x is not NULL and
   * NULL if x is NULL, like {@code x OR NOT x}, or TRUE if x is never NULL.
   */
  private static IExpression trueIfNotNull(IExpression operand) {
    if (!operand.getType().isNullable()) {
      return Literal.TRUE;
    }
    Call predicate = new Call(Nvl2Function.INSTANCE, operand, Literal.TRUE, Literal.NULL_BOOLEAN);
    predicate.inferReturnType();
    return predicate;
  }
}
