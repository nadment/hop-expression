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
import java.util.LinkedList;
import java.util.List;
import java.util.PriorityQueue;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.collections4.multimap.ArrayListValuedHashMap;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Identifier;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.operator.BoolNotFunction;
import org.apache.hop.expression.operator.BoolOrOperator;
import org.apache.hop.expression.operator.EqualOperator;
import org.apache.hop.expression.operator.InListOperator;
import org.apache.hop.expression.operator.IsNullOperator;
import org.apache.hop.expression.operator.NotEqualOperator;
import org.apache.hop.expression.operator.NotInListOperator;
import org.apache.hop.expression.operator.Nvl2Function;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** The predicates of a conjunction, with their terms grouped by kind to simplify them. */
@NullMarked
public final class Conjunction {
  private final PriorityQueue<IExpression> predicates;
  private final List<IExpression> strongTerms = new ArrayList<>();
  private final List<IExpression> nullTerms = new ArrayList<>();
  private final MultiValuedMap<IExpression, Pair<Call, IExpression>> notInTerms =
      new ArrayListValuedHashMap<>();
  private final MultiValuedMap<IExpression, Pair<Call, IExpression>> inTerms =
      new ArrayListValuedHashMap<>();
  private final MultiValuedMap<IExpression, ComparisonBound> bounds =
      new ArrayListValuedHashMap<>();

  /**
   * Creates a conjunction that simplifies the given predicates in place.
   *
   * @param predicates the sorted predicates of the conjunction, modified by the simplifications
   */
  public Conjunction(PriorityQueue<IExpression> predicates) {
    this.predicates = predicates;
  }

  /** Registers a term in the groups of terms it belongs to. */
  public void add(Call term) {
    if (isNullTerm(term)) {
      nullTerms.add(term.getOperand(0));
    }
    ComparisonBound bound = ComparisonBound.of(term);
    if (bound != null && bound.reference().isDeterministic()) {
      bounds.put(bound.reference(), bound);
    }
    addInOrNotIn(term);
    if (Strong.isStrong(term)) {
      if (term.getOperand(0) instanceof Identifier) {
        strongTerms.add(term.getOperand(0));
      }
      if (term.getOperand(1) instanceof Identifier) {
        strongTerms.add(term.getOperand(1));
      }
    }
  }

  private void addInOrNotIn(Call term) {
    if (term.isOperator(EqualOperator.INSTANCE) && term.getOperand(1) instanceof Literal) {
      inTerms.put(term.getOperand(0), Pair.of(term, term.getOperand(1)));
    }
    if (term.isOperator(NotEqualOperator.INSTANCE) && term.getOperand(1) instanceof Literal) {
      notInTerms.put(term.getOperand(0), Pair.of(term, term.getOperand(1)));
    }
    // Only a list of constant values, an expression can be equal to any value
    if (term.isOperator(InListOperator.INSTANCE) && term.getOperand(1).isConstant()) {
      inTerms.put(term.getOperand(0), Pair.of(term, term.getOperand(1)));
    }
    if (term.isOperator(NotInListOperator.INSTANCE)
        && term.getOperand(1) instanceof Array list
        && list.isConstant()) {
      for (IExpression operand : list) {
        notInTerms.put(term.getOperand(0), Pair.of(term, operand));
      }
    }
  }

  /** Removes the disjunctions absorbed by another predicate of the conjunction. */
  public void simplifyAbsorption() {
    // x AND (x OR y) → x, also if x is NULL: NULL AND (NULL OR y) is NULL
    List<IExpression> absorbed = new ArrayList<>();
    for (IExpression predicate : predicates) {
      if (predicate instanceof Call disjunction
          && disjunction.isOperator(BoolOrOperator.INSTANCE)
          && disjunction.getChainedOperands(false).stream().anyMatch(this::contains)) {
        absorbed.add(predicate);
      }
    }
    absorbed.forEach(predicates::remove);
  }

  /** Replaces a predicate and its negation by NVL2(x,FALSE,NULL). */
  public void simplifyComplement() {
    // x AND NOT x → NVL2(x,FALSE,NULL), FALSE if x is not NULL, but NULL if x is NULL
    for (IExpression predicate : List.copyOf(predicates)) {
      if (predicate instanceof Call negation
          && negation.isOperator(BoolNotFunction.INSTANCE)
          && contains(negation.getOperand(0))) {
        predicates.remove(predicate);
        predicates.remove(negation.getOperand(0));
        predicates.add(nullIfNull(negation.getOperand(0)));
      }
    }
  }

  /** Returns whether the conjunction contains a predicate that always evaluates the same. */
  private boolean contains(IExpression expression) {
    return predicates.stream().anyMatch(predicate -> predicate.isSameExpression(expression));
  }

  /** Keeps the most restrictive lower and upper bounds of the range of each expression. */
  public void simplifyRange() {
    // x>5 AND x>=3 → x>5
    // x<1 AND x>5 → NVL2(x,FALSE,NULL), no value is in the range
    for (IExpression reference : bounds.keySet()) {
      ComparisonBound lower = mostRestrictive(bounds.get(reference), true);
      ComparisonBound upper = mostRestrictive(bounds.get(reference), false);
      if (lower != null && upper != null && lower.isEmptyRange(upper)) {
        predicates.remove(lower.term());
        predicates.remove(upper.term());
        predicates.add(nullIfNull(reference));
      }
    }
  }

  /** Returns the most restrictive bound of a side, the other bounds of this side are removed. */
  private @Nullable ComparisonBound mostRestrictive(
      Collection<ComparisonBound> bounds, boolean lower) {
    ComparisonBound result = null;
    for (ComparisonBound bound : bounds) {
      if (bound.lower() != lower) {
        continue;
      }
      if (result == null || bound.isMoreRestrictive(result)) {
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

  /** Replaces the NOT IN and != of the same value by the union of their values. */
  public void simplifyNotIn() {
    // Simplify union
    // X<>1 AND X<>2 → X NOT IN (1,2)
    // X<>1 AND X NOT IN (2,3) → X NOT IN (1,2,3)
    for (IExpression reference : notInTerms.keySet()) {
      Collection<Pair<Call, IExpression>> pairs = notInTerms.get(reference);
      if (pairs.size() > 1) {
        List<IExpression> values = new ArrayList<>();
        for (Pair<Call, IExpression> pair : pairs) {
          values.add(pair.right());
          predicates.remove(pair.left());
        }
        Call predicate = new Call(NotInListOperator.INSTANCE, reference, new Array(values));
        predicate.inferReturnType();
        predicates.add(predicate);
      }
    }
  }

  /** Replaces the IN and = of the same value by the intersection of their values. */
  public void simplifyIn() {
    // Simplify intersection
    // X=1 AND X=2 → NVL2(X,FALSE,NULL), NULL and not FALSE if X is NULL
    // X=1 AND X IN (2,3,4) → NVL2(X,FALSE,NULL)
    // X IN (1,2,3) AND X IN (4,5) → NVL2(X,FALSE,NULL)
    // X IN (1,2,3) AND X IN (2,3,4) → X IN (2,3)
    for (IExpression reference : inTerms.keySet()) {
      Collection<IExpression> values = intersection(inTerms.get(reference));

      // Remove exclusions NOT IN, their condition is then implied by the remaining values, also
      // for a value not in the list: X IN (1,2,3) AND X<>4 → X IN (1,2,3)
      for (Pair<Call, IExpression> pair : notInTerms.get(reference)) {
        values.remove(pair.right());
        predicates.remove(pair.left());
      }

      // Remove the values out of the range of the comparisons, the comparisons are then implied
      // by the remaining values: X=3 AND X>1 → X=3, X IN (1,5) AND X>3 → X=5,
      // X=3 AND X>5 → NVL2(X,FALSE,NULL). Only if all the values can be compared.
      Collection<ComparisonBound> range = bounds.get(reference);
      if (!range.isEmpty()
          && values.stream()
              .allMatch(value -> range.stream().allMatch(bound -> bound.includes(value) != null))) {
        values.removeIf(
            value -> range.stream().anyMatch(bound -> Boolean.FALSE.equals(bound.includes(value))));
        range.forEach(bound -> predicates.remove(bound.term()));
      }

      if (values.isEmpty()) {
        predicates.add(nullIfNull(reference));
      } else if (values.size() == 1) {
        Call predicate = new Call(EqualOperator.INSTANCE, reference, values.iterator().next());
        predicate.inferReturnType();
        predicates.add(predicate);
      } else {
        Call predicate = new Call(InListOperator.INSTANCE, reference, new Array(values));
        predicate.inferReturnType();
        predicates.add(predicate);
      }
    }
  }

  /**
   * Returns the values common to all the terms and removes their predicates, a term is a value or a
   * list of values.
   *
   * @param pairs the predicates with their term
   * @return the common values
   */
  private Collection<IExpression> intersection(Collection<Pair<Call, IExpression>> pairs) {
    // Null until the first term, an empty intersection must stay empty
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
      predicates.remove(pair.left());
    }
    return values == null ? new LinkedList<>() : values;
  }

  /** Replaces x IS NULL and the strong terms of x by NVL2(x,FALSE,NULL). */
  public void simplifyNull() {
    // Simplify IS NULL(x) AND x<5 → NVL2(x,FALSE,NULL), FALSE if x is not NULL, but NULL and not
    // FALSE if x is NULL
    // Don't simplify IS NOT NULL(x) AND x<5 → x<5, the result is FALSE and not NULL if x is NULL
    for (IExpression operand : nullTerms) {
      if (strongTerms.contains(operand)) {
        List<IExpression> unnecessary = new ArrayList<>();
        for (IExpression condition : predicates) {
          if (isNullOrStrongTerm(condition, operand)) {
            unnecessary.add(condition);
          }
        }
        predicates.removeAll(unnecessary);
        predicates.add(nullIfNull(operand));
      }
    }
  }

  /** Returns whether the condition is x IS NULL or a strong term of x. */
  private static boolean isNullOrStrongTerm(IExpression condition, IExpression operand) {
    return condition instanceof Call term
        && ((isNullTerm(term) && term.getOperand(0).equals(operand))
            || (Strong.isStrong(term)
                && (term.getOperand(0).equals(operand) || term.getOperand(1).equals(operand))));
  }

  /**
   * Returns whether the term is x IS NULL, or NVL2(x,FALSE,NULL) which is a simplified x IS NULL
   * AND x<5 that also dominates the strong terms of x.
   *
   * <p>x=1 AND x=2 AND x=3 → NVL2(x,FALSE,NULL) AND x=3 → NVL2(x,FALSE,NULL)
   */
  private static boolean isNullTerm(Call term) {
    return term.isOperator(IsNullOperator.INSTANCE)
        || (term.isOperator(Nvl2Function.INSTANCE)
            && Literal.FALSE.equals(term.getOperand(1))
            && term.getOperand(2).isNull());
  }

  /**
   * Returns {@code NVL2(x,FALSE,NULL)}, the value of a conjunction that is FALSE if x is not NULL
   * and NULL if x is NULL, like {@code x=1 AND x=2}, or FALSE if x is never NULL.
   */
  private static IExpression nullIfNull(IExpression operand) {
    if (!operand.getType().isNullable()) {
      return Literal.FALSE;
    }
    Call predicate = new Call(Nvl2Function.INSTANCE, operand, Literal.FALSE, Literal.NULL_BOOLEAN);
    predicate.inferReturnType();
    return predicate;
  }
}
