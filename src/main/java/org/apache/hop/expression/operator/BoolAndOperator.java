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
package org.apache.hop.expression.operator;

import java.io.StringWriter;
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
import org.apache.hop.expression.ExpressionComparator;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Identifier;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Types;
import org.apache.hop.expression.util.Pair;
import org.apache.hop.expression.util.Strong;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Logical conjunction <code>AND</code> operator.
 *
 * <p>If any of the arguments are false, the result is false; else if any arguments are null, result
 * is null; else true.
 */
@NullMarked
public class BoolAndOperator extends BinaryOperator {

  public static final BoolAndOperator INSTANCE = new BoolAndOperator();

  public BoolAndOperator() {
    super(
        "BOOLAND",
        "AND",
        120,
        Associativity.LEFT,
        ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.BOOLEAN_BOOLEAN,
        OperatorCategory.LOGICAL,
        "/docs/hop-user-manual/expression/booland.html");
  }

  @Override
  public Operator reverse() {
    return this;
  }

  /** Simplifies AND expressions whose answer can be determined without evaluating both sides. */
  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Coerce numeric operands to boolean before simplification, x OR FALSE → x must be a boolean
    Types.coerceOperandsToBoolean(call);

    // Get all chained predicates sorted
    // Remove duplicate predicate
    // x AND x → x
    // x AND y AND x → x AND y
    PriorityQueue<IExpression> predicates = new PriorityQueue<>(new ExpressionComparator());
    predicates.addAll(call.getChainedOperands(false));

    // Simplify x AND TRUE → x, removed before the loop to not change the queue while iterating it
    predicates.removeIf(Literal.TRUE::equals);
    if (predicates.isEmpty()) {
      return Literal.TRUE;
    }

    final Conjunction conjunction = new Conjunction(predicates);
    for (IExpression predicate : predicates) {

      // FALSE as soon as any predicate is FALSE
      if (predicate == Literal.FALSE) {
        return Literal.FALSE;
      }

      if (predicate instanceof Call term) {
        conjunction.add(term);
      }
    }

    // Don't simplify IS NOT NULL(x) AND x<5 → x<5, the result is FALSE and not NULL if x is NULL

    conjunction.simplifyNotIn();
    conjunction.simplifyIn();
    conjunction.simplifyNull();

    // Rebuild conjunctions if more than 1 condition
    if (predicates.size() == 1) {
      return predicates.peek();
    }
    IExpression expression = predicates.poll();
    while (!predicates.isEmpty()) {
      call = new Call(BoolAndOperator.INSTANCE, expression, predicates.poll());
      call.inferReturnType();
      expression = call;
    }

    return expression;
  }

  /** The predicates of a conjunction, with their terms grouped by kind to simplify them. */
  private static final class Conjunction {
    private final PriorityQueue<IExpression> predicates;
    private final List<IExpression> strongTerms = new ArrayList<>();
    private final List<IExpression> nullTerms = new ArrayList<>();
    private final MultiValuedMap<IExpression, Pair<Call, IExpression>> notInTerms =
        new ArrayListValuedHashMap<>();
    private final MultiValuedMap<IExpression, Pair<Call, IExpression>> inTerms =
        new ArrayListValuedHashMap<>();

    Conjunction(PriorityQueue<IExpression> predicates) {
      this.predicates = predicates;
    }

    /** Registers a term in the groups of terms it belongs to. */
    void add(Call term) {
      if (isNullTerm(term)) {
        nullTerms.add(term.getOperand(0));
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

    /** Replaces the NOT IN and != of the same value by the union of their values. */
    void simplifyNotIn() {
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
    void simplifyIn() {
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
     * Returns the values common to all the terms and removes their predicates, a term is a value or
     * a list of values.
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
    void simplifyNull() {
      // Simplify IS NULL(x) AND x<5 → NVL2(x,FALSE,NULL), FALSE if x is not NULL, but NULL and not
      // FALSE if x is NULL
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
  }

  /**
   * Returns {@code NVL2(x,FALSE,NULL)}, the value of a conjunction that is FALSE if x is not NULL
   * and NULL if x is NULL, like {@code x=1 AND x=2}.
   */
  private static IExpression nullIfNull(IExpression operand) {
    Call predicate = new Call(Nvl2Function.INSTANCE, operand, Literal.FALSE, Literal.NULL_BOOLEAN);
    predicate.inferReturnType();
    return predicate;
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coerceOperandsToBoolean(call);
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    Boolean left = operands[0].getValue(Boolean.class);
    if (Boolean.FALSE.equals(left)) {
      return Boolean.FALSE;
    }
    Boolean right = operands[1].getValue(Boolean.class);
    if (Boolean.FALSE.equals(right)) {
      return Boolean.FALSE;
    }

    if (left == null || right == null) {
      return null;
    }

    return Boolean.logicalAnd(left, right);
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, 0, getLeftPrecedence());
    writer.append(" AND ");
    operands[1].unparse(writer, getRightPrecedence(), 0);
  }
}
