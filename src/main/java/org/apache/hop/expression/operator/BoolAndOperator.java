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
import org.apache.hop.expression.Kind;
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
        "/docs/booland.html");
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

    final List<IExpression> strongTerms = new ArrayList<>();
    final List<IExpression> nullTerms = new ArrayList<>();
    final MultiValuedMap<IExpression, Pair<Call, IExpression>> notInTerms =
        new ArrayListValuedHashMap<>();
    final MultiValuedMap<IExpression, Pair<Call, IExpression>> inTerms =
        new ArrayListValuedHashMap<>();

    for (IExpression predicate : predicates) {

      // FALSE as soon as any predicate is FALSE
      if (predicate == Literal.FALSE) {
        return Literal.FALSE;
      }

      if (predicate instanceof Call term) {
        if (term.isOperator(IsNullOperator.INSTANCE)) {
          nullTerms.add(term.getOperand(0));
        }
        if (term.isOperator(EqualOperator.INSTANCE) && term.getOperand(1).isKind(Kind.LITERAL)) {
          inTerms.put(term.getOperand(0), Pair.of(term, term.getOperand(1)));
        }
        if (term.isOperator(NotEqualOperator.INSTANCE) && term.getOperand(1).isKind(Kind.LITERAL)) {
          notInTerms.put(term.getOperand(0), Pair.of(term, term.getOperand(1)));
        }
        // Only a list of constant values, an expression can be equal to any value
        if (term.isOperator(InListOperator.INSTANCE) && term.getOperand(1).isConstant()) {
          inTerms.put(term.getOperand(0), Pair.of(term, term.getOperand(1)));
        }
        if (term.isOperator(NotInListOperator.INSTANCE) && term.getOperand(1).isConstant()) {
          for (IExpression operand : array(term.getOperand(1))) {
            notInTerms.put(term.getOperand(0), Pair.of(term, operand));
          }
        }
        if (Strong.isStrong(term)) {
          if (term.getOperand(0).isKind(Kind.IDENTIFIER)) {
            strongTerms.add(term.getOperand(0));
          }
          if (term.getOperand(1).isKind(Kind.IDENTIFIER)) {
            strongTerms.add(term.getOperand(1));
          }
        }
      }
    }

    // Don't simplify IS NOT NULL(x) AND x<5 → x<5, the result is FALSE and not NULL if x is NULL

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

    // Simplify intersection
    // X=1 AND X=2 → NVL2(X,FALSE,NULL), NULL and not FALSE if X is NULL
    // X=1 AND X IN (2,3,4) → NVL2(X,FALSE,NULL)
    // X IN (1,2,3) AND X IN (4,5) → NVL2(X,FALSE,NULL)
    // X IN (1,2,3) AND X IN (2,3,4) → X IN (2,3)
    for (IExpression reference : inTerms.keySet()) {
      Collection<IExpression> values = new LinkedList<>();
      for (Pair<Call, IExpression> pair : inTerms.get(reference)) {
        IExpression term = pair.right();
        if (values.isEmpty()) {
          if (term.isKind(Kind.ARRAY)) {
            array(term).forEach(values::add);
          } else values.add(term);
        } else {
          if (term.isKind(Kind.ARRAY)) {
            values = CollectionUtils.intersection(values, array(term));
          } else {
            values = CollectionUtils.intersection(values, List.of(term));
          }
        }
        predicates.remove(pair.left());
      }

      // Remove exclusions NOT IN
      for (Pair<Call, IExpression> pair : notInTerms.get(reference)) {
        IExpression term = pair.right();
        if (values.remove(term)) {
          predicates.remove(pair.left());
        }
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

    // Simplify IS NULL(x) AND x<5 → NVL2(x,FALSE,NULL), FALSE if x is not NULL, but NULL and not
    // FALSE if x is NULL
    for (IExpression operand : nullTerms) {
      if (strongTerms.contains(operand)) {
        List<IExpression> unnecessary = new ArrayList<>();
        for (IExpression condition : predicates) {
          if ((condition.isOperator(IsNullOperator.INSTANCE)
                  && call(condition).getOperand(0).equals(operand))
              || (condition instanceof Call term
                  && Strong.isStrong(term)
                  && (term.getOperand(0).equals(operand) || term.getOperand(1).equals(operand)))) {
            unnecessary.add(condition);
          }
        }
        predicates.removeAll(unnecessary);
        predicates.add(nullIfNull(operand));
      }
    }

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
    operands[0].unparse(writer, getLeftPrec(), getRightPrec());
    writer.append(" AND ");
    operands[1].unparse(writer, getLeftPrec(), getRightPrec());
  }
}
