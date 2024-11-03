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
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
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
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Pair;

/**
 * Logical conjunction <code>AND</code> operator.
 *
 * <p>If any of the arguments are false, result is false; else if any arguments are null, result is
 * null; else true.
 */
public class BoolAndOperator extends BinaryOperator {

  public BoolAndOperator() {
    super(
        "BOOLAND",
        "AND",
        160,
        true,
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

    // Get all chained predicates sorted
    // Remove duplicate predicate
    // x AND x → x
    // x AND y AND x → x AND y
    PriorityQueue<IExpression> predicates = new PriorityQueue<>(new ExpressionComparator());
    predicates.addAll(call.getChainedOperands(false));

    final List<IExpression> strongTerms = new ArrayList<>();
    final List<IExpression> nullTerms = new ArrayList<>();
    final List<IExpression> notNullTerms = new ArrayList<>();
    final Map<Pair<IExpression, IExpression>, Call> notEqualTerms = new HashMap<>();
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
        if (term.isOperator(Operators.IS_NULL)) {
          nullTerms.add(term.getOperand(0));
        }
        if (term.isOperator(Operators.IS_NOT_NULL)) {
          notNullTerms.add(term.getOperand(0));
        }
        if (term.isOperator(Operators.EQUAL)) {
          if (term.getOperand(1).is(Kind.LITERAL)) {
            inTerms.put(term.getOperand(0), Pair.of(term, term.getOperand(1)));
          }
        }
        if (term.isOperator(Operators.NOT_EQUAL)) {
          notEqualTerms.put(Pair.of(term.getOperand(0), term.getOperand(1)), term);
          if (term.getOperand(1).is(Kind.LITERAL)) {
            notInTerms.put(term.getOperand(0), Pair.of(term, term.getOperand(1).asLiteral()));
          }
        }
        if (term.isOperator(Operators.IN)) {
          inTerms.put(term.getOperand(0), Pair.of(term, term.getOperand(1)));
        }
        if (term.isOperator(Operators.NOT_IN) && term.getOperand(1).isConstant()) {
          for (IExpression operand : term.getOperand(1).asArray()) {
            notInTerms.put(term.getOperand(0), Pair.of(term, operand.asLiteral()));
          }
        }
        if (Operators.isStrong(term)) {
          if (term.getOperand(0).is(Kind.IDENTIFIER)) {
            strongTerms.add(term.getOperand(0));
          }
          if (term.getOperand(1).is(Kind.IDENTIFIER)) {
            strongTerms.add(term.getOperand(1));
          }
        }
      }
    }

    // Simplify IS NULL(x) AND x<5 → not satisfiable
    if (!Collections.disjoint(nullTerms, strongTerms)) {
      return Literal.FALSE;
    }

    // Simplify strong terms IS NOT NULL(x) AND x<5 → x<5
    for (IExpression operand : notNullTerms) {
      if (strongTerms.contains(operand)) {
        Iterator<IExpression> iterator = predicates.iterator();
        List<IExpression> unnecessary = new ArrayList<>();
        while (iterator.hasNext()) {
          IExpression condition = iterator.next();
          if (condition.isOperator(Operators.IS_NOT_NULL)
              && condition.asCall().getOperand(0).equals(operand)) {
            unnecessary.add(condition);
          }
        }
        predicates.removeAll(unnecessary);
      }
    }

    // Simplify union
    // X<>1 AND X<>2 → X NOT IN (1,2)
    // X<>1 AND X NOT IN (2,3) → X NOT IN (1,2,3)
    for (IExpression reference : notInTerms.keySet()) {
      Collection<Pair<Call, IExpression>> pairs = notInTerms.get(reference);
      if (pairs.size() > 1) {
        List<IExpression> values = new ArrayList<>();
        for (Pair<Call, IExpression> pair : pairs) {
          values.add(pair.getRight());
          predicates.remove(pair.getLeft());
        }
        Call predicate = new Call(Operators.NOT_IN, reference, new Array(values));
        predicate.inferReturnType();
        predicates.add(predicate);
      }
    }

    // Simplify intersection
    // X=1 AND X=2 → FALSE
    // X=1 AND X IN (2,3,4) → FALSE
    // X IN (1,2,3) AND X IN (4,5) → FALSE
    // X IN (1,2,3) AND X IN (2,3,4) → X IN (2,3)
    for (IExpression reference : inTerms.keySet()) {
      Collection<IExpression> values = new LinkedList<>();
      for (Pair<Call, IExpression> pair : inTerms.get(reference)) {
        IExpression term = pair.getRight();
        if (values.isEmpty()) {
          if (term.is(Kind.ARRAY)) {
            term.asArray().forEach(values::add);
          } else values.add(term);
        } else {
          if (term.is(Kind.ARRAY)) {
            values = CollectionUtils.intersection(values, term.asArray());
          } else {
            values = CollectionUtils.intersection(values, List.of(term));
          }
        }
        predicates.remove(pair.getLeft());
      }

      // Remove exclusions NOT IN
      for (Pair<Call, IExpression> pair : notInTerms.get(reference)) {
        IExpression term = pair.getRight();
        if (values.remove(term)) {
          predicates.remove(pair.getLeft());
        }
      }

      if (values.isEmpty()) {
        return Literal.FALSE;
      }

      if (values.size() == 1) {
        Call predicate = new Call(Operators.EQUAL, reference, values.iterator().next());
        predicate.inferReturnType();
        predicates.add(predicate);
      } else {
        Call predicate = new Call(Operators.IN, reference, new Array(values));
        predicate.inferReturnType();
        predicates.add(predicate);
      }
    }

    // Rebuild conjunctions if more than 1 condition
    if (predicates.size() == 1) {
      return predicates.peek();
    }
    IExpression expression = predicates.poll();
    while (!predicates.isEmpty()) {
      call = new Call(Operators.BOOLAND, expression, predicates.poll());
      call.inferReturnType();
      expression = call;
    }

    return expression;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Boolean left = operands[0].getValue(Boolean.class);
    if (left == Boolean.FALSE) {
      return Boolean.FALSE;
    }
    Boolean right = operands[1].getValue(Boolean.class);
    if (right == Boolean.FALSE) {
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
