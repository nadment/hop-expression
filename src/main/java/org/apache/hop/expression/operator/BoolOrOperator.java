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
import org.apache.hop.expression.ExpressionComparator;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Identifier;
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
 * Logical disjunction <code>OR</code> operator.
 *
 * <p>If any of the arguments are true, result is true; else if any arguments are null, result is
 * null; else false.
 */
@NullMarked
public class BoolOrOperator extends BinaryOperator {

  public static final BoolOrOperator INSTANCE = new BoolOrOperator();

  public BoolOrOperator() {
    super(
        "BOOLOR",
        "OR",
        140,
        Associativity.LEFT,
        ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.BOOLEAN_BOOLEAN,
        OperatorCategory.LOGICAL,
        "/docs/boolor.html");
  }

  @Override
  public Operator reverse() {
    return this;
  }

  /** Simplifies OR expressions whose answer can be determined without evaluating both sides. */
  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Coerce numeric operands to boolean before simplification, x OR FALSE → x must be a boolean
    Types.coerceOperandsToBoolean(call);

    // Get all chained predicates sorted
    // Remove duplicate predicates
    // x OR x → x
    // x OR y OR x → x OR y
    PriorityQueue<IExpression> predicates = new PriorityQueue<>(new ExpressionComparator());
    predicates.addAll(call.getChainedOperands(false));

    final Map<IExpression, Call> notNullTerms = new HashMap<>();
    final List<Identifier> identifiers = new ArrayList<>();
    final Map<Pair<IExpression, IExpression>, Call> strongTerms = new HashMap<>();
    final Map<Pair<IExpression, IExpression>, Call> equalTerms = new HashMap<>();
    final Map<Pair<IExpression, IExpression>, Call> notEqualTerms = new HashMap<>();
    final Map<Pair<IExpression, IExpression>, Call> lessThanTerms = new HashMap<>();
    final Map<Pair<IExpression, IExpression>, Call> greaterThanTerms = new HashMap<>();
    final MultiValuedMap<IExpression, Pair<Call, IExpression>> inTerms =
        new ArrayListValuedHashMap<>();
    final MultiValuedMap<IExpression, Pair<Call, IExpression>> notInTerms =
        new ArrayListValuedHashMap<>();

    for (IExpression predicate : predicates) {

      // TRUE as soon as any predicate is TRUE
      if (predicate == Literal.TRUE) {
        return Literal.TRUE;
      }

      // Simplify x OR FALSE → x
      if (predicate == Literal.FALSE && predicates.size() > 1) {
        predicates.remove(predicate);
      }

      if (predicate instanceof Identifier identifier) {
        identifiers.add(identifier);
      } else if (predicate instanceof Call term) {
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
        if (term.isOperator(EqualOperator.INSTANCE)) {
          equalTerms.put(Pair.of(term.getOperand(0), term.getOperand(1)), term);
          if (term.getOperand(1).is(Kind.LITERAL)) {
            inTerms.put(term.getOperand(0), Pair.of(term, term.getOperand(1)));
          }
        }
        if (term.isOperator(InListOperator.INSTANCE) && term.getOperand(1).isConstant()) {
          for (IExpression operand : array(term.getOperand(1))) {
            inTerms.put(term.getOperand(0), Pair.of(term, operand));
          }
        }
        if (term.isOperator(NotInListOperator.INSTANCE)) {
          notInTerms.put(term.getOperand(0), Pair.of(term, term.getOperand(1)));
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
        if (Strong.isStrong(term)) {
          strongTerms.put(Pair.of(term.getOperand(0), term.getOperand(1)), term);
        }
      }
    }

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

    // Simplify intersection NOT IN
    // X NOT IN (1,2,3) OR X NOT IN (2,3,4,5,6) → X NOT IN (2,3)
    for (IExpression reference : notInTerms.keySet()) {
      Collection<Pair<Call, IExpression>> pairs = notInTerms.get(reference);
      Collection<IExpression> values = null;
      for (Pair<Call, IExpression> pair : pairs) {
        IExpression term = pair.right();
        if (values == null) {
          values = new LinkedList<>();
          if (term.is(Kind.ARRAY)) {
            array(term).forEach(values::add);
          } else values.add(term);
        } else {
          if (term.is(Kind.ARRAY)) {
            values = CollectionUtils.intersection(values, array(term));
          } else {
            values = CollectionUtils.intersection(values, List.of(term));
          }
        }
      }

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

    // Simplify union
    // X=1 OR X=2 → X IN (1,2)
    // X=1 OR X IN (2,3) → X IN (1,2,3)
    for (IExpression reference : inTerms.keySet()) {
      Collection<Pair<Call, IExpression>> pairs = inTerms.get(reference);
      if (pairs.size() > 1) {
        List<IExpression> values = new ArrayList<>();
        for (Pair<Call, IExpression> pair : pairs) {
          values.add(pair.right());
          predicates.remove(pair.left());
        }
        Call predicate = new Call(InListOperator.INSTANCE, reference, new Array(values));
        predicate.inferReturnType();
        predicates.add(predicate);
      }
    }

    // Simplify x OR NOT x → TRUE (if x is not nullable)
    // Simplify x OR NOT x → x IS NOT NULL OR NULL (if x is nullable)
    // Simplify x OR (x AND y) → x (if x not nullable)

    // Simplify x OR x IS NOT NULL → NVL2(x,TRUE,NULL), the result is NULL and not FALSE if x is
    // NULL
    final List<IExpression> dominated = new ArrayList<>();
    for (IExpression identifier : identifiers) {
      if (notNullTerms.containsKey(identifier)) {
        predicates.remove(identifier);
        dominated.add(identifier);
      }
    }

    // Simplify IS NOT NULL(x) OR x<5 → NVL2(x,TRUE,NULL), the result is NULL and not FALSE if x is
    // NULL
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

    // Replace x IS NOT NULL by NVL2(x,TRUE,NULL) once its dominated terms are removed. Alone, x IS
    // NOT NULL returns FALSE if x is NULL, while the removed terms made the disjunction NULL:
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

    // Rebuild disjunctions if more than 1 condition
    if (predicates.size() == 1) {
      return predicates.peek();
    }
    IExpression expression = predicates.poll();
    while (!predicates.isEmpty()) {
      call = new Call(this, expression, predicates.poll());
      call.inferReturnType();
      expression = call;
    }

    return expression;
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coerceOperandsToBoolean(call);
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    Boolean left = operands[0].getValue(Boolean.class);
    Boolean right = operands[1].getValue(Boolean.class);

    if (left == null) {
      if (right == null || !right) return null;
      return Boolean.TRUE;
    }
    if (right == null) {
      if (!left) return null;
      return Boolean.TRUE;
    }
    return Boolean.logicalOr(left, right);
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, getLeftPrec(), getRightPrec());
    writer.append(" OR ");
    operands[1].unparse(writer, getLeftPrec(), getRightPrec());
  }
}
