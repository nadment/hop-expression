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
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.collections4.multimap.ArrayListValuedHashMap;
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
import org.apache.hop.expression.Tuple;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Pair;

/**
 * Logical disjunction <code>OR</code> operator.
 *
 * <p>If any of the arguments are true, result is true; else if any arguments are null, result is
 * null; else false.
 */
public class BoolOrOperator extends Operator {

  public BoolOrOperator() {
    super(
        "BOOLOR",
        "OR",
        180,
        true,
        ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.BOOLEAN_BOOLEAN,
        OperatorCategory.LOGICAL,
        "/docs/boolor.html");
  }

  /** Simplifies OR expressions whose answer can be determined without evaluating both sides. */
  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Get all chained predicates sorted
    // Remove duplicate predicates
    // x OR x → x
    // x OR y OR x → x OR y
    PriorityQueue<IExpression> predicates = new PriorityQueue<>(new ExpressionComparator());
    predicates.addAll(this.getChainedOperands(call, false));

    final List<IExpression> isNullTerms = new ArrayList<>();
    final List<IExpression> isNotNullTerms = new ArrayList<>();
    final List<IExpression> identifiers = new ArrayList<>();
    final Map<Pair<IExpression, IExpression>, Call> strongTerms = new HashMap<>();
    final Map<Pair<IExpression, IExpression>, Call> equalTerms = new HashMap<>();
    final Map<Pair<IExpression, IExpression>, Call> notEqualTerms = new HashMap<>();
    final Map<Pair<IExpression, IExpression>, Call> lessThanTerms = new HashMap<>();
    final Map<Pair<IExpression, IExpression>, Call> greaterThanTerms = new HashMap<>();
    final MultiValuedMap<IExpression, Pair<Call, Literal>> equalsLiterals =
        new ArrayListValuedHashMap<>();

    for (IExpression predicate : predicates) {

      // TRUE as soon as any predicate is TRUE
      if (predicate == Literal.TRUE) {
        return Literal.TRUE;
      }

      if (predicate.is(Kind.IDENTIFIER)) {
        identifiers.add(predicate);
      } else if (predicate.is(Kind.CALL)) {
        Call term = predicate.asCall();

        if (term.is(Operators.IS_NULL)) {
          isNullTerms.add(term.getOperand(0));
        }
        if (term.is(Operators.IS_NOT_NULL)) {
          isNotNullTerms.add(term.getOperand(0));
        }
        if (term.is(Operators.EQUAL)) {
          equalTerms.put(Pair.of(term.getOperand(0), term.getOperand(1)), term);

          if (term.getOperand(0).is(Kind.LITERAL)) {
            equalsLiterals.put(term.getOperand(1), Pair.of(term, term.getOperand(0).asLiteral()));
          }
          if (term.getOperand(1).is(Kind.LITERAL)) {
            equalsLiterals.put(term.getOperand(0), Pair.of(term, term.getOperand(1).asLiteral()));
          }
        }
        if (term.is(Operators.IN) && term.getOperand(1).isConstant()) {
          for (IExpression operand : term.getOperand(1).asTuple()) {
            equalsLiterals.put(term.getOperand(0), Pair.of(term, operand.asLiteral()));
          }
        }
        if (term.is(Operators.NOT_EQUAL)) {
          notEqualTerms.put(Pair.of(term.getOperand(0), term.getOperand(1)), term);
        }
        if (term.is(Operators.LESS_THAN)) {
          lessThanTerms.put(Pair.of(term.getOperand(0), term.getOperand(1)), term);
        }
        if (term.is(Operators.GREATER_THAN)) {
          greaterThanTerms.put(Pair.of(term.getOperand(0), term.getOperand(1)), term);
        }
        if (Operators.isStrong(term)) {
          strongTerms.put(Pair.of(term.getOperand(0), term.getOperand(1)), term);
          strongTerms.put(Pair.of(term.getOperand(0), term.getOperand(1)), term);
        }
      }
    }

    // Merge OR comparison
    for (Pair<IExpression, IExpression> pair : lessThanTerms.keySet()) {

      // x<a OR x!=a → x!=a
      if (notEqualTerms.containsKey(pair)) {
        predicates.remove(lessThanTerms.get(pair));
      }

      // x<a OR x=a → x<=a
      if (equalTerms.containsKey(pair)) {
        predicates.remove(equalTerms.get(pair));
        predicates.remove(lessThanTerms.get(pair));
        predicates.add(new Call(Operators.LESS_THAN_OR_EQUAL, pair.getLeft(), pair.getRight()));
      }

      // x<a OR x>a → x!=a
      if (greaterThanTerms.containsKey(pair)) {
        predicates.remove(lessThanTerms.get(pair));
        predicates.remove(greaterThanTerms.get(pair));
        predicates.add(new Call(Operators.NOT_EQUAL, pair.getLeft(), pair.getRight()));
      }
    }

    for (Pair<IExpression, IExpression> pair : greaterThanTerms.keySet()) {

      // x>a OR x!=a → x!=a
      if (notEqualTerms.containsKey(pair)) {
        predicates.remove(greaterThanTerms.get(pair));
      }

      // x>a OR x=a → x>=a
      if (equalTerms.containsKey(pair)) {
        predicates.remove(equalTerms.get(pair));
        predicates.remove(greaterThanTerms.get(pair));
        predicates.add(new Call(Operators.GREATER_THAN_OR_EQUAL, pair.getLeft(), pair.getRight()));
      }
    }

    // Simplify X=1 OR X=2 → X IN (1,2)
    // Simplify X=1 OR X IN (2,3) → X IN (1,2,3)
    for (IExpression reference : equalsLiterals.keySet()) {
      Collection<Pair<Call, Literal>> pairs = equalsLiterals.get(reference);
      if (pairs.size() > 1) {
        List<IExpression> values = new ArrayList<>();
        for (Pair<Call, Literal> pair : pairs) {
          values.add(pair.getRight());
          predicates.remove(pair.getLeft());
        }
        Call predicate = new Call(Operators.IN, reference, new Tuple(values));
        predicate.inferReturnType();
        predicates.add(predicate);
      }
    }

    // Simplify x OR NOT x → TRUE (if x is not nullable)
    // Simplify x OR NOT x → x IS NOT NULL OR NULL (if x is nullable)
    // Simplify x OR (x AND y) → x (if x not nullable)

    // Simplify x OR x IS NOT NULL → x IS NOT NULL
    for (IExpression identifier : identifiers) {
      if (isNotNullTerms.contains(identifier)) {
        predicates.remove(identifier);
      }
    }

    // Simplify IS NOT NULL(x) OR x<5 → IS NOT NULL(x)
    for (Pair<IExpression, IExpression> pair : strongTerms.keySet()) {
      if (isNotNullTerms.contains(pair.getLeft()) || isNotNullTerms.contains(pair.getRight())) {
        predicates.remove(strongTerms.get(pair));
      }
    }

    // Rebuild disjunctions if more than 1 condition
    if (predicates.size() == 1) {
      return predicates.peek();
    }
    IExpression expression = predicates.poll();
    while (!predicates.isEmpty()) {
      call = new Call(Operators.BOOLOR, expression, predicates.poll());
      call.inferReturnType();
      expression = call;
    }

    return expression;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Boolean left = operands[0].getValue(Boolean.class);
    Boolean right = operands[1].getValue(Boolean.class);

    if (left == null) {
      if (!right) return null;
      return right;
    }
    if (right == null) {
      if (!left) return null;
      return left;
    }
    return Boolean.logicalOr(left, right);
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append(" OR ");
    operands[1].unparse(writer);
  }
}
