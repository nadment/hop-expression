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

import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.collections4.multimap.ArrayListValuedHashMap;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.Category;
import org.apache.hop.expression.ExpressionComparator;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.Tuple;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Pair;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;

/**
 * Logical disjunction <code>OR</code> operator.
 * 
 * <p>
 * If any of the arguments are true, result is true;
 * else if any arguments are null, result is null;
 * else false.
 */
public class BoolOrOperator extends Operator {

  public BoolOrOperator() {
    super("BOOLOR", "OR", 180, true, ReturnTypes.BOOLEAN_NULLABLE, OperandTypes.BOOLEAN_VARIADIC,
        Category.LOGICAL, "/docs/boolor.html");
  }

  /**
   * Simplifies OR expressions whose answer can be determined without evaluating both sides.
   */
  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Simplify A OR A IS NOT NULL → A IS NOT NULL
    if (call.getOperand(1).is(Operators.IS_NOT_NULL)) {
      if (call.getOperand(0).equals(call.getOperand(1).asCall().getOperand(0))) {
        return call.getOperand(1);
      }
    }

    // Get all chained predicates sorted  
    // Remove duplicate predicates
    // x OR x → x
    // x OR y OR x → x OR y
    PriorityQueue<IExpression> conditions = new PriorityQueue<>(new ExpressionComparator());
    conditions.addAll(this.getChainedOperands(call, false));
    
    final Map<Pair<IExpression, IExpression>, Call> equalTerms = new HashMap<>();
    final Map<Pair<IExpression, IExpression>, Call> notEqualTerms = new HashMap<>();
    final Map<Pair<IExpression, IExpression>, Call> lessThanTerms = new HashMap<>();
    final Map<Pair<IExpression, IExpression>, Call> greaterThanTerms = new HashMap<>();
    final MultiValuedMap<IExpression, Pair<Call, Literal>> equalsLiterals =
        new ArrayListValuedHashMap<>();

    for (IExpression condition : conditions) {
      
      // TRUE as soon as any predicate is TRUE
      if ( condition==Literal.TRUE ) {
        return Literal.TRUE;
      }
      
      if (condition.is(Kind.CALL)) {
        Call predicate = condition.asCall();
        if (predicate.is(Operators.EQUAL)) {
          equalTerms.put(Pair.of(predicate.getOperand(0), predicate.getOperand(1)), predicate);

          if (predicate.getOperand(0).is(Kind.LITERAL)) {
            equalsLiterals.put(predicate.getOperand(1), Pair.of(predicate, predicate.getOperand(0).asLiteral()));
          }
          if (predicate.getOperand(1).is(Kind.LITERAL)) {
            equalsLiterals.put(predicate.getOperand(0), Pair.of(predicate, predicate.getOperand(1).asLiteral()));
          }
        }
        if (predicate.is(Operators.IN) && predicate.getOperand(1).isConstant()) {
          for (IExpression operand : predicate.getOperand(1).asTuple()) {
            equalsLiterals.put(predicate.getOperand(0), Pair.of(predicate, operand.asLiteral()));
          }
        }
        if (predicate.is(Operators.NOT_EQUAL)) {
          notEqualTerms.put(Pair.of(predicate.getOperand(0), predicate.getOperand(1)), predicate);
        }
        if (predicate.is(Operators.LESS_THAN)) {
          lessThanTerms.put(Pair.of(predicate.getOperand(0), predicate.getOperand(1)), predicate);
        }
        if (predicate.is(Operators.GREATER_THAN)) {
          greaterThanTerms.put(Pair.of(predicate.getOperand(0), predicate.getOperand(1)), predicate);
        }
      }
    }

    // Merge OR comparison
    for (Pair<IExpression, IExpression> pair : lessThanTerms.keySet()) {

      // x<a OR x!=a → x!=a
      if (notEqualTerms.containsKey(pair)) {
        conditions.remove(lessThanTerms.get(pair));
      }

      // x<a OR x=a → x<=a
      if (equalTerms.containsKey(pair)) {
        conditions.remove(equalTerms.get(pair));
        conditions.remove(lessThanTerms.get(pair));
        conditions.add(new Call(Operators.LESS_THAN_OR_EQUAL, pair.getLeft(), pair.getRight()));
      }

      // x<a OR x>a → x!=a
      if (greaterThanTerms.containsKey(pair)) {
        conditions.remove(lessThanTerms.get(pair));
        conditions.remove(greaterThanTerms.get(pair));
        conditions.add(new Call(Operators.NOT_EQUAL, pair.getLeft(), pair.getRight()));
      }
    }

    for (Pair<IExpression, IExpression> pair : greaterThanTerms.keySet()) {

      // x>a OR x!=a → x!=a
      if (notEqualTerms.containsKey(pair)) {
        conditions.remove(greaterThanTerms.get(pair));
      }

      // x>a OR x=a → x>=a
      if (equalTerms.containsKey(pair)) {
        conditions.remove(equalTerms.get(pair));
        conditions.remove(greaterThanTerms.get(pair));
        conditions.add(new Call(Operators.GREATER_THAN_OR_EQUAL, pair.getLeft(), pair.getRight()));
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
          conditions.remove(pair.getLeft());
        }
        conditions.add(new Call(Operators.IN, reference, new Tuple(values)).inferReturnType());
      }
    }

    // X <> A OR X <> B → X IS NOT NULL or NULL

    // Rebuild disjunctions if more than 1 condition
    if (conditions.size() == 1) {
      return conditions.peek();
    }
    IExpression expression = conditions.poll();
    while (!conditions.isEmpty()) {
      expression = new Call(Operators.BOOLOR, expression, conditions.poll()).inferReturnType();
    }
    
    return expression;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Boolean left = operands[0].getValue(Boolean.class);
    Boolean right = operands[1].getValue(Boolean.class);

    if (left == null) {
      if (!right)
        return null;
      return right;
    }
    if (right == null) {
      if (!left)
        return null;
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
