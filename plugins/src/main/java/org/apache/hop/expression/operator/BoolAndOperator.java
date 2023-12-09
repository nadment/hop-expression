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
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.ExpressionComparator;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Pair;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.PriorityQueue;

/**
 * Logical conjunction <code>AND</code> operator.
 * 
 * <p>
 * If any of the arguments are false, result is false;
 * else if any arguments are null, result is null;
 * else true.
 */
public class BoolAndOperator extends Operator {

  public BoolAndOperator() {
    super("BOOLAND", "AND", 160, true, ReturnTypes.BOOLEAN_NULLABLE, OperandTypes.BOOLEAN_BOOLEAN,
        OperatorCategory.LOGICAL, "/docs/booland.html");
  }

  /**
   * Simplifies AND expressions whose answer can be determined without evaluating both sides.
   */
  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Remove duplicate predicate
    // x AND x → x
    // x AND y AND x → x AND y
    PriorityQueue<IExpression> conditions = new PriorityQueue<>(new ExpressionComparator());
    conditions.addAll(this.getChainedOperands(call, false));

    // final Map<IExpression, Range<?>> ranges = new HashMap<>();
    final List<IExpression> isNullTerms = new ArrayList<>();
    final List<IExpression> isNotNullTerms = new ArrayList<>();
    final List<IExpression> strongTerms = new ArrayList<>();
    final MultiValuedMap<IExpression, Pair<Call, Literal>> equalsLiterals =
        new ArrayListValuedHashMap<>();

    for (IExpression condition : conditions) {
      
      // FALSE as soon as any predicate is FALSE
      if ( condition==Literal.FALSE ) {
        return Literal.FALSE;
      }
      
      if (condition.is(Kind.CALL)) {
        Call predicate = condition.asCall();
        if (predicate.is(Operators.IS_NULL)) {
          isNullTerms.add(predicate.getOperand(0));
        }
        if (predicate.is(Operators.IS_NOT_NULL)) {
          isNotNullTerms.add(predicate.getOperand(0));
        }
        if (predicate.is(Operators.EQUAL)) {
          if (predicate.getOperand(0).is(Kind.LITERAL)) {
            equalsLiterals.put(predicate.getOperand(1), Pair.of(predicate, predicate.getOperand(0).asLiteral()));
          }
          if (predicate.getOperand(1).is(Kind.LITERAL)) {
            equalsLiterals.put(predicate.getOperand(0), Pair.of(predicate, predicate.getOperand(1).asLiteral()));
          }
        }

        // if (call.is(Operators.LESS_THAN) && call.getOperand(0).isConstant() ) {
        //
        // this.processRange();
        //
        // Object value = call.getOperand(0).getValue();
        // if ( value instanceof Comparable ) {
        // Comparable<?> c = (Comparable<?>) value;
        // Range<?> range = ranges.get(call);
        //
        // if ( range==null) {
        // ranges.put(call, Range.lessThan(c));
        // }
        // }

        if (Operators.is(predicate, Operators.EQUAL, Operators.NOT_EQUAL, Operators.LESS_THAN,
            Operators.LESS_THAN_OR_EQUAL, Operators.LESS_THAN_OR_GREATER_THAN,
            Operators.GREATER_THAN, Operators.GREATER_THAN_OR_EQUAL)) {
          if (predicate.getOperand(0).is(Kind.IDENTIFIER)) {
            strongTerms.add(predicate.getOperand(0));
          }
          if (predicate.getOperand(1).is(Kind.IDENTIFIER)) {
            strongTerms.add(predicate.getOperand(1));
          }
        }
      }
    }

    // Simplify X=1 AND X=2 → not satisfiable
    for (IExpression reference : equalsLiterals.keySet()) {
      Collection<Pair<Call, Literal>> pairs = equalsLiterals.get(reference);
      Literal literal = null;
      for (Pair<Call, Literal> pair : pairs) {
        if (literal == null) {
          literal = pair.getRight();
        } else if (!literal.equals(pair.getRight())) {
          return Literal.FALSE;
        }
      }
    }

    // Simplify IS NULL(x) AND x<5 → not satisfiable
    if (!Collections.disjoint(isNullTerms, strongTerms)) {
      return Literal.FALSE;
    }

    // Remove not necessary IS NOT NULL expressions IS NOT NULL(x) AND x<5 → x<5
    for (IExpression operand : isNotNullTerms) {
      if (strongTerms.contains(operand)) {
        Iterator<IExpression> iterator = conditions.iterator();
        List<IExpression> unnecessary = new ArrayList<>();
        while (iterator.hasNext()) {
          IExpression condition = iterator.next();
          if (condition.is(Operators.IS_NOT_NULL)
              && condition.asCall().getOperand(0).equals(operand)) {
            unnecessary.add(condition);
          }
        }
        conditions.removeAll(unnecessary);
      }
    }

    // Rebuild conjunctions if more than 1 condition
    if (conditions.size() == 1) {
      return conditions.peek();
    }
    IExpression expression = conditions.poll();
    while (!conditions.isEmpty()) {
      expression = new Call(Operators.BOOLAND, expression, conditions.poll()).inferReturnType();
    }

    return expression;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Boolean left = operands[0].getValue(Boolean.class);
    if (left == Boolean.FALSE) {
      return left;
    }
    Boolean right = operands[1].getValue(Boolean.class);
    if (right == Boolean.FALSE) {
      return right;
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
    operands[0].unparse(writer);
    writer.append(" AND ");
    operands[1].unparse(writer);
  }
}
