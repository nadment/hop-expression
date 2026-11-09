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
import java.util.PriorityQueue;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionComparator;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Types;
import org.apache.hop.expression.util.Disjunction;
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
        "/docs/hop-user-manual/expression/boolor.html");
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

    // Simplify x OR FALSE → x, removed before the loop to not change the queue while iterating it
    predicates.removeIf(Literal.FALSE::equals);
    if (predicates.isEmpty()) {
      return Literal.FALSE;
    }

    final Disjunction disjunction = new Disjunction(predicates);
    for (IExpression predicate : predicates) {

      // TRUE as soon as any predicate is TRUE
      if (predicate == Literal.TRUE) {
        return Literal.TRUE;
      }

      disjunction.add(predicate);
    }

    disjunction.simplifyAbsorption();
    disjunction.simplifyComplement();
    disjunction.simplifyRange();
    disjunction.mergeComparisons();
    disjunction.simplifyNotIn();
    disjunction.simplifyIn();
    disjunction.simplifyNotNull();

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
    // Short-circuit: TRUE OR x is TRUE whatever x, so the right operand is not evaluated
    Boolean left = operands[0].getValue(Boolean.class);
    if (Boolean.TRUE.equals(left)) {
      return Boolean.TRUE;
    }
    Boolean right = operands[1].getValue(Boolean.class);
    if (Boolean.TRUE.equals(right)) {
      return Boolean.TRUE;
    }

    // Neither operand is TRUE, the result is NULL if one of them is NULL
    if (left == null || right == null) {
      return null;
    }

    return Boolean.FALSE;
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, 0, getLeftPrecedence());
    writer.append(" OR ");
    operands[1].unparse(writer, getRightPrecedence(), 0);
  }
}
