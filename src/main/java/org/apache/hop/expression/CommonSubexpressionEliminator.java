/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression;

import java.util.BitSet;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.function.UnaryOperator;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Replaces the calls found several times in compiled expressions by a {@link SharedExpression}, so
 * they are evaluated at most once per state of the context, so once per row with a {@link
 * IRowExpressionContext}.
 *
 * <p>Only the deterministic calls that are not constant are shared. The evaluation stays lazy: a
 * shared call in a branch that is not evaluated, like in a CASE, is still not evaluated.
 *
 * <p>Usage example, to share the calls between the expressions of several fields:
 *
 * <pre>{@code
 * IExpression[] expressions = ...; // compiled with the same context
 * CommonSubexpressionEliminator.eliminate(context, expressions, new BitSet());
 * }</pre>
 */
@NullMarked
public final class CommonSubexpressionEliminator {

  /** The equal calls found in the expressions. */
  private static final class Group {
    private int count;
    private @Nullable SharedExpression shared;
  }

  private final IExpressionContext context;
  private final BitSet modifiedOrdinals;

  /** The group of each distinct call. */
  private final Map<IExpression, Group> groups = new HashMap<>();

  /**
   * The group of each call instance, by identity as the calls are modified when their operands are
   * replaced.
   */
  private final Map<IExpression, Group> callGroups = new IdentityHashMap<>();

  private CommonSubexpressionEliminator(IExpressionContext context, BitSet modifiedOrdinals) {
    this.context = context;
    this.modifiedOrdinals = modifiedOrdinals;
  }

  /**
   * Shares the calls found several times in an expression.
   *
   * @param context the context used to compile the expression
   * @param expression the compiled expression
   * @return the expression with its common sub-expressions shared
   */
  public static IExpression eliminate(IExpressionContext context, IExpression expression) {
    IExpression[] expressions = {expression};
    eliminate(context, expressions, new BitSet());
    return expressions[0];
  }

  /**
   * Shares the calls found several times in expressions evaluated for the same state of the
   * context, like the same row. The expressions are replaced in the array.
   *
   * @param context the context used to compile the expressions
   * @param expressions the compiled expressions, a {@code null} element is ignored
   * @param modifiedOrdinals the index of the fields modified in the row between the evaluation of
   *     two expressions, a call reading one of these fields is not shared
   */
  public static void eliminate(
      IExpressionContext context, @Nullable IExpression[] expressions, BitSet modifiedOrdinals) {

    // The operands of an aggregate function are evaluated by the aggregate processor
    for (IExpression expression : expressions) {
      if (expression != null && containsAggregate(expression)) {
        return;
      }
    }

    CommonSubexpressionEliminator eliminator =
        new CommonSubexpressionEliminator(context, modifiedOrdinals);

    // The calls already shared in an expression can be shared with the other expressions
    for (int i = 0; i < expressions.length; i++) {
      IExpression expression = expressions[i];
      if (expression != null) {
        expression = unshare(expression);
        expressions[i] = expression;
        eliminator.count(expression);
      }
    }

    for (int i = 0; i < expressions.length; i++) {
      IExpression expression = expressions[i];
      if (expression != null) {
        expressions[i] = eliminator.share(expression, 1);
      }
    }
  }

  private static boolean containsAggregate(IExpression expression) {
    if (expression instanceof SharedExpression shared) {
      return containsAggregate(shared.getExpression());
    }
    if (expression instanceof Call call) {
      if (call.getOperator().isAggregate()) {
        return true;
      }
      for (IExpression operand : call.getOperands()) {
        if (containsAggregate(operand)) {
          return true;
        }
      }
    } else if (expression instanceof Array array) {
      for (IExpression element : array) {
        if (containsAggregate(element)) {
          return true;
        }
      }
    }
    return false;
  }

  /** Replaces the shared expressions by the expression they share. */
  private static IExpression unshare(IExpression expression) {
    if (expression instanceof SharedExpression shared) {
      return unshare(shared.getExpression());
    }
    return replaceChildren(expression, CommonSubexpressionEliminator::unshare);
  }

  /**
   * Applies a function to the operands of a call or the elements of an array.
   *
   * @return the expression, or a new array if an element is replaced
   */
  private static IExpression replaceChildren(
      IExpression expression, UnaryOperator<IExpression> function) {
    if (expression instanceof Call call) {
      for (int i = 0; i < call.getOperandCount(); i++) {
        call.setOperand(i, function.apply(call.getOperand(i)));
      }
    } else if (expression instanceof Array array) {
      IExpression[] elements = new IExpression[array.size()];
      boolean changed = false;
      for (int i = 0; i < elements.length; i++) {
        elements[i] = function.apply(array.get(i));
        changed |= elements[i] != array.get(i);
      }
      if (changed) {
        return new Array(array.getType(), elements);
      }
    }
    return expression;
  }

  /** Counts the calls that can be shared. */
  private void count(IExpression expression) {
    if (expression instanceof Call call) {
      if (isShareable(call) && !call.isConstant()) {
        Group group = groups.computeIfAbsent(call, key -> new Group());
        group.count++;
        callGroups.put(call, group);
      }
      for (IExpression operand : call.getOperands()) {
        count(operand);
      }
    } else if (expression instanceof Array array) {
      for (IExpression element : array) {
        count(element);
      }
    }
  }

  /**
   * Checks if the value of an expression is the same each time it is evaluated for a row.
   *
   * @param expression the expression to check
   * @return {@code true} if the value can be shared
   */
  private boolean isShareable(IExpression expression) {
    if (expression instanceof Literal) {
      return true;
    }
    if (expression instanceof Field field) {
      return !modifiedOrdinals.get(field.getOrdinal());
    }
    if (expression instanceof Call call) {
      Operator operator = call.getOperator();
      if (!operator.isDeterministic() || operator.isAggregate()) {
        return false;
      }
      for (IExpression operand : call.getOperands()) {
        if (!isShareable(operand)) {
          return false;
        }
      }
      return true;
    }
    if (expression instanceof Array array) {
      for (IExpression element : array) {
        if (!isShareable(element)) {
          return false;
        }
      }
      return true;
    }
    return false;
  }

  /**
   * Replaces the calls found several times by a shared expression.
   *
   * @param expression the expression to process
   * @param count the number of times the enclosing shared call is found, a call found only inside
   *     it is already evaluated once
   * @return the expression to use
   */
  private IExpression share(IExpression expression, int count) {
    Group group = callGroups.get(expression);
    if (group != null && group.count > count) {
      SharedExpression shared = group.shared;
      if (shared == null) {
        shared = new SharedExpression(context, expression);
        group.shared = shared;
        replaceChildren(expression, child -> share(child, group.count));
      }
      return shared;
    }
    return replaceChildren(expression, child -> share(child, count));
  }
}
