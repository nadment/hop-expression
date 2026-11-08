/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.apache.hop.expression.util;

import org.apache.hop.expression.Call;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.operator.GreaterThanOperator;
import org.apache.hop.expression.operator.GreaterThanOrEqualOperator;
import org.apache.hop.expression.operator.LessThanOperator;
import org.apache.hop.expression.operator.LessThanOrEqualOperator;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * A comparison of an expression with a constant value, the lower or upper bound of a range of
 * values, like {@code x>1} or {@code x<=5}. A comparison with the constant on the left is reversed,
 * {@code 1<x} is the lower bound {@code x>1}.
 *
 * @param term the comparison
 * @param reference the compared expression
 * @param value the constant value
 * @param lower {@code true} for a lower bound ({@code x>v} or {@code x>=v}), {@code false} for an
 *     upper bound
 * @param inclusive {@code true} if the value is included ({@code x>=v} or {@code x<=v})
 */
@NullMarked
public record ComparisonBound(
    Call term, IExpression reference, Object value, boolean lower, boolean inclusive) {

  /**
   * Returns the bound of a comparison with a constant, or null if the term is not such a comparison
   * or if its values can't be ordered.
   *
   * @param term the term of a conjunction or a disjunction
   * @return the bound, or null
   */
  public static @Nullable ComparisonBound of(Call term) {
    boolean greater = term.isOperator(GreaterThanOperator.INSTANCE);
    boolean greaterOrEqual = term.isOperator(GreaterThanOrEqualOperator.INSTANCE);
    boolean less = term.isOperator(LessThanOperator.INSTANCE);
    boolean lessOrEqual = term.isOperator(LessThanOrEqualOperator.INSTANCE);
    if (!(greater || greaterOrEqual || less || lessOrEqual)) {
      return null;
    }

    IExpression reference;
    Literal literal;
    boolean lower = greater || greaterOrEqual;
    if (term.getOperand(1) instanceof Literal right) {
      reference = term.getOperand(0);
      literal = right;
    } else if (term.getOperand(0) instanceof Literal left) {
      // 1<x is x>1
      reference = term.getOperand(1);
      literal = left;
      lower = !lower;
    } else {
      return null;
    }

    // Only the values of the type of the reference, before a coercion 5 is not ordered like '5'
    Object value = literal.getValue();
    Type type = reference.getType();
    if (value == null
        || !literal.getType().isName(type.getName())
        || !(Types.isNumeric(type) || Types.isString(type) || Types.isTemporal(type))) {
      return null;
    }

    return new ComparisonBound(term, reference, value, lower, greaterOrEqual || lessOrEqual);
  }

  /**
   * Returns whether this bound excludes more values than another bound of the same side, {@code
   * x>5} is more restrictive than {@code x>3}, and {@code x>5} than {@code x>=5}.
   *
   * @param other another lower bound if this one is a lower bound, or upper bound otherwise
   * @return true if this bound is more restrictive
   */
  public boolean isMoreRestrictive(ComparisonBound other) {
    int compare = compare(other);
    if (compare == 0) {
      return !inclusive && other.inclusive;
    }
    return lower ? compare > 0 : compare < 0;
  }

  /**
   * Returns whether no value is both above this lower bound and below an upper bound, like {@code
   * x>5} and {@code x<1}, or {@code x>5} and {@code x<=5}.
   *
   * @param upper the upper bound
   * @return true if the range is empty
   */
  public boolean isEmptyRange(ComparisonBound upper) {
    int compare = compare(upper);
    return compare > 0 || (compare == 0 && !(inclusive && upper.inclusive));
  }

  /**
   * Returns whether every value is above this lower bound or below an upper bound, like {@code x>1}
   * or {@code x<5}, or {@code x>=5} or {@code x<5}.
   *
   * @param upper the upper bound
   * @return true if the bounds include every value
   */
  public boolean isFullRange(ComparisonBound upper) {
    int compare = compare(upper);
    return compare < 0 || (compare == 0 && (inclusive || upper.inclusive));
  }

  private int compare(ComparisonBound other) {
    return reference.getType().compare(value, other.value);
  }
}
