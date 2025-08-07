/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression.util;

import java.util.Set;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.operator.EqualOperator;
import org.apache.hop.expression.operator.GreaterThanOperator;
import org.apache.hop.expression.operator.GreaterThanOrEqualOperator;
import org.apache.hop.expression.operator.LessThanOperator;
import org.apache.hop.expression.operator.LessThanOrEqualOperator;
import org.apache.hop.expression.operator.NotEqualOperator;

/** Utilities for strong predicates. */
public final class Strong {

  private static final Set<Operator> OPERATORS =
      Set.of(
          EqualOperator.INSTANCE,
          NotEqualOperator.INSTANCE,
          LessThanOperator.INSTANCE,
          LessThanOrEqualOperator.INSTANCE,
          GreaterThanOperator.INSTANCE,
          GreaterThanOrEqualOperator.INSTANCE);

  private Strong() {
    // Utility class
  }

  /**
   * Returns whether a given expression is strong.
   *
   * <ul>
   *   <li>c = 1 is strong in [c] (definitely null if and only if c is null)
   *   <li>c IS NULL is not strong (always returns TRUE or FALSE, never null)
   *   <li>p1 AND p2 is strong in [p1, p2] (definitely null if either p1 is null or p2 is null)
   *   <li>p1 OR p2 is strong if p1 and p2 are strong
   * </ul>
   */
  public static boolean isStrong(IExpression expression) {
    for (Operator o : OPERATORS) {
      if (expression.isOperator(o)) {
        return true;
      }
    }
    return false;
  }
}
