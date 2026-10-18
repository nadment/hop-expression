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
package org.apache.hop.expression.operator;

import java.math.BigDecimal;
import org.apache.hop.expression.IAggregateProcessor;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Operator;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Computes the statistical variance of a population of all values over a group of rows.
 *
 * <p>The population-based variance (s2) of numeric-expression (x) is computed according to the
 * following formula: s2 = (1/N) * SUM( xI - mean( x ) )2
 *
 * <p>Null values are ignored.
 */
@NullMarked
public class VariancePopProcessor implements IAggregateProcessor {

  // Exact sums computed incrementally, without storing the values
  private long count;
  private BigDecimal sum;
  private BigDecimal sumOfSquares;

  public VariancePopProcessor() {
    count = 0;
    sum = BigDecimal.ZERO;
    sumOfSquares = BigDecimal.ZERO;
  }

  @Override
  public void process(IExpression[] operands) throws Exception {
    BigDecimal value = operands[0].getValue(BigDecimal.class);
    if (value != null) {
      count++;
      sum = sum.add(value);
      sumOfSquares = sumOfSquares.add(value.multiply(value));
    }
  }

  @Override
  public @Nullable BigDecimal getValue() throws Exception {
    // The result of an empty group is NULL
    if (count == 0) return null;

    // (N * SUM(x²) - SUM(x)²) / N², with a single rounding
    BigDecimal n = BigDecimal.valueOf(count);
    return sumOfSquares
        .multiply(n)
        .subtract(sum.multiply(sum))
        .divide(n.multiply(n), Operator.MATH_CONTEXT);
  }
}
