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
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.math3.stat.descriptive.rank.Percentile;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionProcessor;

/**
 * Returns the average (arithmetic mean) of all values in the expression over a group of rows. Null
 * values are ignored.
 */
public class PercentileProcessor implements IExpressionProcessor {

  private static final Percentile PERCENTILE = new Percentile();
  private final double quantile;
  private final List<Double> values;

  public PercentileProcessor(BigDecimal quantile) {
    this.values = new ArrayList<>();
    this.quantile = quantile.doubleValue();
  }

  @Override
  public void process(IExpression[] operands) throws Exception {
    BigDecimal value = operands[0].getValue(BigDecimal.class);
    if (value != null) {
      values.add(value.doubleValue());
    }
  }

  @Override
  public Object getValue() throws Exception {

    final double[] array = new double[values.size()];

    for (int i = 0; i < array.length; i++) array[i] = values.get(i);

    double value = PERCENTILE.evaluate(array, quantile);

    return BigDecimal.valueOf(value);
  }
}
