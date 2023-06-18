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

import org.apache.commons.math3.stat.descriptive.rank.Median;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionProcessor;
import java.util.ArrayList;
import java.util.List;

/**
 * Returns the median of all values in the expression over a group of rows.
 */
public class MedianProcessor implements IExpressionProcessor {

  private static final Median MEDIAN = new Median();

  private List<Double> values;

  public MedianProcessor() {
    values = new ArrayList<>();
  }

  @Override
  public void process(IExpression[] operands) throws Exception {
    Double value = operands[0].getValue(Double.class);
    if (value != null) {
      values.add(value);
    }
  }

  @Override
  public Object getValue() throws Exception {

    final double[] array = new double[values.size()];
    for (int i = 0; i < array.length; i++) {
      array[i] = values.get(i);
    }
    return MEDIAN.evaluate(array);
  }
}
