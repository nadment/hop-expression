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

import org.apache.commons.math3.stat.StatUtils;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.IExpressionProcessor;
import org.apache.hop.expression.util.Coerse;
import java.util.ArrayList;
import java.util.List;

/**
 * Computes the statistical variance of a population of all values over a group of rows.
 * 
 * The population-based variance (s2) of numeric-expression (x) is computed according to the following formula:
 * s2 = (1/N) * SUM( xI - mean( x ) )2
 * 
 * Null values are ignored.
 */
public class VariancePopProcessor implements IExpressionProcessor {

  private List<Double> values;

  public VariancePopProcessor() {
    values = new ArrayList<>();
  }

  @Override
  public void process(IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].getValue(context);
    if (value != null) {
      values.add(Coerse.toNumber(value));
    }
  }

  @Override
  public Object eval(IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    final double[] array = new double[values.size()];
    for (int i = 0; i < array.length; i++) {
      array[i] = values.get(i);
    }
    return StatUtils.populationVariance(array);
  }
}
