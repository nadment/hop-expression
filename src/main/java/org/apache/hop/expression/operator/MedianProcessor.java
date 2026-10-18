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
import org.apache.commons.math3.stat.descriptive.rank.Median;
import org.apache.commons.math3.util.ResizableDoubleArray;
import org.apache.hop.expression.IAggregateProcessor;
import org.apache.hop.expression.IExpression;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** Returns the median of all values in the expression over a group of rows. */
@NullMarked
public class MedianProcessor implements IAggregateProcessor {

  private static final Median MEDIAN = new Median();

  // Primitive array to avoid boxing each value
  private final ResizableDoubleArray values;

  public MedianProcessor() {
    values = new ResizableDoubleArray();
  }

  @Override
  public void process(IExpression[] operands) throws Exception {
    BigDecimal value = operands[0].getValue(BigDecimal.class);
    if (value != null) {
      values.addElement(value.doubleValue());
    }
  }

  @Override
  public @Nullable Object getValue() throws Exception {
    // The result of an empty group is NULL
    if (values.getNumElements() == 0) return null;

    double value = MEDIAN.evaluate(values.getElements());
    return BigDecimal.valueOf(value);
  }
}
