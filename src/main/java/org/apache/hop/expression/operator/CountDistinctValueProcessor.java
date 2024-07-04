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

import java.util.HashSet;
import java.util.Set;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionProcessor;

/** Returns the number of distinct values. */
public class CountDistinctValueProcessor implements IExpressionProcessor {

  private Set<Object> values;

  public CountDistinctValueProcessor() {
    values = new HashSet<>();
  }

  @Override
  public void process(IExpression[] operands) throws Exception {
    Object value = operands[0].getValue();
    if (value != null && !values.contains(value)) {
      values.add(value);
    }
  }

  @Override
  public Object getValue() throws Exception {
    return Long.valueOf(values.size());
  }
}
