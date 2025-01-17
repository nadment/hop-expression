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

import java.util.LinkedList;
import java.util.Queue;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionProcessor;

public class ListAggProcessor implements IExpressionProcessor {
  private final String delimiter;
  private final Queue<String> values;

  public ListAggProcessor(String delimiter) {
    this.values = new LinkedList<>();
    this.delimiter = delimiter;
  }

  @Override
  public void process(IExpression[] operands) throws Exception {

    String value = operands[0].getValue(String.class);

    if (value == null) return;

    values.add(value);
  }

  @Override
  public Object getValue() throws Exception {

    if (values.isEmpty()) {
      return null;
    }

    StringBuilder builder = new StringBuilder();
    for (String str : values) {
      if (builder.length() > 0) {
        builder.append(delimiter);
      }
      builder.append(str);
    }

    return builder.toString();
  }
}
