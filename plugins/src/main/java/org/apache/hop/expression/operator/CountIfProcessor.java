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

import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.IExpressionProcessor;
import org.apache.hop.expression.util.Coerse;

/** Returns the number of input values. Null values are not counted. */
public class CountIfProcessor implements IExpressionProcessor {

  private long count;

  public CountIfProcessor() {
    count = 0L;
  }

  @Override
  public void process(IExpressionContext context, IExpression[] operands)
      throws ExpressionException {

    Object result = operands[0].getValue(context);
    if (Coerse.isTrue(result))
      count++;
  }

  @Override
  public Object eval(IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    return Long.valueOf(count);
  }
}
