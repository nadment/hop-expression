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

import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionProcessor;

public class FirstValueIgnoreNullsProcessor implements IExpressionProcessor {

  private boolean set;
  private Object value;

  public FirstValueIgnoreNullsProcessor() {
    set = false;
    value = null;
  }

  @Override
  public void process(IExpression[] operands) throws Exception {

    if (set)
      return;

    Object v = operands[0].getValue();

    if (v != null) {
      value = v;
      set = true;
    }
  }

  @Override
  public Object getValue() throws Exception {
    return value;
  }
}
