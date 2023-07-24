/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression.operator;

import org.apache.hop.expression.IExpression;


/**
 * String concatenation operator '<code>||</code>'
 */
public class ConcatStringFunction extends ConcatFunction {

  // Function
  public ConcatStringFunction() {
    super();
  }

  @Override
  public Object eval(IExpression[] operands) throws Exception {

    String firstNotNull = null;
    String[] values = new String[operands.length];
    int i = 0;
    for (IExpression operand : operands) {
      String value = operand.getValue(String.class);
      if (firstNotNull == null && value != null)
        firstNotNull = value;
      values[i++] = value;
    }

    if (firstNotNull == null)
      return null;

    StringBuilder builder = new StringBuilder();
    for (IExpression operand : operands) {
      String value = operand.getValue(String.class);
      if (value != null)
        builder.append(value);
    }

    return builder.toString();
  }
}
