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
 * String concatenation function with separator
 */
public class ConcatWsStringFunction extends ConcatWsFunction {
  public static final ConcatWsStringFunction INSTANCE = new ConcatWsStringFunction();

  public ConcatWsStringFunction() {
    super();
  }

  @Override
  public Object eval(final IExpression[] operands) {

    String separator = operands[0].getValue(String.class);
    if (separator == null)
      return null;

    StringBuilder builder = new StringBuilder();
    for (int i = 1; i < operands.length; i++) {
      String value = operands[i].getValue(String.class);
      if (value != null) {
        if (builder.length() > 0) {
          builder.append(separator);
        }
        builder.append(value);
      }
    }

    if (builder.length() == 0)
      return null;

    return builder.toString();
  }
}
