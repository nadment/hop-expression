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
 * The function repeats a string as many times as specified.
 */
public class RepeatStringFunction extends RepeatFunction {
  public static final RepeatStringFunction INSTANCE = new RepeatStringFunction();

  public RepeatStringFunction() {
    super();
  }

  @Override
  public Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null)
      return null;
    Long repeat = operands[1].getValue(Long.class);
    if (repeat == null)
      return null;
    int count = repeat.intValue();

    StringBuilder builder = new StringBuilder(value.length() * count);
    while (count-- > 0) {
      builder.append(value);
    }
    return builder.toString();
  }
}
