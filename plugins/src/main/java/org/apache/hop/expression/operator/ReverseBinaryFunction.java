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
 * The function reverses the order of bytes in a binary value.
 */
public class ReverseBinaryFunction extends ReverseFunction {
  static final ReverseBinaryFunction INSTANCE = new ReverseBinaryFunction();

  public ReverseBinaryFunction() {
    super();
  }

  @Override
  public Object eval(final IExpression[] operands) {
    final byte[] value = operands[0].getValue(byte[].class);
    if (value == null)
      return null;

    final byte[] result = new byte[value.length];
    for (int i = value.length - 1, j = 0; i >= 0; i--, j++) {
      result[j] = value[i];
    }
    return result;
  }
}
