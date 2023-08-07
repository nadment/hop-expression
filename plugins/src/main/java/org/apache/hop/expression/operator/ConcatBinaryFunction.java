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
 * Binary concatenation operator '<code>||</code>'
 */
public class ConcatBinaryFunction extends ConcatFunction {
  static final ConcatBinaryFunction INSTANCE = new ConcatBinaryFunction();

  public ConcatBinaryFunction() {
    super();
  }

  @Override
  public Object eval(final IExpression[] operands) {
    byte[][] values = new byte[operands.length][];
    int i = 0;
    int length = 0;
    for (IExpression operand : operands) {
      byte[] value = operand.getValue(byte[].class);
      values[i++] = value;
      if (value != null) {
        length += value.length;
      }
    }

    if (length == 0)
      return null;

    final byte[] result = new byte[length];
    int index = 0;
    for (byte[] value : values) {
      if (value != null) {
        System.arraycopy(value, 0, result, index, value.length);
        index += value.length;
      }
    }
    return result;
  }
}
