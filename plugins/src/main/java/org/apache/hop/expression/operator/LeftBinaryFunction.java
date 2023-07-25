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
 * The function extracts a number of characters from a string starting from left.
 */
public class LeftBinaryFunction extends LeftFunction {

  public LeftBinaryFunction() {
    super();
  }

  @Override
  public Object eval(final IExpression[] operands) throws Exception {
    byte[] bytes = operands[0].getValue(byte[].class);
    if (bytes == null)
      return null;

    Long v1 = operands[1].getValue(Long.class);
    if (v1 == null)
      return null;
    int length = v1.intValue();
    if (length < 0) {
      length = 0;
    }

    if (bytes.length <= length) {
      return bytes;
    }

    byte[] result = new byte[length];
    System.arraycopy(bytes, 0, result, 0, length);
    return result;

  }
}