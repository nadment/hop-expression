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
 * The function returns TRUE if the first value starts with second value. Both values must be data
 * type binary.
 */
public class StartsWithBinaryFunction extends StartsWithFunction {

  static final StartsWithBinaryFunction INSTANCE = new StartsWithBinaryFunction();
  
  public StartsWithBinaryFunction() {
    super();
  }

  @Override
  public Object eval(IExpression[] operands) {

    byte[] value = operands[0].getValue(byte[].class);
    if (value == null)
      return null;
    byte[] prefix = operands[1].getValue(byte[].class);
    if (prefix == null)
      return null;

    if (prefix.length > value.length) {
      return Boolean.TRUE;
    } else {
      int end = prefix.length;
      for (int i = 0; i < end; i++) {
        if (value[i] != prefix[i]) {
          return Boolean.FALSE;
        }
      }
    }
    return Boolean.TRUE;
  }
}
