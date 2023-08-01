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
 * Contains function
 */
public class ContainsBinaryFunction extends ContainsFunction {
  public static final ContainsBinaryFunction INSTANCE = new ContainsBinaryFunction();
  
  public ContainsBinaryFunction() {
    super();
  }

  @Override
  public Object eval(final IExpression[] operands) {
    byte[] value = operands[0].getValue(byte[].class);
    if (value == null)
      return null;

    byte[] search = operands[1].getValue(byte[].class);
    if (search == null)
      return null;

    if (search.length == 0) {
      return 0;
    }

    outer:
    for (int i = 0; i < value.length - search.length + 1; i++) {
      for (int j = 0; j < search.length; j++) {
        if (value[i + j] != search[j]) {
          continue outer;
        }
      }
      return Boolean.TRUE;
    }
    return Boolean.FALSE;    
  }
}
