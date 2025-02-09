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

import org.apache.hop.expression.Array;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.IExpression;

/** Array to array concatenation */
public class ArrayConcatFunction extends ConcatFunction {

  public static final Function INSTANCE = new ArrayConcatFunction();

  @Override
  public Object eval(final IExpression[] operands) {
    Array array0 = (Array) operands[0];
    Array array1 = (Array) operands[1];

    int size0 = array0.size();
    int size1 = array1.size();

    IExpression[] values = new IExpression[size0 + size1];
    int i = 0;
    for (; i < size0; i++) {
      values[i] = array0.get(i);
    }
    for (int j = 0; j < size1; i++, j++) {
      values[i] = array1.get(j);
    }

    return new Array(array0.getType(), values);
  }
}
