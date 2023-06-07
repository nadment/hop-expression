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
package org.apache.hop.expression.type;

import org.apache.hop.expression.Call;
import org.apache.hop.expression.IExpressionContext;

public class AddOperatorReturnTypeInference implements IReturnTypeInference {

  public AddOperatorReturnTypeInference() {
    super();
  }

  @Override
  public DataType getReturnType(IExpressionContext context, Call call) {
    DataType type1 = call.getOperand(0).getType();
    DataType type2 = call.getOperand(1).getType();
    
    int s1 = type1.getScale();
    int s2 = type2.getScale();
    int s = Math.max(s1, s2);

    int p1 = type1.getPrecision();
    int p2 = type2.getPrecision();
    int p = Math.max(p1 - s1, p2 - s2) + s + 1;
    
    return new NumberDataType(p, s);
  }
}