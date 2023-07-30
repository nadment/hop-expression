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

public class IfFunctionReturnTypeInference implements IReturnTypeInference {

  public IfFunctionReturnTypeInference() {
    super();
  }

  @Override
  public Type getReturnType(Call call) {
    if (call.getOperandCount() < 2)
      return UnknownType.UNKNOWN;

    Type type1 = call.getOperand(1).getType();
    if (call.getOperandCount() == 2) {
      return type1;
    }
    Type type2 = call.getOperand(2).getType();
    if (type1.getName().ordinal() < type2.getName().ordinal()) {
      return type2;
    }

    return type1;
  }
}
