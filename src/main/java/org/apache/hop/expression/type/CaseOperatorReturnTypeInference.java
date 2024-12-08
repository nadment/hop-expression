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

import org.apache.hop.expression.Array;
import org.apache.hop.expression.Call;

/** Least restrictive then expression or else expression */
public class CaseOperatorReturnTypeInference implements IReturnTypeInference {

  public CaseOperatorReturnTypeInference() {
    super();
  }

  @Override
  public Type inferReturnType(Call call) {
    Type returnType = Types.getLeastRestrictive((Array) call.getOperand(2));
    return Types.getLeastRestrictive(returnType, call.getOperand(3).getType());
  }
}
