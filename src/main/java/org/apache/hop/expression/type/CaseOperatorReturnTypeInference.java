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
import org.apache.hop.expression.IExpression;
import org.jspecify.annotations.NullMarked;

/**
 * Type-inference strategy for the CASE operator: the least restrictive type of the <code>then
 * </code> expressions and of the <code>else</code> expression, nullable if one of them is nullable.
 */
@NullMarked
public class CaseOperatorReturnTypeInference implements IReturnTypeInference {

  public CaseOperatorReturnTypeInference() {
    super();
  }

  @Override
  public Type inferReturnType(Call call) {
    IExpression thenTerm = call.getOperand(2);
    Type elseType = call.getOperand(3).getType();

    Type returnType = Types.getLeastRestrictive(Types.getLeastRestrictive(thenTerm), elseType);

    // The result is null as soon as the selected expression is null
    boolean nullable = elseType.isNullable();
    if (thenTerm instanceof Array array) {
      for (IExpression expression : array) {
        nullable |= expression.getType().isNullable();
      }
    } else {
      nullable |= thenTerm.getType().isNullable();
    }

    return returnType.withNullability(nullable);
  }
}
