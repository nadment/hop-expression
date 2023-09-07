/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package org.apache.hop.expression.type;

import org.apache.hop.expression.Call;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Tuple;

public class CaseOperatorOperandTypeChecker implements IOperandTypeChecker {

  public CaseOperatorOperandTypeChecker() {
    
  }

  @Override
  public boolean checkOperandTypes(final Call call) {
    Tuple whenTuple = call.getOperand(1).asTuple();
    Tuple thenTuple = call.getOperand(2).asTuple();
    IExpression elseExpression = call.getOperand(3);

    Type valueType = call.getOperand(0).getType();

    // Searched case operator
    if (call.getOperand(0).isNull()) {
      valueType = BooleanType.BOOLEAN;
    }
    // Simple case operator
    else {
      valueType = call.getOperand(0).getType();
    }

    for (IExpression whenOperand : whenTuple) {
      if (!whenOperand.getType().isSameFamily(valueType)) {
        return false;
      }
    }


    Type thenType = UnknownType.UNKNOWN;
    for (IExpression thenOperand : thenTuple) {
      // First non null
      if (!thenOperand.isNull()) {
        thenType = thenOperand.getType();
      }
    }

    if (thenType.isSameFamily(TypeFamily.NONE)) {
      thenType = elseExpression.getType();
    }

    for (IExpression thenOperand : thenTuple) {
      if (!(thenOperand.getType().isSameFamily(thenType) || thenOperand.isNull())) {
        return false;
      }
    }

    return elseExpression.isNull() || elseExpression.getType().isSameFamily(thenType);
  }

  @Override
  public IOperandCountRange getOperandCountRange() {
    return OperandCountRange.any();
  }
}
