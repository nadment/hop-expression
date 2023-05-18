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
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Tuple;

public class CaseOperatorOperandTypeChecker implements IOperandTypeChecker {

  public CaseOperatorOperandTypeChecker() {}

  @Override
  public boolean checkOperandTypes(Call call) {
    IExpression switchExpression = call.getOperand(0);
    Tuple whenTuple = (Tuple) call.getOperand(1);
    Tuple thenTuple = (Tuple) call.getOperand(2);
    IExpression elseExpression = call.getOperand(3);

    if (switchExpression == Literal.NULL) {
      for (IExpression whenOperand : whenTuple) {
        if (!whenOperand.getType().isSameFamily(DataFamily.BOOLEAN) ) {
          return false;
        }
      }
    } else {
      DataType switchType = switchExpression.getType();

      for (IExpression whenOperand : whenTuple) {
        if (!whenOperand.getType().isSameFamily(switchType)) {
          return false;
        }
      }
    }

    DataType thenType = DataType.UNKNOWN;
    for (IExpression thenOperand : thenTuple) {     
      // First non null      
      if ( !thenOperand.isNull() ) {
        thenType = thenOperand.getType();
      }
    } 
    
    if ( thenType.isSameFamily(DataFamily.NONE) ) {
      thenType = elseExpression.getType();
    }    
    
    for (IExpression thenOperand : thenTuple) {     
      if (!( thenOperand.getType().isSameFamily(thenType) || thenOperand.isNull()) ) {
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
