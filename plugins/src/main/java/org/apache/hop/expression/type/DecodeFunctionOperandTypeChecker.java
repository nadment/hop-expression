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

public class DecodeFunctionOperandTypeChecker implements IOperandTypeChecker {

  public DecodeFunctionOperandTypeChecker() {
    super();
  }

  @Override
  public boolean checkOperandTypes(Call call) {
    Type search = call.getOperand(0).getType();
    Type result = firstNonNull(call.getOperands()).getType();

    int count = ((call.getOperandCount() - 1) / 2) * 2;
    for (int i = 1; i < count; i += 2) {
      if (!search.isFamily(call.getOperand(i).getType().getFamily())) {
        return false;
      }

      IExpression operandResult = call.getOperand(i + 1);
      if (!(result.isFamily(operandResult.getType().getFamily()) || operandResult.isNull())) {
        return false;
      }
    }

    // Check type if function has a default value
    if ((call.getOperandCount() - 1) > count
        && !result.isFamily(call.getOperand(count + 1).getType().getFamily())) {
      return false;
    }

    return true;
  }

  private IExpression firstNonNull(IExpression[] operands) {
    for (int i = 2; i < operands.length; i += 2) {
      if (!operands[i].isNull())
        return operands[i];
    }
    return operands[2];
  }

  @Override
  public IOperandCountRange getOperandCountRange() {
    return OperandCountRange.between(3, Integer.MAX_VALUE);
  }
}
