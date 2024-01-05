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

public class InOperatorOperandTypeChecker implements IOperandTypeChecker {

  public InOperatorOperandTypeChecker() {
    super();
  }

  @Override
  public boolean checkOperandTypes(final Call call) {
    Type type = call.getOperand(0).getType();
    Tuple tuple = call.getOperand(1).asTuple();

    for (IExpression operand : tuple) {
      if (!operand.getType().isCompatibleWithCoercion(type)) {
        return false;
      }
    }

    return true;
  }

  @Override
  public IOperandCountRange getOperandCountRange() {
    return OperandCountRange.of(2);
  }
}
