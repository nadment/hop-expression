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

public class ComparableOperandTypeChecker extends SameOperandTypeChecker {

  private final TypeComparability requiredComparability;

  public ComparableOperandTypeChecker(
      IOperandCountRange range, TypeComparability requiredComparability) {
    super(range);
    this.requiredComparability = requiredComparability;
  }

  @Override
  public boolean checkOperandTypes(final Call call) {
    IOperandCountRange range = getOperandCountRange();
    int max = range.getMax();
    if (max == -1) {
      max = call.getOperandCount();
    }

    TypeFamily firstFamily = null;
    for (int i = 0; i < max; i++) {
      IExpression operand = call.getOperand(i);

      Type type = operand.getType();
      if (type.getComparability().ordinal() < requiredComparability.ordinal()) {
        return false;
      }

      // Ignore null
      if (operand.isNull()) continue;
      if (firstFamily != null) {
        if (!type.getFamily().isCoercible(firstFamily)) {
          return false;
        }
      } else {
        firstFamily = type.getFamily();
      }
    }

    return true;
  }
}
