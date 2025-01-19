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

import java.util.List;
import java.util.function.Predicate;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.IExpression;

/** Operand type-checking strategy which checks operands for inclusion in type families. */
public class FamilyOperandTypeChecker implements IOperandTypeChecker, ISingleOperandTypeChecker {

  private final List<TypeFamily> families;
  private final Predicate<Integer> optional;

  FamilyOperandTypeChecker(List<TypeFamily> families, Predicate<Integer> optional) {
    this.families = families;
    this.optional = optional;
  }

  /** Allows specified parameters to be optional. */
  public FamilyOperandTypeChecker optional(Predicate<Integer> optional) {
    return new FamilyOperandTypeChecker(families, optional);
  }

  @Override
  public boolean checkOperandTypes(final Call call) {

    IOperandCountRange range = this.getOperandCountRange();

    // Variadic
    if (families.size() != range.getMax()) {
      for (IExpression operand : call.getOperands()) {
        if (!operand.getType().getName().isFamily(families.get(0))) {
          return false;
        }
      }

      return true;
    }
    // List
    else {
      for (int i = 0; i < call.getOperandCount(); i++) {
        if (i == families.size()
            || !call.getOperand(i).getType().getName().isFamily(families.get(i))) {
          return false;
        }
      }
    }

    return true;
  }

  @Override
  public IOperandCountRange getOperandCountRange() {
    final int max = families.size();
    int min = max;
    while (min > 0 && optional.test(min - 1)) {
      --min;
    }
    return OperandCountRange.between(min, max);
  }

  @Override
  public boolean isOptional(int i) {
    return optional.test(i);
  }

  @Override
  public boolean checkSingleOperandType(IExpression operand) {
    return !operand.getType().getName().isFamily(families.get(0));
  }
}
