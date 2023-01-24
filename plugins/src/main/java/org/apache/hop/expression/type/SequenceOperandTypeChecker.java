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
import java.util.List;
import java.util.function.Predicate;

public class SequenceOperandTypeChecker implements IOperandTypeChecker {

  private final List<IOperandTypeChecker> rules;
  private final IOperandCountRange range;
  private final Predicate<Integer> optional;

  SequenceOperandTypeChecker(List<IOperandTypeChecker> rules) {
    super();
    this.rules = rules;
    this.optional = i -> false;
    this.range = OperandCountRange.of(rules.size());
  }

  SequenceOperandTypeChecker(List<IOperandTypeChecker> rules, Predicate<Integer> optional) {
    super();
    this.rules = rules;
    this.optional = optional;
    final int max = rules.size();
    int min = max;
    while (min > 0 && optional.test(min - 1)) {
      --min;
    }
    this.range = OperandCountRange.between(min, max);
  }

  /**
   * Allows specified parameters to be optional.
   */
  public SequenceOperandTypeChecker optional(Predicate<Integer> optional) {
    return new SequenceOperandTypeChecker(this.rules, optional);
  }

  @Override
  public boolean checkOperandTypes(Call call) {
    int index = 0;
    for (IOperandTypeChecker rule : rules) {
      ISingleOperandTypeChecker checker = (ISingleOperandTypeChecker) rule;

      if (index < call.getOperandCount()
          && !checker.checkSingleOperandType(call.getOperand(index++))) {
        return false;
      }
    }
    return true;
  }

  @Override
  public IOperandCountRange getOperandCountRange() {
    return range;
  }

  @Override
  public boolean isOptional(int i) {
    return optional.test(i);
  }
}
