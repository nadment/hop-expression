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
import java.util.AbstractList;
import java.util.List;

/**
 * Allows multiple {@link IOperandTypeChecker} rules to be combined into one rule.
 */
public class CompositeOperandTypeChecker implements IOperandTypeChecker {

  public enum Composition {
    AND, OR, SEQUENCE, REPEAT
  }

  protected final Composition composition;
  protected final List<? extends IOperandTypeChecker> rules;
  protected final IOperandCountRange range;

  CompositeOperandTypeChecker(Composition composition, List<? extends IOperandTypeChecker> rules, IOperandCountRange range) {
    super();
    this.composition = composition;
    this.rules = rules;
    this.range = range;
  }

  @Override
  public boolean checkOperandTypes(Call call) {
    switch (composition) {
      case REPEAT:      
        for (IOperandTypeChecker rule : rules) {
          ISingleOperandTypeChecker checker = (ISingleOperandTypeChecker) rule;

          if (!checker.checkSingleOperandType(call.getOperand(0))) {
            return false;
          }
        }
        return true;
      case AND:
        for (IOperandTypeChecker rule : rules) {
          if (!rule.checkOperandTypes(call)) {
            return false;
          }
        }
        return true;

      case OR:
        for (IOperandTypeChecker rule : rules) {
          if (rule.checkOperandTypes(call)) {
            return true;
          }
        }
        return false;

      case SEQUENCE:
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

    return true;
  }

  @Override
  public IOperandCountRange getOperandCountRange() {

    switch (composition) {
      case REPEAT:
        return range;
      case SEQUENCE:
        return OperandCountRange.of(rules.size());
      case AND:
      case OR:
      default:
        final List<IOperandCountRange> ranges = new AbstractList<IOperandCountRange>() {
          @Override
          public IOperandCountRange get(int index) {
            return rules.get(index).getOperandCountRange();
          }

          @Override
          public int size() {
            return rules.size();
          }
        };

        final int min = min(ranges);
        final int max = max(ranges);

        IOperandCountRange composite = new IOperandCountRange() {
          @Override
          public boolean isValid(int count) {
            switch (composition) {
              case AND:
                for (IOperandCountRange range : ranges) {
                  if (!range.isValid(count)) {
                    return false;
                  }
                }
                return true;
              case OR:
              default:
                for (IOperandCountRange range : ranges) {
                  if (range.isValid(count)) {
                    return true;
                  }
                }
                return false;
            }
          }

          @Override
          public int getMin() {
            return min;
          }

          @Override
          public int getMax() {
            return max;
          }
        };


        if (max >= 0) {
          for (int i = min; i <= max; i++) {
            if (!composite.isValid(i)) {
              // Composite is not a simple range. Can't simplify,
              // so return the composite.
              return composite;
            }
          }
        }
        return min == max ? OperandCountRange.of(min) : OperandCountRange.between(min, max);
    }
  }

  public List<? extends IOperandTypeChecker> getRules() {
    return rules;
  }

  private static int min(List<IOperandCountRange> ranges) {
    int min = Integer.MAX_VALUE;
    for (IOperandCountRange range : ranges) {
      min = Math.min(min, range.getMin());
    }
    return min;
  }

  private int max(List<IOperandCountRange> ranges) {
    int max = Integer.MIN_VALUE;
    for (IOperandCountRange range : ranges) {
      if (range.getMax() < 0) {
        if (composition == Composition.OR) {
          return -1;
        }
      } else {
        max = Math.max(max, range.getMax());
      }
    }
    return max;
  }
}
