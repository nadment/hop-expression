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

public class CompositeOperandTypeChecker implements IOperandTypeChecker {
  /** How operands are composed. */
  public enum Composition {
    AND, OR, SEQUENCE, REPEAT
  }
  
  protected final Composition composition;
  protected final List<? extends IOperandTypeChecker> allowedRules;
  
  CompositeOperandTypeChecker(Composition composition, List<? extends IOperandTypeChecker> allowedRules) {
    super();
    this.composition = composition;
    this.allowedRules = allowedRules;
  }
  
  @Override
  public boolean checkOperandTypes(Call call) {
    return true;
  }
  
  @Override public IOperandCountRange getOperandCountRange() {
    switch (composition) {
    case SEQUENCE:
      return OperandCountRange.of(allowedRules.size());
    case AND:
    case OR:
    default:
      final List<IOperandCountRange> ranges = new AbstractList<IOperandCountRange>() {
        @Override public IOperandCountRange get(int index) {
          return allowedRules.get(index).getOperandCountRange();
        }

        @Override public int size() {
          return allowedRules.size();
        }
      };
          
      final int min = minMin(ranges);
      final int max = maxMax(ranges);
      
      IOperandCountRange composite = new IOperandCountRange() {
            @Override public boolean isValidCount(int count) {
              switch (composition) {
              case AND:
                for (IOperandCountRange range : ranges) {
                  if (!range.isValidCount(count)) {
                    return false;
                  }
                }
                return true;
              case OR:
              default:
                for (IOperandCountRange range : ranges) {
                  if (range.isValidCount(count)) {
                    return true;
                  }
                }
                return false;
              }
            }

            @Override public int getMin() {
              return min;
            }

            @Override public int getMax() {
              return max;
            }
          };
          
          
      if (max >= 0) {
        for (int i = min; i <= max; i++) {
          if (!composite.isValidCount(i)) {
            // Composite is not a simple range. Can't simplify,
            // so return the composite.
            return composite;
          }
        }
      }
      return min == max
          ? OperandCountRange.of(min)
          : OperandCountRange.between(min, max);
    }
  }
  
  private static int minMin(List<IOperandCountRange> ranges) {
    int min = Integer.MAX_VALUE;
    for (IOperandCountRange range : ranges) {
      min = Math.min(min, range.getMin());
    }
    return min;
  }

  private int maxMax(List<IOperandCountRange> ranges) {
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
  
  @Override
  public boolean isOptional(int i) {
    for (IOperandTypeChecker allowedRule : allowedRules) {
      if (allowedRule.isOptional(i)) {
        return true;
      }
    }
    return false;
  }
}