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
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Literal;

/**
 * Operand type-checking strategy which checks all operand types must be the same.
 */
public class SameOperandTypeChecker implements IOperandTypeChecker {
  private final IOperandCountRange range;
  private final int start;
  
  public SameOperandTypeChecker(IOperandCountRange range) {
    this(range,0);    
  }

  public SameOperandTypeChecker(IOperandCountRange range, int start) {    
    this.range = range;
    this.start = start;
  }
  
  @Override
  public boolean checkOperandTypes(final Call call) {
    int max = range.getMax();
    if (max == -1) {
      max = call.getOperandCount();
    }
   
    TypeFamily firstFamily = null;
    for (int i = start; i < max; i++) {
      IExpression operand = call.getOperand(i);

      // Ignore null
      if ( operand==Literal.NULL) continue;
      
      Type type = operand.getType();
      if (firstFamily != null) {
        if (!type.isSameFamily(firstFamily)) {
          return false;
        }
      } else {
        firstFamily = type.getFamily();
      }
    }

    return true;
  }

  @Override
  public IOperandCountRange getOperandCountRange() {
    return range;
  }
}
