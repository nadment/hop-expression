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
import org.jspecify.annotations.NullMarked;

/** Operand type-checking strategy that checks all operand types must be the same. */
@NullMarked
public class SameOperandTypeChecker implements IOperandTypeChecker {
  private final IOperandCountRange range;
  private final int start;

  public SameOperandTypeChecker(IOperandCountRange range) {
    this(range, 0);
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

    Type firstType = null;
    for (int i = start; i < max; i++) {
      IExpression operand = call.getOperand(i);

      // Ignore null
      if (operand.isNull()) continue;

      Type type = operand.getType();

      // Ignore a value of any type, like ERROR(), it matches every family whatever its position
      if (type.getFamily() == TypeFamily.ANY) continue;
      if (firstType != null) {
        if (!type.isFamily(firstType.getFamily()) || !isSameArray(firstType, type)) {
          return false;
        }
      } else {
        firstType = type;
      }
    }

    return true;
  }

  /**
   * Checks that two arrays have the same number of dimensions and elements of the same family, so
   * {@code [1]} is not the same as {@code [[2]]} or {@code ['a']}.
   *
   * @param type1 the first type, of the same family as the second type
   * @param type2 the second type
   * @return {@code true} if the types are not arrays or are arrays of the same kind
   */
  private static boolean isSameArray(Type type1, Type type2) {
    if (type1 instanceof ArrayType array1 && type2 instanceof ArrayType array2) {
      Type elementType1 = array1.getElementType();
      Type elementType2 = array2.getElementType();
      // The elements of an array of NULL have no type
      if (elementType1.isName(TypeName.UNKNOWN) || elementType2.isName(TypeName.UNKNOWN)) {
        return true;
      }
      if (elementType1 instanceof ArrayType || elementType2 instanceof ArrayType) {
        return isSameArray(elementType1, elementType2);
      }
      return elementType1.isFamily(elementType2.getFamily());
    }
    return !(type1 instanceof ArrayType) && !(type2 instanceof ArrayType);
  }

  @Override
  public IOperandCountRange getOperandCountRange() {
    return range;
  }
}
