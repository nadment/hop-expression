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
package org.apache.hop.expression.operator;

import org.apache.hop.expression.Array;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeComparability;
import org.apache.hop.expression.type.TypeName;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Return a 1-based index of the first occurrence of an element in the input array. Optionally,
 * starts searching at the specified index.
 */
@FunctionPlugin
@NullMarked
public class ArrayPositionFunction extends Function {

  public static final Function INSTANCE = new ArrayPositionFunction();

  public ArrayPositionFunction() {
    super(
        "ARRAY_POSITION",
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.ARRAY_ANY.or(OperandTypes.ARRAY_ANY_INTEGER),
        OperatorCategory.ARRAY,
        "/docs/array_position.html");
  }

  @Override
  public boolean checkOperandTypes(Call call) {
    if (!super.checkOperandTypes(call)) {
      return false;
    }

    Type arrayType = call.getOperand(0).getType().getElementType();
    Type elementType = call.getOperand(1).getType();
    if (arrayType == null) {
      return false;
    }

    // Array elements and searched value should be comparable
    if (!arrayType.isName(TypeName.ANY)
        && (arrayType.getComparability() == TypeComparability.NONE
            || elementType.getComparability() == TypeComparability.NONE)) {
      return false;
    }

    // Array elements and searched value should be coercible
    return elementType.isCoercible(arrayType);
  }

  @Override
  public @Nullable Object eval(IExpression[] operands) {
    Array array = operands[0].getValue(Array.class);
    if (array == null) return null;
    Object value = operands[1].getValue();
    if (value == null) return null;

    int start = 0;

    // Optionally starts searching at the specified index.
    if (operands.length == 3) {
      Long v2 = operands[2].getValue(Long.class);
      if (v2 == null) return null;
      // The first position in the array is 1
      if (v2 < 1) {
        throw new ExpressionException(ErrorCode.ARGUMENT_OUT_OF_RANGE, 3, v2);
      }
      // Compare the long value, the conversion to int would wrap a value out of the int range
      start = (int) Math.min(v2 - 1, array.size());
    }

    int size = array.size();
    Type type = operands[1].getType();
    for (int index = start; index < size; index++) {
      // A null element never matches
      Object element = array.get(index).getValue();
      if (element != null && type.compare(element, value) == 0) {
        return Long.valueOf(index + 1L);
      }
    }

    // If the element is not found in the array.
    return 0L;
  }
}
