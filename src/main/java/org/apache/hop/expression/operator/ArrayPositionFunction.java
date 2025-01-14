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
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeComparability;
import org.apache.hop.expression.type.TypeName;

/**
 * Return a 1-based index of the first occurrence of an element in the input array. Optionally,
 * starts searching at the specified index.
 */
@FunctionPlugin
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
    boolean success = super.checkOperandTypes(call);
    if (!success) {
      return false;
    }

    Type arrayType = call.getOperand(0).getType().getElementType();
    Type elementType = call.getOperand(1).getType();

    // Array elements and searched value should be comparable
    if (!arrayType.is(TypeName.ANY)
        && (arrayType.getComparability() == TypeComparability.NONE
            || elementType.getComparability() == TypeComparability.NONE)) {
      return false;
    }

    // Array elements and searched value should be coercible
    if (!elementType.isCoercible(arrayType)) {
      return false;
    }

    return success;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Array array = (Array) operands[0];
    if (array == null) return null;
    Object value = operands[1].getValue();
    if (value == null) return null;

    int start = 0;

    // Optionally starts searching at the specified index.
    if (operands.length == 3) {
      Long v2 = operands[2].getValue(Long.class);
      if (v2 == null) return null;
      start = v2.intValue() - 1;
    }

    int size = array.size();
    Type type = operands[1].getType();
    for (int index = start; index < size; index++) {
      IExpression element = array.get(index);
      if (type.compare(element.getValue(), value) == 0) {
        return Long.valueOf(index + 1L);
      }
    }

    // If the element is not found in the array.
    return Long.valueOf(0L);
  }
}
