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
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** Returns TRUE if the value is found in the array. */
@FunctionPlugin
@NullMarked
public class ArrayContainsFunction extends Function {

  public static final Function INSTANCE = new ArrayContainsFunction();

  public ArrayContainsFunction() {
    super(
        "ARRAY_CONTAINS",
        ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.ARRAY_ANY,
        OperatorCategory.ARRAY,
        "/docs/hop-user-manual/expression/array_contains.html");
  }

  @Override
  protected boolean checkOperandTypes(Call call) {
    boolean success = super.checkOperandTypes(call);
    if (success) {
      Type arrayType = call.getOperand(0).getType().getElementType();
      Type elementType = call.getOperand(1).getType();

      // Check if the element and the elements of the array are coercible to their most general
      // type, like ARRAY_CONTAINS([1,2],2.5)
      success = arrayType != null && Types.hasCommonType(arrayType, elementType);
    }

    return success;
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    Array array = operands[0].getValue(Array.class);
    if (array == null) return null;
    // An empty array has no element type to convert the value
    if (array.isEmpty()) return operands[1].getValue() == null ? null : Boolean.FALSE;

    // The values are converted to the most general type of the elements and the searched value,
    // to compare values of different types like INTEGER and NUMBER
    Type type = Types.getLeastRestrictive(array.getType().getElementType(), operands[1].getType());
    Class<?> clazz = type.getName().getJavaClass();
    Object value = operands[1].getValue(clazz);
    if (value == null) return null;

    for (IExpression expression : array) {
      // A null element never matches
      Object element = expression.getValue(clazz);
      if (element != null && type.compareEqual(element, value)) {
        return Boolean.TRUE;
      }
    }

    return Boolean.FALSE;
  }
}
