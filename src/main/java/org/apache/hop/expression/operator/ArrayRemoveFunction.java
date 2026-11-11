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

import java.util.ArrayList;
import java.util.List;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeTransforms;
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Returns an array without all the elements equal to a value.
 *
 * @see ArrayDistinctFunction
 */
@FunctionPlugin
@NullMarked
public class ArrayRemoveFunction extends Function {

  public static final Function INSTANCE = new ArrayRemoveFunction();

  public ArrayRemoveFunction() {
    super(
        "ARRAY_REMOVE",
        ReturnTypes.ARG0.andThen(TypeTransforms.TO_NULLABLE),
        OperandTypes.ARRAY_ANY,
        OperatorCategory.ARRAY,
        "/docs/hop-user-manual/expression/array_remove.html");
  }

  @Override
  protected boolean checkOperandTypes(Call call) {
    boolean success = super.checkOperandTypes(call);
    if (success) {
      Type arrayType = call.getOperand(0).getType().getElementType();
      Type elementType = call.getOperand(1).getType();

      // Check if the removed value is coercible to the array element type
      success = arrayType != null && elementType.isCoercible(arrayType);
    }

    return success;
  }

  @Override
  public @Nullable Object eval(IExpression[] operands) {
    Array array = operands[0].getValue(Array.class);
    if (array == null) return null;

    // The elements kept are the elements compared, evaluated once
    array = array.evaluate();
    // An empty array has no element type to convert the value
    if (array.isEmpty()) return operands[1].getValue() == null ? null : array;

    // The values are converted to the most general type of the elements and the removed value, to
    // compare values of different types like INTEGER and NUMBER
    Type type = Types.getLeastRestrictive(array.getType().getElementType(), operands[1].getType());
    Class<?> clazz = type.getName().getJavaClass();
    Object value = operands[1].getValue(clazz);
    if (value == null) return null;

    List<IExpression> elements = new ArrayList<>();
    for (IExpression element : array) {
      // A NULL element never matches and is kept
      Object item = element.getValue(clazz);
      if (item == null || !type.compareEqual(item, value)) {
        elements.add(element);
      }
    }

    return new Array(array.getType(), elements);
  }
}
